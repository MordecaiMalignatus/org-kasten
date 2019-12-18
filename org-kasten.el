;;; org-kasten --- A Zettelkasten, but in plain text, using emacs as engine underneath.
;; Package-Requires: ((org-mode)(s)(dash))
;;; Commentary:
;; This is my attempt to make Zettelkaesten not suck in the digital space, so
;; I'm piggybacking on org-mode.  Org-mode is 80% there, the chiefly missing
;; thing is that it has bad navigation for this purpose.  This is mostly a bunch
;; of convenience functions atop org.
;;; Code:
(require 's)
(require 'dash)

(defvar org-kasten-home ""
  "Your home for the kasten.
If nil, org-kasten won't do anything.")

(define-minor-mode org-kasten-mode
  "A minor mode providing the features of a Zettelkasten. Requires org."
  :lighter " org-k"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-# C-#") 'org-kasten-open-index)
	    (define-key map (kbd "C-# c n") 'org-kasten-create-note)
	    (define-key map (kbd "C-# c c") 'org-kasten-create-child-note)
	    (define-key map (kbd "C-# l") 'org-kasten-navigate-links)
	    (define-key map (kbd "C-# c l") 'org-kasten-add-link)
	    (define-key map (kbd "C-# d l") 'org-kasten-remove-link)
	    map))

(defun org-kasten--file-in-kasten-p (filepath)
  "Is the file we're looking at in the kasten?
This is needed for figuring out how to deal with links.
FILEPATH: File in question."
  (s-starts-with-p
   (file-truename org-kasten-home)
   (file-truename filepath)))

(defun org-kasten--parse-properties (string)
  "Get list of all regexp match in a STRING.
All lines of format `#+KEY: VALUE' will be extracted, to keep with org syntax."
  (save-match-data
    (let ((regexp "^#\\+\\(\[a-zA-Z\]+\\): \\(.*\\)")
	  (pos 0)
	  matches)
      (while (string-match regexp string pos)
	(if (string= (match-string 2 string) "nil")
	    (push `(,(match-string 1 string) . "") matches)
	  (push `(,(match-string 1 string) . ,(match-string 2 string)) matches))
	(setq pos (match-end 0)))
      matches)))

(defun org-kasten--read-properties ()
  "Read the org-kasten relevant properties from `current-file'."
  (let* ((buffer-text (buffer-substring-no-properties (point-min) (point-max)))
	 (properties (org-kasten--parse-properties buffer-text)))
    (setq-local org-kasten-id (cdr (assoc "ID" properties)))
    (setq-local org-kasten-links (split-string (cdr (assoc "LINKS" properties))))))

(defun org-kasten--write-properties ()
  "Write the buffer-local variables to the properties header."
  (let* ((old-position (point))
	 (removed-header (let* ((buffer (buffer-substring-no-properties (point-min) (point-max)))
				(lines (split-string buffer "\n"))
				(header-less (-drop 2 lines)))
			   (string-join header-less "\n")))
	 (new-body (concat (org-kasten--properties-to-string) removed-header)))
    (erase-buffer)
    (insert new-body)
    (goto-char old-position)))

(defun org-kasten--properties-to-string ()
  "Make a header string that can be inserted on save, with all local variables stringified."
  (let ((id org-kasten-id)		; Must be present
	(links (if (eq org-kasten-links '())
		   "nil"
		 (string-join org-kasten-links " "))))
    (concat "#+ID: " id "\n"
	    "#+LINKS: " links "\n")))

(defun org-kasten--find-notes-for-index (index)
  "Find notes that have INDEX as prefix.
Accepts either string or number `index'"
  (let ((string-index (if (numberp index) (number-to-string index) index)))
    (-filter (lambda (file) (s-starts-with-p (concat string-index "-") file))
	     (org-kasten--notes-in-kasten))))

(defun org-kasten--file-to-index (filepath)
  "Take a full FILEPATH, and return the index of the file, if it is in the kasten."
  (let ((maybe-dropped (if (s-starts-with? org-kasten-home filepath)
			   (substring filepath (length org-kasten-home) (length filepath))
			 filepath)))
    ;;  TODO: Temporary double-check while migration to dropping titles is in progress.
    (if (s-contains-p "-" maybe-dropped)
	(substring maybe-dropped 0 (s-index-of "-" maybe-dropped))
      (substring maybe-dropped 0 (s-index-of "." maybe-dropped)))))

(defun org-kasten--notes-in-kasten ()
  "Return a list of all viable notes in the kasten."
  (-filter
   (lambda (file)
     (s-matches?
      "^[[:digit:]]+[[:alnum:]-]?+.org$"
      file))
   (directory-files org-kasten-home)))

(defun org-kasten--mk-default-note-content (note-id links body)
  "Take the individual pieces of a new note and stitch together the body.
NOTE-ID: the number that will identitify the new note.
LINKS: A list or string of indices that define the links.
BODY: The body of the note."
  (let* ((formatted-links      (if (eq '() links) "nil" (string-join links " ")))
	(strings    (list (concat "#+ID: " note-id)
			  (concat "#+LINKS: " formatted-links)
			  "#+STARTUP: showall\n"
			  body)))
    (string-join strings "\n")))

(defun org-kasten--note-to-full-path (filename)
  "Convenience function for turning FILENAME into a fully-qualified path.
This is especially useful for fixing up `completing-read' filenames."
  (concat org-kasten-home filename))

(defun org-kasten--generate-new-note (links note-body)
  "Generate a new note.
Uses the LINKS and the NOTE-BODY as default values for the template."
  (let* ((current-highest-index (-max (mapcar 'string-to-number  (mapcar 'org-kasten--file-to-index (org-kasten--notes-in-kasten)))))
	 (note-id              (number-to-string (+ 1 current-highest-index)))
	 (file-content         (org-kasten--mk-default-note-content note-id links note-body)))
    (find-file (concat org-kasten-home note-id ".org"))
    (insert file-content)
    (org-kasten--read-properties)
    note-id))

(defun org-kasten--add-link-to-file (file target-index)
  "Add a link to TARGET-INDEX in FILE."
  (save-excursion
    (find-file file)
    (org-kasten--read-properties)
    (setq-local org-kasten-links (push target-index org-kasten-links))
    (org-kasten--write-properties)))

(defun org-kasten--remove-link-from-file (file target-index)
  "Remove TARGET-INDEX from the links in FILE."
  (save-excursion
    (find-file file)
    (org-kasten--read-properties)
    (setq-local org-kasten-links (-remove-item target-index org-kasten-links))
    (org-kasten--write-properties)))

(defun org-kasten-navigate-links ()
  "Navigate to one of the links from the current card.
Uses `completing-read', use with ivy for best results."
  (interactive)
  (org-kasten--read-properties)
  (let* ((files (mapcar 'org-kasten--find-notes-for-index org-kasten-links))
	 (candidate (completing-read "Links: " files)))
    (find-file (concat org-kasten-home candidate))))

(defun org-kasten-create-note ()
  "Create a new, enumerated note in the Kasten."
  (interactive)
  (org-kasten--read-properties)
  (org-kasten--generate-new-note '() ""))

(defun org-kasten-create-child-note ()
  "Create a new card that is linked to the current note."
  (interactive)
  (org-kasten--read-properties)
  (let ((current-file (buffer-file-name))
	(new-id (org-kasten--generate-new-note (list org-kasten-id) ""))
	(new-buffer (buffer-name)))
    (org-kasten--add-link-to-file current-file new-id)
    (switch-to-buffer new-buffer)))

(defun org-kasten-open-index ()
  "Open your index and link file."
  (interactive)
  (find-file (concat org-kasten-home "/0-index.org")))

(defun org-kasten-add-link ()
  "Link this card with another one.
The LINK-INDEX is a shorthand for the note to create a link to."
  (interactive)
  (org-kasten--read-properties)
  (when (not (org-kasten--file-in-kasten-p (buffer-file-name)))
    (error "Current buffer is not part of the kasten"))
  (let* ((files (org-kasten--notes-in-kasten))
	 (candidates (-filter (lambda (file) (not (string= file (buffer-name)))) files))
	 (current-file-index (org-kasten--file-to-index (buffer-file-name)))
	 (target-file (org-kasten--note-to-full-path (completing-read "File to link to: " candidates))))
    (save-current-buffer
      (org-kasten--add-link-to-file target-file org-kasten-id)
      (org-kasten--add-link-to-file (buffer-file-name) (org-kasten--file-to-index target-file)))))

(defun org-kasten-remove-link ()
  "Remove an existing link between this card and another."
  (interactive)
  (org-kasten--read-properties)
  (when (not (org-kasten--file-in-kasten-p (buffer-file-name)))
    (error "Current Buffer not part of the kasten"))
  (let* ((linked-files (mapcar 'org-kasten--find-notes-for-index org-kasten-links))
	 (current-file-index (org-kasten--file-to-index (buffer-file-name)))
	 (target-file (completing-read "Link to remove: " linked-files)))
    (save-current-buffer
      (org-kasten--remove-link-from-file target-file org-kasten-id)
      (org-kasten--remove-link-from-file (buffer-file-name) (org-kasten--file-to-index target-file)))))

(provide 'org-kasten)
;;; org-kasten.el ends here
