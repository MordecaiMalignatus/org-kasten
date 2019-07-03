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

(defun org-kasten--reference-dir ()
  "Home to your references and your otherwise notes.
Located in `org-kasten-home'/References."
  (if (eq nil org-kasten-home)
      nil
    (concat org-kasten-home "References/")))

;; TODO: I need to merge references and links if I'm going to distinguish the
;; two in form of links regardless. Then one header field would fall away and it
;; would look more unifom, not to mention allow me to largely merge reference
;; and links navigation.

(define-minor-mode org-kasten-mode
  "A minor mode providing the features of a Zettelkasten. Requires org."
  :lighter " org-k"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-# C-#") 'org-kasten-open-index)
	    (define-key map (kbd "C-# c n") 'org-kasten-create-note)
            (define-key map (kbd "C-# c r") 'org-kasten-create-reference)
	    ;; (define-key map (kbd "C-# r r") 'org-kasten-remove-reference)
	    (define-key map (kbd "C-# c c") 'org-kasten-create-child-note)
	    (define-key map (kbd "C-# n") 'org-kasten-navigate-links)
	    ;; (define-key map (kbd "C-# r") 'org-kasten-navigate-references)
	    (define-key map (kbd "C-# c l") 'org-kasten-add-link)
	    (define-key map (kbd "C-# r l") 'org-kasten-remove-link)
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
    (setq-local org-kasten-links (split-string (cdr (assoc "LINKS" properties))))
    (setq-local org-kasten-references (split-string (cdr (assoc "REFERENCES" properties))))))

(defun org-kasten--write-properties ()
  "Write the buffer-local variables to the properties header."
  (let* ((old-position (point))
	 (removed-header (let* ((buffer (buffer-substring-no-properties (point-min) (point-max)))
				(lines (split-string buffer "\n"))
				(header-less (-drop 3 lines)))
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
		 (string-join org-kasten-links " ")))
	(references (if (eq org-kasten-references '())
			"nil"
		      (string-join org-kasten-references " "))))
    (concat "#+ID: " id "\n"
	    "#+LINKS: " links "\n"
	    "#+REFERENCES: " references "\n")))

;; TODO: This needs to be expanded to take a path as well, so I can use it for references.
(defun org-kasten--find-file-for-index (index)
  "Convert a link INDEX as number or string to a full filepath."
  (if (not (string= nil org-kasten-home))
      (let* ((notes-in-kasten (org-kasten--notes-in-kasten))
	     (string-index (if (numberp index)
			       (number-to-string index)
			     index))
	     (files-starting-with-index (-filter (lambda (file) (s-starts-with-p (concat string-index "-") file))
						 notes-in-kasten)))
	(cond
	 ((> (length files-starting-with-index) 1)
	  (error (concat "Org-Kasten inconsistent, multiple files with index " string-index)))
	 ((= (length files-starting-with-index) 0)
	  (error (concat "Org-Kasten inconsistent, does not contain a note with index " string-index)))
	 (t
	  (car files-starting-with-index))))))

(defun org-kasten--file-to-index (filepath)
  "Take a full FILEPATH, and return the index of the file, if it is in the kasten."
  (substring
   filepath
   0
   (s-index-of "-" filepath)))

(defun org-kasten--reference-to-index (filepath)
  "Take the FILEPATH to a reference, and extract its index from it."
  ;; Skip one for the "R" in the beginning of the filename.
  (substring filepath 1 (s-index-of "-" filepath)))

(defun org-kasten--notes-in-kasten ()
  "Return a list of all viable notes in the kasten."
  (-filter
   (lambda (file)
     (s-matches?
      "^[[:digit:]]+-[[:alnum:]-]+.org$"
      file))
   (directory-files
    org-kasten-home)))

(defun org-kasten--references-in-kasten ()
  "Return a list of all references in the kasten."
  (-filter
   (lambda (file)
     (s-matches?
      "^R[[:digit:]]+-[[:alnum:]-]+.org$"
      file))
   (directory-files
    (org-kasten--reference-dir))))

(defun org-kasten--mk-default-note-content (note-id headline links references body)
  "Take the individual pieces of a new note and stitch together the body.
NOTE-ID: the number that will identitify the new note.
HEADLINE: The headline, later also the file name fragment.
LINKS: A list or string of indices that define the links.
REFERENCES: A list or string of indices that define the references.
BODY: The body of the note, the part under the headlines."
  (let* ((formatted-links      (if (eq '() links) "nil" (string-join links " ")))
	(formatted-references (if (eq '() references) "nil" (string-join references " ")))
	(strings    (list (concat "#+ID: " note-id)
		       (concat "#+LINKS: " formatted-links)
		       (concat "#+REFERENCES: " formatted-references)
		       "#+STARTUP: showall\n"
		       (concat "* " headline "\n")
		       body)))
    (string-join strings "\n")))

(defun org-kasten--mk-default-reference-content (reference-id headline links body)
  "Take individual pieces and make a new reference.
The REFERENCE-ID usually is auto-generated, but you can manually
enumerate a reference.  HEADLINE is used for what it says on the
tin, LINKS are connections to other notes that you already know
are relevant, and BODY is for when you're trying to transplant a
region, and need somewhere for the text to be."
  (let* ((formatted-links (if (eq '() links) "nil" (string-join links " ")))
	 (strings (list (concat "#+ID: " reference-id)
			(concat "#+LINKS: " formatted-links)
			"#+STARTUP: showall\n"
			(concat "* " headline "\n")
			body)))
    (string-join strings "\n")))

(defun org-kasten--headline-to-filename-fragment (headline)
  "Turn a typed HEADLINE to a filename fragment.
The fragment is the part that goes after the index: `2-this-is-the-fragment.org'"
  (let* ((downcased (s-downcase headline))
	 (no-punctuation (s-replace-regexp "[[:punct:]]" "" downcased))
	 (trimmed (s-trim no-punctuation))
	 (no-spaces (s-replace-regexp "[[:space:]]" "-" trimmed)))
    no-spaces))

(defun org-kasten--generate-new-note (headline links references note-body)
  "Generate a new note.
Uses the HEADLINE, LINKS, REFERENCES and the NOTE-BODY as default values for the template."
  (let* ((current-highest-index (-max (mapcar 'string-to-number  (mapcar 'org-kasten--file-to-index (org-kasten--notes-in-kasten)))))
	 (note-id              (number-to-string (+ 1 current-highest-index)))
	 (file-content         (org-kasten--mk-default-note-content note-id headline links references note-body))
	 (stringified-headline (org-kasten--headline-to-filename-fragment headline)))
    (find-file (concat org-kasten-home note-id "-" stringified-headline  ".org"))
    (insert file-content)
    (org-kasten--read-properties)
    note-id))

(defun org-kasten--generate-new-reference (headline links reference-body)
  "Generate a new reference out of the given parts and sort it into the kasten.
HEADLINE, and REFERENCE-BODY are self explanatory, LINKS are the notes that are already linked to it."
  (let* ((current-highest-index (-max (mapcar 'string-to-number (mapcar'org-kasten--reference-to-index (org-kasten--references-in-kasten)))))
	 (new-reference-id (number-to-string (+ 1 current-highest-index)))
	 (file-content (org-kasten--mk-default-reference-content new-reference-id headline links reference-body))
	 (stringified-headline (org-kasten--headline-to-filename-fragment headline)))
    (find-file (concat (org-kasten--reference-dir) "R" new-reference-id "-" stringified-headline ".org"))
    (insert file-content)
    (org-kasten--read-properties)
    new-reference-id))

(defun org-kasten--add-link-to-file (file target-index)
  "Add a link to TARGET-INDEX in FILE."
  ;; Open/Visit target file, parse properties, push target-index, write properties, go back.
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

;; TODO: Navigating from Reference to Note does not work. (Error stringp nil
;; Meaning this does navigate to ".." for references, landing nowhere.
(defun org-kasten-navigate-links ()
  "Navigate to one of the links from the current card.
Uses `completing-read', use with ivy for best results."
  (interactive)
  (org-kasten--read-properties)
  (let ((files (mapcar 'org-kasten--find-file-for-index org-kasten-links)))
    (find-file (completing-read "Links:" files))))

(defun org-kasten-navigate-references ()
  "Navigate to the bibliographical references of this card."
  (interactive)
  (org-kasten--read-properties)
  (let ((files (mapcar 'org-kasten--find-file-for-index org-kasten-links)))
    (find-file (completing-read "References:" files))))

(defun org-kasten-create-note (read-title)
  "Create a new, enumerated note in the Kasten.
The READ-TITLE is going into the file fragment and the headline of the new note."
  (interactive "MTitle: ")
  (org-kasten--read-properties)
  (org-kasten--generate-new-note read-title '() '() ""))

(defun org-kasten-create-child-note (title)
  "Create a new card with TITLE that is linked to the current note."
  (interactive "MTitle: ")
  (org-kasten--read-properties)
  (let ((current-file (buffer-file-name))
	(new-id (org-kasten--generate-new-note title (list org-kasten-id) '() ""))
	(new-buffer (buffer-name)))
    (org-kasten--add-link-to-file current-file new-id)
    (switch-to-buffer new-buffer)))

(defun org-kasten-create-reference (title)
  "Create a new literary note in the reference store with TITLE."
  (interactive "MTitle: ")
  (org-kasten--generate-new-reference title '() ""))

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
	 (candidates (-filter (lambda (file) (not (string= file (buffer-file-name)))) files))
	 (current-file-index (org-kasten--file-to-index (buffer-file-name)))
	 (target-file (completing-read "File to link to: " candidates)))
    (save-current-buffer
      (org-kasten--add-link-to-file target-file org-kasten-id)
      (org-kasten--add-link-to-file (buffer-file-name) (org-kasten--file-to-index target-file)))))

(defun org-kasten-remove-link ()
  "Remove an existing link between this card and another."
  (interactive)
  (org-kasten--read-properties)
  (when (not (org-kasten--file-in-kasten-p (buffer-file-name)))
    (error "Current Buffer not part of the kasten"))
  (let* ((linked-files (mapcar 'org-kasten--find-file-for-index org-kasten-links))
	 (current-file-index (org-kasten--file-to-index (buffer-file-name)))
	 (target-file (completing-read "Link to remove: " linked-files)))
    (save-current-buffer
      (org-kasten--remove-link-from-file target-file org-kasten-id)
      (org-kasten--remove-link-from-file (buffer-file-name) (org-kasten--file-to-index target-file)))))

;; TODO: Implement function.
;; (defun org-kasten-delete-note ()
;;   "Delete a card and all of its links.
;; Can be useful, if it's useful too often you might need to reconsider."
;;   (interactive))

(provide 'org-kasten)
;;; org-kasten.el ends here
