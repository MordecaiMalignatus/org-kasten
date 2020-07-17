;;; org-kasten.el --- A Zettelkasten, but in plain text, using emacs as engine underneath.
;;
;; Copyright: (C) 2020 Mordecai Malignatus
;; Version: 0.1
;; Package-Requires: (s dash)
;; Author: Mordecai Malignatus <mordecai@malignat.us>
;;
;;; Commentary:
;;
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

(defvar org-kasten-quick-nav t
  "If enabled, add shortcuts to navigation.")

(define-minor-mode org-kasten-mode
  "A minor mode providing the features of a Zettelkasten. Requires org."
  :lighter " org-k"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-# C-#") 'org-kasten-open-index)
	    (define-key map (kbd "C-# c c") 'org-kasten-create-child-note)
            (define-key map (kbd "C-# p") 'org-kasten-navigate-parent)
	    (define-key map (kbd "C-# n") 'org-kasten-navigate-children)
            map))

(if org-kasten-quick-nav
    (progn
      (define-key org-kasten-mode-map (kbd "C-<") 'org-kasten-navigate-parent)
      (define-key org-kasten-mode-map (kbd "C->") 'org-kasten-navigate-children)))

(defun org-kasten--file-in-kasten-p (filepath)
  "Is the file we're looking at in the kasten?
This is needed for figuring out how to deal with links.
FILEPATH: File in question."
  (s-starts-with-p
   (file-truename org-kasten-home)
   (file-truename filepath)))

(defun org-kasten--current-note-id ()
    "Retrieve current note ID from filename."
  (let ((filename (buffer-file-name)))
    (if (not (org-kasten--file-in-kasten-p filename))
        (error "Current file not in org-kasten, or nil")
      (let ((without-dir (s-chop-prefix (file-truename org-kasten-home) filename)))
        (s-chop-suffix ".org" without-dir)))))

(defun org-kasten--parse-properties (string)
  "Get list of all regexp match in a STRING.
All lines of format `#+KEY: VALUE' will be extracted, to keep with org syntax."
  (save-match-data
    (let ((regexp "^#\\+\\(\[a-zA-Z\]+\\): \\(.*\\)")
	  (pos 0)
	  (matches '()))
      (while (string-match regexp string pos)
	(if (string= (match-string 2 string) "nil")
	    (push `(,(match-string 1 string) . "") matches)
	  (push `(,(match-string 1 string) . ,(match-string 2 string)) matches))
	(setq pos (match-end 0)))
      matches)))

(defun org-kasten--read-properties ()
  "Read the org-kasten relevant properties from `current-file'."
  (org-kasten--parse-properties (buffer-substring-no-properties (point-min) (point-max))))

(defun org-kasten--write-properties ()
  "Write the PROPS to the properties header."
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
  (let* ((properties (org-kasten--read-properties))
        (links (cdr (assoc "LINKS" properties))))
    (concat "#+LINKS: " links "\n" "#+STARTUP: showall" "\n\n")))

(defun org-kasten--notes-in-kasten ()
  "Return a list of all viable notes in the kasten."
  (-filter
   (lambda (file)
     (s-matches?
      "^[[:alnum:]]+.org$"
      file))
   (directory-files org-kasten-home)))

(defun org-kasten--increment-segment (note-segment)
  "Increment NOTE-SEGMENT.
Probably going to be inlined in the future.  Useful for now."
  (let* ((is-digit (not (= 0 (string-to-number note-segment))))) ; 0 isn't a valid ID part, so we can cheat a bit.
    (if is-digit
	(number-to-string (+ 1 (string-to-number note-segment)))
      (let* ((letter-list (string-to-list note-segment))
	     (last-letter (car (last letter-list))))
	(if (= last-letter 122) ; last char is ?z, need to increment second-last.
	    (let ((is-single-char (= 1 (length letter-list))))
	      (if is-single-char
		  "aa"
		(concat (org-kasten--increment-segment (substring note-segment 0 (+ -1 (length letter-list))))
			"a")))
	  (concat (-take (+ -1 (length letter-list)) letter-list)
		  (list (+ 1 last-letter))))))))

(defun org-kasten--split-id-segments (note-id)
  "Split a NOTE-ID into its segments."
  (let* ((chars-remaining (string-to-list note-id))
	 (is-digit (lambda (char) (and (<= 47 char) (>= 58 char))))
	 (is-letter (lambda (char) (and (<= 97 char) (>= 122 char))))
	 (take-digits (lambda (string) (-take-while is-digit string)))
	 (take-letters (lambda (string) (-take-while is-letter string)))
	 (results '()))
    (while chars-remaining
      (if (funcall is-digit (first chars-remaining))
	  (push (funcall take-digits chars-remaining) results)
	(push (funcall take-letters chars-remaining) results))
      (setq chars-remaining (-drop (length (first results)) chars-remaining)))
    (reverse (mapcar 'concat results))))

(defun org-kasten--successor-to-note (note-id kasten-contents)
  "Create a new note ID based on NOTE-ID, in relation to KASTEN-CONTENTS.
This is a new ID that is a) unique, and b) follows NOTE-ID directly.
Based on Luhmann's scheme.

This algorithm has been simplified somewhat from the one Luhmann
used.  Luhmann had two types of child-notes, sequences and
regular children.  When he started a new child sequence to the
node `c2', they would be numbered `c2a1', `c2a2', etc, skipping
the actual direct child of `c2a'.  This version of the algorithm
omits sequences, instead treating the ID like a tree path.

Maybe a future patch will introduce the ability to turn a direct
tree descent into a sequence instead."
  (let* ((segments (org-kasten--split-id-segments note-id))
         (everything-but-last (reverse (cdr (reverse segments))))
         ;; Since alphanumeric reads to 0, and 0 isn't a valid segment, we can cheat.
	 (next-segment (if (= 0 (string-to-number (car (last segments))))
                           "1"
                         "a"))
	 (prospective-next-id (string-join (append segments (list next-segment)))))
    (while (-contains? kasten-contents prospective-next-id)
      (setq next-segment (org-kasten--increment-segment next-segment))
      (setq prospective-next-id (string-join (append segments (list next-segment)))))
    prospective-next-id))

(defun org-kasten--generate-new-note (id)
  "Generate a successor to ID."
  (let* ((note-id (org-kasten--successor-to-note id (org-kasten--notes-in-kasten))))
    (find-file (concat org-kasten-home note-id ".org"))
    (insert   "#+STARTUP: showall\n\n")
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

(defun org-kasten--read-links ()
  "Read LINKS for current file, and turn them into find-able paths."
  (let* ((props (org-kasten--read-properties))
         (links (cdr (assoc "LINKS" props))))
    (if (eq links nil)
        nil
      (progn
        (mapcar (lambda (x) (concat org-kasten-home x ".org"))
                (split-links (s-split " " links)))))))

(defun org-kasten-navigate-parent ()
  "Navigate upwards from current buffer.
Upwards here means, 'to parent file', `a1b' finds `a1', `ad17482si' finds `ad17482'."
  (interactive)
  (if (not (org-kasten--file-in-kasten-p (buffer-file-name)))
      (error "Current buffer not part of the kasten"))
  (let* ((id (org-kasten--current-note-id))
         (segments (org-kasten--split-id-segments id))
         (target-segments (reverse (cdr (reverse segments))))
         (target-filename (concat (string-join target-segments) ".org")))
    ;; In case we're at a top-level node, open the index.
    (if (= (length segments) 1)
        (find-file "0.org")
      (find-file target-filename))))

(defun org-kasten-navigate-children ()
  "Navigate to children of current note."
  (interactive)
  (if (not (org-kasten--file-in-kasten-p (buffer-file-name)))
      (error "Current buffer not part of the kasten"))
  (let* ((links (org-kasten--read-links))
         (id (org-kasten--current-note-id))
         (notes (org-kasten--notes-in-kasten))
         (children (-filter (lambda (x) (and (s-starts-with-p id x)
                                             (not (string= (concat id ".org") x))))
                            notes))
         (children-and-links (append children links))
         (children-links-newfile (push "<new child note>" children-and-links))
         (chosen-file (completing-read "Children: " children-and-links)))
    (if (string= chosen-file "<new child note>")
        (org-kasten-create-child-note)
      (find-file chosen-file))))

(defun org-kasten-create-root-note ()
  "Generate a new root-level note.  Works outside of the kasten."
  (interactive)
  (let* ((top-level-files (-filter (lambda (file) (s-matches? "^[[:digit:]]+.org$" file))
                                   (directory-files org-kasten-home)))
         (next-id (let ((tmp-id 1))
                    (while (-contains-p top-level-files (concat (number-to-string tmp-id) ".org"))
                      (setq tmp-id (+ tmp-id 1)))
                    (number-to-string tmp-id))))
    (find-file (concat org-kasten-home next-id ".org"))))


(defun org-kasten-create-child-note ()
  "Create a new card that is linked to the current note."
  (interactive)
  (if (not (org-kasten--file-in-kasten-p (buffer-file-name)))
      (error "Current buffer not part of the kasten"))
  (org-kasten--generate-new-note (org-kasten--current-note-id)))

(defun org-kasten-open-index ()
  "Open your index and link file."
  (interactive)
  (find-file (concat org-kasten-home "/0.org")))

(provide 'org-kasten)
;;; org-kasten.el ends here
