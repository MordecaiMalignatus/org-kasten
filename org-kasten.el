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

(defvar org-kasten-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<") 'org-kasten-navigate-parent)
    (define-key map (kbd "C->") 'org-kasten-navigate-children)
    (define-key map (kbd "C-# c") 'org-kasten-copy-file-id)
    (define-key map (kbd "C-# l t") 'org-kasten-link-to-note)
    (define-key map (kbd "C-# l f") 'org-kasten-link-from-note)
    (define-key map (kbd "C-# s") 'org-kasten-search-for-this-note)
    (define-key map (kbd "C-# C-#") 'org-kasten-open-index)
    map))

(define-minor-mode org-kasten-mode
  "A minor mode providing the features of a Zettelkasten. Requires org."
  :lighter " org-k")

(defun org-kasten--file->id (id)
  "Trim the filepath off of an ID, leaving only the alphanumeric identifier."
  (declare (pure t) (side-effect-free t))
  (s-chop-suffix ".org" (s-chop-prefix (expand-file-name org-kasten-home) id)))

(defun org-kasten--file-in-kasten-p (filepath)
  "Is the file we're looking at in the kasten?
This is needed for figuring out how to deal with links.
FILEPATH: File in question."
  (s-starts-with-p
   (file-truename org-kasten-home)
   (file-truename filepath)))

(defun org-kasten--note->preview (note-id)
  "Turn a NOTE-ID into a preview string."
  (let* ((note (org-kasten--file->id note-id))
         (buffer-text (with-temp-buffer
                        (insert-file-contents (concat org-kasten-home note ".org"))
                        (buffer-string)))
         (stripped-header (s-replace "#+STARTUP: showall\n" "" buffer-text))
         (stripped-links (s-replace-regexp "#\\+LINKS: [[:alnum:][:blank:]]+\n" "" stripped-header))
         (trimmed (s-trim stripped-links))
         (stripped-newlines (s-replace "\n" " " trimmed)))
    (concat note-id " - " stripped-newlines)))

(defun org-kasten--preview->note (preview)
  "Turn a PREVIEW back into a note-id."
  (concat org-kasten-home (seq-take-while (lambda (char) (not (equal char ? ))) preview) ".org"))

(defun org-kasten--is-direct-child-p (note1 note2)
  "Decides if NOTE2 is a direct child of NOTE1.
This fails if, for example, NOTE2 is too far removed: 1a1b1 from 1a.
It also fails if NOTE2 is a parent of NOTE2."
  (declare (pure t) (side-effect-free t))
  (let* ((n1-segments  (org-kasten--split-id-segments (org-kasten--file->id note1)))
         (n2-segments  (org-kasten--split-id-segments (org-kasten--file->id note2))))
    ;; if the parent is 0, any note with single segment is a direct child.
    (or (and (equal n1-segments '("0"))
             (= 1 (length n2-segments))
             (not (equal n2-segments '("0"))))
        ;; If the two notes share n1 as prefix, and n2 is one segment longer, it's a direct child.
        (and (equal (-common-prefix n1-segments n2-segments) n1-segments)
             (= (+ 1  (length n1-segments)) (length n2-segments))))))

(defun org-kasten--current-note-id ()
  "Retrieve current note ID from filename."
  (let ((filename (buffer-file-name)))
    (if (not (org-kasten--file-in-kasten-p filename))
        (error "Current file not in org-kasten, or nil")
      (let ((without-dir (s-chop-prefix (file-truename org-kasten-home) filename)))
        (s-chop-suffix ".org" without-dir)))))

(defun org-kasten--parse-properties ()
  "Get list of all regexp match in current file.
All lines of format `#+KEY: VALUE' will be extracted, to keep with org syntax."
  (save-match-data
    (let ((buffer-string (buffer-substring-no-properties (point-min) (point-max)))
          (regexp "^#\\+\\(\[a-zA-Z\]+\\): \\(.*\\)")
	  (pos 0)
	  (matches '()))
      (while (string-match regexp buffer-string pos)
	(if (string= (match-string 2 buffer-string) "nil")
	    (push `(,(match-string 1 buffer-string) . "") matches)
	  (push `(,(match-string 1 buffer-string) . ,(match-string 2 buffer-string)) matches))
	(setq pos (match-end 0)))
      matches)))

(defun org-kasten--notes-in-kasten ()
  "Return a list of all viable notes in the kasten."
  (let ((raw-files (-filter (lambda (file) (s-matches? "^[[:alnum:]]+.org$" file))
                    (directory-files org-kasten-home))))
    (mapcar #'org-kasten--file->id raw-files)))

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
      (if (funcall is-digit (car chars-remaining))
	  (push (funcall take-digits chars-remaining) results)
	(push (funcall take-letters chars-remaining) results))
      (setq chars-remaining (-drop (length (car results)) chars-remaining)))
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
	 (next-segment (cond
                        ((equal segments '("0"))
                           (setq segments nil) ; Dirty hack to make incrementing from zero work.
                           "1")
                        ((= 0 (string-to-number (car (last segments)))) "1")
                        (t "a")))
	 (prospective-next-id (string-join (append segments (list next-segment)))))
    (while (-contains? kasten-contents prospective-next-id)
      (setq next-segment (org-kasten--increment-segment next-segment))
      (setq prospective-next-id (string-join (append segments (list next-segment)))))
    prospective-next-id))

(defun org-kasten--generate-new-note (id)
  "Generate a successor to ID."
  (let* ((notes (mapcar (lambda (id) (s-chop-suffix ".org" id)) (org-kasten--notes-in-kasten)))
         (note-id (org-kasten--successor-to-note id notes)))
    (find-file (concat org-kasten-home note-id ".org"))
    (insert   "#+STARTUP: showall\n\n")
    note-id))

(defun org-kasten--read-links ()
  "Read LINKS for current file, and turn them into find-able paths."
  (let* ((props (org-kasten--parse-properties))
         (links (or (cdr (assoc "LINKS" props)) (cdr (assoc "LINK" props)))))
    (if (eq links nil)
        nil
      (s-split " " links))))

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
  (let* ((id (org-kasten--current-note-id))
         (notes (org-kasten--notes-in-kasten))
         (children (-filter (lambda (x) (org-kasten--is-direct-child-p id x)) notes))
         (children-and-links (append children (org-kasten--read-links)))
         (previews (mapcar #'org-kasten--note->preview children-and-links))
         (children-links-newfile (append previews '("<new child note>")))
         (chosen-file (completing-read "Children: " children-links-newfile)))
    (if (string= chosen-file "<new child note>")
        (org-kasten-create-child-note)
      (xref-push-marker-stack)
      (find-file (org-kasten--preview->note chosen-file)))))

(defun org-kasten-create-child-note ()
  "Create a new card that is linked to the current note."
  (interactive)
  (if (not (org-kasten--file-in-kasten-p (buffer-file-name)))
      (error "Current buffer not part of the kasten"))
  (org-kasten--generate-new-note (org-kasten--current-note-id)))

(defun org-kasten-search-for-this-note ()
  "Search for the note ID of the open file in the kasten.
This may be interesting for discovering what links to this place
without introducing an explicit link."
  (interactive)
  (let ((note-id (org-kasten--current-note-id)))
    (counsel-rg note-id org-kasten-home)))

(defun org-kasten-open-index ()
  "Open your index and link file."
  (interactive)
  (find-file (concat org-kasten-home "/0.org")))

(defun org-kasten-copy-file-id ()
  "Copy ID of file to kill ring."
  (interactive)
  (kill-new (org-kasten--file->id (buffer-file-name)))
  (message "Copied note ID to kill ring."))

(defun org-kasten--links-line-present-p ()
  "Determine whether or not the current file has a `#+LINKS:' line."
  (save-excursion
    (goto-char (point-min))
    (next-line)
    (s-starts-with-p "#+LINKS: "
                     (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)))))

(defun org-kasten-insert-links-line ()
  "Insert a #+LINKS: line at the top of the file.  This function is brittle but relatively effective."
  (interactive)
  (when (not (org-kasten--links-line-present-p))
    (goto-char (point-min))
    (next-line)
    (insert "#+LINKS: \n")
    (previous-line)
    (org-end-of-line)))

(defun org-kasten-link-to-note ()
  "Link to another note from this note."
  (interactive)
  (let ((origin (buffer-file-name))
        (destination (save-excursion
                       (counsel-rg "" org-kasten-home "" "Link To: ")
                       (org-kasten--file->id (buffer-file-name)))))
    (find-file origin)
    (save-excursion
      (if (not (org-kasten--links-line-present-p))
          (org-kasten-insert-links-line)
        (progn (beginning-of-buffer) (next-line) (end-of-line) (insert " ")))
      (insert destination))))

(defun org-kasten-link-from-note ()
  "Link to *this* note from another note."
  (interactive)
  (let ((destination (buffer-file-name))
        (destination-id (org-kasten--file->id (buffer-file-name)))
        (origin (progn
                  (counsel-rg "" org-kasten-home "" "Link From: ")
                  (buffer-file-name))))
    (if (not (org-kasten--links-line-present-p))
        (org-kasten-insert-links-line)
      ;; Move to end of LINKS line to insert new link.
      (progn (beginning-of-buffer) (next-line) (end-of-line) (insert " ")))
    (insert destination-id)
    (find-file destination)))

(provide 'org-kasten)
;;; org-kasten.el ends here
