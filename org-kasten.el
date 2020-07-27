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
	    (define-key map (kbd "C-# c") 'org-kasten-create-child-note)
            (define-key map (kbd "C-# p") 'org-kasten-navigate-parent)
	    (define-key map (kbd "C-# n") 'org-kasten-navigate-children)
            map))

(if org-kasten-quick-nav
    (progn
      (define-key org-kasten-mode-map (kbd "C-<") 'org-kasten-navigate-parent)
      (define-key org-kasten-mode-map (kbd "C->") 'org-kasten-navigate-children)))

(defun org-kasten--file->id (id)
  "Trim the filepath off of an ID, leaving only the alphanumeric identifier."
  (declare (pure t) (side-effect-free t))
  (s-chop-suffix ".org" (s-chop-prefix org-kasten-home id)))

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
         (raw-string (with-temp-buffer
                       (insert note-id " - ")
                       (insert-file-contents (concat org-kasten-home note ".org") nil 0 250)
                       (buffer-string)))
         (bulk-replaced (s-replace-all '(("\n" . "") ("#+STARTUP: showall\n" . "")) raw-string)))
    (s-replace-regexp "\\#\\+LINKS\\: [[:alnum:]]+\n" "" bulk-replaced)))

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
  (let ((string (buffer-substring-no-properties (point-min) (point-max))))
    (save-match-data
      (let ((regexp "^#\\+\\(\[a-zA-Z\]+\\): \\(.*\\)")
	    (pos 0)
	    (matches '()))
        (while (string-match regexp string pos)
	  (if (string= (match-string 2 string) "nil")
	      (push `(,(match-string 1 string) . "") matches)
	    (push `(,(match-string 1 string) . ,(match-string 2 string)) matches))
	  (setq pos (match-end 0)))
        matches))))

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
         (links (cdr (assoc "LINKS" props))))
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
      (find-file (org-kasten--preview->note chosen-file)))))

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
