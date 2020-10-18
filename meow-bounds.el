;;; meow-bounds.el --- Calculate bounds in Meow  -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'subr-x)
(require 'dash)

(require 'meow-var)

(defun meow--other-side-of-sexp (pos &optional backward)
  "Return the other side of sexp from POS.

If BACKWARD is non-nil, search backward."
  (when pos
    (save-mark-and-excursion
      (goto-char pos)
      (ignore-errors
        (if backward
            (backward-sexp 1)
          (forward-sexp 1))
        (point)))))

(defun meow--bounds-of-regexp (beg-re)
  "Get the bounds of regexp, located by beg-re."
  (save-mark-and-excursion
    (let ((orig (point))
          beg
          end)
      (while (and (setq beg (re-search-backward beg-re nil t))
                  (setq end (meow--other-side-of-sexp (point)))
                  (<= end orig)))
      (when (and end (> end orig))
        (cons beg end)))))

(defun meow--bounds-of-round-parens ()
  (meow--bounds-of-regexp "("))

(defun meow--bounds-of-square-parens ()
  (meow--bounds-of-regexp "\\["))

(defun meow--bounds-of-brace-parens ()
  (meow--bounds-of-regexp "{"))

(defun meow--bounds-of-symbol ()
  (meow--bounds-of-regexp "\\_<"))

(defun meow--bounds-of-string ()
  (when (meow--in-string-p)
    (let (beg end)
      (save-mark-and-excursion
        (while (meow--in-string-p)
          (backward-char 1))
        (setq beg (point)))
      (save-mark-and-excursion
        (while (meow--in-string-p)
          (forward-char 1))
        (setq end (point)))
      (cons beg end))))

(defun meow--parse-bounds-of-thing-char (ch)
  (when-let ((ch-to-thing (assoc ch meow-char-thing-table)))
    (cl-case (cdr ch-to-thing)
      ((round) (meow--bounds-of-round-parens))
      ((square) (meow--bounds-of-square-parens))
      ((curly) (meow--bounds-of-brace-parens))
      ((symbol) (meow--bounds-of-symbol))
      ((string) (meow--bounds-of-string))
      ((window) (cons (window-start) (window-end)))
      ((buffer) (cons (point-min) (point-max)))
      ((paragraph) (bounds-of-thing-at-point 'paragraph))
      ((line) (bounds-of-thing-at-point 'line))
      ((defun) (bounds-of-thing-at-point 'defun))
      ((tag) nil))))

(defun meow--parse-inner-of-thing-char (ch)
  (when-let ((ch-to-thing (assoc ch meow-char-thing-table)))
    (cl-case (cdr ch-to-thing)
      ((round) (-when-let ((beg . end) (meow--bounds-of-round-parens))
                 (cons (1+ beg) (1- end))))
      ((square) (-when-let ((beg . end) (meow--bounds-of-square-parens))
                 (cons (1+ beg) (1- end))))
      ((curly) (-when-let ((beg . end) (meow--bounds-of-brace-parens))
                 (cons (1+ beg) (1- end))))
      ((symbol) (meow--bounds-of-symbol))
      ((string) (-when-let ((beg . end) (meow--bounds-of-string))
                  (cons
                   (save-mark-and-excursion
                     (goto-char beg)
                     (while (looking-at "\"\\|'")
                       (forward-char 1))
                     (point))
                   (save-mark-and-excursion
                     (goto-char end)
                     (while (looking-back "\"\\|'" 1)
                       (backward-char 1))
                     (point)))))
      ((window) (cons (window-start) (window-end)))
      ((buffer) (cons (point-min) (point-max)))
      ((paragraph) (bounds-of-thing-at-point 'paragraph))
      ((line) (bounds-of-thing-at-point 'line))
      ((defun) (bounds-of-thing-at-point 'defun))
      ((tag) nil))))

(provide 'meow-bounds)
;;; meow-bounds.el ends here
