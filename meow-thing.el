;;; meow-thing.el --- Calculate bounds of thing in Meow  -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(require 'cl-lib)
(require 'subr-x)
(require 'dash)

(require 'meow-var)
(require 'meow-util)

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

(defun meow--bounds-of-curly-parens ()
  (meow--bounds-of-regexp "{"))

(defun meow--bounds-of-symbol ()
  (-when-let ((beg . end) (bounds-of-thing-at-point 'symbol))
    (save-mark-and-excursion
      (goto-char end)
      (if (not (looking-at-p "\\s)"))
          (while (looking-at-p " \\|,")
            (goto-char (cl-incf end)))
        (goto-char beg)
        (while (looking-back " \\|," 1)
          (goto-char (cl-decf beg))))
      (cons beg end))))

(defun meow--bounds-of-line ()
  (bounds-of-thing-at-point 'line))

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

(defun meow--bounds-of-indent ()
  (let ((idt (save-mark-and-excursion
               (back-to-indentation)
               (- (point) (line-beginning-position))))
        (beg (line-beginning-position))
        (end (line-end-position)))
    (save-mark-and-excursion
      (let ((break nil))
        (goto-char beg)
        (while (and (> (point) (point-min)) (not break))
          (forward-line -1)
          (back-to-indentation)
          (let ((this-idt (- (point) (line-beginning-position))))
            (if (or (>= this-idt idt) (meow--empty-line-p))
                (goto-char (setq beg (line-beginning-position)))
              (setq break t))))))
    (save-mark-and-excursion
      (let ((break nil))
        (goto-char end)
        (while (and (< (point) (point-max)) (not break))
          (forward-line 1)
          (back-to-indentation)
          (let ((this-idt (- (point) (line-beginning-position))))
            (if (or (>= this-idt idt) (meow--empty-line-p))
                (goto-char (setq end (line-end-position)))
              (setq break t))))))
    (cons beg end)))

(defun meow--bounds-of-extend ()
  (if (region-active-p)
      (-let (((beg . end) (car (region-bounds))))
        (cons (save-mark-and-excursion
                (goto-char beg)
                (skip-syntax-backward meow-extend-syntax)
                (skip-syntax-backward "-")
                (point))
              (save-mark-and-excursion
                (goto-char end)
                (skip-syntax-forward meow-extend-syntax)
                (skip-syntax-forward "-")
                (point))))
    (cons (save-mark-and-excursion
            (skip-syntax-backward meow-extend-syntax)
            (skip-syntax-backward "-")
            (point))
          (save-mark-and-excursion
            (skip-syntax-forward meow-extend-syntax)
            (skip-syntax-forward "-")
            (point)))))

(defun meow--inner-of-round-parens ()
  (-when-let ((beg . end) (meow--bounds-of-round-parens))
    (cons (1+ beg) (1- end))))

(defun meow--inner-of-square-parens ()
  (-when-let ((beg . end) (meow--bounds-of-square-parens))
    (cons (1+ beg) (1- end))))

(defun meow--inner-of-curly-parens ()
  (-when-let ((beg . end) (meow--bounds-of-curly-parens))
    (cons (1+ beg) (1- end))))

(defun meow--inner-of-symbol ()
  (bounds-of-thing-at-point 'symbol))

(defun meow--inner-of-string ()
  (-when-let ((beg . end) (meow--bounds-of-string))
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

(defun meow--inner-of-window ()
  (cons (window-start) (window-end)))

(defun meow--inner-of-buffer ()
  (cons (point-min) (point-max)))

(defun meow--inner-of-paragraph ()
  (bounds-of-thing-at-point 'paragraph))

(defun meow--inner-of-line ()
  (cons (save-mark-and-excursion (back-to-indentation) (point))
        (line-end-position)))

(defun meow--inner-of-defun ()
  (bounds-of-thing-at-point 'defun))

(defun meow--inner-of-indent ()
  (meow--bounds-of-indent))

(defun meow--inner-of-extend ()
  (if (region-active-p)
      (-let (((beg . end) (car (region-bounds))))
        (cons (save-mark-and-excursion
                (goto-char beg)
                (skip-syntax-backward meow-extend-syntax)
                (point))
              (save-mark-and-excursion
                (goto-char end)
                (skip-syntax-forward meow-extend-syntax)
                (point))))
    (cons (save-mark-and-excursion
            (skip-syntax-backward meow-extend-syntax)
            (point))
          (save-mark-and-excursion
            (skip-syntax-forward meow-extend-syntax)
            (point)))))

;;; Registry

(defvar meow--thing-registry nil
  "Thing registry.

This is a plist mapping from thing to (inner-fn . bounds-fn).
Both inner-fn and bounds-fn returns a cons of (start . end) for that thing.")

(defun meow--thing-register (thing inner-fn bounds-fn)
  "Register INNER-FN and BOUNDS-FN to a THING."
  (setq meow--thing-registry
        (plist-put meow--thing-registry
                   thing
                   (cons inner-fn bounds-fn))))

(meow--thing-register 'round #'meow--inner-of-round-parens #'meow--bounds-of-round-parens)
(meow--thing-register 'square #'meow--inner-of-square-parens #'meow--bounds-of-square-parens)
(meow--thing-register 'curly #'meow--inner-of-curly-parens #'meow--bounds-of-curly-parens)
(meow--thing-register 'symbol #'meow--inner-of-symbol #'meow--bounds-of-symbol)
(meow--thing-register 'string #'meow--inner-of-string #'meow--bounds-of-string)
(meow--thing-register 'window #'meow--inner-of-window #'meow--inner-of-window)
(meow--thing-register 'paragraph #'meow--inner-of-paragraph #'meow--inner-of-paragraph)
(meow--thing-register 'buffer #'meow--inner-of-buffer #'meow--inner-of-buffer)
(meow--thing-register 'line #'meow--inner-of-line #'meow--bounds-of-line)
(meow--thing-register 'indent #'meow--inner-of-indent #'meow--inner-of-indent)
(meow--thing-register 'defun #'meow--inner-of-defun #'meow--inner-of-defun)
(meow--thing-register 'extend #'meow--inner-of-extend #'meow--bounds-of-extend)

(defun meow--parse-inner-of-thing-char (ch)
  (when-let ((ch-to-thing (assoc ch meow-char-thing-table)))
    (when-let ((inner-fn (car (plist-get meow--thing-registry (cdr ch-to-thing)))))
      (funcall inner-fn))))

(defun meow--parse-bounds-of-thing-char (ch)
  (when-let ((ch-to-thing (assoc ch meow-char-thing-table)))
    (when-let ((bounds-fn (cdr (plist-get meow--thing-registry (cdr ch-to-thing)))))
      (funcall bounds-fn))))

(provide 'meow-thing)
;;; meow-thing.el ends here
