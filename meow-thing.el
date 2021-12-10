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

(defun meow--bounds-of-string ()
  (bounds-of-thing-at-point 'string))

(defun meow--bounds-of-buffer ()
  (cons (point-min) (point-max)))

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
       (skip-syntax-forward "\"")
       (point))
     (save-mark-and-excursion
       (goto-char end)
       (skip-syntax-backward "\"")
       (point)))))

(defun meow--inner-of-window ()
  (cons (window-start) (window-end)))

(defun meow--inner-of-line ()
  (cons (save-mark-and-excursion (back-to-indentation) (point))
        (line-end-position)))

(defun meow--inner-of-defun ()
  (bounds-of-thing-at-point 'defun))

(defun meow--inner-of-indent ()
  (meow--bounds-of-indent))

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

(defun meow--thing-parse (x near)
  (cond
   ((functionp x)
    x)
   ((symbolp x)
    (lambda () (bounds-of-thing-at-point x)))
   ((equal 'syntax (car x))
    (lambda ()
      (cons
       (save-mark-and-excursion
         (when (use-region-p)
           (goto-char (region-beginning)))
         (skip-syntax-backward (cdr x))
         (point))
       (save-mark-and-excursion
         (when (use-region-p)
           (goto-char (region-end)))
         (skip-syntax-forward (cdr x))
         (point)))))
   ((equal 'regexp (car x))
    (lambda ()
      (let* ((push-re (cadr x))
            (pop-re (caddr x))
            (search (format "\\(%s\\|%s\\)" push-re pop-re))
            (depth  0))
        (cons
         (save-mark-and-excursion
           (when (use-region-p)
             (goto-char (region-beginning)))
           (prog1
               (let ((case-fold-search nil))
                 (while (and (>= depth 0) (re-search-backward search))
                   (if (string-match-p push-re (match-string 0))
                       (cl-decf depth)
                     (cl-incf depth)))
                 (when (< depth 0) (if near (match-end 0) (point))))
             (setq depth 0)))

         (save-mark-and-excursion
           (when (use-region-p)
             (goto-char (region-end)))
           (let ((case-fold-search nil))
             (while (and (>= depth 0) (re-search-forward search))
               (if (string-match-p push-re (match-string 0))
                   (cl-incf depth)
                 (cl-decf depth)))
             (when (< depth 0) (if near (match-beginning 0) (point)))))))))
   (t
    (lambda ()
      (message "Meow: THING definition broken")
      (cons (point) (point))))))

(defun meow-thing-register (thing inner bounds)
  "Register a THING with INNER and BOUNDS.

Argument THING should be symbol, which specified in `meow-char-thing-table'.
Argument INNER and BOUNDS, can be one of the following
- a function receives no argument, return a cons of beginning and end point.
- a symbol represent a built-in thing
- (syntax . \"<syntax expression>\")
- (regexp \"<backward regexp>\" \"<forward regexp>\"), for detail see example 3.

Examples:
1. Register URL
\(meow-thing-register 'url 'url 'url)

2. Register extend by non-whitespaces
\(meow-thing-register 'non-whitespace '(syntax . \"^-\") '(syntax . \"^-\"))

You can find the description for syntax in current buffer
with command `describe-syntax'.

3. Register extend by do/end block
\(meow-thing-register 'do/end '(regexp \"do\" \"end\") '(regexp \"do\" \"end\"))

The depth variable will be used during the search.  when the previous regexp is found,
depth will be incremented, when another is found, depth will be decremented.  So the
matched do/end can be found together.

For the INNER case, the point of near end will be used.  For the BOUNDS case,
the point of far end will be used.
"
  (let ((inner-fn (meow--thing-parse inner t))
        (bounds-fn (meow--thing-parse bounds nil)))
    (meow--thing-register thing inner-fn bounds-fn)))

(meow-thing-register 'round #'meow--inner-of-round-parens #'meow--bounds-of-round-parens)
(meow-thing-register 'square #'meow--inner-of-square-parens #'meow--bounds-of-square-parens)
(meow-thing-register 'curly #'meow--inner-of-curly-parens #'meow--bounds-of-curly-parens)
(meow-thing-register 'symbol #'meow--inner-of-symbol #'meow--bounds-of-symbol)
(meow-thing-register 'string #'meow--inner-of-string #'meow--bounds-of-string)
(meow-thing-register 'window #'meow--inner-of-window #'meow--inner-of-window)
(meow-thing-register 'paragraph 'paragraph 'paragraph)
(meow-thing-register 'sentence 'sentence 'sentence)
(meow-thing-register 'buffer #'meow--bounds-of-buffer #'meow--bounds-of-buffer)
(meow-thing-register 'line #'meow--inner-of-line 'line)
(meow-thing-register 'defun 'defun 'defun)

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
