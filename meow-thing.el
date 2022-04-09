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

(require 'meow-var)
(require 'meow-util)

(defun meow--bounds-of-symbol ()
  (when-let (bounds (bounds-of-thing-at-point 'symbol))
    (let ((beg (car bounds))
          (end (cdr bounds)))
      (save-mark-and-excursion
        (goto-char end)
        (if (not (looking-at-p "\\s)"))
            (while (looking-at-p " \\|,")
              (goto-char (cl-incf end)))
          (goto-char beg)
          (while (looking-back " \\|," 1)
            (goto-char (cl-decf beg))))
        (cons beg end)))))

(defun meow--bounds-of-string ()
  "Return the bounds of the string under the cursor.

The thing `string' is not available in Emacs 27.'"
  (if (version< emacs-version "28")
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
          (cons beg end)))
    (bounds-of-thing-at-point 'string)))

(defun meow--inner-of-symbol ()
  (bounds-of-thing-at-point 'symbol))

(defun meow--inner-of-string ()
  (when-let (bounds (meow--bounds-of-string))
    (let ((beg (car bounds))
          (end (cdr bounds)))
      (cons
       (save-mark-and-excursion
         (goto-char beg)
         (skip-syntax-forward "\"")
         (point))
       (save-mark-and-excursion
         (goto-char end)
         (skip-syntax-backward "\"")
         (point))))))

(defun meow--inner-of-window ()
  (cons (window-start) (window-end)))

(defun meow--inner-of-line ()
  (cons (save-mark-and-excursion (back-to-indentation) (point))
        (line-end-position)))

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

(defun meow--thing-syntax-function (syntax)
  (cons
   (save-mark-and-excursion
     (when (use-region-p)
       (goto-char (region-beginning)))
     (skip-syntax-backward (cdr syntax))
     (point))
   (save-mark-and-excursion
     (when (use-region-p)
       (goto-char (region-end)))
     (skip-syntax-forward (cdr syntax))
     (point))))

(defun meow--thing-regexp-function (b-re f-re near)
  (let ((beg (save-mark-and-excursion
               (when (use-region-p)
                 (goto-char (region-beginning)))
               (when (re-search-backward b-re nil t)
                 (if near (match-end 0) (point)))))
        (end (save-mark-and-excursion
               (when (use-region-p)
                 (goto-char (region-end)))
               (when (re-search-forward f-re nil t)
                 (if near (match-beginning 0) (point))))))
    (when (and beg end)
      (cons beg end))))

(defun meow--thing-parse-pair-search (push-token pop-token back near)
  (let* ((search-fn (if back #'re-search-backward #'re-search-forward))
         (match-fn (if back #'match-end #'match-beginning))
         (cmp-fn (if back #'> #'<))
         (push-next-pos nil)
         (pop-next-pos nil)
         (push-pos (save-mark-and-excursion
                     (when (funcall search-fn push-token nil t)
                       (setq push-next-pos (point))
                       (if near (funcall match-fn 0) (point)))))
         (pop-pos (save-mark-and-excursion
                    (when (funcall search-fn pop-token nil t)
                      (setq pop-next-pos (point))
                      (if near (funcall match-fn 0) (point))))))
    (cond
     ((and (not pop-pos) (not push-pos))
      nil)
     ((not pop-pos)
      (goto-char push-next-pos)
      (cons 'push push-pos))
     ((not push-pos)
      (goto-char pop-next-pos)
      (cons 'pop pop-pos))
     ((funcall cmp-fn push-pos pop-pos)
      (goto-char push-next-pos)
      (cons 'push push-pos))
     (t
      (goto-char pop-next-pos)
      (cons 'pop pop-pos)))))

(defun meow--thing-pair-function (push-token pop-token near)
  (let* ((found nil)
         (depth  0)
         (beg (save-mark-and-excursion
                (prog1
                    (let ((case-fold-search nil))
                      (while (and (<= depth 0)
                                  (setq found (meow--thing-parse-pair-search push-token pop-token t near)))
                        (let ((push-or-pop (car found)))
                          (if (eq 'push push-or-pop)
                              (cl-incf depth)
                            (cl-decf depth))))
                      (when (> depth 0) (cdr found)))
                  (setq depth 0
                        found nil))))
         (end (save-mark-and-excursion
                (let ((case-fold-search nil))
                  (while (and (>= depth 0)
                              (setq found (meow--thing-parse-pair-search push-token pop-token nil near)))
                    (let ((push-or-pop (car found)))
                      (if (eq 'push push-or-pop)
                          (cl-incf depth)
                        (cl-decf depth))))
                  (when (< depth 0) (cdr found))))))
    (when (and beg end)
      (cons beg end))))


(defun meow--thing-make-syntax-function (x)
  (lambda () (meow--thing-syntax-function x)))

(defun meow--thing-make-regexp-function (x near)
  (let* ((b-re (cadr x))
         (f-re (caddr x)))
    (lambda () (meow--thing-regexp-function b-re f-re near))))

(defun meow--thing-make-pair-function (x near)
  (let* ((push-token (let ((tokens (cadr x)))
                       (string-join (mapcar #'regexp-quote tokens) "\\|")))
         (pop-token (let ((tokens (caddr x)))
                      (string-join (mapcar #'regexp-quote tokens) "\\|"))))
    (lambda () (meow--thing-pair-function push-token pop-token near))))

(defun meow--thing-parse-multi (xs near)
  (let ((chained-fns (mapcar (lambda (x) (meow--thing-parse x near)) xs)))
    (lambda ()
      (let ((fns chained-fns)
            ret)
        (while (and fns (not ret))
          (setq ret (funcall (car fns))
                fns (cdr fns)))
        ret))))

(defun meow--thing-parse (x near)
  (cond
   ((functionp x)
    x)
   ((symbolp x)
    (lambda () (bounds-of-thing-at-point x)))
   ((equal 'syntax (car x))
    (meow--thing-make-syntax-function x))
   ((equal 'regexp (car x))
    (meow--thing-make-regexp-function x near))
   ((equal 'pair (car x))
    (meow--thing-make-pair-function x near))
   ((listp x)
    (meow--thing-parse-multi x near))
   (t
    (lambda ()
      (message "Meow: THING definition broken")
      (cons (point) (point))))))

(defun meow-thing-register (thing inner bounds)
  "Register a THING with INNER and BOUNDS.

Argument THING should be symbol, which specified in `meow-char-thing-table'.
Argument INNER and BOUNDS support following expressions:

  EXPR ::= FUNCTION | SYMBOL | SYNTAX-EXPR | REGEXP-EXPR
         | PAIRED-EXPR | MULTI-EXPR
  SYNTAX-EXPR ::= (syntax . STRING)
  REGEXP-EXPR ::= (regexp STRING STRING)
  PAIRED-EXPR ::= (pair TOKENS TOKENS)
  MULTI-EXPR ::= (EXPR ...)
  TOKENS ::= (STRING ...)

FUNCTION is a function receives no arguments, return a cons which
  the car is the beginning of thing, and the cdr is the end of
  thing.

SYMBOL is a symbol represent a builtin thing.

  Example: url

    (meow-thing-register 'url 'url 'url)

SYNTAX-EXPR contains a syntax description used by `skip-syntax-forward'

  Example: non-whitespaces

    (meow-thing-register 'non-whitespace
                         '(syntax . \"^-\")
                         '(syntax . \"^-\"))

  You can find the description for syntax in current buffer with
  \\[describe-syntax].

REGEXP-EXPR contains two regexps, the first is used for
  beginning, the second is used for end. For inner/beginning/end
  function, the point of near end of match will be used.  For
  bounds function, the point of far end of match will be used.

  Example: quoted

    (meow-thing-register 'quoted
                         '(regexp \"`\" \"`\\\\|'\")
                         '(regexp \"`\" \"`\\\\|'\"))

PAIR-EXPR contains two string token lists. The tokens in first
  list are used for finding beginning, the tokens in second list
  are used for finding end.  A depth variable will be used while
  searching, thus only matched pair will be found.

  Example: do/end block

    (meow-thing-register 'do/end
                         '(pair (\"do\") (\"end\"))
                         '(pair (\"do\") (\"end\")))"
    (let ((inner-fn (meow--thing-parse inner t))
          (bounds-fn (meow--thing-parse bounds nil)))
      (meow--thing-register thing inner-fn bounds-fn)))

(meow-thing-register 'round
                     '(pair ("(") (")"))
                     '(pair ("(") (")")))

(meow-thing-register 'square
                     '(pair ("[") ("]"))
                     '(pair ("[") ("]")))

(meow-thing-register 'curly
                     '(pair ("{") ("}"))
                     '(pair ("{") ("}")))

(meow-thing-register 'paragraph 'paragraph 'paragraph)

(meow-thing-register 'sentence 'sentence 'sentence)

(meow-thing-register 'buffer 'buffer 'buffer)

(meow-thing-register 'defun 'defun 'defun)

(meow-thing-register 'symbol #'meow--inner-of-symbol #'meow--bounds-of-symbol)

(meow-thing-register 'string #'meow--inner-of-string #'meow--bounds-of-string)

(meow-thing-register 'window #'meow--inner-of-window #'meow--inner-of-window)

(meow-thing-register 'line #'meow--inner-of-line 'line)

(defun meow--parse-inner-of-thing-char (ch)
  (when-let ((ch-to-thing (assoc ch meow-char-thing-table)))
    (meow--parse-range-of-thing (cdr ch-to-thing) t)))

(defun meow--parse-bounds-of-thing-char (ch)
  (when-let ((ch-to-thing (assoc ch meow-char-thing-table)))
    (meow--parse-range-of-thing (cdr ch-to-thing) nil)))

(defun meow--parse-range-of-thing (thing inner)
  "Parse either inner or bounds of THING. If INNER is non-nil then parse inner."
  (when-let (bounds-fn-pair (plist-get meow--thing-registry thing))
    (if inner
        (funcall (car bounds-fn-pair))
      (funcall (cdr bounds-fn-pair)))))

(provide 'meow-thing)
;;; meow-thing.el ends here
