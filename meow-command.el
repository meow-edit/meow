;;; meow-commands.el --- Commands in Meow
;;; -*- lexical-binding: t -*-

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


;;; Commentary:
;; Implementation for all commands in Meow.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'subr-x)
(require 'seq)

(require 'meow-var)
(require 'meow-util)
(require 'array)

(defun meow--execute-kbd-macro (kbd-macro)
  "Execute KBD-MACRO."
  (when-let ((cmd (key-binding (read-kbd-macro kbd-macro))))
    (call-interactively cmd)))

(defun meow--pop-selection ()
  "Pop a selection from variable `meow--selection-history' and activate."
  (when meow--selection-history
    (let ((sel (pop meow--selection-history)))
      (meow--select-without-history sel))))

(defun meow--make-selection (type mark pos)
  "Make a selection with TYPE, MARK and POS.

The direction of selection is MARK -> POS."
  (list type pos mark))

(defun meow--select (selection)
  "Mark the SELECTION."
  (unless (region-active-p)
    (meow--cancel-selection))
  (-let (((sel-type pos mark) selection))
    (if meow--selection
        (unless (equal meow--selection (car meow--selection-history))
          (push meow--selection meow--selection-history))
      ;; Used to restore the position where we starting selection
      (push (meow--make-selection nil (point) (point))
            meow--selection-history))
    (goto-char pos)
    (when sel-type
      (push-mark mark t t)
      (setq meow--selection selection)))
  (force-mode-line-update))

(defun meow--select-without-history (selection)
  "Mark the SELECTION without record it in `meow--selection-history'."
  (-let (((sel-type point mark) selection))
    (goto-char point)
    (if (not sel-type)
        (progn
          (deactivate-mark)
          (message "No previous selection.")
          (meow--cancel-selection))
      (push-mark mark t t)
      (setq meow--selection selection))))

(defun meow--cancel-selection ()
  "Cancel current selection, clear selection history and deactivate the mark."
  (setq meow--selection-history nil
        meow--selection nil)
  (deactivate-mark t))

(defun meow-undo ()
  "Undo selection or buffer change.

Normal undo when there's no selection, otherwise undo the selection."
  (interactive)
  (meow--execute-kbd-macro meow--kbd-undo))

(defun meow-last-selection (arg)
  (interactive "P")
  (if (meow--with-universal-argument-p arg)
      (while (meow--pop-selection))
    (meow--pop-selection)))

;;; Words Navigation/Selection

(defun meow-mark-or-backward-word (arg)
  "Mark current word or backward word.

Use with universal argument to backward to the begin of current symbol."
  (interactive "P")
  (when (meow--with-universal-argument-p arg)
    (meow--cancel-selection))
  (let ((result
         (or
          (unless (region-active-p)
            (meow--bounds-with-type 'word-mark 'word))

          (when (eq 'word-expand (meow--selection-type))
            ;; We must at the right edge of the word.
            ;; So we backward one char to fix wrong detection on current word.
            (forward-word -1)
            (meow--bounds-with-type 'word-mark 'word))

          (when (region-active-p)
            (save-mark-and-excursion
              (let ((orig-pos (point)))
                (forward-word -1)
                (unless (= orig-pos (point))
                  (meow--bounds-with-type 'word-mark 'word)))))

          (save-mark-and-excursion
            (let ((orig-pos (point)))
              (forward-word -1)
              (unless (= orig-pos (point))
                (meow--bounds-with-type 'word-mark 'word)))))))
    (if (not result)
        (message "Can't mark word!")
      (let ((type (car result))
            (mark (cddr result))
            (pos (cadr result)))
        (-> (meow--make-selection type mark pos)
            (meow--select))
        (when (meow--with-universal-argument-p arg)
          ;; Fix wrong symbol detection.
          (forward-char 1)
          (re-search-forward "\\_<" nil t -1))))))


(defun meow-forward-word (arg)
  "Activate word selection, select current or next word.

Use with universal argument to forward to the end of current symbol."
  (interactive "P")
  (when (meow--with-universal-argument-p arg)
    (meow--cancel-selection))
  ;; result is (mark . pos) or nil
  (let ((result
         (or
          (unless (region-active-p)
            (meow--bounds-with-type 'word 'word))

          (when (eq 'word-mark (meow--selection-type))
            (save-mark-and-excursion
              (let ((mark (point)))
                (forward-word (if (meow--direction-backward-p) 2 -2))
                (cons 'word-expand (cons mark (point))))))

          (when (eq 'word-expand (meow--selection-type))
            (save-mark-and-excursion
              (let ((mark (mark)))
                (forward-word (if (meow--direction-backward-p) -1 1))
                (cons 'word-expand (cons mark (point))))))

          (save-mark-and-excursion
            (let ((orig-pos (point)))
              (forward-word (if (meow--direction-backward-p) -1 1))
              (unless (= orig-pos (point))
                ;; Fix behaviour in camelCase with subword-mode
                (backward-char 1)
                (meow--bounds-with-type 'word 'word)))))))
    (if (not result)
        (message "Can't forward word!")
      (let ((type (car result))
            (mark (cadr result))
            (pos (cddr result)))
        (-> (meow--make-selection type mark pos)
            (meow--select))
        (when (meow--with-universal-argument-p arg)
          (if (meow--direction-backward-p)
              (re-search-forward "\\_<" nil t -1)
            (re-search-forward "\\_>" nil t 1)))))))

;;; Single Char Navigation/Selection

(defun meow-head (arg)
  "Move towards to the head of this line.

Will cancel all other selection, except char selection.

Use with universal argument to move to beginning of line.
Use with numeric argument to move multiple chars at once."
  (interactive "P")
  (unless (eq (meow--selection-type) 'char)
    (meow--cancel-selection))
  (cond
   ((meow--with-universal-argument-p arg)
    (goto-char (line-beginning-position)))
   (t
    (let ((count (prefix-numeric-value arg))
          (bound (line-beginning-position)))
      (backward-char count)
      (when (< (point) bound)
        (goto-char bound))))))

(defun meow-tail (arg)
  "Move towards to the end of this line.

Will cancel all other selection, except char selection.

Use with universal argument to move to beginning of line.
Use with numeric argument to move multiple chars at once."
  (interactive "P")
  (unless (eq (meow--selection-type) 'char)
    (meow--cancel-selection))
  (cond
   ((meow--with-universal-argument-p arg)
    (goto-char (line-end-position)))
   (t
    (let ((count (prefix-numeric-value arg))
          (bound (line-end-position)))
      (forward-char count)
      (when (> (point) bound)
        (goto-char bound))))))

;;; Char navigation/selection

(defun meow-head-select (arg)
  "Activate char selection, then move towards to the head of this line.

See `meow-head' for how prefix arguments work."
  (interactive "P")
  (if (region-active-p)
      (-> (meow--make-selection 'char (mark) (point))
        (meow--select))
    (-> (meow--make-selection 'char (point) (point))
          (meow--select)))
  (cond
   ((meow--with-universal-argument-p arg)
    (goto-char (line-beginning-position)))
   (t
    (let ((count (prefix-numeric-value arg))
          (bound (line-beginning-position)))
      (backward-char count)
      (when (< (point) bound)
        (goto-char bound))))))

(defun meow-tail-select (arg)
  "Activate char selection, then move towards to the end of this line.

See `meow-tail' for how prefix arguments work."
  (interactive "P")
  (if (region-active-p)
      (-> (meow--make-selection 'char (mark) (point))
        (meow--select))
    (-> (meow--make-selection 'char (point) (point))
          (meow--select)))
  (cond
   ((meow--with-universal-argument-p arg)
    (goto-char (line-end-position)))
   (t
    (let ((count (prefix-numeric-value arg))
          (bound (line-end-position)))
      (forward-char count)
      (when (> (point) bound)
        (goto-char bound))))))

;;; Line navigation/selection

(defun meow-prev-line (arg)
  "Move to the previous line.

Will cancel all other selection, except char selection.

Use with universal argument to move to the first line of buffer.
Use with numeric argument to move multiple lines at once."
  (interactive "P")
  (unless (eq (meow--selection-type) 'char)
    (meow--cancel-selection))
  (cond
   ((meow--with-universal-argument-p arg)
    (goto-char (point-min)))
   (t
    (let ((count (prefix-numeric-value arg)))
      (dotimes (i count)
        (call-interactively #'previous-line))))))

(defun meow-next-line (arg)
  "Move to the next line.

Will cancel all other selection, except char selection.

Use with universal argument to move to the last line of buffer.
Use with numeric argument to move multiple lines at once."
  (interactive "P")
  (unless (eq (meow--selection-type) 'char)
    (meow--cancel-selection))
  (cond
   ((meow--with-universal-argument-p arg)
    (goto-char (point-max)))
   (t
    (let ((count (prefix-numeric-value arg)))
      (dotimes (i count)
        (call-interactively #'next-line))))))

;;; line selections

(defun meow-prev-line-select (arg)
  "Activate char selection, then move to previous line.

See `meow-prev-line' for how prefix arguments work."
  (interactive "P")
  (if (region-active-p)
      (-> (meow--make-selection 'char (mark) (point))
        (meow--select))
    (-> (meow--make-selection 'char (point) (point))
          (meow--select)))
  (cond
   ((meow--with-universal-argument-p arg)
    (goto-char (point-min)))
   (t
    (let ((count (prefix-numeric-value arg)))
      (dotimes (i count)
        (call-interactively #'previous-line))))))

(defun meow-next-line-select (arg)
  "Activate char selection, then move to previous line.

See `meow-prev-line' for how prefix arguments work."
  (interactive "P")
  (if (region-active-p)
      (-> (meow--make-selection 'char (mark) (point))
        (meow--select))
    (-> (meow--make-selection 'char (point) (point))
          (meow--select)))
  (cond
   ((meow--with-universal-argument-p arg)
    (goto-char (point-max)))
   (t
    (let ((count (prefix-numeric-value arg)))
      (dotimes (i count)
        (call-interactively #'next-line))))))

;;; List Navigation/Selection

;;; Expression Navigation/Selection

(defun meow--scan-sexps (from count)
  "Like function `scan-sexps' with FROM and COUNT.

Return nil when point has no change.  Wrap with ignore errors."
  (let ((pos (point)))
    (ignore-errors
      (goto-char from)
      (forward-sexp count)
      (unless (= pos (point))
        (point)))))

(defun meow-exp (arg)
  "Mark ARG expressions."
  (interactive "P")
  (-let* ((exchange (< (prefix-numeric-value arg) 0))
          (n (abs (prefix-numeric-value arg)))
          (region-beg (when (region-active-p) (region-beginning)))
          (region-end (when (region-active-p) (region-end)))
          (pos)
          (mark)
          (direction-backward (meow--direction-backward-p)))
    (cond
     ((and (not (eq 'exp (meow--selection-type)))
           (not (eq 'block (meow--selection-type))))
      (-let (((beg . end) (bounds-of-thing-at-point 'sexp)))
        (if exchange
            (save-mark-and-excursion
              (when end (goto-char end))
              (save-mark-and-excursion
                (setq pos (meow--scan-sexps (point) (- n))
                      mark (meow--scan-sexps pos n))))
          (save-mark-and-excursion
            (when beg (goto-char beg))
            (save-mark-and-excursion
              (setq pos (meow--scan-sexps (point) n)
                    mark (meow--scan-sexps pos (- n))))))))

     (direction-backward
      (save-mark-and-excursion
        (setq pos (meow--scan-sexps (point) (- n))
              mark (meow--scan-sexps pos 1))))

     (t
      (save-mark-and-excursion
        (setq pos (meow--scan-sexps (point) n)
              mark (meow--scan-sexps pos -1)))))

    (cond
     ((or (not pos) (not mark))
      (if (eq 'exp (meow--selection-type))
          (exchange-point-and-mark)
        (message "Mark exp failed")))
     (t
      (if exchange
          (-> (meow--make-selection 'exp pos mark)
              (meow--select))
        (-> (meow--make-selection 'exp mark pos)
            (meow--select)))))))

;;; Line Navigation/Selection

(defun meow--indent-of-current-line ()
  (save-mark-and-excursion
    (goto-char (line-beginning-position))
    (skip-syntax-forward " ")))

(defun meow-line (arg)
  "Mark lines.

Use with universal argument, mark all lines with same indentation."
  (interactive "P")
  (if (meow--with-universal-argument-p arg)
      (let ((curr-indent (meow--indent-of-current-line))
            (beg (or (when (eq 'line (meow--selection-type))
                       (region-beginning))
                     (line-beginning-position)))
            end)
        (when curr-indent
          (while (and (not (= (point) (point-max)))
                      (= curr-indent (meow--indent-of-current-line)))
            (setq end (line-end-position))
            (forward-line 1)))
        (when end
          (-> (meow--make-selection 'line beg end)
              (meow--select))))
    (-let* ((exchange (< (prefix-numeric-value arg) 0))
            (n (abs (prefix-numeric-value arg)))
            (direction-backward (meow--direction-backward-p))
            (pos)
            (mark)
            (beg (if (region-active-p) (region-beginning) (line-beginning-position)))
            (end (if (region-active-p) (region-end) (line-end-position))))
      (cond
       ((not (eq 'line (meow--selection-type)))
        (if exchange
            (setq end (line-end-position)
                  beg (save-mark-and-excursion
                        (forward-line (- 1 n))
                        (line-beginning-position)))
          (setq beg (line-beginning-position)
                end (save-mark-and-excursion
                      (forward-line (1- n))
                      (line-end-position)))))

       (direction-backward
        (setq beg (save-mark-and-excursion
                    (goto-char beg)
                    (forward-line (- n))
                    (line-beginning-position))))

       (t
        (setq end (save-mark-and-excursion
                    (goto-char end)
                    (forward-line (- n (if (= end (line-end-position)) 0 1)))
                    (line-end-position)))))

      (when (= end beg)
        (cl-incf end))

      (if (xor direction-backward exchange)
          (setq pos beg
                mark end)
        (setq mark beg
              pos end))
      (-> (meow--make-selection 'line mark pos)
          (meow--select)))))

;;; Mark thing

(defun meow-mark-thing (ch)
  (interactive "cMark:")
  (message (char-to-string ch))
  (-let* ((thing
           (cl-case ch
             (?w 'word)
             (?d 'defun)
             (?l 'line)
             (?b 'list)
             (?s 'symbol)
             (?p 'paragraph)))
          ((beg . end) (bounds-of-thing-at-point thing)))
    (when (and beg end)
      (-> (meow--make-selection 'transient beg end)
          (meow--select)))))

;;; Block Selection/Expanding

(defun meow--block-string-end ()
  "Return the end of string block."
  (save-mark-and-excursion
    (while (and (meow--in-string-p)
                (not (= (point) (point-max))))
      (forward-char))
    (point)))

(defun meow--block-string-beg ()
  "Return the beginning of string block."
  (save-mark-and-excursion
    (while (and (meow--in-string-p)
                (not (= (point) (point-min))))
      (backward-char))
    (point)))

(defun meow--current-block ()
  "Get the current block, the `list' thing in Emacs is not always a list.
So we need our block detection.

Currently, the implementation is simply assume that bounds of list never equal to bounds of symbol."
  (let ((bounds-of-list (bounds-of-thing-at-point 'list))
        (bounds-of-symbol (bounds-of-thing-at-point 'symbol)))
    (unless (equal bounds-of-list bounds-of-symbol)
      bounds-of-list)))

(defun meow--find-block-forward ()
  (let ((open-pos (save-mark-and-excursion (re-search-forward "\\s(" nil t 1)))
        (close-pos (save-mark-and-excursion (re-search-forward "\\s)" nil t 1))))
    (when (and open-pos close-pos (< open-pos close-pos))
      (goto-char open-pos))))

(defun meow--find-block-backward ()
  (let ((open-pos (save-mark-and-excursion (re-search-backward "\\s(" nil t 1)))
        (close-pos (save-mark-and-excursion (re-search-backward "\\s)" nil t 1))))
    (when (and open-pos close-pos (< open-pos close-pos))
      (goto-char close-pos))))

(defun meow--block-mark-defun ()
  (-let (((beg . end) (bounds-of-thing-at-point 'defun)))
    (when beg
      (-> (meow--make-selection 'block beg end)
          (meow--select)))))

(defun meow--block-mark-tag (&optional expand)
  (let ((rbeg (or (when (region-active-p) (region-beginning)) (point)))
        (rend (or (when (region-active-p) (region-end)) (point)))
        beg end err stack special-tag)
    (save-mark-and-excursion
      (when (if expand
                (re-search-forward "\\(?:<\\(/[a-zA-Z0-9]+\\)\\(?: .+\\)?>\\)" nil t)
              (re-search-forward "\\(?:<\\(/[a-zA-Z0-9]+\\)\\(?: .+\\)?>\\|\\(/>\\)\\|\\(%>\\)\\|\\(-->\\)\\)" nil t))
        (cond
         ((match-string 2)
          (setq end (point)
                beg (re-search-backward "<[a-zA-Z0-9]" nil t)
                special-tag t))
         ((match-string 3)
          (setq end (point)
                beg (re-search-backward "<%" nil t)
                special-tag t))
         ((match-string 4)
          (setq end (point)
                beg (search-backward "<!--" nil t)
                special-tag t))
         (t
          (let ((tag (meow--remove-text-properties (match-string 1))))
          (push tag stack)
          (setq end (point))
          (search-backward "<")
          (while (and stack (not err))
            (re-search-backward "<\\(/?[a-zA-Z0-9]+\\)\\(?: .+[^/%-]\\)?>" nil t)
            (let ((tag (meow--remove-text-properties (match-string 1))))
              (if (string-prefix-p "/" tag)
                  (push tag stack)
                (if (string-equal (concat "/" tag) (car stack))
                    (pop stack)
                  (setq err t)))))
          (setq beg (point)))))))
    (when err (message "Found unmatched tag!"))
    (if beg
        (if (or special-tag (not expand) (<= beg rbeg))
            (-> (meow--make-selection 'block beg end)
                (meow--select))
          (goto-char end)
          (set-mark rbeg)
          (meow--block-mark-tag t))
      (message "Mark block failed"))))

(defun meow--block-mark-list (arg)
  (let* ((ra (and (eq 'block (meow--selection-type))))
         (neg (or (< (prefix-numeric-value arg) 0) (meow--direction-backward-p)))
         (search-fn (if neg #'re-search-backward #'re-search-forward))
         (m (if neg 1 2))
         (fix-pos (if neg 1 -1))
         beg end)
    (save-mark-and-excursion
      (unless ra
        (while (when (funcall search-fn "\\(\\s(\\)\\|\\(\\s)\\)" nil t)
                 (meow--in-string-p)))
        (when (match-string m)
          (forward-char fix-pos)))
      (-let ((bounds (bounds-of-thing-at-point 'list)))
        (setq beg (car bounds)
              end (cdr bounds))))
    (if (and beg end)
        (-> (meow--make-selection 'block (if neg end beg) (if neg beg end))
            (meow--select))
      (message "Mark block failed"))))

(defun meow-block (arg)
  "Mark the block or expand to parent block."
  (interactive "P")
  (cond
   ((meow--with-universal-argument-p arg)
    (meow--block-mark-defun))
   ((and (derived-mode-p 'web-mode)
         (not (eq nil (get-text-property (point) 'part-side))))
    (meow--block-mark-list arg))
   ((derived-mode-p 'web-mode 'html-mode 'mhtml-mode)
    (meow--block-mark-tag (region-active-p)))
   (t (meow--block-mark-list arg))))

;;; Mark string.

(defun meow--current-select-kind ()
  (when-let ((sel-type (meow--selection-type)))
    (let ((sel-name (symbol-name sel-type)))
      (when (or (string-prefix-p "inter-" sel-name)
                (string-prefix-p "outer-" sel-name))
        (intern (substring sel-name 6))))))

(defun meow--select-common (open-re close-re kind bound)
  "Common implementation for mark the bounds of block.

RE is the regexp for search.
kind can be one of 'paren, 'brace, 'bracket.
bound can be nil: mark both bounds, 'close: mark the close bound, 'open: mark the open bound."
  (or (meow--toggle-inner-outer kind)
      (let ((orig-pos (point))
            (re (if (eq bound 'open)
                    (concat open-re "\\|" close-re)
                  (concat close-re "\\|" open-re)))
            (cnt 0) found mark pos
            (search-fn (if (eq bound 'open) #'re-search-backward #'re-search-forward))
            (select-fn (if (eq bound 'open) #'forward-sexp #'backward-sexp)))
        (save-mark-and-excursion
          (while (and (not found) (funcall search-fn re nil t))
            (cond
             ((meow--in-string-p))
             ((match-string 1)
              (if (zerop cnt) (setq found (point)) (setq cnt (1- cnt))))
             ((match-string 2)
              (setq cnt (1+ cnt)))))
          (when found
            (setq pos (point)
                  mark (if (not (eq bound 'both))
                           orig-pos
                         (progn (funcall select-fn 1) (point))))))
        (when (and pos mark)
          (let ((beg (min mark pos))
                (end (max mark pos)))
            (-> (meow--make-selection (intern (concat "inner-"
                                                      (symbol-name kind)
                                                      "-"
                                                      (symbol-name bound)))
                                      (if (eq bound 'close) beg (1+ beg))
                                      (if (eq bound 'open) end (1- end)))
                (meow--select)))))))

(defun meow--toggle-inner-outer (tgl-kind)
  (when-let ((sel-type (meow--selection-type)))
    (-let (((type kind bound) (mapcar #'intern (split-string (symbol-name sel-type) "-")))
           ((beg . end) (car (region-bounds))))
      (when (eq kind tgl-kind)
        (cond
         ((eq 'inner type)
          (-> (meow--make-selection (intern (concat "outer-"
                                                    (symbol-name kind)
                                                    "-"
                                                    (symbol-name bound)))
                                    (if (eq bound 'close) beg (1- beg))
                                    (if (eq bound 'open) end (1+ end)))
              (meow--select-without-history))
          t)
         ((eq 'outer type)
          (-> (meow--make-selection (intern (concat "inner-"
                                                    (symbol-name kind)
                                                    "-"
                                                    (symbol-name bound)))
                                    (if (eq bound 'close) beg (1+ beg))
                                    (if (eq bound 'open) end (1- end)))
              (meow--select-without-history))
          t)
         (t nil))))))

(defun meow-select-string (arg)
  (interactive "P")
  (or (meow--toggle-inner-outer 'string)
      (when (meow--in-string-p)
        (let* ((bound (cond ((meow--with-negative-argument-p arg) 'open)
                            ((meow--with-universal-argument-p arg) 'both)
                            (t 'close)))
               (end (if (eq bound 'open) (point) (meow--block-string-end)))
               (beg (if (eq bound 'close) (point) (meow--block-string-beg))))
          (-> (meow--make-selection
               (intern (concat "outer-string-" (symbol-name bound)))
               beg
               end)
              (meow--select))))))

(defun meow-select-paren (arg)
  (interactive "P")
  (cond
   ((meow--with-universal-argument-p arg)
    (meow--select-common "\\((\\)" "\\()\\)" 'paren 'both))
   ((meow--with-negative-argument-p arg)
    (meow--select-common "\\((\\)" "\\()\\)" 'paren 'open))
   (t
    (meow--select-common "\\((\\)" "\\()\\)" 'paren 'close))))

(defun meow-select-bracket (arg)
  (interactive "P")
  (cond
   ((meow--with-universal-argument-p arg)
    (meow--select-common "\\(\\[\\)" "\\(\\]\\)" 'bracket 'both))
   ((meow--with-negative-argument-p arg)
    (meow--select-common "\\(\\[\\)" "\\(\\]\\)" 'bracket 'open))
   (t
    (meow--select-common "\\(\\[\\)" "\\(\\]\\)" 'bracket 'close))))

(defun meow-select-brace (arg)
  (interactive "P")
  (cond
   ((meow--with-universal-argument-p arg)
    (meow--select-common "\\({\\)" "\\(}\\)" 'brace 'both))
   ((meow--with-negative-argument-p arg)
    (meow--select-common "\\({\\)" "\\(}\\)" 'brace 'open))
   (t
    (meow--select-common "\\({\\)" "\\(}\\)" 'brace 'close))))

;;; exchange mark and point

(defun meow-reverse ()
  "Just exchange point and mark."
  (interactive)
  (when (region-active-p)
    (exchange-point-and-mark))
  (force-mode-line-update))

;;; Flip

(defun meow--flip-begin-of-comment ()
  "Mark to the begin of current comment."
  (->> (save-mark-and-excursion
         (1- (re-search-backward "\\s<" nil t 1)))
       (meow--make-selection 'flip-backward (point))
       (meow--select)))

(defun meow--flip-end-of-comment ()
  "Mark to the end of current comment."
  (->> (save-mark-and-excursion
         (1- (re-search-forward "\\s>" nil t 1)))
       (meow--make-selection 'flip-forward (point))
       (meow--select)))

(defun meow--flip-begin-of-string ()
  "Mark to the begin of current string."
  (->> (save-mark-and-excursion
         (while (and (meow--in-string-p) (> (point) (point-min)))
           (backward-char 1))
         (1+ (point)))
       (meow--make-selection 'flip-backward (point))
       (meow--select)))

(defun meow--flip-end-of-string ()
  "Mark to the end of current string."
  (->> (save-mark-and-excursion
         (while (and (meow--in-string-p) (< (point) (point-max)))
           (forward-char 1))
         (1- (point)))
       (meow--make-selection 'flip-forward (point))
       (meow--select)))

(defun meow--flip-begin ()
  "Mark to the begin of current block or line."
  (->> (save-mark-and-excursion
         (let ((min (line-beginning-position))
               (ret (point))
               (continue t))
           (while continue
             (unless (meow--scan-sexps ret -1) (setq continue nil))
             (-let (((_ . end) (bounds-of-thing-at-point 'sexp)))
               (if (and end (>= end min))
                   (setq ret (point))
                 (setq continue nil))))
           ret))
       (meow--make-selection 'flip-backward (point))
       (meow--select)))

(defun meow--flip-end ()
  "Mark to the end of current block or line."
  (->> (save-mark-and-excursion
         (let ((max (line-end-position))
               (ret (point))
               (continue t))
           (while continue
             ;; If no more sexp
             (unless (meow--scan-sexps ret 1) (setq continue nil))
             (-let (((beg . _) (bounds-of-thing-at-point 'sexp)))
               (if (and beg (<= beg max))
                   (setq ret (point))
                 (setq continue nil))))
           ret))
       (meow--make-selection 'flip-forward (point))
       (meow--select)))

(defun meow-flip ()
  "Mark to the end of line(or block) or begin of line(or block)."
  (interactive)
  (let ((sel-type (meow--selection-type)))
    (when (member sel-type '(flip-backward flip-forward))
      (exchange-point-and-mark))
    (cond
     ((meow--in-comment-p)
      (if (eq 'flip-forward sel-type)
          (meow--flip-begin-of-comment)
        (meow--flip-end-of-comment)))
     ((meow--in-string-p)
      (if (eq 'flip-forward sel-type)
          (meow--flip-begin-of-string)
        (meow--flip-end-of-string)))
     (t
      (if (eq 'flip-forward sel-type)
          (meow--flip-begin)
        (meow--flip-end))))))

;;; Buffer

(defun meow-begin-of-buffer ()
  "Mark from current point, to the beginning of buffer with char selection."
  (interactive)
  (-> (meow--make-selection 'transient (point) (point-min))
      (meow--select)))

(defun meow-end-of-buffer ()
  "Mark from current point, to the end of buffer with char selection."
  (interactive)
  (-> (meow--make-selection 'transient (point) (point-max))
      (meow--select)))

(defun meow-find (arg)
  "Mark current position to a position of a specified character."
  (interactive "P")
  (let* ((ch (read-char "Find:"))
         (ch-str (if (eq ch 13) "\n" (char-to-string ch)))
         (n (prefix-numeric-value arg))
         (beg (if (eq (meow--selection-type) 'char)
                  (mark)
                (point)))
         (fix-pos (if (< n 0) 1 -1))
         end)
    (save-mark-and-excursion
      (if (> n 0) (forward-char 1) (forward-char -1))
      (setq end (search-forward ch-str nil t n)))
    (if end
        (progn (-> (meow--make-selection 'char beg (+ end fix-pos))
                   (meow--select))
               (funcall fix-pos))
      (message "character %s not found" ch-str))))

(defun meow-find-repeat (arg)
  (interactive "P")
  (when (and (region-active-p) (eq 'char (meow--selection-type)))
    (-let* ((ch (if (meow--direction-backward-p)
                    (char-before)
                  (char-after)))
            (ch-str (when ch (if (eq ch 13) "\n" (char-to-string ch))))
            (n (* (prefix-numeric-value arg) (if (meow--direction-backward-p) -1 1)))
            (fix-pos (if (< n 0) 1 -1))
            pos)
      (when ch-str
        (save-mark-and-excursion
          (forward-char (if (meow--direction-backward-p) -1 1))
          (when (search-forward ch-str nil t n)
            (setq pos (point))))
        (when pos
          (-> (meow--make-selection 'char (mark) (+ pos fix-pos))
              (meow--select)))))))

(defun meow-find-ref ()
  "Xref find."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-find-ref))

(defun meow-pop-marker ()
  "Pop marker."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-pop-marker))

(defun meow-last-pos ()
  (interactive)
  (meow--cancel-selection)
  (when meow--position-history
    (let ((pos (pop meow--position-history)))
      (goto-char pos))))

;;; Clipboards

(defun meow-copy ()
  "Copy, like command `kill-ring-save'."
  (interactive)
  (if (region-active-p)
      (if (eq 'line (meow--selection-type))
          (progn
            (when (and (not (meow--direction-backward-p))
                       (< (point) (point-max)))
              (forward-char 1))
            (meow--execute-kbd-macro meow--kbd-kill-ring-save))
        (meow--execute-kbd-macro meow--kbd-kill-ring-save))
    (meow--selection-fallback)))

(defun meow-yank (arg)
  "Yank.

Use universal argument for exchange yank.
Use negative argument for overwrite yank.
"
  (interactive "P")
  (cond
   ((meow--with-universal-argument-p arg)
    (when (region-active-p)
      (let ((text (pop kill-ring)))
        (kill-region (region-beginning) (region-end))
        (insert text))))
   ((meow--with-negative-argument-p arg)
    (when (region-active-p)
      (delete-region (region-beginning) (region-end))
      (yank)))
   (t (meow--execute-kbd-macro meow--kbd-yank))))

(defun meow-yank-pop ()
  "Pop yank."
  (interactive)
  (meow--execute-kbd-macro meow--kbd-yank-pop))

;;; Quit

(defun meow-keyboard-quit ()
  "Keyboard quit."
  (interactive)
  (if (region-active-p)
      (deactivate-mark t)
    (meow--execute-kbd-macro meow--kbd-keyboard-quit)))

(defun meow-quit ()
  "Quit current window or buffer."
  (interactive)
  (if (> (seq-length (window-list (selected-frame))) 1)
    (delete-window)
    (previous-buffer)))

;;; Comment

(defun meow-comment ()
  "Comment region or comment line."
  (interactive)
  (meow--execute-kbd-macro meow--kbd-comment))

;;; Delete Operations

(defun meow-kill ()
  "Kill region or kill line."
  (interactive)
  (if (not (region-active-p))
      (meow--execute-kbd-macro meow--kbd-kill-line)
    ;; Kill whole line include eol when current selection is a line selection.
    (if (eq 'line (meow--selection-type))
        (progn
          (when (and (not (meow--direction-backward-p))
                     (< (point) (point-max)))
            (forward-char 1))
          (meow--execute-kbd-macro meow--kbd-kill-region))
        (meow--execute-kbd-macro meow--kbd-kill-region))))

(defun meow-join ()
  "Join current line to the previous line.

Known as built-in command `delete-indentation'."
  (interactive)
  (meow--execute-kbd-macro meow--kbd-delete-indentation))

(defun meow-delete ()
  "Forward delete one char."
  (interactive)
  (meow--execute-kbd-macro meow--kbd-delete-char))

(defun meow-zap ()
  "Delete selection, and shrink multiple spaces into one.
If using without selection, toggle the number of spaces between one/zero."
  (interactive)
  (meow--cancel-selection)
  (let ((cnt 0))
    (save-mark-and-excursion
      (while (equal 32 (char-before))
        (cl-incf cnt)
        (backward-char)))
    (save-mark-and-excursion
      (while (equal 32 (char-after))
        (cl-incf cnt)
        (forward-char)))
    (cond
     ((>= cnt 2) (meow--execute-kbd-macro meow--kbd-just-one-space))
     ((zerop cnt) (meow--execute-kbd-macro meow--kbd-just-one-space))
     ((equal 32 (char-before)) (backward-delete-char 1))
     ((equal 32 (char-after)) (delete-char 1)))))

;;; Toggle Modal State

(defun meow-insert ()
  "Move to the begin of selection, switch to INSERT state."
  (interactive)
  (meow--direction-backward)
  (meow--switch-state 'insert))

(defun meow-append ()
  "Move to the end of selection, switch to INSERT state."
  (interactive)
  (meow--direction-forward)
  (meow--switch-state 'insert))

(defun meow-open-above ()
  "Open a newline above and switch to INSERT state."
  (interactive)
  (goto-char (line-beginning-position))
  (save-mark-and-excursion
    (insert "\n"))
  (indent-for-tab-command)
  (meow--switch-state 'insert))

(defun meow-open ()
  "Open a newline below and switch to INSERT state."
  (interactive)
  (goto-char (line-end-position))
  (newline-and-indent)
  (meow--switch-state 'insert))

(defun meow-change ()
  "Kill current selection and switch to INSERT state."
  (interactive)
  (if (not (region-active-p))
      (meow--selection-fallback)
    (meow--execute-kbd-macro meow--kbd-kill-region)
    (meow--switch-state 'insert)))

(defun meow-replace ()
  "Replace current selection with yank."
  (interactive)
  (if (not (region-active-p))
      (meow--selection-fallback)
    (delete-region (region-beginning) (region-end))
    (yank)))

(defun meow-insert-exit ()
  "Switch to NORMAL state."
  (interactive)
  (cond
   ((meow-keypad-mode-p)
    (meow--exit-keypad-state))
   ((meow-insert-mode-p)
    (when overwrite-mode
      (overwrite-mode -1))
    (meow--switch-state 'normal))))

;;; Pagination

(defun meow-page-up ()
  "Page up."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-scoll-down))

(defun meow-page-down ()
  "Page down."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-scoll-up))

;;; Paren Operations

(defun meow-forward-slurp ()
  "Forward slurp sexp."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-forward-slurp))

(defun meow-backward-slurp ()
  "Backward slurp sexp."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-backward-slurp))

(defun meow-forward-barf ()
  "Forward barf sexp."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-forward-barf))

(defun meow-backward-barf ()
  "Backward barf sexp."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-backward-barf))

(defun meow-raise-sexp ()
  "Raise sexp."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-raise-sexp))

(defun meow-transpose-sexp ()
  "Transpose sexp."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-transpose-sexp))

(defun meow-split-sexp ()
  "Split sexp."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-split-sexp))

(defun meow-join-sexp ()
  "Split sexp."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-join-sexp))

(defun meow-splice-sexp ()
  "Splice sexp."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-splice-sexp))

(defun meow-wrap-round ()
  "Wrap round paren."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-wrap-round))

(defun meow-wrap-square ()
  "Wrap square paren."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-wrap-square))

(defun meow-wrap-curly ()
  "Wrap curly paren."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-wrap-curly))

(defun meow-wrap-string ()
  "Wrap string."
  (interactive)
  (meow--cancel-selection)
  (meow--execute-kbd-macro meow--kbd-wrap-string))


;;; Others

(defun meow-M-x ()
  "Just Meta-x."
  (interactive)
  (meow--execute-kbd-macro meow--kbd-excute-extended-command))

(defun meow-back-to-indentation ()
  "Back to indentation."
  (interactive)
  (meow--execute-kbd-macro meow--kbd-back-to-indentation))

(defun meow-indent ()
  "Indent region or current line."
  (interactive)
  (meow--execute-kbd-macro meow--kbd-indent-region))

(defun meow-search ()
  "Searching for the same text in selection or next visited text."
  (interactive)
  (when (and (not (eq 'visit (meow--selection-type)))
             (region-active-p))
    (setq meow--last-search
          (buffer-substring-no-properties (region-beginning) (region-end))))
  (let ((reverse (meow--direction-backward-p))
        (search meow--last-search))
    (if search
        (if (if reverse
                (re-search-backward search nil t 1)
              (or (re-search-forward search nil t 1)
                  (when (re-search-backward search nil t 1)
                    (setq reverse t))))
            (-let* (((marker-beg marker-end) (match-data))
                    (beg (if reverse (marker-position marker-end) (marker-position marker-beg)))
                    (end (if reverse (marker-position marker-beg) (marker-position marker-end))))
              (-> (meow--make-selection 'visit beg end)
                  (meow--select))
              (if reverse
                  (message "Reverse search: %s" search)
                (message "Search: %s" search)))
          (message "Searching text not found"))
      (message "No search text"))))

(defun meow--visit-point (text reverse)
  "Return the point of text for visit command.
Argument TEXT current search text.
Argument REVERSE if selection is reversed."
  (let ((func (if reverse #'re-search-backward #'re-search-forward))
        (func-2 (if reverse #'re-search-forward #'re-search-backward)))
    (save-mark-and-excursion
      (or (funcall func text nil t 1)
          (funcall func-2 text nil t 1)))))

(defun meow-visit (arg)
  "Mark the search text.
Argument ARG if not nil, reverse the selection when make selection."
  (interactive "P")
  (let* ((reverse arg)
         (pos (point))
         (text (meow--prompt-symbol-and-words
                (if arg "Backward visit: " "Visit: ")
                (point-min) (point-max)))
         (visit-point (meow--visit-point text reverse)))
    (if visit-point
        (-let* (((marker-beg marker-end) (match-data))
                (beg (if (> pos visit-point) (marker-position marker-end) (marker-position marker-beg)))
                (end (if (> pos visit-point) (marker-position marker-beg) (marker-position marker-end))))
          (-> (meow--make-selection 'visit beg end)
              (meow--select))
          (setq meow--last-search text))
      (message "Searching text not found"))))

(defun meow-query-replace (arg)
  "Query-replace.

Argument ARG ignored."
  (interactive "P")
  (if arg
      (meow--execute-kbd-macro meow--kbd-query-replace)
    (meow--execute-kbd-macro meow--kbd-query-replace-regexp)))

(defun meow-last-buffer (arg)
  "Switch to last buffer.
Argument ARG if not nil, switching in a new window."
  (interactive "P")
  (if (not arg)
      (mode-line-other-buffer)
    (split-window)
    (mode-line-other-buffer)))

(defun meow-escape-or-normal-modal ()
  "Keyboard escape quit or switch to normal state."
  (interactive)
  (cond
   ((minibufferp)
    (if (fboundp 'minibuffer-keyboard-quit)
        (call-interactively #'minibuffer-keyboard-quit)
      (call-interactively #'abort-recursive-edit)))
   ((meow-keypad-mode-p)
    (meow--exit-keypad-state))
   ((meow-insert-mode-p)
    (when overwrite-mode
      (overwrite-mode -1))
    (meow--switch-state 'normal))
   ((eq major-mode 'fundamental-mode)
    (meow--switch-state 'normal))))

(defun meow-space ()
  "Execute the original command bound to space."
  (interactive)
  (when meow--space-command (call-interactively meow--space-command)))

(defun meow-eval-last-exp ()
  "Eval last sexp."
  (interactive)
  (meow--execute-kbd-macro meow--kbd-eval-last-exp))

;; Aliases

(provide 'meow-command)

;;; meow-command.el ends here
