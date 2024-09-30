(require 'cl-lib)
(require 'subr-x)
(require 'seq)

(require 'meow-var)
(require 'meow-util)
(require 'meow-visual)
(require 'meow-thing)
(require 'meow-beacon)
;; (require 'meow-keypad)
(require 'array)
(require 'thingatpt)

(defgroup evil-cjk nil
  "CJK support"
  :prefix "evil-cjk-"
  :group 'evil)

(defcustom evil-cjk-emacs-word-boundary nil
  "Determine word boundary exactly the same way as Emacs does."
  :type 'boolean
  :group 'evil-cjk)

(defcustom evil-cjk-word-combining-categories
  '(;; default value in word-combining-categories
    (nil . ?^) (?^ . nil)
    ;; Roman
    (?r . ?k) (?r . ?A) (?r . ?G)
    ;; half-width Katakana
    (?k . ?r) (?k . ?A) (?k . ?G)
    ;; full-width alphanumeric
    (?A . ?r) (?A . ?k) (?A . ?G)
    ;; full-width Greek
    (?G . ?r) (?G . ?k) (?G . ?A)
    )
  "List of pair (cons) of categories to determine word boundary
used in `evil-cjk-word-boundary-p'. See the documentation of
`word-combining-categories'. Use `describe-categories' to see the
list of categories."
  :type '(alist :key-type (choice character (const nil))
                :value-type (choice character (const nil)))
  :group 'evil-cjk)

(defcustom evil-cjk-word-separating-categories
  '(;; Kanji
    (?C . ?H) (?C . ?K) (?C . ?k) (?C . ?A) (?C . ?G)
    ;; Hiragana
    (?H . ?C) (?H . ?K) (?H . ?k) (?H . ?A) (?H . ?G)
    ;; Katakana
    (?K . ?C) (?K . ?H) (?K . ?k) (?K . ?A) (?K . ?G)
    ;; half-width Katakana
    (?k . ?C) (?k . ?H) (?k . ?K) ; (?k . ?A) (?k . ?G)
    ;; full-width alphanumeric
    (?A . ?C) (?A . ?H) (?A . ?K) ; (?A . ?k) (?A . ?G)
    ;; full-width Greek
    (?G . ?C) (?G . ?H) (?G . ?K) ; (?G . ?k) (?G . ?A)
    )
  "List of pair (cons) of categories to determine word boundary
used in `evil-cjk-word-boundary-p'. See the documentation of
`word-separating-categories'. Use `describe-categories' to see
the list of categories."
  :type '(alist :key-type (choice character (const nil))
                :value-type (choice character (const nil)))
  :group 'evil-cjk)

(defvar evil-restriction-stack nil
  "List of previous restrictions.
Using `evil-with-restriction' stores the previous values of
`point-min' and `point-max' as a pair in this list.")

(defmacro evil-with-restriction (beg end &rest body)
  "Execute BODY with the buffer narrowed to BEG and END.
BEG or END may be nil to specify a one-sided restriction."
  (declare (indent 2) (debug t))
  `(save-restriction
     (let ((evil-restriction-stack
            (cons (cons (point-min) (point-max)) evil-restriction-stack)))
       (narrow-to-region (or ,beg (point-min)) (or ,end (point-max)))
       ,@body)))

;;; Thing-at-point motion functions for Evil text objects and motions
(defun forward-evil-empty-line (&optional count)
  "Move forward COUNT empty lines."
  (setq count (or count 1))
  (cond
   ((> count 0)
    (while (and (> count 0) (not (eobp)))
      (when (and (bolp) (eolp))
        (setq count (1- count)))
      (forward-line 1)))
   (t
    (while (and (< count 0) (not (bobp))
                (zerop (forward-line -1)))
      (when (and (bolp) (eolp))
        (setq count (1+ count))))))
  count)

(defmacro evil-motion-loop (spec &rest body)
  "Loop a certain number of times.
Evaluate BODY repeatedly COUNT times with VAR bound to 1 or -1,
depending on the sign of COUNT. Set RESULT, if specified, to the
number of unsuccessful iterations, which is 0 if the loop completes
successfully. This is also the return value.

Each iteration must move point; if point does not change, the loop
immediately quits.

\(fn (VAR COUNT [RESULT]) BODY...)"
  (declare (indent defun)
           (debug ((symbolp form &optional symbolp) body)))
  (let* ((var (or (pop spec) (make-symbol "unitvar")))
         (count (or (pop spec) 0))
         (result (or (pop spec) var))
         (i (make-symbol "loopvar")))
    `(let* ((,i ,count)
            (,var (if (< ,i 0) -1 1)))
       (while (and (/= ,i 0)
                   (/= (point) (progn ,@body (point))))
         (setq ,i (if (< ,i 0) (1+ ,i) (1- ,i))))
       (setq ,result ,i))))

(defun evil-forward-chars (chars &optional count)
  "Move point to the end or beginning of a sequence of CHARS.
CHARS is a character set as inside [...] in a regular expression."
  (let ((notchars (if (= (aref chars 0) ?^)
                      (substring chars 1)
                    (concat "^" chars))))
    (evil-motion-loop (dir (or count 1))
      (cond
       ((< dir 0)
        (skip-chars-backward notchars)
        (skip-chars-backward chars))
       (t
        (skip-chars-forward notchars)
        (skip-chars-forward chars))))))

(defun evil-forward-nearest (count &rest forwards)
  "Move point forward to the first of several motions.
FORWARDS is a list of forward motion functions (i.e. each moves
point forward to the next end of a text object (if passed a +1)
or backward to the preceeding beginning of a text object (if
passed a -1)). This function calls each of these functions once
and moves point to the nearest of the resulting positions. If
COUNT is positive point is moved forward COUNT times, if negative
point is moved backward -COUNT times."
  (evil-motion-loop (dir (or count 1))
    (let ((pnt (point))
          (nxt (if (< dir 0) (point-min) (point-max))))
      (dolist (fwd forwards)
        (goto-char pnt)
        (ignore-errors
          (evil-with-restriction
              (when (< dir 0)
                (save-excursion
                  (goto-char nxt)
                  (line-beginning-position 0)))
              (when (> dir 0)
                (save-excursion
                  (goto-char nxt)
                  (line-end-position 2)))
            (and (zerop (funcall fwd dir))
                 (/= (point) pnt)
                 (if (< dir 0) (> (point) nxt) (< (point) nxt))
                 (setq nxt (point))))))
      (goto-char nxt))))

(defun forward-evil-word (&optional count)
  "Move forward COUNT words.
Moves point COUNT words forward or (- COUNT) words backward if
COUNT is negative.  Point is placed after the end of the word (if
forward) or at the first character of the word (if backward).  A
word is a sequence of word characters matching
\[[:word:]] (recognized by `forward-word'), a sequence of
non-whitespace non-word characters '[^[:word:]\\n\\r\\t\\f ]', or
an empty line matching ^$."
  (evil-forward-nearest
   count
   #'(lambda (&optional cnt)
       (let ((word-separating-categories evil-cjk-word-separating-categories)
             (word-combining-categories evil-cjk-word-combining-categories)
             (pnt (point)))
         (forward-word cnt)
         (if (= pnt (point)) cnt 0)))
   #'(lambda (&optional cnt)
       (evil-forward-chars "^[:word:]\n\r\t\f " cnt))
   #'forward-evil-empty-line))

(defun forward-evil-WORD (&optional count)
  "Move forward COUNT \"WORDS\".
Moves point COUNT WORDS forward or (- COUNT) WORDS backward if
COUNT is negative. Point is placed after the end of the WORD (if
forward) or at the first character of the WORD (if backward). A
WORD is a sequence of non-whitespace characters
'[^\\n\\r\\t\\f ]', or an empty line matching ^$."
  (evil-forward-nearest count
                        #'(lambda (&optional cnt)
                            (evil-forward-chars "^\n\r\t\f " cnt))
                        #'forward-evil-empty-line))

;;; Motion functions
(defun evil-forward-beginning (thing &optional count)
  "Move forward to beginning of THING.
The motion is repeated COUNT times."
  (setq count (or count 1))
  (if (< count 0)
      (forward-thing thing count)
    (let ((bnd (bounds-of-thing-at-point thing))
          rest)
      (when (and bnd (< (point) (cdr bnd)))
        (goto-char (cdr bnd)))
      (ignore-errors
        (when (zerop (setq rest (forward-thing thing count)))
          (when (and (bounds-of-thing-at-point thing)
                     (not (bobp))
                     ;; handle final empty line
                     (not (and (bolp) (eobp))))
            (backward-char))
          (beginning-of-thing thing)))
      rest)))

(defun evil-backward-beginning (thing &optional count)
  "Move backward to beginning of THING.
The motion is repeated COUNT times. This is the same as calling
`evil-backward-beginning' with -COUNT."
  (evil-forward-beginning thing (- (or count 1))))

(defun evil-forward-end (thing &optional count)
  "Move forward to end of THING.
The motion is repeated COUNT times."
  (setq count (or count 1))
  (if (> count 0)
      (progn (unless (eobp) (forward-char))
             (prog1 (forward-thing thing count)
               (unless (bobp) (backward-char))))
    (let ((bnd (bounds-of-thing-at-point thing))
          rest)
      (when (and bnd (< (point) (cdr bnd) ))
        (goto-char (car bnd)))
      (ignore-errors
        (when (zerop (setq rest (forward-thing thing count)))
          (end-of-thing thing)
          (backward-char)))
      rest)))

(defun evil-backward-end (thing &optional count)
  "Move backward to end of THING.
The motion is repeated COUNT times. This is the same as calling
`evil-backward-end' with -COUNT."
  (evil-forward-end thing (- (or count 1))))

(defun evil-forward-word (&optional count)
  "Move by words.
Moves point COUNT words forward or (- COUNT) words backward if
COUNT is negative. This function is the same as `forward-word'
but returns the number of words by which point could *not* be
moved."
  (setq count (or count 1))
  (let* ((dir (if (>= count 0) +1 -1))
         (count (abs count)))
    (while (and (> count 0)
                (forward-word dir))
      (setq count (1- count)))
    count))

(defun meow-move-next-word-start ()
  (interactive)
  (if (region-active-p)
      (progn
	(set-mark (max (point) (mark)))
	(evil-forward-beginning 'evil-word))
    (set-mark (point))
    (evil-forward-beginning 'evil-word))
  ;; (when (and (region-active-p) (> (point) (mark)))
  ;;   (backward-char))
  ;; (when (and (use-region-p) (< (mark) (point)))
  ;;   (forward-char))
  ;; (set-mark (point))
  ;; (let ((p (point)))
  ;;   (evil-forward-beginning 'evil-word)
    ;; (when (= p (- (point) 1))
    ;;   (set-mark (point))
    ;;   (evil-forward-beginning 'evil-word))


    )

(defun meow-move-next-word-end ()
  (interactive)
  (when (and (region-active-p) (> (point) (mark)))
    (backward-char))
  (let ((momentum (and (use-region-p) (< (mark) (point)))))
    (set-mark (point))
    (evil-forward-end 'evil-word 1)
    (when momentum
      (set-mark (+ 1 (mark))))
    (forward-char)))

 ;; To select forward until the next word.
(defun meow-move-prev-word-start ()
  (interactive)
  (when (and (region-active-p) (> (point) (mark)))
    (backward-char))
    (when (and (use-region-p) (< (mark) (point)))
      (forward-char))
  (set-mark (point))
  (backward-word))

;; (defun meow-open-below ()
;;   (interactive)
;;   (meow-insert-newline-below)
;;   (indent-according-to-mode) 
;;   (meow-insert-state 1))

;; (defun meow-copy-selection-on-next-line ()
;;   (interactive)
;;   (mc/create-fake-cursor-at-point)
;;   (next-line)
;;   (multiple-cursors-mode))

(defun meow-collapse-selection ()
  (interactive)
  (when (region-active-p)
    (if (> (point) (mark))
	(progn
	  (deactivate-mark)
	  (backward-char))
        (deactivate-mark))))

(defun meow-delete-selection ()
  (interactive)
  (if (region-active-p)
  
             (kill-region (mark) (point))
             
    (delete-char 1)))

(defun meow-insert-at-line-end ()
  (interactive))

(defun meow-insert-at-line-start ()
  (interactive))

;; (defun meow-open-below ()
;;   (interactive))

;; (defun meow-open-above ()
;;   (interactive))

;; (defun meow-append-mode ()	    
;;   (interactive))

;; (defun meow-insert-mode ()
;;   (interactive))

(defun meow-redo ()
  (interactive)
  (redo))

(defun meow-change-selection ()
  (interactive)
  (meow-delete-selection)
  (meow-insert-mode))

(provide 'meow-helix-commands)
