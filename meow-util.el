;;; meow-util.el --- Utilities for Meow  -*- lexical-binding: t; -*-

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
;; Utilities for Meow.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'color)

(require 'meow-var)
(require 'meow-keymap)
(require 'meow-face)

;; Modes

(defvar meow-normal-mode)

(declare-function meow--remove-match-highlights "meow-visual")
(declare-function meow--remove-expand-highlights "meow-visual")
(declare-function meow--remove-search-highlight "meow-visual")
(declare-function meow-insert-mode "meow-core")
(declare-function meow-motion-mode "meow-core")
(declare-function meow-normal-mode "meow-core")
(declare-function meow-keypad-mode "meow-core")
(declare-function meow-beacon-mode "meow-core")
(declare-function meow-mode "meow-core")
(declare-function meow--keypad-format-keys "meow-keypad")
(declare-function meow--keypad-format-prefix "meow-keypad")
(declare-function meow-minibuffer-quit "meow-command")

(defun meow--execute-kbd-macro (kbd-macro)
  "Execute KBD-MACRO."
  (when-let ((ret (key-binding (read-kbd-macro kbd-macro))))
    (cond
     ((commandp ret)
      (call-interactively ret))

     ((and (not meow-use-keypad-when-execute-kbd) (keymapp ret))
      (set-transient-map ret nil nil))

     ((and meow-use-keypad-when-execute-kbd (keymapp ret))
      (meow-keypad-start-with kbd-macro)))))

(defun meow-insert-mode-p ()
  "Whether insert mode is enabled."
  (bound-and-true-p meow-insert-mode))

(defun meow-motion-mode-p ()
  "Whether motion mode is enabled."
  (bound-and-true-p meow-motion-mode))

(defun meow-normal-mode-p ()
  "Whether normal mode is enabled."
  (bound-and-true-p meow-normal-mode))

(defun meow-keypad-mode-p ()
  "Whether keypad mode is enabled."
  (bound-and-true-p meow-keypad-mode))

(defun meow-beacon-mode-p ()
  "Whether keypad mode is enabled."
  (bound-and-true-p meow-beacon-mode))

(defun meow--disable-current-state ()
  (when meow--current-state
    (funcall (alist-get meow--current-state meow-state-mode-alist) -1)
    (setq meow--current-state nil)))

(defun meow--read-cursor-face-color (face)
  "Read cursor color from face."
  (let ((f (face-attribute face :inherit)))
    (if (equal 'unspecified f)
        (let ((color (face-attribute face :background)))
          (if (equal 'unspecified color)
              (face-attribute 'default :foreground)
            color))
      (meow--read-cursor-face-color f))))

(defun meow--set-cursor-type (type)
  (if (display-graphic-p)
      (setq cursor-type type)
    (let* ((shape (or (car-safe type) type))
           (param (cond ((eq shape 'bar) "6")
                        ((eq shape 'hbar) "4")
                        (t "2"))))
      (send-string-to-terminal (concat "\e[" param " q")))))

(defun meow--set-cursor-color (face)
  "Set cursor color by face."
  (set-cursor-color (meow--read-cursor-face-color face)))

(defun meow--update-cursor-default ()
  "Set default cursor type and color"
  (meow--set-cursor-type meow-cursor-type-default)
  (meow--set-cursor-color 'meow-unknown-cursor))

(defun meow--update-cursor-insert ()
  "Set insert cursor type and color"
  (meow--set-cursor-type meow-cursor-type-insert)
  (meow--set-cursor-color 'meow-insert-cursor))

(defun meow--update-cursor-normal ()
  "Set normal cursor type and color"
  (if meow-use-cursor-position-hack
      (unless (use-region-p)
        (meow--set-cursor-type meow-cursor-type-normal))
    (meow--set-cursor-type meow-cursor-type-normal))
  (meow--set-cursor-color 'meow-normal-cursor))

(defun meow--update-cursor-motion ()
  "Set motion cursor type and color"
  (meow--set-cursor-type meow-cursor-type-motion)
  (meow--set-cursor-color 'meow-motion-cursor))

(defun meow--update-cursor-beacon ()
  "Set beacon cursor type and color"
  (meow--set-cursor-type meow-cursor-type-beacon)
  (meow--set-cursor-color 'meow-beacon-cursor))

(defun meow--cursor-null-p ()
  "Check if cursor-type is null"
  (null cursor-type))

(defun meow--update-cursor ()
  "Update cursor type according to the current state.

This uses the variable meow-update-cursor-functions-alist, finds the first
item in which the car evaluates to true, and runs the cdr. The last item's car
in the list will always evaluate to true."
  (thread-last meow-update-cursor-functions-alist
    (cl-remove-if-not (lambda (el) (funcall (car el))))
    (cdar)
    (funcall)))

(defun meow--get-state-name (state)
  "Get the name of the current state.

Looks up the state in meow-replace-state-name-list"
  (alist-get state meow-replace-state-name-list))

(defun meow--render-indicator ()
  "Renders a short indicator based on the current state."
  (when (bound-and-true-p meow-global-mode)
    (let* ((state (meow--current-state))
           (state-name (meow--get-state-name state))
           (indicator-face (alist-get state meow-indicator-face-alist)))
      (if state-name
          (propertize
           (format " %s " state-name)
           'face indicator-face)
        ""))))

(defun meow--update-indicator ()
  (let ((indicator (meow--render-indicator)))
    (setq-local meow--indicator indicator)))

(defun meow--state-p (state)
  (funcall (intern (concat "meow-" (symbol-name state) "-mode-p"))))

(defun meow--current-state ()
  meow--current-state)

(defun meow--should-update-display-p ()
  (cl-case meow-update-display-in-macro
    ((t) t)
    ((except-last-macro)
     (or (null executing-kbd-macro)
         (not (equal executing-kbd-macro last-kbd-macro))))
    ((nil)
     (null executing-kbd-macro))))

(defun meow-update-display ()
  (when (meow--should-update-display-p)
    (meow--update-indicator)
    (meow--update-cursor)))

(defun meow--switch-state (state &optional no-hook)
  "Switch to STATE execute 'meow-switch-state-hook unless NO-HOOK is non-nil."
  (unless (eq state (meow--current-state))
    (let ((mode (alist-get state meow-state-mode-alist)))
      (funcall mode 1))
    (unless (bound-and-true-p no-hook)
      (run-hook-with-args 'meow-switch-state-hook state))))

(defun meow--exit-keypad-state ()
  "Exit keypad state."
  (meow-keypad-mode -1)
  (when (and (eq 'beacon meow--keypad-previous-state)
             meow--current-state)
    (meow--beacon-apply-command meow--keypad-this-command))
  (when meow--keypad-previous-state
    (meow--switch-state meow--keypad-previous-state)))

(defun meow--direction-forward ()
  "Make the selection towards forward."
  (when (and (region-active-p) (< (point) (mark)))
    (exchange-point-and-mark)))

(defun meow--direction-backward ()
  "Make the selection towards backward."
  (when (and (region-active-p) (> (point) (mark)))
    (exchange-point-and-mark)))

(defun meow--direction-backward-p ()
  "Return whether we have a backward selection."
  (and (region-active-p)
       (> (mark) (point))))

(defun meow--direction-forward-p ()
  "Return whether we have a forward selection."
  (and (region-active-p)
       (<= (mark) (point))))

(defun meow--selection-type ()
  "Return current selection type."
  (when (region-active-p)
    (car meow--selection)))

(defun meow--in-string-p (&optional pos)
  "Return whether POS or current position is in string."
  (save-mark-and-excursion
    (when pos (goto-char pos))
    (nth 3 (syntax-ppss))))

(defun meow--in-comment-p (&optional pos)
  "Return whether POS or current position is in string."
  (save-mark-and-excursion
    (when pos (goto-char pos))
    (nth 4 (syntax-ppss))))

(defun meow--sum (sequence)
  (seq-reduce #'+ sequence 0))

(defun meow--reduce (fn init sequence)
  (seq-reduce fn sequence init))

(defun meow--string-pad (s len pad &optional start)
  (if (<= len (length s))
      s
    (if start
	(concat (make-string (- len (length s)) pad) s)
      (concat s (make-string (- len (length s)) pad)))))

(defun meow--truncate-string (len s ellipsis)
  (if (> (length s) len)
      (concat (substring s 0 (- len (length ellipsis))) ellipsis)
    s))

(defun meow--string-join (sep s)
  (string-join s sep))

(defun meow--prompt-symbol-and-words (prompt beg end)
  "Completion with PROMPT for symbols and words from BEG to END."
  (let ((completions))
    (save-mark-and-excursion
      (goto-char beg)
      (while (re-search-forward "\\_<\\(\\sw\\|\\s_\\)+\\_>" end t)
        (let ((result (match-string-no-properties 0)))
          (when (>= (length result) meow-visit-collect-min-length)
            (if meow-visit-sanitize-completion
                (push (cons result (format "\\_<%s\\_>" (regexp-quote result))) completions)
              (push (format "\\_<%s\\_>" (regexp-quote result)) completions))))))
    (setq completions (delete-dups completions))
    (let ((selected (completing-read prompt completions nil nil)))
      (if meow-visit-sanitize-completion
          (or (cdr (assoc selected completions))
              (regexp-quote selected))
        selected))))

(defun meow--on-window-state-change (&rest _args)
  "Update cursor style after switching window."
  (meow--update-cursor)
  (meow--update-indicator))

(defun meow--on-exit ()
  (unless (display-graphic-p)
    (send-string-to-terminal "\e[2 q")))

(defun meow--get-indent ()
  "Get indent of current line."
  (save-mark-and-excursion
    (back-to-indentation)
    (- (point) (line-beginning-position))))

(defun meow--empty-line-p ()
  "Whether current line is empty."
  (string-match-p "^ *$" (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position))))

(defun meow--ordinal (n)
  (cl-case n
    ((1) "1st")
    ((2) "2nd")
    ((3) "3rd")
    (t (format "%dth" n))))

(defun meow--allow-modify-p ()
  (and (not buffer-read-only)
       (not meow--temp-normal)))

(defun meow--with-universal-argument-p (arg)
  (equal '(4) arg))

(defun meow--with-negative-argument-p (arg)
  (< (prefix-numeric-value arg) 0))

(defun meow--with-shift-p ()
  (member 'shift last-input-event))

(defun meow--bounds-with-type (type thing)
  (when-let ((bounds (bounds-of-thing-at-point thing)))
    (cons type bounds)))

(defun meow--push-search (search)
  (unless (string-equal search (car regexp-search-ring))
    (add-to-history 'regexp-search-ring search regexp-search-ring-max)))

(defun meow--remove-text-properties (text)
  (set-text-properties 0 (length text) nil text)
  text)

(defun meow--toggle-relative-line-number ()
  (when display-line-numbers
    (if (bound-and-true-p meow-insert-mode)
        (setq display-line-numbers t)
      (setq display-line-numbers 'relative))))

(defun meow--render-char-thing-table ()
  (let* ((ww (frame-width))
         (w 25)
         (col (min 5 (/ ww w))))
    (thread-last
      meow-char-thing-table
      (seq-group-by #'cdr)
      (seq-sort-by #'car #'string-lessp)
      (seq-map-indexed
       (lambda (th-pairs idx)
         (let* ((th (car th-pairs))
                (pairs (cdr th-pairs))
                (pre (thread-last
                       pairs
                       (mapcar (lambda (it) (char-to-string (car it))))
                       (meow--string-join " "))))
           (format "%s%s%s%s"
                   (propertize
                    (meow--string-pad pre 8 32 t)
                     'face 'font-lock-constant-face)
                   (propertize " → " 'face 'font-lock-comment-face)
                   (propertize
                    (meow--string-pad (symbol-name th) 13 32 t)
                     'face 'font-lock-function-name-face)
                   (if (= (1- col) (mod idx col))
                       "\n"
                     " ")))))
      (string-join)
      (string-trim-right))))

(defun meow--transpose-lists (lists)
  (when lists
    (let* ((n (seq-max (mapcar #'length lists)))
           (rst (apply #'list (make-list n ()))))
      (mapc (lambda (l)
              (seq-map-indexed
               (lambda (it idx)
                 (cl-replace rst
                             (list (cons it (nth idx rst)))
                             :start1 idx
                             :end1 (1+ idx)))
               l))
            lists)
      (mapcar #'reverse rst))))

(defun meow--get-event-key (e)
  (if (and (integerp (event-basic-type e))
           (member 'shift (event-modifiers e)))
      (upcase (event-basic-type e))
    (event-basic-type e)))

(defun meow--ensure-visible ()
  (let ((overlays (overlays-at (1- (point))))
        ov expose)
    (while (setq ov (pop overlays))
      (if (and (invisible-p (overlay-get ov 'invisible))
               (setq expose (overlay-get ov 'isearch-open-invisible)))
          (funcall expose ov)))))

(defun meow--minibuffer-setup ()
  (local-set-key (kbd "<escape>") #'meow-minibuffer-quit)
  (setq-local meow-normal-mode nil)
  (when (or (member this-command meow-grab-fill-commands)
            (member meow--keypad-this-command meow-grab-fill-commands))
    (when-let ((s (meow--second-sel-get-string)))
      (insert s))))

(defun meow--parse-string-to-keypad-keys (str)
  (let ((strs (split-string str " ")))
    (thread-last
      strs
      (mapcar
       (lambda (str)
         (cond
          ((string-prefix-p "C-M-" str)
           (cons 'both (substring str 4)))
          ((string-prefix-p "C-" str)
           (cons 'control (substring str 2)))
          ((string-prefix-p "M-" str)
           (cons 'meta (substring str 2)))
          (t
           (cons 'literal str)))))
      (reverse))))

(defun meow--parse-input-event (e)
  (cond
   ((equal e 32)
    "SPC")
   ((characterp e)
    (string e))
   ((equal 'tab e)
    "TAB")
   ((equal 'return e)
    "RET")
   ((equal 'backspace e)
    "DEL")
   ((equal 'escape e)
    "ESC")
   ((symbolp e)
    (format "<%s>" e))
   (t nil)))

(defun meow--save-origin-commands ()
  (cl-loop for key-code being the key-codes of meow-motion-state-keymap do
           (ignore-errors
             (let* ((key (meow--parse-input-event key-code))
                    (cmd (key-binding (kbd key))))
               (when (and (commandp cmd)
                          (not (equal cmd 'undefined)))
                 (let ((rebind-key (concat meow-motion-remap-prefix key)))
                   (local-set-key (kbd rebind-key) cmd)))))))

(defun meow--prepare-region-for-kill ()
  (when (and (equal 'line (cdr (meow--selection-type)))
             (meow--direction-forward-p)
             (< (point) (point-max)))
    (forward-char 1)))

(defun meow--prepare-string-for-kill-append (s)
  (let ((curr (current-kill 0 nil)))
    (cl-case (cdr (meow--selection-type))
      ((line) (concat (unless (string-suffix-p "\n" curr) "\n")
                      (string-trim-right s "\n")))
      ((word block) (concat (unless (string-suffix-p " " curr) " ")
                            (string-trim s " " "\n")))
      (t s))))

(defun meow--event-key (e)
  (let ((c (event-basic-type e)))
    (if (and (char-or-string-p c)
             (member 'shift (event-modifiers e)))
        (upcase c)
      c)))

(defun meow--parse-def (def)
  "Return a command or keymap for DEF.

If DEF is a string, return a command that calls the command or keymap
that bound to DEF. Otherwise, return DEF."
  (if (stringp def)
      (let ((cmd-name (gensym 'meow-dispatch_)))
        ;; dispatch command
        (defalias cmd-name
          (lambda ()
            (:documentation
             (format "Execute the command which is bound to %s." def))
            (interactive)
            (meow--execute-kbd-macro def)))
        (put cmd-name 'meow-dispatch def)
        cmd-name)
    def))

(defun meow--second-sel-set-string (string)
  (cond
   ((meow--second-sel-buffer)
    (with-current-buffer (overlay-buffer mouse-secondary-overlay)
      (goto-char (overlay-start mouse-secondary-overlay))
      (delete-region (overlay-start mouse-secondary-overlay) (overlay-end mouse-secondary-overlay))
      (insert string)))
   ((markerp mouse-secondary-start)
    (with-current-buffer (marker-buffer mouse-secondary-start)
      (goto-char (marker-position mouse-secondary-start))
      (insert string)))))

(defun meow--second-sel-get-string ()
  (when (meow--second-sel-buffer)
    (with-current-buffer (overlay-buffer mouse-secondary-overlay)
      (buffer-substring-no-properties
       (overlay-start mouse-secondary-overlay)
       (overlay-end mouse-secondary-overlay)))))

(defun meow--second-sel-buffer ()
  (and (overlayp mouse-secondary-overlay)
       (overlay-buffer mouse-secondary-overlay)))

(defun meow--second-sel-bound ()
  (and (secondary-selection-exist-p)
       (cons (overlay-start mouse-secondary-overlay)
             (overlay-end mouse-secondary-overlay))))

(defmacro meow--with-selection-fallback (&rest body)
  `(if (region-active-p)
       (progn ,@body)
     (meow--selection-fallback)))

(defmacro meow--wrap-collapse-undo (&rest body)
  "Like `progn' but perform BODY with undo collapsed."
  (declare (indent 0) (debug t))
  (let ((handle (make-symbol "--change-group-handle--"))
        (success (make-symbol "--change-group-success--")))
    `(let ((,handle (prepare-change-group))
           ;; Don't truncate any undo data in the middle of this.
           (undo-outer-limit nil)
           (undo-limit most-positive-fixnum)
           (undo-strong-limit most-positive-fixnum)
           (,success nil))
       (unwind-protect
           (progn
             (activate-change-group ,handle)
             (prog1 ,(macroexp-progn body)
               (setq ,success t)))
         (if ,success
             (progn
               (accept-change-group ,handle)
               (undo-amalgamate-change-group ,handle))
           (cancel-change-group ,handle))))))

(defun meow--highlight-pre-command ()
  (unless (member this-command '(meow-search))
    (meow--remove-match-highlights))
  (meow--remove-expand-highlights)
  (meow--remove-search-highlight))

(defun meow--remove-fake-cursor (rol)
  (when (overlayp rol)
    (when-let ((ovs (overlay-get rol 'meow-face-cursor)))
      (mapc (lambda (o) (when (overlayp o) (delete-overlay o)))
            ovs))))

(defvar meow--region-cursor-faces '(meow-region-cursor-1
                                    meow-region-cursor-2
                                    meow-region-cursor-3))

(defun meow--add-fake-cursor (rol)
  (if (and meow-use-enhanced-selection-effect
           (or (meow-normal-mode-p)
               (meow-beacon-mode-p)))
      (when (overlayp rol)
        (let ((start (overlay-start rol))
              (end (overlay-end rol)))
          (unless (= start end)
            (let (ovs)
                (if (meow--direction-forward-p)
                    (progn
                      (let ((p end)
                            (i 0))
                        (while (and (> p start)
                                    (< i 3))
                          (let ((ov (make-overlay (1- p) p)))
                            (overlay-put ov 'face (nth i meow--region-cursor-faces))
                            (overlay-put ov 'priority 10)
                            (overlay-put ov 'window (overlay-get rol 'window))
                            (cl-decf p)
                            (cl-incf i)
                            (push ov ovs)))))
                  (let ((p start)
                        (i 0))
                    (while (and (< p end)
                                (< i 3))
                      (let ((ov (make-overlay p (1+ p))))
                        (overlay-put ov 'face (nth i meow--region-cursor-faces))
                        (overlay-put ov 'priority 10)
                        (overlay-put ov 'window (overlay-get rol 'window))
                        (cl-incf p)
                        (cl-incf i)
                        (push ov ovs)))))
                (overlay-put rol 'meow-face-cursor ovs)))
          rol))
    rol))

(defun meow--redisplay-highlight-region-function (start end window rol)
  (when (and (or (meow-normal-mode-p)
                 (meow-beacon-mode-p))
             (equal window (selected-window)))
    (if (use-region-p)
        (meow--set-cursor-type meow-cursor-type-region-cursor)
      (meow--set-cursor-type meow-cursor-type-normal)))
  (when meow-use-enhanced-selection-effect
    (meow--remove-fake-cursor rol))
  (thread-first
    (funcall meow--backup-redisplay-highlight-region-function start end window rol)
    (meow--add-fake-cursor)))

(defun meow--redisplay-unhighlight-region-function (rol)
  (meow--remove-fake-cursor rol)
  (when (and (overlayp rol)
             (equal (overlay-get rol 'window) (selected-window))
             (or (meow-normal-mode-p)
                 (meow-beacon-mode-p)))
    (meow--set-cursor-type meow-cursor-type-normal))
  (funcall meow--backup-redisplay-unhighlight-region-function rol))

(defun meow--mix-color (color1 color2 n)
  (mapcar (lambda (c) (apply #'color-rgb-to-hex c))
          (color-gradient (color-name-to-rgb color1)
                          (color-name-to-rgb color2)
                          n)))

(defun meow--beacon-inside-secondary-selection ()
  (and
   (secondary-selection-exist-p)
   (< (overlay-start mouse-secondary-overlay)
      (overlay-end mouse-secondary-overlay))
   (<= (overlay-start mouse-secondary-overlay)
       (point)
       (overlay-end mouse-secondary-overlay))))

(defun meow--narrow-secondary-selection ()
  (narrow-to-region (overlay-start mouse-secondary-overlay)
                    (overlay-end mouse-secondary-overlay)))

(defun meow--hack-cursor-pos (pos)
  "Hack the point when `meow-use-cursor-position-hack' is enabled."
  (if meow-use-cursor-position-hack
      (1- pos)
    pos))

(defun meow--remove-modeline-indicator ()
  (setq-default mode-line-format
                (cl-remove '(:eval (meow-indicator)) mode-line-format
                           :test 'equal)))

(defun meow--init-buffers ()
  "Enable meow in existing buffers."
  (dolist (buf (buffer-list))
    (unless (minibufferp buf)
      (with-current-buffer buf
        (setq-local meow-normal-mode 1)))))

(defun meow--get-leader-keymap ()
  (cond
   ((keymapp meow-keypad-leader-dispatch)
    meow-keypad-leader-dispatch)

   ((null meow-keypad-leader-dispatch)
    (alist-get 'leader meow-keymap-alist))))

(provide 'meow-util)
;;; meow-util.el ends here
