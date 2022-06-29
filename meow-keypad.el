;;; meow-keypad.el --- Meow keypad mode -*- lexical-binding: t -*-

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
;; Keypad state is a special state to simulate C-x and C-c key sequences.
;; There are three commands:
;;
;; meow-keypad-start
;; Enter keypad state, and simulate this key with Control modifier.
;;
;; meow-keypad-self-insert
;; This command is bound to every single key in keypad state.
;; The rules,
;; - If current key is SPC, the next will be considered without modifier.
;; - If current key is m, the next will be considered with Meta modifier.
;; - Other keys, or SPC and m after a prefix, means append a key input, by default, with Control modifier.
;;
;; meow-keypad-undo
;; Remove the last input, if there's no input in the sequence, exit the keypad state.

;;; Code:

(require 'subr-x)
(require 'meow-var)
(require 'meow-util)
(require 'meow-helpers)

(defun meow--keypad-format-upcase (k)
  "Return S-k for upcase k."
  (let ((case-fold-search nil))
    (if (and (stringp k)
             (string-match-p "^[A-Z]$" k))
        (format "S-%s" (downcase k))
      k)))

(defun meow--keypad-format-key-1 (key)
  "Return a display format for input KEY."
  (cl-case (car key)
    ('meta (format "M-%s" (cdr key)))
    ('control (format "C-%s" (meow--keypad-format-upcase (cdr key))))
    ('both (format "C-M-%s" (meow--keypad-format-upcase (cdr key))))
    ('literal (cdr key))))

(defun meow--keypad-format-prefix ()
  "Return a display format for current prefix."
  (cond
   ((equal '(4) meow--prefix-arg)
    "C-u ")
   (meow--prefix-arg
    (format "%s " meow--prefix-arg))
   (t "")))

(defun meow--keypad-lookup-key (keys)
  (let* ((overriding-local-map meow--keypad-base-keymap)
         (keybind (key-binding keys)))
    (unless (and (meow--is-self-insertp keybind)
                 (not meow-keypad-self-insert-undefined))
      keybind)))

(defun meow--keypad-has-sub-meta-keymap-p ()
  (and (not meow--use-literal)
       (not meow--use-both)
       (not meow--use-meta)
       (or (not meow--keypad-keys)
           (let* ((key-str (meow--keypad-format-keys nil))
                  (keymap (meow--keypad-lookup-key (kbd key-str))))
             (and (keymapp keymap)
                  (lookup-key keymap ""))))))

(defun meow--keypad-format-keys (&optional prompt)
  "Return a display format for current input keys."
  (let ((result ""))
    (setq result
          (thread-first
              (mapcar #'meow--keypad-format-key-1 meow--keypad-keys)
            (reverse)
            (string-join " ")))
    (cond
     (meow--use-both
      (setq result
            (if (string-empty-p result)
                "C-M-"
              (concat result " C-M-"))))
     (meow--use-meta
      (setq result
            (if (string-empty-p result)
                "M-"
              (concat result " M-"))))
     (meow--use-literal
      (setq result (concat result " ○")))

     (prompt
      (setq result (concat result " C-"))))
    result))

(defun meow--keypad-quit ()
  "Quit keypad state."
  (setq meow--keypad-keys nil
        meow--use-literal nil
        meow--use-meta nil
        meow--use-both nil
        meow--keypad-help nil)
  (setq overriding-local-map nil)
  (meow--exit-keypad-state))

(defun meow-keypad-quit ()
  "Quit keypad state."
  (interactive)
  (setq this-command last-command)
  (when meow-keypad-message
    (message "KEYPAD exit"))
  (meow--keypad-quit))

(defun meow--make-keymap-for-describe (keymap control)
  (let ((km (make-keymap)))
    (suppress-keymap km t)
    (when (keymapp keymap)
      (map-keymap
       (lambda (key def)
         (unless (member (event-basic-type key) '(127))
           (when (if control (member 'control (event-modifiers key))
                   (not (member 'control (event-modifiers key))))
             (define-key km (vector (meow--get-event-key key))
                         (funcall meow-keypad-get-title-function def)))))
       keymap))
    km))

(defun meow--keypad-get-keymap-for-describe ()
  (let* ((input (thread-first
                  (mapcar #'meow--keypad-format-key-1 meow--keypad-keys)
                  (reverse)
                  (string-join " ")))
         (meta-both-keymap (meow--keypad-lookup-key
                            (read-kbd-macro
                             (if (string-blank-p input)
                                 "ESC"
                               (concat input " ESC"))))))
    (cond
     (meow--use-meta
      (when meta-both-keymap
        (meow--make-keymap-for-describe meta-both-keymap nil)))
     (meow--use-both
      (when meta-both-keymap
        (meow--make-keymap-for-describe meta-both-keymap t)))
     (meow--use-literal
      (when-let ((keymap (meow--keypad-lookup-key (read-kbd-macro input))))
        (when (keymapp keymap)
          (meow--make-keymap-for-describe keymap nil))))

     ;; For leader popup
     ;; meow-keypad-leader-dispatch can be string, keymap or nil
     ;; - string, dynamically find the keymap
     ;; - keymap, just use it
     ;; - nil, take the one in meow-keymap-alist
     ;; Leader keymap may contain meow-dispatch commands
     ;; translated names based on the commands they refer to
     ((null meow--keypad-keys)
      (when-let ((keymap (if (stringp meow-keypad-leader-dispatch)
                             (meow--keypad-lookup-key (read-kbd-macro meow-keypad-leader-dispatch))
                           (or meow-keypad-leader-dispatch
                               (alist-get 'leader meow-keymap-alist)))))
        (let ((km (make-keymap)))
          (suppress-keymap km t)
          (map-keymap
           (lambda (key def)
             (when (and (not (member 'control (event-modifiers key)))
                        (not (member key (list meow-keypad-meta-prefix
                                               meow-keypad-ctrl-meta-prefix
                                               meow-keypad-literal-prefix)))
                        (not (alist-get key meow-keypad-start-keys)))
               (let ((keys (vector (meow--get-event-key key))))
                 (unless (lookup-key km keys)
                   (define-key km keys (funcall meow-keypad-get-title-function def))))))
           keymap)
          km)))

     (t
      (when-let ((keymap (meow--keypad-lookup-key (read-kbd-macro input))))
        (when (keymapp keymap)
          (let* ((km (make-keymap))
                 (has-sub-meta (meow--keypad-has-sub-meta-keymap-p))
                 (ignores (if has-sub-meta
                              (list meow-keypad-meta-prefix
                                    meow-keypad-ctrl-meta-prefix
                                    meow-keypad-literal-prefix
                                    127)
                            (list meow-keypad-literal-prefix 127))))
            (suppress-keymap km t)
            (map-keymap
             (lambda (key def)
               (unless (member 'control (event-modifiers key))
                 (unless (member key ignores)
                   (define-key km (vector (meow--get-event-key key)) (funcall meow-keypad-get-title-function def)))))
             keymap)
            (map-keymap
             (lambda (key def)
               (when (member 'control (event-modifiers key))
                 (unless (member (meow--event-key key) ignores)
                   (when def
                     (define-key km (vector (meow--get-event-key key)) (funcall meow-keypad-get-title-function def))))))
             keymap)
            km)))))))

(defun meow--keypad-display-message ()
  (let (overriding-local-map)
    (when meow-keypad-describe-keymap-function
      (when (or
             meow--keypad-keymap-description-activated

             (setq meow--keypad-keymap-description-activated
                   (sit-for meow-keypad-describe-delay t)))
        (let ((keymap (meow--keypad-get-keymap-for-describe)))
          (funcall meow-keypad-describe-keymap-function keymap))))))

(defun meow--describe-keymap-format (pairs &optional width)
  (let* ((fw (or width (frame-width)))
         (cnt (length pairs))
         (best-col-w nil)
         (best-rows nil))
    (cl-loop for col from 5 downto 2  do
             (let* ((row (1+ (/ cnt col)))
                    (v-parts (seq-partition pairs row))
                    (rows (meow--transpose-lists v-parts))
                    (col-w (thread-last
                             v-parts
                             (mapcar
                              (lambda (col)
                                (cons (seq-max (or (mapcar (lambda (it) (length (car it))) col) '(0)))
                                      (seq-max (or (mapcar (lambda (it) (length (cdr it))) col) '(0))))))))
                    ;; col-w looks like:
                    ;; ((3 . 2) (4 . 3))
                    (w (thread-last
                         col-w
                         ;; 4 is for the width of arrow(3) between key and command
                         ;; and the end tab or newline(1)
                         (mapcar (lambda (it) (+ (car it) (cdr it) 4)))
                         (meow--sum))))
               (when (<= w fw)
                 (setq best-col-w col-w
                       best-rows rows)
                 (cl-return nil))))
    (if best-rows
        (thread-last
          best-rows
          (mapcar
           (lambda (row)
             (thread-last
               row
               (seq-map-indexed
                (lambda (it idx)
                  (let* ((key-str (car it))
                         (def-str (cdr it))
                         (l-r (nth idx best-col-w))
                         (l (car l-r))
                         (r (cdr l-r))
                         (key (meow--string-pad key-str l 32 t))
                         (def (meow--string-pad def-str r 32)))
                    (format "%s%s%s"
                            key
                            (propertize " → " 'face 'font-lock-comment-face)
                            def))))
               (meow--string-join " "))))
          (meow--string-join "\n"))
      (propertize "Frame is too narrow for KEYPAD popup" 'face 'meow-cheatsheet-command))))



(defun meow-describe-keymap (keymap)
  (when (and keymap (not defining-kbd-macro) (not meow--keypad-help))
    (let* ((rst))
      (map-keymap
       (lambda (key def)
         (let ((k (if (consp key)
                      (format "%s .. %s"
                              (key-description (list (car key)))
                              (key-description (list (cdr key))))
                    (key-description (list key)))))
           (let (key-str def-str)
             (cond
              ((and (commandp def) (symbolp def))
               (setq key-str (propertize k 'face 'font-lock-constant-face)
                     def-str (propertize (symbol-name def) 'face 'font-lock-function-name-face)))
              ((symbolp def)
               (setq key-str (propertize k 'face 'font-lock-constant-face)
                     def-str (propertize (concat "+" (symbol-name def)) 'face 'font-lock-keyword-face)))
              ((functionp def)
               (setq key-str (propertize k 'face 'font-lock-constant-face)
                     def-str (propertize "?closure" 'face 'font-lock-function-name-face)))
              (t
               (setq key-str (propertize k 'face 'font-lock-constant-face)
                     def-str (propertize "+prefix" 'face 'font-lock-keyword-face))))
             (push (cons key-str def-str) rst))))
       keymap)
      (setq rst (reverse rst))
      (let ((msg (meow--describe-keymap-format rst)))
        (let ((message-log-max)
              (max-mini-window-height 1.0))
          (save-window-excursion
            (with-temp-message
                (format "%s\nKEYPAD: %s%s"
                        msg
                        (let ((pre (meow--keypad-format-prefix)))
                          (if (string-blank-p pre)
                              ""
                            (propertize pre 'face 'font-lock-comment-face)))
                        (propertize (meow--keypad-format-keys nil) 'face 'font-lock-string-face))
              (sit-for 1000000 t))))))))

(defun meow-keypad-get-title (def)
  "Return a symbol as title or DEF.

Returning DEF will result in a generated title."
  (if-let ((cmd (and (symbolp def)
                     (commandp def)
                     (get def 'meow-dispatch))))
      (meow--keypad-lookup-key (kbd cmd))
    def))

(defun meow-keypad-undo ()
  "Pop the last input."
  (interactive)
  (setq this-command last-command)
  (cond
   (meow--use-both
    (setq meow--use-both nil))
   (meow--use-literal
    (setq meow--use-literal nil))
   (meow--use-meta
    (setq meow--use-meta nil))
   (t
    (pop meow--keypad-keys)))
  (if meow--keypad-keys
      (progn
        (meow--update-indicator)
        (meow--keypad-display-message))
    (when meow-keypad-message
      (message "KEYPAD exit"))
    (meow--keypad-quit)))

(defun meow--keypad-show-message ()
  (let ((message-log-max))
    (message "KEYPAD%s: %s%s"
             (if meow--keypad-help " describe key" "")
             (let ((pre (meow--keypad-format-prefix)))
               (if (string-blank-p pre)
                   ""
                 (propertize pre 'face 'font-lock-comment-face)))
             (propertize (meow--keypad-format-keys nil) 'face 'font-lock-string-face))))

(defun meow--keypad-try-execute ()
  "Try execute command.

If there is a command available on the current key binding,
try replacing the last modifier and try again."
  (unless (or meow--use-literal
              meow--use-meta
              meow--use-both)
    (let* ((key-str (meow--keypad-format-keys nil))
           (cmd (meow--keypad-lookup-key (read-kbd-macro key-str))))
      (cond
       ((commandp cmd t)
        (setq current-prefix-arg meow--prefix-arg
              meow--prefix-arg nil)
        (if meow--keypad-help
            (progn
              (meow--keypad-quit)
              (describe-function cmd))
          (let ((meow--keypad-this-command cmd))
            (meow--keypad-quit)
            (setq real-this-command cmd
                  this-command cmd)
            (call-interactively cmd))))
       ((keymapp cmd)
        (when meow-keypad-message (meow--keypad-show-message))
        (meow--keypad-display-message))
       ((equal 'control (caar meow--keypad-keys))
        (setcar meow--keypad-keys (cons 'literal (cdar meow--keypad-keys)))
        (meow--keypad-try-execute))
       (t
        (setq meow--prefix-arg nil)
        (message "%s is undefined" (meow--keypad-format-keys nil))
        (meow--keypad-quit))))))

(defun meow-keypad-self-insert ()
  "Default command when keypad state is enabled."
  (interactive)
  (setq this-command last-command)
  (when-let ((e (meow--event-key last-input-event))
             (key (meow--parse-input-event e)))
    (let ((has-sub-meta (meow--keypad-has-sub-meta-keymap-p)))
      (cond
       (meow--use-literal
        (push (cons 'literal key)
              meow--keypad-keys)
        (setq meow--use-literal nil))
       (meow--use-both
        (push (cons 'both key) meow--keypad-keys)
        (setq meow--use-both nil))
       (meow--use-meta
        (push (cons 'meta key) meow--keypad-keys)
        (setq meow--use-meta nil))
       ((and (equal e meow-keypad-meta-prefix)
             (not meow--use-meta)
             has-sub-meta)
        (setq meow--use-meta t))
       ((and (equal e meow-keypad-ctrl-meta-prefix)
             (not meow--use-both)
             has-sub-meta)
        (setq meow--use-both t))
       ((and (equal e meow-keypad-literal-prefix)
             (not meow--use-literal)
             meow--keypad-keys)
        (setq meow--use-literal t))
       (meow--keypad-keys
        (push (cons 'control key) meow--keypad-keys))
       ((alist-get e meow-keypad-start-keys)
        (push (cons 'control (meow--parse-input-event
                              (alist-get e meow-keypad-start-keys)))
              meow--keypad-keys))
       (meow--keypad-allow-quick-dispatch
        (if-let ((keymap (meow--get-leader-keymap)))
            (setq meow--keypad-base-keymap keymap)
          (setq meow--keypad-keys (meow--parse-string-to-keypad-keys meow-keypad-leader-dispatch)))
        (push (cons 'literal key) meow--keypad-keys))
       (t
        (push (cons 'control key) meow--keypad-keys))))

    ;; Try execute if the input is valid.
    (if (or meow--use-literal
            meow--use-meta
            meow--use-both)
        (progn
          (when meow-keypad-message (meow--keypad-show-message))
          (meow--keypad-display-message))
      (meow--keypad-try-execute))))

(defun meow-keypad ()
  "Enter keypad state."
  (interactive)
  (setq this-command last-command)
  (setq meow--keypad-previous-state (meow--current-state))
  (meow--switch-state 'keypad)
  (setq overriding-local-map meow-keypad-state-keymap
        overriding-terminal-local-map nil)
  (meow--keypad-display-message))

(defun meow-keypad-start ()
  "Enter keypad state with current input as initial key sequences."
  (interactive)
  (setq this-command last-command)
  (setq meow--keypad-previous-state (meow--current-state))
  (meow--switch-state 'keypad)
  (setq overriding-local-map meow-keypad-state-keymap
        overriding-terminal-local-map nil
        meow--keypad-allow-quick-dispatch nil)
  (call-interactively 'meow-keypad-self-insert))

(defun meow-keypad-start-with (input)
  "Enter keypad state with INPUT.

INPUT is a string, stands for initial keys."
  (setq meow--keypad-previous-state (meow--current-state))
  (meow--switch-state 'keypad)
  (setq meow--keypad-keys (meow--parse-string-to-keypad-keys input)
        overriding-terminal-local-map nil
        overriding-local-map meow-keypad-state-keymap)
  (meow--keypad-try-execute))

(defun meow-keypad-describe-key ()
  "Describe key via KEYPAD input."
  (interactive)
  (setq this-command last-command)
  (setq overriding-local-map meow-keypad-state-keymap
        meow--keypad-help t
        meow--keypad-previous-state (meow--current-state))
  (meow--switch-state 'keypad)
  (meow--keypad-show-message)
  (meow--keypad-display-message))

(provide 'meow-keypad)
;;; meow-keypad.el ends here
