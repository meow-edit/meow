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
(require 's)

(defun meow--keypad-format-key-1 (key)
  "Return a display format for input KEY."
  (cl-case (car key)
    ('meta (format "M-%s" (cdr key)))
    ('control (format "C-%s" (cdr key)))
    ('both (format "C-M-%s" (cdr key)))
    ('literal (cdr key))))

(defun meow--keypad-format-prefix ()
  "Return a display format for current prefix."
  (cond
   ((equal '(4) meow--prefix-arg)
    "C-u ")
   (meow--prefix-arg
    (format "%s " meow--prefix-arg))
   (t "")))

(defun meow--keypad-format-keys ()
  "Return a display format for current input keys."
  (let ((result ""))
    (setq result
          (thread-first
              (mapcar #'meow--keypad-format-key-1 meow--keypad-keys)
            (reverse)
            (string-join " ")))
    (when meow--use-both
      (setq result
            (if (string-empty-p result)
                "C-M-"
              (concat result " C-M-"))))
    (when meow--use-meta
      (setq result
            (if (string-empty-p result)
                "M-"
              (concat result " M-"))))
    (when meow--use-literal
      (setq result (concat result " ○")))
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
  (when meow-keypad-message
    (message "Meow: KEYPAD exit"))
  (meow--keypad-quit))

(defun meow--build-temp-keymap (keybindings)
  (->> keybindings
       (seq-sort (lambda (x y)
                   (< (if (numberp (car x)) (car x) most-positive-fixnum)
                      (if (numberp (car y)) (car y) most-positive-fixnum))))
       (-group-by #'car)
       (-keep
        (-lambda ((_k . itms))
          (-last (-lambda ((k . _c))
                   (not (member k '(127 delete backspace))))
                 itms)))
       (-reduce-from (-lambda (rst (k . c))
                       (let ((last-c (cdar rst)))
                         (if (and (equal last-c c))
                             (let ((last-k (caar rst)))
                               (setcar rst (cons (cons k (if (listp last-k) last-k (list last-k)))
                                                 c))
                               rst)
                           (cons (cons k c) rst))))
                     ())
       (cons 'keymap)))

(defun meow--keypad-get-keymap-for-describe ()
  (let* ((input (-> (mapcar #'meow--keypad-format-key-1 meow--keypad-keys)
                    (reverse)
                    (string-join " "))))
    (cond
     (meow--use-meta
      (when-let ((keymap (key-binding (read-kbd-macro
                                       (if (string-blank-p input)
                                           "ESC"
                                         (concat input " ESC"))))))
        (let ((km))
          (when (keymapp keymap)
            (map-keymap
             (lambda (key def)
               (unless (member 'control (event-modifiers key))
                 (push (cons (meow--get-event-key key) def) km)))
             keymap))
          (meow--build-temp-keymap km))))

     (meow--use-both
      (when-let ((keymap (key-binding (read-kbd-macro
                                       (if (string-blank-p input)
                                           "ESC"
                                         (concat input " ESC"))))))
        (let ((km))
          (when (keymapp keymap)
            (map-keymap
             (lambda (key def)
               (when (member 'control (event-modifiers key))
                 (push (cons (meow--get-event-key key) def) km)))
             keymap))
          (setq km (seq-sort (lambda (x y)
                               (> (if (numberp (car x)) (car x) most-positive-fixnum)
                                  (if (numberp (car y)) (car y) most-positive-fixnum)))
                             km))
          (meow--build-temp-keymap km))))

     (meow--use-literal
      (when-let ((keymap (key-binding (read-kbd-macro input))))
        (when (keymapp keymap)
          (let ((km '()))
            (map-keymap
             (lambda (key def)
               (unless (member 'control (event-modifiers key))
                 (push (cons (meow--get-event-key key) def) km)))
             keymap)
            (meow--build-temp-keymap km)))))

     (t
      (when-let ((keymap (key-binding (read-kbd-macro input))))
        (when (keymapp keymap)
          (let ((km '())
                (ignores (list meow--keypad-meta-prefix
                               meow--keypad-both-prefix
                               meow--keypad-literal-prefix)))
            (map-keymap
             (lambda (key def)
               (when (member 'control (event-modifiers key))
                 (unless (member (meow--event-key key) ignores)
                   (push (cons (meow--get-event-key key) def) km))))
             keymap)
            (map-keymap
             (lambda (key def)
               (unless (member 'control (event-modifiers key))
                 (unless (member key ignores)
                   (push (cons (meow--get-event-key key) def) km))))
             keymap)
            (meow--build-temp-keymap km))))))))

(defun meow--keypad-display-message ()
  (let (overriding-local-map)
    (when meow-keypad-describe-keymap-function
      (when (or
             ;; `meow--keypad-keymap-description-activated' will not be unset
             ;; so here we ensure the after first input, popup is delayed.
             (and meow--keypad-keymap-description-activated
                  (or (equal 'meow-keypad-undo this-command)
                      (> (+ (length meow--keypad-keys)
                            (if (or meow--use-both meow--use-literal meow--use-meta) 1 0))
                         1)
                      (member (caar meow--keypad-keys) '(both meta))))

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
                    (v-parts (-partition-all row pairs))
                    (rows (meow--transpose-lists v-parts))
                    (col-w (->> v-parts
                                (-map (lambda (col)
                                        (cons (-max (or (--map (length (car it)) col) '(0)))
                                              (-max (or (--map (length (cdr it)) col) '(0))))))))
                    ;; col-w looks like:
                    ;; ((3 . 2) (4 . 3))
                    (w (->> col-w
                            ;; 4 is for the width of arrow(3) between key and command
                            ;; and the end tab or newline(1)
                            (-map (-lambda ((l . r)) (+ l r 4)))
                            (-sum))))
               (when (<= w fw)
                 (setq best-col-w col-w
                       best-rows rows)
                 (cl-return nil))))
    (if best-rows
        (->> best-rows
             (-map
              (lambda (row)
                (->> row
                     (-map-indexed (-lambda (idx (key-str . cmd-str))
                                     (-let* (((l . r) (nth idx best-col-w))
                                             (key (s-pad-left l " " key-str))
                                             (cmd (s-pad-right r " " cmd-str)))
                                       (format "%s%s%s"
                                               (propertize key 'face 'font-lock-constant-face)
                                               (propertize " → " 'face 'font-lock-comment-face)
                                               (propertize cmd 'face
                                                           (if (string-equal "+prefix" cmd)
                                                               'font-lock-keyword-face
                                                             'font-lock-function-name-face))))))
                     (s-join " "))))
             (s-join "\n"))
      (propertize "Frame is too narrow for KEYPAD popup" 'face 'meow-cheatsheet-command))))

(defun meow-describe-keymap (keymap)
  (when (and keymap (not defining-kbd-macro) (not meow--keypad-help))
    (let* ((rst))
      (map-keymap
       (lambda (key def)
         (let ((k (if (listp key)
                      (if (> (length key) 3)
                          (format "%s .. %s"
                                  (key-description (list (-last-item key)))
                                  (key-description (list (car key))))
                        (->> key
                             (--map (key-description (list it)))
                             (s-join " ")))
                    (key-description (list key)))))
           (if (commandp def)
               (push
                (cons k (symbol-name def))
                rst)
             (push
              (cons k "+prefix")
              rst))))
       keymap)
      (let ((msg (meow--describe-keymap-format rst)))
        (let ((message-log-max)
              (max-mini-window-height 1.0))
          (save-window-excursion
            (with-temp-message
                (format "%s\nMeow: %s%s"
                        msg
                        (let ((pre (meow--keypad-format-prefix)))
                          (if (s-blank-p pre)
                              ""
                            (propertize pre 'face 'font-lock-comment-face)))
                        (propertize (meow--keypad-format-keys) 'face 'font-lock-string-face))
              (sit-for most-positive-fixnum t))))))))

(defun meow-keypad-undo ()
  "Pop the last input."
  (interactive)
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
      (message "Meow: KEYPAD exit"))
    (meow--keypad-quit)))

(defun meow--keypad-show-message ()
  (let ((message-log-max))
    (message "Meow%s: %s%s"
             (if meow--keypad-help " describe key" "")
             (let ((pre (meow--keypad-format-prefix)))
               (if (s-blank-p pre)
                   ""
                 (propertize pre 'face 'font-lock-comment-face)))
             (propertize (meow--keypad-format-keys) 'face 'font-lock-string-face))))

(defun meow--keypad-try-execute ()
  "Try execute command.

If there is a command available on the current key binding, try replacing the last modifier and try again."
  (unless (or meow--use-literal
              meow--use-meta
              meow--use-both)
    (let* ((key-str (meow--keypad-format-keys))
           (cmd (let (overriding-local-map) (key-binding (read-kbd-macro key-str)))))
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
            (setq real-this-command cmd)
            (call-interactively cmd))))
       ((keymapp cmd)
        (when meow-keypad-message (meow--keypad-show-message))
        (meow--keypad-display-message))
       ((equal 'control (caar meow--keypad-keys))
        (setcar meow--keypad-keys (cons 'literal (cdar meow--keypad-keys)))
        (meow--keypad-try-execute))
       (t
        (setq meow--prefix-arg nil)
        (message "Meow: %s is undefined" (meow--keypad-format-keys))
        (meow--keypad-quit))))))

(defun meow-keypad-self-insert ()
  "Default command when keypad state is enabled."
  (interactive)
  (when-let ((e (meow--event-key last-input-event))
             (key (meow--parse-input-event e)))
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
     ((and (equal e meow--keypad-meta-prefix)
           (not meow--use-meta))
      (setq meow--use-meta t))
     ((and (equal e meow--keypad-both-prefix)
           (not meow--use-both))
      (setq meow--use-both t))
     ((and (equal e meow--keypad-literal-prefix)
           (not meow--use-literal))
      (setq meow--use-literal t))
     (t
      (push (cons 'control key) meow--keypad-keys)))

    ;; Try execute if the input is valid.
    (if (or meow--use-literal
            meow--use-meta
            meow--use-both)
        (progn
          (when meow-keypad-message (meow--keypad-show-message))
          (meow--keypad-display-message))
      (meow--keypad-try-execute))))

(defun meow-keypad-start ()
  "Enter keypad state with current input as initial key sequences."
  (interactive)
  (meow--switch-state 'keypad)
  (setq overriding-local-map meow-keypad-state-keymap)
  (call-interactively #'meow-keypad-self-insert))

(defun meow-keypad-describe-key ()
  "Describe key via KEYPAD input."
  (interactive)
  (setq overriding-local-map meow-keypad-state-keymap
        meow--keypad-help t)
  (meow--switch-state 'keypad)
  (meow--keypad-show-message)
  (meow--keypad-display-message))

(provide 'meow-keypad)
;;; meow-keypad.el ends here
