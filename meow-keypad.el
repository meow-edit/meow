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
        meow--use-both nil)
  (meow--exit-keypad-state))

(defun meow--keypad-display-message ()
  (let ((max-mini-window-height 1.0))
    (let* ((input (-> (mapcar #'meow--keypad-format-key-1 meow--keypad-keys)
                      (reverse)
                      (string-join " "))))
      (when meow-keypad-describe-keymap-function
        (cond
         (meow--use-meta
          (when-let ((keymap (key-binding (read-kbd-macro
                                           (if (string-blank-p input)
                                               "ESC"
                                             (concat input " ESC"))))))
            (let ((km))
              (when (meow--keymapp keymap)
                (map-keymap
                 (lambda (key def)
                   (unless (event-modifiers key)
                     (push (cons (event-basic-type key) def) km)))
                 keymap))
              (setq km (seq-sort (lambda (x y)
                                   (< (if (numberp (car x)) (car x) most-positive-fixnum)
                                      (if (numberp (car y)) (car y) most-positive-fixnum)))
                                 km))
              (funcall meow-keypad-describe-keymap-function (cons 'keymap km)))))

         (meow--use-both
          (when-let ((keymap (key-binding (read-kbd-macro
                                           (if (string-blank-p input)
                                               "ESC"
                                             (concat input " ESC"))))))
            (let ((km))
              (when (meow--keymapp keymap)
                (map-keymap
                 (lambda (key def)
                   (when (equal '(control) (event-modifiers key))
                     (push (cons (event-basic-type key) def) km)))
                 keymap))
              (setq km (seq-sort (lambda (x y)
                                   (< (if (numberp (car x)) (car x) most-positive-fixnum)
                                      (if (numberp (car y)) (car y) most-positive-fixnum)))
                                 km))
              (funcall meow-keypad-describe-keymap-function (cons 'keymap km)))))

         (meow--use-literal
          (when-let ((keymap (key-binding (read-kbd-macro input))))
            (when (meow--keymapp keymap)
              (let ((km '()))
                (map-keymap-sorted
                 (lambda (key def)
                   (unless (event-modifiers key)
                     (push (cons (event-basic-type key) def) km)))
                 keymap)
                (funcall meow-keypad-describe-keymap-function (cons 'keymap km))))))

         (t
          (when-let ((keymap (key-binding (read-kbd-macro input))))
            (when (keymapp keymap)
              (let ((km '()))
                (map-keymap-sorted
                 (lambda (key def)
                   (when (equal '(control) (event-modifiers key))
                     (push (cons (event-basic-type key) def) km)))
                 keymap)
                (funcall meow-keypad-describe-keymap-function (cons 'keymap km)))
              ))))))))

(defun meow--keypad-try-execute ()
  "Try execute command.

If there's command available on current key binding, Try replace the last modifier and try again."
  (unless (or meow--use-literal
              meow--use-meta
              meow--use-both)
    (let* ((key-str (meow--keypad-format-keys))
           (cmd (key-binding (read-kbd-macro key-str))))
      (cond
       ((commandp cmd t)
        (setq current-prefix-arg meow--prefix-arg
              meow--prefix-arg nil)
        (meow--keypad-quit)
        (call-interactively cmd))
       ((keymapp cmd)
        (meow--keypad-display-message))
       ((equal 'control (caar meow--keypad-keys))
        (setcar meow--keypad-keys (cons 'literal (cdar meow--keypad-keys)))
        (meow--keypad-try-execute))
       (t
        (setq meow--prefix-arg nil)
        (message "Meow: execute %s failed, command not found!" (meow--keypad-format-keys))
        (meow--keypad-quit))))))

(defun meow-describe-keymap (keymap)
  (let* ((rst)
         (w (frame-width))
         (c (/ w 40)))
    (when (>= c 1)
      (map-keymap
       (lambda (key def)
         (if (commandp def)
             (push (format "% 8s %s %s"
                           (propertize (key-description (list key)) 'face 'font-lock-constant-face)
                           (propertize "→" 'face 'font-lock-comment-face)
                           (s-truncate 28 (symbol-name def) "…"))
                   rst)
           (push (format "% 8s %s %s"
                         (propertize (key-description (list key)) 'face 'font-lock-constant-face)
                         (propertize "→" 'face 'font-lock-comment-face)
                         (propertize "+keymap" 'face 'font-lock-keyword-face))
                 rst)))
       keymap)
      (let ((msg (->> rst
                      (-map-indexed
                       (lambda (idx s)
                         (let ((s (s-pad-right 39 " " s)))
                           (if (= (mod idx c) (1- c))
                               (format "%s\n" s)
                             (format "%s " s)))))
                      (s-join ""))))
        (let ((message-log-max))
          (save-window-excursion
            (with-temp-message
                (concat "KEYPAD:"
                        (propertize (meow--keypad-format-keys) 'face 'font-lock-string-face)
                        "\n"
                        msg)
              (sit-for most-positive-fixnum))))))))

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
    (meow--keypad-quit)))

(defun meow-keypad-self-insert ()
  "Default command when keypad state is enabled."
  (interactive)
  (when-let ((key (cond
                   ((equal last-input-event 32)
                    "SPC")
                   ((characterp last-input-event)
                    (string last-input-event))
                   ((equal 'tab last-input-event)
                    "TAB")
                   ((equal 'return last-input-event)
                    "RET")
                   ((symbolp last-input-event)
                    (format "<%s>" last-input-event))
                   (t nil))))
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
     ((and (string-equal key meow--keypad-meta-prefix)
           (not meow--use-meta))
      (setq meow--use-meta t))
     ((and (string-equal key meow--keypad-both-prefix)
           (not meow--use-both))
      (setq meow--use-both t))
     ((and (string-equal key meow--keypad-literal-prefix)
           (not meow--use-literal))
      (setq meow--use-literal t))
     (t
      (push (cons 'control key) meow--keypad-keys)))
    (when (and meow-keypad-message (not meow-keypad-describe-keymap-function))
      (let ((message-log-max))
        (message "Meow: %s" (meow--keypad-format-keys))))
    ;; Try execute if the input is valid.
    (if (or meow--use-literal
            meow--use-meta
            meow--use-both)
        (meow--keypad-display-message)
      (meow--keypad-try-execute))
    (meow--update-indicator)
    (force-mode-line-update)))

(defun meow-keypad-start ()
  "Enter keypad state with current input as initial key sequences."
  (interactive)
  (meow--switch-state 'keypad)
  (call-interactively #'meow-keypad-self-insert))

(provide 'meow-keypad)
;;; meow-keypad.el ends here
