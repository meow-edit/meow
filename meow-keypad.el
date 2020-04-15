;;; meow-keypad.el --- Meow keypad mode
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

(defun meow--keypad-format-key-1 (key)
  "Return a display format for input KEY."
  (cl-case (car key)
    ('meta (format "M-%s" (cdr key)))
    ('control (format "C-%s" (cdr key)))
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
    (when meow--use-meta
      (setq result (concat result " M-")))
    (when meow--use-literal
      (setq result (concat result " ○")))
    result))

(defun meow--keypad-quit ()
  "Quit keypad state."
  (setq meow--keypad-keys nil
        meow--use-literal nil
        meow--use-meta nil)
  (meow-keypad-mode -1))

(defun meow--keypad-try-execute ()
  "Try execute command.

If there's command available on current key binding, Try replace the last modifier and try again."
  (unless (or meow--use-literal
              meow--use-meta)
    (let* ((key-str (meow--keypad-format-keys))
           (cmd (key-binding (read-kbd-macro key-str))))
      (cond
       ((commandp cmd t)
        (meow--keypad-quit)
        (setq current-prefix-arg meow--prefix-arg)
        (setq meow--prefix-arg nil)
        (call-interactively cmd))
       ((keymapp cmd))
       ((equal 'control (caar meow--keypad-keys))
        (setcar meow--keypad-keys (cons 'literal (cdar meow--keypad-keys)))
        (meow--keypad-try-execute))
       (t
        (setq meow--prefix-arg nil)
        (meow--keypad-quit))))))

(defun meow-keypad-undo ()
  "Pop the last input."
  (interactive)
  (cond
   (meow--use-literal
    (setq meow--use-literal nil))
   (meow--use-meta
    (setq meow--use-meta nil))
   (t
    (pop meow--keypad-keys)))
  (unless meow--keypad-keys
    (meow--keypad-quit)))

(defun meow-keypad-self-insert ()
  "Default command when keypad state is enabled."
  (interactive)
  (when-let ((key (cond
                   ((equal last-input-event 'return) "RET")
                   ((equal last-input-event 'tab) "<tab>")
                   ((characterp last-input-event)
                    (string last-input-event))
                   (t nil))))
    (cond
     (meow--use-literal
      (push (cons 'literal
                  (if (string-equal " " key)
                      "SPC"
                    key))
            meow--keypad-keys)
      (setq meow--use-literal nil))
     (meow--use-meta
      (push (cons 'meta key) meow--keypad-keys)
      (setq meow--use-meta nil))
     ((and (string-equal key meow--keypad-meta-prefix)
           (not meow--use-meta))
      (setq meow--use-meta t))
     ((and (string-equal key meow--keypad-literal-prefix)
           (not meow--use-literal))
      (setq meow--use-literal t))
     (t
      (push (cons 'control key) meow--keypad-keys)))
    (unless (or meow--use-literal
                meow--use-meta)
      (meow--keypad-try-execute))
    ;; We need update mode-line here, otherwise the indiactor will not refresh.
    ;; Don't know why
    (force-mode-line-update)))

(defun meow-keypad-start ()
  "Enter keypad state with current input as initial key sequences."
  (interactive)
  (meow--switch-state 'keypad)
  (call-interactively #'meow-keypad-self-insert))

(provide 'meow-keypad)
;;; meow-keypad.el ends here
