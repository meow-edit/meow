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

(defvar meow--keypad-previous-state nil)
(defvar meow-leader-keys '(32))
(defvar meow-keypad-start-trans-keys '(?x ?c ?h))
(defvar meow-keypad-meta-prefix ?m)
(defvar meow-keypad-ctrl-meta-prefix ?g)
(defvar meow-keypad-literal-prefix 32)
(defvar meow-keypad-fallback-prefix "C-c")

(defun meow--keypad-has-binding-p (e)
  (let ((binding (key-binding
                  (vconcat (seq-take (this-command-keys) (1- (length (this-command-keys))))
                           (list e)))))
    binding))

(defmacro meow--wrap-key-translation (&rest body)
  `(progn
     (set-keymap-parent key-translation-map nil)
     (let* ((keys (seq-take (this-command-keys) (1- (length (this-command-keys)))))
            (events (progn ,@body))
            (keys (vconcat keys events))
            (def (key-binding keys)))
       (if (commandp def)
           (progn
             (meow--switch-state meow--keypad-previous-state)
             events)
         (set-keymap-parent key-translation-map meow-keypad-trans-keymap)
         events))))

(defun meow-keypad-trans-ctrl (_)
  (meow--wrap-key-translation
   (let ((e (event-apply-modifier last-input-event 'control 26 "C-")))
     (if (meow--keypad-has-binding-p e)
         (vector e)
       (vector last-input-event)))))

(defun meow-keypad-trans-meta (_)
  (meow--wrap-key-translation
   (if (meow--keypad-has-binding-p 27)
       (vector (event-apply-modifier (read-event) 'meta 27 "M-"))
     (vector last-input-event))))

(defun meow-keypad-trans-ctrl-meta (_)
  (meow--wrap-key-translation
   (if (meow--keypad-has-binding-p 27)
       (thread-first
         (read-event)
         (event-apply-modifier 'control 26 "C-")
         (event-apply-modifier 'meta 27 "M-")
         (vector))
     (vector last-input-event))))

(defun meow-keypad-trans-literal (_)
  (meow--wrap-key-translation
    (vector (read-event))))

(defvar meow-keypad-entry-keymap
  (let ((map (make-keymap)))
    (dolist (k meow-keypad-start-trans-keys)
      (define-key map (vector k) 'meow-keypad-trans-ctrl))
    (define-key map (vector meow-keypad-meta-prefix) 'meow-keypad-trans-meta)
    (define-key map (vector meow-keypad-ctrl-meta-prefix) 'meow-keypad-trans-ctrl-meta)
    (define-key map (kbd "ESC") (kbd "<escape>"))
    map)
  "The default keymap contains the entries of KEYPAD states.")

(defvar meow-keypad-trans-keymap
  (let ((map (make-sparse-keymap)))
    (let ((i ?\s))
      (while (<= i 255)
        (define-key map (vector i) 'meow-keypad-trans-ctrl)
        (setq i (1+ i))))
    (define-key map (vector meow-keypad-meta-prefix) 'meow-keypad-trans-meta)
    (define-key map (vector meow-keypad-ctrl-meta-prefix) 'meow-keypad-trans-ctrl-meta)
    (define-key map (vector meow-keypad-literal-prefix) 'meow-keypad-trans-literal)
    (define-key map (kbd "ESC") (kbd "<escape>"))
    map)
  "The keymap contains the translations when KEYPAD is activated.")

(defun meow--maybe-exit-keypad-state ()
  (when (and (meow-keypad-mode-p)
             (not (member this-command '(meow-keypad meow-keypad-start))))
    (meow--switch-state meow--keypad-previous-state)))

(defun meow-keypad ()
  (interactive)
  (setq meow--keypad-previous-state (meow--current-state))
  (meow--switch-state 'keypad))

(provide 'meow-keypad)
;;; meow-keypad.el ends here
