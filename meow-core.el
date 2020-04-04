;;; meow-core.el --- Mode definitions for Meow
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

(define-minor-mode meow-insert-mode
  "Meow Insert state."
  nil
  " [I]"
  meow-insert-state-keymap
  (meow--insert-init))

(define-minor-mode meow-normal-mode
  "Meow Normal state."
  nil
  " [N]"
  meow-normal-state-keymap
  (meow--normal-init))

(define-minor-mode meow-keypad-mode
  "Meow keypad state."
  nil
  " [K]"
  meow-keypad-state-keymap
  (meow--keypad-init))

(define-minor-mode meow-motion-mode
  "Meow motion state."
  nil
  " [M]"
  meow-motion-state-keymap
  (meow--motion-init))

(define-minor-mode meow-mode
  "Modal On Dvorak."
  nil
  nil
  meow-keymap
  (if meow-mode
      (meow--enable)
    (meow--disable)))

;;;###autoload
(defun meow-indicator ()
  (interactive)
  (when meow-mode
    (cond
     (meow-keypad-mode
      (concat
       (propertize "KEYPAD [" 'face 'meow-keypad-indicator)
       (meow--keypad-format-prefix)
       (meow--keypad-format-keys)
       (propertize "] " 'face 'meow-keypad-indicator)))
     (meow-normal-mode
      (propertize
       (if (meow--direction-backward-p)
           "NORMAL«"
         "NORMAL")
       'face 'meow-normal-indicator))
     (meow-motion-mode
      (propertize "MOTION" 'face 'meow-motion-indicator))
     (meow-insert-mode
      (cond
       ;; Vterm's vterm-mode is read-only.
       ((and buffer-read-only (not (equal major-mode 'vterm-mode)))
        (propertize "READONLY" 'face 'meow-insert-indicator))
       ((bound-and-true-p overwrite-mode)
        (propertize "OVERWRITE" 'face 'meow-insert-indicator))
       (t (propertize "INSERT" 'face 'meow-insert-indicator))))
     (t ""))))

;;;###autoload
(define-global-minor-mode meow-global-mode meow-mode
  (lambda ()
    (unless (minibufferp)
      (meow-mode 1)))
  (if meow-mode
      (meow--global-enable)
    (meow--global-disable)))

(provide 'meow-core)
;;; meow-core.el ends here
