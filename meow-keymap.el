;;; meow-keymap.el --- Default keybindings for Meow  -*- lexical-binding: t; -*-

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
;; Default keybindings.

;;; Code:

(require 'meow-var)

(defvar-local meow--origin-commands nil
  "Overwritten commands in MOTION state.")

(defvar meow-keymap
  (let ((keymap (make-sparse-keymap)))
    keymap)
  "Global keymap for Meow.")

(defvar meow-leader-keymap
  (let ((keymap (make-sparse-keymap)))
    (suppress-keymap keymap t)
    (define-key keymap (kbd "SPC") 'meow-motion-origin-command)
    (define-key keymap (kbd "u") 'meow-universal-argument)
    (define-key keymap (kbd "m") 'meow-keypad-start)
    (define-key keymap (kbd "g") 'meow-keypad-start)
    (define-key keymap (kbd "x") 'meow-keypad-start)
    (define-key keymap (kbd "h") 'meow-keypad-start)
    (define-key keymap (kbd "c") 'meow-keypad-start)
    (define-key keymap (kbd "1") 'meow-digit-argument)
    (define-key keymap (kbd "2") 'meow-digit-argument)
    (define-key keymap (kbd "3") 'meow-digit-argument)
    (define-key keymap (kbd "4") 'meow-digit-argument)
    (define-key keymap (kbd "5") 'meow-digit-argument)
    (define-key keymap (kbd "6") 'meow-digit-argument)
    (define-key keymap (kbd "7") 'meow-digit-argument)
    (define-key keymap (kbd "8") 'meow-digit-argument)
    (define-key keymap (kbd "9") 'meow-digit-argument)
    (define-key keymap (kbd "0") 'meow-digit-argument)
    keymap)
  "A base keymap for leader key.")

(defvar meow-insert-state-keymap
  (let ((keymap (make-keymap)))
    (define-key keymap (kbd "<escape>") 'meow-insert-exit)
    keymap)
  "Keymap for Meow insert state.")

(defvar meow-numeric-argument-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "1") 'digit-argument)
    (define-key keymap (kbd "2") 'digit-argument)
    (define-key keymap (kbd "3") 'digit-argument)
    (define-key keymap (kbd "4") 'digit-argument)
    (define-key keymap (kbd "5") 'digit-argument)
    (define-key keymap (kbd "6") 'digit-argument)
    (define-key keymap (kbd "7") 'digit-argument)
    (define-key keymap (kbd "8") 'digit-argument)
    (define-key keymap (kbd "9") 'digit-argument)
    (define-key keymap (kbd "0") 'digit-argument)
    keymap))

(defvar meow-normal-state-keymap
  (let ((keymap (make-keymap)))
    (suppress-keymap keymap t)
    (define-key keymap (kbd "SPC") meow-leader-keymap)
    (define-key keymap (kbd "i") 'meow-insert)
    (define-key keymap (kbd "a") 'meow-append)
    keymap)
  "Keymap for Meow normal state.")

(defvar meow-motion-state-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [escape] 'meow-last-buffer)
    (define-key keymap (kbd "SPC") meow-leader-keymap)
    keymap)
  "Keymap for Meow motion state.")

(defvar meow-keypad-state-keymap
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map [remap self-insert-command] 'meow-keypad-self-insert)
    (let ((i ?\s))
      (while (< i 256)
        (define-key map (vector i) 'meow-keypad-self-insert)
        (setq i (1+ i)))
      (define-key map (kbd "DEL") 'meow-keypad-undo)
      (define-key map (kbd "<backspace>") 'meow-keypad-undo)
      (define-key map (kbd "<escape>") 'meow-keypad-quit)
      (define-key map (kbd "C-g") 'meow-keypad-quit)
      (define-key map (kbd "<tab>") 'meow-keypad-self-insert)
      (define-key map (kbd "TAB") 'meow-keypad-self-insert)
      (define-key map (kbd "<return>") 'meow-keypad-self-insert)
      (define-key map (kbd "<up>") 'meow-keypad-self-insert)
      (define-key map (kbd "<down>") 'meow-keypad-self-insert)
      (define-key map (kbd "<left>") 'meow-keypad-self-insert)
      (define-key map (kbd "<right>") 'meow-keypad-self-insert)
      (define-key map (kbd "<home>") 'meow-keypad-self-insert)
      (define-key map (kbd "<end>") 'meow-keypad-self-insert)
      (define-key map (kbd "<next>") 'meow-keypad-self-insert)
      (define-key map (kbd "<prior>") 'meow-keypad-self-insert)
      (define-key map (kbd "RET") 'meow-keypad-self-insert))
    map)
  "Keymap for Meow keypad state.")

(provide 'meow-keymap)
;;; meow-keymap.el ends here
