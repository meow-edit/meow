;;; meow-keymap.el --- Default keybindings for Meow
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
;; Default keybindings.

;;; Code:

(require 'meow-var)

(defvar-local meow--space-command nil
  "Current command on SPC in special mode buffer.")

(defvar-local meow--keymap-loaded nil
  "If keymap is loaded in this buffer.")

(defvar meow-keymap
  (let ((keymap (make-sparse-keymap)))
    keymap)
  "Global keymap for Meow.")

(defvar meow-leader-keymap
  (let ((keymap (make-sparse-keymap)))
    (suppress-keymap keymap t)
    (define-key keymap (kbd "SPC") 'meow-space)
    (define-key keymap (kbd "<escape>") 'meow-temp-normal)
    (define-key keymap (kbd "TAB") 'other-window)
    (define-key keymap (kbd "x") 'meow-keypad-start)
    (define-key keymap (kbd "c") 'meow-keypad-start)
    (define-key keymap (kbd "e") 'meow-eval-last-exp)
    (define-key keymap (kbd "r") 'meow-raise-sexp)
    (define-key keymap (kbd "s") 'meow-split-sexp)
    (define-key keymap (kbd "u") 'meow-splice-sexp)
    (define-key keymap (kbd "t") 'meow-transpose-sexp)
    (define-key keymap (kbd "j") 'meow-join-sexp)
    keymap)
  "A base keymap for leader key.")

(defvar meow-insert-state-keymap
  (let ((keymap (make-keymap)))
    (define-key keymap (kbd "<escape>") 'meow-insert-exit)
    keymap)
  "Keymap for Meow insert state.")

(defvar meow-normal-state-keymap
  (let ((keymap (make-keymap)))
    (suppress-keymap keymap t)
    (define-key keymap (kbd "-") 'negative-argument)
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
    (define-key keymap (kbd "SPC") meow-leader-keymap)
    (define-key keymap (kbd "a") 'meow-append)
    (define-key keymap (kbd "A") 'meow-open)
    (define-key keymap (kbd "b") 'meow-block)
    (define-key keymap (kbd "B") 'meow-block-expand)
    (define-key keymap (kbd "c") 'meow-copy)
    (define-key keymap (kbd "C") 'recenter-top-bottom)
    (define-key keymap (kbd "d") 'meow-delete)
    (define-key keymap (kbd "D") 'meow-backward-delete)
    (define-key keymap (kbd "e") 'meow-line)
    (define-key keymap (kbd "E") 'meow-line-expand)
    (define-key keymap (kbd "f") 'meow-find)
    (define-key keymap (kbd "F") 'meow-find-expand)
    (define-key keymap (kbd "g") 'meow-keyboard-quit)
    (define-key keymap (kbd "G") 'meow-page-up)
    (define-key keymap (kbd "h") 'meow-head)
    (define-key keymap (kbd "H") 'meow-head-expand)
    (define-key keymap (kbd "i") 'meow-insert)
    (define-key keymap (kbd "I") 'meow-open-above)
    (define-key keymap (kbd "j") 'meow-join)
    (define-key keymap (kbd "J") 'meow-newline)
    (define-key keymap (kbd "k") 'meow-kill)
    (define-key keymap (kbd "K") 'meow-kill-whole-line)
    (define-key keymap (kbd "l") 'meow-till)
    (define-key keymap (kbd "L") 'meow-till-expand)
    (define-key keymap (kbd "m") 'meow-mark-word)
    (define-key keymap (kbd "M") 'meow-mark-word-expand)
    (define-key keymap (kbd "n") 'meow-next)
    (define-key keymap (kbd "N") 'meow-next-expand)
    (define-key keymap (kbd "o") 'meow-occur)
    (define-key keymap (kbd "O") 'other-window)
    (define-key keymap (kbd "p") 'meow-prev)
    (define-key keymap (kbd "P") 'meow-prev-expand)
    (define-key keymap (kbd "q") 'meow-quit)
    (define-key keymap (kbd "Q") 'delete-other-windows)
    (define-key keymap (kbd "r") 'meow-reverse)
    (define-key keymap (kbd "R") 'meow-page-down)
    (define-key keymap (kbd "s") 'meow-search)
    (define-key keymap (kbd "S") 'kmacro-start-macro)
    (define-key keymap (kbd "t") 'meow-tail)
    (define-key keymap (kbd "T") 'meow-tail-expand)
    (define-key keymap (kbd "u") 'universal-argument)
    (define-key keymap (kbd "U") 'universal-argument)
    (define-key keymap (kbd "v") 'meow-pop-selection)
    (define-key keymap (kbd "V") 'kmacro-call-macro)
    (define-key keymap (kbd "w") 'meow-word)
    (define-key keymap (kbd "W") 'meow-word-expand)
    (define-key keymap (kbd "x") 'meow-change)
    (define-key keymap (kbd "X") 'apply-macro-to-region-lines)
    (define-key keymap (kbd "y") 'meow-yank)
    (define-key keymap (kbd "Y") 'meow-yank-pop)
    (define-key keymap (kbd "z") 'meow-undo)
    (define-key keymap (kbd "Z") 'kmacro-end-macro)
    (define-key keymap (kbd "'") 'meow-string-inner)
    (define-key keymap (kbd "\"") 'meow-string-outer)
    (define-key keymap (kbd "(") 'meow-round-inner)
    (define-key keymap (kbd ")") 'meow-round-outer)
    (define-key keymap (kbd "[") 'meow-bracket-inner)
    (define-key keymap (kbd "]") 'meow-bracket-outer)
    (define-key keymap (kbd "{") 'meow-brace-inner)
    (define-key keymap (kbd "}") 'meow-brace-outer)
    (define-key keymap (kbd "<") 'meow-forward-barf)
    (define-key keymap (kbd ">") 'meow-forward-slurp)
    (define-key keymap (kbd ",") 'meow-pop-marker)
    (define-key keymap (kbd ".") 'meow-find-ref)
    (define-key keymap (kbd ";") 'meow-comment)
    (define-key keymap (kbd ":") 'meow-M-x)
    (define-key keymap (kbd "&") 'meow-query-replace)
    (define-key keymap (kbd "@") 'pop-to-mark-command)
    (define-key keymap (kbd "<escape>") 'meow-last-buffer)
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
      (define-key map (kbd "<escape>") 'meow-escape-or-normal-modal)
      (define-key map (kbd "<tab>") 'meow-keypad-self-insert)
      (define-key map (kbd "TAB") 'meow-keypad-self-insert)
      (define-key map (kbd "<return>") 'meow-keypad-self-insert)
      (define-key map (kbd "RET") 'meow-keypad-self-insert))
    map)
  "Keymap for Meow keypad state.")

(provide 'meow-keymap)
;;; meow-keymap.el ends here
