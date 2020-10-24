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
    ;; IMPORTANT
    (define-key keymap (kbd "SPC") 'meow-space)
    (define-key keymap (kbd "<escape>") 'meow-temp-normal)
    (define-key keymap (kbd "TAB") 'other-window)
    (define-key keymap (kbd "u") 'universal-argument)
    (define-key keymap (kbd "m") 'meow-keypad-start)
    (define-key keymap (kbd "M") 'meow-keypad-start)
    (define-key keymap (kbd "x") 'meow-keypad-start)
    (define-key keymap (kbd "h") 'meow-keypad-start)
    (define-key keymap (kbd "c") 'meow-keypad-start)
    ;; NON-IMPORTANT
    (define-key keymap (kbd "h") 'meow-keypad-start)
    (define-key keymap (kbd "c") 'meow-keypad-start)
    (define-key keymap (kbd "e") 'meow-eval-last-exp)
    (define-key keymap (kbd "r") 'meow-raise-sexp)
    (define-key keymap (kbd "S") 'meow-split-sexp)
    (define-key keymap (kbd "s") 'meow-splice-sexp)
    (define-key keymap (kbd "t") 'meow-transpose-sexp)
    (define-key keymap (kbd "j") 'meow-join-sexp)
    (define-key keymap (kbd ",") 'meow-pop-marker)
    (define-key keymap (kbd ".") 'meow-find-ref)
    (define-key keymap (kbd ";") 'meow-comment)
    (define-key keymap (kbd "q") 'meow-quit)
    (define-key keymap (kbd "o") 'delete-other-windows)
    (define-key keymap (kbd "'") 'meow-wrap-string)
    (define-key keymap (kbd "(") 'meow-wrap-round)
    (define-key keymap (kbd "[") 'meow-wrap-square)
    (define-key keymap (kbd "{") 'meow-wrap-curly)
    (define-key keymap (kbd "}") 'meow-forward-barf)
    (define-key keymap (kbd ")") 'meow-forward-slurp)
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
    (define-key keymap (kbd "SPC") meow-leader-keymap)
    (define-key keymap (kbd "-") 'negative-argument)
    (define-key keymap (kbd "*") 'meow-expand-0)
    (define-key keymap (kbd "=") 'meow-expand-9)
    (define-key keymap (kbd "!") 'meow-expand-8)
    (define-key keymap (kbd "[") 'meow-expand-7)
    (define-key keymap (kbd "]") 'meow-expand-6)
    (define-key keymap (kbd "{") 'meow-expand-5)
    (define-key keymap (kbd "+") 'meow-expand-4)
    (define-key keymap (kbd "}") 'meow-expand-3)
    (define-key keymap (kbd ")") 'meow-expand-2)
    (define-key keymap (kbd "(") 'meow-expand-1)
    (define-key keymap (kbd ";") 'meow-reverse)
    (define-key keymap (kbd ",") 'meow-inner-of-thing)
    (define-key keymap (kbd ".") 'meow-bounds-of-thing)
    (define-key keymap (kbd "<") 'meow-beginning-of-thing)
    (define-key keymap (kbd ">") 'meow-end-of-thing)
    (define-key keymap (kbd "a") 'meow-append)
    (define-key keymap (kbd "A") 'meow-open-below)
    (define-key keymap (kbd "b") 'meow-back-word)
    (define-key keymap (kbd "B") 'meow-back-symbol)
    (define-key keymap (kbd "c") 'meow-change)
    (define-key keymap (kbd "C") 'meow-change-save)
    (define-key keymap (kbd "d") 'meow-delete)
    (define-key keymap (kbd "D") 'backward-delete-char)
    (define-key keymap (kbd "e") 'meow-line)
    (define-key keymap (kbd "f") 'meow-find)
    (define-key keymap (kbd "F") 'meow-find-expand)
    (define-key keymap (kbd "g") 'meow-keyboard-quit)
    (define-key keymap (kbd "G") 'goto-line)
    (define-key keymap (kbd "h") 'meow-head)
    (define-key keymap (kbd "H") 'meow-head-expand)
    (define-key keymap (kbd "i") 'meow-insert)
    (define-key keymap (kbd "I") 'meow-open-above)
    (define-key keymap (kbd "j") 'meow-join)
    (define-key keymap (kbd "J") 'delete-indentation)
    (define-key keymap (kbd "k") 'meow-kill)
    (define-key keymap (kbd "K") 'meow-kill-whole-line)
    (define-key keymap (kbd "l") 'meow-till)
    (define-key keymap (kbd "L") 'meow-till-expand)
    (define-key keymap (kbd "m") 'meow-mark-word)
    (define-key keymap (kbd "M") 'meow-mark-symbol)
    (define-key keymap (kbd "n") 'meow-next)
    (define-key keymap (kbd "N") 'meow-next-expand)
    (define-key keymap (kbd "o") 'meow-block)
    (define-key keymap (kbd "O") 'meow-block-expand)
    (define-key keymap (kbd "p") 'meow-prev)
    (define-key keymap (kbd "P") 'meow-prev-expand)
    (define-key keymap (kbd "q") 'meow-quit)
    (define-key keymap (kbd "r") 'meow-replace)
    (define-key keymap (kbd "R") 'meow-replace-save)
    (define-key keymap (kbd "s") 'meow-search)
    (define-key keymap (kbd "t") 'meow-tail)
    (define-key keymap (kbd "T") 'meow-tail-expand)
    (define-key keymap (kbd "u") 'undo)
    (define-key keymap (kbd "v") 'meow-visit)
    (define-key keymap (kbd "w") 'meow-next-word)
    (define-key keymap (kbd "W") 'meow-next-symbol)
    (define-key keymap (kbd "x") 'meow-save)
    (define-key keymap (kbd "y") 'meow-yank)
    (define-key keymap (kbd "Y") 'meow-yank-after)
    (define-key keymap (kbd "z") 'meow-pop-selection)
    (define-key keymap (kbd "Z") 'meow-pop-all-selection)
    (define-key keymap (kbd "&") 'meow-query-replace)
    (define-key keymap (kbd "@") 'recenter-top-bottom)
    (define-key keymap (kbd "^") 'meow-pop-to-mark)
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
