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

(defvar meow-leader-base-keymap
  (let ((keymap (make-sparse-keymap)))

    (suppress-keymap keymap t)

    (define-key keymap (kbd "SPC") 'meow-space)
    (define-key keymap (kbd "x") 'meow-keypad-start)
    (define-key keymap (kbd "c") 'meow-keypad-start)
    (define-key keymap (kbd "e") 'meow-eval-last-exp)
    keymap)
  "A base keymap for leader key.")

(defvar meow--leader-mode-keymaps nil
  "Leader keymaps used for major modes.")

(defvar meow-insert-state-keymap
  (let ((keymap (make-keymap)))
    (define-key keymap (kbd "<escape>") 'meow-insert-exit)
    keymap)
  "Keymap for Meow insert state.")

(defvar meow-normal-state-keymap
  (let ((keymap (make-keymap)))

    (suppress-keymap keymap t)

    ;; Keyboard Quit
    (define-key keymap (kbd "g") 'meow-keyboard-quit)

    ;; Prefix Argument
    (define-key keymap (kbd "-") 'negative-argument)
    (define-key keymap (kbd "'") 'universal-argument)
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

    ;; Navgation/Selection
    (define-key keymap (kbd "m") 'meow-mark-or-backward-word)
    (define-key keymap (kbd "w") 'meow-forward-word)
    (define-key keymap (kbd "h") 'meow-head)
    (define-key keymap (kbd "H") 'meow-head-select)
    (define-key keymap (kbd "t") 'meow-tail)
    (define-key keymap (kbd "T") 'meow-tail-select)
    (define-key keymap (kbd "p") 'meow-prev-line)
    (define-key keymap (kbd "P") 'meow-prev-line-select)
    (define-key keymap (kbd "n") 'meow-next-line)
    (define-key keymap (kbd "N") 'meow-next-line-select)
    (define-key keymap (kbd "e") 'meow-exp)
    (define-key keymap (kbd "l") 'meow-line)
    (define-key keymap (kbd "b") 'meow-block)
    (define-key keymap (kbd "f") 'meow-forwarding)
    (define-key keymap (kbd "r") 'meow-reverse)

    ;; Kill
    (define-key keymap (kbd "k") 'meow-kill)
    (define-key keymap (kbd "j") 'meow-join)
    (define-key keymap (kbd "d") 'meow-delete)
    (define-key keymap (kbd "z") 'meow-zap)

    ;; Insert State
    (define-key keymap (kbd "i") 'meow-insert)
    (define-key keymap (kbd "a") 'meow-append)
    (define-key keymap (kbd "o") 'meow-open)
    (define-key keymap (kbd "x") 'meow-change)

    ;; Clipboard
    (define-key keymap (kbd "c") 'meow-copy)
    (define-key keymap (kbd "y") 'meow-yank)
    (define-key keymap (kbd "Y") 'meow-yank-pop)
    (define-key keymap (kbd "X") 'meow-replace)

    ;; Parenthese Operation
    (define-key keymap (kbd "(") 'meow-wrap-round)
    (define-key keymap (kbd "[") 'meow-wrap-square)
    (define-key keymap (kbd "{") 'meow-wrap-curly)
    (define-key keymap (kbd "\"") 'meow-wrap-string)
    (define-key keymap (kbd ")") 'meow-forward-slurp)
    (define-key keymap (kbd "}") 'meow-forward-barf)
    (define-key keymap (kbd "R") 'meow-raise-sexp)
    (define-key keymap (kbd "S") 'meow-split-sexp)
    (define-key keymap (kbd "U") 'meow-splice-sexp)
    (define-key keymap (kbd "O") 'meow-transpose-sexp)
    (define-key keymap (kbd "J") 'meow-join-sexp)

    ;; Pagination
    (define-key keymap (kbd "F") 'meow-page-down)
    (define-key keymap (kbd "B") 'meow-page-up)

    ;; Search
    (define-key keymap (kbd "s") 'meow-search)
    (define-key keymap (kbd "v") 'meow-visit)

    ;; Others
    (define-key keymap (kbd "q") 'meow-quit)
    (define-key keymap (kbd "u") 'meow-undo)
    (define-key keymap (kbd "/") 'meow-query-replace)
    (define-key keymap (kbd "<") 'beginning-of-buffer)
    (define-key keymap (kbd ">") 'end-of-buffer)
    (define-key keymap (kbd ",") 'meow-pop-marker)
    (define-key keymap (kbd ".") 'meow-find-ref)
    (define-key keymap (kbd ";") 'meow-comment)
    (define-key keymap (kbd "\\") 'meow-indent)
    (define-key keymap (kbd "<tab>") 'meow-back-to-indentation)
    (define-key keymap (kbd "TAB") 'meow-back-to-indentation)
    (define-key keymap (kbd "<escape>") 'meow-last-buffer)

    (define-key keymap (kbd "@") 'meow-last-pos)

    (when (eq meow-layout 'qwerty)
      (define-key keymap (kbd "f") 'meow-forward)
      (define-key keymap (kbd "F") 'meow-forward-select)
      (define-key keymap (kbd "b") 'meow-backward)
      (define-key keymap (kbd "B") 'meow-backward-select)
      (define-key keymap (kbd "t") 'meow-forwarding)
      (define-key keymap (kbd "h") 'meow-block)
      (define-key keymap (kbd "w") 'meow-mark-or-backward-word)
      (define-key keymap (kbd "e") 'meow-forward-word)
      (define-key keymap (kbd "m") 'meow-join)
      (define-key keymap (kbd "j") 'meow-exp))

    keymap)
  "Keymap for Meow normal state.")

;;;###autoload
(defvar meow-motion-state-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [escape] 'meow-last-buffer)
    keymap)
  "Keymap for Meow motion state.")

;;;###autoload
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
