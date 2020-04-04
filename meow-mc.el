;;; meow-mc.el --- Meow setup for Multiple Cursors
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

(defconst meow--mc-cmd-run-once
  '(meow-virtual-cursor
    meow-select-or-skip
    meow-quit
    meow-find-ref
    meow-pop-marker
    meow-last-buffer
    meow-keypad-self-insert
    meow-keypad-undo
    meow-undo))

(defconst meow--mc-cmd-run-for-all
  '(meow-forward-word
    meow-mark-or-back-word
    meow-head
    meow-head-select
    meow-tail
    meow-tail-select
    meow-prev-line
    meow-prev-line-select
    meow-next-line
    meow-next-line-select
    meow-mark-word
    meow-back-word
    meow-mark-or-backward-word
    meow-forward-word
    meow-block
    meow-exp
    meow-extend
    meow-line
    meow-keyboard-quit
    meow-flip
    meow-comment
    meow-insert-before
    meow-insert-after
    meow-insert-open
    meow-insert-replace
    meow-insert-exit
    meow-kill
    meow-join
    meow-delete
    meow-zap
    meow-yank
    meow-yank-pop
    meow-copy
    meow-wrap-round
    meow-wrap-square
    meow-wrap-curly
    meow-wrap-string
    meow-forward-slurp
    meow-forward-barf
    meow-raise-exp
    meow-split-exp
    meow-splice-exp
    meow-transpose-exp
    meow-join-exp
    meow-indent
    meow-back-to-indentation))

(defconst meow--mc-cursor-specific-vars
  '(meow--selection-history
    meow--selection
    meow-insert-mode
    meow-normal-mode))

(defun meow--mc-setup ()
  (dolist (cmd meow--mc-cmd-run-once)
    (add-to-list 'mc/cmds-to-run-once cmd))
  (dolist (cmd meow--mc-cmd-run-for-all)
    (add-to-list 'mc/cmds-to-run-for-all cmd))
  (dolist (it meow--mc-cursor-specific-vars)
    (add-to-list 'mc/cursor-specific-vars it)))

(provide 'meow-mc)
;;; meow-mc.el ends here
