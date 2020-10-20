;;; meow-var.el --- Meow variables
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
;; Internal variables and customizable variables.

;;; Code:

;; Customize

(defcustom meow-layout 'dvorak
  "Keyboard layout."
  :options '(dvorak dvp qwerty colemak)
  :type 'symbol
  :group 'meow)

;; Cursor types

(defvar meow-cursor-type-default 'box)
(defvar meow-cursor-type-normal 'box)
(defvar meow-cursor-type-motion 'box)
(defvar meow-cursor-type-insert '(bar . 4))
(defvar meow-cursor-type-keypad 'hollow)

;; Keypad states

(defvar meow--keypad-meta-prefix "m")
(defvar meow--keypad-both-prefix "M")
(defvar meow--keypad-literal-prefix " ")
(defvar meow--keypad-keys nil)
(defvar meow--keypad-previous-state nil)

(defvar meow--prefix-arg nil)
(defvar meow--use-literal nil)
(defvar meow--use-meta nil)
(defvar meow--use-both nil)

;;; Command fallback

(defvar meow-selection-command-fallback
  '((meow-copy . meow-keypad-start)
    (meow-change . meow-keypad-start))
  "Fallback commands for selection commands when there's no available selection.")

;;; KBD Macros
;; We use kbd macro instead of direct command/function invocation,
;; this allow us not hard code the command/function name.
;;
;; The benefit is an out-of-box integration support for other plugins, like: paredit.
;;
;; NOTE: meow is assuming user not modify vanilla Emacs keybindings, otherwise extra complexity will be introduced.

(defvar meow--kbd-undo "C-/"
  "KBD macro for command `undo'.")

(defvar meow--kbd-keyboard-quit "C-g"
  "KBD macro for command `keyboard-quit'.")

(defvar meow--kbd-find-ref "M-."
  "KBD macro for command `xref-find-definitions'.")

(defvar meow--kbd-pop-marker "M-,"
  "KBD macro for command `xref-pop-marker-stack'.")

(defvar meow--kbd-comment "M-;"
  "KBD macro for comment command.")

(defvar meow--kbd-kill-line "C-k"
  "KBD macro for command `kill-line'.")

(defvar meow--kbd-kill-whole-line "<C-S-backspace>"
  "KBD macro for command `kill-whole-line'.")

(defvar meow--kbd-delete-char "C-d"
  "KBD macro for command `delete-char'.")

(defvar meow--kbd-yank "C-y"
  "KBD macro for command `yank'.")

(defvar meow--kbd-yank-pop "M-y"
  "KBD macro for command `yank-pop'.")

(defvar meow--kbd-kill-ring-save "M-w"
  "KBD macro for command `kill-ring-save'.")

(defvar meow--kbd-kill-region "C-w"
  "KBD macro for command `kill-region'.")

(defvar meow--kbd-back-to-indentation "M-m"
  "KBD macro for command `back-to-indentation'.")

(defvar meow--kbd-indent-region "C-M-\\"
  "KBD macro for command `indent-region'.")

(defvar meow--kbd-delete-indentation "M-^"
  "KBD macro for command `delete-indentation'.")

(defvar meow--kbd-forward-slurp "C-)"
  "KBD macro for command forward slurp.")

(defvar meow--kbd-backward-slurp "C-("
  "KBD macro for command backward slurp.")

(defvar meow--kbd-forward-barf "C-}"
  "KBD macro for command forward barf.")

(defvar meow--kbd-backward-barf "C-{"
  "KBD macro for command backward barf.")

(defvar meow--kbd-scoll-up "C-v"
  "KBD macro for command `scroll-up'.")

(defvar meow--kbd-scoll-down "M-v"
  "KBD macro for command `scroll-down'.")

(defvar meow--kbd-just-one-space "M-SPC"
  "KBD macro for command `just-one-space.")

(defvar meow--kbd-wrap-round "M-("
  "KBD macro for command wrap round.")

(defvar meow--kbd-wrap-square "M-["
  "KBD macro for command wrap square.")

(defvar meow--kbd-wrap-curly "M-{"
  "KBD macro for command wrap curly.")

(defvar meow--kbd-wrap-string "M-\""
  "KBD macro for command wrap string.")

(defvar meow--kbd-excute-extended-command "M-x"
  "KBD macro for command `execute-extended-command'.")

(defvar meow--kbd-transpose-sexp "C-M-t"
  "KBD macro for command transpose sexp.")

(defvar meow--kbd-split-sexp "M-S"
  "KBD macro for command split sexp.")

(defvar meow--kbd-splice-sexp "M-s"
  "KBD macro for command splice sexp.")

(defvar meow--kbd-raise-sexp "M-r"
  "KBD macro for command raise sexp.")

(defvar meow--kbd-join-sexp "M-J"
  "KBD macro for command join sexp.")

(defvar meow--kbd-eval-last-exp "C-x C-e"
  "KBD macro for command eval last exp.")

(defvar meow--kbd-query-replace-regexp "C-M-%"
  "KBD macro for command `query-replace-regexp'.")

(defvar meow--kbd-query-replace "M-%"
  "KBD macro for command `query-replace'.")

(defvar meow--kbd-forward-line "C-n"
  "KBD macro for command `forward-line'.")

(defvar meow--kbd-backward-line "C-p"
  "KBD macro for command `backward-line'.")

(defvar meow--kbd-search-forward-regexp "C-M-s"
  "KBD macro for command `search-forward-regexp'.")

(defvar meow--kbd-search-backward-regexp "C-M-r"
  "KBD macro for command `search-backward-regexp'.")

(defvar-local meow--selection nil
  "Current selection.

Has a structure of (sel-type point mark).")

;;; Declare modes we need to activate normal state as default
;;; Other modes will use motion state as default.

(defvar meow-normal-state-mode-list
  '(fundamental-mode
    text-mode
    prog-mode
    conf-mode
    cider-repl-mode
    eshell-mode
    vterm-mode
    json-mode
    wdired-mode
    deft-mode
    pass-view-mode
    telega-chat-mode
    restclient-mode)
  "A list of modes should enable normal state.")

(defvar meow-auto-switch-exclude-mode-list
  '(ripgrep-search-mode
    ivy-occur-grep-mode)
  "A list of modes don't allow auto switch state.")

;;; Search

(defvar meow--last-search nil
  "Last search in command `meow-search'.")

;;; Temporary NORMAL state

(defvar-local meow--temp-normal nil
  "If we are in temporary normal state. ")

;;; Hooks

(defvar meow-switch-state-hook nil
  "Hooks run when switching state.")

(defvar meow-char-thing-table
  '((?r . round)
    (?s . square)
    (?c . curly)
    (?g . string)
    (?e . symbol)
    (?w . window)
    (?b . buffer)
    (?p . paragraph)
    (?l . line)
    (?d . defun)
    (?t . tag))
  "Mapping from char to thing.")

(defvar meow--selection-history nil
  "The history of selection.")

(provide 'meow-var)
;;; meow-var.el ends here
