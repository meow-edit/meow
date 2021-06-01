;;; meow-var.el --- Meow variables  -*- lexical-binding: t; -*-

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

(defgroup meow nil
  "Custom group for meow."
  :group 'meow-module)

;; Behaviors

(defcustom meow-expand-exclude-mode-list
  '(markdown-mode org-mode)
  "A list of major modes where after command expand should be disabled."
  :group 'meow
  :type 'list)

(defcustom meow-selection-command-fallback
  '((meow-replace . meow-replace-char)
    (meow-change . meow-change-char)
    (meow-save . meow-save-char)
    (meow-kill . meow-C-k)
    (meow-delete . meow-C-d)
    (meow-cancel-selection . meow-keyboard-quit)
    (meow-pop . meow-pop-grab))
  "Fallback commands for selection commands when there is no available selection."
  :group 'meow
  :type 'list)

(defcustom meow-extend-syntax
  "^-><()"
  "Syntax description for thing `extend'."
  :group 'meow
  :type 'string)

(defcustom meow-replace-state-name-list
  '((normal . "NORMAL")
    (motion . "MOTION")
    (keypad . "KEYPAD")
    (insert . "INSERT"))
  "A list of mappings for how to display state in indicator."
  :group 'meow
  :type 'list)

(defcustom meow-select-on-change t
  "Whether to activate region when exiting INSERT mode after `meow-change', `meow-change-char' and `meow-change-save'."
  :group 'meow
  :type 'boolean)

(defcustom meow-expand-hint-remove-delay 1.0
  "The delay before the position hint disappears."
  :group 'meow
  :type 'integer)

(defcustom meow-keypad-message t
  "Whether to log keypad messages in minibuffer."
  :group 'meow
  :type 'boolean)

(defcustom meow-char-thing-table
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
    (?i . indent)
    (?x . extend))
  "Mapping from char to thing."
  :group 'meow
  :type 'list)

(defcustom meow-display-thing-help t
  "Whether to display the help prompt for meow-inner/bounds/begin/end-of-thing."
  :group 'meow
  :type :boolean)

(defcustom meow-keypad-describe-delay
  0.5
  "The delay in seconds before popup keybinding descriptions appear."
  :group 'meow
  :type 'float)

(defcustom meow-grab-fill-commands
  '(meow-query-replace meow-query-replace-regexp)
  "A list of commands that meow will auto fill with grabbed content."
  :group 'meow
  :type 'list)

(defcustom meow-visit-collect-min-length 1
  "Minimal length when collecting symbols for `meow-visit'."
  :group 'meow
  :type 'integer)

(defcustom meow-visit-sanitize-completion t
  "Whether let `meow-visit' display symbol regexps in a sanitized format."
  :group 'meow
  :type 'boolean)

(defcustom meow-use-clipboard nil
  "Whether to use system clipboard."
  :group 'meow
  :type 'boolean)

(defcustom meow-use-keypad-when-execute-kbd t
  "Whether to use KEYPAD when the result of executing kbd string is a keymap."
  :group 'meow
  :type 'boolean)

(defcustom meow-mode-state-list
  '((cider-browse-spec-view-mode . motion)
    (beancount-mode . normal)
    (fundamental-mode . normal)
    (occur-edit-mode . normal)
    (text-mode . normal)
    (prog-mode . normal)
    (conf-mode . normal)
    (cider-repl-mode . normal)
    (eshell-mode . normal)
    (vterm-mode . normal)
    (json-mode . normal)
    (deft-mode . normal)
    (pass-view-mode . normal)
    (telega-chat-mode . normal)
    (restclient-mode . normal)
    (help-mode . normal)
    (deadgrep-edit-mode . normal)
    (mix-mode . normal)
    (py-shell-mode . normal)
    (term-mode . normal)
    (Custom-mode . normal))
  "A list of rules, each is (major-mode . init-state).

The init-state can only be `motion' or `normal', and `motion' have a higher priority."
  :group 'meow
  :type 'list)

(defvar meow-keypad-describe-keymap-function 'meow-describe-keymap
  "The function used to describe (KEYMAP) during keypad execution.

To integrate WhichKey-like features with keypad.
Currently, keypad is not working well with which-key,
so Meow ships a default `meow-describe-keymap'.
Use (setq meow-keypad-describe-keymap-function 'nil) to disable popup.")

;; Cursor types

(defvar meow-cursor-type-default 'box)
(defvar meow-cursor-type-normal 'box)
(defvar meow-cursor-type-motion 'box)
(defvar meow-cursor-type-insert '(bar . 4))
(defvar meow-cursor-type-keypad 'hollow)

;; Keypad states

(defvar meow--keypad-meta-prefix ?m)
(defvar meow--keypad-both-prefix ?g)
(defvar meow--keypad-literal-prefix 32)
(defvar meow--keypad-keys nil)
(defvar meow--keypad-previous-state nil)

(defvar meow--prefix-arg nil)
(defvar meow--use-literal nil)
(defvar meow--use-meta nil)
(defvar meow--use-both nil)

;;; KBD Macros
;; We use kbd macro instead of direct command/function invocation,
;; this allows us to avoid hard coding the command/function name.
;;
;; The benefit is an out-of-box integration support for other plugins, like: paredit.
;;
;; NOTE: meow assumes that the user does not modify vanilla Emacs keybindings, otherwise extra complexity will be introduced.

(defvar meow--kbd-undo "C-/"
  "KBD macro for command `undo'.")

(defvar meow--kbd-backward-char "C-b"
  "KBD macro for command `backward-char'.")

(defvar meow--kbd-forward-char "C-f"
  "KBD macro for command `forward-char'.")

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

(defvar-local meow--indicator nil
  "Indicator for current buffer.")

(defvar-local meow--selection nil
  "Current selection.

Has a structure of (sel-type point mark).")

;;; Hooks

(defvar meow-switch-state-hook nil
  "Hooks run when switching state.")

(defvar meow-insert-enter-hook nil
  "Hooks run when enter insert state.")

(defvar meow-insert-exit-hook nil
  "Hooks run when exit insert state.")

;;; Internal variables

(defvar-local meow--temp-normal nil
  "Whether we are in temporary normal state. ")

(defvar meow--selection-history nil
  "The history of selections.")

(defvar meow--expand-nav-function nil
  "Current expand nav function.")

(defvar meow--last-find nil
  "The char for last find command.")

(defvar meow--last-till nil
  "The char for last till command.")

(defvar meow--visual-command nil
  "Current command to highlight.")

(defvar meow--keypad-this-command nil
  "Command name for current keypad execution.")

(defvar meow--expanding-p nil
  "Whether we are expanding.")

(defvar meow--keypad-keymap-description-activated nil
  "Whether KEYPAD keymap description is already activated.")

(defvar meow--keypad-help nil
  "If keypad in help mode.")

(defvar meow--motion-overwrite-keys
  '(" ")
  "A list of keybindings to overwrite in MOTION state.")

(defvar-local meow--insert-pos nil
  "The position where we enter INSERT state.")

(defvar meow-full-width-number-position-chars
  '((0 . "０")
    (1 . "１")
    (2 . "２")
    (3 . "３")
    (4 . "４")
    (5 . "５")
    (6 . "６")
    (7 . "７")
    (8 . "８")
    (9 . "９"))
  "Map number to full-width character.")

(defvar meow-cheatsheet-ellipsis "…"
  "Ellipsis character used in cheatsheet.")

(defvar meow-command-to-short-name-list
  '((meow-expand-0 . "ex →0")
    (meow-expand-1 . "ex →1")
    (meow-expand-2 . "ex →2")
    (meow-expand-3 . "ex →3")
    (meow-expand-4 . "ex →4")
    (meow-expand-5 . "ex →5")
    (meow-expand-6 . "ex →6")
    (meow-expand-7 . "ex →7")
    (meow-expand-8 . "ex →8")
    (meow-expand-9 . "ex →9")
    (digit-argument . "num-arg")
    (meow-inner-of-thing . "←thing→")
    (meow-bounds-of-thing . "[thing]")
    (meow-beginning-of-thing . "←thing")
    (meow-end-of-thing . "thing→")
    (meow-reverse . "reverse")
    (meow-prev . "↑")
    (meow-prev-expand . "ex ↑")
    (meow-next . "↓")
    (meow-next-expand . "ex ↓")
    (meow-head . "←")
    (meow-head-expand . "ex ←")
    (meow-tail . "→")
    (meow-tail-expand . "ex →")
    (meow-left . "←")
    (meow-left-expand . "ex ←")
    (meow-right . "→")
    (meow-right-expand . "ex →")
    (meow-yank . "yank")
    (meow-find . "find")
    (meow-find-expand . "ex find")
    (meow-till . "till")
    (meow-till-expand . "ex till")
    (meow-keyboard-quit . "C-g")
    (meow-cancel-selection . "quit sel")
    (meow-change . "chg")
    (meow-change-save . "chg-save")
    (meow-replace . "rep")
    (meow-replace-save . "rep-save")
    (meow-append . "append")
    (meow-open-below . "open ↓")
    (meow-insert . "insert")
    (meow-open-above . "open ↑")
    (meow-block . "block")
    (meow-line . "line")
    (meow-delete . "del")
    (meow-search . "search")
    (meow-undo . "undo")
    (meow-undo-in-selection . "undo-sel")
    (meow-pop-search . "popsearch")
    (negative-argument . "neg-arg")
    (meow-quit . "quit")
    (meow-join . "join")
    (meow-kill . "kill")
    (meow-save . "save")
    (meow-next-word . "word→")
    (meow-next-symbol . "sym→")
    (meow-back-word . "←word")
    (meow-back-symbol . "←sym")
    (meow-pop-all-selection . "pop-sels")
    (meow-pop-selection . "pop-sel")
    (meow-mark-word . "←word→")
    (meow-mark-symbol . "←sym→")
    (meow-visit . "visit")
    (meow-kmacro-lines . "macro-ln")
    (meow-kmacro-matches . "macro-re")
    (meow-end-or-call-kmacro . "callmacro")
    (meow-cheatsheet . "help")
    (meow-keypad-describe-key . "desc-key")
    (meow-backspace . "backspace"))
  "A list of (command . short-name)")

;;; Backup variables

(defvar meow--backup-var-delete-activae-region nil
  "The backup for `delete-active-region'.

It is used to restore its value when disable `meow'.")

(provide 'meow-var)
;;; meow-var.el ends here
