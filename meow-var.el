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

(defcustom meow-use-cursor-position-hack nil
  "Whether to use cursor position hack."
  :group 'meow
  :type 'boolean)

(defcustom meow-use-enhanced-selection-effect nil
  "Whether to use enhanced cursor effect.

This will affect how selection is displayed."
  :group 'meow
  :type 'boolean)

(defcustom meow-expand-exclude-mode-list
  '(markdown-mode org-mode)
  "A list of major modes where after command expand should be disabled."
  :group 'meow
  :type '(repeat sexp))

(defcustom meow-selection-command-fallback
  '((meow-change . meow-change-char)
    (meow-kill . meow-C-k)
    (meow-cancel-selection . keyboard-quit)
    (meow-pop-selection . meow-pop-grab)
    (meow-beacon-change . meow-beacon-change-char))
  "Fallback commands for selection commands when there is no available selection."
  :group 'meow
  :type '(alist :key-type (function :tag "Command")
                :key-value (function :tag "Fallback")))

(defcustom meow-replace-state-name-list
  '((normal . "NORMAL")
    (motion . "MOTION")
    (keypad . "KEYPAD")
    (insert . "INSERT")
    (beacon . "BEACON"))
  "A list of mappings for how to display state in indicator."
  :group 'meow
  :type '(alist :key-type (symbol :tag "Meow state")
                :key-value (string :tag "Indicator")))

(defvar meow-indicator-face-alist
  '((normal . meow-normal-indicator)
    (motion . meow-motion-indicator)
    (keypad . meow-keypad-indicator)
    (insert . meow-insert-indicator)
    (beacon . meow-beacon-indicator))
  "Alist of meow states -> faces")

(defcustom meow-select-on-change t
  "Whether to activate region when exiting INSERT mode
 after `meow-change', `meow-change-char' and `meow-change-save'."
  :group 'meow
  :type 'boolean)

(defcustom meow-expand-hint-remove-delay 1.0
  "The delay before the position hint disappears."
  :group 'meow
  :type 'number)

(defcustom meow-expand-hint-counts
  '((word . 30)
    (line . 30)
    (block . 30)
    (find . 30)
    (till . 30))
  "The maximum numbers for expand hints of each type."
  :group 'meow
  :type '(alist :key-type (symbol :tag "Hint type")
                :key-value (integer :tag "Value")))

(defcustom meow-keypad-message t
  "Whether to log keypad messages in minibuffer."
  :group 'meow
  :type 'boolean)

(defcustom meow-keypad-self-insert-undefined t
  "Whether to self-insert a key in keypad mode if it is undefined"
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
    (?. . sentence))
  "Mapping from char to thing."
  :group 'meow
  :type '(alist :key-type (character :tag "Char")
                :key-value (symbol :tag "Thing")))

(defcustom meow-thing-selection-directions
  '((inner . forward)
    (bounds . backward)
    (beginning . backward)
    (end . forward))
  "Selection directions for each thing command."
  :group 'meow
  :type '(alist :key-type (symbol :tag "Command")
                :key-value (symbol :tag "Direction")))

(defcustom meow-display-thing-help t
  "Whether to display the help prompt for meow-inner/bounds/begin/end-of-thing."
  :group 'meow
  :type 'boolean)

(defcustom meow-keypad-describe-delay
  0.5
  "The delay in seconds before popup keybinding descriptions appear."
  :group 'meow
  :type 'number)

(defcustom meow-grab-fill-commands
  '(meow-query-replace meow-query-replace-regexp)
  "A list of commands that meow will auto fill with grabbed content."
  :group 'meow
  :type '(repeat function))

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

(defcustom meow-use-dynamic-face-color t
  "Whether to use dynamic calculated face color.

This option will affect the color of position hint and fake region cursor."
  :group 'meow
  :type 'boolean)

(defcustom meow-mode-state-list
  '((authinfo-mode . normal)
    (beancount-mode . normal)
    (bibtex-mode . normal)
    (cider-repl-mode . normal)
    (cider-test-report-mode . normal)
    (cider-browse-spec-view-mode . motion)
    (cargo-process-mode . normal)
    (conf-mode . normal)
    (deadgrep-edit-mode . normal)
    (deft-mode . normal)
    (diff-mode . normal)
    (ediff-mode . motion)
    (gud-mode . normal)
    (haskell-interactive-mode . normal)
    (help-mode . normal)
    (json-mode . normal)
    (jupyter-repl-mode . normal)
    (mix-mode . normal)
    (occur-edit-mode . normal)
    (pass-view-mode . normal)
    (prog-mode . normal)
    (py-shell-mode . normal)
    (restclient-mode . normal)
    (telega-chat-mode . normal)
    (term-mode . normal)
    (text-mode . normal)
    (vterm-mode . normal)
    (Custom-mode . normal))
  "A list of rules, each is (major-mode . init-state).

The init-state can be any state, including custom ones."
  :group 'meow
  :type '(alist :key-type (sexp :tag "Major-mode")
                :value-type (symbol :tag "Initial state")))

(defcustom meow-update-display-in-macro 'except-last-macro
  "Whether update cursor and mode-line when executing kbd macro.

Set to `nil' for no update in macro,
may not work well with some packages. (e.g. key-chord).

Set to `except-last-macro'
for no update only when executing last macro.

Set to `t' to always update.
"
  :group 'meow
  :type '(choice boolean
                 (const except-last-macro)))

(defcustom meow-expand-selection-type 'select
  "Whether to create transient selection for expand commands."
  :group 'meow
  :type '(choice (const select)
                 (const expand)))

(defcustom meow-keypad-leader-dispatch nil
  "The fallback dispatching in KEYPAD when there's no translation.

The value can be either a string or a keymap:
A keymap stands for a base keymap used for further translation.
A string stands for finding the keymap at a specified key binding.
Nil stands for taking leader keymap from `meow-keymap-alist'."
  :group 'meow
  :type '(choice (string :tag "Keys")
                 (variable :tag "Keymap")
                 (const nil)))

(defcustom meow-keypad-meta-prefix ?m
  "The prefix represent M- in KEYPAD state."
  :group 'meow
  :type 'character)

(defcustom meow-keypad-ctrl-meta-prefix ?g
  "The prefix represent C-M- in KEYPAD state."
  :group 'meow
  :type 'character)

(defcustom meow-keypad-literal-prefix 32
  "The prefix represent no modifier in KEYPAD state."
  :group 'meow
  :type 'character)

(defcustom meow-keypad-start-keys
  '((?c . ?c)
    (?h . ?h)
    (?x . ?x))
  "Alist of keys to begin keypad translation. When a key char is pressed,
it's corresponding value is appended to C- and the user is
prompted to finish the command."
  :group 'meow
  :type '(alist :key-type (character :tag "From")
                :value-type (character :tag "To")))

(defcustom meow-motion-remap-prefix "H-"
  "The prefix string used when remapping an occupied key in MOTION state.

For examples:
  \"C-x C-v\" will remap the occupied j to C-x C-v j.
  \"C-M-\" will remap the occupied j to C-M-j."
  :group 'meow
  :type 'string)

(defcustom meow-goto-line-function nil
  "Function to use in `meow-goto-line'.

Nil means find the command by key binding."
  :group 'meow
  :type '(choice function (const nil)))

(defvar meow-state-mode-alist
  '((normal . meow-normal-mode)
    (insert . meow-insert-mode)
    (keypad . meow-keypad-mode)
    (motion . meow-motion-mode)
    (beacon . meow-beacon-mode))
  "Alist of meow states -> modes")

(defvar meow-update-cursor-functions-alist
  '((meow--cursor-null-p . meow--update-cursor-default)
    (minibufferp         . meow--update-cursor-default)
    (meow-insert-mode-p  . meow--update-cursor-insert)
    (meow-normal-mode-p  . meow--update-cursor-normal)
    (meow-motion-mode-p  . meow--update-cursor-motion)
    (meow-keypad-mode-p  . meow--update-cursor-motion)
    (meow-beacon-mode-p  . meow--update-cursor-beacon)
    ((lambda () t)       . meow--update-cursor-default))
  "Alist of predicates to functions that set cursor type and color.")

(defvar meow-keypad-describe-keymap-function 'meow-describe-keymap
  "The function used to describe (KEYMAP) during keypad execution.

To integrate WhichKey-like features with keypad.
Currently, keypad is not working well with which-key,
so Meow ships a default `meow-describe-keymap'.
Use (setq meow-keypad-describe-keymap-function 'nil) to disable popup.")

(defvar meow-keypad-get-title-function 'meow-keypad-get-title
  "The function used to get the title of a keymap or command.")

;; Cursor types

(defvar meow-cursor-type-default 'box)
(defvar meow-cursor-type-normal 'box)
(defvar meow-cursor-type-motion 'box)
(defvar meow-cursor-type-beacon 'box)
(defvar meow-cursor-type-region-cursor '(bar . 2))
(defvar meow-cursor-type-insert '(bar . 2))
(defvar meow-cursor-type-keypad 'hollow)

;; Keypad states

(defvar meow--keypad-keys nil)
(defvar meow--keypad-previous-state nil)
(defvar meow--keypad-allow-quick-dispatch nil)

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

(defvar meow--kbd-goto-line "M-g g"
  "KBD macro for command `goto-line'.")

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

(defvar-local meow--current-state 'normal
  "A symbol represent current state.")

(defvar-local meow--end-kmacro-on-exit nil
  "Whether we end kmacro recording when exit insert state.")

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

(defvar meow--keypad-base-keymap nil
  "The keymap used to lookup keys in KEYPAD state.

Nil means to lookup in top-level.")

(defvar meow--beacon-backup-hl-line
  nil
  "Whether hl-line is enabled by user.")

(defvar meow--beacon-defining-kbd-macro nil
  "Whether we are defining kbd macro at BEACON state.

The value can be nil, quick or record.")

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
    (meow-to-block "→block")
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

(defvar meow--backup-redisplay-highlight-region-function
  redisplay-highlight-region-function)

(defvar meow--backup-redisplay-unhighlight-region-function
  redisplay-unhighlight-region-function)

(defvar meow--backup-var-delete-activate-region
  delete-active-region)

(provide 'meow-var)
;;; meow-var.el ends here
