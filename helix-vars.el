;;; helix-var.el --- Helix variables  -*- lexical-binding: t; -*-

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

(defgroup helix nil
  "Custom group for helix."
  :group 'helix-module)


(defvar helix-cursor-type-default 'box)
(defvar helix-cursor-type-normal 'box)
(defvar helix-cursor-type-motion 'box)
(defvar helix-cursor-type-beacon 'box)
(defvar helix-cursor-type-region-cursor '(bar . 2))
(defvar helix-cursor-type-insert '(bar . 2))
(defvar helix-cursor-type-keypad 'hollow)



(defgroup helix-cjk nil
  "CJK support"
  :prefix "helix-cjk-"
  :group 'helix)

(defvar helix-restriction-stack nil
  "List of previous restrictions.
Using `helix-with-restriction' stores the previous values of
`point-min' and `point-max' as a pair in this list.")

(defcustom helix-cjk-word-separating-categories
  '(;; Kanji
    (?C . ?H) (?C . ?K) (?C . ?k) (?C . ?A) (?C . ?G)
    ;; Hiragana
    (?H . ?C) (?H . ?K) (?H . ?k) (?H . ?A) (?H . ?G)
    ;; Katakana
    (?K . ?C) (?K . ?H) (?K . ?k) (?K . ?A) (?K . ?G)
    ;; half-width Katakana
    (?k . ?C) (?k . ?H) (?k . ?K) ; (?k . ?A) (?k . ?G)
    ;; full-width alphanumeric
    (?A . ?C) (?A . ?H) (?A . ?K) ; (?A . ?k) (?A . ?G)
    ;; full-width Greek
    (?G . ?C) (?G . ?H) (?G . ?K) ; (?G . ?k) (?G . ?A)
    )
  "List of pair (cons) of categories to determine word boundary
used in `helix-cjk-word-boundary-p'. See the documentation of
`word-separating-categories'. Use `describe-categories' to see
the list of categories."
  :type '(alist :key-type (choice character (const nil))
                :value-type (choice character (const nil)))
  :group 'helix-cjk)

(defcustom helix-cjk-word-combining-categories
  '(;; default value in word-combining-categories
    (nil . ?^) (?^ . nil)
    ;; Roman
    (?r . ?k) (?r . ?A) (?r . ?G)
    ;; half-width Katakana
    (?k . ?r) (?k . ?A) (?k . ?G)
    ;; full-width alphanumeric
    (?A . ?r) (?A . ?k) (?A . ?G)
    ;; full-width Greek
    (?G . ?r) (?G . ?k) (?G . ?A)
    )
  "List of pair (cons) of categories to determine word boundary
used in `helix-cjk-word-boundary-p'. See the documentation of
`word-combining-categories'. Use `describe-categories' to see the
list of categories."
  :type '(alist :key-type (choice character (const nil))
                :value-type (choice character (const nil)))
  :group 'helix-cjk)

;; Behaviors

(defcustom helix-use-cursor-position-hack nil
  "Whether to use cursor position hack."
  :group 'helix
  :type 'boolean)

(defcustom helix-use-enhanced-selection-effect nil
  "Whether to use enhanced cursor effect.

This will affect how selection is displayed."
  :group 'helix
  :type 'boolean)

(defcustom helix-expand-exclude-mode-list
  '(markdown-mode org-mode)
  "A list of major modes where after command expand should be disabled."
  :group 'helix
  :type '(repeat sexp))

(defcustom helix-selection-command-fallback
  '((helix-change . helix-change-char)
    (helix-kill . helix-C-k)
    (helix-cancel-selection . keyboard-quit)
    (helix-pop-selection . helix-pop-grab)
    (helix-beacon-change . helix-beacon-change-char))
  "Fallback commands for selection commands when there is no available selection."
  :group 'helix
  :type '(alist :key-type (function :tag "Command")
                :value-type (function :tag "Fallback")))

(defcustom helix-replace-state-name-list
  '((normal . "NORMAL")
    (motion . "MOTION")
    (keypad . "KEYPAD")
    (insert . "INSERT")
    (beacon . "BEACON"))
  "A list of mappings for how to display state in indicator."
  :group 'helix
  :type '(alist :key-type (symbol :tag "Helix state")
                :value-type (string :tag "Indicator")))

(defvar helix-indicator-face-alist
  '((normal . helix-normal-indicator)
    (motion . helix-motion-indicator)
    (keypad . helix-keypad-indicator)
    (insert . helix-insert-indicator)
    (beacon . helix-beacon-indicator))
  "Alist of helix states -> faces")

(defcustom helix-select-on-change t
  "Whether to activate region when exiting INSERT mode
 after `helix-change', `helix-change-char' and `helix-change-save'."
  :group 'helix
  :type 'boolean)

(defcustom helix-select-on-append nil
  "Whether to activate region when exiting INSERT mode after `helix-append'."
  :group 'helix
  :type 'boolean)

(defcustom helix-select-on-insert nil
  "Whether to activate region when exiting INSERT mode after `helix-insert'."
  :group 'helix
  :type 'boolean)

(defcustom helix-expand-hint-remove-delay 1.0
  "The delay before the position hint disappears."
  :group 'helix
  :type 'number)

(defcustom helix-expand-hint-counts
  '((word . 30)
    (line . 30)
    (block . 30)
    (find . 30)
    (till . 30))
  "The maximum numbers for expand hints of each type."
  :group 'helix
  :type '(alist :key-type (symbol :tag "Hint type")
                :value-type (integer :tag "Value")))

(defcustom helix-keypad-message t
  "Whether to log keypad messages in minibuffer."
  :group 'helix
  :type 'boolean)

(defcustom helix-keypad-self-insert-undefined t
  "Whether to self-insert a key in keypad mode if it is undefined"
  :group 'helix
  :type 'boolean)

(defcustom helix-char-thing-table
  '((?r . round)
    (?s . square)
    (?c . curly)
    (?g . string)
    (?e . symbol)
    (?w . window)
    (?b . buffer)
    (?p . paragraph)
    (?l . line)
    (?v . visual-line)
    (?d . defun)
    (?. . sentence))
  "Mapping from char to thing."
  :group 'helix
  :type '(alist :key-type (character :tag "Char")
                :value-type (symbol :tag "Thing")))

(defcustom helix-thing-selection-directions
  '((inner . forward)
    (bounds . backward)
    (beginning . backward)
    (end . forward))
  "Selection directions for each thing command."
  :group 'helix
  :type '(alist :key-type (symbol :tag "Command")
                :value-type (symbol :tag "Direction")))

(defvar helix-word-thing 'word
  "The \\='thing\\=' used for marking and movement by words.

The values is a \\='thing\\=' as understood by `thingatpt' - a symbol that will
be passed to `forward-thing' and `bounds-of-thing-at-point', which see.

This means that they must, at minimum, have a function as the value of their
`forward-op' symbol property (or the function should be defined as
`forward-SYMBOLNAME'). This function should accept a single argument, a number
N, and should move over the next N things, in either the forward or backward
direction depending on the sign of N. Examples of such functions include
`forward-word', `forward-symbol' and `forward-sexp', which `thingatpt' uses for
the `word', `symbol' and `sexp' things, respectively.")

(defvar helix-symbol-thing 'symbol
  "The \\='thing\\=' used for marking and movement by symbols.

The values is a \\='thing\\=' as understood by `thingatpt' - a symbol that will
be passed to `forward-thing' and `bounds-of-thing-at-point', which see.

This means that they must, at minimum, have a function as the value of their
`forward-op' symbol property (or the function should be defined as
`forward-SYMBOLNAME'). This function should accept a single argument, a number
N, and should move over the next N things, in either the forward or backward
direction depending on the sign of N. Examples of such functions include
`forward-word', `forward-symbol' and `forward-sexp', which `thingatpt' uses for
the `word', `symbol' and `sexp' things, respectively.")

(defcustom helix-display-thing-help t
  "Whether to display the help prompt for helix-inner/bounds/begin/end-of-thing."
  :group 'helix
  :type 'boolean)

(defcustom helix-keypad-describe-delay
  0.5
  "The delay in seconds before popup keybinding descriptions appear."
  :group 'helix
  :type 'number)

(defcustom helix-grab-fill-commands
  '(helix-query-replace helix-query-replace-regexp)
  "A list of commands that helix will auto fill with grabbed content."
  :group 'helix
  :type '(repeat function))

(defcustom helix-visit-collect-min-length 1
  "Minimal length when collecting symbols for `helix-visit'."
  :group 'helix
  :type 'integer)

(defcustom helix-visit-sanitize-completion t
  "Whether let `helix-visit' display symbol regexps in a sanitized format."
  :group 'helix
  :type 'boolean)

(defcustom helix-use-clipboard nil
  "Whether to use system clipboard."
  :group 'helix
  :type 'boolean)

(defcustom helix-use-keypad-when-execute-kbd t
  "Whether to use KEYPAD when the result of executing kbd string is a keymap."
  :group 'helix
  :type 'boolean)

(defcustom helix-use-dynamic-face-color t
  "Whether to use dynamic calculated face color.

This option will affect the color of position hint and fake region cursor."
  :group 'helix
  :type 'boolean)

(defcustom helix-mode-state-list
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
    (helpful-mode . normal)
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
    (vterm-mode . emacs)
    (Custom-mode . normal))
  "A list of rules, each is (major-mode . init-state).

The init-state can be any state, including custom ones."
  :group 'helix
  :type '(alist :key-type (sexp :tag "Major-mode")
                :value-type (symbol :tag "Initial state")))

(defcustom helix-update-display-in-macro 'except-last-macro
  "Whether update cursor and mode-line when executing kbd macro.

Set to `nil' for no update in macro,
may not work well with some packages. (e.g. key-chord).

Set to `except-last-macro'
for no update only when executing last macro.

Set to `t' to always update.
"
  :group 'helix
  :type '(choice boolean
                 (const except-last-macro)))

(defcustom helix-expand-selection-type 'select
  "Whether to create transient selection for expand commands."
  :group 'helix
  :type '(choice (const select)
                 (const expand)))

(defcustom helix-keypad-leader-dispatch nil
  "The fallback dispatching in KEYPAD when there's no translation.

The value can be either a string or a keymap:
A keymap stands for a base keymap used for further translation.
A string stands for finding the keymap at a specified key binding.
Nil stands for taking leader keymap from `helix-keymap-alist'."
  :group 'helix
  :type '(choice (string :tag "Keys")
                 (variable :tag "Keymap")
                 (const nil)))

(defcustom helix-keypad-meta-prefix ?m
  "The prefix represent M- in KEYPAD state."
  :group 'helix
  :type 'character)

(defcustom helix-keypad-ctrl-meta-prefix ?g
  "The prefix represent C-M- in KEYPAD state."
  :group 'helix
  :type 'character)

(defcustom helix-keypad-literal-prefix 32
  "The prefix represent no modifier in KEYPAD state."
  :group 'helix
  :type 'character)

(defcustom helix-keypad-start-keys
  '((?c . ?c)
    (?h . ?h)
    (?x . ?x))
  "Alist of keys to begin keypad translation. When a key char is pressed,
it's corresponding value is appended to C- and the user is
prompted to finish the command."
  :group 'helix
  :type '(alist :key-type (character :tag "From")
                :value-type (character :tag "To")))

(defcustom helix-motion-remap-prefix "H-"
  "The prefix string used when remapping an occupied key in MOTION state.

For examples:
  \"C-x C-v\" will remap the occupied j to C-x C-v j.
  \"C-M-\" will remap the occupied j to C-M-j."
  :group 'helix
  :type 'string)

(defcustom helix-goto-line-function nil
  "Function to use in `helix-goto-line'.

Nil means find the command by key binding."
  :group 'helix
  :type '(choice function (const nil)))

(defvar helix-state-mode-alist
  '((normal . helix-normal-mode)
    (insert . helix-insert-mode)
    ;; (keypad . helix-keypad-mode)
    ;; (motion . helix-motion-mode)
    ;; (beacon . helix-beacon-mode)

    )
  "Alist of helix states -> modes")

(defvar helix-update-cursor-functions-alist
  '((helix--cursor-null-p . helix--update-cursor-default)
    (minibufferp         . helix--update-cursor-default)
    (helix-insert-mode-p  . helix--update-cursor-insert)
    (helix-normal-mode-p  . helix--update-cursor-normal)
    ;; (helix-motion-mode-p  . helix--update-cursor-motion)
    ;; (helix-keypad-mode-p  . helix--update-cursor-motion)
    ;; (helix-beacon-mode-p  . helix--update-cursor-beacon)
    ((lambda () t)       . helix--update-cursor-default))
  "Alist of predicates to functions that set cursor type and color.")

(defvar helix-keypad-describe-keymap-function 'helix-describe-keymap
  "The function used to describe (KEYMAP) during keypad execution.

To integrate WhichKey-like features with keypad.
Currently, keypad is not working well with which-key,
so Helix ships a default `helix-describe-keymap'.
Use (setq helix-keypad-describe-keymap-function \\='nil) to disable popup.")

(defvar helix-keypad-get-title-function 'helix-keypad-get-title
  "The function used to get the title of a keymap or command.")

;; Cursor types


;; Keypad states

(defvar helix--keypad-keys nil)
(defvar helix--keypad-previous-state nil)
(defvar helix--keypad-allow-quick-dispatch nil)

(defvar helix--prefix-arg nil)
(defvar helix--use-literal nil)
(defvar helix--use-meta nil)
(defvar helix--use-both nil)

;;; KBD Macros
;; We use kbd macro instead of direct command/function invocation,
;; this allows us to avoid hard coding the command/function name.
;;
;; The benefit is an out-of-box integration support for other plugins, like: paredit.
;;
;; NOTE: helix assumes that the user does not modify vanilla Emacs keybindings, otherwise extra complexity will be introduced.

(defvar helix--kbd-undo "C-/"
  "KBD macro for command `undo'.")

(defvar helix--kbd-backward-char "C-b"
  "KBD macro for command `backward-char'.")

(defvar helix--kbd-forward-char "C-f"
  "KBD macro for command `forward-char'.")

(defvar helix--kbd-keyboard-quit "C-g"
  "KBD macro for command `keyboard-quit'.")

(defvar helix--kbd-find-ref "M-."
  "KBD macro for command `xref-find-definitions'.")

(defvar helix--kbd-pop-marker "M-,"
  "KBD macro for command `xref-pop-marker-stack'.")

(defvar helix--kbd-comment "M-;"
  "KBD macro for comment command.")

(defvar helix--kbd-kill-line "C-k"
  "KBD macro for command `kill-line'.")

(defvar helix--kbd-kill-whole-line "<C-S-backspace>"
  "KBD macro for command `kill-whole-line'.")

(defvar helix--kbd-delete-char "C-d"
  "KBD macro for command `delete-char'.")

(defvar helix--kbd-yank "C-y"
  "KBD macro for command `yank'.")

(defvar helix--kbd-yank-pop "M-y"
  "KBD macro for command `yank-pop'.")

(defvar helix--kbd-kill-ring-save "M-w"
  "KBD macro for command `kill-ring-save'.")

(defvar helix--kbd-kill-region "C-w"
  "KBD macro for command `kill-region'.")

(defvar helix--kbd-exchange-point-and-mark "C-x C-x"
  "KBD macro for command `exchange-point-and-mark'.")

(defvar helix--kbd-back-to-indentation "M-m"
  "KBD macro for command `back-to-indentation'.")

(defvar helix--kbd-indent-region "C-M-\\"
  "KBD macro for command `indent-region'.")

(defvar helix--kbd-delete-indentation "M-^"
  "KBD macro for command `delete-indentation'.")

(defvar helix--kbd-forward-slurp "C-)"
  "KBD macro for command forward slurp.")

(defvar helix--kbd-backward-slurp "C-("
  "KBD macro for command backward slurp.")

(defvar helix--kbd-forward-barf "C-}"
  "KBD macro for command forward barf.")

(defvar helix--kbd-backward-barf "C-{"
  "KBD macro for command backward barf.")

(defvar helix--kbd-scoll-up "C-v"
  "KBD macro for command `scroll-up'.")

(defvar helix--kbd-scoll-down "M-v"
  "KBD macro for command `scroll-down'.")

(defvar helix--kbd-just-one-space "M-SPC"
  "KBD macro for command `just-one-space.")

(defvar helix--kbd-wrap-round "M-("
  "KBD macro for command wrap round.")

(defvar helix--kbd-wrap-square "M-["
  "KBD macro for command wrap square.")

(defvar helix--kbd-wrap-curly "M-{"
  "KBD macro for command wrap curly.")

(defvar helix--kbd-wrap-string "M-\""
  "KBD macro for command wrap string.")

(defvar helix--kbd-excute-extended-command "M-x"
  "KBD macro for command `execute-extended-command'.")

(defvar helix--kbd-transpose-sexp "C-M-t"
  "KBD macro for command transpose sexp.")

(defvar helix--kbd-split-sexp "M-S"
  "KBD macro for command split sexp.")

(defvar helix--kbd-splice-sexp "M-s"
  "KBD macro for command splice sexp.")

(defvar helix--kbd-raise-sexp "M-r"
  "KBD macro for command raise sexp.")

(defvar helix--kbd-join-sexp "M-J"
  "KBD macro for command join sexp.")

(defvar helix--kbd-eval-last-exp "C-x C-e"
  "KBD macro for command eval last exp.")

(defvar helix--kbd-query-replace-regexp "C-M-%"
  "KBD macro for command `query-replace-regexp'.")

(defvar helix--kbd-query-replace "M-%"
  "KBD macro for command `query-replace'.")

(defvar helix--kbd-forward-line "C-n"
  "KBD macro for command `forward-line'.")

(defvar helix--kbd-backward-line "C-p"
  "KBD macro for command `backward-line'.")

(defvar helix--kbd-search-forward-regexp "C-M-s"
  "KBD macro for command `search-forward-regexp'.")

(defvar helix--kbd-search-backward-regexp "C-M-r"
  "KBD macro for command `search-backward-regexp'.")

(defvar helix--kbd-goto-line "M-g g"
  "KBD macro for command `goto-line'.")

(defvar helix--delete-region-function #'delete-region
  "The function used to delete the selection.

Allows support of modes that define their own equivalent of
`delete-region'.")

(defvar helix--insert-function #'insert
  "The function used to insert text in Normal state.

Allows support of modes that define their own equivalent of `insert'.")

(defvar-local helix--indicator nil
  "Indicator for current buffer.")

(defvar-local helix--selection nil
  "Current selection.

Has a structure of (sel-type point mark).")

;;; Hooks

(defvar helix-switch-state-hook nil
  "Hooks run when switching state.")

(defvar helix-insert-enter-hook nil
  "Hooks run when enter insert state.")

(defvar helix-insert-exit-hook nil
  "Hooks run when exit insert state.")

;;; Internal variables

(defvar-local helix--current-state 'normal
  "A symbol represent current state.")

(defvar-local helix--end-kmacro-on-exit nil
  "Whether we end kmacro recording when exit insert state.")

(defvar-local helix--temp-normal nil
  "Whether we are in temporary normal state. ")

(defvar helix--selection-history nil
  "The history of selections.")

(defvar helix--expand-nav-function nil
  "Current expand nav function.")

(defvar helix--last-find nil
  "The char for last find command.")

(defvar helix--last-till nil
  "The char for last till command.")

(defvar helix--visual-command nil
  "Current command to highlight.")

(defvar helix--keypad-this-command nil
  "Command name for current keypad execution.")

(defvar helix--expanding-p nil
  "Whether we are expanding.")

(defvar helix--keypad-keymap-description-activated nil
  "Whether KEYPAD keymap description is already activated.")

(defvar helix--keypad-help nil
  "If keypad in help mode.")

(defvar helix--keypad-base-keymap nil
  "The keymap used to lookup keys in KEYPAD state.

Nil means to lookup in top-level.")

(defvar helix--beacon-backup-hl-line
  nil
  "Whether hl-line is enabled by user.")

(defvar helix--beacon-defining-kbd-macro nil
  "Whether we are defining kbd macro at BEACON state.

The value can be nil, quick or record.")

(defvar-local helix--insert-pos nil
  "The position where we enter INSERT state.")

(defvar helix-full-width-number-position-chars
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

(defvar helix-cheatsheet-ellipsis "…"
  "Ellipsis character used in cheatsheet.")

(defvar helix-command-to-short-name-list
  '((helix-expand-0 . "ex →0")
    (helix-expand-1 . "ex →1")
    (helix-expand-2 . "ex →2")
    (helix-expand-3 . "ex →3")
    (helix-expand-4 . "ex →4")
    (helix-expand-5 . "ex →5")
    (helix-expand-6 . "ex →6")
    (helix-expand-7 . "ex →7")
    (helix-expand-8 . "ex →8")
    (helix-expand-9 . "ex →9")
    (digit-argument . "num-arg")
    (helix-inner-of-thing . "←thing→")
    (helix-bounds-of-thing . "[thing]")
    (helix-beginning-of-thing . "←thing")
    (helix-end-of-thing . "thing→")
    (helix-reverse . "reverse")
    (helix-prev . "↑")
    (helix-prev-expand . "ex ↑")
    (helix-next . "↓")
    (helix-next-expand . "ex ↓")
    (helix-head . "←")
    (helix-head-expand . "ex ←")
    (helix-tail . "→")
    (helix-tail-expand . "ex →")
    (helix-left . "←")
    (helix-left-expand . "ex ←")
    (helix-right . "→")
    (helix-right-expand . "ex →")
    (helix-yank . "yank")
    (helix-find . "find")
    (helix-find-expand . "ex find")
    (helix-till . "till")
    (helix-till-expand . "ex till")
    (helix-keyboard-quit . "C-g")
    (helix-cancel-selection . "quit sel")
    (helix-change . "chg")
    (helix-change-save . "chg-save")
    (helix-replace . "rep")
    (helix-replace-save . "rep-save")
    (helix-append . "append")
    (helix-open-below . "open ↓")
    (helix-insert . "insert")
    (helix-open-above . "open ↑")
    (helix-block . "block")
    (helix-to-block "→block")
    (helix-line . "line")
    (helix-delete . "del")
    (helix-search . "search")
    (helix-undo . "undo")
    (helix-undo-in-selection . "undo-sel")
    (helix-pop-search . "popsearch")
    (negative-argument . "neg-arg")
    (helix-quit . "quit")
    (helix-join . "join")
    (helix-kill . "kill")
    (helix-save . "save")
    (helix-next-word . "word→")
    (helix-next-symbol . "sym→")
    (helix-back-word . "←word")
    (helix-back-symbol . "←sym")
    (helix-pop-all-selection . "pop-sels")
    (helix-pop-selection . "pop-sel")
    (helix-mark-word . "←word→")
    (helix-mark-symbol . "←sym→")
    (helix-visit . "visit")
    (helix-kmacro-lines . "macro-ln")
    (helix-kmacro-matches . "macro-re")
    (helix-end-or-call-kmacro . "callmacro")
    (helix-cheatsheet . "help")
    (helix-keypad-describe-key . "desc-key")
    (helix-backspace . "backspace")
    (helix-pop-to-mark . "<-mark")
    (helix-unpop-to-mark . "mark->"))
  "A list of (command . short-name)")

(defcustom helix-replace-pop-command-start-indexes
  '((helix-replace . 1)
    (helix-replace-char . 1)
    (helix-replace-save . 2))
  "Alist of commands and their starting indices for use by `helix-replace-pop'.

If `helix-replace-pop' is run and the previous command is not
`helix-replace-pop' or a command which is present in this alist,
`helix-replace-pop' signals an error."
  :type '(alist :key-type function :value-type natnum))

(defvar helix--replace-pop-index nil
  "The index of the previous replacement in the `kill-ring'.
See also the command `helix-replace-pop'.")

(defvar helix--replace-start-marker (make-marker)
  "The beginning of the replaced text.

This marker stays before any text inserted at the location, to
account for any automatic formatting that happens after inserting
the replacement text.")

;;; Backup variables

(defvar helix--backup-var-delete-activae-region nil
  "The backup for `delete-active-region'.

It is used to restore its value when disable `helix'.")

(defvar helix--backup-redisplay-highlight-region-function
  redisplay-highlight-region-function)

(defvar helix--backup-redisplay-unhighlight-region-function
  redisplay-unhighlight-region-function)

(defvar helix--backup-var-delete-activate-region
  delete-active-region)


(provide 'helix-vars)
