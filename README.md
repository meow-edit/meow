[![MELPA](https://melpa.org/packages/meow-badge.svg)](https://melpa.org/#/meow)

![Logo](meow.svg)

> Less is more

Meow aims to let users write less configuration, but get better integration. To Remember fewer commands, but can do more things.

[中文文档](README_CN.md)

# Installation

```emacs-lisp
(use-package meow
  :init
  (meow-global-mode 1)
  ;; meow-setup is your custom function, see below
  (meow-setup)
```

Here is the [definition for meow-setup](#2-almost-no-default-keybinding).

# What is Meow?

Meow is a complete modal editing, and this section will explain its highlights and why it's here.

## 1. Four Modes

Meow have 4 modes.

* `NORMAL`: The default mode for text editing, commands bound to single keys. Note there's no default keybinding in Meow for NORMAL mode.
* `INSERT`: The mode for text insertion, press <kbd>ESC</kbd> let you back to `NORMAL` mode.
* `MOTION`: The default mode for all kinds of special modes, only <kbd>SPC</kbd> is bound to Leader, and the origin command on <kbd>SPC</kbd> is bound to <kbd>SPC SPC</kbd>.
* `KEYPAD`: A temporary mode to simulate input with modifiers(ctrl, meta) with single-key sequences.

## 2. Almost no default keybinding

Meow provides a set of complete modal editing commands. User have to build their own keymap. Following is some recommended schemas, that you can use directly or as your startup point.

<details>
    <summary><code>meow-setup</code> For Qwerty, Vim Like</summary>

```emacs-lisp
(defun meow-setup ()
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . meow-motion-origin-command)
   '("k" . meow-motion-origin-command)
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . meow-change-save)
   '("d" . meow-delete)
   '("x" . meow-line)
   '("f" . meow-find)
   '("F" . meow-find-expand)
   '("g" . meow-keyboard-quit)
   '("G" . goto-line)
   '("h" . meow-head)
   '("H" . meow-head-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("m" . meow-join)
   '("M" . delete-indentation)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("T" . meow-till-expand)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-block-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("R" . meow-replace-save)
   '("n" . meow-search)
   '("N" . meow-pop-search)
   '("l" . meow-tail)
   '("L" . meow-tail-expand)
   '("u" . undo)
   '("v" . meow-visit)
   '("e" . meow-next-word)
   '("e" . meow-next-symbol)
   '("y" . meow-save)
   '("p" . meow-yank)
   '("z" . meow-pop-selection)
   '("Z" . meow-pop-all-selection)
   '("&" . meow-query-replace)
   '("%" . meow-query-replace-regexp)
   '("<escape>" . meow-last-buffer)))
```
</details>

<details>
    <summary><code>meow-setup</code> For Dvorak Simplified</summary>

```emacs-lisp
(defun meow-setup ()
  (meow-leader-define-key
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . meow-change-save)
   '("d" . meow-delete)
   '("e" . meow-line)
   '("f" . meow-find)
   '("F" . meow-find-expand)
   '("g" . meow-keyboard-quit)
   '("G" . goto-line)
   '("h" . meow-head)
   '("H" . meow-head-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-join)
   '("J" . delete-indentation)
   '("k" . meow-kill)
   '("l" . meow-till)
   '("L" . meow-till-expand)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-block-expand)
   '("p" . meow-prev)
   '("P" . meow-prev-expand)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("R" . meow-replace-save)
   '("s" . meow-search)
   '("S" . meow-pop-search)
   '("t" . meow-tail)
   '("T" . meow-tail-expand)
   '("u" . undo)
   '("v" . meow-visit)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-save)
   '("y" . meow-yank)
   '("z" . meow-pop-selection)
   '("Z" . meow-pop-all-selection)
   '("&" . meow-query-replace)
   '("%" . meow-query-replace-regexp)
   '("<escape>" . meow-last-buffer)))
```
</details>

<details>
    <summary><code>meow-setup</code> for Dvorak Programmer</summary>

```emacs-lisp
(defun meow-setup ()
  (meow-normal-define-key
   '("*" . meow-expand-0)
   '("=" . meow-expand-9)
   '("!" . meow-expand-8)
   '("[" . meow-expand-7)
   '("]" . meow-expand-6)
   '("{" . meow-expand-5)
   '("+" . meow-expand-4)
   '("}" . meow-expand-3)
   '(")" . meow-expand-2)
   '("(" . meow-expand-1)
   '("1" . digit-argument)
   '("2" . digit-argument)
   '("3" . digit-argument)
   '("4" . digit-argument)
   '("5" . digit-argument)
   '("6" . digit-argument)
   '("7" . digit-argument)
   '("8" . digit-argument)
   '("9" . digit-argument)
   '("0" . digit-argument)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . meow-change-save)
   '("d" . meow-delete)
   '("e" . meow-line)
   '("f" . meow-find)
   '("F" . meow-find-expand)
   '("g" . meow-keyboard-quit)
   '("G" . goto-line)
   '("h" . meow-head)
   '("H" . meow-head-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-join)
   '("J" . delete-indentation)
   '("k" . meow-kill)
   '("l" . meow-till)
   '("L" . meow-till-expand)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-block-expand)
   '("p" . meow-prev)
   '("P" . meow-prev-expand)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("R" . meow-replace-save)
   '("s" . meow-search)
   '("S" . meow-pop-search)
   '("t" . meow-tail)
   '("T" . meow-tail-expand)
   '("u" . undo)
   '("v" . meow-visit)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-save)
   '("y" . meow-yank)
   '("z" . meow-pop-selection)
   '("Z" . meow-pop-all-selection)
   '("&" . meow-query-replace)
   '("%" . meow-query-replace-regexp)
   '("<escape>" . meow-last-buffer)))
```
</details>

<code>meow-setup</code>For Colemak, TBD


## 3. NORMAL, navigation is also selection
The idea borrowed from Kakoune.

The navigation commands, except single character movement, will also activate the selection. For example, Moving forward a word will mark from previous position to current position. So you got two meaningful positions with single command. When selection is activated, use <kbd>i</kbd> to insert at beginning, or <kbd>a</kbd> to insert at end.

## 4. MOTION, painless integration with special mode
Emacs do not use modal editing by default, but each special mode(like dired) will provide nice single key commands. I recommand to use these keybindings instead of maintain our own.

For these special modes, Meow has a `MOTION` mode, in this mode, <kbd>SPC</kbd> is selected as LEADER, and the origin command on <kbd>SPC</kbd> is bound to `LEADER SPC` which is <kbd>SPC SPC</kbd>. If you want to use <kbd>j</kbd> or <kbd>k</kbd> for move up and down, you should consider bound the origin commands on <kbd>j</kbd> and <kbd>k</kbd> to <kbd>SPC j</kbd> and <kbd>SPC k</kbd>.

Meow has a nice mechanism for this demand. See `meow-motion-overwrite-define-key`.

## 5. KEYPAD, all those commands without modifiers
The idea borrowed from god-mode.

Press <kbd>SPC x</kbd>(Default behaviour) in `NORMAL` and `MOTION` mode will enter `KEYPAD` mode, and this input will be converted to `C-x`. Followings single keys, will be translate to the one with `Ctrl` modifier. Once Meow found a valid command for your input, execute and exit `KEYPAD` mode.

If you want some input other than `C-` in `KEYPAD`, you need a prefix:
* <kbd>SPC</kbd> means no modifier. This `SPC` can be omitted when there's no ambiguity.
* <kbd>m</kbd> means with `Meta`.
* <kbd>g</kbd> means with `Ctrl+Meta`.

Some examples:

| Vanilla Emacs | Meow KEYPAD                                                              |
|---------------|--------------------------------------------------------------------------|
| C-x C-f       | <kbd>SPC x f</kbd>                                                       |
| C-c C-c       | <kbd>SPC c c</kbd>                                                       |
| C-h k         | <kbd>SPC h SPC k</kbd> or <kbd>SPC h k</kbd> (when no ambiguity)         |
| C-M-t         | <kbd>SPC g t</kbd>                                                       |
| M-r           | <kbd>SPC m r</kbd>                                                       |
| C-c M-n n     | <kbd>SPC c m n SPC n</kbd> or <kbd>SPC c m n n</kbd> (when no ambiguity) |

# Description for each COMMANDS

## Mode Switching

`meow-insert` switch to `INSERT` mode at the beginning of selection or current position.

`meow-append` switch to `INSERT` mode at the end of selection or current position.

`meow-open-above` open newline above and switch to `INSERT` mode.

`meow-open-below` open newline below and switch to `INSERT` mode.

## Navigation & Selection

When you have a selection, following is available:

`meow-reverse` Reverse the direction of selection, like `exchange-point-and-mark`.

`meow-pop-selection` Back to the previous selection.

`meow-pop-all-selection` Back to the position before you have selection.

`meow-expand-(0-9)` Some commands provide hint for further location, you can use these commands to jump to corresponding positions.

Following is the basic movement, the later one of each group will activate the `char` type selection, and the previous will expand the `char` type selection and will cancel selection with other types.

`meow-head` / `meow-head-expand` Move towards the head of line.

`meow-tail` / `meow-tail-expand` Move towards the tail of line.

`meow-prev` / `meow-prev-expand` previous line.

`meow-next` / `meow-next-expand` next line.

Following commands combines navigation and selection.

`meow-visit` Search regexp and mark it with selection of type `visit`. Use `negative-argument` for backward searching. The search history will be recorded, and use `meow-pop-search` to pop the recent one.

`meow-search` Search for the next occur, the searching direction is based on the direction of selection. (use `meow-reverse` to reverse the direction)

`meow-mark-word` Select current word with selection type `expand word`, and record it into search history. (So you can use `meow-search` after this)

`meow-mark-symbol` Select current symbol with selection type `expand word`(same type with `meow-mark-word`), and record it into search history. (So you can use `meow-search` after this)

`meow-line` Select current line, repeat to expand. Use `meow-reverse` or `negative-argument` to select backward.

`meow-block` / `meow-expand-block` Select the next block(pair of parens), when repeat, the former will select the parent block, the later will expand to the next block. Use `negative-argument` to select backward.

`meow-join` Select the area that will be deleted if `delete-indentation`, use `negative-argument` to search forward.

`meow-find` / `meow-find-expand` Accept an input of a char, select(or expand) to the next char of the occur, backward search with `negative-argument`.

`meow-till` / `meow-till-expand` Accept an input of a char, select(or expand) to the previous char of the occur, backward search with `negative-argument`.

Following commands are working with `thing`s. You can custom variable `meow-char-thing-table` to given a key to each `thing`.

This table describe the default behaviour.

| thing        | default key |
|--------------|-------------|
| round paren  | r           |
| square paren | s           |
| curly paren  | c           |
| string       | g           |
| paragraph    | p           |
| line         | l           |
| defun        | d           |
| buffer       | b           |

`meow-inner-of-thing` Select the inner of thing.

`meow-bounds-of-thing` Select the whole thing.

`meow-beginning-of-thing` Select to the beginning of thing.

`meow-end-of-thing` Select to the end of thing.

## Deletion & Modification

`meow-kill` Kill current selection, do what <kbd>C-k</kbd> do when there's no selection. Note <kbd>C-k</kbd> may have different behaviours depends on the context. For example, when `paredit` is enabled, <kbd>C-k</kbd> will bound to `paredit-kill`.

`meow-delete` Delete current selection(don't change kill-ring) or do what <kbd>C-d</kbd> do when there's no selection.

`meow-change` Delete current selection and switch to `INSERT` mode.

`meow-save` Copy current selection into `kill-ring`, but no system clipboard. Use `kill-ring-save` or `meow-clipboard-save` for system clipboard.

`meow-yank` Paste before current position, or after current position with `negative-argument`. This command will not use system clipboard, use `yank` or `meow-clipboard-yank` for system clipboard.

`meow-replace` replace current region with copy.

`meow-replace-save` exchange current region with copy.

## Other Commands

`meow-quit` Delete current window or back to previous buffer if there's only one window.

`meow-keyboard-quit` just keyboard-quit。

# Helper Functions for customization

`(meow-indicator)` Return an indicator string that you can put into your modeline.

`(meow-normal-define-key & args)` Define keybinding for `NORMAL` mode, you use this to define your own modal editing.

See [here](#2-almost-no-default-keybinding).

`(meow-leader-define-key & args)` Define LEADER keymap.

Example usage:

```emacs-lisp
(meow-leader-define-key
  '("d" . dired)
  '("f" . find-file))
```

`(meow-setup-line-number)` Setup line number the Meow-way. Use relative line number for `NORMAL` mode.

`(meow-motion-overwrite-define-key & args)` Define keybinding for `MOTION` mode.

Following code show how to use <kbd>j</kbd> / <kbd>k</kbd> to move up & down, and use <kbd>SPC j</kbd> and <kbd>SPC k</kbd> for origin commands.

```emacs-lisp
(meow-motion-overwrite-define-key
  '("j" . meow-next)
  '("k" . meow-prev))

;; For the origin commands on j/k
(meow-leader-define-key
  '("j" . meow-motion-origin-command)
  '("k" . meow-motion-origin-command))
```

# Variables for customization

`meow-normal-state-mode-list` A list of major modes that meow should use `NORMAL` mode. Meow is a young package, so the default value of this variables may not contains all the needed. If you find some mode that should use `NORMAL` mode, put code like following in your configuartion, and an Issue is WELCOMED for this case!

```emacs-lisp
(use-package meow
  ...
  :config
  (add-to-list 'meow-normal-state-mode-list 'py-shell-mode))
```

`meow-replace-state-name-list` A list of cons, customize this variable to replace the state name in indicator.

```emacs-lisp
(setq meow-replace-state-name-list
 '((normal . "Ꮚ•ꈊ•Ꮚ")
   (insert . "Ꮚ`ꈊ´Ꮚ")
   (keypad . "Ꮚ'ꈊ'Ꮚ")
   (motion . "Ꮚ-ꈊ-Ꮚ")))
```

# LICENSE

License under GPL v3.
