[![MELPA](https://melpa.org/packages/meow-badge.svg)](https://melpa.org/#/meow)

![Logo](meow.svg)

> Less is more

Meow aims to let users write less configuration, but get better integration. To Remember fewer commands, but can do more things.

[中文文档](README_CN.md)

# Table Of Content
- [Installation](#Installation)
- [What is Meow?](#what-is-meow)
* [Command descriptions](#command-descriptions)
* [Configurations](#configurations)
* [FAQ](#faq)

# Installation

Meow is on [Melpa](https://melpa.org/).

```emacs-lisp
(require 'meow)

(meow-global-mode 1)

(defun meow-setup ()
  ...
  DEFINE COMMAND LAYOUT, SEE BELOW
  ...)

(with-eval-after-load "meow"
  ;; meow-setup is your custom function, see below
  (meow-setup)
  ;; If you want relative line number in NORMAL state(for display-line-numbers-mode)
  (meow-setup-line-number)
  ;; If you need setup indicator, see `meow-indicator' for customizing by hand.
  (meow-setup-indicator))

```

Here are the recommended [definitions for meow-setup](#2-almost-no-default-keybinding).

## If you are using use-package

```emacs-lisp
(defun meow-setup ()
  ...
  DEFINE COMMAND LAYOUT, SEE BELOW
  ...)

(use-package meow
  :demand t
  :init
  (meow-global-mode 1)
  :config
  ;; meow-setup is your custom function, see below
  (meow-setup)
  ;; If you want relative line number in NORMAL state(for display-line-numbers-mode)
  (meow-setup-line-number)
  ;; If you need setup indicator, see `meow-indicator' for customizing by hand.
  (meow-setup-indicator))
```

Here are the recommended [definitions for meow-setup](#2-almost-no-default-keybinding).

# What is Meow?

Meow is a complete modal editing, and this section will explain its highlights and why it's here.

## 1. Four Modes

Meow has 4 modes.

* `NORMAL`: The default mode for text editing, commands bound to single keys. Note there are no default keybindings in Meow for NORMAL mode.
* `INSERT`: The mode for text insertion, press <kbd>ESC</kbd> to get back to `NORMAL` mode.
* `MOTION`: The default mode for all kinds of special modes, only <kbd>SPC</kbd> is bound to Leader, and the original command on <kbd>SPC</kbd> is bound to <kbd>SPC SPC</kbd>.
* `KEYPAD`: A temporary mode to simulate input with modifiers(ctrl, meta) with single-key sequences.

## 2. Almost no default keybinding

Meow provides a set of complete modal editing commands, but users have to build their own keymap. The followings are some recommended schemas, which you can use directly or as your start point.

[Keybindings for each keyboard layout](KEYBINDINGS.md)

### Want a cheatsheet?

Use `M-x meow-cheatsheet`. This command will generate a cheatsheet according to your current keybindings.

The keyboard layout in cheatsheet is specified by the variable `meow-cheatsheet-layout`, you will find this variable set in `meow-setup` examples. You can re-run `meow-cheatsheet` to refresh after you make changes.

![cheatsheet](https://user-images.githubusercontent.com/11796018/103559957-f89c2680-4ef1-11eb-808d-04aecc588565.png)

## 3. NORMAL, navigation is also selection
The idea is borrowed from Kakoune.

The navigation commands, except single character movement, will also activate the selection. For example, moving forward a word will mark from previous position to current position. So you got two meaningful positions with a single command. When selection is activated, use <kbd>i</kbd> to insert at beginning, or <kbd>a</kbd> to insert at end.

## 4. MOTION, painless integration with special mode
Emacs does not use modal editing by default, but each special mode(like dired) will provide nice single key commands. I recommend to use these keybindings instead of maintaining your own.

For these special modes, Meow has a `MOTION` mode, in this mode, <kbd>SPC</kbd> is selected as LEADER, and the original command on <kbd>SPC</kbd> is bound to `LEADER SPC` which is <kbd>SPC SPC</kbd>. If you want to use <kbd>j</kbd> or <kbd>k</kbd> for move up and down, you should consider binding the original commands on <kbd>j</kbd> and <kbd>k</kbd> to <kbd>SPC j</kbd> and <kbd>SPC k</kbd>.

Meow has a nice mechanism for this demand. See `meow-motion-overwrite-define-key`.

## 5. KEYPAD, all those commands without modifiers
The idea is borrowed from god-mode.

Pressing <kbd>SPC x</kbd>(Default behavior) in `NORMAL` and `MOTION` mode will enter `KEYPAD` mode, and this input will be converted to `C-x`. The following single keys, will be translated to the one with `Ctrl` modifier. Once Meow found a valid command for your input, execute and exit `KEYPAD` mode. You can also enter KEYPAD mode with <kbd>SPC c</kbd>, <kbd>SPC h</kbd>, <kbd>SPC m</kbd> or <kbd>SPC g</kbd>(Also default behaviours).

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

### What about Which Key?

Of course, you can use [which-key](https://github.com/justbur/emacs-which-key) with Meow without additional setup.
It will work for Leader keymap and vanilla keybindings. However since `KEYPAD` has its own input merchanism, it can't use
which-key. So Meow ships a built-in functionality for `KEYPAD` only. Again, you can still use which-key for the rest keybindings.

![meow-describe-keymap](https://user-images.githubusercontent.com/11796018/104113302-3efae680-5333-11eb-86cb-f6430add7ae9.png)

# COMMAND DESCRIPTIONS

## Mode Switching

`meow-insert` switch to `INSERT` mode at the beginning of selection or current position.

`meow-append` switch to `INSERT` mode at the end of selection or current position.

`meow-open-above` open newline above and switch to `INSERT` mode.

`meow-open-below` open newline below and switch to `INSERT` mode.

## Navigation & Selection

When you have a selection, the following is available:

`meow-reverse` Reverse the direction of selection, like `exchange-point-and-mark`.

`meow-pop-selection` Back to the previous selection.

`meow-pop-all-selection` Back to the position before you performed selection.

`meow-expand-(0-9)` Some commands provide hint for further location, you can use these commands to jump to the corresponding positions.

<details>
    <summary>This GIF shows what EXPAND looks like</summary>

![meow-expand](https://user-images.githubusercontent.com/11796018/103553967-2a5cbf80-4ee9-11eb-9c80-c85f4a8b1ce0.gif)
</details>

Following is the basic movement, the later one of each group will activate the `char` type selection, and the previous will expand the `char` type selection and will cancel selection with other types.

`meow-left` / `meow-left-expand` Move left by char.

`meow-right` / `meow-right-expand` Move right by char.

`meow-prev` / `meow-prev-expand` previous line.

`meow-next` / `meow-next-expand` next line.

`meow-head` / `meow-head-expand` Move towards the head of line by char.

`meow-tail` / `meow-tail-expand` Move towards the tail of line by char.

Following commands combine navigation and selection.

`meow-visit` Search regexp and mark it with selection of type `visit`. Use `negative-argument` for backward searching. The search history will be recorded, and use `meow-pop-search` to pop the recent one.

`meow-search` Search for the next occurence, the searching direction is based on the direction of selection. (use `meow-reverse` to reverse the direction)

<details>
    <summary>This GIF shows the usage of VISIT and SEARCH</summary>

![meow-search](https://user-images.githubusercontent.com/11796018/103555522-6133d500-4eeb-11eb-9a8e-51ab0cb43f24.gif)
</details>

`meow-mark-word` Select current word with selection type `expand word`, and record it into search history. (So you can use `meow-search` after this)

`meow-mark-symbol` Select current symbol with selection type `expand word`(same type with `meow-mark-word`), and record it into search history. (So you can use `meow-search` after this)

`meow-line` Select current line, repeat to expand. Use `meow-reverse` or `negative-argument` to select backward.

`meow-goto-line` Goto line and recenter screen, then select that line. Use a numeric prefix for line number to goto or you will be asked.

`meow-block` / `meow-block-expand` Select the next block(pair of parens), when repeat, the former will select the parent block, the later will expand to the next block. Use `negative-argument` to select backward.

`meow-join` Select the area that will be deleted if `delete-indentation`, use `negative-argument` to search forward.

`meow-find` / `meow-find-expand` Accept an input of a char, select(or expand) to the next char of the occurence, backward search with `negative-argument`.

`meow-till` / `meow-till-expand` Accept an input of a char, select(or expand) to the previous char of the occurence, backward search with `negative-argument`.

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

`meow-kill` Kill current selection, does what <kbd>C-k</kbd> does when there's no selection. Note <kbd>C-k</kbd> may have different behaviours depending on the context. For example, when `paredit` is enabled, <kbd>C-k</kbd> will be bound to `paredit-kill`.

`meow-delete` Delete current selection(don't change kill-ring) or do what <kbd>C-d</kbd> does when there's no selection.

`meow-change` Delete current selection and switch to `INSERT` mode.

`meow-save` Copy current selection into `kill-ring`, but not system clipboard. Use `kill-ring-save` or `meow-clipboard-save` for system clipboard.

`meow-yank` Paste before current position, or after current position with `negative-argument`. This command will not use system clipboard, use `yank` or `meow-clipboard-yank` for system clipboard.

`meow-replace` Replace current region with copy.

`meow-replace-save` Exchange current region with copy.

## Grab

`meow-grab` is a command to work with Emacs built-in secondary selection(aka the x-selection in Linux). `meow-grab` will convert current region to secondary selection, it's also possible to use a single point. Once the secondary selection(or point) is activated, you can use `meow-pop-grab` to convert secondary selection to region which allows you go back to the grab position.

By default, `meow-pop-grab` is the fallback command for `meow-pop` when there's no selection available. This behavior can be customized by modifying `meow-selection-command-fallback`.

`meow-swap-grab` swap secondary selection with current region.

`meow-sync-grab` sync secondary selection with current region.

## Kmacros

[Kmacros in Meow](KMACROS.md).

## Other Commands

`meow-quit` Delete current window or go back to previous buffer if there's only one window.

`meow-keyboard-quit` Just keyboard-quit。

`meow-cancel-selection` Cancel the selection.

`meow-undo` Like undo, but cancel selection first. We add this command because `undo` will undo modification inside the region if region is activated.

`meow-undo-in-selection` Undo modifications in region.

# CONFIGURATIONS

See [Helper Functions](HELPERS.md) and [Custom Variables](CUSTOMIZATIONS.md)

# FAQ

## Working with EXWM

EXWM is a X tiling window manager. To use Meow with EXWM, you probably want a global leader key and global KEYPAD entry.

```emacs-lisp
(setq exwm-input-global-keys
          `(...
            ([?\s-x] . meow-keypad-start)
            ([?\s-m] . meow-keypad-start)
            ([?\s-g] . meow-keypad-start)
            ([?\s-c] . meow-keypad-start)
            ([?\s-\ ] . ,meow-leader-keymap) ;; This is super+SPC
            ...))
```

# LICENSE

License under GPL v3.
