[![MELPA](https://melpa.org/packages/meow-badge.svg)](https://melpa.org/#/meow)

![Logo](meow.svg)

> Less is more

# Highlights

## No built-in keybindings.

Meow provides a complete set of commands, let user decide their keybindings with preferences.
However, since it's no inconvenient, here we have examples of configuration for each keyboard layouts that you can start with.

### For Qwerty, Vim Like
TBD

### For Dvorak Simplified
TBD

### For Dvorak Programmers
TBD

### For Colemak
TBD

## No integration specific configuration.

Meow should plays well with special modes, like dired, treemacs, etc. Because how it is designed.

## Combined NAVIGATION & SELECTION

This great idea is stolen from Kakoune, a navigation command will also activate region. It's so convenient for Copy&Paste workflow(what I really enjoy). When you want make insertion, use <kbd>i</kbd> / <kbd>a</kbd> to turn to __INSERT__ mode at beginning / end.

### God-mode like KEYPAD state

There's a __KEYPAD__ state, works like a one shot version of God-mode. Allows you to execute commands without modifiers. If you are comfortable with those default keybindings of every packages, just try to avoid `C-`, `M-`, and `C-M-`, this is the best you can have.

# Installation

### Melpa

Meow is available on MELPA.

``` emacs-lisp
(use-package meow
  :init
  (meow-global-mode 1)
  (meow-setup)
```

# Modal States

### INSERT

Just vanilla Emacs, the only difference is that you can switch to __NORMAL__ state with <kbd>ESC</kbd>.

### NORMAL

Here is a list for details of each commands.

### MOTION

### KEYPAD

# Leader Key Configuration

Use `meow-leader-define-key` and `meow-leader-define-mode-key` to customize leader keymap.

```emacs-lisp
;; Put these in the :config section in use-package

(meow-leader-define-key
 '("k" . kill-buffer)
 '("l" . goto-line)
 '("h" . other-window)
 '("o" . delete-other-windows)
 '("-" . split-window-below)
 '("/" . swiper)
 '("\\" . split-window-right)
 '("m" . magit-status)
 '("f" . find-file)
 '("F" . find-file-literally))
```

# LICENSE

License under GPL v3.
