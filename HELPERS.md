# HELPER FUNCTIONS

## meow-setup-indicator

A helper function that puts an indicator at the beginning of mode-line. If you want customize mode-line by hand, see `meow-indicator`.

Example usage:

```
(meow-setup-indicator)
```

## meow-indicator

Return an indicator string that you can use in your `mode-line-format`.

Example usage:

```emacs-lisp
(setq mode-line-format '((:eval (meow-indicator)) ...))
```

## meow-normal-define-key

Define keybinding for `NORMAL` mode, you use this to define your own modal editing.

Example usage:

```emacs-lisp
(meow-leader-define-key
  '("d" . dired)
  '("f" . find-file))
```

## meow-leader-define-key

Define keybinding in LEADER keymap, the default LEADER is <kbd>SPC</kbd>

Example usage:

```emacs-lisp
(meow-leader-define-key
   ;; reverse command query
   '("^" . meow-keypad-describe-key)
   ;; cheatsheet
   '("?" . meow-cheatsheet)
   ;; high frequency keybindings
   '("e" . "C-x C-e")
   '(")" . "C-)")
   '("}" . "C-}")
   '("." . "M-.")
   '("," . "M-,")
   ;; window management
   '("w" . other-window)
   '("o" . delete-other-windows)
   ;; high frequency commands
   '(";" . comment-dwim)
   '("k" . kill-this-buffer)
   '("p" . project-find-file)
   '("j" . project-switch-to-buffer)
   '("d" . dired)
   '("b" . switch-to-buffer)
   '("r" . deadgrep)
   '("f" . find-file)
   '("i" . imenu)
   '("a" . "M-x"))
```

NOTE: besides binding keys to a command, you can also bind keys to a kbd macro string, e.g. `"M-x"`.

## meow-motion-overwrite-define-key

Define keybinding for `MOTION` mode.

Following code show how to use <kbd>j</kbd> / <kbd>k</kbd> to move up & down, and use <kbd>SPC j</kbd> and <kbd>SPC k</kbd> for original commands.

```emacs-lisp
(meow-motion-overwrite-define-key
  '("j" . meow-next)
  '("k" . meow-prev))

;; For the original commands on j/k
(meow-leader-define-key
  '("j" . meow-motion-origin-command)
  '("k" . meow-motion-origin-command))
```
