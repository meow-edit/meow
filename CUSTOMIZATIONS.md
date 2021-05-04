# CUSTOMIZATION VARIABLES

## meow-mode-state-list

default:

```
((cider-browse-spec-view-mode . motion)
 (fundamental-mode . normal)
 (text-mode . normal)
 (prog-mode . normal)
 (conf-mode . normal)
 (json-mode . normal)
 ...)
```

A list of rules, each is `(major-mode . init-state)`.

The init-state can only be `motion` or `normal`, and `motion` have a higher priority.

## meow-expand-exclude-mode-list

default: `(markdown-mode org-mode)`

A list of major modes where after command expand should be disabled.

## meow-selection-command-fallback

default:

```
((meow-replace . meow-replace-char)
 (meow-change . meow-change-char)
 (meow-save . meow-save-char)
 (meow-kill . meow-C-k)
 (meow-delete . meow-C-d)
 (meow-cancel-selection . meow-keyboard-quit)
 (meow-pop . meow-pop-grab))
```

Fallback commands for selection-only commands, called when there's no available selection.

## meow-extend-syntax

default: `"^-><()"`

Syntax description for thing `extend`.

## meow-replace-state-name-list

default:

```
((normal . "NORMAL")
 (motion . "MOTION")
 (keypad . "KEYPAD")
 (insert . "INSERT"))
```

A list of mappings for how state is displayed in modeline indicator.

## meow-select-on-change

default: `t`

Whether to activate region when exiting INSERT mode after `meow-change`, `meow-change-char` and `meow-change-save`.

## meow-expand-hint-remove-delay

default: `1.0`

The delay before the position hint disappears.

## meow-keypad-message

default: `t`

Whether to log keypad messages in minibuffer.

## meow-char-thing-table

default:

```
((?r . round)
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
```

Mapping from char to thing.

Used by `meow-beginning-of-thing`, `meow-end-of-thing`, `meow-inner-of-thing` and `meow-bounds-of-thing`.

## meow-display-thing-help

default: `t`

Whether to display the help prompt for `meow-inner/bounds/begin/end-of-thing`.

## meow-keypad-describe-delay

default: `0.5`

The delay in seconds before popup keybinding descriptions appear.

## meow-grab-fill-commands

default: `(meow-query-replace meow-query-replace-regexp)`

A list of commands that meow will auto fill with grabbed content.

## meow-visit-collect-min-length

default: `1`

Minimal length when collecting symbols for `meow-visit`.

## meow-visit-sanitize-completion

default: `t`

Whether let `meow-visit` display symbol regexps in a sanitized format.

## meow-use-clipboard

default: `nil`

Whether to use system clipboard. Not recommended.
