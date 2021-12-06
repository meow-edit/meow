# Changelog

## Master (Unreleased)

## 1.1.0 (2021-12-06)

### Features
* [#99](https://github.com/meow-edit/meow/pull/99) Add `meow-tutor`.

### Breaking Changes
* rename bmacro -> beacon.

### Enhancements
* Add `meow-expand-hint-counts`.
* Add more defaults to `meow-mode-state-list`.
* Improve color calculation in beacon state.
* Support change char in beacon state (as the fallback behaviour for change, by default).
* Support `expand char` selection in beacon state.
* Support `kill` in beacon state.
* Add thing `sentence`, default bound to `.`.

### Bugs fixed
* Fix expand for `meow-line`.
* Fix nils displayed in keypad popup.
* Fix C-S- and C-M-S- in keypad.
* Eval `meow-motion-overwrite-define-key` multiple times cause invalid remap.
* Set `undo-tree-enable-undo-in-region` for undo-tree automatically.

## 1.0.1 (2021-11-30)
### Bugs fixed
* `SPC SPC` doesn't work in motion state.

## 1.0.0 (2021-11-28)
Initial release.
