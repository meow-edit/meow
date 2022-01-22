# Changelog

## Master (Unreleased)

### Breaking Changes

#### Keypad Refactor
The rules of KEYPAD is slightly updated to eliminate the need for a leader system.
The overall usage is basically unchanged, use same keys for same commands.

* `meow-leader-keymap` is removed.
* A new command `meow-keypad` is introduced, bound to `SPC` in NORMAL/MOTION state.
* Press `SPC` to enter KEYPAD state.
* Add quick dispatching from `SPC <key>` to `C-c <key>`, where `<key>` is not one of x, c, h, m, g.

Check document or `meow-tutor` for updated information.

### Enhancements
* Improve document for word movements.

### Bugs fixed
* Meow is not enabled in existing buffers after `desktop-read`.

## 1.3.0 (2021-01-15)

### Enhancements
* [#155](https://github.com/meow-edit/meow/pull/155) [#166](https://github.com/meow-edit/meow/pull/166) [#158](https://github.com/meow-edit/meow/pull/158) Add `meow-define-state` and `meow-register-state` to allow user define custom state.
* Remap `describe-key` to `meow-describe-key` which handles the dispatched keybinds.
* Allow leader in beacon state(still can not switch to keypad).
* [#164](https://github.com/meow-edit/meow/issues/164) Add fallback support for meta & control-meta prefix in keypad.

### Bugs fixed
* [#148](https://github.com/meow-edit/meow/issues/148) Wrap `regexp-quote` for raw search in `meow-search`.
* [#144](https://github.com/meow-edit/meow/pull/144) [#145](https://github.com/meow-edit/meow/pull/145) [#151](https://github.com/meow-edit/meow/pull/151) Improve wording in `meow-tutor`.
* [#153](https://github.com/meow-edit/meow/pull/153) Avoid executing symbol-name w.r.t lambda func.
* In some cases previous state can't be stored, when dispatching to a keymap with keypad.

## 1.2.1 (2021-12-22)

### Bugs fixed
* `hl-line-mode` is not restored correctly after beacon state.
* Using `meow-grab` in beacon kmacro recording causes residual overlays.
* [#138](https://github.com/meow-edit/meow/issues/138) meow-global-mode does not work after being turned off.
* Wrong count in search indicator when searching same contents cross buffers.
* Better initial state detection.
* [#143](https://github.com/meow-edit/meow/issues/143) Wrong column beacon positions when secondary selection is not started with line beginning.

## 1.2.0 (2021-12-16)

### Breaking Changes

#### Changes for THING register
The built-in thing definition shipped by meow should be more close to what Emacs gives us.
So two previously added, complex things are removed. A helper function is added, so you can easily
register new thing with Emacs things, functions, syntax descriptions or regexp pairs.

- A helper function `meow-thing-register` is provided, check its document for usage.
- Thing `indent` and `extend` has been removed.
- Variable `meow-extend-syntax`(undocumented) has been removed.
- Add custom variable `meow-thing-selection-directions`.
- `meow-bounds-of-thing` will create a backward selection by default.

### Enhancements
* Remove paredit shims, no longer needed.
* [#110](https://github.com/meow-edit/meow/issues/110) Only disable hint overlay for modes in `meow-expand-exclude-mode-list`.
* Add custom variable `meow-motion-remap-prefix.`
* Remove `dash.el` and `s.el` from dependencies.
* Add more defaults to `meow-mode-state-list`.
* `meow-swap/sync-grab` will grab on current position, thus you can go
  back to previous position with `meow-pop-grab` later.
* Improve char-thing table format for thing-commands.
* Improve default state detection.

## 1.1.1 (2021-12-06)

### Enhancements
* Prevent user call `meow-mode` directly.

### Bugs fixed
* Fix for disabling `meow-global-mode`.

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
