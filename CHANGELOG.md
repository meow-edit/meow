# Changelog

## Master (Unreleased)

## 1.4.5 (2024-02-11)

### Bugs fixed
* [#557](https://github.com/meow-edit/meow/issues/557) Fix the shim code for `wdired`.
* [#546](https://github.com/meow-edit/meow/issues/546) Fix `meow-back-symbol` that unconditionally reverse direction.
* [#545](https://github.com/meow-edit/meow/issues/545) Fix position hint before tabs with width 2.
* [#539](https://github.com/meow-edit/meow/issues/539) Fix beacon change with consecutive characters.
* [#373](https://github.com/meow-edit/meow/issues/373) Do not cancel selection when entering beacon mode.
* [#514](https://github.com/meow-edit/meow/issues/514) Fix meow-esc in `emacsclient -t`.

### Enhancements
* [#517](https://github.com/meow-edit/meow/pull/517) Consider local keybindings when moving commands for the Motion state.
* [#512](https://github.com/meow-edit/meow/pull/512) Add shim for realgud.
* [#503](https://github.com/meow-edit/meow/pull/503) Add shim for sly.

## 1.4.4 (2023-08-23)

### Bugs fixed

* Fix keypad command display priority
* Fix global mode initialization, which causes both normal and motion are enabled

## 1.4.3 (2023-07-11)

### Bugs fixed

* [#223](https://github.com/meow-edit/meow/pull/223) Fix the complete behavior in `meow-open-above` when `tab-always-indent` is set to `'complete`.
* [#290](https://github.com/meow-edit/meow/issues/290) Clean up beacon overlays on mode diasbling.
* [#318](https://github.com/meow-edit/meow/pull/318) Skip string-fence syntax class in meow--{inner,bounds}-of-string
* [#327](https://github.com/meow-edit/meow/pull/327) Fix two minore issue with cursor updating.
* Fix the order of beacons for `meow-search`.
* Fix `meow-line` mark bug.
* Fix literal key pad bug.
* Fix `meow-goto-line` when there's no region available.

### Enhancements

* Add a variable `meow-keypad-self-insert-undefined`, it controls whether to insert a key when it's undefined in keypad.
* Add keyboard layouts for Colemak-DH [#284](https://github.com/meow-edit/meow/pull/284), FWYR [#326](https://github.com/meow-edit/meow/pull/326),
* [#416](https://github.com/meow-edit/meow/pull/416) Add visual-line versions of some Meow operations.

### Breaking Changes

* [#209](https://github.com/meow-edit/meow/pull/209) Make
  `meow-keypad-start-keys` an association list to enhance customizability.
  See [CUSTOMIZATIONS](./CUSTOMIZATIONS) for more details.
* `meow-quit` uses `quit-window` instead of `delete-window`.

## 1.4.2 (2022-03-13)

### Bugs fixed

* [#163](https://github.com/meow-edit/meow/issues/163) Fix using command with Meta key bindings in BEACON state.

### Enhancements

* Update the oldest supported Emacs version to 27.1.
* [#204](https://github.com/meow-edit/meow/pull/204) Allow using keypad in BEACON state.
* Add "MOVE AROUND THINGs" section to `meow-tutor.el`.
* Update `meow-goto-line` to expand `meow-line`.

### Bugs fixed

* Fix `meow-mark-symbol` in BEACON state.
* [#204](https://github.com/meow-edit/meow/pull/204) Fix keypad in telega.
* Fix no variable `meow--which-key-setup` error when deactivating meow.

## 1.4.1 (2022-02-16)

### Enhancements
* Add which-key support.
* Add custom variable `meow-goto-line-function`.
* ~~Support specified leader keymap by altering `meow-keymap-alist`.~~
* Support specifying the target of `meow-leader-define-key` by altering `meow-keymap-alist`.
* Add a variable `meow-keypad-leader-dispatch`.

### Bugs fixed

* Fix keypad popup delay.
* Fix keypad popup when C-c is bound to other keymap.
* [#197](https://github.com/meow-edit/meow/issues/197) Fix `meow-kill` for `select line` selection.
* [#198](https://github.com/meow-edit/meow/issues/198) Fix invalid mode states with poly mode.

## 1.4.0 (2022-01-24)

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

## 1.3.0 (2022-01-15)

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
