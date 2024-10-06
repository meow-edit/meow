## Keymap

- [Normal mode](#normal-mode)
  - [Movement](#movement)
  - [Changes](#changes)
    - [Shell](#shell)
  - [Selection manipulation](#selection-manipulation)
  - [Search](#search)
  - [Minor modes](#minor-modes)
    - [View mode](#view-mode)
    - [Goto mode](#goto-mode)
    - [Match mode](#match-mode)
    - [Window mode](#window-mode)
    - [Space mode](#space-mode)
      - [Popup](#popup)
      - [Completion Menu](#completion-menu)
      - [Signature-help Popup](#signature-help-popup)
    - [Unimpaired](#unimpaired)
- [Insert mode](#insert-mode)
- [Select / extend mode](#select--extend-mode)
- [Picker](#picker)
- [Prompt](#prompt)

> 💡 Mappings marked (**LSP**) require an active language server for the file.

> 💡 Mappings marked (**TS**) require a tree-sitter grammar for the file type.

> ⚠️ Some terminals' default key mappings conflict with Helix's. If any of the mappings described on this page do not work as expected, check your terminal's mappings to ensure they do not conflict. See the [wiki](https://github.com/helix-editor/helix/wiki/Terminal-Support) for known conflicts.

## Normal mode

Normal mode is the default mode when you launch Helix. You can return to it from other modes by pressing the `Escape` key.

### Movement

> NOTE: Unlike Vim, `f`, `F`, `t` and `T` are not confined to the current line.

| Key                  | Description                                    | Command                               | Todo | Notes                                                            |
|----------------------|------------------------------------------------|---------------------------------------|------|------------------------------------------------------------------|
| `h`, `Left`          | Move left                                      | `helix-backward-char`                 | [x]  | the only extra thing these movements do is deactivate the region |
| `j`, `Down`          | Move down                                      | `helix-next-line`                     | [x]  |                                                                  |
| `k`, `Up`            | Move up                                        | `helix-previous-line`                 | [x]  |                                                                  |
| `l`, `Right`         | Move right                                     | `helix-forward-char`                  | [x]  |                                                                  |
| `w`                  | Move next word start                           | `helix-move-next-word-start`          | []   | these keypresses don't yet extend the region in visual mode      |
| `b`                  | Move previous word start                       | `helix-move-previous-word-start`      | []   |                                                                  |
| `e`                  | Move next word end                             | `helix-move-next-word-end`            | []   |                                                                  |
| `W`                  | Move next WORD start                           | `helix-move-next-long-word-start`     | []   |                                                                  |
| `B`                  | Move previous WORD start                       | `helix-move-previous-long-word-start` | []   |                                                                  |
| `E`                  | Move next WORD end                             | `helix-move-next-long-word-end`       | []   |                                                                  |
| `t`                  | Find 'till next char                           |                                       | []   |                                                                  |
| `f`                  | Find next char                                 | `helix-find-char`                     | [x]  |                                                                  |
| `T`                  | Find 'till previous char                       |                                       | []   |                                                                  |
| `F`                  | Find previous char                             |                                       | []   |                                                                  |
| `G`                  | Go to line number `<n>`                        |                                       | [x]  | not sure this even work in Helix                                 |
| `Alt-.`              | Repeat last motion (`f`, `t`, `m`, `[` or `]`) |                                       | []   |                                                                  |
| `Home`               | Move to the start of the line                  |                                       | []   |                                                                  |
| `End`                | Move to the end of the line                    |                                       | []   |                                                                  |
| `Ctrl-b`, `PageUp`   | Move page up                                   |                                       | []   |                                                                  |
| `Ctrl-f`, `PageDown` | Move page down                                 |                                       | []   |                                                                  |
| `Ctrl-u`             | Move cursor and page half page up              |                                       | []   |                                                                  |
| `Ctrl-d`             | Move cursor and page half page down            |                                       | []   |                                                                  |
| `Ctrl-i`             | Jump forward on the jumplist                   |                                       | []   |                                                                  |
| `Ctrl-o`             | Jump backward on the jumplist                  |                                       | []   |                                                                  |
| `Ctrl-s`             | Save the current selection to the jumplist     |                                       | []   |                                                                  |

### Changes

| Key         | Description                                                          | Command                | Todo | Notes                                                      |
|-------------|----------------------------------------------------------------------|------------------------|------|------------------------------------------------------------|
| `r`         | Replace with a character                                             |                        | []   |                                                            |
| `R`         | Replace with yanked text                                             |                        | []   |                                                            |
| `~`         | Switch case of the selected text                                     |                        | []   |                                                            |
| `` ` ``     | Set the selected text to lower case                                  |                        | []   |                                                            |
| `` Alt-` `` | Set the selected text to upper case                                  | `helix-insert`         | [x]  |                                                            |
| `i`         | Insert before selection                                              | `helix-append`         | [x]  |                                                            |
| `a`         | Insert after selection (append)                                      |                        | []   |                                                            |
| `I`         | Insert at the start of the line                                      |                        | []   |                                                            |
| `A`         | Insert at the end of the line                                        | `helix-open-below`     | []   | Need to add multiple cursors when used with number prefix  |
| `o`         | Open new line below selection                                        | `helix-open-above`     | []   | Need to add multiple cursors when used with number prefixy |
| `O`         | Open new line above selection                                        |                        | [x]  |                                                            |
| `.`         | Repeat last insert                                                   |                        | []   |                                                            |
| `u`         | Undo change                                                          | `undo`                 | [x]  |                                                            |
| `U`         | Redo change                                                          | `redo`                 | [x]  |                                                            |
| `Alt-u`     | Move backward in history                                             |                        | []   |                                                            |
| `Alt-U`     | Move forward in history                                              |                        | []   |                                                            |
| `y`         | Yank selection                                                       | `helix-kill-ring-save` | [x]  |                                                            |
| `p`         | Paste after selection                                                | `yank`                 | [x]  |                                                            |
| `P`         | Paste before selection                                               |                        | []   |                                                            |
| `"` `<reg>` | Select a register to yank to or paste from                           |                        | []   |                                                            |
| `>`         | Indent selection                                                     |                        | []   |                                                            |
| `<`         | Unindent selection                                                   |                        | []   |                                                            |
| `=`         | Format selection (**LSP**)                                           |                        | []   |                                                            |
| `d`         | Delete selection                                                     |                        | []   |                                                            |
| `Alt-d`     | Delete selection, without yanking                                    |                        | []   |                                                            |
| `c`         | Change selection (delete and enter insert mode)                      |                        | []   |                                                            |
| `Alt-c`     | Change selection (delete and enter insert mode, without yanking)     |                        | []   |                                                            |
| `Ctrl-a`    | Increment object (number) under cursor                               |                        | []   |                                                            |
| `Ctrl-x`    | Decrement object (number) under cursor                               |                        | []   |                                                            |
| `Q`         | Start/stop macro recording to the selected register (experimental)   |                        | []   |                                                            |
| `q`         | Play back a recorded macro from the selected register (experimental) |                        | []   |                                                            |
|             |                                                                      |                        | []   |                                                            |
|             |                                                                      |                        | []   |                                                            |

#### Shell

| Key                     | Description                                                                      | Command               |
|-------------------------|----------------------------------------------------------------------------------|-----------------------|
| <code>&#124;</code>     | Pipe each selection through shell command, replacing with output                 | `shell_pipe`          |
| <code>Alt-&#124;</code> | Pipe each selection into shell command, ignoring output                          | `shell_pipe_to`       |
| `!`                     | Run shell command, inserting output before each selection                        | `shell_insert_output` |
| `Alt-!`                 | Run shell command, appending output after each selection                         | `shell_append_output` |
| `$`                     | Pipe each selection into shell command, keep selections where command returned 0 | `shell_keep_pipe`     |


### Selection manipulation

| Key                  | Description                                                   | Command                 | Todo |
|----------------------|---------------------------------------------------------------|-------------------------|------|
| `s`                  | Select all regex matches inside selections                    |                         |      |
| `S`                  | Split selection into sub selections on regex matches          |                         |      |
| `Alt-s`              | Split selection on newlines                                   |                         |      |
| `Alt-minus`          | Merge selections                                              |                         |      |
| `Alt-_`              | Merge consecutive selections                                  |                         |      |
| `&`                  | Align selection in columns                                    |                         |      |
| `_`                  | Trim whitespace from the selection                            |                         |      |
| `;`                  | Collapse selection onto a single cursor                       |                         |      |
| `Alt-;`              | Flip selection cursor and anchor                              |                         |      |
| `Alt-:`              | Ensures the selection is in forward direction                 |                         |      |
| `,`                  | Keep only the primary selection                               |                         |      |
| `Alt-,`              | Remove the primary selection                                  |                         |      |
| `C`                  | Copy selection onto the next line (Add cursor below)          |                         |      |
| `Alt-C`              | Copy selection onto the previous line (Add cursor above)      |                         |      |
| `(`                  | Rotate main selection backward                                |                         |      |
| `)`                  | Rotate main selection forward                                 |                         |      |
| `Alt-(`              | Rotate selection contents backward                            |                         |      |
| `Alt-)`              | Rotate selection contents forward                             |                         |      |
| `%`                  | Select entire file                                            | `mark-whole-buffer`     | [x]  |
| `x`                  | Select current line, if already selected, extend to next line |                         | [x]  |
| `X`                  | Extend selection to line bounds (line-wise selection)         |                         | []   |
| `Alt-x`              | Shrink selection to line bounds (line-wise selection)         |                         |      |
| `J`                  | Join lines inside selection                                   |                         |      |
| `Alt-J`              | Join lines inside selection and select the inserted space     |                         |      |
| `K`                  | Keep selections matching the regex                            |                         |      |
| `Alt-K`              | Remove selections matching the regex                          |                         |      |
| `Ctrl-c`             | Comment/uncomment the selections                              |                         |      |
| `Alt-o`, `Alt-up`    | Expand selection to parent syntax node (**TS**)               | `flit-expand-selection` | []   |
| `Alt-i`, `Alt-down`  | Shrink syntax tree object selection (**TS**)                  | `flit-shrink-selection` | []   |
| `Alt-p`, `Alt-left`  | Select previous sibling node in syntax tree (**TS**)          |                         |      |
| `Alt-n`, `Alt-right` | Select next sibling node in syntax tree (**TS**)              |                         |      |
| `Alt-a`              | Select all sibling nodes in syntax tree (**TS**)              |                         |      |
| `Alt-e`              | Move to end of parent node in syntax tree (**TS**)            |                         |      |
| `Alt-b`              | Move to start of parent node in syntax tree (**TS**)          |                         |      |

### Search

Search commands all operate on the `/` register by default. To use a different register, use `"<char>`.

| Key   | Description                                 | Command              |
| ----- | -----------                                 | -------              |
| `/`   | Search for regex pattern                    | `search`             |
| `?`   | Search for previous pattern                 | `rsearch`            |
| `n`   | Select next search match                    | `search_next`        |
| `N`   | Select previous search match                | `search_prev`        |
| `*`   | Use current selection as the search pattern | `search_selection`   |

### Minor modes

These sub-modes are accessible from normal mode and typically switch back to normal mode after a command.

| Key      | Description                                        | Command        |
| -----    | -----------                                        | -------        |
| `v`      | Enter [select (extend) mode](#select--extend-mode) | `select_mode`  |
| `g`      | Enter [goto mode](#goto-mode)                      | N/A            |
| `m`      | Enter [match mode](#match-mode)                    | N/A            |
| `:`      | Enter command mode                                 | `command_mode` |
| `z`      | Enter [view mode](#view-mode)                      | N/A            |
| `Z`      | Enter sticky [view mode](#view-mode)               | N/A            |
| `Ctrl-w` | Enter [window mode](#window-mode)                  | N/A            |
| `Space`  | Enter [space mode](#space-mode)                    | N/A            |

These modes (except command mode) can be configured by
[remapping keys](https://docs.helix-editor.com/remapping.html#minor-modes).

#### View mode

Accessed by typing `z` in [normal mode](#normal-mode).

View mode is intended for scrolling and manipulating the view without changing
the selection. The "sticky" variant of this mode (accessed by typing `Z` in
normal mode) is persistent and can be exited using the escape key. This is
useful when you're simply looking over text and not actively editing it.


| Key                  | Description                                               | Command                 |
| -----                | -----------                                               | -------                 |
| `z`, `c`             | Vertically center the line                                | `align_view_center`     |
| `t`                  | Align the line to the top of the screen                   | `align_view_top`        |
| `b`                  | Align the line to the bottom of the screen                | `align_view_bottom`     |
| `m`                  | Align the line to the middle of the screen (horizontally) | `align_view_middle`     |
| `j`, `down`          | Scroll the view downwards                                 | `scroll_down`           |
| `k`, `up`            | Scroll the view upwards                                   | `scroll_up`             |
| `Ctrl-f`, `PageDown` | Move page down                                            | `page_down`             |
| `Ctrl-b`, `PageUp`   | Move page up                                              | `page_up`               |
| `Ctrl-u`             | Move cursor and page half page up                         | `page_cursor_half_up`   |
| `Ctrl-d`             | Move cursor and page half page down                       | `page_cursor_half_down` |

#### Goto mode

Accessed by typing `g` in [normal mode](#normal-mode).

Jumps to various locations.

| Key   | Description                                      | Command                    |
| ----- | -----------                                      | -------                    |
| `g`   | Go to line number `<n>` else start of file       | `goto_file_start`          |
| `e`   | Go to the end of the file                        | `goto_last_line`           |
| `f`   | Go to files in the selections                    | `goto_file`                |
| `h`   | Go to the start of the line                      | `goto_line_start`          |
| `l`   | Go to the end of the line                        | `goto_line_end`            |
| `s`   | Go to first non-whitespace character of the line | `goto_first_nonwhitespace` |
| `t`   | Go to the top of the screen                      | `goto_window_top`          |
| `c`   | Go to the middle of the screen                   | `goto_window_center`       |
| `b`   | Go to the bottom of the screen                   | `goto_window_bottom`       |
| `d`   | Go to definition (**LSP**)                       | `goto_definition`          |
| `y`   | Go to type definition (**LSP**)                  | `goto_type_definition`     |
| `r`   | Go to references (**LSP**)                       | `goto_reference`           |
| `i`   | Go to implementation (**LSP**)                   | `goto_implementation`      |
| `a`   | Go to the last accessed/alternate file           | `goto_last_accessed_file`  |
| `m`   | Go to the last modified/alternate file           | `goto_last_modified_file`  |
| `n`   | Go to next buffer                                | `goto_next_buffer`         |
| `p`   | Go to previous buffer                            | `goto_previous_buffer`     |
| `.`   | Go to last modification in current file          | `goto_last_modification`   |
| `j`   | Move down textual (instead of visual) line       | `move_line_down`           |
| `k`   | Move up textual (instead of visual) line         | `move_line_up`             |
| `w`   | Show labels at each word and select the word that belongs to the entered labels | `goto_word` |

#### Match mode

Accessed by typing `m` in [normal mode](#normal-mode).

Please refer to the relevant sections for detailed explanations about [surround](./surround.md) and [textobjects](./textobjects.md).

| Key              | Description                                     | Command                    |
| -----            | -----------                                     | -------                    |
| `m`              | Goto matching bracket (**TS**)                  | `match_brackets`           |
| `s` `<char>`     | Surround current selection with `<char>`        | `surround_add`             |
| `r` `<from><to>` | Replace surround character `<from>` with `<to>` | `surround_replace`         |
| `d` `<char>`     | Delete surround character `<char>`              | `surround_delete`          |
| `a` `<object>`   | Select around textobject                        | `select_textobject_around` |
| `i` `<object>`   | Select inside textobject                        | `select_textobject_inner`  |

TODO: Mappings for selecting syntax nodes (a superset of `[`).

#### Window mode

Accessed by typing `Ctrl-w` in [normal mode](#normal-mode).

This layer is similar to Vim keybindings as Kakoune does not support windows.

| Key                    | Description                                          | Command           |
| -----                  | -------------                                        | -------           |
| `w`, `Ctrl-w`          | Switch to next window                                | `rotate_view`     |
| `v`, `Ctrl-v`          | Vertical right split                                 | `vsplit`          |
| `s`, `Ctrl-s`          | Horizontal bottom split                              | `hsplit`          |
| `f`                    | Go to files in the selections in horizontal splits   | `goto_file`       |
| `F`                    | Go to files in the selections in vertical splits     | `goto_file`       |
| `h`, `Ctrl-h`, `Left`  | Move to left split                                   | `jump_view_left`  |
| `j`, `Ctrl-j`, `Down`  | Move to split below                                  | `jump_view_down`  |
| `k`, `Ctrl-k`, `Up`    | Move to split above                                  | `jump_view_up`    |
| `l`, `Ctrl-l`, `Right` | Move to right split                                  | `jump_view_right` |
| `q`, `Ctrl-q`          | Close current window                                 | `wclose`          |
| `o`, `Ctrl-o`          | Only keep the current window, closing all the others | `wonly`           |
| `H`                    | Swap window to the left                              | `swap_view_left`  |
| `J`                    | Swap window downwards                                | `swap_view_down`  |
| `K`                    | Swap window upwards                                  | `swap_view_up`    |
| `L`                    | Swap window to the right                             | `swap_view_right` |

#### Space mode

Accessed by typing `Space` in [normal mode](#normal-mode).

This layer is a kludge of mappings, mostly pickers.

| Key     | Description                                                             | Command                                    |
| -----   | -----------                                                             | -------                                    |
| `f`     | Open file picker                                                        | `file_picker`                              |
| `F`     | Open file picker at current working directory                           | `file_picker_in_current_directory`         |
| `b`     | Open buffer picker                                                      | `buffer_picker`                            |
| `j`     | Open jumplist picker                                                    | `jumplist_picker`                          |
| `g`     | Open changed file picker                                                | `changed_file_picker`                      |
| `G`     | Debug (experimental)                                                    | N/A                                        |
| `k`     | Show documentation for item under cursor in a [popup](#popup) (**LSP**) | `hover`                                    |
| `s`     | Open document symbol picker (**LSP**)                                   | `symbol_picker`                            |
| `S`     | Open workspace symbol picker (**LSP**)                                  | `workspace_symbol_picker`                  |
| `d`     | Open document diagnostics picker (**LSP**)                              | `diagnostics_picker`                       |
| `D`     | Open workspace diagnostics picker (**LSP**)                             | `workspace_diagnostics_picker`             |
| `r`     | Rename symbol (**LSP**)                                                 | `rename_symbol`                            |
| `a`     | Apply code action (**LSP**)                                             | `code_action`                              |
| `h`     | Select symbol references (**LSP**)                                      | `select_references_to_symbol_under_cursor` |
| `'`     | Open last fuzzy picker                                                  | `last_picker`                              |
| `w`     | Enter [window mode](#window-mode)                                       | N/A                                        |
| `c`     | Comment/uncomment selections                                            | `toggle_comments`                          |
| `C`     | Block comment/uncomment selections                                      | `toggle_block_comments`                    |
| `Alt-c` | Line comment/uncomment selections                                       | `toggle_line_comments`                     |
| `p`     | Paste system clipboard after selections                                 | `paste_clipboard_after`                    |
| `P`     | Paste system clipboard before selections                                | `paste_clipboard_before`                   |
| `y`     | Yank selections to clipboard                                            | `yank_to_clipboard`                        |
| `Y`     | Yank main selection to clipboard                                        | `yank_main_selection_to_clipboard`         |
| `R`     | Replace selections by clipboard contents                                | `replace_selections_with_clipboard`        |
| `/`     | Global search in workspace folder                                       | `global_search`                            |
| `?`     | Open command palette                                                    | `command_palette`                          |

> 💡 Global search displays results in a fuzzy picker, use `Space + '` to bring it back up after opening a file.

##### Popup

Displays documentation for item under cursor. Remapping currently not supported.

| Key      | Description |
| ----     | ----------- |
| `Ctrl-u` | Scroll up   |
| `Ctrl-d` | Scroll down |

##### Completion Menu

Displays documentation for the selected completion item. Remapping currently not supported.

| Key                         | Description                      |
| ----                        | -----------                      |
| `Shift-Tab`, `Ctrl-p`, `Up` | Previous entry                   |
| `Tab`, `Ctrl-n`, `Down`     | Next entry                       |
| `Enter`                     | Close menu and accept completion |
| `Ctrl-c`                    | Close menu and reject completion |

Any other keypresses result in the completion being accepted.

##### Signature-help Popup

Displays the signature of the selected completion item. Remapping currently not supported.

| Key     | Description        |
| ----    | -----------        |
| `Alt-p` | Previous signature |
| `Alt-n` | Next signature     |

#### Unimpaired

These mappings are in the style of [vim-unimpaired](https://github.com/tpope/vim-unimpaired).

| Key      | Description                                  | Command               |
| -----    | -----------                                  | -------               |
| `]d`     | Go to next diagnostic (**LSP**)              | `goto_next_diag`      |
| `[d`     | Go to previous diagnostic (**LSP**)          | `goto_prev_diag`      |
| `]D`     | Go to last diagnostic in document (**LSP**)  | `goto_last_diag`      |
| `[D`     | Go to first diagnostic in document (**LSP**) | `goto_first_diag`     |
| `]f`     | Go to next function (**TS**)                 | `goto_next_function`  |
| `[f`     | Go to previous function (**TS**)             | `goto_prev_function`  |
| `]t`     | Go to next type definition (**TS**)          | `goto_next_class`     |
| `[t`     | Go to previous type definition (**TS**)      | `goto_prev_class`     |
| `]a`     | Go to next argument/parameter (**TS**)       | `goto_next_parameter` |
| `[a`     | Go to previous argument/parameter (**TS**)   | `goto_prev_parameter` |
| `]c`     | Go to next comment (**TS**)                  | `goto_next_comment`   |
| `[c`     | Go to previous comment (**TS**)              | `goto_prev_comment`   |
| `]T`     | Go to next test (**TS**)                     | `goto_next_test`      |
| `[T`     | Go to previous test (**TS**)                 | `goto_prev_test`      |
| `]p`     | Go to next paragraph                         | `goto_next_paragraph` |
| `[p`     | Go to previous paragraph                     | `goto_prev_paragraph` |
| `]g`     | Go to next change                            | `goto_next_change`    |
| `[g`     | Go to previous change                        | `goto_prev_change`    |
| `]G`     | Go to last change                            | `goto_last_change`    |
| `[G`     | Go to first change                           | `goto_first_change`   |
| `]Space` | Add newline below                            | `add_newline_below`   |
| `[Space` | Add newline above                            | `add_newline_above`   |

## Insert mode

Accessed by typing `i` in [normal mode](#normal-mode).

Insert mode bindings are minimal by default. Helix is designed to
be a modal editor, and this is reflected in the user experience and internal
mechanics. Changes to the text are only saved for undos when
escaping from insert mode to normal mode.

> 💡 New users are strongly encouraged to learn the modal editing paradigm
> to get the smoothest experience.

| Key                                         | Description                 | Command                  |
| -----                                       | -----------                 | -------                  |
| `Escape`                                    | Switch to normal mode       | `normal_mode`            |
| `Ctrl-s`                                    | Commit undo checkpoint      | `commit_undo_checkpoint` |
| `Ctrl-x`                                    | Autocomplete                | `completion`             |
| `Ctrl-r`                                    | Insert a register content   | `insert_register`        |
| `Ctrl-w`, `Alt-Backspace`                   | Delete previous word        | `delete_word_backward`   |
| `Alt-d`, `Alt-Delete`                       | Delete next word            | `delete_word_forward`    |
| `Ctrl-u`                                    | Delete to start of line     | `kill_to_line_start`     |
| `Ctrl-k`                                    | Delete to end of line       | `kill_to_line_end`       |
| `Ctrl-h`, `Backspace`, `Shift-Backspace`    | Delete previous char        | `delete_char_backward`   |
| `Ctrl-d`, `Delete`                          | Delete next char            | `delete_char_forward`    |
| `Ctrl-j`, `Enter`                           | Insert new line             | `insert_newline`         |

These keys are not recommended, but are included for new users less familiar
with modal editors.

| Key                                         | Description                 | Command                  |
| -----                                       | -----------                 | -------                  |
| `Up`                                        | Move to previous line       | `move_line_up`           |
| `Down`                                      | Move to next line           | `move_line_down`         |
| `Left`                                      | Backward a char             | `move_char_left`         |
| `Right`                                     | Forward a char              | `move_char_right`        |
| `PageUp`                                    | Move one page up            | `page_up`                |
| `PageDown`                                  | Move one page down          | `page_down`              |
| `Home`                                      | Move to line start          | `goto_line_start`        |
| `End`                                       | Move to line end            | `goto_line_end_newline`  |

As you become more comfortable with modal editing, you may want to disable some
insert mode bindings. You can do this by editing your `config.toml` file.

```toml
[keys.insert]
up = "no_op"
down = "no_op"
left = "no_op"
right = "no_op"
pageup = "no_op"
pagedown = "no_op"
home = "no_op"
end = "no_op"
```

## Select / extend mode

Accessed by typing `v` in [normal mode](#normal-mode).

Select mode echoes Normal mode, but changes any movements to extend
selections rather than replace them. Goto motions are also changed to
extend, so that `vgl`, for example, extends the selection to the end of
the line.

Search is also affected. By default, `n` and `N` will remove the current
selection and select the next instance of the search term. Toggling this
mode before pressing `n` or `N` makes it possible to keep the current
selection. Toggling it on and off during your iterative searching allows
you to selectively add search terms to your selections.

## Picker

Keys to use within picker. Remapping currently not supported.
See the documentation page on [pickers](./pickers.md) for more info.
[Prompt](#prompt) keybinds also work in pickers, except where they conflict with picker keybinds.

| Key                          | Description                                                |
| -----                        | -------------                                              |
| `Shift-Tab`, `Up`, `Ctrl-p`  | Previous entry                                             |
| `Tab`, `Down`, `Ctrl-n`      | Next entry                                                 |
| `PageUp`, `Ctrl-u`           | Page up                                                    |
| `PageDown`, `Ctrl-d`         | Page down                                                  |
| `Home`                       | Go to first entry                                          |
| `End`                        | Go to last entry                                           |
| `Enter`                      | Open selected                                              |
| `Alt-Enter`                  | Open selected in the background without closing the picker |
| `Ctrl-s`                     | Open horizontally                                          |
| `Ctrl-v`                     | Open vertically                                            |
| `Ctrl-t`                     | Toggle preview                                             |
| `Escape`, `Ctrl-c`           | Close picker                                               |

## Prompt

Keys to use within prompt, Remapping currently not supported.

| Key                                         | Description                                                             |
| -----                                       | -------------                                                           |
| `Escape`, `Ctrl-c`                          | Close prompt                                                            |
| `Alt-b`, `Ctrl-Left`                        | Backward a word                                                         |
| `Ctrl-b`, `Left`                            | Backward a char                                                         |
| `Alt-f`, `Ctrl-Right`                       | Forward a word                                                          |
| `Ctrl-f`, `Right`                           | Forward a char                                                          |
| `Ctrl-e`, `End`                             | Move prompt end                                                         |
| `Ctrl-a`, `Home`                            | Move prompt start                                                       |
| `Ctrl-w`, `Alt-Backspace`, `Ctrl-Backspace` | Delete previous word                                                    |
| `Alt-d`, `Alt-Delete`, `Ctrl-Delete`        | Delete next word                                                        |
| `Ctrl-u`                                    | Delete to start of line                                                 |
| `Ctrl-k`                                    | Delete to end of line                                                   |
| `Backspace`, `Ctrl-h`, `Shift-Backspace`    | Delete previous char                                                    |
| `Delete`, `Ctrl-d`                          | Delete next char                                                        |
| `Ctrl-s`                                    | Insert a word under doc cursor, may be changed to Ctrl-r Ctrl-w later   |
| `Ctrl-p`, `Up`                              | Select previous history                                                 |
| `Ctrl-n`, `Down`                            | Select next history                                                     |
| `Ctrl-r`                                    | Insert the content of the register selected by following input char     |
| `Tab`                                       | Select next completion item                                             |
| `BackTab`                                   | Select previous completion item                                         |
| `Enter`                                     | Open selected                                                           |
