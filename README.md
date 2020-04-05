# Meow: Modal Editing On Wheel
 
> Less is more

Document is still WIP.
 
# Highlights

- Semantic but still effective layout on Dvorak.
- A complete modal editing plays well with special modes, like dired, etc.
- Combined __navigation__ and __selection__, you rarely use a command to activate selection.
- Use kbd macros when calling built-in commands, drop-in replacement will work out-of-box.
- A `keypad` state allows you execute commands from C-x and C-c keymaps without modifiers.
- Multiple-cursors aware implementation. 

# Modal States

### INSERT 

Just vanilla Emacs, the only difference is that you can switch to `NORMAL` state with <kbd>SPC</kbd>.

### NORMAL

Default state for text editing buffers.

![Cheatsheet](https://i.imgur.com/uDNJUKw.png "Cheatsheet")

### MOTION

Default state for special-mode buffers. There are only two additional bindings in `MOTION` state.

<kbd>SPC</kbd> is used as `LEADER` key, the original command on <kbd>SPC</kbd> is bound to <kbd>SPC SPC</kbd>.

<kbd>ESC</kbd> is used to switch to previous buffer.

### KEYPAD

This is inspired by god-mode. You can activate `KEYPAD` state with <kbd>SPC x</kbd> or <kbd>SPC c</kbd> in `NORMAL` and `MOTION` state.

In this state, every single key is considered with <kbd>Control</kbd>, <kbd>x</kbd> means <kbd>C-x</kbd>; If you want a single <kbd>x</kbd>, use <kbd>SPC x</kbd>; If you want a <kbd>M-x</kbd>, use <kbd>m x</kbd>.

To call a command on <kbd>C-c M-j</kbd>, use <kbd>SPC c m j</kbd>.

To call a command on <kbd>C-c C-x RET</kbd>, use <kbd>SPC c x SPC RET</kbd>. If there's no command on <kbd>C-c C-x C-RET</kbd>, it is okay to omit the second <kbd>SPC</kbd>, just use <kbd>SPC c x RET</kbd>.

### LICENSE

License under GPL v3.

