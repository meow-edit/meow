[![MELPA](https://melpa.org/packages/meow-badge.svg)](https://melpa.org/#/meow)

![Logo](meow.svg)

> 少即是多

Meow 尝试让使用者用更少的配置，达到更好的集成效果。以及使用更少的命令，做更多的事。

# 安装

Meow 已经发布到了 [Melpa](https://melpa.org/)。

```emacs-lisp
(require 'meow)

(meow-global-mode 1)

(with-eval-after-load "meow"
  ;; meow-setup 用于自定义按键绑定，可以直接使用下文中的示例
  (meow-setup)
  ;; 如果你需要在 NORMAL 下使用相对行号（基于 display-line-numbers-mode）
  (meow-setup-line-number)
  ;; 如果你需要自动的 mode-line 设置（如果需要自定义见下文对 `meow-indicator' 说明）
  (meow-setup-indicator))

```

## 如果使用 use-package

```emacs-lisp
(use-package meow
  ;; 如果你设置了 `use-package-always-defer'
  :demand nil
  :init
  (meow-global-mode 1)
  :config
  ;; meow-setup 用于自定义按键绑定，可以直接使用下文中的示例
  (meow-setup)
  ;; 如果你需要在 NORMAL 下使用相对行号（基于 display-line-numbers-mode）
  (meow-setup-line-number)
  ;; 如果你需要自动的 mode-line 设置（如果需要自定义见下文对 `meow-indicator' 说明）
  (meow-setup-indicator))
```

# Meow 是什么？

Meow 是一套完整的模式编辑的方案，这节内容会说明它的特点以及存在的意义。

## 1. 四个模式

Meow 有四个模式。

* `NORMAL`：在文本编辑中的默认模式，单键绑定为 Meow 中的命令。见后文中完整的命令说明。
* `INSERT`：插入模式，基本为默认的 Emacs 按键，仅有 <kbd>ESC</kbd> 会回到 `NORMAL` 模式。
* `MOTION`：Special Mode 中默认使用的模式，基本为默认的 Emacs 按键，仅有 <kbd>SPC</kbd> 绑定为 Leader 并将 <kbd>SPC SPC</kbd> 绑定为原 <kbd>SPC</kbd> 上的命令。
* `KEYPAD`：用于执行组合键命令的临时模式，用单键的序列来模拟组合键的输入。

## 2. 几乎没有默认的按键绑定
<a id="keybinding"></a>Meow 提供一套完整的模式编辑命令，交由用户自己根据喜好设置按键布局。下面有一些针对不同键盘布局的推荐方案，可以直接使用或做为自定义的起点。

<details>
    <summary>适用于 Qwerty 布局，Vim 类似的按键风格的 <code>meow-setup</code> 实现</summary>

```emacs-lisp
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
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
   '("G" . meow-goto-line)
   '("h" . meow-left)
   '("H" . meow-left-expand)
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
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("u" . undo)
   '("v" . meow-visit)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
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
    <summary>适用于 Dvorak Simplified 布局的 <code>meow-setup</code> 实现</summary>

```emacs-lisp
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-dvorak)
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
   '("G" . meow-goto-line)
   '("h" . meow-left)
   '("H" . meow-left-expand)
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
   '("t" . meow-right)
   '("T" . meow-right-expand)
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
    <summary>适用于 Dvorak Programmer 布局的 <code>meow-setup</code> 实现</summary>

```emacs-lisp
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-dvp)
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
   '("G" . meow-goto-line)
   '("h" . meow-left)
   '("H" . meow-left-expand)
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
   '("t" . meow-right)
   '("T" . meow-right-expand)
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
    <summary>适用于 Colemak 布局的 <code>meow-setup</code> 实现</summary>

```emacs-lisp
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak)
  (meow-motion-overwrite-define-key
   ;; Use e to move up, n to move down.
   ;; Since special modes usually use n to move down, we only overwrite e here.
   '("e" . meow-prev))
  (meow-leader-define-key
   ;; To execute the originally e in MOTION state, use SPC e.
   '("e" . meow-motion-origin-command)
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
   '("1" . meow-expand-1)
   '("2" . meow-expand-2)
   '("3" . meow-expand-3)
   '("4" . meow-expand-4)
   '("5" . meow-expand-5)
   '("6" . meow-expand-6)
   '("7" . meow-expand-7)
   '("8" . meow-expand-8)
   '("9" . meow-expand-9)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("/" . meow-visit)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . meow-change-save)
   '("d" . meow-delete)
   '("e" . meow-prev)
   '("E" . meow-prev-expand)
   '("f" . meow-find)
   '("F" . meow-find-expand)
   '("g" . meow-keyboard-quit)
   '("G" . meow-goto-line)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-right)
   '("I" . meow-right-expand)
   '("j" . meow-join)
   '("J" . delete-indentation)
   '("k" . meow-kill)
   '("K" . meow-kill-whole-line)
   '("l" . meow-line)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-block-expand)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("R" . meow-replace-save)
   '("s" . meow-insert)
   '("S" . meow-open-above)
   '("t" . meow-till)
   '("T" . meow-till-expand)
   '("u" . undo)
   '("v" . meow-search)
   '("V" . meow-pop-search)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-delete)
   '("y" . meow-save)
   '("z" . meow-pop-selection)
   '("Z" . meow-pop-all-selection)
   '("&" . meow-query-replace)
   '("%" . meow-query-replace-regexp)
   '("?" . meow-cheatsheet)
   '("<escape>" . meow-last-buffer)
   '("<f3>" . meow-start-kmacro)
   '("<f4>" . meow-end-or-call-kmacro)))
```
</details>

### 需要 cheatsheet?

使用 `M-x meow-cheatsheet`. 这个命令会根据你当前绑定的按键为你生成一个 Cheatsheet。

## 3. NORMAL 模式，移动即是选择
这是 Meow 向 Kakoune 借鉴的一个极为好用的特点。

Meow 中的移动命令，除了单一字符的移动，都会同时选中一个区域。例如向前移动一个词会选中当前位置到下一个词的结尾，这样就可以直接获得两个较为有意义的位置。（跟据你绑定的按键）之后你可以使用 <kbd>i</kbd> 在选择的开头进入 `INSERT` 模式，或用 <kbd>a</kbd> 在选择的结尾进入 `INSERT` 模式。

假设使用这样的按键设定 <kbd>i</kbd> `meow-insert`, <kbd>a</kbd> `meow-append`, <kbd>e</kbd> `meow-line`, <kbd>k</kbd> `meow-kill`, <kbd>x</kbd> `meow-save`, <kbd>;</kbd> `meow-reverse`, <kbd>0</kbd> - <kbd>9</kbd> `digit-argument`.以下表格列出了 Evil 和 Meow 中做同样的一些事所有的按键。

| 目的        | Evil | Meow |
|-------------|------|------|
| 选择一行    | V    | e   |
| 移动到行首  | 0    | e;    |
| 移动到行尾  | $    | e    |
| 在行首插入  | 0i   | ei   |
| 在行尾插入  | A    | ea   |
| 删除一行    | dd   | ek   |
| 复制一行    | yy   | ex   |
| 选择5行     | V4j  | 5e   |
| 复制5行     | y4j  | 5ex  |
| 删除5行     | d4j  | 5ek  |
| 向上删除5行 | d4k  | -5ek |
| 向上复制5行 | y4k  | -5ex |

注：在 Meow 的理念中，使用 Shift 也是一个按键，所以用两个小写的单键来替换一个大写按键并不吃亏。设计原则上在常用的命令上保持简洁，不常用命令允许稍长但命令的种类和概念尽量少。

## 4. MOTION 模式，和 Special mode 无痛的集成
Emacs 默认并不使用模式编辑，各类非编辑型的 Special Mode 通常都直接以合理的单键做为命令。我更推荐于使用这些原本设定好的默认按键，而不是花费精力来维持一套自定义的快捷键。

对于各种 Special Mode，Meow 提供了 `MOTION` 模式，该模式下 <kbd>SPC</kbd> 做为 Leader，而原本绑定在 <kbd>SPC</kbd> 上的功能被绑定在 <kbd>SPC SPC</kbd> 。如果想使用 <kbd>j</kbd> 或 <kbd>k</kbd> 做为此时的上下方向，可以用类似的方式，即将原本在 <kbd>j</kbd> 和 <kbd>k</kbd> 的命令绑定在 <kbd>SPC j</kbd> 和 <kbd>SPC k</kbd> 上。

见后文中 `meow-motion-overwrite-define-key` 函数的说明。

## 5. KEYPAD 模式，以不按修饰键的方式输入组合键命令
Meow 借鉴 God Mode 引入了 `KEYPAD` 模式。

在 `NORMAL` 或 `MOTION` 模式中（默认）<kbd>SPC x</kbd> 将会触发 `KEYPAD` 模式，并将当前的输入转化成 `C-x`。后续的单键输入，将被自动翻译成带有 `Ctrl` 修饰的组合键，直到匹配到一个有效的命令，执行并退出 `KEYPAD` 模式。你还可以使用 <kbd>SPC c</kbd>， <kbd>SPC h</kbd>, <kbd>SPC m</kbd> 或 <kbd>SPC g</kbd> 进入 `KEYPAD` 模式（默认）。

在 `KEYPAD` 中如果要需要非 `C-` 的输入则需要使用前缀：
- <kbd>SPC</kbd> 做为前缀，表示没有任何修饰符，在没有歧义时，可以省略。
- <kbd>m</kbd> 做为前缀，表示以 `Meta` 键修饰。
- <kbd>g</kbd> 做为前缀，表示以 `Ctrl+Meta` 键修饰。

以下是一些例子：
| Vanilla Emacs | Meow KEYPAD                                                     |
|---------------|-----------------------------------------------------------------|
| C-x C-f       | <kbd>SPC x f</kbd>                                              |
| C-c C-c       | <kbd>SPC c c</kbd>                                              |
| C-h k         | <kbd>SPC h SPC k</kbd> 或 <kbd>SPC h k</kbd> (无歧义时)         |
| C-M-t         | <kbd>SPC g t</kbd>                                              |
| M-r           | <kbd>SPC m r</kbd>                                              |
| C-c M-n n     | <kbd>SPC c m n SPC n</kbd> 或 <kbd>SPC c m n n</kbd> (无歧义时) |

如此一来你便可以不用刻意为每个插件绑定一套符合模式编辑风格的快捷键，又几乎可以在不用修饰键（Ctrl 和 Meta）的情况下执行所有的命令。

### 如何和 which-key 集成？

在 Meow 中可以使用 [which-key](https://github.com/justbur/emacs-which-key)，并不需要任何额外的配置，对 Leader 或是 Emacs 原生的按键都有效。
但是由于 `KEYPAD` 的执行机制比较特别，所以 Meow 内置了一个专门做用于 `KEYPAD` 的按键提示功能。对于其它的按键的提示，可以使用 which-key。

![meow-describe-keymap](https://user-images.githubusercontent.com/11796018/104113302-3efae680-5333-11eb-86cb-f6430add7ae9.png)


# 完整的命令说明

## 模式切换

`meow-insert` 在当前位置或是选择的开始位置进入 `INSERT` 模式。

`meow-append` 在当前位置或是选择的结束位置进入 `INSERT` 模式。

`meow-open-above` 在当前位置上面插入一行，并进入 `INSERT` 模式。

`meow-open-below` 在当前位置下面插入一行，并进入 `INSERT` 模式。

## 移动&选择

对于以有的选择的情况，可以使用这些命令

`meow-reverse` 翻转选择的方向，类似于 `exchange-point-and-mark`。

`meow-pop-selection` 返回到上一个选择。

`meow-pop-all-selection` 返回到激活选择之前的位置。

`meow-expand-(0-9)` 一部分命令在选择后会有数字的位置提示，可以使用该组命令直接移动到对应的位置。

以下是四个方向上的基础移动，每一组命令的后者会激活 `char` 类型的选择；而前者可以延展 `char` 类型的选择，取消非 `char` 类型的选择。

`meow-left` 和 `meow-left-expand` 按字符向左移动。

`meow-right` 和 `meow-right-expand` 按字符向右移动。

`meow-head` 和 `meow-head-expand` 按字符朝向行首移动。

`meow-tail` 和 `meow-tail-expand` 按字符朝向行尾移动。

`meow-prev` 和 `meow-prev-expand` 前一行。

`meow-next` 和 `meow-next-expand` 后一行。

接下来的这些命令都是同时完成选择和移动。

`meow-visit` 搜索正则，到达并激活类型为 `visit` 的选择，可以使用 `negative-argument` 进行反向的搜索，最近若干次搜索的历史会被记录，可以使用 `meow-pop-search` 弹出最近的一项。

注1：该命令提供了对文中出现的符号的补全，也可以不使用补全，搜索任意正则。

`meow-search` 搜索并选择下一个出现的位置，根据选择的方向，来决定向前还是向后搜索。（使用 `meow-reverse` 来翻转选择的方向）

`meow-mark-word` 选择当前的词，类型为 `expand word`，并记录到最近的搜索（可以使用 `meow-search` 搜索）

`meow-mark-symbol` 选择当前的符号，类型为 `expand word` （与 `meow-mark-word` 相同），并记录到最近的搜索（可以使用 `meow-search` 搜索）

`meow-next-word` 和 `meow-back-word` 分别为向前或后移动一个词，并激活移动范围的选择。如果当前选择的类型为 `expand word` 则会延展选择范围，否则选择类型为 `word`。

`meow-next-symbol` 和 `meow-back-symbol` 分别为向前或后移动一个符号，并激活移动范围的选择。如果当前选择的类型为 `expand word` 则会延展选择范围，否则选择类型为 `word`。

`meow-line` 选择当前行，重复使用时会扩展选择。可以通过 `meow-reverse` 反转方向或用 `negative-argument` 反向。

`meow-goto-line` 移动到指定行，选择那一行并居中屏幕。使用命令的数字前缀指定行号或执行命令后输入行号。

`meow-block` 和 `meow-expand-block` 选择下一个块（指一对括号），重复使用时前者会扩展到更大的块，后者会向扩展到下一个块。使用 `negative-argument` 反向。

`meow-join` 选择以当前位置 `delete-indentation` 会影响的范围，使用 `negative-argument` 反向。

`meow-find` 追加输入一个字符，并选择当前位置到该字符的后面，使用 `negative-argument` 反向。

`meow-till` 追加输入一个字符，并选择当前位置到该字符的前面，使用 `negative-argument` 反向。

以下为一些针对 thing 的选择，目前可用的 thing 如下。通过配置 `meow-char-thing-table` 为不同的 thing 分配不同的按键。

| thing          | 默认按键 |
|----------------|----------|
| 圆括号 round   | r        |
| 方括号 square  | s        |
| 大括号 curly   | c        |
| 字符串 string  | g        |
| 段落 paragraph | p        |
| 行 line        | l        |
| 定义 defun     | d        |
| 缓冲区 buffer  | b        |

`meow-inner-of-thing` 选择 thing 的内部。

`meow-bounds-of-thing` 选择 thing 的全部。

`meow-beginning-of-thing` 选择当前位置到 thing 的起点。

`meow-end-of-thing` 选择当前位置到 thing 的终点。

## 删除&修改

`meow-kill` Kill当前的选择，无选择时为执行 <kbd>C-k</kbd> 的功能。（在不同场合表现不同，如在 paredit-mode 下，`C-k` 会被绑定为 `paredit-kill`）

`meow-delete` 删除当前选择（不进入 kill-ring），无选择时为执行 <kbd>C-d</kbd> 的功能。（不同场合表现不同）

`meow-change` 删除当前选择并进入插入模式。

`meow-save` 复制当前选择（不会进入系统剪贴板，如需复制到系统剪贴板，使用 `kill-ring-save`）

`meow-yank` 粘贴到当前光标前，可以使用 `negative-argument` 粘贴到当前光标后。（不会使用系统剪贴板，如需系统剪贴板，使用 `yank`）

`meow-replace` 用粘贴的方式替换掉当前的选择区域。

`meow-replace-save` 将当前的选择区域的内容和当前 kill-ring 的首项交换。

## Grab

![grab](https://user-images.githubusercontent.com/11796018/106320175-719e5c00-62ad-11eb-88ac-5cff587ec036.gif)

Meow 可以创建一个 Grab 区域（第二选区）。Grab 如果不在当前的任意一个窗口展示，则会自动取消。在 Grab 有效时，所有操作 kill-ring 的 Meow 中的命令都会视 Grab 区域为 kill-ring 的 Car。所有 `meow-kill` 会将当前的内容移动并覆盖到 Grab 区中， `meow-replace-save` 会交换 Grab 和当前选择， `meow-save` 会复制当前的内容并覆盖到 Grab 区中。

Grab 的另一个作用是在激活时，如果当前执行的命令在 `meow-grab-fill-commands` 中，则会自动将内容插入到 minibuffer。

`meow-grab` 创建 Grab。

`meow-pop-grab` 返回 Grab 的位置并选消（无论是否在当前的 buffer)。

如果想原地关闭，可以使用 `meow-grab`， `meow-pop-grab` 的方式，由于 grab 全局唯一。

## Kmacros

`meow-start-kmacro` 功能类似于 `kmacro-start-macro-or-insert-counter`. 区别在于如果当前的选择类型为 `line`，会将光标自动移动至选择区域的开头，并取消选择。在录制结束时，会自动的将 kmacro 应用到每一行（调用 `apply-macro-to-region-lines`）

`meow-end-or-call-kmacro` 功能类似于 `kmacro-end-or-call-macro`, 和 `meow-start-kmacro` 对应.

## 其它命令

`meow-quit` 关闭当前的 window 或退到上一个 buffer。

`meow-keyboard-quit` 类似 keyboard-quit。

`meow-cancel-selection` 取消选择。

`meow-undo` 撤销，类似 `undo`，但会先取消选择。（默认的 `undo` 在 region 激活时会撤销 region 之内的修改）

# 常用函数说明

`(meow-setup-indicator)` 向当前的 modeline 的开头追加一个显示当前模式的指示器，如果需要更加自定义的配置，见下文。

`(meow-indicator)` 返回一个可以用在 modeline 中的指示器。

`(meow-normal-define-key & args)` 用于定义 `NORMAL` 模式下的按键，你将使用这个函数定义你完整的键盘布局。

见上文中对于[每种键盘布局的设置](#keybinding)。

`(meow-leader-define-key & args)` 用于定义 Leader Keymap。

使用示例如下：

```emacs-lisp
(meow-leader-define-key
  '("d" . dired)
  '("f" . find-file))
```

`(meow-setup-line-number)` 按 Meow 的方式来设置行号，即在 `NORMAL` 模式中使用相对行号。

`(meow-motion-overwrite-define-key & args)` 用于定义 `MOTION` 模式下的按键。

以下的示例展示了如何在 `MOTION` 模式中使用 <kbd>j</kbd> 和 <kbd>k</kbd> 进行上下移动，并用 <kbd>SPC j</kbd> 和 <kbd>SPC k</kbd> 来执行原本的命令。

```emacs-lisp
(meow-motion-overwrite-define-key
  '("j" . meow-next)
  '("k" . meow-prev))

;; 如果你希望用 Leader 前缀来执行原本在 j/k 的命令
(meow-leader-define-key
  '("j" . meow-motion-origin-command)
  '("k" . meow-motion-origin-command))
```

# 常用变量说明

`meow-normal-state-mode-list` 指示 Meow 应该使用 `NORMAL` 模式的主模式的列表，因为 Meow 还比较新，所以这个变量的默认值可能不能满足你的需要。如果你发现某个模式应该使用 `NORMAL` 模式（而不是 `MOTION` 模式），可以使用如下的配置并欢迎开 Issue 说明这种情况。

```emacs-lisp
(use-package meow
  ...
  :config
  (add-to-list 'meow-normal-state-mode-list 'py-shell-mode))
```

`meow-selection-command-fallback` Meow 中有一组命令是专门作用于选择区域的，这个变量允许你设置在没有选择时的回调行为。该组命令包括：

- `meow-cancel-selection`
- `meow-pop-selection`
- `meow-reverse`
- `meow-save`
- `meow-change`
- `meow-replace`
- `meow-delete`
- `meow-kill`

```emacs-lisp
;; 默认值
(setq meow-selection-command-fallback
  '((meow-replace . meow-replace-char)
    (meow-change . meow-change-char)
    (meow-save . meow-save-char)
    (meow-kill . meow-C-k)
    (meow-delete . meow-C-d)))

;; 如果你喜欢在没有选择时直接进入 KEYPAD 模式。
(setq meow-selection-command-fallback
  '((meow-replace . meow-keypad-start)
    (meow-change . meow-keypad-start)
    (meow-save . meow-keypad-start)
    (meow-reverse . meow-keypad-start)))
```

`meow-expand-hint-remove-delay` 在一个移动的命令之后，扩展的数字提示显示时长，默认为 `1.0` 秒。

```emacs-lisp
(setq meow-expand-hint-remove-delay 1.0)
```

`meow-select-on-exit` 是否在退出 INSERT 模式的时候，激活选择（从当前位置到进入 INSERT 模式的位置），默认为 `nil`。

`meow-replace-state-name-list` 可以用来定制指示器的文本。

```emacs-lisp
(setq meow-replace-state-name-list
 '((normal . "Ꮚ•ꈊ•Ꮚ")
   (insert . "Ꮚ`ꈊ´Ꮚ")
   (keypad . "Ꮚ'ꈊ'Ꮚ")
   (motion . "Ꮚ-ꈊ-Ꮚ")))
```

# FAQ
## 使用 EXWM

EXWM 是一个 X 的平铺窗口管理器，使用 EXWM 时，你可能需要设置一组全局的快捷键，用来做为 Leader 和 KEYPAD 的入口。

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
