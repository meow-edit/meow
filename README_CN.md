[![MELPA](https://melpa.org/packages/meow-badge.svg)](https://melpa.org/#/meow)

![Logo](meow.svg)

> 少即是多

Meow 尝试让使用者用更少的配置，达到更好的集成效果。以及使用更少的命令，做更多的事。

# 安装

```emacs-lisp
(use-package meow
  :init
  (meow-global-mode 1)
  ;; meow-setup 用于自定义按键绑定，可以直接使用下文中的示例
  (meow-setup)
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
Meow 提供一套完整的模式编辑命令，交由用户自己根据喜好设置按键布局。下面有一些针对不同键盘布局的推荐方案，可以直接使用或做为自定义的起点。

### 针对 Qwerty 布局，Vim 类似的按键风格
TBD

### 针对 Dvorak Simplified 布局
TBD

### 针对 Dvorak Programmer 布局
TBD

### 针对 Colemak 布局
TBD

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

在 `NORMAL` 或 `MOTION` 模式中（默认）<kbd>SPC x</kbd> 将会触发 `KEYPAD` 模式，并将当前的输入转化成 `C-x`。后续的单键输入，将被自动翻译成带有 `Ctrl` 修饰的组合键，直到匹配到一个有效的命令，执行并退出 `KEYPAD` 模式。你还可以使用 <kbd>SPC c</kbd> 和 <kbd>SPC h</kbd> 进入 `KEYPAD` 模式（默认）。

在 `KEYPAD` 中如果要需要非 `C-` 的输入则需要使用前缀：
- <kbd>SPC</kbd> 做为前缀，表示没有任何修饰符，在没有歧义时，可以省略。
- <kbd>m</kbd> 做为前缀，表示以 `Meta` 键修饰。
- <kbd>g</kbd> 做为前缀，表示以 `Ctrl+Meta` 键修饰。

以下是一些例子：
| Vanilla Emacs | Meow KEYPAD                                   |
|---------------|-----------------------------------------------|
| C-x C-f       | SPC x f                                       |
| C-c C-c       | SPC c c                                       |
| C-h k         | SPC h SPC k 或 SPC h k (无歧义时)             |
| C-M-t         | SPC g t                                       |
| M-r           | SPC m r                                       |
| C-c M-n n     | SPC c c m n SPC n 或 SPC c c m n n (无歧义时) |

如此一来你便可以不用刻意为每个插件绑定一套符合模式编辑风格的快捷键，又几乎可以在不用修饰键（Ctrl 和 Meta）的情况下执行所有的命令。

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

以下是四个方向上的基础移动，每一组命令的后者会激活 `char` 类型的选择；而前者可以延展 `char` 类型的选择，取消非 `char` 类型的选择。

`meow-head` 和 `meow-head-expand` 朝向行首移动。

`meow-tail` 和 `meow-tail-expand` 朝向行尾移动。

`meow-prev` 和 `meow-prev-expand` 前一行。

`meow-next` 和 `meow-next-expand` 后一行。

接下来的这些命令都是同时完成选择和移动。

`meow-visit` 搜索正则，到达并激活类型为 `visit` 的选择，可以使用 `negative-argument` 进行反向的搜索，最近若干次搜索的历史会被记录，可以使用 `meow-pop-search` 弹出最近的一项。

注1：该命令提供了对文中出现的符号的补全，也可以不使用补全，搜索任意正则。

`meow-search` 搜索并选择下一个出现的位置，根据选择的方向，来决定向前还是向后搜索。（使用 `meow-reverse` 来翻转选择的方向）

`meow-mark-word` 选择当前的词，类型为 `expand word`，并记录到最近的搜索（可以使用 `meow-search` 搜索）

`meow-mark-symbol` 选择当前的符号，类型为 `expand symbol`，并记录到最近的搜索（可以使用 `meow-search` 搜索）

`meow-next-word` 和 `meow-back-word` 分别为向前或后移动一个词，并激活移动范围的选择。如果当前选择的类型为 `expand word` 则会延展选择范围，否则选择类型为 `word`。

`meow-next-symbol` 和 `meow-back-symbol` 分别为向前或后移动一个符号，并激活移动范围的选择。如果当前选择的类型为 `expand word` 则会延展选择范围，否则选择类型为 `word`。

More TBD

## 删除&修改

`meow-kill` Kill当前的选择，无选择时为执行 <kbd>C-k</kbd> 的功能。（在不同场合表现不同，如在 paredit-mode 下，`C-k` 会被绑定为 `paredit-kill`）

`meow-delete` 删除当前选择（不进入 kill-ring），无选择时为执行 <kbd>C-d</kbd> 的功能。（不同场合表现不同）

`meow-change` 删除当前选择并进入插入模式。

`meow-save` 复制当前选择（不会进入系统剪贴板，如需复制到系统剪贴板，使用 `kill-ring-save`）

`meow-yank` 粘贴到当前光标前，可以使用 `negative-argument` 粘贴到当前光标后。（不会使用系统剪贴板，如需系统剪贴板，使用 `yank`）

`meow-replace` 用粘贴的方式替换掉当前的选择区域。

# 常用函数说明
TBD

# LICENSE

License under GPL v3.
