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

# 特色

## 1. 几乎没有默认的按键绑定
Meow 提供一套完整的模式编辑需要的命令，交由用户自己跟据喜好设置按键布局。这样听起来会不太方便，所以下面有一些推荐的针对不同键盘布局的按键方案，可以直接使用或做为自定义的起点。

### 针对 Qwerty 布局，Vim 类似的按键风格
TBD

### 针对 Dvorak Simplified 布局
TBD

### 针对 Dvorak Programmer 布局
TBD

### 针对 Colemak 布局
TBD

## 2. 和各类 Special mode 无痛的集成
虽然 Emacs 自身默认并不使用模式编辑，但各类非编辑型的 Special Mode 通常都直接以合理的单键做为命令。我更推荐于使用这些原本设定好的默认按键，而不是花费大量的精力来维持一套自定义的快捷键（以大量自定义快捷键的方式来匹配模式编辑成本非常高）。

对于各种 Special Mode，Meow 提供了 `MOTION` 模式，该模式下 <kbd>SPC</kbd> 做为 Leader，而原本绑定在 <kbd>SPC</kbd> 上的功能被绑定在 <kbd>SPC SPC</kbd> 。如果想使用 <kbd>j</kbd> 或 <kbd>k</kbd> 做为此时的上下方向，可以用类似的方式，即将原本在 <kbd>j</kbd> 和 <kbd>k</kbd> 的命令绑定在 <kbd>SPC j</kbd> 和 <kbd>SPC k</kbd> 上。

见后文中 `meow-motion-overwrite-define-key` 函数的说明。

## 3. 用类似 God Mode 的方式来执行 Emacs 中的组合快捷键
在 Meow 中，（默认） <kbd>SPC x</kbd> 将会触发 `KEYPAD` 模式，并将当前的输入转化成 `C-x`。后续的单键输入，将被自动翻译成带有 `Ctrl` 修饰的组合键，直到匹配到一个有效的命令，执行并退出 `KEYPAD` 模式。你还可以使用 <kbd>SPC c</kbd> 和 <kbd>SPC h</kbd> 进入 `KEYPAD` 模式（默认）。

在 `KEYPAD` 中如果要需要非 `C-` 前缀的输入则使用前缀：
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

## 4. 移动即是选择
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

# 完整的命令说明
TBD

# 函数说明
TBD

# LICENSE

License under GPL v3.
