# MEOW的KMACRO

## meow-kmacro-lines
这个指令可以对一个region内的所有行调用宏，这个功能和`apply-macro-to-region-lines`类似。主要的区别是：

- 支持选择的fallback机制
- undo boundary会被合并

## meow-kmacro-matches
这个命令在每一个匹配`(car regexp-search-ring)`的地方调用宏。

用`negative-argument`可以进行反向搜索

通常来说，我们可以设置通过`meow-mark-word`, `meow-mark-symbol`, `meow-visit`或者`isearch-forward-regexp`来设置`regexp-search-ring`。

我们的操作可能会包含递归的操作而造成死循环，这个时候可以通过<kbd>C-g<kbd>来打断。

# 为什么用KMACROS
虽然基本上所有的现代编辑器都支持多光标编辑，Emacs也有multiple-cursors和evil-mc的多光标支持，但是meow处于以下考虑，还是建议在需要多光标的时候使用kmacro。

## Kmacro是内置的
kmacro本身就是Emacs的内置功能

## 更好的兼容性
如果我们想把A，B，C这三次输入应用到3个地方，使用kmacro会得到ABCABCABC，而多光标则会得到AAABBBCCC。

我们会发现有的时候执行的顺序很重要，因为Emacs里面有非常多的变量。使用多光标的时候，我们就需要找出来哪些变量对每个光标而言是独立的。另外，我们还需要说明哪些操作需要对每一个光标做，哪些操作只需要执行一次。

## 记录的时候不会有卡顿
如果使用多光标，当光标的数量多的时候响应会非常慢。

比如我们使用smartparens来补全括号，每一个补全都需要1ms。如果有1000个光标，每插入一对括号就会卡顿1秒，这个实在让人生草。

而使用kmacro则像写一个程序，录制完毕之后调用并且等待就可以了。

## Kmacro可以复用
多光标场景下我们的操作不会被记录，所以我们无法复用我们的操作。

Kmacro就像一个程序，我们可以记录一次，调用多次。我们可以给宏明明，操作甚至拼接两个宏

# TIPS

## 基本使用
假设我们把`meow-line`绑定到`x`，把`meow-kmacro-lines`绑定到`X`，现在我们要把
```
foo
foo
foo
foo
foo
```
改成
```
the foo is not bar
the foo is not bar
the foo is not bar
the foo is not bar
the foo is not bar
```

我们需要做的是进入`normal mode`，然后把光标移动到第一个`foo`，并且开始记录键盘宏。Emacs内置的`C-x (`和`C-x )`分别绑定了`meow-start-kmacro`和`meow-end-or-call-kmacro`。这两个函数**只能**在`normal mode`下调用。

记录完成之后，我们把光标移动到第二个`foo`，然后按下`x 3 X`就可以完成文本的转换了。

`meow-kmacro-matches`的使用方式类似。

## 使用计数器
我们可以通过`kmacro-start-macro-or-insert-conuter`插入计数器。

我们可以通过`kmacro-add-counter`控制计数器如何计数。

我们可以通过`kmacro-set-format`控制计数器的格式。
