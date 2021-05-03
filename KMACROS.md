# KMACRO IN MEOW

## meow-kmacro-lines

This command will call macro on each lines in region. It is similar to `apply-macro-to-region-lines`.
The differeces are:

- Support selection fallback behaviour
- The undo boundary will be collapsed

## meow-kmacro-matches

This command will call macro on each place that matchs `(car regexp-search-ring)`.

Use `negative-argument` for backward searching.

Typically, you can set `regexp-search-ring` via  `meow-mark-word`, `meow-mark-symbol`, `meow-visit` or `isearch-forward-regexp`.

Based on your operations, it's possible to have a recursive behaivour which cause the infinite loop, you can interrupt with <kbd>C-g</kbd>.

# WHY USE KMACROS

Even though every modern editors support more than one cursors(we call it multiple cursors), and there are multiple-cursors and evil-mc in Emacs world.
Meow still suggests using kmacro as a general solution for these cases.

## Kmacro is built-in
Kmacro is a built-in features in Emacs.

## Better compatibility
If you want to apply A, B, C to 3 places. Using kmacro is like doing ABCABCABC, using multiple cursors is like doing AAABBBCCC.

You may notice the execution order matters in some cases, because there are so many variables in Emacs.
When using multiple cursors, you have to figure out which variable should be treated as independent for each cursor.
Additionally, you have to teach it about which command should execute for every cursor and which command should execute only once.

## No lag when recording
With multiple cursors, it will be really slow if you have a large amount of cursors.

For example, if you use smartparens to complete pairs, and each completion need 1ms.
With 1000 cursors, it will freeze for 1 second after you insert a pair, the lag will drive you crazy.

However, using kmacro is like writing a small program. When you finish recording, you call it and wait.

## Kmacro is reusable
With multiple cursors, your operations are disposable, there's no reuse.

Kmacro is like a program, you record once, call multiple times. You can also give it a name, manipulate, or concat two macros.

# TIPS

## Use counter
You can insert a conuter via `kmacro-start-macro-or-insert-conuter`.

You can control how counter increase via `kmacro-add-counter`.

You can specify format for counter via `kmacro-set-format`.
