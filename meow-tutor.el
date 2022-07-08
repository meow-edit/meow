;;; meow-tutor.el --- Tutor for Meow  -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; A tutorial for Meow.
;;
;; To start, with M-x meow-tutor

;;; Code:

(require 'meow-var)

(defconst meow--tutor-content
  "
             ███╗░░░███╗███████╗░█████╗░░██╗░░░░░░░██╗
             ████╗░████║██╔════╝██╔══██╗░██║░░██╗░░██║
             ██╔████╔██║█████╗░░██║░░██║░╚██╗████╗██╔╝
             ██║╚██╔╝██║██╔══╝░░██║░░██║░░████╔═████║░
             ██║░╚═╝░██║███████╗╚█████╔╝░░╚██╔╝░╚██╔╝░
             ╚═╝░░░░░╚═╝╚══════╝░╚════╝░░░░╚═╝░░░╚═╝░░

==================================================================
=                      MEOW INTRODUCTION                         =
==================================================================

 Meow is yet another modal editing mode for Emacs.
 What's modal editing? How do I use Meow? Let's start our journey!

 If you wonder what a keystroke means when reading this, just ask
 Emacs! Press C-h k then press the key you want to query.

==================================================================
=                     BASIC CURSOR MOVEMENT                      =
==================================================================

  To move up, press \\[meow-prev]
  To move down, press \\[meow-next]
  To move left, press \\[meow-left]
  To move right, press \\[meow-right]
       ↑
       \\[meow-prev]
   ← \\[meow-left]   \\[meow-right] →
       \\[meow-next]
       ↓

 You can move the cursor using the \\[meow-left], \\[meow-next], \\[meow-prev], \\[meow-right] keys, as shown
 above. Arrow keys also work, but it is faster to use the \\[meow-left]\\[meow-next]\\[meow-prev]\\[meow-right]
 keys as they are closer to the other keys you will be using.
 Try moving around to get a feel for \\[meow-left]\\[meow-next]\\[meow-prev]\\[meow-right].
 Once you're ready, hold \\[meow-next] to continue to the next lesson.

 Meow provides modal editing which means you have different
 modes for inserting and editing text. The primary modes you will
 use are Normal mode and Insert mode. While in Normal mode, the
 keys you press won't actually type text. Instead, they will
 perform various actions with the text. This allows for more
 efficient editing. This tutor will teach you how you can make
 use of Meow's modal editing features. To begin, ensure your
 caps-lock key is not pressed and hold the \\[meow-next] key until you reach
 the first lesson.

=================================================================
=                           DELETION                            =
=================================================================

 Pressing the \\[meow-delete] key deletes the character under the cursor.
 \\[meow-backward-delete] key deletes the character before the cursor (backspace).

 1. Move the cursor to the line below marked -->.
 2. Move the cursor to each extra character, and press \\[meow-delete] to
    delete it.

 --> Thhiss senttencee haass exxtra charracterss.
     This sentence has extra characters.

 Once both sentences are identical, move to the next lesson.

=================================================================
=                          INSERT MODE                          =
=================================================================

 Pressing the \\[meow-insert] key enters the Insert mode. In that mode you can
 enter text. <ESC> returns you back to Normal mode. The modeline
 will display your current mode. When you press \\[meow-insert], '%s'
 changes to '%s'.

 1. Move the cursor to the line below marked -->.
 2. Insert the missing characters with \\[meow-insert] key.
 3. Press <ESC> to return back to Normal mode.
 4. Repeat until the line matches the line below it.

 --> Th stce misg so.
     This sentence is missing some text.

 Note: If you want to move the cursor while in Insert mode, you
       can use the arrow keys instead of exiting and re-entering
       Insert mode.

=================================================================
=                      MORE ON INSERT MODE                      =
=================================================================

 Pressing \\[meow-insert] is not the only way to enter Insert Mode. Here are
 some other ways to enter Insert mode at different locations.

 Common examples of insertion commands include:

   \\[meow-insert]   - Insert cursor before the selection.
   \\[meow-append]   - Insert cursor after the selection.
   \\[meow-open-above]   - Insert new line above the current line.
   \\[meow-open-below]   - Insert new line below the current line.
   \\[meow-join] \\[meow-append] - Insert cursor at the start of the line.
   \\[meow-line] \\[meow-append] - Insert cursor at the end of the line.

 These commands are composable. \\[meow-join] will select the beginning of the
 current line up until the end of the non-empty line above.
 \\[meow-append] switches to Insert mode at the end of current selection.
 Using both commands together will result in the cursor position being at
 the beginning of the line (Insert mode). \\[meow-line] selects the whole
 line and enables the use of the same insertion commands.

 1. Move to anywhere in the line below marked -->.
 2. Press \\[meow-line] \\[meow-append], your cursor will move to the end of the line
    and you will be able to type.
 3. Type the necessary text to match the line below.
 4. Press \\[meow-join] \\[meow-append] for the cursor to move to the beginning of the line.
    This will place the cursor before -->. For now just return to
    Normal mode and move cursor past it.

 -->  sentence is miss
     This sentence is missing some text.

=================================================================
=                             RECAP                             =
=================================================================

 + Use the \\[meow-left], \\[meow-next], \\[meow-prev], \\[meow-right] keys to move the cursor.

 + Press \\[meow-delete] to delete the character under the cursor.

 + Press \\[meow-backward-delete] to delete the character before the cursor.

 + Press \\[meow-insert] to enter Insert mode to input text. Press <ESC> to
   return to Normal mode.

 + Press \\[meow-join] to select the start of the current line and
   the non-empty line above.

 + Press \\[meow-append] to enter Insert mode, with the cursor position being
   at the end of the selected region.

=================================================================
=                    MOTIONS AND SELECTIONS                     =
=================================================================

 Pressing \\[meow-next-word] will select everything from the cursor position
 until the end of the current word.
 Numbers that show up on the screen indicate a quick way to extend your selection.
 You can unselect the region with the \\[meow-cancel-selection] key.

 Pressing \\[meow-kill] will delete the current selection.

 The \\[meow-delete] key deletes the character below the cursor, while
 \\[meow-kill] deletes all of the selected text.

 1. Move the cursor to the line below marked -->.
 2. Move to the beginning of a word that needs to be deleted.
 3. Press \\[meow-next-word] to select a word.
 4. Press \\[meow-kill] to delete the selection.
 5. Repeat for all extra words in the line.

 --> This sentence pencil has vacuum extra words in the it.
     This sentence has vacuum words in it.

 Note: Pressing \\[meow-kill] without a selection will delete everything
       from cursor position until the end of line.

=================================================================
=                       WORDS VS SYMBOLS                        =
=================================================================

 Pressing \\[meow-mark-word] will select the whole word under the cursor. \\[meow-mark-symbol] will
 select the whole symbol. Symbols are separated only by whitespace,
 whereas words can also be separated by other characters.

 To understand the difference better, do the following exercise:

 1. Move the cursor to the line below marked -->.
 2. Use \\[meow-mark-word] and \\[meow-mark-symbol] on each word in a sentence.
 3. Observe the difference in selection.

 --> Select-this and this.

=================================================================
=                    EXTENDING SELECTION                        =
=================================================================

 Motions are useful for extending the current selection and for
 quick movement around the text. After selecting the word under
 the cursor with \\[meow-mark-word] you can extend the selection with some common
 movements listed below.

   \\[meow-next-word] - Moves forward to the end of the current word.
   \\[meow-back-word] - Moves backward to the beginning of the current word.
   \\[meow-next-symbol] - Moves to the end of the current symbol.
   \\[meow-back-symbol] - Moves to the start of the current symbol.

 Cursor position can be reversed with \\[meow-reverse] to extend the selection
 the other directions. In-case too much gets selected, you can
 undo the previous selection with \\[meow-pop-selection] key.

 1. Move the cursor to the line below marked -->.
 2. Select the word with \\[meow-mark-word].
 3. Extend the selection with \\[meow-next-word].
 4. Press \\[meow-kill] to delete the selection.
 5. (Optional) Try reversing the cursor and extending the selection.

 --> This sentence is most definitelly not at all short.
     This sentence is short.

=================================================================
=                        SELECTING LINES                        =
=================================================================

 Pressing \\[meow-line] will select the whole line. Pressing it again will
 add the next line to the selection. Numbers can also be used
 to select multiple lines at once.

 1. Move the cursor to the second line below marked -->.
 2. Press \\[meow-line] to select the current line, and \\[meow-kill] to delete it.
 3. Move to the fourth line.
 4. Select 2 lines either by hitting \\[meow-line] twice or \\[meow-line] 1 in combination.
 5. Delete the selection with \\[meow-kill].

 --> 1) Roses are red,
 --> 2) Mud is fun,
 --> 3) Violets are blue,
 --> 4) I have a car,
 --> 5) Clocks tell time,
 --> 6) Sugar is sweet,
 --> 7) And so are you.

=================================================================
=                 EXTENDING SELECTION BY OBJECT                 =
=================================================================

 Expanding the selected region is easy. In fact every motion
 command has its own expand type. Motions can be expanded in
 different directions and units.

 Common selection expanding motions by a THING:

   \\[meow-beginning-of-thing] - expand before cursor until beginning of...
   \\[meow-end-of-thing] - expand after cursor until end of...
   \\[meow-inner-of-thing] - select the inner part of...
   \\[meow-bounds-of-thing] - select the whole part of...

 Some of THING modifiers may include:

  r - round parenthesis
  s - square parenthesis
  c - curly parenthesis
  g - string
  p - paragraph
  l - line
  d - defun
  b - buffer

 1. Move the cursor to the paragraph below.
 2. Type \\[meow-bounds-of-thing] p to select the whole paragraph.
 3. Type \\[meow-cancel-selection] to cancel the selection.
 4. Type \\[meow-inner-of-thing] l to select one line.
 5. Type \\[meow-cancel-selection] to cancel the selection.
 6. Play with the commands you learned this section. You can do anything
    you want with these powerful commands!

 War and Peace by Leo Tolstoy, is considered one of the greatest works of
 fiction.It is regarded, along with Anna Karenina (1873–1877), as Tolstoy's
 finest literary achievement. Epic in scale, War and Peace delineates in graphic
 detail events leading up to Napoleon's invasion of Russia, and the impact of the
 Napoleonic era on Tsarist society, as seen through the eyes of five Russian
 aristocratic families.Newsweek in 2009 ranked it top of its list of Top 100
 Books.Tolstoy himself, somewhat enigmatically, said of War and Peace that it was
 \"not a novel, even less is it a poem, and still less an historical chronicle.\"

=================================================================
=                      MOVE AROUND THINGs                       =
=================================================================

 You can also move around things. In fact, Meow combines move and
 selection together. Everytime you select something, the curosr
 will move to the beginning/end/inner/bound of things depending
 on your commands. Let's practise!

 * How to jump to the beginning of buffer quickly?

   Type \\[meow-beginning-of-thing] and \"b\". Remember to come
   back by typing \\[meow-pop-selection].

 * How to jump to the end of buffer quickly?

   I believe you could figure it out. Do it!

 * How to jump to the end of the current function quickly?

   1. Move cursor to the function below marked -->.
   2. Type \\[meow-bounds-of-thing] and \"c\", then \"a\".

   -->
   fn count_ones(mut n: i64) -> usize {
    let mut count: usize = 0;
    while 0 < n {
        count += (1 & n) as usize;
        n >>= 1;
    }
    count
   }

 Note that Meow need the major mode for the programming language
 to find functions correctly. Then if you type \\[meow-bounds-of-thing] and \"d\" to
 select the whole function here, it won't work. Go to your
 favorite programming language mode and practise!

=================================================================
=                   THE FIND/TILL COMMAND                       =
=================================================================

 Type \\[meow-till] to select until the next specific character.

 1. Move the cursor to the line below marked -->.
 2. Press \\[meow-till]. A prompt will appear in minibuffer.
 4. Type 'a'. The correct position for the next 'a' will be
    selected.

 --> I like to eat apples since my favorite fruit is apples.

 Note: If you want go backward, use \\[negative-argument] as a prefix, there is also
       a similar command on \\[meow-find], which will jump over that
       character.

=================================================================
=                            RECAP                              =
=================================================================

 + Unselect region with \\[meow-cancel-selection] key.

 + Reverse cursor position in selected region with \\[meow-reverse] key.

 + Undo selection with \\[meow-pop-selection].

 + Press \\[meow-next-word] to select until the end of current word.

 + Press \\[meow-back-word] to select until the start of closest word.

 + Press \\[meow-next-symbol] to select until the end of symbol.

 + Press \\[meow-back-symbol] to select until the start of symbol.

 + Press \\[meow-line] to select the entire current line. Type \\[meow-line] again to
   select the next line.

 + Motion can be repeated multiple times by using a number modifier.

 + Extend selection by using THING modifiers
   Motion Prefix: (\\[meow-beginning-of-thing] \\[meow-end-of-thing] \\[meow-inner-of-thing] \\[meow-bounds-of-thing])
   THING as a Suffix: (r,s,c,g,p,l,d,b)

 + Find by a single character with \\[meow-till] and \\[meow-find].

=================================================================
=                      THE CHANGE COMMAND                       =
=================================================================

 Pressing \\[meow-change] will delete the current selection and switch to
 Insert mode. If there is no selection it will only delete
 the character under the cursor and switch to Insert mode.
 It is a shorthand for \\[meow-delete] \\[meow-insert].

 1. Move the cursor to the line below marked -->.
 2. Select the incorrect word with \\[meow-next-word].
 3. Press \\[meow-change] to delete the word and enter Insert mode.
 4. Replace it with correct word and return to Normal mode.
 5. Repeat until the line matches the line below it.

 --> This paper has heavy words behind it.
     This sentence has incorrect words in it.

=================================================================
=                         KILL AND YANK                         =
=================================================================

 The \\[meow-kill] key also copies the deleted content which can then be
 pasted with \\[meow-yank].

 1. Move the cursor to the line below marked -->.
 2. Type \\[meow-line] to select the line.
 3. Type \\[meow-kill] to cut the current selection.
 4. Type \\[meow-yank] to paste the copied content.
 5. You can paste as many times as you want.

 --> Violets are blue, and I love you.

=================================================================
=                         SAVE AND YANK                         =
=================================================================

 Pressing \\[meow-save] copies the selection, which can then be pasted
 with \\[meow-yank] under the cursor.

 1. Move the cursor to the line below marked -->.
 2. Press \\[meow-line] to select one line forward.
 3. Press \\[meow-save] to copy the current selection.
 4. Press \\[meow-yank] to paste the copied content.
 5. You can paste as many times as you want.

 --> Violets are blue, and I love you.

=================================================================
=                            UNDOING                            =
=================================================================

 Pressing \\[meow-undo] triggers undo. The \\[meow-undo-in-selection] key will only undo the changes
 in the selected region.

 1. Move the cursor to the line below marked -->.
 2. Move to the first error, and press \\[meow-delete] to delete it.
 3. Type \\[meow-undo] to undo your deletion.
 4. Fix all the errors on the line.
 5. Type u several times to undo your fixes.

 --> Fiix the errors on thhis line and reeplace them witth undo.
     Fix the errors on this line and replace them with undo.

=================================================================
=                             RECAP                             =
=================================================================

 + Press \\[meow-change] to delete the selection and enter Insert mode.

 + Press \\[meow-save] to copy the selection.

 + Press \\[meow-yank] to paste the copied or deleted text.

 + Press \\[meow-undo] to undo last change.

 + Press \\[meow-undo-in-selection] to only undo changes in the selected region.

=================================================================
=               BEACON (BATCHED KEYBOARD MACROS)                =
=================================================================

 Keyboard macro is a function that is built-in to Emacs. Now with Meow, it's
 more powerful. We can do things like multi-editing with Beacon
 mode in Meow.

 Select a region, then press \\[meow-grab] to \"grab\" it, then enter
 Insert mode, meow will now enter Beacon mode. Meow will create multiple
 cursors and all edits you do to one cursor will be synced to other
 cursors after you exit Insert mode. Type \\[meow-grab] again to cancel
 grabbing.

 1. Move the cursor to the first line below marked -->.
 2. Select the six lines.
 3. Type \\[meow-grab] to grab the selection. Edits you
    make will be synced to the other cursors.
 4. Use Insert mode to correct the lines. Then exit Insert mode.
    Other cursors will fix the other lines after you exit Insert mode.
 5. Type \\[meow-grab] to cancel the grabbing.

 --> Fix th six nes at same ime.
 --> Fix th six nes at same ime.
 --> Fix th six nes at same ime.
 --> Fix th six nes at same ime.
 --> Fix th six nes at same ime.
 --> Fix th six nes at same ime.
     Fix these six lines at the same time.

=================================================================
=                         MORE ON BEACON                        =
=================================================================

 BEACON is powerful! Let's do some more practice.

 Ex. A. How to achieve this?
        1 2 3
        =>
        [| \"1\" |] [| \"2\" |] [| \"3\" |]

 1. Move the cursor to the line below marked -->
 2. Select the whole line (you know how to do this)
 3. Press \\[meow-grab] to grab the selection
 4. Press \\[meow-back-word] to create fake cursors at the beginning of each word
    in the backwards direction.
 5. Enter Insert Mode then edit.
 6. Press \\[meow-normal-mode] to stop macro recording and apply
    your edits to all fake cursors.
 7. Press \\[meow-grab] to cancel grab.
 --> 1 2 3
     [| \"1\" |] [| \"2\" |] [| \"3\" |]

 Ex. B. How to achieve this?
        x-y-foo-bar-baz
        =>
        x_y_foo_bar_baz

 1. Move the cursor to the line below marked -->
 2. Select the whole symbol with \\[meow-mark-symbol]
 3. Press \\[meow-grab] to activate secondary selection
 4. Press \\[negative-argument] \\[meow-find] and - to backward search for
    character -, will create fake cursor at each -
 5. Meow will start recording. Press \\[meow-change] to switch to Insert mode
    (character under current cursor is deleted)
 6. type _
 7. Press ESC to go back to NORMAL, then the macro will
    be applied to all fake cursors.
 8. Press \\[meow-grab] again to cancel the grab

 --> x-y-foo-bar-baz
     x_y_foo_bar_baz

=================================================================
=                     QUICK VISIT AND SEARCH                    =
=================================================================

 The visit command \\[meow-visit] can help to select a symbol in your
 buffer with completion. Once you have something selected with the \\[meow-visit] key,
 you can use \\[meow-search] to search for the next occurance of that selection.

 If you want a backword search, you can reverse the selection with \\[meow-reverse]
 because \\[meow-search] will respect the direction of the current selection.

 1. Move the cursor to the line below marked -->.
 2. Select the word \"dog\" with \\[meow-visit] dog RET.
 3. Change it to \"cat\" with \\[meow-change] cat ESC.
 4. Save it with \\[meow-save].
 5. Search for next \"dog\" and replace it with \\[meow-search] \\[meow-replace].
 6. Repeat 5 to replace next \"dog\".

 --> I'm going to tell you something:
     dog is beautiful
     and dog is agile
     the last one, dog says meow

 Note: You can also start searching after \\[meow-mark-word] or \\[meow-mark-symbol]. Actually, you
       can use \\[meow-search] whenever you have any kind of selection. The search command
       is built on regular expression. The symbol boundary will be
       added to your search if the selection is created with \\[meow-visit], \\[meow-mark-word] and \\[meow-mark-symbol].

=================================================================
=                    KEYPAD AND MOTION MODE                     =
=================================================================

 One of the most notable features of Meow is the Keypad. It
 enables the use of modifier keybinds without pressing modifiers.

 To enter Keypad mode, press SPC in Normal mode or Motion mode.

 Once Keypad is started, your single key input, will be translated
 based on following rules:

 1. The first letter input, except x, c, h, m, g will be
 translated to C-c <key>.
 
 Example: a => C-c a

 Press SPC a, call the command on C-c a, which is
 undefined by default.

 2. m will be translated to M-, means next input should be
 modified with Meta.

 Example: m h => M-h

 Press SPC m h, call the command on M-h, which is
 mark-paragraph by default.

 3. g will be translated to C-M-, means next input should be
 modified with both Control and Meta.

 Example: g l => C-M-l

 SPC g l, call the command on C-M-l, which is
 reposition-window by default.

 4. A SPC in the middle represent the literal prefix, means
 next input should not be modified.

 Example: m g SPC g => M-g g

 Press SPC m g SPC g, call the command on M-g g, which is
 goto-line by default.

 Sometimes, you can omit this SPC when there's no ambiguity.

 5. For any other cases, the input key will be translated to 
 C-<key>.

 Example: x f => C-x C-f

 Press SPC x f, call the command on C-x C-f, which is
 find-file by default.

 After one execution, no matter succeed or failed, Keypad will
 quit automatically, and the previous mode will be enabled.

 To revoke one input, press BACKSPACE. To cancel and exit Keypad
 immediately, press ESC or C-g.

=================================================================
=                     MEOW CHEAT SHEET                          =
=================================================================

 All these keybinds are shown on the cheat sheet which can be
 opened by pressing \\[meow-cheatsheet].

=================================================================
")

(defun meow-tutor ()
  "Open a buffer with meow tutor."
  (interactive)
  (let ((buf (get-buffer-create "*Meow Tutor*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format (substitute-command-keys meow--tutor-content)
                      (alist-get 'normal meow-replace-state-name-list)
                      (alist-get 'insert meow-replace-state-name-list)))
      (setq-local scroll-conservatively 1)
      (setq-local scroll-margin 3)
      (setq-local scroll-step 1)
      (goto-char (point-min))
      (display-line-numbers-mode))
    (switch-to-buffer buf)))

(provide 'meow-tutor)
;;; meow-tutor.el ends here
