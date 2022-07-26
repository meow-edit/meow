;;; meow-helpers.el --- Meow helpers for customization  -*- lexical-binding: t; -*-

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
;;
;; Define custom keys in a state with function `meow-define-keys'.
;; Define custom keys in normal map with function `meow-normal-define-key'.
;; Define custom keys in global leader map with function `meow-leader-define-key'.
;; Define custom keys in leader map for specific mode with function `meow-leader-define-mode-key'.
;; Define a custom state with the macro `meow-define-state'
;;; Code:

(require 'cl-lib)

(require 'meow-util)
(require 'meow-var)
(require 'meow-keymap)

(defun meow-intern (name suffix &optional two-dashes prefix)
  "Convert a string into a meow symbol. Macro helper.
Concat the string PREFIX or \"meow\" if PREFIX is null, either
one or two hyphens based on TWO-DASHES, the string NAME, and the
string SUFFIX. Then, convert this string into a symbol."
  (intern (concat (if prefix prefix "meow") (if two-dashes "--" "-")
                  name suffix)))

(defun meow-define-keys (state &rest keybinds)
  "Define KEYBINDS in STATE.

Example usage:
  (meow-define-keys
    ;; state
    'normal

    ;; bind to a command
    '(\"a\" . meow-append)

    ;; bind to a keymap
    (cons \"x\" ctl-x-map)

    ;; bind to a keybinding which holds a keymap
    '(\"c\" . \"C-c\")

    ;; bind to a keybinding which holds a command
    '(\"q\" . \"C-x C-q\"))"
  (declare (indent 1))
  (let ((map (alist-get state meow-keymap-alist)))
    (pcase-dolist (`(,key . ,def) keybinds)
      (define-key map (kbd key) (meow--parse-def def)))))

(defun meow-normal-define-key (&rest keybinds)
  "Define key for NORMAL state with KEYBINDS.

Example usage:
  (meow-normal-define-key
    ;; bind to a command
    '(\"a\" . meow-append)

    ;; bind to a keymap
    (cons \"x\" ctl-x-map)

    ;; bind to a keybinding which holds a keymap
    '(\"c\" . \"C-c\")

    ;; bind to a keybinding which holds a command
    '(\"q\" . \"C-x C-q\"))"
  (apply #'meow-define-keys 'normal keybinds))

(defun meow-leader-define-key (&rest keybinds)
  "Define key in leader keymap with KEYBINDS.

Meow use `mode-specific-map' as leader keymap.
Usually, the command on C-c <key> can be called in Meow via SPC <key>.

Thus, users should not add a dispatching keybinding like (\"<key>\" . \"C-c <key>\")
with this helper, it will result in recursive calls.

Check `meow-normal-define-key' for usages."
  (apply #'meow-define-keys 'leader keybinds))

;; Remap Leader SPC
(meow-leader-define-key (cons "SPC" (concat meow-motion-remap-prefix "SPC")))

(defun meow-motion-overwrite-define-key (&rest keybinds)
  "Define key for MOTION state.

Check `meow-normal-define-key' for usages."
  (apply #'meow-define-keys 'motion keybinds))

(defun meow-setup-line-number ()
  (add-hook 'display-line-numbers-mode-hook #'meow--toggle-relative-line-number)
  (add-hook 'meow-insert-mode-hook #'meow--toggle-relative-line-number))

(defun meow-setup-indicator ()
  "Setup indicator appending the return of function
`meow-indicator' to the modeline.

This function should be called after you setup other parts of the mode-line
 and will work well for most cases.

If this function is not enough for your requirements,
use `meow-indicator' to get the raw text for indicator
and put it anywhere you want."
  (unless (cl-find '(:eval (meow-indicator)) mode-line-format :test 'equal)
    (setq-default mode-line-format (append '((:eval (meow-indicator))) mode-line-format))))

(defun meow--define-state-minor-mode (name
                                      init-value
                                      description
                                      keymap
                                      lighter
                                      form)
  "Generate a minor mode definition with name meow-NAME-mode,
DESCRIPTION and LIGHTER."
  `(define-minor-mode ,(meow-intern name "-mode")
     ,description
     :init-value ,init-value
     :lighter ,lighter
     :keymap ,keymap
     (if (not ,(meow-intern name "-mode"))
	 (setq-local meow--current-state nil)
       (meow--disable-current-state)
       (setq-local meow--current-state ',(intern name))
       (meow-update-display))
     ,form))

(defun meow--define-state-active-p (name)
  "Generate a predicate function to check if meow-NAME-mode is
currently active. Function is named meow-NAME-mode-p."
  `(defun ,(meow-intern name "-mode-p") ()
     ,(concat "Whether " name " mode is enabled.\n"
              "Generated by meow-define-state-active-p")
     (bound-and-true-p ,(meow-intern name "-mode"))))

(defun meow--define-state-cursor-type (name)
  "Generate a cursor type meow-cursor-type-NAME."
  `(defvar ,(meow-intern name nil nil "meow-cursor-type")
     meow-cursor-type-default))

(defun meow--define-state-cursor-function (name &optional face)
  `(defun ,(meow-intern name nil nil "meow--update-cursor") ()
     (meow--set-cursor-type ,(meow-intern name nil nil "meow-cursor-type"))
     (meow--set-cursor-color ',(if face face 'meow-unknown-cursor))))

(defun meow-register-state (name mode activep cursorf &optional keymap)
  "Register a custom state with symbol NAME and symbol MODE
associated with it. ACTIVEP is a function that returns t if the
state is active, nil otherwise. CURSORF is a function that
updates the cursor when the state is entered. For help with
making a working CURSORF, check the variable
meow-update-cursor-functions-alist and the utility functions
meow--set-cursor-type and meow--set-cursor-color."
  (add-to-list 'meow-state-mode-alist `(,name . ,mode))
  (add-to-list 'meow-replace-state-name-list
               `(,name . ,(upcase (symbol-name name))))
  (add-to-list 'meow-update-cursor-functions-alist
               `(,activep . ,cursorf))
  (add-to-list 'meow-keymap-alist `(,name . ,keymap)))

;;;###autoload
(defmacro meow-define-state (name-sym
                             description
                             &rest body)
  "Define a custom meow state.

The state will be called NAME-SYM, and have description
DESCRIPTION. Following these two arguments, pairs of keywords and
values should be passed, similarly to define-minor-mode syntax.

Recognized keywords:
:keymap - the keymap to use for the state
:lighter - the text to display in the mode line while state is active
:face - custom cursor face
:form - one lisp form that will be run when the minor mode turns on AND off.
If you want to hook into only the turn-on event, check whether
(meow-NAME-SYM-mode) is true.

Example usage:
(meow-define-state mystate
  \"My meow state\"
  :lighter \" [M]\"
  :keymap 'my-keymap)

Also see meow-register-state, which is used internally by this
function, if you want more control over defining your state. This
is more helpful if you already have a keymap and defined minor
mode that you only need to integrate with meow.

This function produces several items:
1. meow-NAME-mode: a minor mode for the state. This is the main entry point.
2. meow-NAME-mode-p: a predicate for whether the state is active.
3. meow-cursor-type-NAME: a variable for the cursor type for the state.
4. meow--update-cursor-NAME: a function that sets the cursor type to 3.
 and face FACE or 'meow-unknown cursor if FACE is nil."
  (declare (indent 1))
  (let ((name       (symbol-name name-sym))
        (init-value (plist-get body :init-value))
        (keymap     (plist-get body :keymap))
        (lighter    (plist-get body :lighter))
        (face       (plist-get body :face))
        (form       (unless (cl-evenp (length body))
                    (car (last body)))))
    `(progn
       ,(meow--define-state-active-p name)
       ,(meow--define-state-minor-mode name init-value description keymap lighter form)
       ,(meow--define-state-cursor-type name)
       ,(meow--define-state-cursor-function name face)
       (meow-register-state ',(intern name) ',(meow-intern name "-mode")
                            ',(meow-intern name "-mode-p")
                            #',(meow-intern name nil nil
					    "meow--update-cursor")
			    ,keymap))))

(defun meow--is-self-insertp (cmd)
  (and (symbolp cmd)
       (string-match-p "\\`.*self-insert.*\\'"
                       (symbol-name cmd))))

(defun meow--mode-guess-state ()
  "Get initial state for current major mode.
If any of the keys a-z are bound to self insert, then we should
probably start in normal mode, otherwise we start in motion."
  (let ((state meow--current-state))
    (meow--disable-current-state)
    (let* ((letters (split-string "abcdefghijklmnopqrstuvwxyz" "" t))
           (bindings (mapcar #'key-binding letters))
           (any-self-insert (cl-some #'meow--is-self-insertp bindings)))
      (meow--switch-state state t)
      (if any-self-insert
          'normal
        'motion))))

(defun meow--mode-get-state (&optional mode)
  "Get initial state for MODE or current major mode if and only if
MODE is nil."
  (let* ((mode (if mode mode major-mode))
         (parent-mode (get mode 'derived-mode-parent))
         (state (alist-get mode meow-mode-state-list)))
    (cond
     (state state)
     (parent-mode (meow--mode-get-state parent-mode))
     (t (meow--mode-guess-state)))))

(provide 'meow-helpers)
;;; meow-helpers.el ends here
