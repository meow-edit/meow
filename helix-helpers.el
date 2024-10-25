;;; helix-helpers.el --- Helix helpers for customization  -*- lexical-binding: t; -*-

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
;; Define custom keys in a state with function `helix-define-keys'.
;; Define custom keys in normal map with function `helix-normal-define-key'.
;; Define custom keys in global leader map with function `helix-leader-define-key'.
;; Define custom keys in leader map for specific mode with function `helix-leader-define-mode-key'.
;; Define a custom state with the macro `helix-define-state'
;;; Code:

(require 'cl-lib)

;; (require 'helix-util)
(require 'helix-vars)
;; (require 'helix-keymap)

(defun helix-intern (name suffix &optional two-dashes prefix)
  "Convert a string into a helix symbol. Macro helper.
Concat the string PREFIX or \"helix\" if PREFIX is null, either
one or two hyphens based on TWO-DASHES, the string NAME, and the
string SUFFIX. Then, convert this string into a symbol."
  (intern (concat (if prefix prefix "helix") (if two-dashes "--" "-")
                  name suffix)))

(defun helix--is-mode-p (mode)
  "Check if MODE is a valid major or minor mode."
  (and (functionp mode)))
       ;; (or (member mode minor-mode-list)
       ;;     (eq mode major-mode))))


(defmacro helix-define-key-for-mode (mode state key command)
  
  "For a given MODE and STATE, define a minor mode that is activated
   when both the MODE and STATE are active, and bind the given KEY to COMMAND
   in that mode's keymap."

  (let ((minor-mode-name (intern (concat (symbol-name (eval mode)) "-helix-" (symbol-name (eval state)) "-mode"))))
    `(progn
       (define-minor-mode ,minor-mode-name
         ,(format "Minor mode for %s and %s." "TODO" "TODO")
         :keymap (let ((map (make-sparse-keymap)))
                   (define-key map ,key ,command)
                   map))

       ;; Ensure the minor mode is available to be enabled
       ;; (add-hook (intern (concat (symbol-name mode) "-hook"))
       ;;           (lambda () (,minor-mode-name 1)))


       )

    ))



(defun helix-define-key (states scope key command)
  "Define key bindings for different STATES in the given SCOPE.
STATES should be a list of symbols representing states like 'normal or 'visual.
SCOPE should be either 'global or a specific keymap.normal
KEY is the key sequence to bind, and COMMAND is the function to call."

  (dolist (state (if (listp states) states (list states)))
    (let* ((map-symbol (intern (format "helix-%s-state-keymap" state)))
           (keymap (symbol-value map-symbol)))
      (define-key keymap key command))))


(defun helix-setup-line-number ()
  (add-hook 'display-line-numbers-mode-hook #'helix--toggle-relative-line-number)
  (add-hook 'helix-insert-mode-hook #'helix--toggle-relative-line-number))

(defun helix-setup-indicator ()
  "Setup indicator appending the return of function
`helix-indicator' to the modeline.

This function should be called after you setup other parts of the mode-line
 and will work well for most cases.

If this function is not enough for your requirements,
use `helix-indicator' to get the raw text for indicator
and put it anywhere you want."
  (unless (cl-find '(:eval (helix-indicator)) mode-line-format :test 'equal)
    (setq-default mode-line-format (append '((:eval (helix-indicator))) mode-line-format))))

(defun helix--define-state-minor-mode (name
                                      init-value
                                      description
                                      keymap
                                      lighter
                                      form)
  "Generate a minor mode definition with name helix-NAME-mode,
DESCRIPTION and LIGHTER."
  `(define-minor-mode ,(helix-intern name "-mode")
     ,description
     :init-value ,init-value
     :lighter ,lighter
     :keymap ,keymap
     (if (not ,(helix-intern name "-mode"))
	 (setq-local helix--current-state nil)
       (helix--disable-current-state)
       (setq-local helix--current-state ',(intern name))
       (helix-update-display))
     ,form))

(defun helix--define-state-active-p (name)
  "Generate a predicate function to check if helix-NAME-mode is
currently active. Function is named helix-NAME-mode-p."
  `(defun ,(helix-intern name "-mode-p") ()
     ,(concat "Whether " name " mode is enabled.\n"
              "Generated by helix-define-state-active-p")
     (bound-and-true-p ,(helix-intern name "-mode"))))

(defun helix--define-state-cursor-type (name)
  "Generate a cursor type helix-cursor-type-NAME."
  `(defvar ,(helix-intern name nil nil "helix-cursor-type")
     helix-cursor-type-default))

(defun helix--define-state-cursor-function (name &optional face)
  `(defun ,(helix-intern name nil nil "helix--update-cursor") ()
     (helix--set-cursor-type ,(helix-intern name nil nil "helix-cursor-type"))
     (helix--set-cursor-color ',(if face face 'helix-unknown-cursor))))

(defun helix-register-state (name mode activep cursorf &optional keymap)
  "Register a custom state with symbol NAME and symbol MODE
associated with it. ACTIVEP is a function that returns t if the
state is active, nil otherwise. CURSORF is a function that
updates the cursor when the state is entered. For help with
making a working CURSORF, check the variable
helix-update-cursor-functions-alist and the utility functions
helix--set-cursor-type and helix--set-cursor-color."
  (add-to-list 'helix-state-mode-alist `(,name . ,mode))
  (add-to-list 'helix-replace-state-name-list
               `(,name . ,(upcase (symbol-name name))))
  (add-to-list 'helix-update-cursor-functions-alist
               `(,activep . ,cursorf))
  ;; (add-to-list 'helix-keymap-alist `(,name . ,keymap))

  )

;;;###autoload
(defmacro helix-define-state (name-sym
                             description
                             &rest body)
  "Define a custom helix state.

The state will be called NAME-SYM, and have description
DESCRIPTION. Following these two arguments, pairs of keywords and
values should be passed, similarly to define-minor-mode syntax.

Recognized keywords:
:keymap - the keymap to use for the state
:lighter - the text to display in the mode line while state is active
:face - custom cursor face

The last argument is an optional lisp form that will be run when the minor
mode turns on AND off. If you want to hook into only the turn-on event,
check whether (helix-NAME-SYM-mode) is true.

Example usage:
(helix-define-state mystate
  \"My helix state\"
  :lighter \" [M]\"
  :keymap \\='my-keymap
  (message \"toggled state\"))

Also see helix-register-state, which is used internally by this
function, if you want more control over defining your state. This
is more helpful if you already have a keymap and defined minor
mode that you only need to integrate with helix.

This function produces several items:
1. helix-NAME-mode: a minor mode for the state. This is the main entry point.
2. helix-NAME-mode-p: a predicate for whether the state is active.
3. helix-cursor-type-NAME: a variable for the cursor type for the state.
4. helix--update-cursor-NAME: a function that sets the cursor type to 3.
 and face FACE or \\='helix-unknown cursor if FACE is nil."
  (declare (indent 1))
  (let* ((name       (symbol-name name-sym))
        (init-value (plist-get body :init-value))
        (user-keymap     (plist-get body :keymap))
	(keymap-name (intern (concat "helix-" name "-state-keymap")))
	(keymap (or user-keymap keymap-name))  ; Use provided keymap or create new one
         
        (lighter    (plist-get body :lighter))
        (face       (plist-get body :face))
        (form       (unless (cl-evenp (length body))
                    (car (last body)))))
    `(progn

       ;; Define the keymap if not provided by the user
       (defvar ,keymap-name (make-sparse-keymap)
         ,(concat "Keymap for helix state " name "."))
       
       ,(helix--define-state-active-p name)
       ,(helix--define-state-minor-mode name init-value description keymap lighter form)
       ,(helix--define-state-cursor-type name)
       ,(helix--define-state-cursor-function name face)
       (helix-register-state ',(intern name) ',(helix-intern name "-mode")
                            ',(helix-intern name "-mode-p")
                            #',(helix-intern name nil nil
					    "helix--update-cursor")
			    ,keymap))))

(defun helix--is-self-insertp (cmd)
  (and (symbolp cmd)
       (string-match-p "\\`.*self-insert.*\\'"
                       (symbol-name cmd))))

(defun helix--mode-guess-state ()
  "Get initial state for current major mode.
If any of the keys a-z are bound to self insert, then we should
probably start in normal mode, otherwise we start in motion."
  (let ((state helix--current-state))
    (helix--disable-current-state)
    (let* ((letters (split-string "abcdefghijklmnopqrstuvwxyz" "" t))
           (bindings (mapcar #'key-binding letters))
           (any-self-insert (cl-some #'helix--is-self-insertp bindings)))
      (helix--switch-state state t)
      (if any-self-insert
          'normal
        'motion))))

(defun helix--mode-get-state (&optional mode)
  "Get initial state for MODE or current major mode if and only if
MODE is nil."
  (let* ((mode (if mode mode major-mode))
         (parent-mode (get mode 'derived-mode-parent))
         (state (alist-get mode helix-mode-state-list)))
    (cond
     (state state)
     (parent-mode (helix--mode-get-state parent-mode))
     (t (helix--mode-guess-state)))))

(provide 'helix-helpers)
;;; helix-helpers.el ends here
