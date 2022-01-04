;;; meow-helpers.el --- Meow Helpers for define keybinding  -*- lexical-binding: t; -*-

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
;; Define custom keys in normal map with function `meow-normal-define-key'.
;; Define custom keys in global leader map with function `meow-leader-define-key'.
;; Define custom keys in leader map for specific mode with function `meow-leader-define-mode-key'.
;; Define a custom state with the macro `meow-define-state'
;;; Code:

(require 'cl-lib)

(require 'meow-util)
(require 'meow-var)
(require 'meow-keymap)

(defun meow-intern-string (name suffix &optional two-dashes prefix)
  "Convert a string into a meow symbol. Macro helper.
Concat the string PREFIX, either one or two hyphens based on TWO-DASHES,
the string NAME, and the string SUFFIX"
  (intern (concat (if prefix prefix "meow") (if two-dashes "--" "-")
                  name suffix)))

;; Macro to produce define-key function helpers
;; for each of the items in the alist meow-keymap-alist.
(defmacro meow-generate-define-key (mode keymap)
  (let ((mode-string (symbol-name mode))
        (keybind (gensym)))
    `(defun ,(meow-intern-string mode-string
                                 "-define-key") (&rest ,keybind)
       ;; Documentation string
       ,(concat "Define key for " mode-string " keymap \n\n"
                "Usage: \n"
                " (meow-" mode-string "-define-key) \n"
                "    '(\"@\" . hs-toggle-hiding))\n"
                "Optional argument ARGS key definitions")
       (mapcar (lambda (key-def)
                 (define-key ,keymap
                   (kbd (car key-def))
                   (meow--parse-def (cdr key-def))))
               ,keybind))))

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
  (mapcar (lambda (key-def)
            (define-key meow-normal-state-keymap
              (kbd (car key-def))
              (meow--parse-def (cdr key-def))))
          keybinds))

(defun meow-leader-define-key (&rest keybinds)
  "Define key in leader keymap with KEYBINDS.

Check `meow-normal-define-key' for usages."
  (mapcar (lambda (key-def)
            (define-key meow-leader-keymap
              (kbd (car key-def))
              (meow--parse-def (cdr key-def))))
          keybinds))

(defun meow-motion-overwrite-define-key (&rest keybinds)
  "Define key for MOTION state.

Check `meow-normal-define-key' for usages."
  (mapc (lambda (key-def)
          (define-key meow-motion-state-keymap
            (kbd (car key-def))
            (meow--parse-def (cdr key-def))))
        keybinds)
  (cl-loop for keybind in keybinds do
           (add-to-list 'meow--motion-overwrite-keys (car keybind))))

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

(defun meow--define-state-keymap (name keymap sparse suppress)
  "Generate a keymap called meow-NAME-state-keymap, or use KEYMAP.

If KEYMAP is non-nil, then set meow-NAME-state-keymap to KEYMAP and ignore
SPARSE and SUPPRESS.
When generating a keymap (if KEYMAP is nil),
If SPARSE is non-nil, then this is a sparse map.
If SUPPRESS is non-nil, then (suppress-keymap) is called on the map."
  `(defvar ,(meow-intern-string name "-state-keymap")
     ',(if keymap
          keymap
        (let ((map (if sparse (make-sparse-keymap) (make-keymap))))
          (if suppress (suppress-keymap map t) nil)
          map))))

(defun meow--define-state-init (name form)
  "Generate a state init function for a custom state.
The function is named meow--NAME-init. Run FORM on both init and exit."
  `(defun ,(meow-intern-string name "-init" t) ()
     ,(concat "Initialization function for meow-" name "-mode.\n"
              "Generated by meow-define-state-init.")
     
     (when ,(meow-intern-string name "-mode")
       (meow--disable-current-state)
       (setq-local meow--current-state ',(intern name))
       (meow-update-display))
     ,form))

(defun meow--define-state-minor-mode (name description lighter)
  "Generate a minor mode definition with name meow-NAME-mode,
DESCRIPTION and LIGHTER."
  `(define-minor-mode ,(meow-intern-string name "-mode")
     ,description
     :init-value nil
     :lighter ,lighter
     :keymap ,(meow-intern-string name "-state-keymap")
     (,(meow-intern-string name "-init" t))))

(defun meow--define-state-active-p (name)
  "Generate a predicate function to check if meow-NAME-mode is
currently active. Function is named meow-NAME-mode-p."
  `(defun ,(meow-intern-string name "-mode-p") ()
     ,(concat "Whether " name " mode is enabled.\n"
              "Generated by meow-define-state-active-p")
     (bound-and-true-p ,(meow-intern-string name "-mode"))))

(defun meow--define-state-cursor-type (name)
  "Generate a cursor type meow-cursor-type-NAME."
  `(defvar ,(meow-intern-string name nil nil "meow-cursor-type")
     meow-cursor-type-default))

(defun meow--define-state-cursor-function (name &optional face)
  `(,(meow-intern-string name "-mode-p") .
    (progn (meow--set-cursor-type
            ,(meow-intern-string name nil nil "meow-cursor-type"))
           (meow--set-cursor-color
            ',(if face face
                'meow-unknown-cursor)))))

(defun meow--define-state-cursor-function (name &optional face)
  `(defun ,(meow-intern-string name nil nil "meow--update-cursor") ()
     (meow--set-cursor-type ,(meow-intern-string name nil nil "meow-cursor-type"))
     (meow--set-cursor-color ',(if face face 'meow-unknown-cursor))))

(defun meow--register-state (name mode activep cursorf)
  "Register a custom state with symbol NAME and symbol MODE associated
with it.

Add (NAME . MODE) to `meow-state-mode-alist'.
Also update variable `meow-replace-state-name-list'."
  (add-to-list 'meow-state-mode-alist `(,name . ,mode))
  (add-to-list 'meow-replace-state-name-list
               `(,name . ,(upcase (symbol-name name))))
  (add-to-list 'meow-update-cursor-functions-alist
               `(,activep . ,cursorf)))

;;;###autoload
(defmacro meow-define-state (name
                             description
                             &rest body)
  "Define a custom meow state.

Define a custom meow state with name NAME, description DESCRIPTION,
lighter (modeline indicator) LIGHTER, and optionally face
FACE. Omitting FACE defines the face as meow-unknown-cursor.

This function produces several items:
1. meow-NAME-state-keymap: a keymap for the state. If SPARSE is non-nil, it is
a sparse keymap. If SUPPRESS is non-nil, (suppress-keymap) is called on the map.
This means that self-insert commands are overriden to be undefined. Finally,
if KEYMAP is non-nil, SPARSE and SUPPRESS options and ignored and the keymap given
is used.
2. meow--NAME-init: an init function for the state. Disables all other modes
upon entry
3. meow-NAME-mode: a minor mode for the state
4. meow-NAME-define-key: a helper function to define keys for the state.
See the documentation on meow-generate-define-key.
5. meow-NAME-mode-p: a predicate for whether the state is active.
6. meow-cursor-type-NAME: a variable for the cursor type for the state.
7. meow--update-cursor-NAME: a function that sets the cursor type to 6. and color FACE or
'meow-unknown cursor if FACE is nil."
  (let ((keymap   (plist-get body :keymap))
        (lighter  (plist-get body :lighter))
        (sparse   (plist-get body :sparse))
        (suppress (plist-get body :suppress))
        (face     (plist-get body :face))
        (initform    (unless (cl-evenp (length body))
                       (car (last body)))))
    `(progn
       ,(meow--define-state-active-p name)
       ,(meow--define-state-init name initform)
       ,(meow--define-state-minor-mode name description keymap lighter)
       ,(meow--define-state-cursor-type name)
       ,(meow--define-state-cursor-function name face)
       (meow--register-state ',(intern name) ',(meow-intern-string name "-mode")
                             ',(meow-intern-string name "-mode-p")
                             #',(meow-intern-string name nil nil
					                                "meow--update-cursor")))))


(provide 'meow-helpers)
;;; meow-helpers.el ends here
