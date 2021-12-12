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
;; Define custom keys for specific meow state and specific mode `meow-state-define-key'.

;;; Code:

(require 'cl-lib)

(require 'meow-util)
(require 'meow-var)
(require 'meow-keymap)

(defun meow-leader-define-key (&rest args)
  "Define key for Leader.

Usage:
  (meow-leader-define-key
   '(\"h\" . hs-toggle-hiding))
Optional argument ARGS key definitions."
  (mapcar (lambda (key-def)
            (define-key meow-leader-keymap
              (kbd (car key-def))
              (meow--parse-def (cdr key-def))))
          args))

(defun meow-normal-define-key (&rest args)
  "Define key for normal state.

Usage:
  (meow-normal-define-key
   '(\"@\" . hs-toggle-hiding))
Optional argument ARGS key definitions."
  (mapcar (lambda (key-def)
            (define-key meow-normal-state-keymap
              (kbd (car key-def))
              (meow--parse-def (cdr key-def))))
          args))

(defun meow-motion-overwrite-define-key (&rest args)
  "Define key for motion state."
  (mapc (lambda (key-def)
          (define-key meow-motion-state-keymap
            (kbd (car key-def))
            (meow--parse-def (cdr key-def))))
        args)
  (cl-loop for arg in args do
           (add-to-list 'meow--motion-overwrite-keys (car arg))))

(defun meow-state-define-key (keymaps states &rest args)
  "Define keys under KEYMAPS for STATES. KEYMAPS can either be a quoted keymap
or a list of quoted keymaps. STATES can either be a quoted meow state or a
list of quoted meow states.

Usage:
  (meow-state-define-key 'org-mode-map 'insert
    '(\"TAB\" . company-expand))
  (meow-state-define-key '(prog-mode-map org-mode-map) '(normal insert)
    '(\"Q\" . keyboard-quit))"
  (declare (indent 2))
  (dolist (state (if (listp states) states (list states)))
    (dolist (keymap-sym (if (listp keymaps) keymaps (list keymaps)))
      (let* ((keymap (symbol-value keymap-sym))
             (overlay-map (meow--get-overlay-map keymap state t)))
        (dolist (key-def args)
          (define-key overlay-map
            (kbd (car key-def))
            (meow--parse-def (cdr key-def))))))))

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

(provide 'meow-helpers)
;;; meow-helpers.el ends here
