;;; meow-shims.el --- Make Meow play well with other packages.  -*- lexical-binding: t; -*-

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
;; The file contains all the shim code we need to make meow
;; work with other packages.

;;; Code:

(require 'meow-var)
(require 'meow-command)
(require 'delsel)

(declare-function meow-normal-mode "meow")
(declare-function meow-motion-mode "meow")
(declare-function meow-insert-exit "meow-command")

(defun meow--switch-to-motion (&rest _ignore)
  "Switch to motion state, used for advice.
Optional argument IGNORE ignored."
  (meow--switch-state 'motion))

(defun meow--switch-to-normal (&rest _ignore)
  "Switch to normal state, used for advice.
Optional argument IGNORE ignored."
  (meow--switch-state 'normal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; undo-tree

(defvar undo-tree-enable-undo-in-region)

(defun meow--setup-undo-tree (enable)
  "Setup `undo-tree-enable-undo-in-region' for undo-tree.

Command `meow-undo-in-selection' will call undo-tree undo.

Argument ENABLE non-nill means turn on."
  (when enable (setq undo-tree-enable-undo-in-region t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eldoc

(defvar meow--eldoc-setup nil
  "Whether already setup eldoc.")

(defconst meow--eldoc-commands
  '(meow-head
    meow-tail
    meow-left
    meow-right
    meow-prev
    meow-next
    meow-insert
    meow-append)
  "A list of meow commands that trigger eldoc.")

(defun meow--setup-eldoc (enable)
  "Setup commands that trigger eldoc.

Basically, all navigation commands should trigger eldoc.
Argument ENABLE non-nill means turn on."
  (setq meow--eldoc-setup enable)
  (if enable
      (apply #'eldoc-add-command meow--eldoc-commands)
    (apply #'eldoc-remove-command meow--eldoc-commands)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company

(defvar meow--company-setup nil
  "Whether already setup company.")

(declare-function company--active-p "company")
(declare-function company-abort "company")

(defvar company-candidates)

(defun meow--company-maybe-abort-advice ()
  "Adviced for `meow-insert-exit'."
  (when company-candidates
    (company-abort)))

(defun meow--setup-company (enable)
  "Setup for company.
Argument ENABLE non-nil means turn on."
  (setq meow--company-setup enable)
  (if enable
      (add-hook 'meow-insert-exit-hook #'meow--company-maybe-abort-advice)
    (remove-hook 'meow-insert-exit-hook #'meow--company-maybe-abort-advice)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; repeat-map

(defvar meow--diff-hl-setup nil
  "Whether already setup diff-hl.")

(defun meow--setup-diff-hl (enable)
  "Setup diff-hl."
  (if enable
      (progn
        (advice-add 'diff-hl-show-hunk-inline-popup :before 'meow--switch-to-motion)
        (advice-add 'diff-hl-show-hunk-posframe :before 'meow--switch-to-motion)
        (advice-add 'diff-hl-show-hunk-hide :after 'meow--switch-to-normal))
    (advice-remove diff-hl-show-hunk-inline-popup 'meow--switch-to-motion)
    (advice-remove diff-hl-show-hunk-posframe 'meow--switch-to-motion)
    (advice-remove diff-hl-show-hunk-hide 'meow--switch-to-normal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wgrep

(defvar meow--wgrep-setup nil
  "Whether already setup wgrep.")

(defun meow--setup-wgrep (enable)
  "Setup wgrep.

We use advice here because wgrep doesn't call its hooks.
Argument ENABLE non-nil means turn on."
  (setq meow--wgrep-setup enable)
  (if enable
      (progn
        (advice-add 'wgrep-change-to-wgrep-mode :after #'meow--switch-to-normal)
        (advice-add 'wgrep-exit :after #'meow--switch-to-motion)
        (advice-add 'wgrep-finish-edit :after #'meow--switch-to-motion)
        (advice-add 'wgrep-save-all-buffers :after #'meow--switch-to-motion))
    (advice-remove 'wgrep-change-to-wgrep-mode #'meow--switch-to-normal)
    (advice-remove 'wgrep-exit #'meow--switch-to-motion)
    (advice-remove 'wgrep-finish-edit #'meow--switch-to-motion)
    (advice-remove 'wgrep-save-all-buffers #'meow--switch-to-motion)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wdired

(defvar meow--wdired-setup nil
  "Whether already setup wdired.")

(defvar wdired-mode-hook)

(declare-function wdired-exit "wgrep")
(declare-function wdired-finish-edit "wgrep")
(declare-function wdired-abort-changes "wgrep")

(defun meow--setup-wdired (enable)
  "Setup wdired.

Argument ENABLE non-nil means turn on."
  (setq meow--wdired-setup enable)
  (if enable
      (progn
        (add-hook 'wdired-mode-hook #'meow--switch-to-normal)
        (advice-add #'wdired-exit :after #'meow--switch-to-motion)
        (advice-add #'wdired-abort-changes :after #'meow--switch-to-motion)
        (advice-add #'wdired-finish-edit :after #'meow--switch-to-motion))
    (remove-hook 'wdired-mode-hook #'meow--switch-to-normal)
    (advice-remove #'wdired-exit #'meow--switch-to-motion)
    (advice-remove #'wdired-abort-changes #'meow--switch-to-motion)
    (advice-remove #'wdired-finish-edit #'meow--switch-to-motion)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rectangle-mark-mode

(defvar meow--rectangle-mark-setup nil
  "Whether already setup rectangle-mark.")

(defun meow--rectangle-mark-init ()
  "Patch the meow selection type to prevent it from being cancelled."
  (when (bound-and-true-p rectangle-mark-mode)
    (setq meow--selection
          '((expand . char) 0 0))))

(defun meow--setup-rectangle-mark (enable)
  "Setup `rectangle-mark-mode'.
Argument ENABLE non-nil means turn on."
  (setq meow--rectangle-mark-setup enable)
  (if enable
      (add-hook 'rectangle-mark-mode-hook 'meow--rectangle-mark-init)
    (remove-hook 'rectangle-mark-mode-hook 'meow--rectangle-mark-init)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; edebug

(defvar meow--edebug-setup nil)

(defun meow--edebug-hook-function ()
  "Switch meow state when entering/leaving edebug."
  (if (bound-and-true-p edebug-mode)
      (meow--switch-to-motion)
    (meow--switch-to-normal)))

(defun meow--setup-edebug (enable)
  "Setup edebug.
Argument ENABLE non-nil means turn on."
  (setq meow--edebug-setup enable)
  (if enable
      (add-hook 'edebug-mode-hook 'meow--edebug-hook-function)
    (remove-hook 'edebug-mode-hook 'meow--edebug-hook-function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cider (debug)

(defvar meow--cider-setup nil)

(defun meow--cider-debug-hook-function ()
  "Switch meow state when entering/leaving cider debug."
  (if (bound-and-true-p cider--debug-mode)
      (meow--switch-to-motion)
    (meow--switch-to-normal)))

(defun meow--setup-cider (enable)
  "Setup cider.
Argument ENABLE non-nil means turn on."
  (setq meow--cider-setup enable)
  (if enable
      (add-hook 'cider--debug-mode-hook 'meow--cider-debug-hook-function)
    (remove-hook 'cider--debug-mode-hook 'meow--cider-debug-hook-function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sly (db)

(defvar meow--sly-setup nil)

(defun meow--sly-debug-hook-function ()
  "Switch meow state when entering/leaving sly-db-mode."
  (if (bound-and-true-p sly-db-mode-hook)
      (meow--switch-to-motion)
    (meow--switch-to-motion)))

(defun meow--setup-sly (enable)
  "Setup sly.
Argument ENABLE non-nil means turn on."
  (setq meow--sly-setup enable)
  (if enable
      (add-hook 'sly-db-hook 'meow--sly-debug-hook-function)
    (remove-hook 'sly-db-hook 'meow--sly-debug-hook-function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; realgud (debug)

(defvar meow--realgud-setup nil)

(defun meow--realgud-debug-hook-function ()
  "Switch meow state when entering/leaving realgud-short-key-mode."
  (if (bound-and-true-p realgud-short-key-mode)
      (meow--switch-to-motion)
    (meow--switch-to-normal)))

(defun meow--setup-realgud (enable)
  "Setup realgud.
Argument ENABLE non-nil means turn on."
  (setq meow--realgud-setup enable)
  (if enable
      (add-hook 'realgud-short-key-mode-hook 'meow--realgud-debug-hook-function)
    (remove-hook 'realgud-short-key-mode-hook 'meow--realgud-debug-hook-function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-key

(defvar meow--which-key-setup nil)

(defun meow--which-key-describe-keymap ()
  (if which-key-mode
      (setq meow-keypad-describe-keymap-function
	(lambda (keymap)
	  (which-key--create-buffer-and-show nil keymap nil (concat "Meow: " (meow--keypad-format-keys)))))
    (setq meow-keypad-describe-keymap-function 'meow-describe-keymap)))

(defun meow--setup-which-key (enable)
  (setq meow--which-key-setup enable)
  (if enable
      (add-hook 'which-key-mode-hook 'meow--which-key-describe-keymap)
    (remove-hook 'which-key-mode-hook 'meow--which-key-describe-keymap)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; input methods

(defvar meow--input-method-setup nil)

(defun meow--input-method-advice (fnc key)
  "Intended to be advice for quail-input-method. Only use the input method in insert mode."
  (funcall (if (and (boundp 'meow-mode) meow-mode (not (meow-insert-mode-p))) #'list fnc) key))

(defun meow--setup-input-method (enable)
  (setq meow--input-method-setup enable)
  (if enable
      (advice-add 'quail-input-method :around 'meow--input-method-advice)
    (advice-remove 'quail-input-method 'meow--input-method-advice)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; polymode

(defvar polymode-move-these-vars-from-old-buffer)

(defvar meow--polymode-setup nil)

(defun meow--setup-polymode (enable)
  "Setup polymode.

Argument ENABLE non-nil means turn on."
  (setq meow--polymode-setup enable)
  (when enable
    (dolist (v '(meow--selection
                 meow--selection-history
                 meow--current-state
                 meow-normal-mode
                 meow-insert-mode
                 meow-keypad-mode
                 meow-beacon-mode
                 meow-motion-mode))
      ;; These vars allow us the select through the polymode chunk
      (add-to-list 'polymode-move-these-vars-from-old-buffer v))))

;; Enable / Disable shims

(defun meow--enable-shims ()
  "Use a bunch of shim setups."
  ;; This lets us start input without canceling selection.
  ;; We will backup `delete-active-region'.
  (setq meow--backup-var-delete-activate-region delete-active-region)
  (setq delete-active-region nil)
  (meow--setup-eldoc t)
  (meow--setup-rectangle-mark t)

  (eval-after-load "wdired" (lambda () (meow--setup-wdired t)))
  (eval-after-load "edebug" (lambda () (meow--setup-edebug t)))
  (eval-after-load "wgrep" (lambda () (meow--setup-wgrep t)))
  (eval-after-load "company" (lambda () (meow--setup-company t)))
  (eval-after-load "polymode" (lambda () (meow--setup-polymode t)))
  (eval-after-load "cider" (lambda () (meow--setup-cider t)))
  (eval-after-load "sly" (lambda () (meow--setup-sly t)))
  (eval-after-load "realgud" (lambda () (meow--setup-realgud t)))
  (eval-after-load "which-key" (lambda () (meow--setup-which-key t)))
  (eval-after-load "undo-tree" (lambda () (meow--setup-undo-tree t)))
  (eval-after-load "diff-hl" (lambda () (meow--setup-diff-hl t)))
  (eval-after-load "quail" (lambda () (meow--setup-input-method t))))

(defun meow--disable-shims ()
  "Remove shim setups."
  (setq delete-active-region meow--backup-var-delete-activate-region)
  (when meow--eldoc-setup (meow--setup-eldoc nil))
  (when meow--rectangle-mark-setup (meow--setup-rectangle-mark nil))
  (when meow--wdired-setup (meow--setup-wgrep nil))
  (when meow--edebug-setup (meow--setup-edebug nil))
  (when meow--company-setup (meow--setup-company nil))
  (when meow--wgrep-setup (meow--setup-wgrep nil))
  (when meow--polymode-setup (meow--setup-polymode nil))
  (when meow--cider-setup (meow--setup-cider nil))
  (when meow--which-key-setup (meow--setup-which-key nil))
  (when meow--diff-hl-setup (meow--setup-diff-hl nil))
  (when meow--input-method-setup (meow--setup-input-method nil)))

;;; meow-shims.el ends here
(provide 'meow-shims)
