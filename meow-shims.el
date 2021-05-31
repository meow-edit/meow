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
Basically, all navigation commands should trigger eldoc."
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
  "Adviced for meow-insert-exit."
  (when company-candidates
    (company-abort)))

(defun meow--setup-company (enable)
  "Setup for company."
  (setq meow--company-setup enable)
  (if enable
      (add-hook 'meow-insert-exit-hook #'meow--company-maybe-abort-advice)
    (remove-hook 'meow-insert-exit-hook #'meow--company-maybe-abort-advice)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wgrep

(defvar meow--wgrep-setup nil
  "Whether already setup wgrep.")

(defun meow--wgrep-to-normal (&rest _ignore)
  "Switch to normal state, used in advice for wgrep.
Optional argument IGNORE ignored."
  (meow--switch-state 'normal))

(defun meow--wgrep-to-motion (&rest _ignore)
  "Switch to motion state, used in advice for wgrep.
Optional argument IGNORE ignored."
  (meow--switch-state 'motion))

(defun meow--setup-wgrep (enable)
  "Setup wgrep.

We use advice here because wgrep doesn't call its hooks."
  (setq meow--wgrep-setup enable)
  (if enable
      (progn
        (advice-add 'wgrep-change-to-wgrep-mode :after #'meow--wgrep-to-normal)
        (advice-add 'wgrep-exit :after #'meow--wgrep-to-motion)
        (advice-add 'wgrep-finish-edit :after #'meow--wgrep-to-motion)
        (advice-add 'wgrep-save-all-buffers :after #'meow--wgrep-to-motion))
    (advice-remove 'wgrep-change-to-wgrep-mode #'meow--wgrep-to-normal)
    (advice-remove 'wgrep-exit #'meow--wgrep-to-motion)
    (advice-remove 'wgrep-finish-edit #'meow--wgrep-to-motion)
    (advice-remove 'wgrep-save-all-buffers #'meow--wgrep-to-motion)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wdired

(defvar meow--wdired-setup nil
  "Whether already setup wdired.")

(defun meow--wdired-enter (&rest _ignore)
  "Switch to normal state, used in hook for wdired.
Optional argument IGNORE ignored."
  (meow--switch-state 'normal))

(defun meow--wdired-exit (&rest _ignore)
  "Switch to motion state, used in advice for wdired.
Optional argument IGNORE ignored."
  (meow--switch-state 'motion))

(defun meow--setup-wdired (enable)
  (setq meow--wdired-setup enable)
  (if enable
      (progn
        (add-hook 'wdired-mode-hook #'meow--wdired-enter)
        (advice-add #'wdired-exit :after #'meow--wdired-exit)
        (advice-add #'wdired-abort-changes :after #'meow--wdired-exit)
        (advice-add #'wdired-finish-edit :after #'meow--wdired-exit))
    (remove-hook 'wdired-mode-hook #'meow--wdired-enter)
    (advice-remove #'wdired-exit #'meow--wdired-exit)
    (advice-remove #'wdired-abort-changes #'meow--wdired-exit)
    (advice-remove #'wdired-finish-edit #'meow--wdired-exit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rectangle-mark-mode

(defvar meow--rectangle-mark-setup nil
  "Whether already setup rectangle-mark.")

(defun meow--rectangle-mark-init ()
  (when (bound-and-true-p rectangle-mark-mode)
    (setq meow--selection
          '((expand . char) 0 0))))

(defun meow--setup-rectangle-mark (enable)
  (setq meow--rectangle-mark-setup enable)
  (if enable
      (add-hook 'rectangle-mark-mode-hook 'meow--rectangle-mark-init)
    (remove-hook 'rectangle-mark-mode-hook 'meow--rectangle-mark-init)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; paredit-mode
;; Paredit will rebind (, [, { to make them insert paired parentheses.
;; However, it's very common for Meow to having an activated region, in this case,
;; paredit will wrap the region with parentheses, this is inconvenient for our case.
;; Since we have modal editing and keypad mode, wrap could be done use SPC m (.
;; So we replace these commands in paredit-mode with our implementations.

(declare-function paredit-open-round "paredit")
(declare-function paredit-open-square "paredit")
(declare-function paredit-open-curly "paredit")
(declare-function paredit-open-angled "paredit")

(defvar meow--paredit-setup nil
  "Whether already setup paredit.")

(defun meow-paredit-open-angled ()
  (interactive)
  (when (region-active-p) (meow--cancel-selection))
  (call-interactively #'paredit-open-angled))

(defun meow-paredit-open-round ()
  (interactive)
  (when (region-active-p) (meow--cancel-selection))
  (call-interactively #'paredit-open-round))

(defun meow-paredit-open-square ()
  (interactive)
  (when (region-active-p) (meow--cancel-selection))
  (call-interactively #'paredit-open-square))

(defun meow-paredit-open-curly ()
  (interactive)
  (when (region-active-p) (meow--cancel-selection))
  (call-interactively #'paredit-open-curly))

(defun meow-paredit-doublequote ()
  (interactive)
  (when (region-active-p) (meow--cancel-selection))
  (call-interactively #'paredit-doublequote))

(defun meow--setup-paredit (enable)
  (setq meow--paredit-setup enable)
  (let ((keymap (symbol-value 'paredit-mode-map)))
    (if enable
        (progn
          (define-key keymap [remap paredit-open-round] #'meow-paredit-open-round)
          (define-key keymap [remap paredit-open-angled] #'meow-paredit-open-angled)
          (define-key keymap [remap paredit-open-square] #'meow-paredit-open-square)
          (define-key keymap [remap paredit-open-curly] #'meow-paredit-open-curly)
          (define-key keymap [remap paredit-doublequote] #'meow-paredit-doublequote))
      (define-key keymap [remap meow-paredit-open-round] #'paredit-open-round)
      (define-key keymap [remap meow-paredit-open-angled] #'paredit-open-angled)
      (define-key keymap [remap meow-paredit-open-square] #'paredit-open-square)
      (define-key keymap [remap meow-paredit-open-curly] #'paredit-open-curly)
      (define-key keymap [remap meow-paredit-doublequote] #'paredit-doublequote))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; edebug

(defvar meow--edebug-setup nil)

(defun meow--edebug-hook-function ()
  (if (bound-and-true-p edebug-mode)
      (meow--switch-state 'motion)
    (meow--switch-state 'normal)))

(defun meow--setup-edebug (enable)
  (setq meow--edebug-setup enable)
  (if enable
      (add-hook 'edebug-mode-hook 'meow--edebug-hook-function)
    (remove-hook 'edebug-mode-hook 'meow--edebug-hook-function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cider (debug)

(defvar meow--cider-setup nil)

(defun meow--cider-debug-hook-function ()
  (if (bound-and-true-p cider--debug-mode)
      (meow--switch-state 'motion)
    (meow--switch-state 'normal)))

(defun meow--setup-cider (enable)
  (setq meow--cider-setup enable)
  (if enable
      (add-hook 'cider--debug-mode-hook 'meow--cider-debug-hook-function)
    (remove-hook 'cider--debug-mode-hook 'meow--cider-debug-hook-function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; polymode

(defvar meow--polymode-setup nil)

(defun meow--setup-polymode (enable)
  (setq meow--polymode-setup enable)
  (when enable
    (dolist (v '(meow--selection meow--selection-history))
      ;; These vars allow us the select through the polymode chunk
      (add-to-list 'polymode-move-these-vars-from-old-buffer v))))

;; Enable / Disable shims

(defun meow--enable-shims ()
  ;; This lets us start input without canceling selection.
  ;; We will backup `delete-active-region'.
  (setq meow--backup-var-delete-activate-region delete-active-region)
  (setq delete-active-region nil)
  (meow--setup-eldoc t)
  (meow--setup-rectangle-mark t)
  (with-eval-after-load "wdired" (meow--setup-wdired t))
  (with-eval-after-load "edebug" (meow--setup-edebug t))
  (with-eval-after-load "wgrep" (meow--setup-wgrep t))
  (with-eval-after-load "company" (meow--setup-company t))
  (with-eval-after-load "paredit" (meow--setup-paredit t))
  (with-eval-after-load "polymode" (meow--setup-polymode t))
  (with-eval-after-load "cider" (meow--setup-cider t)))

(defun meow--disable-shims ()
  (setq delete-active-region meow--backup-var-delete-activate-region)
  (when meow--eldoc-setup (meow--setup-eldoc nil))
  (when meow--rectangle-mark-setup (meow--setup-rectangle-mark nil))
  (when meow--wdired-setup (meow--setup-wgrep nil))
  (when meow--edebug-setup (meow--setup-edebug nil))
  (when meow--company-setup (meow--setup-company nil))
  (when meow--wgrep-setup (meow--setup-wgrep nil))
  (when meow--paredit-setup (meow--setup-paredit nil))
  (when meow--polymode-setup (meow--setup-polymode nil))
  (when meow--cider-setup (meow--setup-cider nil)))

;;; meow-shims.el ends here
(provide 'meow-shims)
