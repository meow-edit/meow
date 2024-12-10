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
;; corfu

(declare-function corfu-quit "corfu")

(defvar meow--corfu-setup nil
  "Whether already setup corfu.")

(defun meow--corfu-maybe-abort-advice ()
  "Adviced for `meow-insert-exit'."
  (when (bound-and-true-p corfu-mode) (corfu-quit)))

(defun meow--setup-corfu (enable)
  "Setup for corfu.
Argument ENABLE non-nil means turn on."
  (setq meow--corfu-setup enable)
  (if enable
      (add-hook 'meow-insert-exit-hook #'meow--corfu-maybe-abort-advice)
    (remove-hook 'meow-insert-exit-hook #'meow--corfu-maybe-abort-advice)))

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
    (advice-remove 'diff-hl-show-hunk-inline-popup 'meow--switch-to-motion)
    (advice-remove 'diff-hl-show-hunk-posframe 'meow--switch-to-motion)
    (advice-remove 'diff-hl-show-hunk-hide 'meow--switch-to-normal)))

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
        (advice-add 'wgrep-abort-changes :after #'meow--switch-to-motion)
        (advice-add 'wgrep-save-all-buffers :after #'meow--switch-to-motion))
    (advice-remove 'wgrep-change-to-wgrep-mode #'meow--switch-to-normal)
    (advice-remove 'wgrep-exit #'meow--switch-to-motion)
    (advice-remove 'wgrep-abort-changes #'meow--switch-to-motion)
    (advice-remove 'wgrep-finish-edit #'meow--switch-to-motion)
    (advice-remove 'wgrep-save-all-buffers #'meow--switch-to-motion)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wdired

(defvar meow--wdired-setup nil
  "Whether already setup wdired.")

(defvar wdired-mode-hook)

(declare-function wdired-exit "wdired")
(declare-function wdired-finish-edit "wdired")
(declare-function wdired-abort-changes "wdired")

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
;; magit

(defvar meow--magit-setup nil)

(defun meow--magit-blame-hook-function ()
  "Switch meow state when entering/leaving `magit-blame-read-only-mode'."
  (if (bound-and-true-p magit-blame-read-only-mode)
      (meow--switch-to-motion)
    (meow--switch-to-normal)))

(defun meow--setup-magit (enable)
  "Setup magit.
Argument ENABLE non-nil means turn on."
  (setq meow--magit-setup enable)
  (if enable
      (add-hook 'magit-blame-mode-hook 'meow--magit-blame-hook-function)
    (remove-hook 'magit-blame-mode-hook 'meow--magit-blame-hook-function)))

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
;; macrostep

(defvar macrostep-overlays)
(defvar macrostep-mode)

(defvar meow--macrostep-setup nil)
(defvar meow--macrostep-setup-previous-state nil)

(defun meow--macrostep-inside-overlay-p ()
  "Return whether point is inside a `macrostep-mode' overlay."
  (seq-some (let ((pt (point)))
              (lambda (ov)
                (and (<= (overlay-start ov) pt)
                     (< pt (overlay-end ov)))))
            macrostep-overlays))

(defun meow--macrostep-post-command-function ()
  "Function to run in `post-commmand-hook' when `macrostep-mode' is enabled.

`macrostep-mode' uses a local keymap for the overlay showing the
expansion.  Switch to Motion state when we enter the overlay and
try to switch back to the previous state when leaving it."
  (if (meow--macrostep-inside-overlay-p)
      ;; The overlay is not editable, so the `macrostep-mode' commands are
      ;; likely more important than the Beacon-state commands and possibly more
      ;; important than any custom-state commands.  It is less important than
      ;; Keypad state.
      (unless (eq meow--current-state 'keypad)
        (meow--switch-to-motion))
    (meow--switch-state meow--macrostep-setup-previous-state)))

(defun meow--macrostep-record-outside-state (state)
  "Record the Meow STATE in most circumstances, so that we can return to it later.

This function receives the STATE to which one switches via `meow--switch-state'
inside `meow-switch-state-hook'.

Record the state if:
- We are outside the overlay and not in Keypad state.
- We are inside the overlay and not in Keypad or Motion state."
  ;; We assume that the user will not try to switch to Motion state for the
  ;; entire buffer while we are already in Motion state while inside an overlay.
  (unless (eq state 'keypad)
    (if (not (meow--macrostep-inside-overlay-p))
        (setq-local meow--macrostep-setup-previous-state state)
      (unless (eq state 'motion)
        (setq-local meow--macrostep-setup-previous-state state)))))

(defun meow--macrostep-hook-function ()
  "Switch Meow state when entering/leaving `macrostep-mode' or its overlays."
  (if macrostep-mode
      (progn
        (setq-local meow--macrostep-setup-previous-state meow--current-state)
        ;; Add to end of `post-command-hook', so that this function is run after
        ;; the check for whether we should switch to Beacon state.
        (add-hook 'post-command-hook #'meow--macrostep-post-command-function 90 t)
        (add-hook 'meow-switch-state-hook #'meow--macrostep-record-outside-state nil t))
    ;; The command `macrostep-collapse' does not seem to trigger
    ;; `post-command-hook', so we switch back manually.
    (meow--switch-state meow--macrostep-setup-previous-state)
    (setq-local meow--macrostep-setup-previous-state nil)
    (remove-hook 'meow-switch-state-hook #'meow--macrostep-record-outside-state t)
    (remove-hook 'post-command-hook #'meow--macrostep-post-command-function t)))

(defun meow--setup-macrostep (enable)
  "Setup macrostep.
Argument ENABLE non-nil means turn on."
  (setq meow--macrostep-setup enable)
  (if enable
      (add-hook 'macrostep-mode-hook 'meow--macrostep-hook-function)
    (remove-hook 'macrostep-mode-hook 'meow--macrostep-hook-function)))

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

(defvar which-key-mode)
(declare-function which-key--create-buffer-and-show "which-key"
                  (&optional prefix-keys from-keymap filter prefix-title))

(defvar meow--which-key-setup nil)

(defun meow--which-key-describe-keymap ()
  "Use which-key for keypad popup."
  (if which-key-mode
      (setq
       which-key-use-C-h-commands nil
       meow-keypad-describe-keymap-function
	(lambda (keymap)
	  (which-key--create-buffer-and-show nil keymap nil (concat meow-keypad-message-prefix (meow--keypad-format-keys))))
        meow-keypad-clear-describe-keymap-function 'which-key--hide-popup)

    (setq meow-keypad-describe-keymap-function 'meow-describe-keymap
          meow-keypad-clear-describe-keymap-function nil
          which-key-use-C-h-commands t)))

(defun meow--setup-which-key (enable)
  "Setup which-key.
Argument ENABLE non-nil means turn on."
  (setq meow--which-key-setup enable)
  (if enable
      (add-hook 'which-key-mode-hook 'meow--which-key-describe-keymap)
    (remove-hook 'which-key-mode-hook 'meow--which-key-describe-keymap)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; input methods

(defvar meow--input-method-setup nil)

(defun meow--input-method-advice (fnc key)
  "Advice for `quail-input-method'.

Only use the input method in insert mode.
Argument FNC, input method function.
Argument KEY, the current input."
  (funcall (if (and (boundp 'meow-mode) meow-mode (not (meow-insert-mode-p))) #'list fnc) key))

(defun meow--setup-input-method (enable)
  "Setup input-method.
Argument ENABLE non-nil means turn on."
  (setq meow--input-method-setup enable)
  (if enable
      (advice-add 'quail-input-method :around 'meow--input-method-advice)
    (advice-remove 'quail-input-method 'meow--input-method-advice)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ddskk

(defvar skk-henkan-mode)

(defvar meow--ddskk-setup nil)
(defun meow--ddskk-skk-previous-candidate-advice (fnc &optional arg)
  (if (and (not (eq skk-henkan-mode 'active))
           (not (eq last-command 'skk-kakutei-henkan))
           last-command-event
           (eq last-command-event
               (seq-first (car (where-is-internal
                                'meow-prev
                                meow-normal-state-keymap)))))
      (forward-line -1)
    (funcall fnc arg)))

(defun meow--setup-ddskk (enable)
  (setq meow--ddskk-setup enable)
  (if enable
      (advice-add 'skk-previous-candidate :around
                  'meow--ddskk-skk-previous-candidate-advice)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eat-eshell

(defvar meow--eat-eshell-setup nil)
(defvar meow--eat-eshell-mode-override nil)

(declare-function eat-eshell-emacs-mode "eat")
(declare-function eat-eshell-semi-char-mode "eat")
(declare-function eat-eshell-char-mode "eat")

(declare-function meow-insert-mode "meow-core")

(defun meow--eat-eshell-mode-override-enable ()
  (setq-local meow--eat-eshell-mode-override t)
  (add-hook 'meow-insert-enter-hook #'eat-eshell-char-mode nil t)
  (add-hook 'meow-insert-exit-hook #'eat-eshell-emacs-mode nil t)
  (if (bound-and-true-p meow-insert-mode)
      (eat-eshell-char-mode)
    (eat-eshell-emacs-mode)))

(defun meow--eat-eshell-mode-override-disable ()
  (setq-local meow--eat-eshell-mode-override nil)
  (remove-hook 'meow-insert-enter-hook #'eat-eshell-char-mode t)
  (remove-hook 'meow-insert-exit-hook #'eat-eshell-emacs-mode t))

(defun meow--setup-eat-eshell (enable)
  (setq meow--eat-eshell-setup enable)
  (if enable
      (progn (add-hook 'eat-eshell-exec-hook #'meow--eat-eshell-mode-override-enable)
             (add-hook 'eat-eshell-exit-hook #'meow--eat-eshell-mode-override-disable)
             (add-hook 'eat-eshell-exit-hook #'meow--update-cursor))

    (remove-hook 'eat-eshell-exec-hook #'meow--eat-eshell-mode-override-enable)
    (remove-hook 'eat-eshell-exit-hook #'meow--eat-eshell-mode-override-disable)
    (remove-hook 'eat-eshell-exit-hook #'meow--update-cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ediff
(defvar meow--ediff-setup nil)

(defun meow--setup-ediff (enable)
  "Setup Ediff.
Argument ENABLE, non-nil means turn on."
  (if enable
      (add-hook 'ediff-mode-hook 'meow-motion-mode)
    (remove-hook 'ediff-mode-hook 'meow-motion-mode)))

;; Enable / Disable shims

(defun meow--enable-shims ()
  "Use a bunch of shim setups."
  ;; This lets us start input without canceling selection.
  ;; We will backup `delete-active-region'.
  (setq meow--backup-var-delete-activate-region delete-active-region)
  (setq delete-active-region nil)
  (meow--setup-eldoc t)
  (meow--setup-rectangle-mark t)

  (eval-after-load "macrostep" (lambda () (meow--setup-macrostep t)))
  (eval-after-load "wdired" (lambda () (meow--setup-wdired t)))
  (eval-after-load "edebug" (lambda () (meow--setup-edebug t)))
  (eval-after-load "magit" (lambda () (meow--setup-magit t)))
  (eval-after-load "wgrep" (lambda () (meow--setup-wgrep t)))
  (eval-after-load "company" (lambda () (meow--setup-company t)))
  (eval-after-load "corfu" (lambda () (meow--setup-corfu t)))
  (eval-after-load "polymode" (lambda () (meow--setup-polymode t)))
  (eval-after-load "cider" (lambda () (meow--setup-cider t)))
  (eval-after-load "sly" (lambda () (meow--setup-sly t)))
  (eval-after-load "realgud" (lambda () (meow--setup-realgud t)))
  (eval-after-load "which-key" (lambda () (meow--setup-which-key t)))
  (eval-after-load "undo-tree" (lambda () (meow--setup-undo-tree t)))
  (eval-after-load "diff-hl" (lambda () (meow--setup-diff-hl t)))
  (eval-after-load "quail" (lambda () (meow--setup-input-method t)))
  (eval-after-load "skk" (lambda () (meow--setup-ddskk t)))
  (eval-after-load "eat" (lambda () (meow--setup-eat-eshell t)))
  (eval-after-load "ediff" (lambda () (meow--setup-ediff t))))

(defun meow--disable-shims ()
  "Remove shim setups."
  (setq delete-active-region meow--backup-var-delete-activate-region)
  (when meow--macrostep-setup (meow--setup-macrostep nil))
  (when meow--eldoc-setup (meow--setup-eldoc nil))
  (when meow--rectangle-mark-setup (meow--setup-rectangle-mark nil))
  (when meow--wdired-setup (meow--setup-wdired nil))
  (when meow--edebug-setup (meow--setup-edebug nil))
  (when meow--magit-setup (meow--setup-magit nil))
  (when meow--company-setup (meow--setup-company nil))
  (when meow--corfu-setup (meow--setup-corfu nil))
  (when meow--wgrep-setup (meow--setup-wgrep nil))
  (when meow--polymode-setup (meow--setup-polymode nil))
  (when meow--cider-setup (meow--setup-cider nil))
  (when meow--which-key-setup (meow--setup-which-key nil))
  (when meow--diff-hl-setup (meow--setup-diff-hl nil))
  (when meow--input-method-setup (meow--setup-input-method nil))
  (when meow--ddskk-setup (meow--setup-ddskk nil))
  (when meow--eat-eshell-setup (meow--setup-eat-eshell nil))
  (when meow--ediff-setup (meow--setup-ediff nil)))

;;; meow-shims.el ends here
(provide 'meow-shims)
