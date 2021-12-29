;;; meow-core.el --- Mode definitions for Meow  -*- lexical-binding: t; -*-

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

;;; Modes definition in Meow.

;;; Code:

(require 'cl-lib)

(require 'meow-util)
(require 'meow-command)
(require 'meow-keypad)
(require 'meow-var)
(require 'meow-esc)
(require 'meow-shims)
(require 'meow-beacon)

;;;###autoload
(define-minor-mode meow-insert-mode
  "Meow Insert state."
  :init-value nil
  :lighter " [I]"
  :keymap meow-insert-state-keymap
  (meow--insert-init))

;;;###autoload
(define-minor-mode meow-normal-mode
  "Meow Normal state."
  :init-value nil
  :lighter " [N]"
  :keymap meow-normal-state-keymap
  (meow--normal-init))

;;;###autoload
(define-minor-mode meow-keypad-mode
  "Meow keypad state."
  :init-value nil
  :lighter " [K]"
  ;; use overriding-local-map for highest keymap priority
  ;; so KEYPAD won't be affected by overlays' keymap
  :keymap meow-keypad-state-keymap
  (meow--keypad-init))

;;;###autoload
(define-minor-mode meow-motion-mode
  "Meow motion state."
  :init-value nil
  :lighter " [M]"
  :keymap meow-motion-state-keymap
  (meow--motion-init))

;;;###autoload
(define-minor-mode meow-beacon-mode
  "Meow cursor state."
  :init-value nil
  :lighter " [C]"
  :keymap meow-beacon-state-keymap
  (meow--beacon-init))

;;;###autoload
(define-minor-mode meow-mode
  "Meow minor mode.

This minor mode is used by meow-global-mode, should not be enabled directly."
  :init-value nil
  :interactive nil
  :global nil
  :keymap meow-keymap
  (if meow-mode
      (meow--enable)
    (meow--disable)))

;;;###autoload
(defun meow-indicator ()
  "Indicator showing current mode."
  (or meow--indicator (meow--update-indicator)))

;;;###autoload
(define-global-minor-mode meow-global-mode meow-mode
  (lambda ()
    (unless (minibufferp)
      (meow-mode 1)))
  :group 'meow
  (if meow-mode
      (meow--global-enable)
    (meow--global-disable)))

(defun meow--normal-init ()
  "Init normal state."
  (when meow-normal-mode
    (meow-disable-other-modes 'meow-normal-mode)
    (meow-update-display)))

(defun meow--insert-init ()
  "Init insert state."
  (if meow-insert-mode
      (progn
        (meow-disable-other-modes 'meow-insert-mode)
        (meow-update-display)
        (run-hooks 'meow-insert-enter-hook))
    (when (and meow--insert-pos
               meow-select-on-change
               (not (= (point) meow--insert-pos)))
      (thread-first
          (meow--make-selection '(select . transient) meow--insert-pos (point))
        (meow--select)))
    (run-hooks 'meow-insert-exit-hook)
    (setq-local meow--insert-pos nil)))

(defun meow--motion-init ()
  "Init motion state."
  (when meow-motion-mode
    (if (meow-normal-mode-p)
        (progn
          (meow-disable-other-modes nil) ; disable all modes
          (meow--save-origin-commands)
          (meow-motion-mode)) ; on next entry, normal-mode-p must be nil.
      (progn
        (meow-disable-other-modes 'meow-motion-mode)
        (meow-update-display)))))

(defun meow--keypad-init ()
  "Init keypad state.

We have to remember previous state, so that we can restore it."
  (when meow-keypad-mode
    (setq meow--prefix-arg current-prefix-arg
          ;; meow--keypad-this-command nil
          meow--keypad-keys nil
          meow--use-literal nil
          meow--use-meta nil
          meow--use-both nil)
    (meow-update-display)))

(defun meow--beacon-init ()
  "Init cursor state."
  (if meow-beacon-mode
      (progn
        (setq meow--beacon-backup-hl-line (bound-and-true-p hl-line-mode))
        (meow--cancel-selection)
        (meow-disable-other-modes 'meow-beacon-mode)
        (hl-line-mode -1)
        (meow-update-display))
    (meow-normal-mode 1)
    (when meow--beacon-backup-hl-line
      (hl-line-mode 1))))

(defun meow--enable ()
  "Enable Meow.

This function will switch to the proper state for current
major mode. Firstly, the variable `meow-mode-state-list' will be used.
If current major mode derived from any mode from the list, specified
state will be used.  When no result is found, give a test on the command
bound to key A.  if the command name contains \"self-insert\", then NORMAL
state will be used.  Otherwise, MOTION state will be used.

Before turning on MOTION state, the original commands will be remap.
The new keybinding is generated by prepend `meow-motion-remap-prefix' to
the original keybinding.

Note: When this function is called, NORMAL state is already enabled.
NORMAL state is enabled globally when `meow-global-mode' is used.
because in `fundamental-mode', there's no chance for meow to call
an init function."
  (let ((state-to-modes (seq-group-by #'cdr meow-mode-state-list)))
    (cond
     ;; if MOTION is specified
     ((apply #'derived-mode-p (mapcar #'car (alist-get 'motion state-to-modes)))
      (meow-normal-mode -1)
      (meow--save-origin-commands)
      (meow-motion-mode 1))

     ;; if NORMAL is specified
     ((apply #'derived-mode-p (mapcar #'car (alist-get 'normal state-to-modes)))
      nil)

     ;; if key A is bound to a self-insert command
     ((progn
        ;; Disable meow-normal-mode, we need test the command name bound to a single letter key.
        (meow-normal-mode -1)
        (let ((meow-normal-mode nil)
              (cmd (key-binding "a")))
          (and
           (commandp cmd)
           (symbolp cmd)
           (string-match-p "\\`.*self-insert.*\\'" (symbol-name cmd)))))
      (meow-normal-mode 1))

     ;; fallback to MOTION state
     (t
      (meow--save-origin-commands)
      (meow-motion-mode 1)))))

(defun meow--disable ()
  "Disable Meow."
  (meow-normal-mode -1)
  (meow-insert-mode -1)
  (meow-motion-mode -1))

(defun meow--global-enable ()
  "Enable meow globally."
  (setq-default meow-normal-mode t)
  (dolist (buf (buffer-list))
    (unless (minibufferp buf)
      (with-current-buffer buf
        (setq-local meow-normal-mode 1))))
  (add-hook 'window-state-change-functions #'meow--on-window-state-change)
  (add-hook 'minibuffer-setup-hook #'meow--minibuffer-setup)
  (add-hook 'pre-command-hook 'meow--highlight-pre-command)
  (add-hook 'post-command-hook 'meow--maybe-toggle-beacon-state)
  (add-hook 'suspend-hook 'meow--on-exit)
  (add-hook 'suspend-resume-hook 'meow--update-cursor)
  (add-hook 'kill-emacs-hook 'meow--on-exit)
  (meow--enable-shims)
  ;; meow-esc-mode fix ESC in TUI
  (unless window-system
    (meow-esc-mode 1))
  ;; raise Meow keymap priority
  (add-to-ordered-list 'emulation-mode-map-alists
                       `((meow-motion-mode . ,meow-motion-state-keymap)))
  (add-to-ordered-list 'emulation-mode-map-alists
                       `((meow-normal-mode . ,meow-normal-state-keymap)))
  (add-to-ordered-list 'emulation-mode-map-alists
                       `((meow-keypad-mode . ,meow-keypad-state-keymap)))
  (add-to-ordered-list 'emulation-mode-map-alists
                       `((meow-beacon-mode . ,meow-beacon-state-keymap)))
  (when meow-use-cursor-position-hack
    (setq redisplay-highlight-region-function #'meow--redisplay-highlight-region-function)
    (setq redisplay-unhighlight-region-function #'meow--redisplay-unhighlight-region-function))
  (meow--prepare-face)
  (advice-add 'load-theme :after 'meow--prepare-face))

(defun meow--global-disable ()
  "Disable Meow globally."
  (setq-default meow-normal-mode nil)
  (remove-hook 'window-state-change-functions #'meow--on-window-state-change)
  (remove-hook 'minibuffer-setup-hook #'meow--minibuffer-setup)
  (remove-hook 'pre-command-hook 'meow--highlight-pre-command)
  (remove-hook 'post-command-hook 'meow--maybe-toggle-beacon-state)
  (remove-hook 'suspend-hook 'meow--on-exit)
  (remove-hook 'suspend-resume-hook 'meow--update-cursor)
  (remove-hook 'kill-emacs-hook 'meow--on-exit)
  (meow--disable-shims)
  (meow--remove-modeline-indicator)
  (when meow-use-cursor-position-hack
    (setq redisplay-highlight-region-function meow--backup-redisplay-highlight-region-function)
    (setq redisplay-unhighlight-region-function meow--backup-redisplay-unhighlight-region-function))
  (unless window-system
    (meow-esc-mode -1))
  (advice-remove 'load-theme 'meow--prepare-face))

(provide 'meow-core)
;;; meow-core.el ends here
