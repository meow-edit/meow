;;; meow-core.el --- Mode definitions for Meow
;;; -*- lexical-binding: t -*-

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
(require 'dash)

(require 'meow-util)
(require 'meow-keypad)
(require 'meow-var)
(require 'meow-eldoc)
(require 'meow-wgrep)
(require 'meow-yas)
(require 'meow-company)
(require 'meow-esc)

;;;###autoload
(define-minor-mode meow-insert-mode
  "Meow Insert state."
  nil
  " [I]"
  meow-insert-state-keymap
  (meow--insert-init))

;;;###autoload
(define-minor-mode meow-normal-mode
  "Meow Normal state."
  nil
  " [N]"
  meow-normal-state-keymap
  (if meow-normal-mode
	  (meow--normal-init)
	(meow--normal-uninit)))

;;;###autoload
(define-minor-mode meow-keypad-mode
  "Meow keypad state."
  nil
  " [K]"
  meow-keypad-state-keymap
  (meow--keypad-init))

;;;###autoload
(define-minor-mode meow-motion-mode
  "Meow motion state."
  nil
  " [M]"
  meow-motion-state-keymap
  (meow--motion-init))

;;;###autoload
(define-minor-mode meow-mode
  "Meow minor mode.

This minor mode is used by meow-global-mode, should not be enabled directly."
  nil
  nil
  meow-keymap
  (if meow-mode
      (meow--enable)
    (meow--disable)))

(defun meow-minimal-indicator ()
  "Minimal indicator show current mode."
  (when (bound-and-true-p meow-global-mode)
    (cond
     (meow-keypad-mode
      (propertize (concat
                   " KEYPAD ["
                   (meow--keypad-format-prefix)
                   (meow--keypad-format-keys)
                   "] ")
                  'face 'meow-keypad-indicator))
     (meow-normal-mode
      (propertize
       " NORMAL "
       'face 'meow-normal-indicator))
     (meow-motion-mode
      (propertize " MOTION " 'face 'meow-motion-indicator))
     (meow-insert-mode
      (cond
       ;; Vterm's vterm-mode is read-only.
       ((and buffer-read-only (not (equal major-mode 'vterm-mode)))
        (propertize " READONLY " 'face 'meow-insert-indicator))
       ((bound-and-true-p overwrite-mode)
        (propertize " OVERWRITE " 'face 'meow-insert-indicator))
       (t (propertize " INSERT " 'face 'meow-insert-indicator))))
     (t ""))))

(defun meow-indicator ()
  "Indicator show current mode."
  (when (bound-and-true-p meow-global-mode)
    (cond
     (meow-keypad-mode
      (propertize (concat
                   " KEYPAD ["
                   (meow--keypad-format-prefix)
                   (meow--keypad-format-keys)
                   "] ")
                  'face 'meow-keypad-indicator))
     (meow-normal-mode
      (propertize
       (concat
        (if (meow--direction-backward-p)
            " NORMAL« "
          " NORMAL ")
        (when-let ((sel-type (meow--selection-type)))
          (concat "["
                  (symbol-name sel-type)
                  "] ")))
       'face 'meow-normal-indicator))
     (meow-motion-mode
      (propertize " MOTION " 'face 'meow-motion-indicator))
     (meow-insert-mode
      (cond
       ;; Vterm's vterm-mode is read-only.
       ((and buffer-read-only (not (equal major-mode 'vterm-mode)))
        (propertize " READONLY " 'face 'meow-insert-indicator))
       ((bound-and-true-p overwrite-mode)
        (propertize " OVERWRITE " 'face 'meow-insert-indicator))
       (t (propertize " INSERT " 'face 'meow-insert-indicator))))
     (t ""))))

;;;###autoload
(define-global-minor-mode meow-global-mode meow-mode
  (lambda ()
    (unless (minibufferp)
      (meow-mode 1)))
  (if meow-mode
      (meow--global-enable)
    (meow--global-disable)))

(defun meow--normal-init ()
  "Init normal state."
  (when meow-normal-mode
    (meow-insert-mode -1)
    (meow-motion-mode -1)
	(let ((keymap (meow--get-mode-leader-keymap major-mode t)))
      (unless meow--keymap-loaded
        (define-key meow-normal-state-keymap (kbd "SPC") keymap)
        (setq-local meow--keymap-loaded t)))))

(defun meow--normal-uninit ())

(defun meow--insert-init ()
  "Init insert state."
  (when meow-insert-mode
    (meow-normal-mode -1)
    (meow-motion-mode -1)))

(defun meow--motion-init ()
  "Init motion state."
  (when meow-motion-mode
    (meow-normal-mode -1)
    (meow-insert-mode -1)
    (unless meow--keymap-loaded
      (let ((keymap (meow--get-mode-leader-keymap major-mode t)))
        (define-key meow-motion-state-keymap (kbd "SPC") keymap))
      (setq-local meow--keymap-loaded t))))

(defun meow--keypad-init ()
  "Init keypad state.

We have to remember previous state, so that we can restore it."
  (cond
   ((meow-motion-mode-p)
    (setq meow--keypad-previous-state 'motion)
    (meow-motion-mode -1))
   ((meow-normal-mode-p)
    (setq meow--keypad-previous-state 'normal)
    (meow-normal-mode -1)))
  (setq meow--prefix-arg current-prefix-arg
        meow--keypad-keys nil
        meow--use-literal nil
        meow--use-meta nil))

(defun meow--enable ()
  "Enable Meow.

We will save command on SPC to variable `meow--space-command'
before activate any state.
then SPC will be bound to LEADER."
  (unless meow--space-command
    (let ((cmd (key-binding (read-kbd-macro "SPC"))))
      (when (and (commandp cmd)
                 (not (equal cmd 'undefined)))
        (setq-local meow--space-command cmd))))
  (if (apply #'derived-mode-p meow-normal-state-mode-list)
      (meow--switch-state 'normal)
    (meow--switch-state 'motion))
  (meow--update-cursor))

(defun meow--disable ()
  "Disable Meow."
  (meow-normal-mode -1)
  (meow-insert-mode -1)
  (meow-motion-mode -1))

(defun meow--global-enable ()
  "Enable meow globally."
  (global-set-key (kbd "<escape>") 'meow-escape-or-normal-modal)
  (setq delete-active-region nil)
  (meow--eldoc-setup)
  (when (featurep 'wgrep)
    (meow--wgrep-setup))
  (when (featurep 'yasnippet)
    (meow--yas-setup))
  (when (featurep 'company)
    (meow--company-setup))
  (meow-esc-mode 1)
  (add-hook 'window-state-change-functions #'meow--window-change-function)
  (add-hook 'post-command-hook #'meow--update-cursor))

(defun meow--global-disable ()
  "Disable Meow globally."
  (global-unset-key (kbd "<escape>"))
  (meow-esc-mode -1)
  (remove-hook 'window-state-change-functions #'meow--window-change-function)
  (remove-hook 'post-command-hook #'meow--update-cursor))

(provide 'meow-core)
;;; meow-core.el ends here
