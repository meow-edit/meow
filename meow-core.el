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
(require 'dash)

(require 'meow-util)
(require 'meow-command)
(require 'meow-keypad)
(require 'meow-var)
(require 'meow-esc)
(require 'meow-shims)

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
  (meow--normal-init))

;;;###autoload
(define-minor-mode meow-keypad-mode
  "Meow keypad state."
  nil
  " [K]"
  ;; use overriding-local-map for highest keymap priority
  ;; so KEYPAD won't be affected by overlays' keymap
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

;;;###autoload
(defun meow-indicator ()
  "Indicator show current mode."
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
    (meow-insert-mode -1)
    (meow-motion-mode -1)))

(defun meow--insert-init ()
  "Init insert state."
  (if meow-insert-mode
      (progn
        (meow-normal-mode -1)
        (meow-motion-mode -1)
        (setq-local meow--insert-pos (point)))
    (when (and meow--insert-pos meow-select-on-exit
               (not (= (point) meow--insert-pos))
               ;; This feature should be only enable in text, conf and prog mode.
               (derived-mode-p 'text-mode 'conf-mode 'prog-mode))
      (-> (meow--make-selection '(select . transient) meow--insert-pos (point))
          (meow--select)))
    (setq-local meow--insert-pos nil)))

(defun meow--motion-init ()
  "Init motion state."
  (when meow-motion-mode
    (meow-normal-mode -1)
    (meow-insert-mode -1)))

(defun meow--keypad-init ()
  "Init keypad state.

We have to remember previous state, so that we can restore it."
  (when meow-keypad-mode
    (progn
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
            meow--use-meta nil
            meow--use-both nil))))

(defun meow--enable ()
  "Enable Meow.

before activate any state.
then SPC will be bound to LEADER."
  (unless (apply #'derived-mode-p meow-normal-state-mode-list)
    (meow-normal-mode -1)
    (cl-loop for key in meow--motion-overwrite-keys do
             (let ((cmd (key-binding key)))
               (when (and (commandp cmd)
                          (not (equal cmd 'undefined)))
                 (push (cons key cmd) meow--origin-commands))))
    (meow-motion-mode 1)))

(defun meow--disable ()
  "Disable Meow."
  (meow-normal-mode -1)
  (meow-insert-mode -1)
  (meow-motion-mode -1))

(defun meow--global-enable ()
  "Enable meow globally."
  (setq-default meow-normal-mode t)
  (add-hook 'window-state-change-functions #'meow--on-window-state-change)
  (add-hook 'minibuffer-setup-hook #'meow--minibuffer-setup)
  (meow--enable-shims)
  (meow-esc-mode 1)
  ;; raise Meow keymap priority
  (add-to-ordered-list 'emulation-mode-map-alists
					   `((meow-normal-mode . ,meow-normal-state-keymap)))
  (add-to-ordered-list 'emulation-mode-map-alists
					   `((meow-keypad-mode . ,meow-keypad-state-keymap))))

(defun meow--global-disable ()
  "Disable Meow globally."
  (setq-default meow-normal-mode nil)
  (remove-hook 'window-state-change-functions #'meow--on-window-state-change)
  (remove-hook 'minibuffer-setup-hook #'meow--minibuffer-setup)
  (meow--disable-shims)
  (meow-esc-mode -1))

(provide 'meow-core)
;;; meow-core.el ends here
