;;; meow-esc.el --- make ESC works in TUI       -*- lexical-binding: t; -*-

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; In the terminal, ESC can be used as META, because they send the
;; same keycode.  To allow both usages simulataneously, you can
;; customize meow-esc-delay, the maximum time between ESC and the
;; keypress that should be treated as a meta combo. If the time is
;; longer than the delay, it's treated as pressing ESC and then the
;; key separately.
;;; Code:

(defvar meow-esc-delay 0.1)
(defvar meow--escape-key-seq [?\e])

;;;###autoload
(define-minor-mode meow-esc-mode
  "Mode that ensures ESC works in the terminal"
  :init-value nil
  :interactive nil
  :global t
  :keymap nil
  (if meow-esc-mode
      (progn
        (setq meow-esc-mode t)
        (add-hook 'after-make-frame-functions #'meow--init-esc-if-tui)
        (mapc #'meow--init-esc-if-tui (frame-list)))
    (progn
      (remove-hook 'after-make-frame-functions #'meow--init-esc-if-tui)
      (mapc #'meow--deinit-esc-if-tui (frame-list))
      (setq meow-esc-mode nil))))


(defun meow--init-esc-if-tui (frame)
  (with-selected-frame frame
    (unless window-system
      (let ((term (frame-terminal frame)))
        (when (not (terminal-parameter term 'meow-esc-map))
          (let ((meow-esc-map (lookup-key input-decode-map [?\e])))
            (set-terminal-parameter term 'meow-esc-map meow-esc-map)
            (define-key input-decode-map meow--escape-key-seq
                        `(menu-item "" ,meow-esc-map :filter ,#'meow-esc))))))))

(defun meow--deinit-esc-if-tui (frame)
  (with-selected-frame frame
    (unless window-system
      (let ((term (frame-terminal frame)))
        (when (terminal-live-p term)
          (let ((meow-esc-map (terminal-parameter term 'meow-esc-map)))
            (when meow-esc-map
              (define-key input-decode-map meow--escape-key-seq meow-esc-map)
              (set-terminal-parameter term 'meow-esc-map nil))))))))

(defun meow-esc (map)
  (if (and (let ((keys (this-single-command-keys)))
             (and (> (length keys) 0)
                  (= (aref keys (1- (length keys))) ?\e)))
           (sit-for meow-esc-delay))
      (prog1 [escape]
        (when defining-kbd-macro
          (end-kbd-macro)
          (setq last-kbd-macro (vconcat last-kbd-macro [escape]))
          (start-kbd-macro t t)))
    map))

(provide 'meow-esc)
;;; meow-esc.el ends here
