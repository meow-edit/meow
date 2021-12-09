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

;;

;;; Code:

(defvar meow-esc-delay 0.1)
(defvar meow-esc-mode nil)

(defun meow-esc-mode (&optional arg)
  (cond
   ((or (null arg) (eq arg 0))
    (meow-esc-mode (if meow-esc-mode -1 +1)))
   ((> arg 0)
    (unless meow-esc-mode
      (setq meow-esc-mode t)
      (add-hook 'after-make-frame-functions #'meow-init-esc)
      (mapc #'meow-init-esc (frame-list))))
   ((< arg 0)
    (when meow-esc-mode
      (remove-hook 'after-make-frame-functions #'meow-init-esc)
      (mapc #'meow-deinit-esc (frame-list))
      (setq meow-esc-mode nil)))))

(defvar meow--escape-key-seq [?\e])

(defun meow-init-esc (frame)
  (with-selected-frame frame
    (let ((term (frame-terminal frame)))
      (when (not (terminal-parameter term 'meow-esc-map))
        (let ((meow-esc-map (lookup-key input-decode-map [?\e])))
          (set-terminal-parameter term 'meow-esc-map meow-esc-map)
          (define-key input-decode-map meow--escape-key-seq
            `(menu-item "" ,meow-esc-map :filter ,#'meow-esc)))))))

(defun meow-deinit-esc (frame)
  (with-selected-frame frame
    (let ((term (frame-terminal frame)))
      (when (terminal-live-p term)
        (let ((meow-esc-map (terminal-parameter term 'meow-esc-map)))
          (when meow-esc-map
            (define-key input-decode-map meow--escape-key-seq meow-esc-map)
            (set-terminal-parameter term 'meow-esc-map nil)))))))

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
