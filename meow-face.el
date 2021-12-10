;;; meow-face.el --- Faces for Meow  -*- lexical-binding: t; -*-

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
;; Faces for Meow.

;;; Code:

(require 'meow-var)

(declare-function meow--mix-color "meow-util")

(defface meow-normal-indicator
  '((((class color) (background dark))
     ())
    (((class color) (background light))
     ()))
  "Normal state indicator."
  :group 'meow)

(defface meow-beacon-indicator
  '((((class color) (background dark))
     ())
    (((class color) (background light))
     ()))
  "Cursor state indicator."
  :group 'meow)

(defface meow-keypad-indicator
  '((((class color) (background dark))
     ())
    (((class color) (background light))
     ()))
  "Keypad state indicator."
  :group 'meow)

(defface meow-insert-indicator
  '((((class color) (background dark))
     ())
    (((class color) (background light))
     ()))
  "Insert state indicator."
  :group 'meow)

(defface meow-motion-indicator
  '((((class color) (background dark))
     ())
    (((class color) (background light))
     ()))
  "Motion state indicator."
  :group 'meow)

(defface meow-normal-cursor
  '((((class color) (background dark))
     (:inherit cursor))
    (((class color) (background light))
     (:inherit cursor)))
  "Normal state cursor."
  :group 'meow)

(defface meow-insert-cursor
  '((((class color) (background dark))
     (:inherit cursor))
    (((class color) (background light))
     (:inherit cursor)))
  "Insert state cursor."
  :group 'meow)

(defface meow-motion-cursor
  '((((class color) (background dark))
     (:inherit cursor))
    (((class color) (background light))
     (:inherit cursor)))
  "Motion state cursor."
  :group 'meow)

(defface meow-keypad-cursor
  '((((class color) (background dark))
     (:inherit cursor))
    (((class color) (background light))
     (:inherit cursor)))
  "Keypad state cursor."
  :group 'meow)

(defface meow-beacon-cursor
  '((((class color) (background dark))
     (:inherit cursor))
    (((class color) (background light))
     (:inherit cursor)))
  "Keypad state cursor."
  :group 'meow)

(defface meow-beacon-cursor
  '((t (:inherit cursor)))
  "BEACON cursor face."
  :group 'meow)

(defface meow-beacon-fake-selection
  '((t (:inherit region)))
  "BEACON selection face."
  :group 'meow)

(defface meow-beacon-fake-cursor
  '((t (:inherit region :extend nil)))
  "BEACON selection face."
  :group 'meow)

(defface meow-unknown-cursor
  '((((class color) (background dark))
     (:inherit cursor))
    (((class color) (background light))
     (:inherit cursor)))
  "Unknown state cursor."
  :group 'meow)

(defface meow-region-cursor-1
  `((((class color) (background dark)))
    (((class color) (background light))))
  "Indicator for region direction."
  :group 'meow)

(defface meow-region-cursor-2
  `((((class color) (background dark)))
    (((class color) (background light))))
  "Indicator for region direction."
  :group 'meow)

(defface meow-region-cursor-3
  `((((class color) (background dark)))
    (((class color) (background light))))
  "Indicator for region direction."
  :group 'meow)

(defface meow-kmacro-cursor
  `((t (:underline t)))
  "Indicator for region direction."
  :group 'meow)

(defface meow-search-highlight
  '((t (:inherit lazy-highlight)))
  "Search target highlight."
  :group 'meow)

(defface meow-position-highlight-number
  '((((class color) (background dark))
     (:inherit default))
    (((class color) (background light))
     (:inherit default)))
  "Num position highlight."
  :group 'meow)

(defface meow-position-highlight-number-1
  '((t (:inherit meow-position-highlight-number)))
  "Num position highlight."
  :group 'meow)

(defface meow-position-highlight-number-2
  '((t (:inherit meow-position-highlight-number)))
  "Num position highlight."
  :group 'meow)

(defface meow-position-highlight-number-3
  '((t (:inherit meow-position-highlight-number)))
  "Num position highlight."
  :group 'meow)

(defface meow-position-highlight-reverse-number-1
  '((t (:inherit meow-position-highlight-number-1)))
  "Num position highlight."
  :group 'meow)

(defface meow-position-highlight-reverse-number-2
  '((t (:inherit meow-position-highlight-number-2)))
  "Num position highlight."
  :group 'meow)

(defface meow-position-highlight-reverse-number-3
  '((t (:inherit meow-position-highlight-number-3)))
  "Num position highlight."
  :group 'meow)

(defface meow-search-indicator
  '((((class color) (background dark))
     (:foreground "grey40"))
    (((class color) (background light))
     (:foreground "grey60")))
  "Face for search indicator."
  :group 'meow)

(defface meow-cheatsheet-command
  '((((class color) (background dark))
     (:height 0.7 :foreground "grey90"))
    (((class color) (background light))
     (:height 0.7 :foreground "grey10")))
  "Face for Meow cheatsheet command."
  :group 'meow)

(defface meow-cheatsheet-highlight
  '((((class color) (background dark))
     (:foreground "grey90"))
    (((class color) (background light))
     (:foreground "grey10")))
  "Face for Meow cheatsheet highlight text."
  :group 'meow)

(defun meow--prepare-face (&rest _ignore)
  "Calculate faces based on current theme dynamically.

This function will be called after each time the theme changed."
  (when meow-use-dynamic-face-color
    (when-let ((r (face-background 'region nil t))
               (c (face-background 'cursor nil t))
               (s (face-background 'secondary-selection nil t))
               (b (face-background 'default nil t))
               (f (face-foreground 'default nil t))
               (bc (face-background 'meow-beacon-cursor nil t)))
      (when (and (color-defined-p r)
                 (color-defined-p c))
        (let* ((clrs (meow--mix-color c r 3))
               (c1 (car clrs))
               (c2 (cadr clrs))
               (c3 (caddr clrs)))
          (set-face-attribute 'meow-region-cursor-1 nil :background c1 :foreground f :distant-foreground b)
          (set-face-attribute 'meow-region-cursor-2 nil :background c2 :foreground f :distant-foreground b)
          (set-face-attribute 'meow-region-cursor-3 nil :background c3 :foreground f :distant-foreground b)))

      (set-face-attribute 'meow-position-highlight-number nil :foreground b :distant-foreground f)

      (when (and (color-defined-p c)
                 (color-defined-p b))
        (let ((c-b-3 (meow--mix-color c b 3)))
          (set-face-background 'meow-position-highlight-number-1 (car c-b-3))
          (set-face-background 'meow-position-highlight-number-2 (cadr c-b-3))
          (set-face-background 'meow-position-highlight-number-3 (caddr c-b-3))))

      (when (and (color-defined-p r)
                 (color-defined-p s))
        (set-face-attribute 'meow-beacon-fake-selection
                            nil
                            :foreground b
                            :distant-foreground f
                            :background (car (meow--mix-color r s 1))))

      (when (and (color-defined-p bc)
                 (color-defined-p s))
        (set-face-attribute 'meow-beacon-fake-cursor
                            nil
                            :foreground b
                            :distant-foreground f
                            :extend nil
                            :background (car (meow--mix-color bc s 1)))))))

(provide 'meow-face)
;;; meow-face.el ends here
