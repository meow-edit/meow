;;; meow-face.el --- Faces for Meow
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
;; Faces for Meow.

;;; Code:

(defface meow-normal-indicator
  '((((class color) (background dark))
     (:inherit font-lock-constant-face))
    (((class color) (background light))
     (:inherit font-lock-constant-face)))
  "Normal state indicator."
  :group 'meow)

(defface meow-keypad-indicator
  '((((class color) (background dark))
     (:inherit font-lock-string-face))
    (((class color) (background light))
     (:inherit font-lock-string-face)))
  "Keypad indicator"
  :group 'meow)

(defface meow-insert-indicator
  '((((class color) (background dark))
     (:inherit font-lock-function-name-face))
    (((class color) (background light))
     (:inherit font-lock-function-name-face)))
  "Insert indicator"
  :group 'meow)

(defface meow-motion-indicator
  '((((class color) (background dark))
     (:inherit font-lock-type-face))
    (((class color) (background light))
     (:inherit font-lock-type-face)))
  "Motion indicator"
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

(defface meow-unknown-cursor
  '((((class color) (background dark))
     (:inherit cursor))
    (((class color) (background light))
     (:inherit cursor)))
  "Unknown state cursor."
  :group 'meow)

(defface meow-search-highlight
  '((((class color) (background dark))
     (:background "grey20"))
    (((class color) (background light))
     (:background "grey80")))
  "Search target highlight"
  :group 'meow)

(defface meow-position-highlight-number-1
  '((((class color) (background dark))
     (:foreground "mediumspringgreen"))
    (((class color) (background light))
     (:foreground "red")))
  "Num position highlight"
  :group 'meow)

(defface meow-position-highlight-number-2
  '((((class color) (background dark))
     (:foreground "PaleGreen4"))
    (((class color) (background light))
     (:foreground "red3")))
  "Num position highlight"
  :group 'meow)

(defface meow-position-highlight-number-3
  '((((class color) (background dark))
     (:foreground "DarkGreen"))
    (((class color) (background light))
     (:foreground "DarkRed")))
  "Num position highlight"
  :group 'meow)

(defface meow-position-highlight-reverse-number-1
  '((((class color) (background dark))
     (:foreground "SkyBlue"))
    (((class color) (background light))
     (:foreground "DarkOrange1")))
  "Num position highlight"
  :group 'meow)

(defface meow-position-highlight-reverse-number-2
  '((((class color) (background dark))
     (:foreground "CadetBlue"))
    (((class color) (background light))
     (:foreground "DarkOrange3")))
  "Num position highlight"
  :group 'meow)

(defface meow-position-highlight-reverse-number-3
  '((((class color) (background dark))
     (:foreground "SteelBlue"))
    (((class color) (background light))
     (:foreground "DarkOrange4")))
  "Num position highlight"
  :group 'meow)

(defface meow-cheatsheet
  '((((class color) (background dark))
     (:height 80))
    (((class color) (background light))
     (:height 80)))
  "Face for Meow cheatsheet."
  :group 'meow)

(provide 'meow-face)
;;; meow-face.el ends here
