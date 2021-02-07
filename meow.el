;;; meow.el --- Modal Editing On Wheel  -*- lexical-binding: t; -*-

;; Author: Shi Tianshu
;; Keywords: convenience, modal-editing
;; Package-Requires: ((emacs "26.3") (dash "2.12.0") (cl-lib "0.6.1") (s "1.12.0"))
;; Version: 1.0.2
;; URL: https://www.github.com/DogLooksGood/meow
;;
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

;; Enable `meow-global-mode' to activate modal editing.

;;; Code:

;;; Dependencies

(require 'cl-lib)
(require 'dash)

;;; Modules

(require 'meow-var)
(require 'meow-face)
(require 'meow-keymap)
(require 'meow-helpers)
(require 'meow-util)
(require 'meow-keypad)
(require 'meow-command)
(require 'meow-core)
(require 'meow-cheatsheet)

(provide 'meow)
;;; meow.el ends here
