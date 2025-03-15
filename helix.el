;;; helix.el --- Sort of emulate Helix I guess -*- lexical-binding: t; -*-

;; Author: <tprost@users.noreply.github.com>
;; Keywords: convenience, helix, modal-editing
;; Package-Requires: ((emacs "27.1") (evil "1.14.0"))
;; Version: 0.1.0
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

;; (require 'helix-evil)

(require 'helix-vars)
(require 'helix-faces)
(require 'helix-core)
(require 'helix-helpers)
;; (require 'helix-util)



(require 'expand-region)
(require 'multiple-cursors)

(require 'helix-things)





;; (require 'helix-helpers)
;;(require 'helix-states)

(require 'helix-commands)
(require 'helix-selections)
;; (require 'helix-evil-multiedit)
(require 'helix-keybindings)




(provide 'helix)
