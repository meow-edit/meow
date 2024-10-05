(require 'evil-common)
(require 'evil-core)

;; movement
(evil-define-key 'normal 'global (kbd "k")   'previous-line)
(evil-define-key 'normal 'global (kbd "j")   'next-line)
(evil-define-key 'normal 'global (kbd "l")   'forward-char)
(evil-define-key 'normal 'global (kbd "h")   'backward-char)

(evil-define-key 'normal 'global (kbd "i")   'helix-insert)
(evil-define-key 'insert 'global (kbd "ESC") 'evil-normal-state)
(evil-define-key 'normal 'global (kbd "a")   'helix-append)

(evil-define-key 'normal 'global (kbd "e")   'helix-move-next-word-end)
(evil-define-key 'normal 'global (kbd "b")   'helix-move-prev-word-start)
(evil-define-key 'normal 'global (kbd "B")   'helix-move-prev-long-word-start)
(evil-define-key 'normal 'global (kbd "w")   'helix-move-next-word-start)
(evil-define-key 'normal 'global (kbd "W")   'helix-move-next-long-word-start)
(evil-define-key 'normal 'global (kbd "x")   'helix-mark-line)
(evil-define-key 'normal 'global (kbd "d")   'helix-delete-region-or-char)
(evil-define-key 'normal 'global (kbd "p")   'yank)
(evil-define-key 'normal 'global (kbd "y")   'helix-kill-ring-save)
(evil-define-key 'normal 'global (kbd "u")   'undo)
(evil-define-key 'normal 'global (kbd "U")   'redo)
;; (evil-define-key 'normal 'global (kbd "q")   'helix-toggle-kmacro-recording)
;; (evil-define-key 'normal 'global (kbd "Q")   'kmacro-call-macro)
(evil-define-key 'normal 'global (kbd ";")   'helix-collapse-region)
(evil-define-key 'normal 'global (kbd "f")   'helix-find-char)

(evil-define-key 'normal 'global (kbd "c")   'helix-change)
(evil-define-key 'normal 'global (kbd "o")   'helix-open-line)

(evil-set-leader '(normal) (kbd "SPC"))

(provide 'helix-keybindings)
