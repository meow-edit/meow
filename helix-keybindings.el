(require 'evil-common)
(require 'evil-core)



(evil-define-key 'normal 'global (kbd "1") 'digit-argument)
(evil-define-key 'normal 'global (kbd "2") 'digit-argument)
(evil-define-key 'normal 'global (kbd "3") 'digit-argument)
(evil-define-key 'normal 'global (kbd "4") 'digit-argument)
(evil-define-key 'normal 'global (kbd "5") 'digit-argument)
(evil-define-key 'normal 'global (kbd "6") 'digit-argument)
(evil-define-key 'normal 'global (kbd "7") 'digit-argument)
(evil-define-key 'normal 'global (kbd "8") 'digit-argument)
(evil-define-key 'normal 'global (kbd "9") 'digit-argument)
(evil-define-key 'normal 'global (kbd "0") 'digit-argument)

;; movement
(evil-define-key 'visual 'global (kbd "k")   'previous-line)
(evil-define-key 'visual 'global (kbd "j")   'next-line)
(evil-define-key 'visual 'global (kbd "l")   'forward-char)
(evil-define-key 'visual 'global (kbd "h")   'backward-char)
(evil-define-key 'normal 'global (kbd "k")   'helix-previous-line)
(evil-define-key 'normal 'global (kbd "j")   'helix-next-line)
(evil-define-key 'normal 'global (kbd "l")   'helix-forward-char)
(evil-define-key 'normal 'global (kbd "h")   'helix-backward-char)

;; insert mode
(evil-define-key 'normal 'global (kbd "i")   'helix-insert)
(evil-define-key 'insert 'global (kbd "ESC") 'evil-normal-state)
(evil-define-key 'normal 'global (kbd "a")   'helix-append)

;; word motions
(evil-define-key 'normal 'global (kbd "e")   'helix-move-next-word-end)
(evil-define-key 'normal 'global (kbd "E")   'helix-move-next-long-word-end)
(evil-define-key 'normal 'global (kbd "b")   'helix-move-previous-word-start)
(evil-define-key 'normal 'global (kbd "B")   'helix-move-previous-long-word-start)
(evil-define-key 'normal 'global (kbd "w")   'helix-move-next-word-start)
(evil-define-key 'normal 'global (kbd "W")   'helix-move-next-long-word-start)

(evil-define-key 'normal 'global (kbd "C")   'helix-add-cursor-below)


(evil-define-key 'normal 'global (kbd "x")   'helix-extend-line-below)
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
(evil-define-key 'normal 'global (kbd "o")   'helix-open-below)
(evil-define-key 'normal 'global (kbd "O")   'helix-open-above)
(evil-define-key 'normal 'global (kbd "G")   'goto-line)
(evil-define-key 'normal 'global (kbd "s")   'mc/mark-all-in-region-regexp)

(evil-define-key 'normal 'global (kbd "v") 'evil-visual-state)
(evil-define-key 'visual 'global (kbd "v") 'evil-normal-state)
(evil-define-key 'visual 'global (kbd "ESC") 'evil-normal-state)
;; (evil-define-key 'normal 'global (kbd "ESC") 'keyboard-quit)
;; (evil-define-key 'normal 'global (kbd "M-x") 'execute-extended-command)
(evil-define-key 'normal 'global (kbd "%") 'mark-whole-buffer)

(define-prefix-command 'helix-goto-prefix-command)
(define-key 'helix-goto-prefix-command (kbd "g") 'beginning-of-buffer)
(define-key 'helix-goto-prefix-command (kbd "e") 'end-of-buffer)
(define-key 'helix-goto-prefix-command (kbd "s") 'beginning-of-line-text)
(define-key 'helix-goto-prefix-command (kbd "h") 'start-of-line)
(define-key 'helix-goto-prefix-command (kbd "l") 'end-of-line)
(define-key 'helix-goto-prefix-command (kbd "n") 'next-buffer)
(define-key 'helix-goto-prefix-command (kbd "p") 'previous-buffer)
(define-key 'helix-goto-prefix-command (kbd ".") 'goto-last-change)
(define-key 'helix-goto-prefix-command (kbd "y") 'lsp-goto-type-definition)
(define-key 'helix-goto-prefix-command (kbd "i") 'lsp-implementation)
(define-key 'helix-goto-prefix-command (kbd "d") 'lsp-find-definition)
(define-key 'helix-goto-prefix-command (kbd "r") 'lsp-find-references)
(evil-define-key 'normal 'global (kbd "g") 'helix-goto-prefix-command)

(evil-define-key 'normal 'global (kbd "mm") 'helix-match-brackets)

(evil-set-leader '(normal) (kbd "SPC"))

(provide 'helix-keybindings)
