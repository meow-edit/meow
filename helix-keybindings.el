(require 'helix-states)

(define-key helix-normal-mode-map (kbd "1") 'digit-argument)
(define-key helix-normal-mode-map (kbd "2") 'digit-argument)
(define-key helix-normal-mode-map (kbd "3") 'digit-argument)
(define-key helix-normal-mode-map (kbd "4") 'digit-argument)
(define-key helix-normal-mode-map (kbd "5") 'digit-argument)
(define-key helix-normal-mode-map (kbd "6") 'digit-argument)
(define-key helix-normal-mode-map (kbd "7") 'digit-argument)
(define-key helix-normal-mode-map (kbd "8") 'digit-argument)
(define-key helix-normal-mode-map (kbd "9") 'digit-argument)
(define-key helix-normal-mode-map (kbd "0") 'digit-argument)

;; ;; movement
(define-key helix-normal-mode-map (kbd "k") 'previous-line)
(define-key helix-normal-mode-map (kbd "j") 'next-line)
(define-key helix-normal-mode-map (kbd "l") 'forward-char)
(define-key helix-normal-mode-map (kbd "h") 'backward-char)

;; ;; insert mode


(define-key helix-normal-mode-map (kbd "i") 'helix-insert)
(define-key helix-normal-mode-map (kbd "a") 'helix-append)

(define-key helix-insert-mode-map (kbd "<escape>") 'helix-normal-mode)

;; ;; word motions
(define-key helix-normal-mode-map (kbd "e") 'helix-move-next-word-end)
(define-key helix-normal-mode-map (kbd "E") 'helix-move-next-long-word-end)
(define-key helix-normal-mode-map (kbd "b") 'helix-move-previous-word-start)
(define-key helix-normal-mode-map (kbd "B") 'helix-move-previous-long-word-start)
(define-key helix-normal-mode-map (kbd "w") 'helix-move-next-word-start)
(define-key helix-normal-mode-map (kbd "W") 'helix-move-next-long-word-start)
(define-key helix-normal-mode-map (kbd "f") 'helix-find-char)
(define-key helix-normal-mode-map (kbd "t") 'helix-find-till-char)
(define-key helix-normal-mode-map (kbd "F") 'helix-find-previous-char)
(define-key helix-normal-mode-map (kbd "T") 'helix-find-till-previous-char)
(define-key helix-normal-mode-map (kbd "G") 'goto-line)

(define-key helix-normal-mode-map (kbd "\"") 'helix-select-register)
(define-key helix-normal-mode-map (kbd "q")  'helix-start-stop-kmacro-to-register)
(define-key helix-normal-mode-map (kbd "Q")  'helix-call-kmacro-from-register)


(define-key helix-normal-mode-map (kbd "x") 'helix-extend-line-below)
(define-key helix-normal-mode-map (kbd "d") 'helix-delete-region-or-char)
(define-key helix-normal-mode-map (kbd "p") 'yank)
(define-key helix-normal-mode-map (kbd "y") 'helix-kill-ring-save)
(define-key helix-normal-mode-map (kbd "u") 'undo)
(define-key helix-normal-mode-map (kbd "U") 'redo)
;; ;; (helix-define-key 'normal 'global (kbd "q")   'helix-toggle-kmacro-recording)
;; ;; (helix-define-key 'normal 'global (kbd "Q")   'kmacro-call-macro)
(define-key helix-normal-mode-map (kbd ";") 'helix-collapse-region)
(define-key helix-normal-mode-map (kbd "f") 'helix-find-char)
(define-key helix-normal-mode-map (kbd "c") 'helix-change)
(define-key helix-normal-mode-map (kbd "o") 'helix-open-below)
(define-key helix-normal-mode-map (kbd "O") 'helix-open-above)

(define-key helix-normal-mode-map (kbd "s") 'mc/mark-all-in-region)
(define-key helix-normal-mode-map (kbd "S") 'todo)
(define-key helix-normal-mode-map (kbd "_") 'todo)
(define-key helix-normal-mode-map (kbd ";") 'todo)
(define-key helix-normal-mode-map (kbd ",") 'todo)
(define-key helix-normal-mode-map (kbd "(") 'todo)
(define-key helix-normal-mode-map (kbd ")") 'todo)

(define-key helix-normal-mode-map (kbd "/") 'search-forward-regexp)
(define-key helix-normal-mode-map (kbd "?") 'search-backward-regexp)
(define-key helix-normal-mode-map (kbd "n") 'todo)
(define-key helix-normal-mode-map (kbd "N") 'todo)

(define-key helix-normal-mode-map (kbd "C") 'helix-add-cursor-below)


;; (helix-define-key 'normal 'global (kbd "v") 'helix-visual-state)
;; (helix-define-key 'visual 'global (kbd "v") 'helix-normal-state)
;; (helix-define-key 'visual 'global (kbd "<escape>") 'helix-normal-state)
;; ;; (helix-define-key 'normal 'global (kbd "ESC") 'keyboard-quit)
;; ;; (helix-define-key 'normal 'global (kbd "M-x") 'execute-extended-command)
(define-key helix-normal-mode-map (kbd "%") 'mark-whole-buffer)

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
;; (helix-define-key (list 'normal 'visual) 'global (kbd "g") 'helix-goto-prefix-command)

(define-key helix-normal-mode-map (kbd "g") 'helix-goto-prefix-command)

(define-prefix-command 'helix-leader-prefix-command)
(define-key 'helix-leader-prefix-command (kbd "g") 'magit)

(define-key helix-normal-mode-map (kbd "SPC") 'helix-leader-prefix-command)
            
;; (helix-define-key 'normal 'global (kbd "mm") 'helix-match-brackets)

;; (helix-set-leader '(normal visual) (kbd "SPC"))

(provide 'helix-keybindings)
