(require 'evil-common)
(require 'evil-core)

(defun evil-repeat-pre-hook ())
(defun evil-repeat-post-hook ())

(defun evil-visual-activate-hook (&optional _command)
  "Enable Visual state if the region is activated."
  ;; (unless (evil-visual-state-p)
  ;;   (evil-with-delay nil
  ;;       (post-command-hook nil t "evil-activate-visual-state")
  ;;     ;; the activation may only be momentary, so re-check
  ;;     ;; in `post-command-hook' before entering Visual state
  ;;     (unless (or ;; (evil-visual-state-p)
  ;;                 (evil-insert-state-p)
  ;;                 (evil-emacs-state-p))
  ;;       (when (and (region-active-p)
  ;;                  (not deactivate-mark))
  ;;         (evil-visual-state))))


    )
(put 'evil-visual-activate-hook 'permanent-local-hook t)

(defun evil-visual-deactivate-hook (&optional command)
  "Deactivate the region and restore Transient Mark mode."
  (evil-normal-state)
  ;; (setq command (or command this-command))
  ;; (remove-hook 'deactivate-mark-hook
  ;;              #'evil-visual-deactivate-hook t)
  ;; (remove-hook 'evil-normal-state-entry-hook
  ;;              #'evil-visual-deactivate-hook t)
  ;; (cond
  ;;  ((and (evil-visual-state-p) command
  ;;        (not (evil-get-command-property command :keep-visual)))
  ;;   (setq evil-visual-region-expanded nil)
  ;;   (evil-exit-visual-state))
  ;;  ((not (evil-visual-state-p))
  ;;   (evil-active-region -1)
  ;;   (evil-restore-transient-mark-mode)))
  )
(put 'evil-visual-deactivate-hook 'permanent-local-hook t)

(evil-define-state normal
  "Normal state.
AKA \"Command\" state."
  :tag " NOR "
  ;; :enable (motion)
  (cond
   ((evil-normal-state-p)
    (deactivate-mark)
    (overwrite-mode -1))
   (t
    )))

(evil-define-state insert
  "Insert state."
  :tag " INS "
  :cursor (bar . 2)
  :message "-- INSERT --"
  ;; :entry-hook (evil-start-track-last-insertion)
  ;; :exit-hook (evil-cleanup-insert-state evil-stop-track-last-insertion)
  :input-method t
  (cond
   ((evil-insert-state-p)
    (deactivate-mark)
    (message "insert mode"))
   (t
    (message "insert mode no more"))))

(evil-define-state emacs
  "Emacs state."
  :tag " EMA "
  :message "-- EMACS --"
  :input-method t
  :intercept-esc nil)

(evil-define-state visual
  "Visual state."
  :tag " SEL "
  ;; :enable (motion normal)
  :message 'evil-visual-message
  (cond
   ((evil-visual-state-p)
    (when (not (region-active-p))
	(set-mark (point)))
        
		  
  ;;   ;; (evil-save-transient-mark-mode)
  ;;   (setq select-active-regions nil)
  ;;   (cond
  ;;    ((region-active-p)
  ;;     (if (< (evil-visual-direction) 0)
  ;;         (evil-visual-select (region-beginning) (region-end)
  ;;                             evil-visual-char
  ;;                             (evil-visual-direction))
  ;;       (evil-visual-make-selection (mark t) (point)
  ;;                                   evil-visual-char))
  ;;     (evil-visual-highlight))
  ;;    (t
  ;;     (evil-visual-make-region (point) (point) evil-visual-char)))
  ;;   (add-hook 'pre-command-hook #'evil-visual-pre-command nil t)
  ;;   (add-hook 'post-command-hook #'evil-visual-post-command nil t)
     (add-hook 'deactivate-mark-hook #'evil-visual-deactivate-hook nil t))
    (t
  ;;   ;; Postpone deactivation of region if next state is Insert.
  ;;   ;; This gives certain insertion commands (auto-pairing characters,
  ;;   ;; for example) an opportunity to access the region.
  ;;   (if (and (eq evil-next-state 'insert)
  ;;            (eq evil-visual-selection 'char))
  ;;       (add-hook 'evil-normal-state-entry-hook
  ;;                 #'evil-visual-deactivate-hook nil t)
  ;;     (evil-visual-deactivate-hook))
  ;;   (setq evil-visual-region-expanded nil)
  ;;   (remove-hook 'pre-command-hook #'evil-visual-pre-command t)
  ;;   (remove-hook 'post-command-hook #'evil-visual-post-command t)
    (remove-hook 'deactivate-mark-hook #'evil-visual-deactivate-hook t)))
  ;;   (evil-visual-highlight -1)))


  )

(provide 'helix-evil)
