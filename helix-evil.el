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
  :tag " <N> "
  ;; :enable (motion)
  (cond
   ((evil-normal-state-p)
    (overwrite-mode -1)
    )
   (t
    )))

(evil-define-state insert
  "Insert state."
  :tag " <I> "
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
  :tag " <E> "
  :message "-- EMACS --"
  :input-method t
  :intercept-esc nil)

(evil-define-state visual
  "Visual state."
  :tag " <V> "
  :message "-- VISUAL --"
  :input-method t
  :intercept-esc nil)

(provide 'helix-evil)
