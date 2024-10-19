(require 'helix-vars)
(require 'helix-core)
(require 'helix-helpers)


(helix-define-state insert
  "Helix INSERT state minor mode."
  :lighter " [I]"
  
  :face helix-insert-cursor
  ;; (if helix-insert-mode
  ;;     (run-hooks 'helix-insert-enter-hook)
  ;;   (when (and helix--insert-pos
  ;;              (or helix-select-on-change
  ;;                  helix-select-on-append
  ;;                  helix-select-on-insert)
  ;;              (not (= (point) helix--insert-pos)))
  ;;     (thread-first
  ;;       (helix--make-selection '(select . transient) helix--insert-pos (point))
  ;;       (helix--select)))
  ;;   (run-hooks 'helix-insert-exit-hook)
  ;;   (setq-local helix--insert-pos nil))

  )

(helix-define-state normal
  "Helix NORMAL state minor mode."
  :lighter " [N]"
  
  :face helix-normal-cursor)

(helix-define-state visual
  "Helix VISUAL state minor mode."
  :lighter " [V]"
  
  :face helix-visual-cursor)

(provide 'helix-states)
;; (defun helix-repeat-pre-hook ())
;; (defun helix-repeat-post-hook ())

;; (defun helix-visual-activate-hook (&optional _command)
;;   "Enable Visual state if the region is activated."
;;   ;; (unless (helix-visual-state-p)
;;   ;;   (helix-with-delay nil
;;   ;;       (post-command-hook nil t "helix-activate-visual-state")
;;   ;;     ;; the activation may only be momentary, so re-check
;;   ;;     ;; in `post-command-hook' before entering Visual state
;;   ;;     (unless (or ;; (helix-visual-state-p)
;;   ;;                 (helix-insert-state-p)
;;   ;;                 (helix-emacs-state-p))
;;   ;;       (when (and (region-active-p)
;;   ;;                  (not deactivate-mark))
;;   ;;         (helix-visual-state))))


;;     )
;; (put 'helix-visual-activate-hook 'permanent-local-hook t)

;; (defun helix-visual-deactivate-hook (&optional command)
;;   "Deactivate the region and restore Transient Mark mode."
;;   (helix-normal-state)
;;   ;; (setq command (or command this-command))
;;   ;; (remove-hook 'deactivate-mark-hook
;;   ;;              #'helix-visual-deactivate-hook t)
;;   ;; (remove-hook 'helix-normal-state-entry-hook
;;   ;;              #'helix-visual-deactivate-hook t)
;;   ;; (cond
;;   ;;  ((and (helix-visual-state-p) command
;;   ;;        (not (helix-get-command-property command :keep-visual)))
;;   ;;   (setq helix-visual-region-expanded nil)
;;   ;;   (helix-exit-visual-state))
;;   ;;  ((not (helix-visual-state-p))
;;   ;;   (helix-active-region -1)
;;   ;;   (helix-restore-transient-mark-mode)))
;;   )
;; (put 'helix-visual-deactivate-hook 'permanent-local-hook t)

;; (helix-define-state normal
;;   "Normal state.
;; AKA \"Command\" state."
;;   :tag " NOR "
;;   ;; :enable (motion)
;;   (cond
;;    ((helix-normal-state-p)
;;     (deactivate-mark)
;;     (overwrite-mode -1))
;;    (t
;;     )))

;; (helix-define-state insert
;;   "Insert state."
;;   :tag " INS "
;;   :cursor (bar . 2)
;;   :message "-- INSERT --"
;;   ;; :entry-hook (helix-start-track-last-insertion)
;;   ;; :exit-hook (helix-cleanup-insert-state helix-stop-track-last-insertion)
;;   :input-method t
;;   (cond
;;    ((helix-insert-state-p)
;;     (deactivate-mark)
;;     (message "insert mode"))
;;    (t
;;     (message "insert mode no more"))))

;; (helix-define-state emacs
;;   "Emacs state."
;;   :tag " EMA "
;;   :message "-- EMACS --"
;;   :input-method t
;;   :intercept-esc nil)

;; (helix-define-state visual
;;   "Visual state."
;;   :tag " SEL "
;;   ;; :enable (motion normal)
;;   :message 'helix-visual-message
;;   (cond
;;    ((helix-visual-state-p)
;;     (when (not (region-active-p))
;; 	(set-mark (point)))
        
		  
;;   ;;   ;; (helix-save-transient-mark-mode)
;;   ;;   (setq select-active-regions nil)
;;   ;;   (cond
;;   ;;    ((region-active-p)
;;   ;;     (if (< (helix-visual-direction) 0)
;;   ;;         (helix-visual-select (region-beginning) (region-end)
;;   ;;                             helix-visual-char
;;   ;;                             (helix-visual-direction))
;;   ;;       (helix-visual-make-selection (mark t) (point)
;;   ;;                                   helix-visual-char))
;;   ;;     (helix-visual-highlight))
;;   ;;    (t
;;   ;;     (helix-visual-make-region (point) (point) helix-visual-char)))
;;   ;;   (add-hook 'pre-command-hook #'helix-visual-pre-command nil t)
;;   ;;   (add-hook 'post-command-hook #'helix-visual-post-command nil t)
;;      (add-hook 'deactivate-mark-hook #'helix-visual-deactivate-hook nil t))
;;     (t
;;   ;;   ;; Postpone deactivation of region if next state is Insert.
;;   ;;   ;; This gives certain insertion commands (auto-pairing characters,
;;   ;;   ;; for example) an opportunity to access the region.
;;   ;;   (if (and (eq helix-next-state 'insert)
;;   ;;            (eq helix-visual-selection 'char))
;;   ;;       (add-hook 'helix-normal-state-entry-hook
;;   ;;                 #'helix-visual-deactivate-hook nil t)
;;   ;;     (helix-visual-deactivate-hook))
;;   ;;   (setq helix-visual-region-expanded nil)
;;   ;;   (remove-hook 'pre-command-hook #'helix-visual-pre-command t)
;;   ;;   (remove-hook 'post-command-hook #'helix-visual-post-command t)
;;     (remove-hook 'deactivate-mark-hook #'helix-visual-deactivate-hook t)))
;;   ;;   (helix-visual-highlight -1)))


;;   )

;; (provide 'helix-states)
