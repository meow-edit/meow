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

(helix-define-state motion
  "Helix MOTION state minor mode."
  :lighter " [M]"
  
  :face helix-motion-cursor)

(helix-define-state visual
  "Helix VISUAL state minor mode."
  :lighter " [V]"
  
  :face helix-visual-cursor)

(provide 'helix-states)
