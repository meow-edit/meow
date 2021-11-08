(require 'meow-util)

(defvar meow--bmacro-overlays nil)

(defun meow--bmacro-add-overlay-at-point (pos)
  (let ((ov (make-overlay pos (1+ pos))))
    (overlay-put ov 'face 'meow-multi-cursor)
    (overlay-put ov 'meow-bmacro-type 'cursor)
    (push ov meow--bmacro-overlays)))

(defun meow--bmacro-add-overlay-at-region (type p1 p2 backward)
  (let ((ov (make-overlay p1 p2)))
    (overlay-put ov 'face 'meow-multi-cursor)
    (overlay-put ov 'meow-bmacro-type type)
    (overlay-put ov 'meow-bmacro-backward backward)
    (push ov meow--bmacro-overlays)))

(defun meow--bmacro-remove-overlays ()
  (mapc (lambda (it) (delete-overlay it)) meow--bmacro-overlays))

(defun meow--maybe-toggle-cursor-mode ()
  (unless (or defining-kbd-macro executing-kbd-macro)
    (let ((inside (meow--bmacro-inside-secondary-selection)))
      (cond
       ((and (meow-normal-mode-p)
             inside)
        (meow--switch-state 'bmacro)
        (meow--bmacro-update-overlays))
       ((and (meow-bmacro-mode-p))
        (if inside
            (meow--bmacro-update-overlays)
          (meow--bmacro-remove-overlays)
          (meow--switch-state 'normal)))))))

(defun meow--bmacro-shrink-selection ()
  (let ((m (if (meow--direction-forward-p)
               (setq m (1- (point)))
             (setq m (1+ (point)))))
        (type (meow--selection-type)))
    (meow-cancel-selection)
    (-> (meow--make-selection '(select . transient) m (point))
      (meow--select))))

(defun meow--bmacro-apply-kmacros (&optional wrap-behavior)
  (when meow--bmacro-overlays
    (let ((bak (overlay-get (car meow--bmacro-overlays)
                            'meow-bmacro-backward)))
      (meow--wrap-collapse-undo
        (save-mark-and-excursion
          (cl-loop for ov in (if bak
                                 (reverse meow--bmacro-overlays)
                               meow--bmacro-overlays) do
                   (when (and (overlayp ov) (overlay-start ov))
                     (let ((type (overlay-get ov 'meow-bmacro-type))
                           (backward (overlay-get ov 'meow-bmacro-backward)))
                       ;; always switch to normal state before applying kmacro
                       (meow--switch-state 'normal)

                       (if (eq type 'cursor)
                           (goto-char (overlay-start ov))
                         (-> (if backward
                                 (meow--make-selection type (overlay-end ov) (overlay-start ov))
                               (meow--make-selection type (overlay-start ov) (overlay-end ov)))
                           (meow--select)))

                       (cl-case wrap-behavior
                         ((insert) (meow-insert))
                         ((append) (meow-append))
                         ((change) (meow-change)))

                       (call-interactively 'kmacro-call-macro)))))))))

(defun meow--add-cursors-for-char ()
  (save-restriction
    (meow--narrow-secondary-selection)
    (let ((curr (point))
          (col (- (point) (line-beginning-position)))
          break)
      (save-mark-and-excursion
        (while (< (line-end-position) (point-max))
          (forward-line 1)
          (let ((pos (+ col (line-beginning-position))))
            (when (<= pos (line-end-position))
              (meow--bmacro-add-overlay-at-point pos)))))
      (save-mark-and-excursion
        (goto-char (point-min))
        (while (not break)
          (if (>= (line-end-position) curr)
              (setq break t)
            (let ((pos (+ col (line-beginning-position))))
              (when (<= pos (line-end-position))
                (meow--bmacro-add-overlay-at-point pos)))
            (forward-line 1))))))
  (setq meow--bmacro-overlays (reverse meow--bmacro-overlays))
  (meow--cancel-selection))

(defun meow--add-cursors-for-word ()
  (save-restriction
    (meow--narrow-secondary-selection)
    (let ((orig (point)))
      (if (meow--direction-forward-p)
          ;; forward direction, add cursors at words' end
          (progn
            (save-mark-and-excursion
              (goto-char (point-min))
              (while (forward-word 1)
                (unless (= (point) orig)
                  (meow--bmacro-add-overlay-at-point (1- (point)))))))

        (save-mark-and-excursion
          (goto-char (point-max))
          (while (forward-word -1)
            (unless (= (point) orig)
              (meow--bmacro-add-overlay-at-point (point))))))))
  (meow--bmacro-shrink-selection))

(defun meow--add-cursors-for-match ()
  (save-restriction
    (meow--narrow-secondary-selection)
    (let ((orig-end (region-end))
          (orig-beg (region-beginning)))
      (save-mark-and-excursion
        (goto-char (point-min))
        (while (re-search-forward (car regexp-search-ring) nil t)
          (unless (or (= orig-end (point))
                      (= orig-beg (point)))
            (let ((match (match-data)))
              (meow--bmacro-add-overlay-at-region
               '(select . visit)
               (car match)
               (cadr match)
               (meow--direction-backward-p)))))))))

(defun meow--add-cursors-for-line ()
  (save-restriction
    (meow--narrow-secondary-selection)
    (let ((curr (line-end-position)))
      (save-mark-and-excursion
        (while (< (line-end-position) (point-max))
          (forward-line 1)
          (meow--bmacro-add-overlay-at-region
           '(select . line)
           (line-beginning-position)
           (line-end-position)
           (meow--direction-backward-p))))
      (save-mark-and-excursion
        (goto-char (point-min))
        (let (break)
          (while (not break)
            (if (>= (line-end-position) curr)
                (setq break t)
              (meow--bmacro-add-overlay-at-region
               '(select . line)
               (line-beginning-position)
               (line-end-position)
               (meow--direction-backward-p))
              (forward-line 1))))))))

(defun meow--add-cursors-for-join ()
  (save-restriction
    (meow--narrow-secondary-selection)
    (let ((orig (point)))
      (save-mark-and-excursion
        (goto-char (point-min))
        (back-to-indentation)
        (unless (= (point) orig)
          (meow--bmacro-add-overlay-at-point (point)))
        (while (< (line-end-position) (point-max))
          (forward-line 1)
          (back-to-indentation)
          (unless (= (point) orig)
            (meow--bmacro-add-overlay-at-point (point))))))
    (meow--cancel-selection)))

(defun meow--add-cursors-for-find ()
  (let ((ch-str (if (eq meow--last-find 13)
                   "\n"
                 (char-to-string meow--last-find))))
    (save-restriction
      (meow--narrow-secondary-selection)
      (let ((orig (point))
            (case-fold-search nil))
        (if (meow--direction-forward-p)
            (save-mark-and-excursion
              (goto-char (point-min))
              (while (search-forward ch-str nil t)
                (unless (= orig (point))
                  (meow--bmacro-add-overlay-at-point (1- (point))))))
          (save-mark-and-excursion
              (goto-char (point-max))
              (while (search-backward ch-str nil t)
                (unless (= orig (point))
                  (meow--bmacro-add-overlay-at-point (point))))))))
    (meow--bmacro-shrink-selection)))

(defun meow--add-cursors-for-till ()
  (let ((ch-str (if (eq meow--last-till 13)
                    "\n"
                  (char-to-string meow--last-till))))
    (save-restriction
      (meow--narrow-secondary-selection)
      (let ((orig (point))
            (case-fold-search nil))
        (if (meow--direction-forward-p)
            (progn
              (save-mark-and-excursion
                (goto-char (point-min))
                (while (search-forward ch-str nil t)
                  (unless (or (= orig (1- (point)))
                              (zerop (- (point) 2)))
                    (meow--bmacro-add-overlay-at-point (- (point) 2))))))
          (save-mark-and-excursion
            (goto-char (point-max))
            (while (search-backward ch-str nil t)
              (unless (or (= orig (1+ (point)))
                          (= (point) (point-max)))
                (meow--bmacro-add-overlay-at-point (1+ (point)))))))))
    (meow--bmacro-shrink-selection)))

(defun meow--bmacro-remove-expand-type ()
  (setq meow--selection
        (cons (cons 'select (cdar meow--selection))
              (cdr meow--selection))))

(defun meow--bmacro-update-overlays ()
  (meow--bmacro-remove-overlays)
  (when (meow--bmacro-inside-secondary-selection)
    (-let* (((ex . type) (meow--selection-type)))
      (cl-case type
        ((nil transient) (meow--add-cursors-for-char))
        ((word) (if (not (eq 'expand ex))
                    (meow--add-cursors-for-word)
                  ;; change type to match
                  (setq meow--selection
                        (cons '(select . visit)
                              (cdr meow--selection)))
                  (meow--add-cursors-for-match)))
        ((visit) (meow--add-cursors-for-match))
        ((line)
         (meow--bmacro-remove-expand-type)
         (meow--add-cursors-for-line))
        ((join) (meow--add-cursors-for-join))
        ((find) (meow--add-cursors-for-find))
        ((till) (meow--add-cursors-for-till))))))

(defun meow-bmacro-end-and-apply-kmacro ()
  (interactive)
  (call-interactively #'kmacro-end-macro)
  (meow--bmacro-apply-kmacros))

(defun meow-bmacro-start ()
  "Start kmacro recording, apply to all cursors when terminate."
  (interactive)
  (meow--switch-state 'normal)
  (call-interactively 'kmacro-start-macro)
  (setq-local meow--bmacro-wrap-behavior nil)
  (let ((map (make-sparse-keymap)))
    (define-key map [remap kmacro-end-or-call-macro] 'meow-bmacro-end-and-apply-kmacro)
    (set-transient-map map (lambda () defining-kbd-macro))))

(defun meow-bmacro-insert-exit ()
  "Exit insert mode and terminate kmacro recording."
  (interactive)
  (when defining-kbd-macro
    (end-kbd-macro)
    (meow--bmacro-apply-kmacros meow--bmacro-wrap-behavior)
    ;; discard this macro
    (setq last-kbd-macro (car (kmacro-pop-ring1))))
  (meow--switch-state 'bmacro))

(defun meow-bmacro-insert ()
  "Insert and start kmacro recording, will terminate recording when exit insert mode. The recorded kmacro will be applied to all cursors immediately."
  (interactive)
  (meow-bmacro-mode -1)
  (meow-insert)
  (call-interactively #'kmacro-start-macro)
  (setq-local meow--bmacro-wrap-behavior 'insert)
  (let ((map (make-sparse-keymap)))
    (define-key map [remap meow-insert-exit] 'meow-bmacro-insert-exit)
    (set-transient-map map (lambda () defining-kbd-macro))))

(defun meow-bmacro-append ()
  "Append and start kmacro recording, will terminate recording when exit insert mode. The recorded kmacro will be applied to all cursors immediately."
  (interactive)
  (meow-bmacro-mode -1)
  (meow-append)
  (call-interactively #'kmacro-start-macro)
  (setq-local meow--bmacro-wrap-behavior 'append)
  (let ((map (make-sparse-keymap)))
    (define-key map [remap meow-insert-exit] 'meow-bmacro-insert-exit)
    (set-transient-map map (lambda () defining-kbd-macro))))

(defun meow-bmacro-change ()
  "Change and start kmacro recording, will terminate recording when exit insert mode. The recorded kmacro will be applied to all cursors immediately."
  (interactive)
  (meow-bmacro-mode -1)
  (meow-change)
  (call-interactively #'kmacro-start-macro)
  (setq-local meow--bmacro-wrap-behavior 'change)
  (let ((map (make-sparse-keymap)))
    (define-key map [remap meow-insert-exit] 'meow-bmacro-insert-exit)
    (set-transient-map map (lambda () defining-kbd-macro))))

(defun meow-bmacro-replace ()
  "Replace all selection cursors with current kill-ring head."
  (interactive)
  (meow--wrap-collapse-undo
    (meow-replace)
    (save-mark-and-excursion
      (cl-loop for ov in meow--bmacro-overlays do
               (when (and (overlayp ov)
                          (not (eq 'cursor (overlay-get ov 'meow-bmacro-type)))
                          (overlay-start ov))
                 (goto-char (overlay-start ov))
                 (push-mark (overlay-end ov) t)
                 (meow-replace)
                 (delete-overlay ov))))))

(defun meow-bmacro-apply-kmacro ()
  (interactive)
  (meow--switch-state 'normal)
  (call-interactively #'kmacro-call-macro)
  (meow--bmacro-apply-kmacros)
  (meow--switch-state 'bmacro))

(defun meow-bmacro-noop ()
  "Noop, to disable some keybindings in cursor state."
  (interactive))

(defun meow-bmacro-disallow-keypad-start ()
  "This command used to disallow user start keypad state directly in bmacro state."
  (interactive)
  (message "Can't start keypad in bmacro state"))

(provide 'meow-bmacro)
;;; meow-bmacro.el ends here
