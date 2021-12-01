;;; meow-bmacros.el --- Batch Macro state in Meow  -*- lexical-binding: t; -*-

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
;; The file contains BMACRO state implementation.

;;; Code:

(require 'meow-util)
(require 'kmacro)
(require 'seq)

(declare-function meow-replace "meow-command")
(declare-function meow-insert "meow-command")
(declare-function meow-change "meow-command")
(declare-function meow-append "meow-command")
(declare-function meow-cancel-selection "meow-command")
(declare-function meow--make-selection "meow-command")
(declare-function meow--select "meow-command")
(declare-function meow-bmacro-mode "meow-core")

(defvar-local meow--bmacro-overlays nil)
(defvar-local meow--bmacro-insert-enter-key nil)

(defun meow--bmacro-add-overlay-at-point (pos)
  (let ((ov (make-overlay pos (1+ pos))))
    (overlay-put ov 'face 'meow-bmacro-fake-cursor)
    (overlay-put ov 'meow-bmacro-type 'cursor)
    (push ov meow--bmacro-overlays)))

(defun meow--bmacro-add-overlay-at-region (type p1 p2 backward)
  (let ((ov (make-overlay p1 p2)))
    (overlay-put ov 'face 'meow-bmacro-fake-selection)
    (overlay-put ov 'meow-bmacro-type type)
    (overlay-put ov 'meow-bmacro-backward backward)
    (push ov meow--bmacro-overlays)))

(defun meow--bmacro-remove-overlays ()
  (mapc (lambda (it) (delete-overlay it)) meow--bmacro-overlays)
  (setq meow--bmacro-overlays nil))

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
  (if meow-use-cursor-position-hack
      (let ((m (if (meow--direction-forward-p)
                   (1- (point))
                 (1+ (point))))
            (type (meow--selection-type)))
        (meow-cancel-selection)
        (-> (meow--make-selection '(select . transient) m (point))
          (meow--select)))
    (meow--cancel-selection)))

(defun meow--wrap-kmacro-switch-insert ()
  (setq last-kbd-macro
        (apply #'vector
               meow--bmacro-insert-enter-key
               (append last-kbd-macro
                       ;; in GUI, we append the missing escape to last-kbd-macro
                       ;; in TUI, the escape is already recorded
                       (when (display-graphic-p)
                         '(escape))))))

(defun meow--bmacro-apply-kmacros ()
  (when meow--bmacro-overlays
    (let ((bak (overlay-get (car meow--bmacro-overlays)
                            'meow-bmacro-backward)))
      (meow--wrap-collapse-undo
        (save-mark-and-excursion
          (cl-loop for ov in (if bak (reverse meow--bmacro-overlays) meow--bmacro-overlays) do
                   (when (and (overlayp ov))
                     (let ((type (overlay-get ov 'meow-bmacro-type))
                           (backward (overlay-get ov 'meow-bmacro-backward)))
                       ;; always switch to normal state before applying kmacro
                       (meow--switch-state 'normal)

                       (if (eq type 'cursor)
                           (progn
                             (meow--cancel-selection)
                             (goto-char (overlay-start ov)))
                         (-> (if backward
                                 (meow--make-selection type (overlay-end ov) (overlay-start ov))
                               (meow--make-selection type (overlay-start ov) (overlay-end ov)))
                           (meow--_elect)))

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
                  (meow--bmacro-add-overlay-at-point (meow--hack-cursor-pos (point)))))))

        (save-mark-and-excursion
          (goto-char (point-max))
          (while (forward-word -1)
            (unless (= (point) orig)
              (meow--bmacro-add-overlay-at-point (point))))))))
  (meow--bmacro-shrink-selection))

(defun meow--add-cursors-for-match (match)
  (save-restriction
    (meow--narrow-secondary-selection)
    (let ((orig-end (region-end))
          (orig-beg (region-beginning))
          (back (meow--direction-backward-p)))
      (save-mark-and-excursion
        (goto-char (point-min))
        (let ((case-fold-search nil))
          (while (re-search-forward match nil t)
            (unless (or (= orig-end (point))
                        (= orig-beg (point)))
              (let ((match (match-data)))
                (meow--bmacro-add-overlay-at-region
                 '(select . visit)
                 (car match)
                 (cadr match)
                 back)))))))))

(defun meow--bmacro-count-lines (beg end)
  (if (and (= (point) (line-beginning-position))
           (meow--direction-forward-p))
      (1+ (count-lines beg end))
    (count-lines beg end)))

(defun meow--bmacro-forward-line (n bound)
  (cond
   ((> n 0)
    (when (> n 1) (forward-line (1- n)))
    (unless (<= bound (line-end-position))
      (forward-line 1)))
   ((< n 0)
    (when (< n -1) (forward-line (+ n 1)))
    (unless (>= bound (line-beginning-position))
      (forward-line -1)))
   (t
    (not (= (point) bound)))))

(defun meow--add-cursors-for-line ()
  (save-restriction
    (meow--narrow-secondary-selection)
    (let* ((beg (region-beginning))
           (end (region-end))
           (ln (meow--bmacro-count-lines beg end))
           (back (meow--direction-backward-p))
           prev)
      (save-mark-and-excursion
        (goto-char end)
        (forward-line (if back -1 1))
        (setq prev (point))
        (while (meow--bmacro-forward-line
                (1- ln)
                (point-max))
          (meow--bmacro-add-overlay-at-region
           '(select . line)
           prev
           (line-end-position)
           back)
          (forward-line 1)
          (setq prev (point))))
      (save-mark-and-excursion
        (goto-char (point-min))
        (setq prev (point))
        (while (meow--bmacro-forward-line
                (1- ln)
                beg)
          (meow--bmacro-add-overlay-at-region
           '(select . line)
           prev
           (line-end-position)
           back)
          (forward-line 1)
          (setq prev (point)))))))

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
                  (meow--bmacro-add-overlay-at-point (meow--hack-cursor-pos (point))))))
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
                    (meow--bmacro-add-overlay-at-point (meow--hack-cursor-pos (1- (point))))))))
          (save-mark-and-excursion
            (goto-char (point-max))
            (while (search-backward ch-str nil t)
              (unless (or (= orig (1+ (point)))
                          (= (point) (point-max)))
                (meow--bmacro-add-overlay-at-point (1+ (point)))))))))
    (meow--bmacro-shrink-selection)))

(defun meow--bmacro-region-words-to-match ()
  (format "\\<%s\\>"
          (regexp-quote
           (buffer-substring-no-properties
            (region-beginning)
            (region-end)))))

(defun meow--bmacro-update-overlays ()
  (meow--bmacro-remove-overlays)
  (when (meow--bmacro-inside-secondary-selection)
    (-let* (((ex . type) (meow--selection-type)))
      (cl-case type
        ((nil transient) (meow--add-cursors-for-char))
        ((word) (if (not (eq 'expand ex))
                    (meow--add-cursors-for-word)
                  (meow--add-cursors-for-match (meow--bmacro-region-words-to-match))))
        ((visit) (meow--add-cursors-for-match (car regexp-search-ring)))
        ((line) (meow--add-cursors-for-line))
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
  (setq-local meow--bmacro-insert-enter-key nil)
  (let ((map (make-sparse-keymap)))
    (define-key map [remap kmacro-end-or-call-macro] 'meow-bmacro-end-and-apply-kmacro)
    (set-transient-map map (lambda () defining-kbd-macro))))

(defun meow-bmacro-insert-exit ()
  "Exit insert mode and terminate kmacro recording."
  (interactive)
  (when defining-kbd-macro
    (end-kbd-macro)
    (meow--wrap-kmacro-switch-insert)
    (meow--bmacro-apply-kmacros))
  (meow--switch-state 'bmacro))

(defun meow-bmacro-insert ()
  "Insert and start kmacro recording.

Will terminate recording when exit insert mode.
The recorded kmacro will be applied to all cursors immediately."
  (interactive)
  (meow-bmacro-mode -1)
  (meow-insert)
  (call-interactively #'kmacro-start-macro)
  (setq-local meow--bmacro-insert-enter-key last-input-event)
  (let ((map (make-sparse-keymap)))
    (define-key map [remap meow-insert-exit] 'meow-bmacro-insert-exit)
    (set-transient-map map (lambda () defining-kbd-macro))))

(defun meow-bmacro-append ()
  "Append and start kmacro recording.

Will terminate recording when exit insert mode.
The recorded kmacro will be applied to all cursors immediately."
  (interactive)
  (meow-bmacro-mode -1)
  (meow-append)
  (call-interactively #'kmacro-start-macro)
  (setq-local meow--bmacro-insert-enter-key last-input-event)
  (let ((map (make-sparse-keymap)))
    (define-key map [remap meow-insert-exit] 'meow-bmacro-insert-exit)
    (set-transient-map map (lambda () defining-kbd-macro))))

(defun meow-bmacro-change ()
  "Change and start kmacro recording.

Will terminate recording when exit insert mode.
The recorded kmacro will be applied to all cursors immediately."
  (interactive)
  (meow--with-selection-fallback
   (meow-bmacro-mode -1)
   (meow-change)
   (call-interactively #'kmacro-start-macro)
   (setq-local meow--bmacro-insert-enter-key last-input-event)
   (let ((map (make-sparse-keymap)))
     (define-key map [remap meow-insert-exit] 'meow-bmacro-insert-exit)
     (set-transient-map map (lambda () defining-kbd-macro)))))

(defun meow-bmacro-change-char ()
  "Change and start kmacro recording.

Will terminate recording when exit insert mode.
The recorded kmacro will be applied to all cursors immediately."
  (interactive)
  (meow-bmacro-mode -1)
  (meow-change-char)
  (call-interactively #'kmacro-start-macro)
  (setq-local meow--bmacro-insert-enter-key last-input-event)
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
                          (not (eq 'cursor (overlay-get ov 'meow-bmacro-type))))
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
