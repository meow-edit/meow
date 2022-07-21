;;; meow-beacons.el --- Batch Macro state in Meow  -*- lexical-binding: t; -*-

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
;; The file contains BEACON state implementation.

;;; Code:

(require 'meow-util)
(require 'meow-var)
(require 'kmacro)
(require 'seq)

(declare-function meow-replace "meow-command")
(declare-function meow-insert "meow-command")
(declare-function meow-change "meow-command")
(declare-function meow-change-char "meow-command")
(declare-function meow-append "meow-command")
(declare-function meow-kill "meow-command")
(declare-function meow--cancel-selection "meow-command")
(declare-function meow--selection-fallback "meow-command")
(declare-function meow--make-selection "meow-command")
(declare-function meow--select "meow-command")
(declare-function meow-beacon-mode "meow-core")

(defvar-local meow--beacon-overlays nil)
(defvar-local meow--beacon-insert-enter-key nil)

(defun meow--beacon-add-overlay-at-point (pos)
  "Create an overlay to draw a fake cursor as beacon at POS."
  (let ((ov (make-overlay pos (1+ pos))))
    (overlay-put ov 'face 'meow-beacon-fake-cursor)
    (overlay-put ov 'meow-beacon-type 'cursor)
    (push ov meow--beacon-overlays)))

(defun meow--beacon-add-overlay-at-region (type p1 p2 backward)
  "Create an overlay to draw a fake selection as beacon from P1 to 12.

TYPE is used for selection type.
Non-nil BACKWARD means backward direction."
  (let ((ov (make-overlay p1 p2)))
    (overlay-put ov 'face 'meow-beacon-fake-selection)
    (overlay-put ov 'meow-beacon-type type)
    (overlay-put ov 'meow-beacon-backward backward)
    (push ov meow--beacon-overlays)))

(defun meow--beacon-remove-overlays ()
  "Remove all beacon overlays from current buffer."
  (mapc #'delete-overlay meow--beacon-overlays)
  (setq meow--beacon-overlays nil))

(defun meow--maybe-toggle-beacon-state ()
  "Maybe switch to BEACON state."
  (unless (or defining-kbd-macro executing-kbd-macro)
    (let ((inside (meow--beacon-inside-secondary-selection)))
      (cond
       ((and (meow-normal-mode-p)
             inside)
        (meow--switch-state 'beacon)
        (meow--beacon-update-overlays))
       ((meow-beacon-mode-p)
        (if inside
            (meow--beacon-update-overlays)
          (meow--beacon-remove-overlays)
          (meow--switch-state 'normal)))))))

(defun meow--beacon-shrink-selection ()
  "Shrink selection to one char width."
  (if meow-use-cursor-position-hack
      (let ((m (if (meow--direction-forward-p)
                   (1- (point))
                 (1+ (point)))))
        (meow--cancel-selection)
        (thread-first
          (meow--make-selection '(select . transient) m (point))
          (meow--select)))
    (meow--cancel-selection)))

(defun meow--beacon-apply-command (cmd)
  "Apply CMD in BEACON state."
  (when meow--beacon-overlays
    (let ((bak (overlay-get (car meow--beacon-overlays)
                            'meow-beacon-backward)))
      (meow--wrap-collapse-undo
        (save-mark-and-excursion
          (cl-loop for ov in (if bak (reverse meow--beacon-overlays) meow--beacon-overlays) do
                   (when (and (overlayp ov))
                     (let ((type (overlay-get ov 'meow-beacon-type))
                           (backward (overlay-get ov 'meow-beacon-backward)))
                       ;; always switch to normal state before applying kmacro
                       (meow--switch-state 'normal)

                       (if (eq type 'cursor)
                           (progn
                             (meow--cancel-selection)
                             (goto-char (overlay-start ov)))
                         (thread-first
                           (if backward
                               (meow--make-selection
                                type (overlay-end ov) (overlay-start ov))
                             (meow--make-selection type (overlay-start ov) (overlay-end ov)))
                           (meow--select)))

                       (call-interactively cmd))
                     (delete-overlay ov))))))))

(defun meow--beacon-apply-kmacros-from-insert ()
  "Apply kmacros in BEACON state, after exiting from insert.

This is treated separately beacuse we must enter each insert state the
same way, and escape ecah time the macro is applied."
  (meow--beacon-apply-command (lambda ()
                                (interactive)
                                (meow--execute-kbd-macro
                                 (key-description
                                  (vector meow--beacon-insert-enter-key)))
                                (call-interactively #'kmacro-call-macro)
                                (meow-escape-or-normal-modal))))

(defun meow--beacon-apply-kmacros ()
  "Apply kmacros in BEACON state."
  (meow--beacon-apply-command 'kmacro-call-macro))

(defun meow--add-beacons-for-char ()
  "Add beacon for char movement."
  (save-restriction
    (let* ((bounds (meow--second-sel-bound))
           (beg (car bounds))
           (end (cdr bounds))
           (curr (point))
           (col (- (point) (line-beginning-position)))
           break)
      (save-mark-and-excursion
        (while (< (line-end-position) end)
          (forward-line 1)
          (let ((pos (+ col (line-beginning-position))))
            (when (<= pos (min end (line-end-position)))
              (meow--beacon-add-overlay-at-point pos)))))
      (save-mark-and-excursion
        (goto-char beg)
        (while (not break)
          (if (>= (line-end-position) curr)
              (setq break t)
            (let ((pos (+ col (line-beginning-position))))
              (when (and
                     (>= pos beg)
                     (<= pos (line-end-position)))
                (meow--beacon-add-overlay-at-point pos)))
            (forward-line 1))))))
  (setq meow--beacon-overlays (reverse meow--beacon-overlays))
  (meow--cancel-selection))

(defun meow--add-beacons-for-char-expand ()
  "Add beacon for char expand movement."
  (save-restriction
    (let* ((bounds (meow--second-sel-bound))
           (ss-beg (car bounds))
           (ss-end (cdr bounds))
           (curr (point))
           (bak (meow--direction-backward-p))
           (beg-col (- (region-beginning) (line-beginning-position)))
           (end-col (- (region-end) (line-beginning-position)))
           break)
      (save-mark-and-excursion
        (while (< (line-end-position) ss-end)
          (forward-line 1)
          (let ((beg (+ beg-col (line-beginning-position)))
                (end (+ end-col (line-beginning-position))))
            (when (<= end (min ss-end (line-end-position)))
              (meow--beacon-add-overlay-at-region
               '(expand . char)
               beg
               end
               bak)))))
      (save-mark-and-excursion
        (goto-char ss-beg)
        (while (not break)
          (if (>= (line-end-position) curr)
              (setq break t)
            (let ((beg (+ beg-col (line-beginning-position)))
                  (end (+ end-col (line-beginning-position))))
              (when (and
                     (>= beg ss-beg)
                     (<= end (line-end-position)))
                (meow--beacon-add-overlay-at-region
                 '(expand . char)
                 beg
                 end
                 bak)))
            (forward-line 1)))))
    (setq meow--beacon-overlays (reverse meow--beacon-overlays))))

(defun meow--add-beacons-for-word ()
  "Add beacon for word movement."
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
                  (meow--beacon-add-overlay-at-point (meow--hack-cursor-pos (point)))))))

        (save-mark-and-excursion
          (goto-char (point-max))
          (while (forward-word -1)
            (unless (= (point) orig)
              (meow--beacon-add-overlay-at-point (point))))))))
  (meow--beacon-shrink-selection))

(defun meow--add-beacons-for-match (match)
  "Add beacon for match(mark, visit or search).

MATCH is the search regexp."
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
                (meow--beacon-add-overlay-at-region
                 '(select . visit)
                 (car match)
                 (cadr match)
                 back)))))
        (setq meow--beacon-overlays (reverse meow--beacon-overlays))))))

(defun meow--beacon-count-lines (beg end)
  "Count selected lines from BEG to END."
  (if (and (= (point) (line-beginning-position))
           (meow--direction-forward-p))
      (1+ (count-lines beg end))
    (count-lines beg end)))

(defun meow--beacon-forward-line (n bound)
  "Forward N line, inside BOUND."
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

(defun meow--add-beacons-for-line ()
  "Add beacon for line movement."
  (save-restriction
    (meow--narrow-secondary-selection)
    (let* ((beg (region-beginning))
           (end (region-end))
           (ln (meow--beacon-count-lines beg end))
           (back (meow--direction-backward-p))
           prev)
      (save-mark-and-excursion
        (goto-char end)
        (forward-line (if back -1 1))
        (setq prev (point))
        (while (meow--beacon-forward-line
                (1- ln)
                (point-max))
          (meow--beacon-add-overlay-at-region
           '(select . line)
           prev
           (line-end-position)
           back)
          (forward-line 1)
          (setq prev (point))))
      (save-mark-and-excursion
        (goto-char (point-min))
        (setq prev (point))
        (while (meow--beacon-forward-line
                (1- ln)
                beg)
          (meow--beacon-add-overlay-at-region
           '(select . line)
           prev
           (line-end-position)
           back)
          (forward-line 1)
          (setq prev (point)))))))

(defun meow--add-beacons-for-join ()
  "Add beacon for join movement."
  (save-restriction
    (meow--narrow-secondary-selection)
    (let ((orig (point)))
      (save-mark-and-excursion
        (goto-char (point-min))
        (back-to-indentation)
        (unless (= (point) orig)
          (meow--beacon-add-overlay-at-point (point)))
        (while (< (line-end-position) (point-max))
          (forward-line 1)
          (back-to-indentation)
          (unless (= (point) orig)
            (meow--beacon-add-overlay-at-point (point))))))
    (meow--cancel-selection)))

(defun meow--add-beacons-for-find ()
  "Add beacon for find movement."
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
                  (meow--beacon-add-overlay-at-point (meow--hack-cursor-pos (point))))))
          (save-mark-and-excursion
              (goto-char (point-max))
              (while (search-backward ch-str nil t)
                (unless (= orig (point))
                  (meow--beacon-add-overlay-at-point (point))))))))
    (meow--beacon-shrink-selection)))

(defun meow--add-beacons-for-till ()
  "Add beacon for till movement."
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
                    (meow--beacon-add-overlay-at-point (meow--hack-cursor-pos (1- (point))))))))
          (save-mark-and-excursion
            (goto-char (point-max))
            (while (search-backward ch-str nil t)
              (unless (or (= orig (1+ (point)))
                          (= (point) (point-max)))
                (meow--beacon-add-overlay-at-point (1+ (point)))))))))
    (meow--beacon-shrink-selection)))

(defun meow--beacon-region-words-to-match ()
  "Convert the word selected in region to a regexp."
  (let ((s (buffer-substring-no-properties
            (region-beginning)
            (region-end)))
        (re (car regexp-search-ring)))
    (if (string-match-p (format "\\`%s\\'" re) s)
        re
      (format "\\<%s\\>" (regexp-quote s)))))

(defun meow--beacon-update-overlays ()
  "Update overlays for BEACON state."
  (meow--beacon-remove-overlays)
  (when (meow--beacon-inside-secondary-selection)
    (let* ((ex (car (meow--selection-type)))
           (type (cdr (meow--selection-type))))
      (cl-case type
        ((nil transient) (meow--add-beacons-for-char))
        ((word) (if (not (eq 'expand ex))
                    (meow--add-beacons-for-word)
                  (meow--add-beacons-for-match (meow--beacon-region-words-to-match))))
        ((visit) (meow--add-beacons-for-match (car regexp-search-ring)))
        ((line) (meow--add-beacons-for-line))
        ((join) (meow--add-beacons-for-join))
        ((find) (meow--add-beacons-for-find))
        ((till) (meow--add-beacons-for-till))
        ((char) (when (eq 'expand ex) (meow--add-beacons-for-char-expand)))))))

(defun meow-beacon-end-and-apply-kmacro ()
  "End or apply kmacro."
  (interactive)
  (call-interactively #'kmacro-end-macro)
  (meow--beacon-apply-kmacros))

(defun meow-beacon-start ()
  "Start kmacro recording, apply to all cursors when terminate."
  (interactive)
  (meow--switch-state 'normal)
  (call-interactively 'kmacro-start-macro)
  (setq-local meow--beacon-insert-enter-key nil)
  (setq meow--beacon-defining-kbd-macro 'record))

(defun meow-beacon-insert-exit ()
  "Exit insert mode and terminate kmacro recording."
  (interactive)
  (when defining-kbd-macro
    (end-kbd-macro)
    (meow--beacon-apply-kmacros-from-insert))
  (meow--switch-state 'beacon))

(defun meow-beacon-insert ()
  "Insert and start kmacro recording.

Will terminate recording when exit insert mode.
The recorded kmacro will be applied to all cursors immediately."
  (interactive)
  (meow-beacon-mode -1)
  (meow-insert)
  (call-interactively #'kmacro-start-macro)
  (setq-local meow--beacon-insert-enter-key last-input-event)
  (setq meow--beacon-defining-kbd-macro 'quick))

(defun meow-beacon-append ()
  "Append and start kmacro recording.

Will terminate recording when exit insert mode.
The recorded kmacro will be applied to all cursors immediately."
  (interactive)
  (meow-beacon-mode -1)
  (meow-append)
  (call-interactively #'kmacro-start-macro)
  (setq-local meow--beacon-insert-enter-key last-input-event)
  (setq meow--beacon-defining-kbd-macro 'quick))

(defun meow-beacon-change ()
  "Change and start kmacro recording.

Will terminate recording when exit insert mode.
The recorded kmacro will be applied to all cursors immediately."
  (interactive)
  (meow--with-selection-fallback
   (meow-beacon-mode -1)
   (meow-change)
   (call-interactively #'kmacro-start-macro)
   (setq-local meow--beacon-insert-enter-key last-input-event)
   (setq meow--beacon-defining-kbd-macro 'quick)))

(defun meow-beacon-change-char ()
  "Change and start kmacro recording.

Will terminate recording when exit insert mode.
The recorded kmacro will be applied to all cursors immediately."
  (interactive)
  (meow-beacon-mode -1)
  (meow-change-char)
  (call-interactively #'kmacro-start-macro)
  (setq-local meow--beacon-insert-enter-key last-input-event)
  (setq meow--beacon-defining-kbd-macro 'quick))

(defun meow-beacon-replace ()
  "Replace all selection with current kill-ring head."
  (interactive)
  (meow--with-selection-fallback
   (meow--wrap-collapse-undo
     (meow-replace)
     (save-mark-and-excursion
       (cl-loop for ov in meow--beacon-overlays do
                (when (and (overlayp ov)
                           (not (eq 'cursor (overlay-get ov 'meow-beacon-type))))
                  (goto-char (overlay-start ov))
                  (push-mark (overlay-end ov) t)
                  (meow-replace)
                  (delete-overlay ov)))))))

(defun meow--beacon-delete-region ()
  (delete-region (region-beginning) (region-end)))

(defun meow-beacon-kill-delete ()
  "Delete all selections.

By default, this command will be remapped to `meow-kill'.
Because `meow-kill' are used for deletion on region.

Only the content in real selection will be saved to `kill-ring'."
  (interactive)
  (meow--with-selection-fallback
   (meow--wrap-collapse-undo
     (meow-kill)
     (save-mark-and-excursion
       (cl-loop for ov in meow--beacon-overlays do
                (when (and (overlayp ov)
                           (not (eq 'cursor (overlay-get ov 'meow-beacon-type))))
                  (goto-char (overlay-start ov))
                  (push-mark (overlay-end ov) t)
                  (meow--beacon-delete-region)
                  (delete-overlay ov)))))))

(defun meow-beacon-apply-kmacro ()
  (interactive)
  (meow--switch-state 'normal)
  (call-interactively #'kmacro-call-macro)
  (meow--beacon-apply-kmacros)
  (meow--switch-state 'beacon))

(defun meow-beacon-noop ()
  "Noop, to disable some keybindings in cursor state."
  (interactive))

(provide 'meow-beacon)
;;; meow-beacon.el ends here
