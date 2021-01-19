;;; meow-visual.el --- Visual effect in Meow  -*- lexical-binding: t; -*-

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
;; Implementation for all commands in Meow.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'subr-x)
(require 'pcase)

(require 'meow-var)
(require 'meow-util)

(declare-function hl-line-highlight "hl-line")

(defvar meow--highlight-overlays nil
  "Overlays used to highlight in buffer.")

(defvar meow--search-indicator-overlay nil
  "Overlays used to display search indicator in current line.")

(defvar meow--search-indicator-state nil
  "The state for search indicator.

Value is a list of (last-regexp last-pos idx cnt).")

(defvar meow--dont-remove-overlay nil
  "Indicate we should prevent removing overlay for once.")


(defun meow--remove-highlights ()
  (mapc (lambda (it) (delete-overlay it)) meow--highlight-overlays)
  (setq meow--highlight-overlays nil))

(defun meow--remove-search-indicator ()
  (when meow--search-indicator-overlay
    (delete-overlay meow--search-indicator-overlay))
  (setq meow--search-indicator-overlay nil
        meow--search-indicator-state nil))

(defun meow--show-indicator (pos idx cnt)
  (goto-char pos)
  (goto-char (line-end-position))
  (if (= (point) (point-max))
      (let ((ov (make-overlay (point) (point))))
        (overlay-put ov 'after-string (propertize (format " [%d/%d]" idx cnt) 'face 'meow-search-indicator))
        (setq meow--search-indicator-overlay ov))
    (let ((ov (make-overlay (point) (1+ (point)))))
      (overlay-put ov 'display (propertize (format " [%d/%d] \n" idx cnt) 'face 'meow-search-indicator))
      (setq meow--search-indicator-overlay ov))))

(defun meow--highlight-match ()
  (let ((beg (match-beginning 0))
        (end (match-end 0)))
    (unless (--find (overlay-get it 'meow) (overlays-at beg))
      (let ((ov (make-overlay beg end)))
        (overlay-put ov 'face 'meow-search-highlight)
        (overlay-put ov 'meow t)
        (push ov meow--highlight-overlays)))))

(defun meow--highlight-regexp-in-buffer (regexp)
  "Highlight all regexp in this buffer.

There is a cache mechanism, if the REGEXP is not changed, we simplily inc/dec idx and redraw the overlays. Only count for the first time."
  (when (region-active-p)
    (-let ((cnt 0)
           (idx 0)
           (pos (region-end))
           ((last-regexp last-pos last-idx last-cnt) meow--search-indicator-state)
           (win-start (save-mark-and-excursion
                        (goto-char (window-start))
                        (forward-line (- (window-height)))
                        (line-beginning-position)))
           (win-end (save-mark-and-excursion
                        (goto-char (window-end))
                        (forward-line (window-height))
                        (line-end-position))))
      (setq meow--expand-nav-function nil)
      (setq meow--visual-command this-command)
      (cond
       ((equal pos last-pos))

       ((equal last-regexp regexp)
        (setq cnt last-cnt
              idx (if (> pos last-pos) (1+ last-idx) (1- last-idx)))
        (meow--remove-search-indicator)
        (save-mark-and-excursion
          (goto-char win-start)
          (while (re-search-forward regexp win-end t)
            (meow--highlight-match))
          (meow--show-indicator pos idx cnt)
          (setq meow--search-indicator-state (list regexp pos idx cnt))))

       ;; For initializing searching highlight, we need to count
       (t
        (save-mark-and-excursion
          (meow--remove-search-indicator)
          (let ((case-fold-search nil))
            (goto-char (point-min))
            (while (re-search-forward regexp (point-max) t)
              (cl-incf cnt)
              (when (<= (match-beginning 0) pos (match-end 0))
                (setq idx cnt))
              (when (<= win-start (point) win-end)
                (meow--highlight-match)))
            (meow--show-indicator pos idx cnt)
            (setq meow--search-indicator-state (list regexp pos idx cnt)))))))
    (when (bound-and-true-p hl-line-mode) (hl-line-highlight))
    (sit-for most-positive-fixnum)
    (meow--remove-highlights)
    (meow--remove-search-indicator)))

(defun meow--format-full-width-number (n)
  (alist-get n meow-full-width-number-position-chars))

(defun meow--remove-highlight-overlays ()
  (if meow--dont-remove-overlay
      (setq meow--dont-remove-overlay nil)
    (unless (or (equal this-command meow--visual-command)
                (member this-command
                        '(meow-expand
                          meow-expand-0
                          meow-expand-1
                          meow-expand-2
                          meow-expand-3
                          meow-expand-4
                          meow-expand-5
                          meow-expand-6
                          meow-expand-7
                          meow-expand-8
                          meow-expand-9)))
      (meow--remove-highlights)
      (setq meow--visual-command nil
            meow--expand-nav-function nil))
    (unless (member this-command '(meow-reverse meow-visit meow-search meow-mark-symbol meow-mark-word))
      (meow--remove-search-indicator)
      (setq meow--visual-command nil))))

(defun meow--highlight-num-positions-1 (nav-function faces bound)
  (save-mark-and-excursion
    (let ((pos (point)))
      (cl-loop for face in faces
               do
               (cl-loop for i from 1 to 10 do
                        (unless (funcall nav-function)
                          (cl-return))
                        (if (or (> (point) (cdr bound))
                                (< (point) (car bound))
                                (= (point) pos))
                            (cl-return)
                          (setq pos (point))
                          (let ((ov (make-overlay (point) (1+ (point))))
                                (before-full-width-char (and (char-after) (= 2 (char-width (char-after)))))
                                (before-newline (equal 10 (char-after)))
                                (before-tab (equal 9 (char-after)))
                                (n (if (= i 10) 0 i)))
                            (cond
                             (before-full-width-char
                              (overlay-put ov 'display (propertize (format "%s" (meow--format-full-width-number n)) 'face face)))
                             (before-newline
                              (overlay-put ov 'display (propertize (format "%s\n" n) 'face face)))
                             (before-tab
                              (overlay-put ov 'display (propertize (format "%s\t" n) 'face face)))
                             (t
                              (overlay-put ov 'display (propertize (format "%s" n) 'face face))))
                            (push ov meow--highlight-overlays))))))))

(defun meow--highlight-num-positions (&optional nav-functions)
  (when-let ((nav-functions (or nav-functions meow--expand-nav-function)))
    (setq meow--expand-nav-function nav-functions)
    (setq meow--visual-command this-command)
    (meow--remove-highlights)
    (meow--remove-search-indicator)
    (-let ((bound (cons (window-start) (window-end)))
           (faces (if (meow--direction-backward-p)
                      '(meow-position-highlight-reverse-number-1
                        meow-position-highlight-reverse-number-2
                        meow-position-highlight-reverse-number-3)
                    '(meow-position-highlight-number-1
                      meow-position-highlight-number-2
                      meow-position-highlight-number-3)))
           (nav-function (if (meow--direction-backward-p)
                             (car nav-functions)
                           (cdr nav-functions))))
      (meow--highlight-num-positions-1 nav-function faces bound)
      ;; sit-for is disabled when recording kmacros,
      ;; we use this fallback behavior which remove highlight after delay seconds
      ;; Meow don't use pre/post-command-hook for performance reason.
      (if defining-kbd-macro
          (run-at-time
           (time-add (current-time)
                     (seconds-to-time meow-expand-hint-remove-delay))
           nil
           #'meow--remove-highlights)
        (when (bound-and-true-p hl-line-mode) (hl-line-highlight))
        (sit-for meow-expand-hint-remove-delay t)
        (meow--remove-highlights)))))

(defun meow--select-expandable-p ()
  (when-let ((sel (meow--selection-type)))
    (or
     (equal '(expand . word) sel)
     (equal '(select . word) sel)
     (equal '(expand . line) sel)
     (equal 'find (car sel))
     (equal 'till (car sel))
     (equal '(expand . block) sel))))

(defun meow--maybe-highlight-num-positions (&optional nav-functions)
  (when (and (not (member major-mode meow-expand-exclude-mode-list))
             (meow--select-expandable-p))
    (meow--highlight-num-positions nav-functions)))

(provide 'meow-visual)
;;; meow-visual.el ends here
