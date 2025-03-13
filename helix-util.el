;;; helix-util.el --- Utilities for Helix  -*- lexical-binding: t; -*-

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
;; Utilities for Helix.

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'seq)
(require 'color)

(require 'helix-vars)
;; (require 'helix-keymap)
;; (require 'helix-face)

;; Modes

(defvar helix-normal-mode)

;; (declare-function helix--remove-match-highlights "helix-visual")
;; (declare-function helix--remove-expand-highlights "helix-visual")
;; (declare-function helix--remove-search-highlight "helix-visual")
;; (declare-function helix-insert-mode "helix-core")
;; (declare-function helix-motion-mode "helix-core")
;; (declare-function helix-normal-mode "helix-core")
;; (declare-function helix-keypad-mode "helix-core")
;; (declare-function helix-beacon-mode "helix-core")
;; (declare-function helix-mode "helix-core")
;; (declare-function helix--keypad-format-keys "helix-keypad")
;; (declare-function helix--keypad-format-prefix "helix-keypad")
(declare-function helix-minibuffer-quit "helix-command")
(declare-function helix--enable "helix-core")
;; (declare-function helix--beacon-apply-command "helix-beacon")
;; (declare-function helix-keypad-start-with "helix-keypad")

;; (defun helix--execute-kbd-macro (kbd-macro)
;;   "Execute KBD-MACRO."
;;   (when-let ((ret (key-binding (read-kbd-macro kbd-macro))))
;;     (cond
;;      ((commandp ret)
;;       (call-interactively ret))

;;      ((and (not helix-use-keypad-when-execute-kbd) (keymapp ret))
;;       (set-transient-map ret nil nil))

;;      ((and helix-use-keypad-when-execute-kbd (keymapp ret))
;;       (helix-keypad-start-with kbd-macro)))))

;; (defun helix-insert-mode-p ()
;;   "Whether insert mode is enabled."
;;   (bound-and-true-p helix-insert-mode))

;; (defun helix-motion-mode-p ()
;;   "Whether motion mode is enabled."
;;   (bound-and-true-p helix-motion-mode))

;; (defun helix-normal-mode-p ()
;;   "Whether normal mode is enabled."
;;   (bound-and-true-p helix-normal-mode))

;; (defun helix-keypad-mode-p ()
;;   "Whether keypad mode is enabled."
;;   (bound-and-true-p helix-keypad-mode))

;; (defun helix-beacon-mode-p ()
;;   "Whether keypad mode is enabled."
;;   (bound-and-true-p helix-beacon-mode))

(defun helix--disable-current-state ()
  (when helix--current-state
    (funcall (alist-get helix--current-state helix-state-mode-alist) -1)
    (setq helix--current-state nil)))

(defun helix--read-cursor-face-color (face)
  "Read cursor color from face."
  (let ((f (face-attribute face :inherit)))
    (if (equal 'unspecified f)
        (let ((color (face-attribute face :background)))
          (if (equal 'unspecified color)
              (face-attribute 'default :foreground)
            color))
      (helix--read-cursor-face-color f))))

(defun helix--set-cursor-type (type)
  (if (display-graphic-p)
      (setq cursor-type type)
    (let* ((shape (or (car-safe type) type))
           (param (cond ((eq shape 'bar) "6")
                        ((eq shape 'hbar) "4")
                        (t "2"))))
      (send-string-to-terminal (concat "\e[" param " q")))))

(defun helix--set-cursor-color (face)
  "Set cursor color by face."
  (let ((color (helix--read-cursor-face-color face)))
    (unless (equal (frame-parameter nil 'cursor-color) color)
      (set-cursor-color color))))

(defun helix--update-cursor-default ()
  "Set default cursor type and color"
  (helix--set-cursor-type helix-cursor-type-default)
  (helix--set-cursor-color 'helix-unknown-cursor))

(defun helix--update-cursor-insert ()
  "Set insert cursor type and color"
  (helix--set-cursor-type helix-cursor-type-insert)
  (helix--set-cursor-color 'helix-insert-cursor))

(defun helix--update-cursor-normal ()
  "Set normal cursor type and color"
  (if helix-use-cursor-position-hack
      (unless (use-region-p)
        (helix--set-cursor-type helix-cursor-type-normal))
    (helix--set-cursor-type helix-cursor-type-normal))
  (helix--set-cursor-color 'helix-normal-cursor))

(defun helix--update-cursor-motion ()
  "Set motion cursor type and color"
  (helix--set-cursor-type helix-cursor-type-motion)
  (helix--set-cursor-color 'helix-motion-cursor))

(defun helix--update-cursor-beacon ()
  "Set beacon cursor type and color"
  (helix--set-cursor-type helix-cursor-type-beacon)
  (helix--set-cursor-color 'helix-beacon-cursor))

(defun helix--cursor-null-p ()
  "Check if cursor-type is null"
  (null cursor-type))

(defun helix--update-cursor ()
  "Update cursor type according to the current state.

This uses the variable helix-update-cursor-functions-alist, finds the first
item in which the car evaluates to true, and runs the cdr. The last item's car
in the list will always evaluate to true."
  (with-current-buffer (window-buffer)
    (thread-last helix-update-cursor-functions-alist
      (cl-remove-if-not (lambda (el) (funcall (car el))))
      (cdar)
      (funcall))))

(defun helix--get-state-name (state)
  "Get the name of the current state.

Looks up the state in helix-replace-state-name-list"
  (alist-get state helix-replace-state-name-list))

(defun helix--render-indicator ()
  "Renders a short indicator based on the current state."
  (when (bound-and-true-p helix-global-mode)
    (let* ((state (helix--current-state))
           (state-name (helix--get-state-name state))
           (indicator-face (alist-get state helix-indicator-face-alist)))
      (if state-name
          (propertize
           (format " %s " state-name)
           'face indicator-face)
        ""))))

(defun helix--update-indicator ()
  (let ((indicator (helix--render-indicator)))
    (setq-local helix--indicator indicator)))

;; (defun helix--state-p (state)
;;   (funcall (intern (concat "helix-" (symbol-name state) "-mode-p"))))

(defun helix--current-state ()
  helix--current-state)

(defun helix--should-update-display-p ()
  (cl-case helix-update-display-in-macro
    ((t) t)
    ((except-last-macro)
     (or (null executing-kbd-macro)
         (not (equal executing-kbd-macro last-kbd-macro))))
    ((nil)
     (null executing-kbd-macro))))

(defun helix-update-display ()
  (when (helix--should-update-display-p)
    (helix--update-indicator)
    (helix--update-cursor)))

(defun helix--switch-state (state &optional no-hook)
  "Switch to STATE execute `helix-switch-state-hook' unless NO-HOOK is non-nil."
  (unless (eq state (helix--current-state))
    (let ((mode (alist-get state helix-state-mode-alist)))
      (funcall mode 1))
    (unless (bound-and-true-p no-hook)
      (run-hook-with-args 'helix-switch-state-hook state)))

  )

;; (defvar helix--beacon-apply-command "helix-beacon")

;; (defun helix--exit-keypad-state ()
;;   "Exit keypad state."
;;   (helix-keypad-mode -1)
;;   (when (and (eq 'beacon helix--keypad-previous-state)
;;              helix--current-state)
;;     (helix--beacon-apply-command helix--keypad-this-command))
;;   (when helix--keypad-previous-state
;;     (helix--switch-state helix--keypad-previous-state)))

;; (defun helix--direction-forward ()
;;   "Make the selection towards forward."
;;   (when (and (region-active-p) (< (point) (mark)))
;;     (exchange-point-and-mark)))

;; (defun helix--direction-backward ()
;;   "Make the selection towards backward."
;;   (when (and (region-active-p) (> (point) (mark)))
;;     (exchange-point-and-mark)))

;; (defun helix--direction-backward-p ()
;;   "Return whether we have a backward selection."
;;   (and (region-active-p)
;;        (> (mark) (point))))

;; (defun helix--direction-forward-p ()
;;   "Return whether we have a forward selection."
;;   (and (region-active-p)
;;        (<= (mark) (point))))

;; (defun helix--selection-type ()
;;   "Return current selection type."
;;   (when (region-active-p)
;;     (car helix--selection)))

;; (defun helix--in-string-p (&optional pos)
;;   "Return whether POS or current position is in string."
;;   (save-mark-and-excursion
;;     (when pos (goto-char pos))
;;     (nth 3 (syntax-ppss))))

;; (defun helix--in-comment-p (&optional pos)
;;   "Return whether POS or current position is in string."
;;   (save-mark-and-excursion
;;     (when pos (goto-char pos))
;;     (nth 4 (syntax-ppss))))

;; (defun helix--sum (sequence)
;;   (seq-reduce #'+ sequence 0))

;; (defun helix--reduce (fn init sequence)
;;   (seq-reduce fn sequence init))

;; (defun helix--string-pad (s len pad &optional start)
;;   (if (<= len (length s))
;;       s
;;     (if start
;; 	(concat (make-string (- len (length s)) pad) s)
;;       (concat s (make-string (- len (length s)) pad)))))

;; (defun helix--truncate-string (len s ellipsis)
;;   (if (> (length s) len)
;;       (concat (substring s 0 (- len (length ellipsis))) ellipsis)
;;     s))

;; (defun helix--string-join (sep s)
;;   (string-join s sep))

;; (defun helix--prompt-symbol-and-words (prompt beg end &optional disallow-empty)
;;   "Completion with PROMPT for symbols and words from BEG to END."
;;   (let ((completions))
;;     (save-mark-and-excursion
;;       (goto-char beg)
;;       (while (re-search-forward "\\_<\\(\\sw\\|\\s_\\)+\\_>" end t)
;;         (let ((result (match-string-no-properties 0)))
;;           (when (>= (length result) helix-visit-collect-min-length)
;;             (if helix-visit-sanitize-completion
;;                 (push (cons result (format "\\_<%s\\_>" (regexp-quote result))) completions)
;;               (push (format "\\_<%s\\_>" (regexp-quote result)) completions))))))
;;     (setq completions (delete-dups completions))
;;     (let ((selected (completing-read prompt completions nil nil)))
;;       (while (and (string-empty-p selected)
;;                   disallow-empty)
;;         (setq selected (completing-read
;;                         (concat "[Input must be non-empty] " prompt)
;;                         completions nil nil)))
;;       (if helix-visit-sanitize-completion
;;           (or (cdr (assoc selected completions))
;;               (regexp-quote selected))
;;         selected))))

(defun helix--on-window-state-change (&rest _args)
  "Update cursor style after switching window."
  (helix--update-cursor)
  (helix--update-indicator))

(defun helix--on-exit ()
  (unless (display-graphic-p)
    (send-string-to-terminal "\e[2 q")))

;; (defun helix--get-indent ()
;;   "Get indent of current line."
;;   (save-mark-and-excursion
;;     (back-to-indentation)
;;     (- (point) (line-beginning-position))))

;; (defun helix--empty-line-p ()
;;   "Whether current line is empty."
;;   (string-match-p "^ *$" (buffer-substring-no-properties
;;                           (line-beginning-position)
;;                           (line-end-position))))

;; (defun helix--ordinal (n)
;;   (cl-case n
;;     ((1) "1st")
;;     ((2) "2nd")
;;     ((3) "3rd")
;;     (t (format "%dth" n))))

;; (defun helix--allow-modify-p ()
;;   (and (not buffer-read-only)
;;        (not helix--temp-normal)))

;; (defun helix--with-universal-argument-p (arg)
;;   (equal '(4) arg))

;; (defun helix--with-negative-argument-p (arg)
;;   (< (prefix-numeric-value arg) 0))

;; (defun helix--with-shift-p ()
;;   (member 'shift last-input-event))

;; (defun helix--bounds-with-type (type thing)
;;   (when-let ((bounds (bounds-of-thing-at-point thing)))
;;     (cons type bounds)))

;; (defun helix--insert (&rest args)
;;   "Use `helix--insert-function' to insert ARGS at point."
;;   (apply helix--insert-function args))

;; (defun helix--delete-region (start end)
;;   "Use `helix--delete-region-function' to delete text between START and END."
;;   (funcall helix--delete-region-function start end))

;; (defun helix--push-search (search)
;;   (unless (string-equal search (car regexp-search-ring))
;;     (add-to-history 'regexp-search-ring search regexp-search-ring-max)))

;; (defun helix--remove-text-properties (text)
;;   (set-text-properties 0 (length text) nil text)
;;   text)

;; (defun helix--toggle-relative-line-number ()
;;   (when display-line-numbers
;;     (if (bound-and-true-p helix-insert-mode)
;;         (setq display-line-numbers t)
;;       (setq display-line-numbers 'relative))))

;; (defun helix--render-char-thing-table ()
;;   (let* ((ww (frame-width))
;;          (w 25)
;;          (col (min 5 (/ ww w))))
;;     (thread-last
;;       helix-char-thing-table
;;       (seq-group-by #'cdr)
;;       (seq-sort-by #'car #'string-lessp)
;;       (seq-map-indexed
;;        (lambda (th-pairs idx)
;;          (let* ((th (car th-pairs))
;;                 (pairs (cdr th-pairs))
;;                 (pre (thread-last
;;                        pairs
;;                        (mapcar (lambda (it) (char-to-string (car it))))
;;                        (helix--string-join " "))))
;;            (format "%s%s%s%s"
;;                    (propertize
;;                     (helix--string-pad pre 8 32 t)
;;                      'face 'font-lock-constant-face)
;;                    (propertize " → " 'face 'font-lock-comment-face)
;;                    (propertize
;;                     (helix--string-pad (symbol-name th) 13 32 t)
;;                      'face 'font-lock-function-name-face)
;;                    (if (= (1- col) (mod idx col))
;;                        "\n"
;;                      " ")))))
;;       (string-join)
;;       (string-trim-right))))

;; (defun helix--transpose-lists (lists)
;;   (when lists
;;     (let* ((n (seq-max (mapcar #'length lists)))
;;            (rst (apply #'list (make-list n ()))))
;;       (mapc (lambda (l)
;;               (seq-map-indexed
;;                (lambda (it idx)
;;                  (cl-replace rst
;;                              (list (cons it (nth idx rst)))
;;                              :start1 idx
;;                              :end1 (1+ idx)))
;;                l))
;;             lists)
;;       (mapcar #'reverse rst))))

;; (defun helix--get-event-key (e)
;;   (if (and (integerp (event-basic-type e))
;;            (member 'shift (event-modifiers e)))
;;       (upcase (event-basic-type e))
;;     (event-basic-type e)))

;; (defun helix--ensure-visible ()
;;   (let ((overlays (overlays-at (1- (point))))
;;         ov expose)
;;     (while (setq ov (pop overlays))
;;       (if (and (invisible-p (overlay-get ov 'invisible))
;;                (setq expose (overlay-get ov 'isearch-open-invisible)))
;;           (funcall expose ov)))))



;; (defun helix--parse-string-to-keypad-keys (str)
;;   (let ((strs (split-string str " ")))
;;     (thread-last
;;       strs
;;       (mapcar
;;        (lambda (str)
;;          (cond
;;           ((string-prefix-p "C-M-" str)
;;            (cons 'both (substring str 4)))
;;           ((string-prefix-p "C-" str)
;;            (cons 'control (substring str 2)))
;;           ((string-prefix-p "M-" str)
;;            (cons 'meta (substring str 2)))
;;           (t
;;            (cons 'literal str)))))
;;       (reverse))))

;; (defun helix--parse-input-event (e)
;;   (cond
;;    ((equal e 32)
;;     "SPC")
;;    ((characterp e)
;;     (string e))
;;    ((equal 'tab e)
;;     "TAB")
;;    ((equal 'return e)
;;     "RET")
;;    ((equal 'backspace e)
;;     "DEL")
;;    ((equal 'escape e)
;;     "ESC")
;;    ((symbolp e)
;;     (format "<%s>" e))
;;    (t nil)))

;; (defun helix--save-origin-commands ()
;;   "Save the commands overridden by the Motion map to modified bindings.

;; The new key binding, modified by the prefix in
;; `helix-motion-remap-prefix', is bound to a command that calls the
;; command locally bound to the original key binding, or, if that is
;; nil, the original command.

;; For example, under the default and suggested settings, in a
;; Magit status buffer, `k' could be bound to `helix-previous'
;; and `H-k' would be bound to a command that would try
;; to use the status buffer's original `k' binding at point."
;;   (cl-loop for key-code being the key-codes of helix-motion-state-keymap do
;;            (ignore-errors
;;              (let* ((key (helix--parse-input-event key-code))
;;                     (cmd (key-binding (kbd key))))
;;                (when (and (commandp cmd)
;;                           (not (equal cmd 'undefined)))
;;                  (let ((rebind-key (concat helix-motion-remap-prefix key)))
;;                    (local-set-key (kbd rebind-key)
;;                                   (lambda ()
;;                                     (interactive)
;;                                     ;; Local maps are those local to the buffer
;;                                     ;; or a region of the buffer.
;;                                     (let* ((local (lookup-key (current-local-map) key))
;;                                            (remapped (command-remapping local)))
;;                                       (call-interactively
;;                                        (cond
;;                                         ((commandp remapped)
;;                                          remapped)
;;                                         ((commandp local)
;;                                          local)
;;                                         (t
;;                                          cmd))))))))))))

;; (defun helix--prepare-region-for-kill ()
;;   (when (and (equal 'line (cdr (helix--selection-type)))
;;              (helix--direction-forward-p)
;;              (< (point) (point-max)))
;;     (forward-char 1)))

;; (defun helix--prepare-string-for-kill-append (s)
;;   (let ((curr (current-kill 0 nil)))
;;     (cl-case (cdr (helix--selection-type))
;;       ((line) (concat (unless (string-suffix-p "\n" curr) "\n")
;;                       (string-trim-right s "\n")))
;;       ((word block) (concat (unless (string-suffix-p " " curr) " ")
;;                             (string-trim s " " "\n")))
;;       (t s))))

;; (defun helix--event-key (e)
;;   (let ((c (event-basic-type e)))
;;     (if (and (char-or-string-p c)
;;              (member 'shift (event-modifiers e)))
;;         (upcase c)
;;       c)))

;; (defun helix--parse-def (def)
;;   "Return a command or keymap for DEF.

;; If DEF is a string, return a command that calls the command or keymap
;; that bound to DEF. Otherwise, return DEF."
;;   (if (stringp def)
;;       (let ((cmd-name (gensym 'helix-dispatch_)))
;;         ;; dispatch command
;;         (defalias cmd-name
;;           (lambda ()
;;             (:documentation
;;              (format "Execute the command which is bound to %s." def))
;;             (interactive)
;;             (helix--execute-kbd-macro def)))
;;         (put cmd-name 'helix-dispatch def)
;;         cmd-name)
;;     def))

;; (defun helix--second-sel-set-string (string)
;;   (cond
;;    ((helix--second-sel-buffer)
;;     (with-current-buffer (overlay-buffer mouse-secondary-overlay)
;;       (goto-char (overlay-start mouse-secondary-overlay))
;;       (helix--delete-region (overlay-start mouse-secondary-overlay) (overlay-end mouse-secondary-overlay))
;;       (helix--insert string)))
;;    ((markerp mouse-secondary-start)
;;     (with-current-buffer (marker-buffer mouse-secondary-start)
;;       (goto-char (marker-position mouse-secondary-start))
;;       (helix--insert string)))))

;; (defun helix--second-sel-get-string ()
;;   (when (helix--second-sel-buffer)
;;     (with-current-buffer (overlay-buffer mouse-secondary-overlay)
;;       (buffer-substring-no-properties
;;        (overlay-start mouse-secondary-overlay)
;;        (overlay-end mouse-secondary-overlay)))))

;; (defun helix--second-sel-buffer ()
;;   (and (overlayp mouse-secondary-overlay)
;;        (overlay-buffer mouse-secondary-overlay)))

;; (defun helix--second-sel-bound ()
;;   (and (secondary-selection-exist-p)
;;        (cons (overlay-start mouse-secondary-overlay)
;;              (overlay-end mouse-secondary-overlay))))

;; (defmacro helix--with-selection-fallback (&rest body)
;;   `(if (region-active-p)
;;        (progn ,@body)
;;      (helix--selection-fallback)))

;; (defmacro helix--wrap-collapse-undo (&rest body)
;;   "Like `progn' but perform BODY with undo collapsed."
;;   (declare (indent 0) (debug t))
;;   (let ((handle (make-symbol "--change-group-handle--"))
;;         (success (make-symbol "--change-group-success--")))
;;     `(let ((,handle (prepare-change-group))
;;            ;; Don't truncate any undo data in the middle of this.
;;            (undo-outer-limit nil)
;;            (undo-limit most-positive-fixnum)
;;            (undo-strong-limit most-positive-fixnum)
;;            (,success nil))
;;        (unwind-protect
;;            (progn
;;              (activate-change-group ,handle)
;;              (prog1 ,(macroexp-progn body)
;;                (setq ,success t)))
;;          (if ,success
;;              (progn
;;                (accept-change-group ,handle)
;;                (undo-amalgamate-change-group ,handle))
;;            (cancel-change-group ,handle))))))

;; (defun helix--highlight-pre-command ()
;;   (unless (member this-command '(helix-search))
;;     (helix--remove-match-highlights))
;;   (helix--remove-expand-highlights)
;;   (helix--remove-search-highlight))

;; (defun helix--remove-fake-cursor (rol)
;;   (when (overlayp rol)
;;     (when-let ((ovs (overlay-get rol 'helix-face-cursor)))
;;       (mapc (lambda (o) (when (overlayp o) (delete-overlay o)))
;;             ovs))))

;; (defvar helix--region-cursor-faces '(helix-region-cursor-1
;;                                     helix-region-cursor-2
;;                                     helix-region-cursor-3))

;; (defun helix--add-fake-cursor (rol)
;;   (if (and helix-use-enhanced-selection-effect
;;            (or (helix-normal-mode-p)
;;                (helix-beacon-mode-p)))
;;       (when (overlayp rol)
;;         (let ((start (overlay-start rol))
;;               (end (overlay-end rol)))
;;           (unless (= start end)
;;             (let (ovs)
;;                 (if (helix--direction-forward-p)
;;                     (progn
;;                       (let ((p end)
;;                             (i 0))
;;                         (while (and (> p start)
;;                                     (< i 3))
;;                           (let ((ov (make-overlay (1- p) p)))
;;                             (overlay-put ov 'face (nth i helix--region-cursor-faces))
;;                             (overlay-put ov 'priority 10)
;;                             (overlay-put ov 'window (overlay-get rol 'window))
;;                             (cl-decf p)
;;                             (cl-incf i)
;;                             (push ov ovs)))))
;;                   (let ((p start)
;;                         (i 0))
;;                     (while (and (< p end)
;;                                 (< i 3))
;;                       (let ((ov (make-overlay p (1+ p))))
;;                         (overlay-put ov 'face (nth i helix--region-cursor-faces))
;;                         (overlay-put ov 'priority 10)
;;                         (overlay-put ov 'window (overlay-get rol 'window))
;;                         (cl-incf p)
;;                         (cl-incf i)
;;                         (push ov ovs)))))
;;                 (overlay-put rol 'helix-face-cursor ovs)))
;;           rol))
;;     rol))

;; (defun helix--redisplay-highlight-region-function (start end window rol)
;;   (when (and (or (helix-normal-mode-p)
;;                  (helix-beacon-mode-p))
;;              (equal window (selected-window)))
;;     (if (use-region-p)
;;         (helix--set-cursor-type helix-cursor-type-region-cursor)
;;       (helix--set-cursor-type helix-cursor-type-normal)))
;;   (when helix-use-enhanced-selection-effect
;;     (helix--remove-fake-cursor rol))
;;   (thread-first
;;     (funcall helix--backup-redisplay-highlight-region-function start end window rol)
;;     (helix--add-fake-cursor)))

;; (defun helix--redisplay-unhighlight-region-function (rol)
;;   (helix--remove-fake-cursor rol)
;;   (when (and (overlayp rol)
;;              (equal (overlay-get rol 'window) (selected-window))
;;              (or (helix-normal-mode-p)
;;                  (helix-beacon-mode-p)))
;;     (helix--set-cursor-type helix-cursor-type-normal))
;;   (funcall helix--backup-redisplay-unhighlight-region-function rol))

(defun helix--mix-color (color1 color2 n)
  (mapcar (lambda (c) (apply #'color-rgb-to-hex c))
          (color-gradient (color-name-to-rgb color1)
                          (color-name-to-rgb color2)
                          n)))

;; (defun helix--beacon-inside-secondary-selection ()
;;   (and
;;    (secondary-selection-exist-p)
;;    (< (overlay-start mouse-secondary-overlay)
;;       (overlay-end mouse-secondary-overlay))
;;    (<= (overlay-start mouse-secondary-overlay)
;;        (point)
;;        (overlay-end mouse-secondary-overlay))))

;; (defun helix--narrow-secondary-selection ()
;;   (narrow-to-region (overlay-start mouse-secondary-overlay)
;;                     (overlay-end mouse-secondary-overlay)))

;; (defun helix--hack-cursor-pos (pos)
;;   "Hack the point when `helix-use-cursor-position-hack' is enabled."
;;   (if helix-use-cursor-position-hack
;;       (1- pos)
;;     pos))

(defun helix--remove-modeline-indicator ()
  (setq-default mode-line-format
                (cl-remove '(:eval (helix-indicator)) mode-line-format
                           :test 'equal)))

(defun helix--init-buffers ()
  "Enable helix in existing buffers."
  (dolist (buf (buffer-list))
    (unless (minibufferp buf)
      (with-current-buffer buf
        (helix--enable)))))

;; (defun helix--get-leader-keymap ()
;;   (cond
;;    ((keymapp helix-keypad-leader-dispatch)
;;     helix-keypad-leader-dispatch)

;;    ((null helix-keypad-leader-dispatch)
;;     (alist-get 'leader helix-keymap-alist))))

(provide 'helix-util)
;;; helix-util.el ends here
