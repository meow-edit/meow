;;; -*- lexical-binding: t -*-
;; A script used to bundle whole package into a single file

(require 'cl-lib)
(require 'seq)

(defconst meow--bundle-file-list
  '("meow-util.el"
    "meow-var.el"
    "meow-face.el"
    "meow-visual.el"
    "meow-thing.el"
    "meow-command.el"
    "meow-keypad.el"
    "meow-beacon.el"
    "meow-keymap.el"
    "meow-shims.el"
    "meow-tutor.el"
    "meow-helpers.el"
    "meow-esc.el"
    "meow-cheatsheet-layout.el"
    "meow-cheatsheet.el"
    "meow-core.el"
    "scripts/meow-test.el"))

(defun meow-bundle (dir tgt)
  (let ((buf (get-buffer-create "*meow bundle*")))

    (with-current-buffer buf
      (erase-buffer)
      (insert ";;; meow.el --- Yet Another modal editing -*- lexical-binding: t; -*-\n")

      (dolist (file meow--bundle-file-list)
        (let ((content (with-temp-buffer
                         (let (beg end)
                           (insert-file-contents (expand-file-name file dir))
                           (goto-char (point-min))
                           (replace-regexp-in-region "(require 'meow-.+?)\n" "")
                           (replace-regexp-in-region "(declare-function meow-.+?)\n" "")
                           (search-forward ";;; Code:")
                           (setq beg (point))
                           (search-forward "(provide")
                           (setq end (match-beginning 0))
                           (buffer-substring-no-properties beg end)))))
          (insert (format ";; --- Bundle from %s ---" file))
          (insert content)
          (insert "\n")))
      (write-file tgt))))

;; (meow-bundle "/home/tianshu/.emacs.d/straight/repos/meow/" "/home/tianshu/meow.el")
