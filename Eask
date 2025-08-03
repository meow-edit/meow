;; -*- mode: eask; lexical-binding: t -*-

(package "meow"
         "1.5.0"
         "Yet Another modal editing")

(website-url "https://www.github.com/DogLooksGood/meow")
(keywords "convenience" "modal-editing")

(package-file "meow.el")
(files "meow-*.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "27.1")

(development
 (depends-on "macrostep")
 (depends-on "wdired")
 (depends-on "edebug")
 (depends-on "magit")
 (depends-on "wgrep")
 (depends-on "grep")
 (depends-on "company")
 (depends-on "corfu")
 (depends-on "polymode")
 (depends-on "cider")
 (depends-on "sly")
 (depends-on "realgud")
 (depends-on "which-key")
 (depends-on "undo-tree")
 (depends-on "diff-hl")
 (depends-on "skk")
 (depends-on "eat")
 (depends-on "ediff"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432

(add-hook 'eask-before-compile-hook
           (lambda ()
             ;; TODO: Enable this when clean up all compile warnings.
             (setq byte-compile-error-on-warn nil)))
