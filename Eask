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

(depends-on "emacs" "27.1")

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432

(add-hook 'eask-before-compile-hook
           (lambda ()
             ;; TODO: Enable this when clean up all compile warnings.
             (setq byte-compile-error-on-warn nil)))
