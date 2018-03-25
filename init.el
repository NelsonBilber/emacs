;;; init.el --- Emacs Settings
;; Author: Nelson Rodrigues <rodrigues.b.nelson@gmail.com>
;;; Commentary:
;; Load an org file with settings to apply to emacs

(require 'org)
(org-babel-load-file
 (expand-file-name "settings.org"
                   user-emacs-directory))
