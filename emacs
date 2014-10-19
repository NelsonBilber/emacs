(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-all-caps t)
 '(ansi-color-names-vector ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(custom-enabled-themes (quote (manoj-dark)))
 '(only-global-abbrevs t)
 '(pre-abbrev-expand-hook (quote (ignore))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(global-linum-mode t)
(setq make-backup-files nil) ;;stop creating those backup~ files
(setq auto-save-default nil) ;;stop creating those #autosave# files


;(menu-bar-mode -1)
;(tool-bar-mode -1)


;; OS X keybord special keys
(setq default-input-method "MacOSX")
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'none)

;;disable buffer sounds
(setq ring-bell-function 'ignore) 

;; Org-mode
(global-set-key "\C-cl" 'org-store-link)
     (global-set-key "\C-cc" 'org-capture)
     (global-set-key "\C-ca" 'org-agenda)
     (global-set-key "\C-cb" 'org-iswitchb)

(put 'upcase-region 'disabled nil)

;;dirTree
(add-to-list 'load-path "~/.emacs.d/")
(require 'tree-mode)
(require 'windata)
(require 'dirtree)
(autoload 'dirtree "dirtree" "Add directory to tree view" t)
(global-set-key "\C-o" 'dirtree-show)
