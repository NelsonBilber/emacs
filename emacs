(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (wheatgrass))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; OSX bracelets
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)


;;backup files and auto-save (~)
(global-linum-mode t)
(setq make-backup-files nil) ;stop creating those baclup~ files                                
(setq auto-save-default nil) ;stop creating those #autosave# files                             

;(menu-bar-mode -1)                                                                            
;(tool-bar-mode -1)                                                                            

(setq ring-bell-function 'ignore) ;disable buffer sounds



;neotree
(add-to-list 'load-path "/Users/…/.emacs.d/neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;dirtree
(add-to-list 'load-path "/Users/…/.emacs.d/dirtree")
(require 'tree-mode)
(require 'windata)
(require 'dirtree)
(autoload 'dirtree "dirtree" "Add directory to tree view" t)
(global-set-key "\C-o" 'dirtree-show)




;; C++ 
;; C++ part I - https://www.youtube.com/watch?v=HTUE03LnaXA

; start package.el with emacs
(require 'package)
;add MELPA to repository list
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
;initialize package.el
(package-initialize)
;start auto-complete with emacs
(require 'auto-complete)
;do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)
;start yasnippet with emacs
(require 'yasnippet)
(yas-global-mode 1)
;let's define a function wich initializes auto-complete-c-headers and
;gets called for c/c++ hooks
(defun my:ac-c-header-init()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'achead:include-directories '"/usr/include")); localization of OS include files
;call this function from c/c++ hooks
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;C++ - part 3
;https://www.youtube.com/watch?v=Ib914gNr0ys

;turn on Semantic
(semantic-mode 1)
;function wich adds semantic as a suggestion backend to auto complete
;and hook this function to c-mode-common-hook
(defun my:add-semantic-to-autocomplete()
  (add-to-list 'ac-sources 'ac-source-semantic)
)
(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)
;turn on ede mode
(global-ede-mode 1)
; you can use system-include-path for setting up the system header
; file locations.
; turn on automatic reparsing of open bufferss in semantic
(global-semantic-idle-scheduler-mode 1)

