(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

(setq show-paren-mode t)

;; Ispell
(setq-default ispell-dictionary "en")

;; Gnus
(require 'gnus)
(add-hook 'message-mode-hook 'turn-on-auto-fill)

;; Info
(require 'info)
(setq Info-default-directory-list
      (cons (expand-file-name "~/Dropbox/info") 
	    Info-default-directory-list))

;; Mixed DOS and UNIX line endings
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; PHP
(add-to-list 'load-path "~/.emacs.d/php-mode-1.15.3")
(require 'php-mode)

(setq php-manual-path "~/.emacs.d/php-manual")

(add-hook 'php-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq c-basic-offset 4)
	    (remove-dos-eol)))

(define-key php-mode-map (kbd "TAB") (lambda () 
				       (interactive) 
				       (c-indent-line-or-region)
				       ;(php-complete-function)
				       ))

(define-key php-mode-map (kbd "RET") 'newline-and-indent)


;; ;; Version Control
;; (load-file "/usr/local/share/emacs/site-lisp/dvc/dvc-load.el")
;; (add-to-list 'Info-default-directory-list "/path/to/install/share/info/")

;; ;; Outline mode prefix ('cause C-c @ sucks)
;; (setq outline-minor-mode-prefix (kbd "C-c C-o"))

;; ;; R
;; (require 'ess-site)
;; (setq ess-ask-for-ess-directory nil)
;; (setq ess-directory "~/rdata/")
;; (add-hook 'ess-mode-hook
;; 	  (lambda ()
;; 	    (outline-minor-mode t)
;; 	    (setq outline-regexp "##")
;; 	    (setq outline-level 
;; 		  (lambda () 
;; 		    (cond
;; 		      ((looking-at "#####") 1)
;; 		      (t 2))))))

;; Lisp

(load (expand-file-name "c:/Users/Cers/quicklisp/slime-helper.el"))
(require 'slime)
(slime-setup '(slime-fancy))
(setq inferior-lisp-program "c:/ccl-1.10/wx86cl64.exe")

(define-key slime-mode-map (kbd "RET") 'newline-and-indent)
(define-key slime-mode-map (kbd "TAB") 'slime-complete-symbol)
(define-key slime-mode-map (kbd "C-c TAB") 'slime-complete-symbol)
;(define-key slime-mode-map (kbd "(") 'insert-parentheses)
(global-set-key "\C-cs" 'slime-selector)

(define-key slime-mode-map (kbd "(") 'insert-parentheses)
(define-key slime-mode-map (kbd ")") 'move-past-close-and-reindent)
(define-key slime-mode-map (kbd "C-)") (lambda () (interactive) (insert ")")))
(define-key slime-mode-map (kbd "C-(") (lambda () (interactive) (insert "(")))

(define-key slime-mode-map (kbd "C-M-h") 'backward-sexp)
(define-key slime-mode-map (kbd "C-M-t") 'transpose-sexps)
(define-key slime-mode-map (kbd "C-M-n") 'forward-sexp)
(define-key slime-mode-map (kbd "C-M-k") 'kill-sexp)

; lisp-interactive-mode.
(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (setq lisp-indent-function 'common-lisp-indent-function
	          slime-complete-symbol-function 'slime-fuzzy-complete-symbol)))

; lisp-mode.
(add-hook 'lisp-mode-hook
          (lambda ()
            (slime-mode t)
            (setq lisp-indent-function 'common-lisp-indent-function
	          slime-complete-symbol-function 'slime-fuzzy-complete-symbol)))

;; ;; ;; LaTeX
;; (require 'tex-site)
;; (require 'latex)
;; (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
;; (add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
;; (setq TeX-auto-save t)
;; (setq TeX-parse-self t)
;; (setq-default TeX-master nil)
;; (setq-default TeX-PDF-mode t)
;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
;; (setq-default TeX-newline-function 'reindent-then-newline-and-indent)
;; ;(add-hook 'find-file-hook 'TeX-fold-buffer t)
;; ; Thesis folds
;; (setq LaTeX-fold-math-spec-list 
;;       (cons (list "ε" (list "emptyword"))
;; 	    LaTeX-fold-math-spec-list))

;; (setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")))
;; (setq TeX-view-program-selection '((output-pdf "Evince")))
;; ;;(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
;; (setq TeX-source-correlate-start-server t)

;; Org-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-col" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cob" 'org-iswitchb)
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
;(add-hook 'org-mode-hook 'turn-on-flyspell)

;; Tramp
(setq tramp-default-method "plink")

;; Win paths
(push "C:/Program Files (x86)/Git/bin" exec-path)
(push "C:/Program Files (x86)/PuTTY" exec-path)

(setenv "PATH" (concat "C:\\Program Files (x86)\\PuTTY;" (getenv "PATH")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 143 :width normal)))))
