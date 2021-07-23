(setq package-list '(use-package lsp-mode company rust-mode ace-popup-menu smex lsp-ui multi-term clang-format dashboard treemacs markdown-mode cmake-ide yasnippet monokai-pro-theme cmake-mode flycheck-irony flycheck-rust nasm-mode projectile rustic))

;; Melpa Repo
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'org)
(require 'company)
(require 'multi-term)
(require 'clang-format)
(require 'treemacs)
(require 'markdown-mode)
(require 'cmake-mode)
(require 'nasm-mode)
(require 'yasnippet)
(require 'rust-mode)
(require 'lsp-mode)
(require 'lsp-ui)
(require 'rustic)
(require 'powerline)

;; =================================================================================

;;=============
;; GUI settings
;;=============
(set-frame-font "Hack-12")
(load-theme 'monokai-pro t)
(add-hook 'find-file-hook (lambda () (linum-mode 1))) ; Line Nr
(column-number-mode 1)
(tool-bar-mode -1)                                    ; Disable Toolbar
(menu-bar-mode -1)                                    ; Disable Menubar
(scroll-bar-mode -1)                                  ; Disable Scrollbar
(show-paren-mode 1)                                   ; Show parens
(electric-pair-mode 1)
(powerline-default-theme)
(global-set-key [f8] 'treemacs)
(set-cursor-color "#ffffff")
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(ido-mode t)
(dashboard-setup-startup-hook)
(setq dashboard-startup-banner 'logo)
(ace-popup-menu-mode 1)
;; =================================================================================

(defun load-conf-file (file)
  (interactive "f")
  (load-file (concat (concat (getenv "HOME") "/.emacs.conf.d/") file))
  )

(load-conf-file "ide.el")
(load-conf-file "rust.el")
(load-conf-file "lsp.el")
(load-conf-file "cc.el")
(load-conf-file "nasm.el")
;; (load-conf-file "magit.el")
;; (load-conf-file "mu4e.el")

;; Copy and Paste
(transient-mark-mode 1)
(add-hook 'term-mode-hook (lambda ()
                            (define-key term-raw-map (kbd "C-y") 'term-paste)))

;; Backup-diretory and Server
(setf backup-directory-alist '((".*" . "~/.saves/")))
(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(powerline lua-mode ssh-deploy php-mode lsp-treemacs lsp-pyright smex ranger exwm pinentry magit treemacs-all-the-icons yasnippet use-package rustic rtags nasm-mode multi-term monokai-pro-theme lsp-ui flycheck-rust flycheck-irony dashboard company-irony-c-headers company-irony cmake-project cmake-mode cmake-ide clang-format cargo))
 '(smtpmail-smtp-server "smtp.mailbox.org")
 '(smtpmail-smtp-service 25))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
