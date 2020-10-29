(setq package-list '(rtags company-irony company-irony-c-headers multi-term irony clang-format dashboard neotree markdown-mode cmake-ide yasnippet monokai-pro-theme cmake-mode flycheck-irony flycheck-rust nasm-mode projectile rustic cmake-project))

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
(require 'cmake-project)
(require 'rtags)
(require 'company)
(require 'multi-term)
(require 'irony)
(require 'clang-format)
(require 'dashboard)
(require 'neotree)
(require 'markdown-mode)
(require 'cmake-ide)
(require 'cmake-mode)
(require 'flycheck-irony)
(require 'nasm-mode)
(require 'company-irony-c-headers)
(require 'yasnippet)

;; =================================================================================

;;=============
;; GUI settings
;;=============
(dashboard-setup-startup-hook)
					; (set-default-font "Inconsolata-12")
(set-frame-font "Hack-12")		;
(load-theme 'monokai-pro t)
(add-hook 'find-file-hook (lambda () (linum-mode 1))) ; Line Nr
(column-number-mode 1)
(tool-bar-mode -1)                                    ; Disable Toolbar
(menu-bar-mode -1)                                    ; Disable Menubar
(scroll-bar-mode -1)                                  ; Disable Scrollbar
(show-paren-mode 1)                                   ; Show parens
(global-set-key [f8] 'neotree-toggle)
(electric-pair-mode 1)
(set-cursor-color "#ffffff")
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; =================================================================================

;; ===============
;; Hooks (C & C++)
;; ===============
(add-hook 'c++-mode-hook 'irony-mode)	; Irony Mode C++ Hook
(add-hook 'c++-mode-hook 'maybe-cmake-project-hook)
(add-hook 'c-mode-hook 'irony-mode)		; Irony Mode C Hook
(add-hook 'c-mode-common-hook 'hs-minor-mode) ; Fold Mode C Hook
(add-hook 'c++-mode-common-hook 'hs-minor-mode) ; Fold Mode C++ Hook
(add-hook 'c++-mode-hook 'flycheck-mode)		; Flycheck Mode C++ Hook
(add-hook 'c-mode-hook 'flycheck-mode)			; Flycheck Mode C Hook
(add-hook 'c++-mode-hook 'company-mode)			; Company Mode C++ Hook
(add-hook 'c-mode-hook 'company-mode)			; Company Mode C Hook

;; Starting Irony-Company
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
	        'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
	        'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(eval-after-load 'company
		 '(add-to-list 'company-backends 'company-irony))
(eval-after-load "irony"
  '(custom-set-variables '(irony-additional-clang-options '("-std=c++14 -Wall -Wextra -I/usr/lib/clang/10.0.0/include/ -I/usr/local/include"))))
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

(defun irony--check-expansion ()
  (save-excursion
      (if (looking-at "\\_>") t
	    (backward-char 1)
	        (if (looking-at "\\.") t
		        (backward-char 1)
			      (if (looking-at "->") t nil)))))
(defun irony--indent-or-complete ()
  "Indent or Complete"
  (interactive)
  (cond ((and (not (use-region-p))
	      (irony--check-expansion))
	 (message "complete")
	 (company-complete-common))
	      (t
		       (message "indent")
		              (call-interactively 'c-indent-line-or-region))))
(defun irony-mode-keys ()
  "Modify keymaps used by `irony-mode'."
  (local-set-key (kbd "TAB") 'irony--indent-or-complete)
  (local-set-key [tab] 'irony--indent-or-complete))
(add-hook 'c-mode-common-hook 'irony-mode-keys)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-irony-c-headers))

;; Starting Flycheck
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
  (flycheck-add-next-checker 'irony '(warning . c/c++-cppcheck)))

;; Starting rtags and Cmake-IDE
(cmake-ide-setup)

(setq rtags-autostart-diagnostics t)
(rtags-diagnostics)
(setq rtags-completions-enabled t)
(push 'company-rtags company-backends)
(global-company-mode)
(define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))

(eval-after-load 'cc-mode
  '(progn
     (require 'rtags)
     (mapc (lambda (x)
             (define-key c-mode-base-map
               (kbd (concat "C-c r " (car x))) (cdr x)))
           '(("." . rtags-find-symbol-at-point)
             ("," . rtags-find-references-at-point)
             ("v" . rtags-find-virtuals-at-point)
             ("V" . rtags-print-enum-value-at-point)
             ("/" . rtags-find-all-references-at-point)
             ("Y" . rtags-cycle-overlays-on-screen)
             (">" . rtags-find-symbol)
             ("<" . rtags-find-references)
             ("-" . rtags-location-stack-back)
             ("+" . rtags-location-stack-forward)
             ("D" . rtags-diagnostics)
             ("G" . rtags-guess-function-at-point)
             ("p" . rtags-set-current-project)
             ("P" . rtags-print-dependencies)
             ("e" . rtags-reparse-file)
             ("E" . rtags-preprocess-file)
             ("R" . rtags-rename-symbol)
             ("M" . rtags-symbol-info)
             ("S" . rtags-display-summary)
             ("O" . rtags-goto-offset)
             (";" . rtags-find-file)
             ("F" . rtags-fixit)
             ("X" . rtags-fix-fixit-at-point)
             ("B" . rtags-show-rtags-buffer)
             ("I" . rtags-imenu)
             ("T" . rtags-taglist)))))

(global-set-key (quote [f11]) (quote compile))

;; Indent
(setq c-default-style "bsd")
(global-set-key [C-M-tab] 'clang-format-region)
(setq clang-format-style-option "~/.clang-format")
(defun clang-format-save-hook-for-this-buffer ()
  "Create a buffer local save hook."
  (add-hook 'before-save-hook
    (lambda ()
      (progn
        (when (locate-dominating-file "." ".clang-format")
          (clang-format-buffer))
        nil))
    nil
    t))
(add-hook 'c-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))
(add-hook 'c++-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))
(add-hook 'glsl-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))

;; NASM
(add-to-list 'auto-mode-alist '("\\.nasm\\'" . nasm-mode))
(add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode))

;; RUST
;; (require 'rust-mode)
;; (autoload 'rust-mode "rust-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
;; (add-hook 'rust-mode-hook
;;           (lambda () (setq indent-tabs-mode nil)))
;; (add-hook 'rust-mode-hook #'racer-mode)
;; (add-hook 'racer-mode-hook #'company-mode)
;; (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
;; (setq company-tooltip-align-annotations t)
;; (add-hook 'rust-mode-hook #'flycheck-mode)
;; (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
;; (setq rust-format-on-save t)
;; (define-key rust-mode-map (kbd "C-c C-r") 'rust-run)
;; (define-key rust-mode-map (kbd "C-c C-c") 'rust-compile)
;; (define-key rust-mode-map (kbd "C-c C-l") 'rust-run-clippy)

(setq lsp-rust-analyzer-server-command '("/usr/local/bin/rust-analyzer"))
(setq rustic-format-on-save t)


;; Rename Buffers and Files
(defun rename-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))
(global-set-key (kbd "C-c R") 'rename-this-buffer-and-file)

;; Copy and Paste
(cua-mode t)
(setq cua-auto-tabify-rectangles nil)
(transient-mark-mode 1)
(setq cua-keep-region-after-copy t)
(add-hook 'term-mode-hook (lambda ()
                            (define-key term-raw-map (kbd "C-y") 'term-paste)))

;; Org Mode Agenda
(setq recentf-exclude '("^/var/folders\\.*"
                        "COMMIT_EDITMSG\\'"
                        ".*-autoloads\\.el\\'"
                        "[/\\]\\.emacs.d/"))

;; Multi-Term
(require 'multi-term)
(when (require 'multi-term nil t)
  (global-set-key (kbd "<f5>") 'multi-term)
  (global-set-key (kbd "<C-next>") 'multi-term-next)
  (global-set-key (kbd "<C-prior>") 'multi-term-prev)
  (setq multi-term-buffer-name "term"
        multi-term-program "/usr/local/bin/ksh"))

;; Window mover
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;; Snippets
(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "C-c y") #'yas-expand)


;; Backup-diretory and Server
(setf backup-directory-alist '((".*" . "~/.saves/")))
(server-start)

;;====================================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(irony-additional-clang-options
   '("-std=c++14 -Wall -Wextra -I/usr/lib/clang/10.0.0/include/"))
 '(package-selected-packages
   '(lsp-mode rustic yasnippet haskell-snippets haskell-mode ranger anzu flycheck-rust w3m rtags pyenv-mode py-autopep8 projectile neotree nasm-mode multi-term markdown-mode helm flycheck-irony elpy dashboard counsel-dash company-irony-c-headers company-irony cmake-project cmake-mode cmake-ide clang-format badwolf-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
