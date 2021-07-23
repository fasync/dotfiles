(setq lsp-rust-analyzer-server-command '("/usr/local/bin/rust-analyzer"))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic-mode))
(use-package cargo
        :ensure t
        :config 
        ;; change emacs PATH o include cargo/bin
        (setenv "PATH" (concat (getenv "PATH") ":~/.cargo/bin"))
        (setq exec-path (append exec-path '("~/.cargo/bin")))
)
(setq rustic-format-on-save t)