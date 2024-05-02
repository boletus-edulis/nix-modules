(leaf doom-modeline
  :ensure t
  :hook (emacs-startup-hook . doom-modeline-mode))

(leaf transient-dwim
  :ensure t
  :bind (("M-=" . transient-dwim-dispatch)))

(leaf whitespace
  :custom ((whitespace-style . '(face trailing lines-tail space-before-tab))
	   (whitespace-line-column . 80))
  :global-minor-mode global-whitespace-mode)

(leaf python :custom ((python-shell-interpreter . "python3")))

(leaf elpy
  :ensure t
  :custom ((elpy-rpc-backend . "jedi")
	   (elpy-rpc-python-command . "python3")
	   (elpy-modules . (elpy-module-sane-defaults
			    elpy-module-company
			    elpy-module-eldoc
			    elpy-module-highlight-indentation
			    elpy-module-pyvenv
			    elpy-module-yasnippet
			    elpy-module-django)))
  :hook (python-mode-hook)
  :config
  (leaf jedi
    :ensure t)
  (elpy-enable)
  (add-hook 'elpy-mode-hook
	    (lambda ()
	      (add-hook 'before-save-hook
			'elpy-black-fix-code nil t))))

(leaf flycheck
  :ensure t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :custom ((flycheck-clang-pedantic . t)
	   (flycheck-clang-warnings . '("all" "extra"))
	   (flycheck-gcc-pedantic . t)
	   (flycheck-gcc-warnings . '("all" "extra")))
  :hook (emacs-lisp-mode-hook
	 lisp-interaction-mode-hook
	 c-mode-hook
	 elpy-mode-hook)
  :config
  (leaf flycheck-elsa
    :ensure t
    :config (flycheck-elsa-setup))
  (leaf flycheck-rust
    :ensure t))

(leaf magit
  :ensure t
  :custom `((magit-log-section-commit-count . 500)))

(leaf yasnippet
  :ensure t
  :global-minor-mode yas-global-mode)

(leaf company
  :ensure t
  :after yasnippet
  :bind (:company-active-map
         ("M-<tab>" . company-complete-common))
  :hook (emacs-startup-hook . global-company-mode)
  :custom ((company-idle-delay . 0)))

(leaf ivy
  :ensure t
  :custom ((ivy-re-builders-alist . '((t . ivy--regex-fuzzy))))
  :hook (after-init-hook ivy-mode))

(leaf rustic
  :ensure t
  :custom
  `((lsp-rust-analyzer-lens-references-adt-enable . t)
    (lsp-rust-analyzer-lens-references-enumVariant-enable . t)
    (lsp-rust-analyzer-lens-references-method-enable . t)
    (lsp-rust-analyzer-lens-references-trait-enable . t)
    (lsp-rust-analyzer-hover-actions-references-enable . t)
    (lsp-rust-analyzer-inlayHints-bindingModeHints-enable . t)
    (lsp-rust-analyzer-rustc-source . "discover") ;; find rustc-dev in rust-src
    (rustic-format-trigger . 'on-save)
    (rustic-rustfmt-args . "--edition 2021")
    (rustic-rustfmt-config-alist . ((max_width . 120)))
    (indent-tabs-mode . nil)
    (whitespace-line-column . 120)
    (lsp-inlay-hint-enable . t)
    )
  :config
  (setq mode-line-compact t))

(leaf rust-auto-use :ensure t)
(leaf ron-mode :ensure t)
(leaf lsp-mode :ensure t)

(leaf org
  :ensure t
  :config
  (add-to-list 'org-latex-packages-alist '("" "tabularx" t)))

(leaf lsp-nix
  :config
  ;;(use-package-ensure-elpa 'lsp-nix '(lsp-mode) '(:demand t))
  (with-eval-after-load 'lsp-mode
    (let ((custom--inhibit-theme-enable nil))
      (unless (memq 'use-package custom-known-themes)
        (deftheme use-package)
        (enable-theme 'use-package)
        (setq custom-enabled-themes (remq 'use-package custom-enabled-themes)))
      (custom-theme-set-variables
       'use-package '(lsp-nix-nil-formatter
                      ["nixpkgs-fmt"] nil nil "Customized with use-package lsp-nix")))
    (require 'lsp-nix nil nil)))

(leaf nix-mode
  :ensure t
  :commands lsp-deferred
  :hook ((nix-mode-hook . lsp-deferred)))
