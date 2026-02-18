(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))

  (package-initialize)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file 'noerror)

  (custom-set-faces
   '(default ((t (:family "Iosevka Term Slab" :foundry "BE5N" :slant normal :weight normal :height 128 :width normal))))
   '(Info-quoted ((t (:family "Iosevka"))))
   '(fixed-pitch ((t (:family "Iosevka"))))
   '(fixed-pitch-serif ((t (:family "Iosevka"))))
   '(mode-line ((t (:background "gray30" :box (:line-width 1 :color "red") :height 128 :family "Iosevka"))))
   '(mode-line-inactive ((t (:background "gray30" :box (:line-width 1 :color "red") :family "Iosevka"))))
   '(variable-pitch ((t (:family "Iosevka")))))

  (transient-mark-mode t)
  (show-paren-mode t)
  (menu-bar-mode -1)
  (which-function-mode t)
  (global-auto-revert-mode t)
  (column-number-mode t)
  (xterm-mouse-mode -1)

  (setq show-trailing-whitespace t)
  (setq make-backup-files -1)
  (setq auto-save-default -1)

  (when (display-graphic-p)
    (tool-bar-mode -1))

  (load-theme 'modus-vivendi-tritanopia)

  (use-package whitespace
    :ensure t  ;; I also tried without this line
    :config
    (setq whitespace-style '(face trailing lines-tail space-before-tab))
    (setq whitespace-line-column 80)
    (global-whitespace-mode))

  (use-package treesit-auto
    :ensure t
    :custom
    (treesit-auto-install 'prompt)
    :config
    (treesit-auto-add-to-auto-mode-alist 'all)
    (global-treesit-auto-mode))

  (use-package doom-modeline
    :ensure t
    :init (doom-modeline-mode 1))

  (use-package nerd-icons
    :custom
    (nerd-icons-font-family "Symbols Nerd Font Mono"))

  (use-package nerd-icons-grep
    :init
    (nerd-icons-grep-mode)
    :custom
    (grep-use-headings t))

  (use-package nerd-icons-completion
    :config
    (nerd-icons-completion-mode))

  (use-package magit
    :ensure t
    :config
    (setq magit-log-section-commit-count 500))

  (use-package transient-dwim
    :ensure t
    :bind ("M-=" . transient-dwim-dispatch))

;;  (use-package python
;;    :config
;;    (setq python-shell-interpreter "python3"))

  (use-package blacken
    :ensure t
    :hook ((python-mode-hook . (lambda () (blacken-mode 1)))
           (python-ts-mode-hook . (lambda () (blacken-mode 1)))))

  ;;(leaf elpy
  ;;  :ensure t
  ;;  :custom ((elpy-rpc-backend . "jedi")
  ;;           (elpy-rpc-virtualenv-path . 'global)
  ;;           (elpy-rpc-python-command . "python3")
  ;;           (elpy-modules . (elpy-module-sane-defaults
  ;;      		      elpy-module-company
  ;;      		      elpy-module-eldoc
  ;;      		      elpy-module-highlight-indentation
  ;;      		      elpy-module-pyvenv
  ;;      		      elpy-module-yasnippet
  ;;      		      elpy-module-django)))
  ;;  :hook python-mode-hook
  ;;  :config
  ;;  (leaf jedi
  ;;    :ensure t)
  ;;  (add-hook 'elpy-mode-hook (lambda ()
  ;;                              (add-hook 'before-save-hook
  ;;                                        'elpy-black-fix-code nil t)))
  ;;  (elpy-enable)
  ;;  )

  (use-package flycheck
    :ensure t
    :bind
    (("M-n" . flycheck-next-error)
     ("M-p" . flycheck-previous-error))
    :config
    (setq flycheck-clang-pedantic t)
    (setq flycheck-clang-warnings '("all" "extra"))
    (setq flycheck-gcc-pedantic t)
    (setq flycheck-gcc-warnings '("all" "extra"))
    :init (global-flycheck-mode))

  (use-package eglot
    :ensure t
    :hook (( python-ts-mode ) . eglot-ensure)
    :custom
    (eglot-stay-out-of '(yasnippet)))

  (use-package helm
    :ensure t
    :bind (("M-x" . helm-M-x)
           ("C-x C-f" . helm-find-files)
	   ("C-x b" . helm-buffers-list))
    :config (helm-mode 1))

  (use-package yasnippet
    :ensure t
    :init (yas-global-mode))

;;  (use-package company-prescient
;;    :ensure t
;;    :hook company-mode)
;;
;;  (use-package company
;;    :ensure t
;;    :after yasnippet
;;    :bind (:map company-mode-map
;;           ("M-<tab>" . company-complete-common))
;;    :hook prog-mode)

;;  (leaf rustic
;;    :ensure t
;;    :custom
;;    `((lsp-rust-analyzer-lens-references-adt-enable . t)
;;      (lsp-rust-analyzer-lens-references-enumVariant-enable . t)
;;      (lsp-rust-analyzer-lens-references-method-enable . t)
;;      (lsp-rust-analyzer-lens-references-trait-enable . t)
;;      (lsp-rust-analyzer-hover-actions-references-enable . t)
;;      (lsp-rust-analyzer-inlayHints-bindingModeHints-enable . t)
;;      ;;(lsp-rust-analyzer-rustc-source . "discover") ;; find rustc-dev in rust-src
;;      (rustic-format-trigger . 'on-save)
;;      (rustic-rustfmt-args . "--edition 2021")
;;      (rustic-rustfmt-config-alist . ((max_width . 120)))
;;      (indent-tabs-mode . nil)
;;      (whitespace-line-column . 120)
;;      (lsp-inlay-hint-enable . t)
;;      )
;;    :mode "\\.rs\\'"
;;    ;;(setq mode-line-compact t)
;;    )

;;  (leaf rust-auto-use :ensure t)

;;  (leaf lsp-mode
;;    :ensure t
;;    :hook (prog-mode-hook . lsp-mode)
;;    :custom `((gc-cons-threshold . 300000000)
;;    	      (read-process-output-max . ,(* 10 1024 1024)))
;;    :config
;;    (defun lsp-booster--advice-json-parse (old-fn &rest args)
;;      "Try to parse bytecode instead of json."
;;      (or
;;       (when (equal (following-char) ?#)
;;    	 (let ((bytecode (read (current-buffer))))
;;           (when (byte-code-function-p bytecode)
;;             (funcall bytecode))))
;;       (apply old-fn args)))
;;
;;    (advice-add (if (progn (require 'json)
;;                           (fboundp 'json-parse-buffer))
;;                    'json-parse-buffer
;;                  'json-read)
;;    		:around
;;    		#'lsp-booster--advice-json-parse)
;;
;;    (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
;;      "Prepend emacs-lsp-booster command to lsp CMD."
;;      (let ((orig-result (funcall old-fn cmd test?)))
;;    	(if (and (not test?)                             ;; for check lsp-server-present?
;;    		 (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
;;    		 lsp-use-plists
;;    		 (not (functionp 'json-rpc-connection))  ;; native json-rpc
;;    		 (executable-find "emacs-lsp-booster"))
;;            (progn
;;    	      (message "Using emacs-lsp-booster for %s!" orig-result)
;;    	      (cons "emacs-lsp-booster" orig-result))
;;          orig-result)))
;;
;;    (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

  (use-package nix-ts-mode :ensure t)

  (use-package yaml-mode :ensure t)

  (use-package yasnippet-capf :ensure t)

  (use-package ispell :config (setq ispell-program-name "hunspell")))
