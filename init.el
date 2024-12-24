(eval-and-compile
  (setenv "LSP_USE_PLISTS" "true")
  (setq lsp-use-plists t)

  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))

  (package-initialize)

  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :config
    (leaf-keywords-init))

  (leaf leaf-convert :ensure t)
  (leaf leaf-tree :ensure t)

  (leaf custom-file
    :config
    (setq custom-file "~/.emacs.d/custom.el")
    (condition-case nil
        (load custom-file)
      (error nil)))

  (leaf custom-set-faces
    :config
    (custom-set-faces
     '(default ((t (:family "Iosevka Term Slab" :foundry "BE5N" :slant normal
			    :weight normal :height 192 :width normal))))
     '(Info-quoted ((t (:family "Iosevka"))))
     '(fixed-pitch ((t (:family "Iosevka"))))
     '(fixed-pitch-serif ((t (:family "Iosevka"))))
     '(mode-line ((t (:background "gray30" :box (:line-width 1 :color "red")
				  :family "Iosevka"))))
     '(variable-pitch ((t (:family "Iosevka"))))))

  (leaf custom-startup
    :custom ((transient-mark-mode . t)
	     (show-paren-mode . t)
             (menu-bar-mode . nil)
             (indent-tabs-mode . t)
	     (which-function-mode . t)
	     (show-paren-delay . 0)
	     (display-time-24hr-format . t)
	     (show-trailing-whitespace . t)
	     (make-backup-files . nil)
	     (auto-save-default . nil)
	     (global-auto-revert-mode . t)
	     (column-number-mode . t))
    :config (when (display-graphic-p)
              (tool-bar-mode -1)
              (scroll-bar-mode -1)))

  (leaf whitespace
    :custom ((whitespace-style . '(face trailing lines-tail space-before-tab))
	     (whitespace-line-column . 80))
    :global-minor-mode global-whitespace-mode)

  (leaf vterm :ensure t)

  (leaf treesit-auto
    :ensure t
    :custom ((treesit-auto-install . 'prompt))
    :config
    (require 'treesit-auto)
    (treesit-auto-add-to-auto-mode-alist 'all)
    (global-treesit-auto-mode))

  (load-theme 'modus-vivendi))

(condition-case nil
    (load-file "~/.emacs.d/init-real.el")
  (error nil))
