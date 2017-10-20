(define-globalized-minor-mode evil-cleverparens-global-mode evil-cleverparens-mode evil-cleverparens-mode)
(add-hook 'smartparens-mode-hook #'evil-cleverparens-mode)

(windmove-default-keybindings)
(electric-indent-mode nil)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome"
      auto-completion-return-key-behavior nil
      auto-completion-tab-key-behavior 'complete)
