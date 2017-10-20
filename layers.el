(let* ((general-layers '(auto-completion better-defaults (colors :variables colors-enable-rainbow-identifiers t)
                         deft gtags (ibuffer :variables ibuffer-group-buffers-by 'projects) nixos org restclient
                         semantic (shell :variables shell-default-height 30 shell-default-position 'bottom)
                         smex spell-checking syntax-checking) )
       (configuration-file-layers '(ansible))
       (framework-layers '(react ruby-on-rails))
       (language-layers '(c-c++ faust clojure elixir emacs-lisp erlang go haskell html java javascript latex markdown (python :variables python-sort-imports-on-save t python-enable-yapf-format-on-save t) ruby rust scala shell-scripts sql yaml nginx csv systemd))
       (version-control-layers '(git github))
       (tool-layers '(dash
                      imenu-list
                      pandoc
                      (ranger :variables ranger-show-preview t ranger-cleanup-on-disable t ranger-show-dotfiles t)))
       (vim-layers '(evil-cleverparens evil-commentary evil-snipe vim-empty-lines vinegar))
       (all-layer-names (mapcar
                         (lambda (e) (intern (concat e "-layers")))
                         '("general" "configuration-file" "framework" "language" "version-control" "tool" "vim")))
       (all-layers (apply #'append (mapcar (lambda (e) (eval e)) all-layer-names))))
  (configuration-layer/declare-layers all-layers))
