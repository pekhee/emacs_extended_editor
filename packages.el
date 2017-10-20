(defconst extended-editor-packages
  '(editorconfig delight s (org-projectile-extended :location local))
  "Lisp packages needed for extended-editor.")

(defun extended-editor/init-s () (use-package s))

(defun extended-editor/init-editorconfig () (use-package editorconfig
                                              :config (editorconfig-mode)))

(defun extended-editor/init-delight () (use-package delight
                                         :after (editorconfig evil-cleverparens)
                                         :config (delight '((evil-cleverparens-mode "(C)" evil-cleverparens)
                                                           (editorconfig-mode nil)))))

(defun extended-editor/init-org-projectile-extended () (use-package org-projectile-extended
                                                         :after org))
