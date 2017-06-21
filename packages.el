;;; packages.el --- extended-editor layer packages file for Spacemacs.
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `extended-editor-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `extended-editor/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `extended-editor/pre-init-PACKAGE' and/or
;;   `extended-editor/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst extended-editor-packages
  '(editorconfig)
  "The list of Lisp packages required by the extended-editor layer. ")
