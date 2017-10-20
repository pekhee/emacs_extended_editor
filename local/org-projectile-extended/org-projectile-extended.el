;;; org-projectile-extended.el --- Org Capture + projectile integration

;; Copyright (C) 2017 Pooyan Khosravi.

;; Author: Pooyan Khosravi <pekhee@gmail.com>
;; Version: 0.1
;; Package-Requires: ((org-projectile) (s) (cl))
;; Keywords: org-mode, projectile, org-capture, org-capture-templates

;;; Commentary:

;; * Why?
;;   org-projectile has Low utility to complexity/hassle ratio; get more utility
;;   much easier. Install this sand you'll get default project aware org-capture
;;   templates.

;;   Customization happens through org-projectile-extended function.

;;   Of course this wasn't the only reason.
;;   | org-projectile-extended       | org-projectile   |
;;   |-------------------------------+------------------|
;;   | Git compatible                | Git incompatible |
;;   | Target file+heading or lambda | Target file      |
;;   | Any Type                      | Only entry       |
;;   | Project related template vars | Vanila org-mode  |

;; * Usage
;;   Install. That's it. There is a template for FIXME and one for TODO that use
;;   file path as header and link to this line.
;;

;;; code:

(require 's)
(require 'cl)
(require 'org)
(require 'org-projectile)

(defgroup org-projectile-extended ()
  "Register project aware org-capture templates"
  :prefix "org-projectile-extended-" :group 'org)

(defvar org-projectile-extended-relative-file "TODOS.org")

;; Target
(defun org-projectile-extended:default-target ()
  "Target list."
  '(function org-projectile-extended:goto-target))
(defun org-projectile-extended:goto-target ()
  (or
   (org-projectile-extended:goto-current-entry)
   (org-projectile-extended:goto-heading)))
(defun org-projectile-extended:goto-heading ()
  (org-projectile-extended:insert-path-or-goto-namespace
   (org-projectile-extended:entry-heading)))
(defun org-projectile-extended:goto-current-enty ()
  (find-file-other-window
   (org-projectile-extended:current-entry-location)))

;;;autoload
(defun org-projectile-extended (key template
                                    &optional description type
                                    &rest passthrough)
  "Define a project aware `org-capture' template.

  - KEY (string): used to select template
  - TEMPLATE (string): usual `org-capture-templates' template
    and project related vars
  - DESCRIPTION (string): used to describe template when selecting it
  - TYPE (symbol): what kind of node does this template create
  - PASSTHROUGH: pass `org-capture-templates'

Available variables
  - ${link} relative link to captured file
  - ${file} file name relative to project root
  - ${name} project name
  - ${heading-stars} correct number of stars for creating a nested heading"
  (let* ((target (org-projectile-extended:default-target))
         (template
          (org-projectile-extended:with-differed-template-vars template)))
    (add-to-list 'org-capture-templates
                 `(,key
                   ,description
                   ,type
                   ',target
                   ',template))))

(defun org-projectile-extended:with-template-vars (template)
  "An alist containing project related vars."
  (let* ((link (org-projectile-extended:original-file-link))
         (name (projectile-project-name))
         (file
          (file-relative-name (buffer-file-name) (projectile-project-root)))
         (level (length (org-projectile-extended:entry-heading)))
         (next-level (+ 1 level))
         (heading-stars (s-repeat next-level "*")))
    (eval `(s-lex-format ,template))))

(defun org-projectile-extended:with-differed-template-vars (template)
  (let ((name (wrap-lambda
               (lambda ()
                 (org-projectile-extended:with-template-vars template)))))
    `(function ,name)))

(defmacro wrap-lambda (l)
  "Wrap a lambda inside an anonymous function"
  (let* ((name (gensym "wrap-lambda-wrapped-"))
         (arglist (elt l 1))
         (decl `(defun ,name ,arglist
                  (funcall ,l ,@arglist))))
    `(progn ,decl ',name)))

(wrap-lambda (lambda (e) (print e)))

(defun org-projectile-extended:current-entry-location ()
  "Current project's associated `org-mode' file."
  (let* ((root (projectile-project-root))
         (fname (expand-file-name org-projectile-extended-relative-file root)))
    fname))

(defun org-projectile-extended:entry-heading ()
  "Org style headers from file name relative to project root."
  (with-current-buffer (org-capture-get :original-buffer)
    (let* ((project-dir (projectile-project-root))
          (current-file (buffer-file-name))
          (project-name-and-namespace
           (file-name-sans-extension
            (file-relative-name current-file project-dir)))
          (project-name-tokens
           (remove "" (s-split "/" project-name-and-namespace))))
      project-name-tokens)))

(defun org-projectile-extended:original-file-link ()
  "Org style relative link."
  (with-current-buffer (org-capture-get :original-buffer)
    (let* ((project-dir (projectile-project-root))
           (current-file (buffer-file-name))
           (relative-name (file-relative-name current-file project-dir))
           (ln (line-number-at-pos)))
      (s-lex-format "[[file:${relative-name}::${ln}]]"))))

;; This is about finding correct position in target file

(defun org-projectile-extended:insert-path-or-goto-namespace (namespace)
  (beginning-of-buffer)
  (org-projectile-extended:require-org-mode)
  (org-projectile-extended:insert-or-goto-heading (first namespace))
  (save-restriction
    (org-projectile-extended:narrow-to-current-heading)
    (org-projectile-extended:insert-path-or-goto-namespace (rest namespace))))

(defun org-projectile-extended:insert-or-goto-heading (heading level)
  (org-projectile-extended:require-org-mode)
  (unless (org-projectile-extended:search-for-heading heading)
    (progn
      (goto-char (point-max))
      (unless (bolp) (newline))
      (org-insert-heading-respect-content t)
      (insert heading)))
  (nth 4 (org-heading-components)))

(defun org-projectile-extended:narrow-to-current-heading ()
  (let ((end (save-excursion
               (org-back-to-heading)
               (org-get-next-sibling)))
        (beginning (save-excursion
                     (org-back-to-heading)
                     (point))))
    (narrow-to-region beginning end)))

(defun org-projectile-extended:search-for-heading (heading)
  (org-goto-local-search-headings heading nil t))

(defun org-projectile-extended:require-org-mode ()
  (unless (derived-mode-p 'org-mode)
    (error
     "Target buffer \"%s\" for file+headline should be in Org mode"
     (current-buffer))))

;;;autoload
(eval-after-load 'org
  '(progn (org-projectile-extended "f"
                           "[ ] %? ${link}\n"
                           "PROJECTILE: FIXME"
                           'checkitem)

  (org-projectile-extended "t"
                           "${heading-stars} TODO %? ${link}\n"
                           "PROJECTILE: TODO"
                           'entry)))

(provide 'org-projectile-extended)
;;; org-projectile-extended ends here
