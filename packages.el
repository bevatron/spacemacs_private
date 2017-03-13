;;; packages.el --- personal-setting layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Chris Chen <chrisc@luna.in.cog.systems>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `personal-setting-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `personal-setting/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `personal-setting/pre-init-PACKAGE' and/or
;;   `personal-setting/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst personal-setting-packages
  '(
    diff-hl
    diff-mode
    cc-mode
    tramp
    )
  "The list of Lisp packages required by the personal-setting layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A lis
t of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

;; C/C++ style hook
(defun MyCHook ()
  (setq c-doc-comment-style
        '((c-mode . 'gtkdoc)
          (c++-mode . 'gtkdoc)
          ))

  (setq fci-rule-column '80)
  (setq fci-rule-width 5)
  (fci-mode)

  "setup shared by all languages (java/groovy/c++ ...)"
  (setq c-basic-offset 4)
  (setq tab-width 4)

  (setq comment-start "/* " comment-end " */")
  ;;  (c-set-offset 'case-label '+)
  )

(defun personal-setting/post-init-diff-mode ()
  (evil-set-initial-state 'diff-mode 'normal)
  )

(defun personal-setting/post-init-diff-hl()
  (setq diff-hl-side 'left)
  )

(defun personal-setting/post-init-cc-mode ()
  ;; C/C++ setting
  (add-hook 'c-mode-hook 'MyCHook)
  (add-hook 'c++-mode-hook 'MyCHook)
  (setq which-func-modes '(c++-mode c-mode org-mode java-mode))

  ;; man page setting
  (setenv "MANWIDTH" "72")

  ;; gitgutter
  ;;(setq git-gutter-fr+-side 'left-fringe)

  ;; show line number
  ;;(setq-default dotspacemacs-line-numbers 't)

  ;; C-C++
  (setq-default dotspacemacs-configuration-layers
                '((c-c++ :variables
                         c-c++-default-mode-for-headers 'c++-mode)))
  ;; (editorconfig-mode 1)
  )

(defun personal-setting/init-tramp ()
  (use-package tramp
    :defer t
    :config
    (progn
      ;; set tramp
      (setq tramp-default-method "ssh")
      (setq password-cache-expiry 36000)
      (add-to-list 'tramp-remote-path "~/tools_bin/bin/")
      (add-to-list 'tramp-remote-path "/usr/bin/")
      (add-to-list 'tramp-remote-path "/bin/")
      (add-to-list 'backup-directory-alist
                   (cons "." "~/.emacs.d/backups/"))
      (setq tramp-backup-directory-alist backup-directory-alist)
      )
    )
  )


;;; packages.el ends here
