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
    helm
    evil
    spaceline
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

  (which-func-mode)

  ;; "setup shared by all languages (java/groovy/c++ ...)"
  (setq c-basic-offset 4)
  (setq tab-width 4)

  (c-set-offset 'innamespace 0)
  (c-set-offset 'substatement-open 0)

  (setq comment-start "/* " comment-end " */")
  ;;  (c-set-offset 'case-label '+)
  )

(defun personal-setting/post-init-diff-mode ()
  (evil-set-initial-state 'diff-mode 'normal)
  )

(defun personal-setting/post-init-diff-hl()
  (setq diff-hl-side 'left)
  )

(defun personal-setting/post-init-helm()
  (progn
    (spacemacs/set-leader-keys
      "fw"   'helm-find
      )
    (setq helm-buffer-max-length 'nil)
    (setq helm-ag-command-option "-i")
    ;; ;; rebind tab to do persistent action
    ;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    ;; ;; make TAB works in terminal
    ;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
    ;; ;; list actions using C-z
    ;; (define-key helm-map (kbd "C-z") 'helm-select-action)
    )
  )

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(defun personal-setting/post-init-evil ()
  (setq-default evil-escape-key-sequence "gq")
  )

(defun personal-setting/post-init-spaceline ()
  (spaceline-toggle-version-control-off)
  )

(defun personal-setting/post-init-cc-mode ()
  ;; C/C++ setting
  (progn
   (add-hook 'c-mode-hook 'MyCHook)
   (add-hook 'c++-mode-hook 'MyCHook)
   (add-hook 'java-mode-hook 'MyCHook)
   (setq which-func-modes '(c++-mode c-mode org-mode java-mode))

   ;; man page setting
   (setenv "MANWIDTH" "72")

   (setq-default dotspacemacs-configuration-layers
                 '((c-c++ :variables
                          c-c++-default-mode-for-headers 'c++-mode)))

   (c-add-style
    "linux-tabs-only"
    '("linux" (c-offsets-alist
               (arglist-cont-nonempty
                c-lineup-gcc-asm-reg
                c-lineup-arglist-tabs-only))))

   (setq-default dotspacemacs-configuration-layers
                 '((c-c++ :variables
                          c-c++-default-mode-for-headers 'c++-mode)))
   ;; gitgutter
   ;; (setq git-gutter-fr+-side 'left-fringe)

   ;; show line number
   ;;(setq-default dotspacemacs-line-numbers 't)

   ;; (editorconfig-mode 1)
  )
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
