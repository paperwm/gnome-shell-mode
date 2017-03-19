;;; packages.el --- gnome-shell layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author:  Ole Jørgen and Tor Hedin
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
;; added to `gnome-shell-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `gnome-shell/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `gnome-shell/pre-init-PACKAGE' and/or
;;   `gnome-shell/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst gnome-shell-packages
  '(company
    lua-mode
    (gnome-shell-eldoc-mode :location local)
    (gnome-shell-mode :location local)
    (company-gnome-shell :location local)
    flycheck)
  "The list of Lisp packages required by the notion layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
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

(defun gnome-shell/init-gnome-shell-mode ()
  (use-package gnome-shell-mode
    :commands (gnome-shell-mode)
    :init
    (progn
      (add-hook 'gnome-shell-mode-hook #'gnome-shell-eldoc-mode))
    :config
    (progn
      ;; (spacemacs/set-leader-keys-for-major-mode 'gnome-shell-mode "db" 'gnome-shell-send-buffer)
      (spacemacs/set-leader-keys-for-major-mode 'gnome-shell-mode
        "sb" 'gnome-shell-send-buffer
        "sf" 'gnome-shell-send-proc
        "sl" 'gnome-shell-send-current-line
        "sr" 'gnome-shell-send-region
        "hh" 'gnome-shell-look-up-notion-function-at-point)
      (spacemacs/declare-prefix-for-mode 'gnome-shell-mode "mh" "documentation")
      (spacemacs/declare-prefix-for-mode 'gnome-shell-mode "ms" "send to Notion"))
    )
  )

(defun gnome-shell/init-company-gnome-shell ()
  (use-package company-gnome-shell
    :if (configuration-layer/package-usedp 'company)
    :commands (company-gnome-shell)
    :init
    (progn
      ;; (require 'company)
      (spacemacs|add-company-backends :backends company-gnome-shell :modes gnome-shell-mode)

      )))

(defun gnome-shell/init-gnome-shell-eldoc-mode ()
  (use-package gnome-shell-eldoc-mode))

(defun gnome-shell/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'gnome-shell-mode))

(defun gnome-shell/post-init-gnome-shell-mode ()
  ;; (spacemacs|add-company-hook gnome-shell-mode)
  )

;;; packages.el ends here
