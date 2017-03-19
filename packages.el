;;; packages.el --- notion-wm layer packages file for Spacemacs.
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
;; added to `notion-wm-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `notion-wm/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `notion-wm/pre-init-PACKAGE' and/or
;;   `notion-wm/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst notion-wm-packages
  '(company
    lua-mode
    (lua-eldoc-mode :location local)
    (notion-wm-mode :location local)
    (company-notion-wm :location local)
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

(defun notion-wm/init-notion-wm-mode ()
  (use-package notion-wm-mode
    :commands (notion-wm-mode)
    :init
    (progn
      (add-hook 'notion-wm-mode-hook #'lua-eldoc-mode))
    :config
    (progn
      ;; (spacemacs/set-leader-keys-for-major-mode 'notion-wm-mode "db" 'notion-wm-send-buffer)
      (spacemacs/set-leader-keys-for-major-mode 'notion-wm-mode
        "sb" 'notion-wm-send-buffer
        "sf" 'notion-wm-send-proc
        "sl" 'notion-wm-send-current-line
        "sr" 'notion-wm-send-region
        "hh" 'notion-wm-look-up-notion-function-at-point)
      (spacemacs/declare-prefix-for-mode 'notion-wm-mode "mh" "documentation")
      (spacemacs/declare-prefix-for-mode 'notion-wm-mode "ms" "send to Notion"))
    )
  )

(defun notion-wm/init-company-notion-wm ()
  (use-package company-notion-wm
    :if (configuration-layer/package-usedp 'company)
    :commands (company-notion-wm)
    :init
    (progn
      ;; (require 'company)
      (spacemacs|add-company-backends :backends company-notion-wm :modes notion-wm-mode)

      )))

(defun notion-wm/init-lua-eldoc-mode ()
  (use-package lua-eldoc-mode))

(defun notion-wm/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'notion-wm-mode))

(defun notion-wm/post-init-notion-wm-mode ()
  ;; (spacemacs|add-company-hook notion-wm-mode)
  )

;;; packages.el ends here
