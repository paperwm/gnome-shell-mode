;;; company-gnome-shell.el --- Gnome Shell runtime js completion  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019 Tor Hedin Brønner and Ole Jørgen Brønner

;; Author: Tor Hedin Brønner <torhedinbronner@gmail.com>
;;      Ole Jørgen Brønner <olejorgenb@yahoo.no>
;; Homepage: https://github.com/paperwm/gnome-shell-mode
;; Version: 0.1
;; Package-Requires: ((emacs "24") (company "20181105") (gnome-shell-mode "0.1"))

;; This file is not part of GNU Emacs

;;; Code:

(require 'gnome-shell-mode)
(require 'company)

(defun company-gnome-shell--candidates ()
  "Candidates handler for the company backend."
  (cons :async
        (lambda (cb)
          (let* ((context (gnome-shell--name-at-point))
                 (candidates (gnome-shell--dbus-complete context)))
            (funcall cb candidates)))))

(defun company-gnome-shell--prefix ()
  (unless (company-in-string-or-comment)
    ;; Trigger completion at once if the immediate left char is '.' or ':'
    ;; (ignoring company-minimum-prefix-length).
    ;; See 'prefix' documentation in company.el
    (or (company-grab-symbol-cons "[.:]" 1)
        'stop)))

(defun company-gnome-shell (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (message "%s" command)
  (cl-case command
    (interactive (company-begin-backend 'company-gnome-shell))
    (prefix (company-gnome-shell--prefix))
    (candidates (company-gnome-shell--candidates))
    (duplicates t)
    (sorted nil)))

(provide 'company-gnome-shell)

;;; company-gnome-shell.el ends here
