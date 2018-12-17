;;; comany-gnome-shell-mode.el --- completion -*- lexical-binding: t -*-

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
  (cl-case command
    (interactive (company-begin-backend 'company-gnome-shell))
    (prefix (company-gnome-shell--prefix))
    (candidates (company-gnome-shell--candidates))
    (duplicates t)
    (sorted nil)))

(provide 'company-gnome-shell)
