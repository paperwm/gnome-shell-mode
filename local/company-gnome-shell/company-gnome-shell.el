

(defun company-gnome-shell--candidates ()
  "Candidates handler for the company backend."
  (cons :async
        (lambda (cb)
          (company-gnome-shell--inject-lua-helper)
          (let* ((context (gnome-shell--name-at-point))
                 (raw-result (gnome-shell-cmd
                              (format "emacs.completion_candidates(\"%s\")" context)))
                 (result (split-string (read raw-result))))
            (funcall cb result)))))

(defun company-gnome-shell--prefix ()
  (unless (company-in-string-or-comment)
    ;; Trigger completion at once if the immediate left char is '.' or ':'
    ;; (ignoring company-minimum-prefix-length).
    ;; See 'prefix' documentation in company.el
    (or (company-grab-symbol-cons "[.:]" 1)
        'stop)))

(defun company-gnome-shell--inject-lua-helper ()
  ;; lua5.2 -> _ENV, lua5.2 -> getfenv()
  (gnome-shell-send-string
   "
      if emacs == undefined then
        emacs = {}
        function emacs.completion_candidates(str)
          local env = _ENV or getfenv()
          local completions = mod_query.do_complete_lua(env, str)
          return table.concat(completions, \" \")
        end
      end"
   ))

(defun company-gnome-shell (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-gnome-shell))
    (prefix (company-gnome-shell--prefix))
    (candidates (company-gnome-shell--candidates))
    (duplicates t)
    (sorted nil)))

(provide 'company-gnome-shell)
