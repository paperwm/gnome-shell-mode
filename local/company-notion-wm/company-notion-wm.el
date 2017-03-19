

(defun company-notion-wm--candidates ()
  "Candidates handler for the company backend."
  (cons :async
        (lambda (cb)
          (company-notion-wm--inject-lua-helper)
          (let* ((context (notion-wm--name-at-point))
                 (raw-result (notion-wm-cmd
                              (format "emacs.completion_candidates(\"%s\")" context)))
                 (result (split-string (read raw-result))))
            (funcall cb result)))))

(defun company-notion-wm--prefix ()
  (unless (company-in-string-or-comment)
    ;; Trigger completion at once if the immediate left char is '.' or ':'
    ;; (ignoring company-minimum-prefix-length).
    ;; See 'prefix' documentation in company.el
    (or (company-grab-symbol-cons "[.:]" 1)
        'stop)))

(defun company-notion-wm--inject-lua-helper ()
  ;; lua5.2 -> _ENV, lua5.2 -> getfenv()
  (notion-wm-send-string
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

(defun company-notion-wm (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-notion-wm))
    (prefix (company-notion-wm--prefix))
    (candidates (company-notion-wm--candidates))
    (duplicates t)
    (sorted nil)))

(provide 'company-notion-wm)
