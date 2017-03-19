;;; notion.el --- Tight integration of emacs with the notion window manager

;; Copyright (C) 2005-2006 by Stefan Reichör

;; Filename: notion.el
;; Author: Stefan Reichör, <stefan@xsteve.at>

;; notion.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; notion.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary

;; notion.el is an emacs interface for the notion window manager

;; You need mod_notionflux-3 (at least from 2005-04-21)
;; mod_notionflux-3 can be found here: http://modeemi.fi/~tuomov/repos/

;; Put the following in your .emacs to make the notion-wm-mode function available:
;; (autoload 'notion-wm-mode "notion" "Major mode to edit notion config files" t)

;; The latest version of notion.el can be found at http://www.xsteve.at/prg/emacs/notion.el

;; Comments / suggestions welcome!

;;; Todo
;;  * Better error handling - at the moment they are only shown on the
;;    terminal, where notion was started

;;; History:
;;

;;; Code:

;; --------------------------------------------------------------------------------
;; notion interaction via notionflux
;; --------------------------------------------------------------------------------

(defvar notion-wm-display-target ":0"
  "The DISPLAY to target. Useful when debugging a separate notion in eg. a Xephyr server")

(defconst notion-wm--lua-helper-path
  (concat (file-name-directory (or load-file-name buffer-file-name))
          "emacs.lua"))

(defconst notion-wm-documentation-url
  "http://notion.sourceforge.net/notionconf/")

(defun notion-wm--name-at-point ()
  "Get current Name { ['.'|':'} Name } sequence."
  ;; Taken from lua-mode.el
  ;; NB: copying/modifying syntax table for each call may incur a penalty
  (with-syntax-table (copy-syntax-table)
    (modify-syntax-entry ?. "_")
    (modify-syntax-entry ?: "_")
    (current-word t)))

(defun notion-wm-run-notionflux-interactively (cmd insert-result show-result)
  "Helper that handles common options relevant for interactive commands"

  (let ((result (notion-wm-run-notionflux cmd)))

    ;; notionflux return strings with line continuation
    (setq result (replace-regexp-in-string "\\\\$" "" result))

    (when insert-result
      (save-excursion
        (end-of-line)

        (if (eobp)
            (newline)
          (forward-line))

        (while (looking-at "--: ")
          (kill-whole-line))

        (newline)
        (forward-line -1)

        (insert (replace-regexp-in-string "^" "--: " result))
        (kill-whole-line)
        ))

    (when show-result
      (message result))

    result))

(defun notion-wm-run-notionflux (cmd)
  (let* ((wrapped (format "return emacs.eval(%s)" (lua-make-lua-string cmd)))
         (exit-code nil)
         (result nil))
    (with-temp-buffer
      ;; notionflux have a relative low limit on data accepted as arguments so
      ;; we need to pass the code through stdin.

      ;; Emacs seems to use buffers instead of stream objects communication
      ;; with external processes.

      (insert (format "if emacs == undefined then loadfile('%s')() end\n"
                      notion-wm--lua-helper-path))
      (insert wrapped)

      ;; Call notion flux with the temp buffer content, replacing it with the
      ;; output. (both stdout and stderr)
      (setq exit-code
            (let ((process-environment process-environment))
              (setenv "DISPLAY" notion-wm-display-target)
              (call-process-region (point-min) (point-max) "notionflux" t t)))
      (setq result (buffer-string))
      (when (< 0 exit-code)
        (error "notionflux failed (%s): %s" exit-code result))
      result)
    ))

(defun notion-wm-send-string (str)
  "Send STR to notion, using the notionflux program."
  (notion-wm-run-notionflux str))

(defun notion-wm-send-region (start end &optional insert-result)
  "Send send the region to notion, using the notionflux program."
  (interactive "r\nP")
  (notion-wm-run-notionflux-interactively (buffer-substring start end)
                                          insert-result (called-interactively-p)))

(defun notion-wm-send-current-line (&optional insert-result)
  "Send send the actual line to notion, using the notionflux program."
  (interactive "P")
  (notion-wm-run-notionflux-interactively (buffer-substring (line-beginning-position) (line-end-position))
                                          insert-result (called-interactively-p)))

(defun notion-wm-repl ()
  (interactive)
  (let (a b)
    (if (region-active-p)
        (setq a (min (point) (mark))
              b (max (point) (mark)))
      (setq a (line-beginning-position)
            b (line-end-position)))

    (let ((cmd (buffer-substring a b)))
      (save-excursion
        (goto-char b)

        (when (eq b (line-beginning-position))
          ;; What I actually want to check for is if the region is active and is
          ;; in "line mode". Then b will be at the line _after_ the last code
          ;; line selected

          ;; Maybe simply back up all blank lines too?
          (forward-line -1))

        (beginning-of-line)

        ;; IMPROVEMENT: might want to do this for the normal "send-" functions too?

        ;; Is the last statement an simple assignment?
        ;; If so - add a "return variable" to actually see the result
        ;; NB: This is a "minimal effort" thing. Probably lots of edge-cases with
        ;; strange behavior. eg. "foo={bar=2} return foo.bar"
        
        (let* ((found-match? (search-forward-regexp
                              "^\\s-*\\(?:local\\s-+\\)?\\([.:_a-z]+\\)\\s-*="
                              b t))
               (assigned-variable (and found-match? (match-string 1))))
          (when assigned-variable
            (setq cmd (concat cmd " return " assigned-variable)))

          (notion-wm-run-notionflux-interactively cmd t nil)))
      )))

(defun notion-wm-send-proc ()
  "Send proc around point to notion."
  (interactive)
  (let (start end)
    (save-excursion
      (lua-beginning-of-proc)
      (setq start (point))
      (lua-end-of-proc)
      (setq end (point)))
    (notion-wm-send-region start end)))

(defun notion-wm-send-buffer ()
  "Send send the buffer content to notion, using the notionflux program."
  (interactive)
  (notion-wm-send-region (point-min) (point-max)))


(defun notion-wm-cmd (cmd &optional insert-result)
  "Send a command to notion.
The command is prefixed by a return statement."
  (interactive "sNotion cmd: \nP")
  (notion-wm-run-notionflux-interactively cmd insert-result (called-interactively-p)))


;; --------------------------------------------------------------------------------
;; Utility functions that need notion-wm-emacs.lua
;; --------------------------------------------------------------------------------

(defun notion-wm-client-list ()
  "Return the list of the notion clients."
  (let* ((s (notion-wm-cmd "emacs.list_clients()"))
         (s0 (substring s 1 (- (length s) 2)))
         (client-list (split-string s0 "\\\\\n")))
    client-list))


;; (ido-completing-read "notion window: " (notion-wm-client-list) t t nil nil (car (notion-wm-client-list)))

(defun notion-wm-goto-client (name)
  ;;(interactive (list (ido-completing-read "select: " '("a" "aaab" "a/b" "a/b/c" "x/z"))))
  (interactive (list (ido-completing-read "select: " (notion-wm-client-list))))
  (notion-wm-send-string (concat "WRegion.goto(ioncore.lookup_clientwin(\"" name "\"))")))

(defun notion-wm-look-up-notion-function-at-point ()
  (interactive)
  ;; Documentation still uses ioncore instead of notioncore
  (let* ((funcname (replace-regexp-in-string "^notioncore\\." "ioncore."
                                             (notion-wm--name-at-point)))
         (lua-req (format "return emacs.canonical_funcname(\"%s\")" funcname))
         (canonical-funcname (read (notion-wm-send-string lua-req))) ;; CLEANUP
         (url (concat notion-wm-documentation-url
                      "node7.html#fn:" canonical-funcname)))
    (browse-url url))
  )

(defun notion-wm-eldoc (function-name)
  (read (notion-wm-send-string (format "return emacs.eldoc(\"%s\")" function-name))))

(defun notion-wm--resolve-lua-source-file (relative-path)
  ;; Byte compiled lua files contain file _name_ at best
  (let* ((candidates
          (remove-if-not (lambda (project-file-path)
                           (string-suffix-p relative-path project-file-path))
                         (projectile-current-project-files)))
         (project-file (if (rest candidates)
                           (completing-read "Source file" candidates)
                         (first candidates))))
    (if project-file
        (concat (projectile-project-root) project-file)
      (helm-find-files-1 relative-path))))

(defun notion-wm-goto-definition (function-name)
  (interactive (list (notion-wm--name-at-point)))
  ;; Hackety-hack...
  (let* ((raw (notion-wm-send-string (format "return emacs.defined_at(\"%s\")" function-name)))
         (as-string (and raw (read raw)))
         (location (and as-string (read as-string))))

    (when location
      (let* ((path          (car location))
             (line-number   (cadr location))
             (resolved-path (if (file-name-absolute-p path)
                                path
                              (notion-wm--resolve-lua-source-file path))))
        (when resolved-path
          (find-file resolved-path)
          (goto-line line-number)
          resolved-path)))))

;; --------------------------------------------------------------------------------
;; The notion edit mode, based on lua mode
;; --------------------------------------------------------------------------------

(defvar notion-wm-mode-map () "Keymap used in `notion-wm-mode' buffers.")

(when (not notion-wm-mode-map)
  (setq notion-wm-mode-map (make-sparse-keymap))
  (define-key notion-wm-mode-map [(control ?c) (control ?p)] 'notion-wm-send-proc)
  (define-key notion-wm-mode-map [(control ?c) (control ?r)] 'notion-wm-send-region)
  (define-key notion-wm-mode-map [(control ?c) (control ?b)] 'notion-wm-send-buffer)
  (define-key notion-wm-mode-map [(control ?c) (control ?l)] 'notion-wm-send-line)
  (define-key notion-wm-mode-map (kbd "C-<return>") 'notion-wm-repl)
  )

(easy-menu-define notion-wm-mode-menu notion-wm-mode-map
"'notion-wm-mode' menu"
                  '("Notion"
                    ("Interaction"
                    ["Send Procedure" notion-wm-send-proc t]
                    ["Send Region" notion-wm-send-region t]
                    ["Send Buffer" notion-wm-send-buffer t]
                    ["Send String" notion-wm-send-string t]
                    ["Send Line" notion-wm-send-line t]
                    )
                    ["Goto client" notion-wm-goto-client t]
                    ))

(define-derived-mode notion-wm-mode lua-mode "notion"
  "notion-wm-mode provides a tight integration of emacs and notion.
"
  (use-local-map notion-wm-mode-map))

;; --------------------------------------------------------------------------------
;; various stuff for testing purposes
;; --------------------------------------------------------------------------------


;; (notion-wm-send-string "ioncore.goto_next_screen()")
;; (notion-wm-cmd "ioncore.goto_next_screen()")

;;(defun notion-wm-show-message-for-cmd (cmd)
;;  (interactive "snotion command: ")
;;  (notion-wm-run-notionflux (concat "mod_query.message(ioncore.find_screen_id(0)," cmd ")")))


;; (notion-wm-client-list)


;; (notion-wm-show-message-for-cmd "ioncore.version()")
;; (notion-wm-send-string "return ioncore.version()")
;; (notion-wm-send-string "return 4+5")

;; (notion-wm-cmd "ioncore.version()")
;; (notion-wm-cmd "4+5")

 ;; (setenv "NOTIONFLUX_SOCKET" "/tmp/fileM5J57y")

;; things to support
;;table ioncore.clientwin_list()

;; WClientWin ioncore.lookup_clientwin(string name)

;; bool WRegion.goto(WRegion reg)

(provide 'notion-wm-mode)

;;; notion.el ends here

;; arch-tag: 17c5fcf9-ea23-4ca5-b7d5-a0635b8b4230


