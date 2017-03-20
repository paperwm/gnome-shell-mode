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

;; Put the following in your .emacs to make the gnome-shell-mode function available:
;; (autoload 'gnome-shell-mode "notion" "Major mode to edit notion config files" t)

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

(defvar gnome-shell-display-target ":0"
  "The DISPLAY to target. Useful when debugging a separate notion in eg. a Xephyr server")

(defconst gnome-shell--lua-helper-path
  (concat (file-name-directory (or load-file-name buffer-file-name))
          "emacs.lua"))

(defconst gnome-shell-documentation-url
  "http://notion.sourceforge.net/notionconf/")

(defun gnome-shell--name-at-point ()
  "Get current Name { ['.'|':'} Name } sequence."
  ;; Taken from lua-mode.el
  ;; NB: copying/modifying syntax table for each call may incur a penalty
  (with-syntax-table (copy-syntax-table)
    (modify-syntax-entry ?. "_")
    (modify-syntax-entry ?: "_")
    (current-word t)))

(defun gnome-shell-run-interactively (cmd insert-result show-result)
  "Helper that handles common options relevant for interactive commands"

  (let ((result (gnome-shell-run cmd)))

    (when insert-result
      (save-excursion
        (end-of-line)

        (if (eobp)
            (newline)
          (forward-line))

        (while (looking-at "//: ")
          (kill-whole-line))

        (newline)
        (forward-line -1)

        (insert (replace-regexp-in-string "^" "//: " result))
        ))

    (when show-result
      (message result))

    result))

(defun gnome-shell-run (cmd)
  (let* ((response (dbus-call-method :session "org.gnome.Shell" "/org/gnome/Shell"
                                   "org.gnome.Shell" "Eval" cmd))
         (result (cadr response)))
    (message result)

    result))

(defun gnome-shell-send-string (str)
  "Send STR to notion, using the notionflux program."
  (gnome-shell-run str))

(defun gnome-shell-send-region (start end &optional insert-result)
  "Send send the region to notion, using the notionflux program."
  (interactive "r\nP")
  (gnome-shell-run-interactively (buffer-substring start end)
                                          insert-result (called-interactively-p)))

(defun gnome-shell-send-current-line (&optional insert-result)
  "Send send the actual line to notion, using the notionflux program."
  (interactive "P")
  (gnome-shell-run-interactively (buffer-substring (line-beginning-position) (line-end-position))
                                          insert-result (called-interactively-p)))

(defun gnome-shell-repl ()
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

          (gnome-shell-run-interactively cmd t nil)))
      )))

(defun gnome-shell-send-proc ()
  "Send proc around point to notion."
  (interactive)
  (let (start end)
    (save-excursion
      (lua-beginning-of-proc)
      (setq start (point))
      (lua-end-of-proc)
      (setq end (point)))
    (gnome-shell-send-region start end)))

(defun gnome-shell-send-buffer ()
  "Send send the buffer content to notion, using the notionflux program."
  (interactive)
  (gnome-shell-send-region (point-min) (point-max)))


(defun gnome-shell-cmd (cmd &optional insert-result)
  "Send a command to notion.
The command is prefixed by a return statement."
  (interactive "sNotion cmd: \nP")
  (gnome-shell-run-interactively cmd insert-result (called-interactively-p)))


;; --------------------------------------------------------------------------------
;; Utility functions that need gnome-shell-emacs.lua
;; --------------------------------------------------------------------------------

(defun gnome-shell-client-list ()
  "Return the list of the notion clients."
  (let* ((s (gnome-shell-cmd "emacs.list_clients()"))
         (s0 (substring s 1 (- (length s) 2)))
         (client-list (split-string s0 "\\\\\n")))
    client-list))


;; (ido-completing-read "notion window: " (gnome-shell-client-list) t t nil nil (car (gnome-shell-client-list)))

(defun gnome-shell-goto-client (name)
  ;;(interactive (list (ido-completing-read "select: " '("a" "aaab" "a/b" "a/b/c" "x/z"))))
  (interactive (list (ido-completing-read "select: " (gnome-shell-client-list))))
  (gnome-shell-send-string (concat "WRegion.goto(ioncore.lookup_clientwin(\"" name "\"))")))

(defun gnome-shell-look-up-function-at-point ()
  (interactive)
  ;; Documentation still uses ioncore instead of notioncore
  (let* ((funcname (replace-regexp-in-string "^notioncore\\." "ioncore."
                                             (gnome-shell--name-at-point)))
         (lua-req (format "return emacs.canonical_funcname(\"%s\")" funcname))
         (canonical-funcname (read (gnome-shell-send-string lua-req))) ;; CLEANUP
         (url (concat gnome-shell-documentation-url
                      "node7.html#fn:" canonical-funcname)))
    (browse-url url))
  )

(defun gnome-shell-eldoc (function-name)
  (read (gnome-shell-send-string (format "return emacs.eldoc(\"%s\")" function-name))))

(defun gnome-shell--resolve-lua-source-file (relative-path)
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

(defun gnome-shell-goto-definition (function-name)
  (interactive (list (gnome-shell--name-at-point)))
  ;; Hackety-hack...
  (let* ((raw (gnome-shell-send-string (format "return emacs.defined_at(\"%s\")" function-name)))
         (as-string (and raw (read raw)))
         (location (and as-string (read as-string))))

    (when location
      (let* ((path          (car location))
             (line-number   (cadr location))
             (resolved-path (if (file-name-absolute-p path)
                                path
                              (gnome-shell--resolve-lua-source-file path))))
        (when resolved-path
          (find-file resolved-path)
          (goto-line line-number)
          resolved-path)))))

;; --------------------------------------------------------------------------------
;; The notion edit mode, based on lua mode
;; --------------------------------------------------------------------------------

(defvar gnome-shell-mode-map () "Keymap used in `gnome-shell-mode' buffers.")

(when (not gnome-shell-mode-map)
  (setq gnome-shell-mode-map (make-sparse-keymap))
  (define-key gnome-shell-mode-map [(control ?c) (control ?p)] 'gnome-shell-send-proc)
  (define-key gnome-shell-mode-map [(control ?c) (control ?r)] 'gnome-shell-send-region)
  (define-key gnome-shell-mode-map [(control ?c) (control ?b)] 'gnome-shell-send-buffer)
  (define-key gnome-shell-mode-map [(control ?c) (control ?l)] 'gnome-shell-send-line)
  (define-key gnome-shell-mode-map (kbd "C-<return>") 'gnome-shell-repl)
  )

(easy-menu-define gnome-shell-mode-menu gnome-shell-mode-map
"'gnome-shell-mode' menu"
                  '("Notion"
                    ("Interaction"
                    ["Send Procedure" gnome-shell-send-proc t]
                    ["Send Region" gnome-shell-send-region t]
                    ["Send Buffer" gnome-shell-send-buffer t]
                    ["Send String" gnome-shell-send-string t]
                    ["Send Line" gnome-shell-send-line t]
                    )
                    ["Goto client" gnome-shell-goto-client t]
                    ))

(define-derived-mode gnome-shell-mode js2-mode "notion"
  "gnome-shell-mode provides a tight integration of emacs and notion.
"
  (use-local-map gnome-shell-mode-map))

;; --------------------------------------------------------------------------------
;; various stuff for testing purposes
;; --------------------------------------------------------------------------------


;; (gnome-shell-send-string "ioncore.goto_next_screen()")
;; (gnome-shell-cmd "ioncore.goto_next_screen()")

;;(defun gnome-shell-show-message-for-cmd (cmd)
;;  (interactive "snotion command: ")
;;  (gnome-shell-run (concat "mod_query.message(ioncore.find_screen_id(0)," cmd ")")))


;; (gnome-shell-client-list)


;; (gnome-shell-show-message-for-cmd "ioncore.version()")
;; (gnome-shell-send-string "return ioncore.version()")
;; (gnome-shell-send-string "return 4+5")

;; (gnome-shell-cmd "ioncore.version()")
;; (gnome-shell-cmd "4+5")

 ;; (setenv "NOTIONFLUX_SOCKET" "/tmp/fileM5J57y")

;; things to support
;;table ioncore.clientwin_list()

;; WClientWin ioncore.lookup_clientwin(string name)

;; bool WRegion.goto(WRegion reg)

(provide 'gnome-shell-mode)

;;; notion.el ends here

;; arch-tag: 17c5fcf9-ea23-4ca5-b7d5-a0635b8b4230


