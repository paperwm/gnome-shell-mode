;;; gnome-shell-mode.el --- Tight integration of emacs with gnome-shell

;; Based on notion-wm-mode which again is loosely based on notion.el by Stefan
;; Reichör

;; Filename: gnome-shell-mode.el
;; Authors:
;; - Tor Hedin Brønner <torhedinbronner@gmail.com>
;; - Ole Jørgen Brønner <olejorgenb@yahoo.no>

;; gnome-shell-mode.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; gnome-shell-mode.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary

;; gnome-shell-mode.el is an emacs interface to gnome-shell

;; Put the following in your .emacs to make the gnome-shell-mode function available:
;; (autoload 'gnome-shell-mode "gnome-shell" "Major mode to edit gnome-shell javascript files" t)

;; The latest version of gnome-shell-mode.el can be found at
;; https://github.com/hedning/gnome-shell-mode

;; Comments / suggestions welcome!

;;; History:
;;

;;; Code:


(defconst gnome-shell--helper-path
  (concat (file-name-directory (or load-file-name buffer-file-name))
          "emacs.js"))

(defconst gnome-shell-documentation-url
  "https://people.gnome.org/~gcampagna/docs/")

(defun gnome-shell--name-at-point ()
  "Get current Name { ['.'|':'} Name } sequence."
  ;; Taken from lua-mode.el
  ;; NB: copying/modifying syntax table for each call may incur a penalty
  (with-syntax-table (copy-syntax-table)
    (modify-syntax-entry ?. "_")
    (modify-syntax-entry ?: "_")
    (current-word t)))

(defun gnome-shell-run-interactively (start end insert-result show-result)
  "Helper that handles common options relevant for interactive commands"
  (destructuring-bind (successp result) (gnome-shell-run
                                         (buffer-substring start end))
    (when insert-result
      (save-excursion
        (end-of-line)

        (if (eobp)
            (newline)
          (forward-line))

        (while (looking-at "//[:!] ")
          (kill-whole-line))

        (newline)
        (forward-line -1)

        (let ((marker (if successp "//: " "//! ")))
          (insert (if (string-equal "" result)
                      ;; replace-regexp-in-string is buggy for empty strings
                      marker
                    (replace-regexp-in-string "^" marker result))))
        ))

    (when (or show-result insert-result)
      (pulse-momentary-highlight-region start end
                                        (if successp
                                            'diff-refine-added
                                          'diff-refine-removed)))
    (when show-result
      (let ((presented-result result)
            presented-face)

        (cond ((and successp (string-empty-p result))
               (setq presented-face 'success)
               (setq presented-result "[No return value]"))
              ((not successp)
               (setq presented-face 'error)
               (when (string-empty-p result)
                 (setq presented-result "[Error without message]"))))

        (message (if presented-face (propertize presented-result 'face presented-face)
                   presented-result))))

    result))

(defun gnome-shell-run (cmd)
  "Return a two-element list where `car' is a flag indicating success/failure,
and `cadr' is the stringified result/error message"
  (unless (car (gnome-shell--run "emacs"))
    ;; send init code
    (with-temp-buffer
      (insert-file-contents gnome-shell--helper-path)
      (gnome-shell--run (buffer-string))))

  (gnome-shell--run cmd))

(defun gnome-shell--run (cmd)
  (dbus-call-method :session "org.gnome.Shell" "/org/gnome/Shell"
                    "org.gnome.Shell" "Eval" cmd))

(defun gnome-shell-send-string (str)
  "Send STR to gnome-shell, using the dbus Eval method."
  (gnome-shell-run str))

(defun gnome-shell-send-region (start end &optional insert-result interactively)
  "Send send the region to gnome-shell, using the dbus Eval method."
  (interactive "r\nP\np")
  (gnome-shell-run-interactively start end insert-result interactively))

(defun gnome-shell-send-current-line (&optional insert-result interactively)
  "Send send the actual line to gnome-shell, using the dbus Eval method."
  (interactive "P\np")
  (gnome-shell-run-interactively (line-beginning-position) (line-end-position)
                                 insert-result interactively))

(defun gnome-shell-repl ()
  (interactive)
  (let (a b)
    (if (region-active-p)
        (setq a (min (point) (mark))
              b (max (point) (mark)))
      (setq a (line-beginning-position)
            b (line-end-position)))

    (save-excursion
      (goto-char b)

      (when (eq b (line-beginning-position))
        ;; What I actually want to check for is if the region is active and is
        ;; in "line mode". Then b will be at the line _after_ the last code
        ;; line selected

        ;; Maybe simply back up all blank lines too?
        (forward-line -1))

      (beginning-of-line)

      (gnome-shell-run-interactively a b t nil))))

(defun gnome-shell-send-proc (&optional interactively)
  "Send proc around point to gnome-shell."
  (interactive "p")
  (let (start end)
    (save-excursion
      (beginning-of-defun)
      (setq start (point))
      (end-of-defun)
      (setq end (point)))
    (gnome-shell-send-region start end nil interactively)))

(defun gnome-shell-send-buffer (&optional interactively)
  "Send send the buffer content to gnome-shell, using the dbus Eval method."
  (interactive "p")
  (gnome-shell-send-region (point-min) (point-max) nil interactively))


(defun gnome-shell-cmd (cmd &optional insert-result interactively)
  "Send a expression to gnome-shell."
  (interactive "sGnome shell cmd: \nPp")
  ;; FIXME: respect insert-result and interactively arguments
  (gnome-shell-run cmd))


(defun gnome-shell-client-list ()
  "Return the list of the managed clients"
  (throw "not implemented yet"))


(defun gnome-shell-goto-client (name)
  ;;(interactive (list (ido-completing-read "select: " '("a" "aaab" "a/b" "a/b/c" "x/z"))))
  (interactive (list (ido-completing-read "select: " (gnome-shell-client-list))))
  (throw "not implemented yet"))

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

;; --------------------------------------------------------------------------------
;; The gnome-shell edit mode, based on js2-mode
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

(define-derived-mode gnome-shell-mode js2-mode "gnome-shell"
  "gnome-shell-mode provides tight integration of emacs and gnome-shell.
"
  (use-local-map gnome-shell-mode-map))


(provide 'gnome-shell-mode)

;;; gnome-shell-mode.el ends here

;; arch-tag: 17c5fcf9-ea23-4ca5-b7d5-a0635b8b4230


