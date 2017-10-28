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

(defconst gnome-shell-gjs-documentation-url
  "https://people.gnome.org/~gcampagna/docs/")
(defconst gnome-symbol-query-url
  "https://developer.gnome.org/symbols/")

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
  (let* ((result-obj (gnome-shell-eval (buffer-substring start end)))
         (successp (eq (alist-get 'success result-obj) t)) ;; false -> :json-false..
         (result   (alist-get 'value result-obj))
         (is-undefined (alist-get 'undefined result-obj))
         ;; The result is already reasonable pretty, but we need to represent
         ;; null values
         (pp-result (if result result
                      (if is-undefined "undefined" "null"))))
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
          (insert (if (string-equal "" pp-result)
                      ;; replace-regexp-in-string is buggy for empty strings
                      marker
                    (replace-regexp-in-string "^" marker pp-result))))
        ))

    (when (or show-result insert-result)
      ;; This was an interactive action so we give the user appropriate feedback

      ;; pulse.el is part of cedet. An alternative separate package is
      ;; flash-region (don't seem to animate the flash)
      (pulse-momentary-highlight-region start end
                                        (if successp
                                            'diff-refine-added
                                          'diff-refine-removed))

      (flycheck-clear) ;; wtf, no way to clear by error id?!
      (when (not successp)
        ;; Ensure that error occured in the evaled code
        ;; If not, we can check the stacktrace for where the failing code was
        ;; called
        (let* ((column (alist-get 'columnNumber result-obj))
               (line   (alist-get 'lineNumber result-obj))
               (buf-line   (+ (line-number-at-pos start) line -1)) ;; 1 vs 0 indexed
               (buf-column (+ (save-excursion
                                (goto-char start)
                                (current-column))
                              column)))

          ;; FIXME: When using gnome-shell-repl the flycheck error is cleared for some reason?
          (flycheck-add-overlay 
           (flycheck-error-new-at buf-line buf-column 'error result
                                  :id 'gnome-shell-repl-error)))))

    (when show-result
      (let ((presented-result pp-result)
            presented-face)

        (cond ((and successp is-undefined)
               (setq presented-face 'success)
               (setq presented-result "[No return value]"))
              ((not successp)
               (setq presented-face 'error)
               (when is-undefined
                 (setq presented-result "[Error without message]"))))

        (message (if presented-face (propertize presented-result 'face presented-face)
                   presented-result))))

    result))

(defun gnome-shell--dbus-bootstrap-eval (cmd)
  "Function to bootstrap our own Eval"
  (dbus-call-method :session "org.gnome.Shell" "/org/gnome/Shell"
                    "org.gnome.Shell" "Eval" cmd))

(defun gnome-shell--dbus-eval (cmd)
  "Raw dbus eval call. Returns a list: (success/boolean result/string)"
  (dbus-call-method :session "org.gnome.Shell" "/gnome/shell/mode"
                    "gnome.shell.mode" "Eval"
                    cmd (projectile-project-root) (gnome-shell--module-path)))

(defun gnome-shell--module-path ()
  (file-relative-name (buffer-file-name) (projectile-project-root)))

(defun gnome-shell-eval (code)
  "Evaluates `code' in gnome-shell and returns an alist:
  'success : true if no error occured
If success:
  'value : semi-pretty result value
  'raw_value : The raw value serialized to JSON and decoded to lisp equivalent values
If error:
  Most properties from Error. Eg. 'message, 'stack, 'lineNumber, 'columnNumber,
'file"
  (unless (car (gnome-shell--dbus-bootstrap-eval "emacs === undefined"))
    ;; send init code
    (message "sending init code")
    (with-temp-buffer
      (insert-file-contents gnome-shell--helper-path)
      (gnome-shell--dbus-bootstrap-eval (buffer-string))))

  ;; HACK: The init code changes the Eval method
  (destructuring-bind (successp jsonres)
      (gnome-shell--dbus-eval code)
    (json-read-from-string jsonres)))

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
  (gnome-shell-eval cmd))


(defun gnome-shell-client-list ()
  "Return the list of the managed clients"
  (throw "not implemented yet"))


(defun gnome-shell-goto-client (name)
  ;;(interactive (list (ido-completing-read "select: " '("a" "aaab" "a/b" "a/b/c" "x/z"))))
  (interactive (list (ido-completing-read "select: " (gnome-shell-client-list))))
  (throw "not implemented yet"))

(defun gnome-shell--lookup-symbol-candidates (partial-symbol)
  ;; Endpoint source: https://git.gnome.org/browse/library-web/tree/web/api.py
  (let ((query (concat gnome-symbol-query-url "lookup/" partial-symbol "?")))
    (with-current-buffer (url-retrieve-synchronously query)
      (progn
        ;; WTF!!.. I have to parse the response myself?!
        ;; I don't really have time for that shit, so just assume things went well
        ;; Note: Seems the async method at least provides a parsed status..
        (goto-char (point-min))
        (forward-evil-paragraph)
        (forward-line)
        (let ((lines 
               (split-string (buffer-substring (point) (point-max)) "\n")))
          (kill-buffer)
          lines))))
  )

(defun gnome-shell-look-up-function-at-point ()
  (interactive)
  (let* ((funcname (car (last (split-string (gnome-shell--name-at-point)
                                            "\\."))))
         (candidates (gnome-shell--lookup-symbol-candidates funcname))
         (candidate-count (length candidates))
         (selected-candidate))
    ;; (message "candidate-count %s" candidate-count)
    ;; (message "funcname %s" funcname)
    ;; (message "candidates %s" candidates)
    (cond ((= 1 candidate-count)
           (setq selected-candidate (first candidates))
           (message "Opening reference page"))
          ((>= candidate-count 50)
           ;; /symbols/lookup return max 50 results
           (message "Too many results, redirect to search page")
           (setq selected-candidate funcname))
          ((= 0 candidate-count)
           (message "No results"))
          (t
           (setq selected-candidate (completing-read "Candidates: " candidates))
           (when selected-candidate (message "Opening reference page"))))
    (when selected-candidate
        (browse-url (concat gnome-symbol-query-url "?q=" selected-candidate))
      )))

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


