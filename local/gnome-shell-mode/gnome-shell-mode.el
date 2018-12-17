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
;; https://github.com/paperwm/gnome-shell-mode

;; Comments / suggestions welcome!

;;; History:
;;

;;; Code:

(eval-when-compile (require 'cl))

(require 'js2-mode)
(require 'dbus)
(require 'flycheck)
(require 'json)

(defconst gnome-shell--helper-path
  (file-name-directory (or load-file-name buffer-file-name)))

(defvar gnome-shell-dbus-address :session
  "The dbus address used for connections (value of DBUS_SESSION_BUS_ADDRESS)
NB: don't set directly, use `gnome-shell-set-dbus-address'")

(defconst gnome-shell-gjs-documentation-url
  "https://people.gnome.org/~gcampagna/docs/")
(defconst gnome-symbol-query-url
  "https://developer.gnome.org/symbols/")

(defun gnome-shell-set-dbus-address (address)
  "Set the dbus address. (set to :session for the default bus)"
  (interactive "sDBus address: ")
  (if (or (equal address "")
          (equal address ":session"))
      (setq address :session))
  (dbus-init-bus address)
  (setq gnome-shell-dbus-address address))

(defun gnome-shell--name-at-point ()
  "Get current Name { ['.'|[<number>|<variable>]} Name } sequence."
  ;; Taken from lua-mode.el
  ;; NB: copying/modifying syntax table for each call may incur a penalty
  (with-syntax-table (copy-syntax-table)
    (modify-syntax-entry ?. "_")
    (modify-syntax-entry ?\[ "_")
    (modify-syntax-entry ?\] "_")
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
        (forward-line)

        (gnome-shell--clear-output-at (point))

        (let ((overlay (make-overlay start (point) nil nil nil)))
          (overlay-put overlay 'after-string
                       (propertize
                        (concat pp-result "\n")
                        'face (list (unless successp 'flyspell-incorrect)
                                    ;; Black background
                                    'secondary-selection
                                    ;; Default foreground
                                    'default)))
          (overlay-put overlay 'gnome-shell-output t)
          (overlay-put overlay 'evaporate t))))

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
        (gnome-shell--flycheck-error result-obj start end)))

    (when show-result
      (gnome-shell--show-result result-obj))

    result))

(defun gnome-shell-copy-output ()
  (interactive)
  (save-excursion
    (end-of-line)
    (let ((overlays (-select (lambda (o) (overlay-get o 'gnome-shell-output))
                             (overlays-in (- (point) 1) (point)))))
      (kill-new (overlay-get (car overlays) 'after-string)))))

(defun gnome-shell-clear-output ()
  (interactive)
  (remove-overlays (point-min) (point-max) 'gnome-shell-output t))

(defun gnome-shell-clear-output-at-point ()
  (interactive)
  (save-excursion
    (end-of-line)
    (gnome-shell--clear-output-at (point))))

(defun gnome-shell--clear-output-at (p)
  (seq-map 'delete-overlay
           (-select (lambda (o) (overlay-get o 'gnome-shell-output))
                    (overlays-in (- p 1) p))))

(defun gnome-shell--flycheck-error (result-obj start end)
  (let* ((result   (alist-get 'value result-obj))
         (column (alist-get 'columnNumber result-obj))
         (line   (alist-get 'lineNumber result-obj))
         (buf-line   (+ (line-number-at-pos start) line -1)) ;; 1 vs 0 indexed
         (buf-column (+ (save-excursion
                          (goto-char start)
                          (current-column))
                        column)))
    ;; FIXME: When using gnome-shell-repl the flycheck error is cleared for some reason?
    (flycheck-add-overlay
     (flycheck-error-new-at buf-line buf-column 'error result
                            :id 'gnome-shell-repl-error))))

(defun gnome-shell--show-result (result-obj)
  (let* ((successp (eq (alist-get 'success result-obj) t))
         (result   (alist-get 'value result-obj))
         (is-undefined (alist-get 'undefined result-obj))
         (pp-result (if result result
                      (if is-undefined "undefined" "null")))
         (presented-result pp-result)
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

(defun gnome-shell--dbus-bootstrap-eval (cmd)
  "Function to bootstrap our own Eval"
  (dbus-call-method gnome-shell-dbus-address "org.gnome.Shell" "/org/gnome/Shell"
                    "org.gnome.Shell" "Eval" cmd))

(defun gnome-shell--ensure-bootstrap ()
  (unless (dbus-introspect-get-interface gnome-shell-dbus-address
                                         "org.gnome.Shell" "/gnome/shell/mode"
                                         "gnome.shell.mode")
    ;; send init code
    (message "sending init code")
    (with-temp-buffer
      (insert-file-contents
       (concat gnome-shell--helper-path "bootstrap.js"))
      (gnome-shell--dbus-bootstrap-eval
       (concat (buffer-string) "('" gnome-shell--helper-path "')")))))

(defun gnome-shell--dbus-call-mode (&rest args)
  (apply #'dbus-call-method gnome-shell-dbus-address
         "org.gnome.Shell" "/gnome/shell/mode" "gnome.shell.mode"
         args))


(defun gnome-shell--dbus-eval (cmd)
  "Raw dbus eval call. Returns a list: (success/boolean result/string)"
  (gnome-shell--dbus-call-mode "Eval"
                               cmd
                               (or (buffer-file-name) "")))

(defun gnome-shell--dbus-reload ()
  "Ask dbus to reload the extension."
  (gnome-shell--dbus-call-mode "Reload"
                          (buffer-string)
                          (or (buffer-file-name) "")))

(defun gnome-shell--dbus-complete (context)
  "Ask dbus to reload the extension."
  (gnome-shell--ensure-bootstrap)
  (gnome-shell--dbus-call-mode "Complete"
                               context
                               (or (buffer-file-name) "")))

(defun gnome-shell-restart ()
  "Disable the extension that the current buffer is part of and restart Gnome
Shell afterwards. This can make restarts a bit more controlled as the extension
is given a chance to clean things up etc."
  (interactive)
  (gnome-shell--ensure-bootstrap)
  (gnome-shell--dbus-call-mode "Restart"
                               (or (buffer-file-name) "")))

(defun gnome-shell-reload ()
  "Reload the current buffer.

Disables the extension, evaluates the buffer and enables the extension again."
  (interactive)
  (gnome-shell--ensure-bootstrap)
  (let* ((result-obj (destructuring-bind (successp jsonres)
                         (gnome-shell--dbus-reload)
                       (json-read-from-string jsonres)))
         (successp (eq (alist-get 'success result-obj) t))
         (start (point-min))
         (end (point-max)))

    (pulse-momentary-highlight-region start end
                                      (if successp
                                          'diff-refine-added
                                        'diff-refine-removed))
    (unless successp
      (gnome-shell--flycheck-error result-obj start end))
    (gnome-shell--show-result result-obj)))

(defun gnome-shell-eval (code)
  "Evaluates `code' in gnome-shell and returns an alist:
  'success : true if no error occured
If success:
  'value : semi-pretty result value
  'raw_value : The raw value serialized to JSON and decoded to lisp equivalent values
If error:
  Most properties from Error. Eg. 'message, 'stack, 'lineNumber, 'columnNumber,
'file"
  (gnome-shell--ensure-bootstrap)

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

(defvar gnome-shell--process nil
  "The gnome shell process, if running a nested shell")

(defun gnome-shell--get-extension ()
  (let* ((buffer (current-buffer))
         (dir (file-name-directory (buffer-file-name buffer))))
    (while (and (equal dir "/")
                (not (file-exists-p (concat dir "metadata.json"))))
      ;; this doesn't make sense, but returns the parent directory
      (setq dir (file-name-directory (directory-file-name dir))))
    (cons (cons 'root dir)
          (json-read-file (concat dir "metadata.json")))))


(defun gnome-shell-launch-session (&optional wayland extensions)
  "Launch a nested X11/wayland session or show the log if a session is already
running"
  (interactive)
  (unless (process-live-p gnome-shell--process)
    (dolist (err gnome-shell--errors)
      (when-let ((filename (flycheck-error-filename err))
                 (buffer (find-buffer-visiting filename)))
        (with-current-buffer buffer
          (flycheck-clear))))
    (setq gnome-shell--errors nil)
    (gnome-shell-set-dbus-address :session)
    (let* ((name "gnome-session")
          (bus-address nil)
          (buffer (create-file-buffer " *gnome-session*"))
          (extension (gnome-shell--get-extension))
          (root (alist-get 'root extension))
          (uuid (alist-get 'uuid extension)))

      (setq gnome-shell--process
            (start-process
             name buffer
             (concat gnome-shell--helper-path "session.sh")
             "" root uuid))

      (set-process-filter
       gnome-shell--process
       (lambda (process string)
         (with-current-buffer (process-buffer process)
           (goto-char (point-max))
           (insert string))
         (when (search "unix:abstract" string)
           (setq string (substring string 0 (search "\n" string)))
           (gnome-shell-set-dbus-address string))
         (when (search "JS ERROR: " string)
           (gnome-shell--flycheck-log process string))))))

  ;; Always show the log when launching
  (gnome-shell-session-log))


(defvar gnome-shell--errors nil
  "The gnome shell process, if running a nested shell")

(defun gnome-shell--add-all-errors ()
  "Add any existing errors association with buffer's file."
  (when (and flycheck-mode
             (equal major-mode 'gnome-shell-mode))
    ;; Prevent flycheck from killing errors
    (setq flycheck-check-syntax-automatically nil)
    (dolist (err gnome-shell--errors)
      (when-let ((buffer (find-buffer-visiting (flycheck-error-filename err))))
        (when (equal (current-buffer) buffer)
          (setf (flycheck-error-buffer err) (current-buffer))
          (flycheck-report-current-errors (list err)))))))

(defun gnome-shell--flycheck-log (process string)
  (let* ((from (+ (search "JS ERROR: " string) 10))
         (to (search "\n" string :start2 from))
         (message (substring string from to))

         (from (1+ (search "@" string :start2 to)))
         (to (search "\n" string :start2 from))
         (location (substring string from to))
         (file (replace-regexp-in-string  ":[0-9]*:[0-9]*$" "" location))
         (loc (split-string (substring location (1+ (length file))) ":"))
         (buffer (find-buffer-visiting file))

         (err (flycheck-error-new-at
               (string-to-number (car loc)) (string-to-number (cadr loc))
               'error message :filename file
               :id 'gnome-shell-log-error))
         )
    (setq gnome-shell--errors (cons err gnome-shell--errors))
    (when buffer
      (setf (flycheck-error-buffer err) buffer)
      (with-current-buffer buffer
        (flycheck-report-current-errors (list err))))))

(defun gnome-shell-session-log ()
  "Show the output of current session"
  (interactive)
  (pop-to-buffer (process-buffer gnome-shell--process))
  (goto-char (point-max)))

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
                  '("Gnome-Shell"
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
  (use-local-map gnome-shell-mode-map)

  (add-hook
   'flycheck-mode-hook
   #'gnome-shell--add-all-errors)

  (add-hook 'gnome-shell-mode-hook
            #'flycheck-mode))

(provide 'gnome-shell-mode)

;;; gnome-shell-mode.el ends here

;; arch-tag: 17c5fcf9-ea23-4ca5-b7d5-a0635b8b4230


