;;; lua-eldoc-mode.el --- Display the arguments for the standard Lua functions in the minibuffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Rolando Pereira

;; Author: Rolando Pereira <rolando_pereira@sapo.pt>
;; Keywords: languages, help, extensions, eldoc

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Display the arguments for the standard Lua functions in the
;; minibuffer when the cursor is on them just like `eldoc-mode' does
;; for elisp.
;;
;; To install place this file somewhere in your load-path and add the
;; following to your .emacs:
;;
;;     (require 'lua-eldoc-mode)
;;     (add-hook 'lua-mode-hook 'lua-eldoc-mode)
;; 

;;; Code:

(require 's)
(require 'dash)
(require 'auto-complete)

(defvar lua-eldoc-mode-standard-functions
  '(
     ("assert" . "assert (v [, message])")
     ("collectgarbage" . "collectgarbage ([opt [, arg]])")
     ("dofile" . "dofile ([filename])")
     ("error" . "error (message [, level])")
     ("getfenv" . "getfenv ([f])")
     ("getmetatable" . "getmetatable (object)")
     ("ipairs" . "ipairs (t)")
     ("load" . "load (func [, chunkname])")
     ("loadfile" . "loadfile ([filename])")
     ("loadstring" . "loadstring (string [, chunkname])")
     ("next" . "next (table [, index])")
     ("pairs" . "pairs (t)")
     ("pcall" . "pcall (f, arg1, ...)")
     ("print" . "print (...)")
     ("rawequal" . "rawequal (v1, v2)")
     ("rawget" . "rawget (table, index)")
     ("rawset" . "rawset (table, index, value)")
     ("select" . "select (index, ...)")
     ("setfenv" . "setfenv (f, table)")
     ("setmetatable" . "setmetatable (table, metatable)")
     ("tonumber" . "tonumber (e [, base])")
     ("tostring" . "tostring (e)")
     ("type" . "type (v)")
     ("unpack" . "unpack (list [, i [, j]])")
     ("xpcall" . "xpcall (f, err)")
     ("coroutine.create" . "coroutine.create (f)")
     ("coroutine.resume" . "coroutine.resume (co [, val1, ...])")
     ("coroutine.running" . "coroutine.running ()")
     ("coroutine.status" . "coroutine.status (co)")
     ("coroutine.wrap" . "coroutine.wrap (f)")
     ("coroutine.yield" . "coroutine.yield (...)")
     ("module" . "module (name [, ...])")
     ("require" . "require (modname)")
     ("package.loadlib" . "package.loadlib (libname, funcname)")
     ("package.seeall" . "package.seeall (module)")
     ("string.byte" . "string.byte (s [, i [, j]])")
     ("string.char" . "string.char (...)")
     ("string.dump" . "string.dump (function)")
     ("string.find" . "string.find (s, pattern [, init [, plain]])")
     ("string.format" . "string.format (formatstring, ...)")
     ("string.gmatch" . "string.gmatch (s, pattern)")
     ("string.gsub" . "string.gsub (s, pattern, repl [, n])")
     ("string.len" . "string.len (s)")
     ("string.lower" . "string.lower (s)")
     ("string.match" . "string.match (s, pattern [, init])")
     ("string.rep" . "string.rep (s, n)")
     ("string.reverse" . "string.reverse (s)")
     ("string.sub" . "string.sub (s, i [, j])")
     ("string.upper" . "string.upper (s)")

     ;; The "string" functions used as a method
     ("byte" . "[string]:byte ([, i [, j]])")
     ;;("char" . "[string]:char (...)")                      ; Can this be used as methods?
     ;;("dump" . "[string]:dump (function)")                 ; Can this be used as methods?
     ("find" . "[string]:find (pattern [, init [, plain]])")
     ;;("format" . "[string]:format (formatstring, ...)")    ; Can this be used as methods?
     ("gmatch" . "[string]:gmatch (pattern)")
     ("gsub" . "[string]:gsub (pattern, repl [, n])")
     ("len" . "[string]:len ()")
     ("lower" . "[string]:lower ()")
     ("match" . "[string]:match (pattern [, init])")
     ("rep" . "[string]:rep (n)")
     ("reverse" . "[string]:reverse ()")
     ("sub" . "[string]:sub (i [, j])")
     ("upper" . "[string]:upper ()")

     ("table.concat" . "table.concat (table [, sep [, i [, j]]])")
     ("table.insert" . "table.insert (table, [pos,] value)")
     ("table.maxn" . "table.maxn (table)")
     ("table.remove" . "table.remove (table [, pos])")
     ("table.sort" . "table.sort (table [, comp])")
     ("math.abs" . "math.abs (x)")
     ("math.acos" . "math.acos (x)")
     ("math.asin" . "math.asin (x)")
     ("math.atan" . "math.atan (x)")
     ("math.atan2" . "math.atan2 (y, x)")
     ("math.ceil" . "math.ceil (x)")
     ("math.cos" . "math.cos (x)")
     ("math.cosh" . "math.cosh (x)")
     ("math.deg" . "math.deg (x)")
     ("math.exp" . "math.exp (x)")
     ("math.floor" . "math.floor (x)")
     ("math.fmod" . "math.fmod (x, y)")
     ("math.frexp" . "math.frexp (x)")
     ("math.ldexp" . "math.ldexp (m, e)")
     ("math.log" . "math.log (x)")
     ("math.log10" . "math.log10 (x)")
     ("math.max" . "math.max (x, ...)")
     ("math.min" . "math.min (x, ...)")
     ("math.modf" . "math.modf (x)")
     ("math.pow" . "math.pow (x, y)")
     ("math.rad" . "math.rad (x)")
     ("math.random" . "math.random ([m [, n]])")
     ("math.randomseed" . "math.randomseed (x)")
     ("math.sin" . "math.sin (x)")
     ("math.sinh" . "math.sinh (x)")
     ("math.sqrt" . "math.sqrt (x)")
     ("math.tan" . "math.tan (x)")
     ("math.tanh" . "math.tanh (x)")
     ("io.close" . "io.close ([file])")
     ("io.flush" . "io.flush ()")
     ("io.input" . "io.input ([file])")
     ("io.lines" . "io.lines ([filename])")
     ("io.open" . "io.open (filename [, mode])")
     ("io.output" . "io.output ([file])")
     ("io.popen" . "io.popen (prog [, mode])")
     ("io.read" . "io.read (...)")
     ("io.tmpfile" . "io.tmpfile ()")
     ("io.type" . "io.type (obj)")
     ("io.write" . "io.write (...)")
     ;; These ones can't be parsed because "file" could be anything
     ;; ("file:close" . "file:close ()")
     ;; ("file:flush" . "file:flush ()")
     ;; ("file:lines" . "file:lines ()")
     ;; ("file:read" . "file:read (...)")
     ;; ("file:seek" . "file:seek ([whence] [, offset])")
     ;; ("file:setvbuf" . "file:setvbuf (mode [, size])")
     ;; ("file:write" . "file:write (...)")
     ("os.clock" . "os.clock ()")
     ("os.date" . "os.date ([format [, time]])")
     ("os.difftime" . "os.difftime (t2, t1)")
     ("os.execute" . "os.execute ([command])")
     ("os.exit" . "os.exit ([code])")
     ("os.getenv" . "os.getenv (varname)")
     ("os.remove" . "os.remove (filename)")
     ("os.rename" . "os.rename (oldname, newname)")
     ("os.setlocale" . "os.setlocale (locale [, category])")
     ("os.time" . "os.time ([table])")
     ("os.tmpname" . "os.tmpname ()")
     ("debug.debug" . "debug.debug ()")
     ("debug.getfenv" . "debug.getfenv (o)")
     ("debug.gethook" . "debug.gethook ([thread])")
     ("debug.getinfo" . "debug.getinfo ([thread,] function [, what])")
     ("debug.getlocal" . "debug.getlocal ([thread,] level, local)")
     ("debug.getmetatable" . "debug.getmetatable (object)")
     ("debug.getregistry" . "debug.getregistry ()")
     ("debug.getupvalue" . "debug.getupvalue (func, up)")
     ("debug.setfenv" . "debug.setfenv (object, table)")
     ("debug.sethook" . "debug.sethook ([thread,] hook, mask [, count])")
     ("debug.setlocal" . "debug.setlocal ([thread,] level, local, value)")
     ("debug.setmetatable" . "debug.setmetatable (object, table)")
     ("debug.setupvalue" . "debug.setupvalue (func, up, value)")
     ("debug.traceback" . "debug.traceback ([thread,] [message [, level]])"))
  "A alist contains the standard functions that come with Lua.")

(defun lua-eldoc-mode-thing-at-point-no-properties (thing)
  "Return just the text of thing at point."
  ;; In emacs 24.4 "thing-at-point" has an optional NO-PROPERTIES
  ;; argument that does the same as this function. However that option doesn't
  ;; exist in emacs prior to 24.4 so this function is provided.
  (let ((text (thing-at-point thing)))
    (set-text-properties 0 (length text) nil text)
    text))

(defun lua-eldoc-mode--doc-for-stdlib (function-name)
  ;; If `function-name' contains the ":" character then instead
  ;; of searching for e.g. "string.len", try to find only "len"
  (if (s-contains? ":" function-name)
      (cdr (assoc (-last-item (s-split ":" function-name)) lua-eldoc-mode-standard-functions))
    (cdr (assoc function-name lua-eldoc-mode-standard-functions)))
  )

(defvar lua-eldoc-mode--last-lookup '("" . "")
  "eldoc really spams this function. (even when `point' is still it hammers it)

Considering we do a relative expensive lookup we cache the last result in this variable.")

(defun lua-eldoc-mode-help-at-point ()
  "Return the arguments for the standard function at point."
  ;; The `lua-mode' syntax table considers the "." character as a
  ;; puntuation character. This causes `thing-at-point' to return, for
  ;; example, "format" instead of "string.format" when searching for
  ;; symbols. As a workaround this buffer's syntax-table is
  ;; temporarily modified so it considers "." as belonging to
  ;; word. This makes `thing-at-point' return "string.format"
  (with-syntax-table (copy-syntax-table)
    (modify-syntax-entry ?. "w")
    (modify-syntax-entry ?: "w")
    (save-excursion
      (backward-word)
      (let* ((function-name (lua-eldoc-mode-thing-at-point-no-properties 'symbol)))
        ;; Special case: When `point' is on the first line of the
        ;; buffer and that line is empty, `function-name' will be NIL,
        ;; which causes `s-contains' to error out.
        (when (not (null function-name))
          (if (equal (car lua-eldoc-mode--last-lookup) function-name)
              (cdr lua-eldoc-mode--last-lookup)
            (let ((eldoc (or (lua-eldoc-mode--doc-for-stdlib function-name)
                             (notion-wm-eldoc function-name))))
              (setcar lua-eldoc-mode--last-lookup function-name)
              (setcdr lua-eldoc-mode--last-lookup eldoc)
              eldoc)))))))


;;;###autoload
(defun lua-eldoc-mode (&optional arg)
  "Display in the minibuffer the arguments that the standard Lua functions take."
  (interactive (list (or current-prefix-arg 'toggle)))
  (set (make-local-variable 'eldoc-documentation-function) 'lua-eldoc-mode-help-at-point)
  (eldoc-mode arg))


(provide 'lua-eldoc-mode)

;;; lua-eldoc-mode.el ends here
