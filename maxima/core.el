;; Copyright (C) 2016 Rocky Bernstein

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

(eval-when-compile (require 'cl))

(require 'realgud)

(declare-function realgud:expand-file-name-if-exists 'realgud-core)
(declare-function realgud-lang-mode? 'realgud-lang)
(declare-function realgud-parse-command-arg 'realgud-core)
(declare-function realgud-query-cmdline 'realgud-core)

;; FIXME: I think the following could be generalized and moved to
;; realgud-... probably via a macro.
(defvar realgud:maxima-minibuffer-history nil
  "minibuffer history list for the command `maxima'.")

(easy-mmode-defmap realgud:maxima-minibuffer-local-map
  '(("\C-i" . comint-dynamic-complete-filename))
  "Keymap for minibuffer prompting of gud startup command."
  :inherit minibuffer-local-map)

;; FIXME: I think this code and the keymaps and history
;; variable chould be generalized, perhaps via a macro.
(defun realgud:maxima-query-cmdline (&optional opt-debugger)
  (realgud-query-cmdline
   'realgud:maxima-suggest-invocation
   realgud:maxima-minibuffer-local-map
   'realgud:maxima-minibuffer-history
   opt-debugger))

(defvar realgud:maxima-file-remap (make-hash-table :test 'equal)
  "How to remap maxima files in  when we otherwise can't find in
  the filesystem. The hash key is the file string we saw, and the
  value is associated filesystem string presumably in the
  filesystem")

(defun realgud:maxima-find-file(filename)
  "A find-file specific for maxima. We use `global' to map a
name to a filename. Failing that
we will prompt for a mapping and save that in `realgud:maxima-file-remap' when
that works."
  (let ((resolved-filename filename)
	(global-output)
	(remapped-filename (gethash filename realgud:maxima-file-remap)))
    (cond
     ((and remapped-filename (stringp remapped-filename)
	   (file-exists-p remapped-filename)) remapped-filename)
     ((file-exists-p filename) filename)
     ((and (setq resolved-filename (shell-command-to-string (format "global -P %s" filename)))
	   (stringp resolved-filename)
	   (file-exists-p (setq resolved-filename (realgud:strip resolved-filename))))
     	(puthash filename resolved-filename realgud:maxima-file-remap))
     ('t
      (setq resolved-filename
	    (buffer-file-name
	     (compilation-find-file (point-marker) filename nil "")))
      (puthash filename resolved-filename realgud:maxima-file-remap)))
     ))

(defun realgud:maxima-loc-fn-callback(text filename lineno source-str
					 ignore-file-re cmd-mark)
  (realgud:file-loc-from-line filename lineno
			      cmd-mark source-str nil
			      ignore-file-re 'realgud:maxima-find-file))

(defun realgud:maxima-parse-cmd-args (orig-args)
  "Parse command line ARGS for the annotate level and name of script to debug.

ORIG_ARGS should contain a tokenized list of the command line to run.

We return the a list containing
* the name of the debugger given (e.g. maxima) and its arguments - a list of strings
* nil (a placehoder in other routines of this ilk for a debugger
* the script name and its arguments - list of strings
* whether the emacs option was given ('--emacs) - a boolean

For example for the following input
  (map 'list 'symbol-name
   '(maxima --tty /dev/pts/1 -cd ~ --emacs ./gcd.py a b))

we might return:
   ((\"maxima\" \"--tty\" \"/dev/pts/1\" \"-cd\" \"home/rocky\' \"--emacs\") nil \"(/tmp/gcd.py a b\") 't\")

Note that path elements have been expanded via `expand-file-name'.
"

  ;; Parse the following kind of pattern:
  ;;  maxima maxima-options script-name script-options
  (let (
	(args orig-args)
	(pair)          ;; temp return from

	;; One dash is added automatically to the below, so
	;; a is really -a. maxima doesn't seem to have long
	;; (--) options.
	(maxima-two-args '("a" "f" "c" "s" "o" "S" "k" "L"
			"p" "O"  "K"))
	;; maxima doesn't optional 2-arg options.
	(maxima-opt-two-args '("r"))

	;; Things returned
	(script-name nil)
	(debugger-name nil)
	(debugger-args '())
	(script-args '())
	(annotate-p nil))

    (if (not (and args))
	;; Got nothing: return '(nil nil nil nil)
	(list debugger-args nil script-args annotate-p)
      ;; else
      (progn

	;; Remove "maxima" from "maxima --maxima-options script
	;; --script-options"
	(setq debugger-name (file-name-sans-extension
			     (file-name-nondirectory (car args))))
	(unless (string-match "^maxima.*" debugger-name)
	  (message
	   "Expecting debugger name `%s' to be `maxima'"
	   debugger-name))
	(setq debugger-args (list (pop args)))

	;; Skip to the first non-option argument.
	(while (and args (not script-name))
	  (let ((arg (car args)))
	    (cond
	     ;; path-argument ooptions
	     ((member arg '("-cd" ))
	      (setq arg (pop args))
	      (nconc debugger-args
		     (list arg (realgud:expand-file-name-if-exists
				(pop args)))))
	     ;; Options with arguments.
	     ((string-match "^-" arg)
	      (setq pair (realgud-parse-command-arg
			  args maxima-two-args maxima-opt-two-args))
	      (nconc debugger-args (car pair))
	      (setq args (cadr pair)))
	     ;; Anything else must be the script to debug.
	     (t (setq script-name arg)
		(setq script-args args))
	     )))
	(list debugger-args nil script-args annotate-p)))))

(defvar realgud:maxima-command-name)

(defun realgud:maxima-executable (file-name)
"Return a priority for wehther file-name is likely we can run maxima on"
  (let ((output (shell-command-to-string (format "file %s" file-name))))
    (cond
     ((string-match "ASCII" output) 2)
     ((string-match "ELF" output) 7)
     ((string-match "executable" output) 6)
     ('t 5))))


(defun realgud:maxima-suggest-invocation (&optional debugger-name)
  "Suggest a maxima command invocation. Here is the priority we use:
* an executable file with the name of the current buffer stripped of its extension
* any executable file in the current directory with no extension
* the last invocation in maxima:minibuffer-history
* any executable in the current directory
When all else fails return the empty string."
  (let* ((file-list (directory-files default-directory))
	 (priority 2)
	 (best-filename nil)
	 (try-filename (file-name-base (or (buffer-file-name) "maxima"))))
    (when (member try-filename (directory-files default-directory))
	(setq best-filename try-filename)
	(setq priority (+ (realgud:maxima-executable try-filename) 2)))

    ;; FIXME: I think a better test would be to look for
    ;; c-mode in the buffer that have a corresponding executable
    (while (and (setq try-filename (car-safe file-list)) (< priority 8))
      (setq file-list (cdr file-list))
      (if (and (file-executable-p try-filename)
	       (not (file-directory-p try-filename)))
	  (if (equal try-filename (file-name-sans-extension try-filename))
	      (progn
		(setq best-filename try-filename)
		(setq priority (1+ (realgud:maxima-executable best-filename))))
	    ;; else
	    (progn
	      (setq best-filename try-filename)
	      (setq priority (realgud:maxima-executable best-filename))
	      ))
	))
    (if (< priority 8)
	(cond
	 (realgud:maxima-minibuffer-history
	  (car realgud:maxima-minibuffer-history))
	 ((equal priority 7)
	  (concat "maxima " best-filename))
	 (t "maxima "))
      ;; else
      (concat "maxima " best-filename))
    ))

(defun realgud:maxima-reset ()
  "Maxima cleanup - remove debugger's internal buffers (frame,
breakpoints, etc.)."
  (interactive)
  ;; (maxima-breakpoint-remove-all-icons)
  (dolist (buffer (buffer-list))
    (when (string-match "\\*maxima-[a-z]+\\*" (buffer-name buffer))
      (let ((w (get-buffer-window buffer)))
        (when w
          (delete-window w)))
      (kill-buffer buffer))))

;; (defun maxima-reset-keymaps()
;;   "This unbinds the special debugger keys of the source buffers."
;;   (interactive)
;;   (setcdr (assq 'maxima-debugger-support-minor-mode minor-mode-map-alist)
;; 	  maxima-debugger-support-minor-mode-map-when-deactive))


(defun realgud:maxima-customize ()
  "Use `customize' to edit the settings of the `realgud:maxima' debugger."
  (interactive)
  (customize-group 'realgud:maxima))

(provide-me "realgud:maxima-")
