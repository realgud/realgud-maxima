;; Copyright (C) 2017 Rocky Bernstein

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

;;  `realgud:maxima' Main interface to maxima via Emacs
(require 'load-relative)
(require 'realgud)
(require-relative-list '("core" "track-mode") "realgud:maxima-")

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 24.
(defgroup realgud:maxima nil
  "The realgud interface to maxima"
  :group 'realgud
  :version "24.5")

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom realgud:maxima-command-name
  "maxima"
  "File name for executing the and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'realgud:maxima)

(declare-function realgud:maxima-track-mode     'realgud:maxima-track-mode)
(declare-function realgud-command             'realgud:maxima-core)
(declare-function realgud:maxima-parse-cmd-args 'realgud:maxima-core)
(declare-function realgud:maxima-query-cmdline  'realgud:maxima-core)
(declare-function realgud:run-process         'realgud-core)
(declare-function realgud:flatten             'realgud-utils)
(declare-function realgud:remove-ansi-schmutz 'realgud-utils)

;; -------------------------------------------------------------------
;; The end.
;;

;;;###autoload
(defun realgud:maxima (&optional opt-cmd-line no-reset)
  "Invoke the maxima debugger and start the Emacs user interface.

OPT-CMD-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'.

Normally, command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset. See `loc-changes-clear-buffer' to clear
fringe and marginal icons.
"
  (interactive)
  (let* ((cmd-str (or opt-cmd-line (realgud:maxima-query-cmdline "maxima")))
	 (cmd-args (split-string-and-unquote cmd-str))
	 (parsed-args (realgud:maxima-parse-cmd-args cmd-args))
	 (script-args (caddr parsed-args))
	 (script-name (car script-args))
	 (parsed-cmd-args
	  (cl-remove-if 'nil (realgud:flatten parsed-args)))
	 (cmd-buf (realgud:run-process realgud:maxima-command-name
				       script-name parsed-cmd-args
				       'realgud:maxima-minibuffer-history
				       nil))
	 )
    )
  )

(defalias 'maxima 'realgud:maxima)

(provide-me "realgud-")

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
