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

;;; maxima debugger

(eval-when-compile (require 'cl-lib))

(require 'realgud)

(defvar realgud-pat-hash)
(declare-function make-realgud-loc-pat (realgud-loc))

(defvar realgud:maxima-pat-hash (make-hash-table :test 'equal)
  "hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  the values of a hash entry is a
realgud-loc-pat struct")

(declare-function make-realgud-loc "realgud-loc" (a b c d e f))

(defconst realgud:maxima-frame-file-regexp
  (format "\\(.+\\):%s" realgud:regexp-captured-num))

(defconst realgud:maxima-frame-start-regexp
  "\\(?:^\\|\n\\)")

(defconst realgud:maxima-frame-num-regexp
  (format "#%s: "
	  realgud:regexp-captured-num))

(setf (gethash "loc-callback-fn" realgud:maxima-pat-hash) 'realgud:maxima-loc-fn-callback)

;; realgud-loc-pat that describes a maxima location generally shown
;; before a command prompt.
;; For example:
;; /tmp/foobar.mac:2::
;;
(setf (gethash "loc" realgud:maxima-pat-hash)
      (make-realgud-loc-pat
       :regexp (format "^\\* thread #%s: .+ at %s, "
		       realgud:regexp-captured-num realgud:maxima-frame-file-regexp)
       :file-group 2
       :line-group 3))

;; Top frame number
(setf (gethash "top-frame-num" realgud:maxima-pat-hash) 0)

;; realgud-loc-pat that describes a maxima frame generally shown
;; before a command prompt or in frame switching commands
;;  frame #1: 0x00000000004015e2 ctest`main(argc=1, argv=0x00007fffffffd778) + 90 at ctest.c:83
;; Some versions of maxima give:
;; #0  main (argc=2, argv=0xbffff564, envp=0xbffff570) at main.c:935
;; instead

(setf (gethash "selected-frame" realgud:maxima-pat-hash)
      (make-realgud-loc-pat
       :regexp 	(format "^%s.* at %s"
			realgud:maxima-frame-num-regexp
			realgud:maxima-frame-file-regexp
			)
       :num 1
       :file-group 2
       :line-group 3)
      )

;; realgud-loc-pat that describes a maxima prompt
;; For example:
;;   (%i1)
;;   (%i33)
(setf (gethash "prompt" realgud:maxima-pat-hash)
      (make-realgud-loc-pat
       :regexp   "^(dbm:[0-9]+) "
       ))

;; realgud-loc-pat that describes a "breakpoint set" line
;; For example:
;;   Bkpt 0 for foo (in /tmp/foobar.mac line 1)
(setf (gethash "brkpt-set" realgud:maxima-pat-hash)
      (make-realgud-loc-pat
       :regexp (format "^Bkpt %s: for .* (in \\(.+\\) line %s"
		       realgud:regexp-captured-num
		       realgud:regexp-captured-num)
       :num 1
       :file-group 2
       :line-group 3))

;; realgud-loc-pat that describes a maxima "backtrace" command line.
;; For example:
;; #0: foo(y=5)(foobar.mac line 1)
;; #1: bar(x=2,y=3)(foobar.mac line 9)

(setf (gethash "debugger-backtrace" realgud:maxima-pat-hash)
      (make-realgud-loc-pat
       :regexp 	(concat realgud:maxima-frame-start-regexp
			realgud:maxima-frame-num-regexp
			"\\(?:.\\|\\(?:[\n] \\)\\)+[ ]+at "
			realgud:maxima-frame-file-regexp
			)
       :num 1
       :file-group 2
       :line-group 3)
      )

(setf (gethash "font-lock-keywords" realgud:maxima-pat-hash)
      '(
	;; #2  0x080593ac in main (argc=2, argv=0xbffff5a4, envp=0xbffff5b0)
	;;    at main.c:952
	("[ \n]+at \\(.*\\):\\([0-9]+\\)"
	 (1 realgud-file-name-face)
	 (2 realgud-line-number-face))

	;; The frame number and first type name, if present.
	;; E.g. =>#0  Makefile.in at /tmp/Makefile:216
	;;      ---^
	( "#\\(?:^\\|\n\\)\\([0-9]+\\)  "
	 (1 realgud-backtrace-number-face))
	))

;;  Prefix used in variable names (e.g. short-key-mode-map) for
;; this debugger
(setf (gethash "maxima" realgud:variable-basename-hash) "realgud:maxima")

(defvar realgud:maxima-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'continue' and the value is
  the maxima command to use, like 'process continue'")

(setf (gethash "backtrace"   realgud:maxima-command-hash) ":bt")
(setf (gethash "clear"       realgud:maxima-command-hash) "*not-implemented*")
(setf (gethash "continue"    realgud:maxima-command-hash) ":continue")
(setf (gethash "delete"      realgud:maxima-command-hash) ":delete %p")
(setf (gethash "delete_all"  realgud:maxima-command-hash) ":delete")
(setf (gethash "disable"     realgud:maxima-command-hash) ":disable %p")
(setf (gethash "disable_all" realgud:maxima-command-hash) ":disable")
(setf (gethash "enable"      realgud:maxima-command-hash) ":enable %p")
(setf (gethash "enable_all"  realgud:maxima-command-hash)  ":enable")
(setf (gethash "eval"        realgud:maxima-command-hash) ":lisp %s")
(setf (gethash "finish"      realgud:maxima-command-hash) "*not-implemented*")
(setf (gethash "frame"       realgud:maxima-command-hash) ":frame %p")
(setf (gethash "help"        realgud:maxima-command-hash) ":help")
(setf (gethash "info"        realgud:maxima-command-hash) ":info")
(setf (gethash "quit"        realgud:maxima-command-hash) ":quit")
(setf (gethash "run"         realgud:maxima-command-hash) ":resume")
(setf (gethash "step"        realgud:maxima-command-hash) ":step")

(setf (gethash "maxima" realgud-command-hash) realgud:maxima-command-hash)
(setf (gethash "maxima" realgud-pat-hash) realgud:maxima-pat-hash)

(provide-me "realgud:maxima-")
