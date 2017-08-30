;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) (file-name-directory (locate-library "realgud.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)
(load-file "../maxima/core.el")
(load-file "./regexp-helper.el")

(eval-when-compile
  (defvar realgud:maxima-minibuffer-history)
  (defvar test:realgud-maxima-executable-save)
  (defvar test:realgud-minibuffer-history-save)
)

(declare-function realgud:maxima-suggest-invocation 'realgud:bashdb)
(declare-function __FILE__              'require-relative)

(test-simple-start)

;; Save value realgud:run-process and change it to something we want
(setq test:realgud-maxima-executable-save (symbol-function 'realgud:maxima-executable))
(setq test:realgud-minibuffer-history-save realgud:maxima-minibuffer-history)

(defun realgud:maxima-executable (filename)
  "Mock function for testing"
  (cond ((equal filename "bar.sh") 7)
	((equal filename "foo") 8)
	((equal filename "baz") 8)
	(t 3)))

(defun maxima-test()
  (note "realgud:maxima-suggest-invocation")
  (setq realgud:maxima-minibuffer-history nil)
  (let ((my-directory (file-name-directory (__FILE__))))
    (save-excursion
      (note "Test preference to buffer editing")
      (setq default-directory
	    (concat my-directory "maxima"))
      (find-file-literally "foo.c")
      (assert-equal "maxima foo" (realgud:maxima-suggest-invocation)
		    "Should find file sans extension - foo")
      (find-file-literally "baz.c")
      (assert-equal "maxima baz" (realgud:maxima-suggest-invocation)
		    "Should find file sans extension - baz")
      )
    (save-excursion
      (note "Pick up non-sans executable")
      (setq default-directory
	    (concat my-directory  "maxima/test2"))
      ;; (assert-equal "maxima bar.sh" (realgud:maxima-suggest-invocation))
      (setq realgud:maxima-minibuffer-history '("maxima testing"))
      (setq default-directory
	    (concat my-directory  "maxima/test2"))
      (assert-equal "maxima testing" (realgud:maxima-suggest-invocation)
		    "After setting minibuffer history - takes precidence")
      )
    (setq default-directory my-directory)
    )
  )
(maxima-test)
(end-tests)

;; Restore the old values.
;; You might have to run the below if you run this interactively.
(fset 'realgud:maxima-executable test:realgud-maxima-executable-save)
(setq realgud:maxima-minibuffer-history test:realgud-minibuffer-history-save)
(setq default-directory (file-name-directory (__FILE__)))
