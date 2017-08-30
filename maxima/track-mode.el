;; Copyright (C) 2016 Rocky Bernstein
;;; track-mode.el ---

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

;; maxima tracking a comint or eshell buffer.

(eval-when-compile (require 'cl))

(require 'realgud)

(require-relative-list '("core" "init") "realgud:maxima-")

(realgud-track-mode-vars "realgud:maxima")

(declare-function realgud-track-mode 'realgud-track-mode)
(declare-function realgud:track-mode-hook 'realgud-track-mode)
(declare-function realgud-track-mode-setup 'realgud-track-mode)
(declare-function realgud:track-set-debugger 'realgud-track-mode)

(define-key realgud:maxima-track-mode-map
  (kbd "C-c !b") 'realgud:goto-debugger-backtrace-line)

(defun realgud:maxima-track-mode-hook()
  (use-local-map realgud:maxima-track-mode-map)
  (realgud-track-mode-setup 't)
  (message "realgud:maxima track-mode-hook called")
)

(define-minor-mode realgud:maxima-track-mode
  "Minor mode for tracking maxima inside a process shell via realgud.

If called interactively with no prefix argument, the mode is toggled. A prefix argument, captured as ARG, enables the mode if the argument is positive, and disables it otherwise.

Key bindings:
\\{realgud:maxima-track-mode-map}
"
  :init-value nil
  ;; :lighter " maxima"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'realgud:maxima
  :keymap realgud:maxima-track-mode-map
  (if realgud:maxima-track-mode
      (progn
	(realgud:track-set-debugger "maxima")
        (realgud:maxima-track-mode-hook)
        (realgud:track-mode-enable))
    (progn
      (setq realgud::maxima-track-mode nil)
      ))
)

(provide-me "realgud:maxima-")
;;; track-mode.el ends here
