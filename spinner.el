;;; spinner.el --- Add spinners and progress-bars to the mode-line for ongoing operations -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Artur Malabarba <emacs@endlessparentheses.com>
;; Version: 1.2
;; Package-Requires: ((cl-lib "0.5"))
;; URL: https://github.com/Malabarba/spinner.el
;; Keywords: processes mode-line

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
;; 1 Usage
;; ═══════
;;
;;   First of all, don’t forget to add `(spinner "VERSION")' to your
;;   package’s dependencies.
;;
;;
;; 1.1 Major-modes
;; ───────────────
;;
;;   1. Just call `(spinner-start)' and a spinner will be added to the
;;      mode-line.
;;   2. Call `(spinner-stop)' on the same buffer when you want to remove
;;      it.
;;
;;   The default spinner is a line drawing that rotates. You can pass an
;;   argument to `spinner-start' to specify which spinner you want. All
;;   possibilities are listed in the `spinner-types' variable, but here are
;;   a few examples for you to try:
;;
;;   • `(spinner-start 'vertical-breathing 10)'
;;   • `(spinner-start 'minibox)'
;;   • `(spinner-start 'moon)'
;;   • `(spinner-start 'triangle)'
;;
;;   You can also define your own as a vector of strings (see the examples
;;   in `spinner-types').
;;
;;
;; 1.2 Minor-modes
;; ───────────────
;;
;;   Minor-modes can create a spinner (that can be added to the mode’s
;;   lighter) with `spinner-make-construct'. They can then start the
;;   spinner by setting a variable and calling `spinner-start-timer'.
;;   Finally, they can stop the spinner (and the timer) by just setting the
;;   same variable to nil.
;;
;;   Here’s an example for a minor-mode named `foo'.
;;   ┌────
;;   │ (defvar foo--spinner nil)
;;   │ (defvar foo--timer nil)
;;   │ (defconst foo--lighter
;;   │   (list " foo"
;;   │         (spinner-make-construct 'foo--spinner 'foo--timer)))
;;   │
;;   │ (defun foo--start-spinning ()
;;   │   "Start foo's spinner."
;;   │   (setq foo--spinner
;;   │         (cdr (assq 'horizontal-bar spinner-types)))
;;   │   (spinner-start-timer 'foo--spinner 'foo--timer))
;;   │
;;   │ (defun foo--stop-spinning ()
;;   │   "Stop foo's spinner"
;;   │   (setq foo--spinner nil))
;;   └────
;;
;;   This will use the `horizontal-bar' spinner, but you can use anything
;;   defined in the `spinner-types' variable, or even define your own.

;;; Code:
(require 'cl-lib)

(defconst spinner-types
  '((3-line-clock . ["┤" "┘" "┴" "└" "├" "┌" "┬" "┐"])
    (2-line-clock . ["┘" "└" "┌" "┐"])
    (flipping-line . ["_" "\\" "|" "/"])
    (rotating-line . ["-" "\\" "|" "/"])
    (progress-bar . ["[    ]" "[=   ]" "[==  ]" "[=== ]" "[====]" "[ ===]" "[  ==]" "[   =]"])
    (progress-bar-filled . ["|    |" "|█   |" "|██  |" "|███ |" "|████|" "| ███|" "|  ██|" "|   █|"])
    (vertical-breathing . ["▁" "▂" "▃" "▄" "▅" "▆" "▇" "█" "▇" "▆" "▅" "▄" "▃" "▂" "▁" " "])
    (vertical-rising . ["▁" "▄" "█" "▀" "▔"])
    (horizontal-breathing . [" " "▏" "▎" "▍" "▌" "▋" "▊" "▉" "▉" "▊" "▋" "▌" "▍" "▎" "▏"])
    (horizontal-breathing-long
     . ["  " "▎ " "▌ " "▊ " "█ " "█▎" "█▌" "█▊" "██" "█▊" "█▌" "█▎" "█ " "▊ " "▋ " "▌ " "▍ " "▎ " "▏ "])
    (horizontal-moving . ["  " "▌ " "█ " "▐▌" " █" " ▐"])
    (minibox . ["▖" "▘" "▝" "▗"])
    (triangle . ["◢" "◣" "◤" "◥"])
    (box-in-box . ["◰" "◳" "◲" "◱"])
    (box-in-circle . ["◴" "◷" "◶" "◵"])
    (half-circle . ["◐" "◓" "◑" "◒"])
    (moon . ["🌑" "🌘" "🌖" "🌕" "🌔" "🌒"]))
  "Predefined alist of spinners.
Each car is a symbol identifying the spinner, and each cdr is a
vector, the spinner itself.")

(defvar spinner-current nil
  "Spinner curently being displayed on the mode-line.")
(make-variable-buffer-local 'spinner-current)

(defvar spinner--counter 0
  "Current frame of the spinner.")
(make-variable-buffer-local 'spinner--counter)

(defun spinner-make-construct (spinner-var timer-var)
  "Make a mode-line spinner construct, using symbol SPINNER-VAR.
SPINNER-VAR is the name of the variable holding the spinner type
to be used (one of the cdr's in `spinner-types').  To st"
  `((,spinner-var
     (" "
      (:eval (elt ,spinner-var
                  (% spinner--counter
                     (length ,spinner-var)))))
     (,timer-var
      (:eval (spinner-stop ,spinner-var ,timer-var))))))

(defconst spinner--mode-line-construct
  (spinner-make-construct 'spinner-current 'spinner--timer)
  "Construct used to display the spinner.")
(put 'spinner--mode-line-construct 'risky-local-variable t)

(defvar spinner--timer nil
  "Holds the timer being used on the current buffer.")
(make-variable-buffer-local 'spinner--timer)

(defvar spinner-frames-per-second 5
  "Default speed at which spinners spin, in frames per second.
Applications can override this value.")


;;; The main functions
(defun spinner-start-timer (fps spinner-var timer-var)
  "Start a spinner timer at FPS frames per second.
SPINNER-VAR is the name of the variable holding the spinner type,
and TIMER-VAR is the name of the variable that will be used to
hold the timer."
  (let ((old-timer (symbol-value timer-var)))
    (when (timerp old-timer)
      (cancel-timer old-timer))
    ;; Create timer.
    (let ((buffer (current-buffer))
          ;; Create the timer as a lex variable so it can cancel itself.
          (timer (run-at-time t
                              (/ 1.0 (or fps spinner-frames-per-second))
                              #'ignore)))
      (timer-set-function
       timer (lambda ()
               (if (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (setq spinner--counter (1+ spinner--counter))
                     (force-mode-line-update))
                 (ignore-errors (cancel-timer timer)))))
      (set timer-var timer)
      ;; Return a stopping function.
      (lambda () (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (spinner-stop spinner-var timer-var)))))))

;;;###autoload
(defun spinner-start (&optional type fps noadd)
  "Start a mode-line spinner of given TYPE.
Spinners are buffer local. It is added to the mode-line in the
buffer where `spinner-start' is called.

Return value is a function which can be called anywhere to stop
this spinner.  You can also call `spinner-stop' in the same
buffer where the spinner was created.

FPS, if given, is the number of desired frames per second.
Default is `spinner-frames-per-second'.

If NOADD is non-nil, the spinner is not added to the mode-line.
It is then your responsibility to add the symbol
`spinner--mode-line-construct' somewhere in the mode-line,
probably as part of a minor-mode lighter.

If TYPE is nil, use the first element of `spinner-types'.
If TYPE is `random', use a random element of `spinner-types'.
If it is a symbol, it specifies an element of `spinner-types'.
If it is a vector, it used as the spinner.
If it is a list, it should be a list of symbols, and a random one
is chosen as the spinner type."
  ;; Choose type.
  (setq spinner-current
        (cond
         ((vectorp type) type)
         ((not type) (cdr (car spinner-types)))
         ((eq type 'random)
          (cdr (elt spinner-types
                    (random (length spinner-types)))))
         ((listp type)
          (cdr (assq (elt type (random (length type)))
                     spinner-types)))
         ((symbolp type) (cdr (assq type spinner-types)))
         (t (error "Unknown spinner type: %s" type))))
  (setq spinner--counter 0)

  ;; Maybe add to mode-line.
  (unless (or noadd
              (memq 'spinner--mode-line-construct mode-line-process))
    (setq mode-line-process
          (list (or mode-line-process "")
                'spinner--mode-line-construct)))

  ;; Create timer.
  (spinner-start-timer fps 'spinner-current 'spinner--timer))

(defun spinner-stop (&optional spinner-var timer-var)
  "Stop the current buffer's spinner."
  (let ((timer (symbol-value timer-var)))
    (when (timerp timer)
      (cancel-timer timer))
    (set (or timer-var 'spinner--timer) nil)
    (set (or spinner-var 'spinner-current) nil)))

(provide 'spinner)

;;; spinner.el ends here
