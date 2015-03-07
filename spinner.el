;;; spinner.el --- Mode-line spinner for operations in progress  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Artur Malabarba

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; Version: 1.0
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

;; Run `(spinner-start)' to see the effect.


;;; Code:

(defconst spinner-types
  '((3-line-clock . ["┤" "┘" "┴" "└" "├" "┌" "┬" "┐"])
    (2-line-clock . ["┘" "└" "┌" "┐"])
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

(defconst spinner--mode-line-construct
  '((spinner-current
     (" "
      (:eval (elt spinner-current
                  (% spinner--counter
                     (length spinner-current)))))
     (spinner--timer
      (:eval (spinner-stop)))))
  "Construct used to display the spinner.")
(put 'spinner--mode-line-construct 'risky-local-variable t)

(defvar spinner--timer nil
  "Holds the timer being used on the current buffer.")
(make-variable-buffer-local 'spinner--timer)

(defvar spinner-frames-per-second 5
  "Default speed at which spinners spin, in frames per second.
Applications can override this value.")


;;; The main function
;;;###autoload
(defun spinner-start (&optional type fps)
  "Start a mode-line spinner of given TYPE.
Spinners are buffer local. It is added to the mode-line in the
buffer where `spinner-start' is called.

Return value is a function which can be called anywhere to stop
this spinner.  You can also call `spinner-stop' in the same
buffer where the spinner was created.

FPS, if given, is the number of desired frames per second.
Default is `spinner-frames-per-second'.

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
  (unless (memq 'spinner--mode-line-construct mode-line-format)
    (setq mode-line-format (copy-list mode-line-format))
    (let ((cell (memq 'mode-line-buffer-identification mode-line-format)))
      (if cell
          (setcdr cell (cons 'spinner--mode-line-construct (cdr cell)))
        (setcdr (last mode-line-format) '(spinner--mode-line-construct)))))

  ;; Create timer.
  (when (timerp spinner--timer)
    (cancel-timer spinner--timer))
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
    (setq spinner--timer timer)
    ;; Return a stopping function.
    (lambda () (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (spinner-stop))))))

(defun spinner-stop ()
  "Stop the current buffer's spinner."
  (when (timerp spinner--timer)
    (cancel-timer spinner--timer))
  (setq spinner--timer nil
        spinner-current nil)
  (setq mode-line-format
        (remove 'spinner--mode-line-construct mode-line-format)))

(provide 'spinner)

;;; spinner.el ends here
