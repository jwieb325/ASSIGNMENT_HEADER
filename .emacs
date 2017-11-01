;;; column-enforce-mode.el --- Highlight text that extends beyond a  column
;;
;; Filename: column-enforce-mode.el
;; Description:
;; Author: Jordon Biondo
;; Maintainer:
;; Created: Fri Oct 11 12:14:25 2013 (-0400)
;; Version: 1.0.4
;; Package-Requires: ()
;; Last-Updated: Sun Dec  8 20:23:51 2013 (-0500)
;;           By: Jordon Biondo
;;     Update #: 13
;; URL: www.github.com/jordonbiondo/column-enforce-mode
;; Keywords: 
;; Compatibility: >= Emacs 22.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Highlight text that extends beyond a certain column (80 column rule)
;;
;;  By default, text after 80 columns will be highlighted in red
;;
;;  To customize behavior, see `column-enforce-column' and `column-enforce-face'
;;
;;  To enable: M-x column-enforce-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; don't judge me
(require 'cl)

(defgroup column-enforce nil
  "Highlight text that extends beyond a certain column (80 column rule)"
  :group 'convenience)


(defcustom column-enforce-column 80
  "Highlight text extending beyond this many columns \
when using function `column-enforce-mode'."
  :type 'integer
  :group 'column-enforce)

(defcustom column-enforce-comments t
  "Non-nil means to mark comments that exceed the column limit."
  :type 'boolean
  :group 'column-enforce)

(defcustom column-enforce-column-getter nil
  "A function that will return the maximun column for the current line.
Using this variable will override the value of `column-enforce-column',
the function will be called with no arguments and will be expected to return a
number to use in place of `column-enforce-column'. This can be used for
changing the max column based on context, such as restricting the column count
further on the first line."
  :type 'function
  :group 'column-enforce)

(defun column-enforce-get-column ()
  "Gets the value of variable `column-enforce-column' or if nil, \
the value of variable `fill-column', or if nil, 80."
  (let ((getter (if (functionp column-enforce-column-getter)
                    column-enforce-column-getter
                  'column-enforce-default-column-getter)))
    (or
     (ignore-errors
       (save-excursion
         (funcall getter)))
     80)))
(defun column-enforce-default-column-getter ()
  (or column-enforce-column fill-column 80))

(defface column-enforce-face
  `((t (:inherit font-lock-warning-face :underline t)))
  "Face to be used to highlight lines confilicting the the current column rule"
  :group 'column-enforce)


(defvar column-enforce-face 'column-enforce-face
  "Face to be used to highlight lines confilicting the the current column rule")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun column-enforce-n (n)
  "Turn on `column-enforce-mode' with warnings at column N.
N can be given as a prefix argument.
Ex:
  C-u 70 M-x column-enforce-n <enter>
  sets up `column-enforce-mode' to mark \
text that extends beyond 70 columns."
  (interactive "P")
  (let ((n (if (and n (integerp n)) n column-enforce-column)))
    (setq column-enforce-mode-line-string
          (column-enforce-make-mode-line-string n))
    (column-enforce-mode -1)
    (set (make-local-variable 'column-enforce-column) n)
    (column-enforce-mode t)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predefined column rules
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;###autoload
(defmacro make-column-rule(n)
  "Create an interactive function to enforce an N-column-rule."
  `(let ((__n ,n))
     (assert (integerp __n) nil "Wrong type argument")
     (eval `(defun ,(intern (format "%d-column-rule" __n)) ()
	            ,(format "Visually mark text after %d columns." __n)
		          (interactive)
			        (if (and column-enforce-mode (= ,__n (column-enforce-get-column)))
				      (column-enforce-mode -1)
				  (column-enforce-n ,__n))))))

(make-column-rule 100)
(make-column-rule 90)
(make-column-rule 80)
(make-column-rule 70)
(make-column-rule 60)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun column-enforce-make-mode-line-string(rule)
  "Returns the string to display in the mode line"
  (format " %scol" rule))

(defvar column-enforce-mode-line-string
  (column-enforce-make-mode-line-string (column-enforce-get-column))
  "The current string for the mode line.")

;;;###autoload
(define-minor-mode column-enforce-mode
  "Minor mode for highlighting text that extends beyond a certain column.
Variable `column-enforce-column' decides which column to start warning at.
 Default is 80
Variable `column-enforce-face' decides how to display the warnings"
  :init-value nil
  :lighter column-enforce-mode-line-string
  :keymap nil
  :global nil
  (setq column-enforce-mode-line-string
        (column-enforce-make-mode-line-string (column-enforce-get-column)))
  (if column-enforce-mode
      ;; use add-hook so we can append it, (force it to run last)
      (progn
        (jit-lock-register 'column-enforce-warn-on-region t)
        (column-enforce-warn-on-region (point-min) (point-max)))
    (progn
      (dolist (ov (column-enforce-get-cem-overlays-in (point-min) (point-max)))
        (delete-overlay ov))
      (jit-lock-unregister 'column-enforce-warn-on-region))))

(defun column-enforce-mode-toggle-if-applicable ()
  (if column-enforce-mode
      (column-enforce-mode -1)
    (when (derived-mode-p  'prog-mode)
      (column-enforce-mode t))))

;;;###autoload
(define-global-minor-mode global-column-enforce-mode column-enforce-mode
  column-enforce-mode-toggle-if-applicable)

;; internal
(defun column-enforce-get-cem-overlays-in (beg end)
  "Get all overlays between BEG and END that have a 'is-cem-ov property."
  (remove-if-not (lambda (ov) (overlay-get ov 'is-cem-ov))
                 (overlays-in beg end)))

(defun column-enforce-warn-on-region (beg end)
  "Jit lock function for function `column-enforce-mode' that will \
mark text that extends beyond `column-enforce-column' with the \
`column-enforce-face' using overlays between BEG and END."
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (let ((cem-ovs (column-enforce-get-cem-overlays-in
                      (point-at-bol) (point-at-eol))))
        (dolist (ov cem-ovs) (delete-overlay ov))
        (move-to-column (column-enforce-get-column))
        (if (and (not (= (point) (point-at-eol)))
                 (or column-enforce-comments
                     (not (equal (syntax-ppss-context (syntax-ppss (point)))
                                 'comment))))
            (let ((new-ov (make-overlay (point)
                                        (point-at-eol)
                                        nil t t)))
              (overlay-put new-ov 'face 'column-enforce-face)
              (overlay-put new-ov 'is-cem-ov t)))
        (forward-line 1)))))


(provide 'column-enforce-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; column-enforce-mode.el ends here

(add-hook 'c++-mode-hook 'column-enforce-mode)
(add-hook 'c++-mode-hook 'flyspell-prog-mode)
(add-hook 'c-mode-hook 'column-enforce-mode)
(add-hook 'c-mode-hook 'flyspell-prog-mode)

(defun save-and-compile () 
  (interactive) 
  (save-some-buffers 1)
  (compile compile-command)
  (kbd "C-x b")
  )

; from enberg on #emacs
(setq compilation-finish-function
  (lambda (buf str)
    (if (null (string-match ".*exited abnormally.*" str))
        ;;no errors, make the compilation window go away in a few seconds
        (progn
          (run-at-time
           "2 sec" nil 'delete-windows-on
           (get-buffer-create "*compilation*"))
          (message "No Compilation Errors!")))))

;removes lines at #if DEBUG until #endif
(defun remove-debug-statements ()
  (interactive)
  (re-search-forward "#define DEBUG.*" nil t)
  (replace-match "")
  (while (re-search-forward "#if DEBUG\\(.\\|\n\\)*?#endif" nil t)
    (replace-match "")
    )
  )

(defun expand_macros_into_comments()
  (interactive)
  (save-excursion
    (while (re-search-forward "#define \\([A-Z_]+\\) \\(.+\$\\)" nil t)
      (save-excursion
	(save-match-data
	  (let (( macro (match-string 1)) ( value (match-string 2)) (pos (match-end 0)))
	    (message "found macro: %s value: %s" macro value (buffer-name))
	    (while(re-search-forward (concat "\\(/\\*[\0-\377[:nonascii:]]*?+\\*/\\)\\|\\(//.*\\)") nil t )
	      (message "found comment %s" sub (buffer-name))
	      (let ((limit (match-beginning 0)))
		(save-match-data
		  (while(re-search-backward macro limit t )
		    (message "found match %s" (match-end 0) (buffer-name))
		    (replace-match value)))
		)
	      )
	    )
	  )
	)
      )
    )
  )

(defun window-toggle-split-direction ()
  "Switch window split from horizontally to vertically, or vice versa.

i.e. change right window to bottom, or change bottom window to right."
  (interactive)
  (require 'windmove)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))

;;insert date/time
(defun insert-current-date () (interactive)
    (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

;; enable ASNI colouring
(require 'ansi-color)
(defun my/ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)

(setq gdb-show-main t)
(setq-default fill-column 80)

(global-set-key (kbd "C-x j") 'ff-find-other-file)

(global-set-key (kbd "C-c C-j") 'windmove-left)
(global-set-key (kbd "C-c C-k ") 'windmove-right)
(global-set-key (kbd "C-c C-h") 'windmove-up)
(global-set-key (kbd "C-c C-l") 'windmove-down)

(defun append-kill-line (&optional arg)
  " "
  (interactive)
  (if (use-region-p)
    (append-to-buffer "kill" (region-beginning) (region-end))			      		    
      (append-to-buffer "kill" (line-beginning-position) (line-beginning-position 2))
    )
  )

(global-set-key (kbd "C-x |") 'window-toggle-split-direction)
(global-set-key (kbd "C-x C-k") 'append-kill-line)
(global-set-key (kbd "C-x C-[")(kbd "{ RET } <up> C-e RET TAB")) 

(global-set-key (kbd "C-x C-d") 'save-and-compile)
;; Red Hat Linux default .emacs initialization file

;; Are we running XEmacs or Emacs?
(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.
(global-set-key [delete] 'delete-char)
(global-set-key [select] 'end-of-line)
(global-set-key [kp-1] "1")
(global-set-key [kp-2] "2")
(global-set-key [kp-3] "3")
(global-set-key [kp-4] "4")
(global-set-key [kp-5] "5")
(global-set-key [kp-6] "6")
(global-set-key [kp-7] "7")
(global-set-key [kp-8] "8")
(global-set-key [kp-9] "9")
(global-set-key [kp-0] "0")
(global-set-key [kp-add] "+")
(global-set-key [kp-subtract] "-")
(global-set-key [kp-multiply] "*")
(global-set-key [kp-divide] "/")
(global-set-key [kp-decimal] ".")
(global-set-key [kp-enter] 'newline)
(global-set-key [kp-delete] 'delete-char)
(global-set-key [kp-end] 'end-of-line)
(global-set-key [kp-select] 'end-of-line)
(global-set-key [kp-home] 'beginning-of-line)
(global-set-key [kp-insert] 'overwrite-mode)
(global-set-key [kp-prior] 'scroll-down)
(global-set-key [kp-next] 'scroll-up)

;; Visual feedback on selections
(setq-default transient-mark-mode t)

;; Always end a file with a newline
(setq require-final-newline t)

;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

;; Enable wheelmouse support by default
(cond (window-system
       (mwheel-install)
))

;;for c/c++ compiles
(defun cplusplus-compile-command(comm)
  (make-local-variable 'compile-command)
  (if (or (file-exists-p "makefile")
           (file-exists-p "Makefile"))
       (setq compile-command "make -k all ")
       (setq compile-command
             (concat comm " "
                   (file-name-nondirectory buffer-file-truename)
                   " -ansi -pedantic-errors -Wall -Wconversion -o "
                   (file-name-sans-extension (file-name-nondirectory buffer-file-truename) )))))

;;for java compile
(defun java-compile-command()
  (make-local-variable 'compile-command)
  (setq compile-command (concat "javac " (file-name-nondirectory buffer-file-truename)))
)

;;for php lint
(defun php-compile-command()
  (make-local-variable 'compile-command)
  (setq compile-command (concat "php -l " (file-name-nondirectory buffer-file-truename)))
)

(add-hook 'c++-mode-hook (lambda() (cplusplus-compile-command "g++")))
(add-hook 'c-mode-hook (lambda() (cplusplus-compile-command "gcc")))
(add-hook 'java-mode-hook (lambda() (java-compile-command)))
(add-hook 'php-mode-hook (lambda() (php-compile-command)))

(defun my-c-style ()
  "loads my custom c-style."
  (c-set-style "stroustrup")
  (c-set-offset 'inline-open '0)
  (setq c-indent-comments-syntactically-p t
        case-fold-search nil
        c-recognize-knr-p nil
        c-auto-newline nil))
(add-hook 'c-mode-common-hook 'my-c-style)

;;time 
  (defun insert-current-date () (interactive)
    (insert (shell-command-to-string "echo -n $(date +\"%x - %X\")")))

(require 'color-theme)
    (color-theme-initialize)
    (color-theme-comidia)
