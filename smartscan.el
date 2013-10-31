;;; smartscan.el --- Jumps between other symbols found at point

;; Copyright (C) 2011-2013 Mickey Petersen

;; Author: Mickey Petersen <mickey@masteringemacs.org>
;; Keywords: extensions

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
;; This code comes from my article Effective Editing I: Movement"
;; article on
;; http://www.masteringemacs.org/articles/2011/01/14/effective-editing-movement/
;;
;; A long time ago I used an IDE feature that let you quickly search
;; up or down for whatever identifier the point was on, and I decided
;; I wanted something like it in Emacs so I wrote the code snippet you
;; see below. Now, it's perfectly possible to do what it does with
;; isearch but I use the code as it avoids the hassle of fidgeting
;; with isearch to get the term under point into the search field.

;; In the code below the commands are bound to M-p and M-n.

;; Smart Scan's main advantage over isearch is that all you need to do
;; is move the point to whatever identifier you wish to search for and
;; then press M-n to find the next match in the buffer. The main use
;; is not that you go "looking" for the identifier you wish to search
;; for first: you are probably better off using isearch then; no, the
;; main advantage is when you're already writing code -- or stepping
;; through it with a debugger -- then smart scan will beat out
;; isearch.

;; Smart Scan is also clever enough to ignore comments and strings
;; containing the identifier you are looking for.

;;; Customizations

;; You can customize `smart-use-extended-syntax' to alter
;; (temporarily, when you search) the syntax table used by Smart Scan
;; to find matches in your buffer.

;;; Code:

(provide 'smart-scan)


;;; Default Keybindings
(global-set-key (kbd "M-n") 'smart-symbol-go-forward)
(global-set-key (kbd "M-p") 'smart-symbol-go-backward)

(defvar smart-use-extended-syntax nil
  "If t the smart symbol functionality will consider extended
syntax in finding matches, if such matches exist.")

(defvar smart-last-symbol-name ""
  "Contains the current symbol name.

This is only refreshed when `last-command' does not contain
either `smart-symbol-go-forward' or `smart-symbol-go-backward'")

(defvar smart-symbol-old-pt nil
  "Contains the location of the old point")

(make-local-variable 'smart-use-extended-syntax)

(defun smart-symbol-goto (name direction)
  "Jumps to the next NAME in DIRECTION in the current buffer.

DIRECTION must be either `forward' or `backward'; no other option
is valid."

  ;; if `last-command' did not contain
  ;; `smart-symbol-go-forward/backward' then we assume it's a
  ;; brand-new command and we re-set the search term.
  (unless (memq last-command '(smart-symbol-go-forward
                               smart-symbol-go-backward))
    (setq smart-last-symbol-name name))
  (setq smart-symbol-old-pt (point))
  (message (format "%s scan for symbol \"%s\""
                   (capitalize (symbol-name direction))
                   smart-last-symbol-name))
  (with-smart-symbol
    (unless (catch 'done
              (while (funcall (cond
                               ((eq direction 'forward) ; forward
                                're-search-forward)
                               ((eq direction 'backward) ; backward
                                're-search-backward)
                               (t (error "Invalid direction"))) ; all others
                              (concat "\\<" smart-last-symbol-name "\\>") nil t)
                (unless (memq (syntax-ppss-context
                               (syntax-ppss (point))) '(string comment))
                  (throw 'done t))))
      (goto-char smart-symbol-old-pt))))

(defun smart-symbol-go-forward ()
  "Jumps forward to the next symbol at point"
  (interactive)
  (smart-symbol-goto (smart-symbol-at-pt 'end) 'forward))

(defun smart-symbol-go-backward ()
  "Jumps backward to the previous symbol at point"
  (interactive)
  (smart-symbol-goto (smart-symbol-at-pt 'beginning) 'backward))

(defmacro with-smart-symbol (body)
  "Macro that initialises the syntax table"
  (declare (indent defun))
  `(with-syntax-table (make-syntax-table)
     (if smart-use-extended-syntax
         (modify-syntax-entry ?. "w"))
     ;; we need this outside the if-statement as using the word
     ;; parameter with `thing-at-point' will treat underscore as a word
     ;; separator.
     (modify-syntax-entry ?_ "w")
     (modify-syntax-entry ?- "w")
     ,body))

  
(defun smart-symbol-at-pt (&optional dir)
  "Returns the symbol at point and moves point to DIR (either `beginning' or `end') of the symbol.

If `smart-use-extended-syntax' is t then that symbol is returned
instead."
  ;; we need a quick-and-dirty syntax table hack here to make
  ;; `thing-at-point' pick up on the fact that '.', '_', etc. are all
  ;; part of a single expression.
  (with-smart-symbol
    ;; grab the word and return it
    (let ((word (thing-at-point 'word))
          (bounds (bounds-of-thing-at-point 'word)))
      (if word
          (progn
            (cond
             ((eq dir 'beginning) (goto-char (car bounds)))
             ((eq dir 'end) (goto-char (cdr bounds)))
             (t (error "Invalid direction")))
            word)
        (error "No symbol found")))))


(provide 'smartscan)
;;; smartscan.el ends here
