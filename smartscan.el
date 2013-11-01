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
;; Smart Scan let's you jump between symbols in your buffer, based on
;; the initial symbol your point was on when you started the
;; search. Incremental calls will still respect the original search
;; query so you can move up or down in your buffer, quickly, to find
;; other matches without having to resort to `isearch' to find things
;; first. The main advantage over isearch is speed: Smart Scan will
;; guess the symbol point is on and immediately find other symbols
;; matching it, in an unintrusive way.
;;; HOW TO USE IT
;;
;; Simply type `smart-symbol-go-forward' (or press M-n) to go forward;
;; or `smart-symbol-go-backward' (M-p) to go back.

;;; Customizations

;; You can customize `smart-use-extended-syntax' to alter
;; (temporarily, when you search) the syntax table used by Smart Scan
;; to find matches in your buffer.

;;; Code:

(provide 'smartscan)


;;; Default Keybindings
(global-set-key (kbd "M-n") 'smartscan-symbol-go-forward)
(global-set-key (kbd "M-p") 'smartscan-symbol-go-backward)

(defvar smartscan-use-extended-syntax nil
  "If t the smart symbol functionality will consider extended
syntax in finding matches, if such matches exist.")

(defvar smartscan-last-symbol-name ""
  "Contains the current symbol name.

This is only refreshed when `last-command' does not contain
either `smartscan-symbol-go-forward' or `smartscan-symbol-go-backward'")

(defvar smartscan-symbol-old-pt nil
  "Contains the location of the old point")

(make-local-variable 'smartscan-use-extended-syntaxp)

(defun smartscan-symbol-goto (name direction)
  "Jumps to the next NAME in DIRECTION in the current buffer.

DIRECTION must be either `forward' or `backward'; no other option
is valid."

  ;; if `last-command' did not contain
  ;; `smartscan-symbol-go-forward/backward' then we assume it's a
  ;; brand-new command and we re-set the search term.
  (unless (memq last-command '(smartscan-symbol-go-forward
                               smartscan-symbol-go-backward))
    (setq smartscan-last-symbol-name name))
  (setq smartscan-symbol-old-pt (point))
  (message (format "%s scan for symbol \"%s\""
                   (capitalize (symbol-name direction))
                   smartscan-last-symbol-name))
  (smartscan-with-symbol
    (unless (catch 'done
              (while (funcall (cond
                               ((eq direction 'forward) ; forward
                                're-search-forward)
                               ((eq direction 'backward) ; backward
                                're-search-backward)
                               (t (error "Invalid direction"))) ; all others
                              (concat "\\<" smartscan-last-symbol-name "\\>") nil t)
                (unless (memq (syntax-ppss-context
                               (syntax-ppss (point))) '(string comment))
                  (throw 'done t))))
      (goto-char smartscan-symbol-old-pt))))

(defun smartscan-symbol-go-forward ()
  "Jumps forward to the next symbol at point"
  (interactive)
  (smartscan-symbol-goto (smartscan-symbol-at-pt 'end) 'forward))

(defun smartscan-symbol-go-backward ()
  "Jumps backward to the previous symbol at point"
  (interactive)
  (smartscan-symbol-goto (smartscan-symbol-at-pt 'beginning) 'backward))

(defmacro smartscan-with-symbol (body)
  "Macro that initialises the syntax table"
  (declare (indent defun))
  `(with-syntax-table (make-syntax-table)
     (if smartscan-use-extended-syntax
         (modify-syntax-entry ?. "w"))
     ;; we need this outside the if-statement as using the word
     ;; parameter with `thing-at-point' will treat underscore as a word
     ;; separator.
     (modify-syntax-entry ?_ "w")
     (modify-syntax-entry ?- "w")
     ,body))

  
(defun smartscan-symbol-at-pt (&optional dir)
  "Returns the symbol at point and moves point to DIR (either `beginning' or `end') of the symbol.

If `smartscan-use-extended-syntax' is t then that symbol is returned
instead."
  ;; we need a quick-and-dirty syntax table hack here to make
  ;; `thing-at-point' pick up on the fact that '.', '_', etc. are all
  ;; part of a single expression.
  (smartscan-with-symbol
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
