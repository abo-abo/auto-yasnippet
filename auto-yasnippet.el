;;; auto-yasnippet.el --- Quickly create disposable yasnippets

;; Author: Oleh Krehel <ohwoeowho@gmail.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Setup:
;; 
;; 1. Download yasnippet http://code.google.com/p/yasnippet/ and set it up.
;; 2. Put this file into your elisp folder.
;; 
;; 3. In your .emacs file:
;;     (require 'yasnippet)       
;;     (global-set-key (kbd "H-w") 'aya-create)
;;     (global-set-key (kbd "H-y") 'aya-expand)

;; Usage:
;; e.g. in JavaScript write:
;; 
;; field~1 = document.getElementById("field~1");
;; 
;; Since this just one line,
;; just call `aya-create' (from anywhere on this line).
;; The ~ chars disappear, yielding valid code.
;; `aya-current' becomes:
;; "field$1 = document.getElementById(\"field$1\");"
;; Now by calling `aya-expand' multiple times, you get:
;; 
;; field1 = document.getElementById("field1");
;; field2 = document.getElementById("field2");
;; field3 = document.getElementById("field3");
;; fieldFinal = document.getElementById("fieldFinal");
;; 
;; e.g. in Java write:
;; 
;; class Light~On implements Runnable {
;;   public Light~On() {}
;;   public void run() {
;;     System.out.println("Turning ~on lights");
;;     light = ~true;
;;   }
;; }
;; 
;; This differs from the code that you wanted to write only by 4 ~ chars.
;; Since it's more than one line, select the region and call `aya-create'.
;; Again, the ~ chars disappear, yielding valid code.
;; `aya-current' becomes:
;; "class Light$1 implements Runnable {
;;   public Light$1() {}
;;   public void run() {
;;     System.out.println(\"Turning $2 lights\");
;;     light = $3;
;;   }
;; }"
;; 
;; Now by calling `aya-expand', you can quickly fill in:
;; class LightOff implements Runnable {
;;   public LightOff() {}
;;   public void run() {
;;     System.out.println("Turning off lights");
;;     light = false;
;;   }
;; }
(defvar aya-current ""
  "Used as snippet body, when 'aya-expand is called")

(defvar aya-marker "~"
  "Used to mark fields and mirrors.
Another good option is \\$, if you don't care about LaTeX")

(defvar aya-field-regex "\\([a-z0-9-_]+\\)")

(defvar aya-default-action nil
  "function to call if no snippet markers were on line / in region.")
(make-variable-buffer-local 'aya-default-action)

(defun aya-create ()
  "Works on either the current line, or, if `mark-active', the current region.
Removes `aya-marker' prefixes,
writes the corresponding snippet to `aya-current',
with words prefixed by `aya-marker' as fields, and mirrors properly set up."
  (interactive)
  (let* ((head (if mark-active 
                   (region-beginning)
                 (save-excursion (back-to-indentation) (point))))
         (tail (if mark-active (region-end) (line-end-position)))
         (s (buffer-substring-no-properties head tail)))
    (cl-labels ((parse (in vars out)
		  (if in
		      (let ((p (string-match (concat
					      aya-marker
					      aya-field-regex) in)))
			(if p
			    (let* ((var (match-string 1 in))
				   (mult (assoc var vars))
				   (vars (if mult vars
					   (cons (cons var (+ 1 (cdar vars)))
						 vars))))
			      (parse (substring in (+ p (length var) 1))
				     vars
				     (concat out
					     (substring in 0 p)
					     "$"
					     (number-to-string (if mult
								   (cdr mult)
								 (cdar vars))))))
			  (concat out in)))
		    out)))
      (setq aya-current
	    (replace-regexp-in-string "\\\\" "\\\\\\\\" (parse s (list (cons "" 0)) "")))
      (if (string-match "\\$" aya-current)
          (progn
            (delete-region head tail)
            (insert (replace-regexp-in-string aya-marker "" s)))
        ;; try some other useful action if it's defined for current buffer
        (and (functionp aya-default-action) (funcall aya-default-action))))))

(defun aya-expand ()
  "Inserts the last yasnippet created by `aya-create'"
  (interactive)
  (yas/expand-snippet aya-current))

(defvar *yas-invokation-buffer*
  "The buffer where `yas/expand' was called")

(defvar *yas-invokation-point*
  "The point in buffer where `yas/expand' was called")

(defvar *yas-tab*
  "The distance from line beginning where `yas/expand' was called")

(defun aya-open-line ()
  (interactive)
  (cond ((and (looking-back " ") (looking-at "[\s\n}]+"))
	 (insert "\n\n")
	 (indent-according-to-mode)
	 (forward-line -1)
	 (indent-according-to-mode))
	((expand-abbrev))
	(t
	 (setq *yas-invokation-point* (point))
         (setq *yas-invokation-buffer* (current-buffer))
	 (setq *yas-tab* (- (point) (line-beginning-position)))
         ;; Try to expand a snippet at a key before point,
         ;; otherwise delegate to `yas/next-field'
         (if (or (and (fboundp 'yas/snippets-at-point) (yas/snippets-at-point))
                 (and (fboundp 'yas--snippets-at-point) (yas--snippets-at-point)))
             (yas/next-field)
           (yas/expand)))))

(provide 'auto-yasnippet)
;;; auto-yasnippet.el ends here

