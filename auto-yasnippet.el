;;; auto-yasnippet.el --- Quickly create disposable yasnippets

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/auto-yasnippet
;; Version: 0.3
;; Package-Requires: ((yasnippet "0.8.0"))

;; This file is not part of GNU Emacs

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
;; 1. Download yasnippet from https://github.com/capitaomorte/yasnippet
;;    and set it up.
;;
;; 2. Put this file into your elisp folder.
;;
;; 3. In your .emacs file:
;;     (require 'auto-yasnippet)
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
;;
;; e.g. in C++ write:
;; const Point<3> curl(grad[~2][~1] - grad[~1][~2],
;;
;; select the region between the paren and the comma and call `aya-create'.
;;
;; You can easily obtain the final code:

;; const Point<3> curl(grad[2][1] - grad[1][2],
;;                     grad[0][2] - grad[2][0],
;;                     grad[1][0] - grad[0][1]);
;;
;; Note how annoying it would be to triple check that the indices match.
;; Now you just have to check for one line.

;;; Code:
(require 'yasnippet)

(defgroup auto-yasnippet nil
  "Auto YASnippet."
  :group 'yasnippet)

(defcustom aya-persist-snippets-dir
  "~/.emacs.d/snippets"
  "Directory to save auto yasnippets."
  :type 'directory)

(defvar aya-current ""
  "Used as snippet body, when `aya-expand' is called.")

(defvar aya-marker "~"
  "Used to mark fields and mirrors.
Another good option is \\$, if you don't care about LaTeX")

(defvar aya-marker-one-line "$"
  "Used to mark one mirror for `aya-create-one-line'.")

(defvar aya-field-regex "\\([A-Za-z0-9-_]+\\)"
  "Defines how the filed looks like.
With the default [A-Za-z0-9-], Foo_bar will expand to $1_bar.
But if you set [A-Za-z0-9-_], Foo_bar will expand to $1.")

;; you can chain `aya-create' with a function of your choosing,
;; e.g. copy current line/region if there's no snippet
;; when `aya-create' is called.
(defvar aya-default-function nil
  "Function to call if no snippet markers were on line / in region.")
(make-variable-buffer-local 'aya-default-function)

;;;###autoload
(defun aya-create-one-line ()
  "A simplistic `aya-create' to create only one mirror.
You can still have as many instances of this mirror as you want.
It's less flexible than `aya-create', but faster.
It uses a different marker, which is `aya-marker-one-line'.
You can use it to quickly generate one-liners such as
menu.add_item(spamspamspam, \"spamspamspam\")"
  (interactive)
  (let* ((beg (line-beginning-position))
         (end (line-end-position))
         (line (buffer-substring-no-properties beg (point))))
    (when (string-match "\\$" line)
      (setq line
            (concat
             (replace-regexp-in-string "\\$" "$1" line)
             "$1"
             (buffer-substring-no-properties (point) end)))
      (delete-region beg end)
      (setq aya-current line)
      (yas-expand-snippet line))))

(defun aya-create-symbol ()
  "A simplistic `aya-create' that only mirrors a symbol verbatim.
The symbol should be bounded by `'. The very first match is used.
Works either on current line or region."
  (let* ((beg (if (region-active-p)
                  (region-beginning)
                (line-beginning-position)))
         (end (if (region-active-p)
                  (region-end)
                (line-end-position)))
         (str (buffer-substring-no-properties beg end))
         (case-fold-search nil))
    (when (string-match "`[^']+'" str)
      (let ((sym (substring (match-string 0 str) 1 -1)))
        (setq str (replace-match sym nil nil str))
        (setq aya-current
              (replace-regexp-in-string
               (regexp-quote sym)
               "$1" str))
        (delete-region beg end)
        (insert str)
        t))))

;;;###autoload
(defun aya-create ()
  "Works on either the current line, or, if `mark-active', the current region.
Removes `aya-marker' prefixes,
writes the corresponding snippet to `aya-current',
with words prefixed by `aya-marker' as fields, and mirrors properly set up."
  (interactive)
  (cond
   ((aya-create-one-line))
   ((aya-create-symbol))
   (t
    (let* ((head (if mark-active
                     (region-beginning)
                   (save-excursion (back-to-indentation) (point))))
           (tail (if mark-active (region-end) (line-end-position)))
           (s (buffer-substring-no-properties head tail)))
      (cl-labels
          ((parse
            (in vars out)
            (if in
                (let ((p (string-match (concat
                                        aya-marker
                                        aya-field-regex)
                                       in)))
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
              (replace-regexp-in-string "\\\\" "\\\\\\\\"
                                        (parse s (list (cons "" 0)) "")))
        (if (string-match "\\$" aya-current)
            (progn
              (delete-region head tail)
              (insert (replace-regexp-in-string aya-marker "" s)))
          ;; try some other useful action if it's defined for current buffer
          (and (functionp aya-default-function)
               (funcall aya-default-function))))))))

(defun aya-expand ()
  "Insert the last yasnippet created by `aya-create'."
  (interactive)
  (if (region-active-p)
      (let ((str (buffer-substring-no-properties
                  (region-beginning)
                  (region-end))))
        (yas-expand-snippet (replace-regexp-in-string
                             "\\$1"
                             "$0"
                             aya-current))
        (insert str))
    (yas-expand-snippet aya-current)))

(defvar aya-invokation-buffer nil
  "The buffer where `yas-expand' was called.")

;; here's a use-case for this one:
;;
;; # -*- mode: snippet -*-
;; # name: println
;; # condition: (> aya-invokation-point 10)
;; # key: p
;; # --
;; System.out.println($0);
;;
;; # -*- mode: snippet -*-
;; # name: package
;; # condition: (< aya-invokation-point 10)
;; # key: p
;; # --
;; `(insert (concat "package " (java-package-name (buffer-file-name)) ";\n"))`
;;
;; Both snippets share the same key "p" based on the `aya-invokation-point'.
(defvar aya-invokation-point nil
  "The point in buffer where `yas-expand' was called.")

;; here's a use-case of this one:
;;
;; # -*- mode: snippet -*-
;; # name: short comment
;; # key: sc
;; # --
;; //———$1${1:$(make-string (- 47 aya-tab-position (length text)) ?—)}$0
;;
;; This snippet will produce comment separators of consistent length
;; no matter from which indent position it was called from
(defvar aya-tab-position nil
  "The distance from line beginning where `yas-expand' was called.")

;;;###autoload
(defun aya-open-line ()
  "Call `open-line', unless there are abbrevs or snippets at point.
In that case expand them.  If there's a snippet expansion in progress,
move to the next field.  Call `open-line' if nothing else applies."
  (interactive)
  (cond ((expand-abbrev))

        ((yas--snippets-at-point)
         (yas-next-field-or-maybe-expand))

        ((ignore-errors
           (setq aya-invokation-point (point))
           (setq aya-invokation-buffer (current-buffer))
           (setq aya-tab-position (- (point) (line-beginning-position)))
           (yas-expand)))

        (t
         (open-line 1))))

;;;###autoload
(defun aya-yank-snippet ()
  "Insert current snippet at point.
To save a snippet permanently, create an empty file and call this."
  (interactive)
  (unless (= 0 (buffer-size))
    (error "Must be called from an empty file"))
  (insert "# -*- mode: snippet -*-\n")
  (insert "# name: \n# key: \n# --\n")
  (insert aya-current))

(defun aya-insert-snippet-function-extra (name)
  "Insert the snippet body based on NAME."
  (let ((key (read-string "Snippet key: ")))
    (insert
     "# -*- mode: snippet -*-"
     "\n# contributor: " user-full-name
     "\n# name: " name
     "\n# key: " key
     "\n# --\n"
     aya-current)
    t))

(defun aya-insert-snippet-function-default (name)
  "Insert the snippet body based on NAME."
  (insert
   "# -*- mode: snippet -*-"
   "\n# contributor: " user-full-name
   "\n# name: " name
   "\n# key: "
   "\n# --\n"
   aya-current)
  nil)

(defvar aya-insert-snippet-function
  #'aya-insert-snippet-function-default
  "The function for inserting a snippet body.
When it returns non-nil, save and close the buffer after inserting.")

(defun aya-persist-snippet (name)
  "Persist the current snippet in file NAME.

The full path is `aya-persist-snippets-dir'/`major-mode'/NAME.

Make sure to configure yasnippet to scan `aya-persist-snippets-dir'
for snippets.

Use `yas/reload-all' after defining a batch of snippets,
or `yas-load-snippet-buffer' for the current one.

Customizing `aya-insert-snippet-function' affects the behavior."
  (interactive
   (if (eq aya-current "")
       (user-error "Aborting: You don't have a current auto-snippet defined")
     (list
      (read-string "Snippet name: "))))
  (let ((default-directory
         (format "%s/%S" aya-persist-snippets-dir major-mode)))
    (unless (file-exists-p default-directory)
      (make-directory default-directory t))
    (if (file-exists-p name)
        (user-error
         "A snippet called \"%s\" already exists in \"%s\""
         name default-directory)
      (with-current-buffer (find-file-noselect name)
        (if (funcall aya-insert-snippet-function name)
            (progn
              (save-buffer)
              (kill-buffer))
          (snippet-mode)
          (goto-char (point-min))
          (search-forward "key: ")
          (pop-to-buffer (current-buffer)))))))

(provide 'auto-yasnippet)

;;; auto-yasnippet.el ends here
