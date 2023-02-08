;;; auto-yasnippet.el --- Quickly create disposable yasnippets -*- lexical-binding: t; -*-
;;
;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;;         Jason Milkins <jasonm23@gmail.com>
;;
;; Maintainer: Jason Milkins <jasonm23@gmail.com>
;;
;; URL: https://github.com/abo-abo/auto-yasnippet
;; Version: 1.0.0
;; Package-Requires: ((yasnippet "0.14.0") (emacs "25.1"))
;;
;; This file is not part of GNU Emacs
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;;
;; # Auto-YASnippet 1.0.0
;;
;; Auto-YASnippet is a hybrid of [keyboard
;; macros](http://www.gnu.org/software/emacs/manual/html_node/emacs/Basic-Keyboard-Macro.html)
;; and [YASnippet](https://github.com/joaotavora/yasnippet). You create the snippet
;; on the go and it'll be ready to use immediately. Because you're not leaving the
;; current buffer the workflow is very fast.
;;
;; All you do is enter the code you'd enter anyway but placing `~' chars where you would
;; like YASnippet fields and mirrors to be.
;;
;; ## Setup/Install
;;
;; It's easiest/recommended to install from [MELPA](http://melpa.org/).
;; Here's a minimal MELPA configuration for your `~/.emacs':
;;
;;     (package-initialize)
;;     (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;;
;; Afterwards, <kbd>M-x</kbd> `package-install RET auto-yasnippet' <kbd>RET</kbd> (you might
;; want to <kbd>M-x</kbd> `package-refresh-contents' <kbd>RET</kbd> beforehand if
;; you haven't done so recently).
;;
;; ## Configuration
;;
;; In your Emacs init file set keys for the `aya' commands.
;;
;; For example:
;;
;;     (global-set-key (kbd "C-c C-y w")   #'aya-create)
;;     (global-set-key (kbd "C-c C-y TAB") #'aya-expand)
;;     (global-set-key (kbd "C-c C-y SPC") #'aya-expand-from-history)
;;     (global-set-key (kbd "C-c C-y d")   #'aya-delete-from-history)
;;     (global-set-key (kbd "C-c C-y c")   #'aya-clear-history)
;;     (global-set-key (kbd "C-c C-y n")   #'aya-next-in-history)
;;     (global-set-key (kbd "C-c C-y p")   #'aya-previous-in-history)
;;     (global-set-key (kbd "C-c C-y s")   #'aya-persist-snippet)
;;     (global-set-key (kbd "C-c C-y o")   #'aya-open-line)
;;
;; ## Examples
;;
;; If we need to write some repetitive code in an expression:
;;
;;     count_of_red = get_total("red");
;;     count_of_blue = get_total("blue");
;;     count_of_green = get_total("green");
;;
;; We can write a template, using `~' to represent text we want to replace:
;;
;;     count_of_~red = get_total("~red");
;;
;; With the cursor on this line, or with selected text, call <kbd>M-x</kbd> `aya-create' <kbd>RET</kbd>.
;; An auto-snippet is created and the text is converted to remove the `~' marker:
;;
;;     count_of_red = get_total("red");
;;
;; Now we can call `aya-expand' and we can insert text at each marker in the
;; template, note, because both words are the same, we just type it one.
;;
;; Yasnippet controls all the interaction while expanding so refer to the yasnippet
;; docs. Basic interaction is to enter text at a marker and `TAB' to the next one.
;;
;;     count_of_[CURSOR] = get_total("");
;;
;; Say we enter `blue' once at `count_of_'` the result will be.
;;
;;     count_of_blue = get_total("blue");
;;
;; ###  Multiple placeholders
;;
;; You can replace multiple values in a template, just like normal
;; yasnippet.
;;
;; In this example, our template has multiple lines, so we need to
;; select the relevant lines before calling `aya-create'
;;
;;     ~FooType get~Foo() {
;;         // Get the ~foo attribute on this.
;;         return this.~foo;
;;     }
;;
;; We fill in two placeholders in this example (the 2nd and 4th are the same as
;; the 3rd). Yasnippet places us at 1st, entering: Type `TAB' (yasnippet puts
;; us at 3rd) entering: bar `TAB' will expand to:
;;
;;     Type getBar() {
;;       // Get the bar attribute on this.
;;       return this.foo;
;;     }
;;
;; ### Mixed case templates
;;
;; You can create mixed case templates setting `aya-case-fold' to `t'. This will result
;; in templates where variables that start with a character of a different case will be
;; treated as the same variable. The case of the first character will be preserved in the
;; resulting snippet.
;;
;; Using the earlier example with a slight twist:
;;
;;     count_of_~red = get_total("~Red");
;;
;; Then calling `aya-create', then `aya-expand', and finally typing `blue', the result
;; would be:
;;
;;     count_of_blue = get_total("Blue");
;;
;; Notice that `blue' was placed in both locations with proper casing.
;;
;; ### Expanding around a region
;;
;; If you create an auto-yasnippet with one field, it's value will be filled in
;; from the current region. For example if we create a new snippet:
;;
;;     print("\(~thing)")
;;
;; Select text:
;;
;;     myVar + 10
;;
;; `aya-expand' gives us...
;;
;;     print("\(myVar + 10)")
;;
;; You can also use the YASnippet built in marker `$0' in the point where you
;; want to finish expanding the snippet. YASnippet allows `$0' to be the region,
;; by setting `yas-wrap-around-region' to `t'.
;;
;; # Functions
;;
;; ## aya-create
;;
;; Removes "~" from current line or region (if mark is active) yielding
;; valid code. The created snippet is recorded into `aya-current' and
;; appended to `aya-history'.
;;
;; ## aya-expand
;;
;; Expands whatever is currently in `aya-current'
;;
;; ## aya-expand-from-history
;;
;; Select and expand from snippets in `aya-history'. The selected
;; snippet will become `aya-current'.
;;
;; ## aya-delete-from-history
;;
;; Select and delete a snippet from `aya-history'. The next available
;; snippet will become `aya-current'. When there are no other snippets
;; available `aya-current' will be set to `""'.
;;
;; ## aya-next-in-history & aya-previous-in-history
;;
;; Set `aya-current' to the next or previous in `aya-history'.
;;
;; ## aya-open-line
;;
;; Generic expansion function. It will either expand or move
;; to the next field depending on the context.
;;
;; ## aya-persist-snippet
;;
;; Save the current auto-snippet to a user snippets folder (this defaults to
;; `~/.emacs.d/snippets/'.)  The current `major-mode' name will be used
;; to determine the snippets sub-directory to store the snippet.  For
;; example when working in `js2-mode' the snippet will be saved to (by
;; default) `~/.emacs.d/snippets/js2-mode/'.
;;
;; You will be prompted for the snippet **name**. The appropriate file will be opened but not saved,
;; with the point on the `key: ' parameter of the snippet. If you wish to proceed, fill in the key,
;; save the buffer and call <kbd>C-c C-l</kbd> (`yas-load-snippet-buffer'). Otherwise, simply kill the
;; buffer - there will be no side effects.
;;
;; You can customize `aya-persist-snippets-dir' to use a different folder
;; for storing auto-snippets.
;;
;; You will need to run `yas/reload-all' before using the new snippet
;; with its **key** trigger.
;;
;; ## aya-persist-snippet-from-history
;;
;; Functionally equivalent to `aya-persist-snippet' but using a snippet selected
;; from `aya-history'
;;
;;; Code:

(require 'cl-lib)
(require 'yasnippet nil t)

(defgroup auto-yasnippet nil
  "Auto YASnippet."
  :group 'yasnippet)

(defcustom aya-persist-snippets-dir
  (format "%s/snippets" user-emacs-directory)
  "Directory to save auto yasnippets."
  :type 'directory)

(defcustom aya-create-with-newline nil
  "If non-nil `aya-create' creates snippet with trailing newline."
  :type 'boolean)

(defcustom aya-case-fold t
  "If non-nil `aya-create' creates snippets matching mixed cases."
  :type 'boolean)

(defcustom aya-trim-one-line nil
  "If non-nil one-line snippets will begin from the first non-space character."
  :type 'boolean)

(defvar aya-current ""
  "Used as snippet body, when `aya-expand' is called.")

(defvar aya--escape-chars-alist '(("`" . "\\\\`"))
  "An alist of chars that must be escaped for yasnippet.")

(defvar aya-history '()
  "A List of auto yasnippets created in this session.")

(defvar aya-marker "~"
  "Used to mark fields and mirrors.
Another good option is \\$, if you don't care about LaTeX")

(defvar aya-field-regex "\\sw\\|\\s_"
  "Defines how the field looks.
With \"\\sw\", Foo_bar will expand to $1_bar.
But \"\\sw\\|\\s_\", Foo_bar will expand to $1.")

(defvar aya-invocation-buffer nil
  "The buffer where `yas-expand' was called.")

;; you can chain `aya-create' with a function of your choosing,
;; e.g. copy current line/region if there's no snippet
;; when `aya-create' is called.
(defvar aya-default-function nil
  "Function to call if no snippet markers were on line / in region.")

;; Make aya-default-function buffer local
(make-variable-buffer-local 'aya-default-function)

(defun aya--maybe-append-newline (str)
  "Append newline to STR if `aya-create-with-newline' is non-nil."
  (if (and aya-create-with-newline
           (not (string= "\n" (substring str -1))))
      (concat str "\n")
    str))

(defun aya--alist-create-value-specifier (alist all)
  "Create yasnippet template specifier for value in ALIST.
Use ALL to ensure proper template is generated."
  (if (and aya-case-fold
           (cdr (assoc 'ucase alist))
           (aya--matching-lowercase-value-exists alist all))
      (format "${%d:$(aya--upcase-first-char yas-text)}" (cdr (assoc 'idx alist)))
    (format "$%d" (cdr (assoc 'idx alist)))))

(defun aya--matching-lowercase-value-exists (alist all)
  "Verify ALL has lowercase value for idx in ALIST."
  (cl-some (lambda (other)
              (and (= (cdr (assoc 'idx alist)) (cdr (assoc 'idx other)))
                   (not (cdr (assoc 'ucase other)))))
           (cl-remove-if-not (lambda (x) (listp x)) all)))

(defun aya--alist-get-proper-case-value (alist)
  "Get value from ALIST with proper case."
  (if (and aya-case-fold (cdr (assoc 'ucase alist)))
      (aya--upcase-first-char (cdr (assoc 'value alist)))
    (cdr (assoc 'value alist))))

(defun aya--upcase-first-char (str)
  "Set first char in STR to uppercase."
  (if (not (string= "" str))
      (concat (upcase (substring str 0 1)) (substring str 1))
    str))

(defun aya--maybe-downcase-first-char (str)
  "Set first char in STR to lowercase."
  (if (and aya-case-fold (not (string= "" str)))
      (concat (downcase (substring str 0 1)) (substring str 1))
    str))

(defun aya--first-char-is-upcase (str)
  "Check if first char in STR is uppercase."
  (let ((char (string-to-char str)))
    (= (upcase char) char)))

(defun aya--set-current (snippet)
  "Wrap setq `aya-current' to SNIPPET.
Also append the new value of `aya-current' to `aya-history'."
  (let ((escaped-snippet (aya--escape-snippet snippet)))
   (setq aya-history (append aya-history `(,escaped-snippet)))
   (setq aya-current escaped-snippet)))

(defun aya--parse (str)
  "Parse STR."
  (let ((start 0)
        (mirror-idx 0)
        (mirror-tbl (make-hash-table :test 'equal))
        (regex (format
                "\\(?:`\\(?1:[^']+\\)'\\|%s\\(?1:\\(?:%s\\)+\\)\\)"
                aya-marker
                aya-field-regex))
        res)
    (while (string-match regex str start)
      (unless (= (match-beginning 0) start)
        (push (substring str start (match-beginning 0)) res))
      (let* ((mirror (match-string 1 str))
             (cased-mirror (aya--maybe-downcase-first-char mirror))
             (idx (gethash cased-mirror mirror-tbl))
             (ucase (aya--first-char-is-upcase mirror)))
        (unless idx
          (setq idx (cl-incf mirror-idx))
          (puthash cased-mirror idx mirror-tbl))
        (push (list (cons 'idx idx)
                    (cons 'value cased-mirror)
                    (cons 'ucase ucase)) res))
      (setq start (match-end 0)))
    (unless (= start (length str))
      (push (substring str start) res))
    (nreverse res)))

(defun aya--beginning-of-line ()
  "Return the beginning of the line.
If `aya-trim-one-line' is non-nil return the position of the first
non-space character.  Otherwise just return the position of the first
character in the current line."
  (if aya-trim-one-line
      (save-excursion
        (move-beginning-of-line nil)
        (re-search-forward "[\t ]*")
        (point))
    (line-beginning-position)))

;;;###autoload
(defun aya-create (&optional beg end)
  "Create a snippet from the text between BEG and END.
When the bounds are not given, use either the current region or line.

Remove `aya-marker' prefixes, write the corresponding snippet to
`aya-current', with words prefixed by `aya-marker' as fields, and
mirrors properly set up."
  (interactive)
  (let* ((beg (cond (beg)
                    ((region-active-p)
                     (region-beginning))
                    (t
                     (aya--beginning-of-line))))
         (end (cond (end)
                    ((region-active-p)
                     (region-end))
                    (t
                     (line-end-position))))
         (str (buffer-substring-no-properties beg end))
         (case-fold-search nil)
         (res (aya--parse str)))
    (when (cl-some #'listp res)
      (delete-region beg end)
      (insert (mapconcat
               (lambda (x) (if (listp x) (aya--alist-get-proper-case-value x) x))
               res ""))
      (aya--set-current
            (aya--maybe-append-newline
             (mapconcat
              (lambda (x) (if (listp x) (aya--alist-create-value-specifier x res) x))
              res "")))
      ;; try some other useful action if it's defined for current buffer
      (and (functionp aya-default-function)
           (funcall aya-default-function)))))

(defun aya--set-region-exit-point (snippet &optional field-index)
  "Set SNIPPET region wrapping/exit point, using FIELD-INDEX (default 1)."
    (let* ((field-index (or field-index 1))
           (field-regex (format "\\$\\({?\\)%i" field-index))
           (snippet-with-region-exit-point (replace-regexp-in-string
                                            field-regex
                                            "`(yas-selected-text)`$\\10"
                                            snippet)))
      snippet-with-region-exit-point))

;;;###autoload
(defun aya-create-one-line ()
  "A simplistic `aya-create' to create only one mirror.
You can still have as many instances of this mirror as you want.
It's less flexible than `aya-create', but faster.
It uses a different marker, which is `aya-marker-one-line'.
You can use it to quickly generate one-liners such as
menu.add_item(spamspamspam, \"spamspamspam\")"
  (interactive)
  (when aya-marker-one-line
    (let* ((beg (aya--beginning-of-line))
           (end (line-end-position))
           (line (buffer-substring-no-properties beg (point)))
           (re (regexp-quote aya-marker-one-line)))
        (when (and (not (string-match (regexp-quote aya-marker) line))
                   (string-match re line))
          (setq line
                (aya--maybe-append-newline
                  (concat
                   (replace-regexp-in-string re "$1" line)
                   (if (= (point) end) "" "$1")
                   (buffer-substring-no-properties (point) end))))
          (delete-region beg end)
          (when aya-create-with-newline (delete-char 1))
          (setq aya-current line)
          (yas-minor-mode 1)
          (yas-expand-snippet line)))))

;;;###autoload
(defun aya-expand (&optional prefix)
  "Insert the last yasnippet created by `aya-create'.

Optionally use PREFIX to set any field as `$0' for wrapping the
current region. (`$0' also sets the exit point after `aya-expand'
when there's no active region.) When PREFIX is it defaults to 1.

For example let's say the second field in a snippet is where you
want to wrap the currently selected region.

Use `M-2' \\[aya-expand].

If we use this text as a snippet:

```~lang
~code
````'

and assume the selected region as:

`let somePrettyComplexCode = \"Hello World!\"'

we'd do `M-2' \\[aya-expand] which allows us to
fill in `~lang' as `javascript' and wraps our
code into the code-fences like this.

```javascript
let somePrettyComplexCode = \"Hello World!\"
```

Hint: if you view the current snippet(s) in history with
`aya-expand-from-history'. The snippets are shown with their
fields numbered.

In our example the snippet looks like like this:

\\`\\`\\`$1⤶$2⤶\\`\\`\\`⤶"
;; FIXME: broken when expanding a field which uses `${n}' form

  (interactive "p")
  (unless (not (string= "" aya-current))
    (user-error "There is no aya-current snippet available"))
  (unless yas-global-mode (yas-global-mode))
  (if (region-active-p)
      (yas-expand-snippet (aya--set-region-exit-point aya-current prefix))
    (if (= prefix 1)
        (yas-expand-snippet aya-current)
      (yas-expand-snippet
       (replace-regexp-in-string
        (format "\\$%i" prefix) "$0" aya-current)))))


;;;###autoload
(defun aya-expand-from-history (&optional prefix)
  "Select and insert a yasnippet from the `aya-history'.
The selected snippet will become `aya-current'
and will be used for consecutive `aya-expand' commands.

When PREFIX is given, the corresponding field number is
modified to make it the current point after expansion."
  (interactive "p")
  (unless (> (length aya-history) 0)
             (user-error "Nothing in aya-history to expand"))
  (setq aya-current (completing-read "Select aya-snippet: " aya-history))
  (aya-expand prefix))

;;;###autoload
(defun aya-delete-from-history ()
  "Select and delete one or more snippets from `aya-history'.
If the selected snippet is also `aya-current', it will be replaced
by the next snippet in history, or blank if no other history items
are available."
  (interactive)
  (unless (> (length aya-history) 0)
    (user-error "Nothing in aya-history to delete"))
  (let* ((snippets (completing-read-multiple
                    "Select aya-snippet(s) to delete: "
                    aya-history))
         (confirmation (y-or-n-p
                        (format "Delete %i snippets, confirm?"
                                (length snippets))))
         (history (seq-remove
                   (lambda (snippet) (member snippet snippets))
                   aya-history))
         (current (if (member aya-current history)
                      aya-current
                    (or (nth (- (length history) 1) history) ""))))
    (when confirmation
       (setq aya-history history
             aya-current current))))


(defun aya-clear-history ()
  "Clear aya-history."
  (interactive)
  (setq aya-current nil
        aya-history '("")))

(defun aya--history-index-of (snippet)
  "Get the zero based index of SNIPPET from `aya-history'."
  (let* ((indexed
           (let ((i  -1))
            (mapcar (lambda (item)
                      `(,item . ,(cl-incf i)))
                    aya-history)))
         (index (assoc snippet indexed)))
    (cdr index)))

(defun aya--history-snippet-of (index)
  "Get the snippet of INDEX from `aya-history'."
  (let* ((indexed
           (let ((i  -1))
            (mapcar (lambda (item)
                      `(,(cl-incf i) . ,item))
                    aya-history)))
         (snippet (assoc index indexed)))
    (cdr snippet)))

(defun aya-next-in-history ()
  "Set `aya-current' to the next item in history.
Wraps at the end of history."
  (interactive)
  (when (= 0 (length aya-history))
    (user-error "Nothing in aya-history"))
  (when  (=  1  (length aya-history))
    (user-error "Nothing else in aya-history"))
  (let* ((current aya-current)
         (index (aya--history-index-of aya-current))
         (next-index (if (= index (- (length aya-history) 1))
                         0
                       (+ index 1)))
         (snippet (aya--history-snippet-of next-index)))
        (setq aya-current snippet)
        (message "aya-current:\n%s" snippet)))

(defun aya-previous-in-history ()
  "Set `aya-current' to the previous item in history.
Wraps around at start of history."
  (interactive)
  (when (= 0 (length aya-history))
    (user-error "Nothing in aya-history"))
  (when  (=  1  (length aya-history))
    (user-error "Nothing else in aya-history"))
  (let* ((index (aya--history-index-of aya-current))
         (previous-index (if (= index 0)
                             (- (length aya-history) 1)
                           (- index 1)))
         (snippet (aya--history-snippet-of previous-index)))
        (setq aya-current snippet)
        (message "aya-current:\n%s" snippet)))

;; here's a use-case for this one:
;;
;; # -*- mode: snippet -*-
;; # name: println
;; # condition: (> aya-invocation-point 10)
;; # key: p
;; # --
;; System.out.println($0);
;;
;; # -*- mode: snippet -*-
;; # name: package
;; # condition: (< aya-invocation-point 10)
;; # key: p
;; # --
;; `(insert (concat "package " (java-package-name (buffer-file-name)) ";\n"))`
;;
;; Both snippets share the same key "p" based on the `aya-invocation-point'.
(defvar aya-invocation-point nil
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
        ((progn
           (unless yas-global-mode
             (yas-global-mode 1))
           (yas-active-snippets))
         (yas-next-field-or-maybe-expand))
        ((ignore-errors
           (setq aya-invocation-point (point))
           (setq aya-invocation-buffer (current-buffer))
           (setq aya-tab-position (- (point) (line-beginning-position)))
           (yas-expand)))
        ((and (fboundp 'tiny-expand)
              (funcall 'tiny-expand)))
        (t
         (open-line 1))))

;;;###autoload
(defun aya-yank-snippet ()
  "Insert current snippet at point.
To save a snippet permanently, create an empty file and call this."
  (interactive)
  (unless (= 0 (buffer-size))
    (user-error "Must be called from an empty file"))
  (insert "# -*- mode: snippet -*-\n")
  (insert "# name: \n# key: \n# --\n")
  (insert aya-current))

;;;###autoload
(defun aya-yank-snippet-from-history ()
  "Insert snippet from history at point."
  (interactive)
  (unless (> (length aya-history) 0)
    (user-error "Nothing in aya-history to yank"))
  (let ((snippet (completing-read "Select aya-snippet: " aya-history)))
    (unless (= 0 (buffer-size))
      (user-error "Must be called from an empty file"))
    (insert "# -*- mode: snippet -*-\n")
    (insert "# name: \n# key: \n# --\n")
    (insert snippet)))


(defun aya-insert-snippet-function-extra (snippet name)
  "Insert the SNIPPET body based on NAME.
Also prompt user for YASnippet `key'."
  (let ((key (read-string "Snippet key: ")))
    (insert
     "# -*- mode: snippet -*-"
     "\n# contributor: " user-full-name
     "\n# name: " name
     "\n# key: " key
     "\n# --\n"
     snippet)
    t))

(defun aya-insert-snippet-function-default (snippet name)
  "Insert the SNIPPET body with NAME."
  (insert
   "# -*- mode: snippet -*-"
   "\n# contributor: " user-full-name
   "\n# name: " name
   "\n# key: "
   "\n# --\n"
   snippet)
  nil)

(defun aya--escape-snippet (snippet)
  "Escape special yasnippet chars in the SNIPPET."
  (cl-reduce
   (lambda (acc it)
     (replace-regexp-in-string (car it) (cdr it) acc))
   aya--escape-chars-alist
   :initial-value snippet))

(defvar aya-insert-snippet-function
  #'aya-insert-snippet-function-default
  "Set a function for inserting a snippet body.
The function must accept SNIPPET and NAME arguments.
When it returns non-nil, save and close the buffer after
inserting.")

(defun aya-persist-snippet-from-history (snippet name)
  "Persist a SNIPPET from history in file NAME."
  (interactive
    (unless (> (length aya-history) 0)
        (user-error "You don't have any snippets in history"))
    (list
      (completing-read "Select Snippet: " aya-history)
      (read-string "Snippet name: ")))
  (aya--persist snippet name))

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
       (user-error "You don't have an auto-snippet defined")
     (list
      (read-string "Snippet name: "))))
  (aya--persist aya-current name))

(defun aya--persist (snippet name)
  "Internal function to persist SNIPPET definition to a file NAME."
  (let ((default-directory
         (format "%s/%S" aya-persist-snippets-dir major-mode)))
    (unless (file-exists-p default-directory)
      (make-directory default-directory t))
    (if (file-exists-p name)
        (user-error
         "A snippet called \"%s\" already exists in \"%s\""
         name default-directory)
      (with-current-buffer (find-file-noselect name)
        (if (funcall aya-insert-snippet-function snippet name)
            (progn
              (save-buffer)
              (kill-buffer))
          (snippet-mode)
          (goto-char (point-min))
          (search-forward "key: ")
          (pop-to-buffer (current-buffer)))))))

;; Local Variables:
;; eval: (and (fboundp 'nameless-mode) (setq nameless-current-name "aya")(nameless-mode 1))
;; End:

(provide 'auto-yasnippet)

;;; auto-yasnippet.el ends here
