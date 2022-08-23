;;; ayah.el --- History extensions for auto-yasnippet -*- lexical-binding: t; -*-
;;
;; Author: Jason Milkins <jasonm23@gmail.com>
;;
;; URL: https://github.com/emacsfodder/ayah
;; Version: 0.1.0
;; Package-Requires: ((auto-yasnippet "0.14.0"))
;;
;; This file is not part of GNU Emacs
;;
;;; License:
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
;; # Ayah
;;
;; Ayah (or Auto-yasnippet history) see below...
;;
;; ## Setup/Install
;;
;; Install with straight.el.
;;
;; ```emacs
;; (straight-use-package) ;;  TODO
;; ```
;;
;; ### Install on Doom Emacs
;;
;; Doom Emacs ships with abo-abo/auto-snippet.  To use this fork add this to `package.el`:
;;
;; ```emacs
;; (unpin! auto-yasnippet)
;; (package! auto-yasnippet :recipe (:host github :repo "emacsfodder/auto-yasnippet"))
;; ```
;;
;; ## Configuration
;;
;; In your Emacs init file bind keys to the `aya` commands.
;;
;; For example:
;;
;; ```emacs
;; (bind-key "C-c C-y SPC" #'ayah-expand-from-history)
;; (bind-key "C-c C-y d"   #'ayah-delete-from-history)
;; (bind-key "C-c C-a n"   #'ayah-next-in-history)
;; (bind-key "C-c C-a p"   #'ayah-previous-in-history)
;; (bind-key "C-c C-y s"   #'ayah-persist-snippet)
;; ```
;;
;; # Functions
;;
;; ## ayah-expand-from-history
;;
;; Select and expand from snippets in `aya-history`. The selected
;; snippet will become `aya-current`.
;;
;; ## ayah-delete-from-history
;;
;; Select and delete a snippet from `aya-history`. The next available
;; snippet will become `aya-current`. When there are no other snippets
;; available `aya-current` will be set to `""`.
;;
;; ## ayah-next-in-history & aya-previous-in-history
;;
;; Set `aya-current' to the next or previous in `aya-history'.
;;
;; ## ayah-persist-snippet-from-history
;;
;; Functionally equivalent to `aya-persist-snippet' in but using a snippet selected
;; from `ayah-history'
;;
;;; Code:

(require 'auto-yasnippet)

(defvar ayah-history '()
  "A List of auto yasnippets created in this session.")

(defvar ayah--escape-chars-alist '(("`" . "\\\\`"))
  "An alist of chars that must be escaped for yasnippet.")

(defun ayah--set-current (snippet)
  "Wrap setq `aya-current' to SNIPPET.
Also append the new value of `aya-current' to `ayah-history'."
  (let ((escaped-snippet (ayah--escape-snippet snippet)))
   (setq ayah-history (append ayah-history `(,escaped-snippet)))
   (setq aya-current escaped-snippet)))

;;;###autoload
(defun ayah-expand-from-history (&optional prefix)
  "Select and insert a yasnippet from the `aya-history'.
The selected snippet will become `aya-current'
and will be used for consecutive `aya-expand' commands.

When PREFIX is given, the corresponding field is modified to
make it the current point after expansion."
  (interactive "p")
  (if (> (length ayah-history) 0)
      (progn
        (setq aya-current (completing-read "Select aya-snippet: " ayah-history))
        (aya-expand prefix))
    (user-error "Nothing in ayah-history to expand")))

;;;###autoload
(defun ayah-delete-from-history ()
  "Select and delete one or more snippets from `ayah-history'.
If the selected snippet is also `aya-current', it will be replaced
by the next snippet in history, or blank if no other history items
are available."
  (interactive)
  (if (> (length ayah-history) 0)
    (let ((snippets (completing-read-multiple
                     "Select aya-snippet(s) to delete: "
                      ayah-history)))
      (let ((confirmation (y-or-n-p
                           (format "Delete %i snippets, confirm?"
                                   (length snippets)))))
        (let* ((history (seq-remove
                         (lambda (snippet) (member snippet snippets))
                         ayah-history))
               (current (if (member aya-current history)
                            aya-current
                          (or (nth (- (length history) 1) history) ""))))
          (when confirmation
             (setq ayah-history history
                   aya-current current)))))
   (user-error "Nothing in aya-history to delete")))

(defun ayah--history-index-of (snippet)
  "Get the zero based index of SNIPPET from `aya-history'."
  (let* ((indexed
           (let ((i  -1))
            (mapcar (lambda (item)
                      `(,item . ,(cl-incf i)))
                    ayah-history)))
         (index (assoc snippet indexed)))
    (cdr index)))

(defun ayah--history-snippet-of (index)
  "Get the snippet of INDEX from `ayah-history'."
  (let* ((indexed
           (let ((i  -1))
            (mapcar (lambda (item)
                      `(,(cl-incf i) . ,item))
                    ayah-history)))
         (snippet (assoc index indexed)))
    (cdr snippet)))

(defun ayah-next-in-history ()
  "Set `aya-current' to the next item in history.
Wraps at the end of history."
  (interactive)
  (when (= 0 (length ayah-history))
    (user-error "Nothing in ayah-history"))
  (when  (=  1  (length ayah-history))
    (user-error "Nothing else in ayah-history"))
  (let* ((current aya-current)
         (index (ayah--history-index-of aya-current))
         (next-index (if (= index (- (length ayah-history) 1))
                         0
                       (+ index 1)))
         (snippet (ayah--history-snippet-of next-index)))
        (setq aya-current snippet)
        (message "aya-current:\n%s" snippet)))

(defun ayah-previous-in-history ()
  "Set `aya-current' to the previous item in history.
Wraps around at start of history."
  (interactive)
  (when (= 0 (length ayah-history))
    (user-error "Nothing in ayah-history"))
  (when  (=  1  (length ayah-history))
    (user-error "Nothing else in ayah-history"))
  (let* ((index (ayah--history-index-of aya-current))
         (previous-index (if (= index 0)
                             (- (length ayah-history) 1)
                           (- index 1)))
         (snippet (aya--history-snippet-of previous-index)))
        (setq aya-current snippet)
        (message "aya-current:\n%s" snippet)))

;;;###autoload
(defun ayah-yank-snippet-from-history ()
  "Insert snippet from history at point."
  (interactive
    (if (> (length ayah-history) 0)
        (progn
          (let ((snippet (completing-read "Select aya-snippet: " ayah-history)))
            (unless (= 0 (buffer-size))
              (user-error "Must be called from an empty file"))
            (insert "# -*- mode: snippet -*-\n")
            (insert "# name: \n# key: \n# --\n")
            (insert snippet)))
      (user-error "Nothing in ayah-history to yank"))))

(defun ayah--escape-snippet (snippet)
  "Escape special yasnippet chars in the SNIPPET."
  (cl-reduce
   (lambda (acc it)
     (replace-regexp-in-string (car it) (cdr it) acc))
   ayah--escape-chars-alist
   :initial-value snippet))

(defun ayah-persist-snippet-from-history (snippet name)
  "Persist a SNIPPET from history in file NAME."
  (interactive
    (if (= (length ayah-history) 0)
        (user-error "Aborting: You don't have a current auto-snippet defined")
      (list
       (completing-read "Select Snippet: " ayah-history)
       (read-string "Snippet name: "))))
  (ayah--persist snippet name))

(defun ayah--persist (snippet name)
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

(provide 'ayah)

;;; ayah.el ends here
