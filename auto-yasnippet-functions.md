
# Auto-Snippet Functions

## aya-create

Removes "~" from current line or region (if mark is active) yielding
valid code. The created snippet is recorded into `aya-current` and
appended to `aya-history`.

## aya-expand

Expands whatever is currently in `aya-current`

## aya-expand-from-history

Select and expand from snippets in `aya-history`. The selected
snippet will become `aya-current`.

## aya-delete-from-history

Select and delete a snippet from `aya-history`. The next available
snippet will become `aya-current`. When there are no other snippets
available `aya-current` will be set to `""`.

## aya-next-in-history & aya-previous-in-history

Set `aya-current' to the next or previous in `aya-history'.

## aya-open-line

Generic expansion function. It will either expand or move
to the next field depending on the context.

## aya-persist-snippet

Save the current auto-snippet to a user snippets folder (this defaults to
`~/.emacs.d/snippets/`.)  The current `major-mode` name will be used
to determine the snippets sub-directory to store the snippet.  For
example when working in `js2-mode` the snippet will be saved to (by
default) `~/.emacs.d/snippets/js2-mode/`.

You will be prompted for the snippet **name**. The appropriate file will be opened but not saved,
with the point on the `key: ` parameter of the snippet. If you wish to proceed, fill in the key,
save the buffer and call <kbd>C-c C-l</kbd> (`yas-load-snippet-buffer`). Otherwise, simply kill the
buffer - there will be no side effects.

You can customize `aya-persist-snippets-dir` to use a different folder
for storing auto-snippets.

You will need to run `yas/reload-all` before using the new snippet
with its **key** trigger.

 - - -
## Functions

### aya--alist-create-value-specifier

Create yasnippet template specifier for value in `alist`.
Use `all` to ensure proper template is generated.

```lisp
(aya--alist-create-value-specifier (alist all))
```
<sup>function signature</sup>
- - -

### aya--alist-get-proper-case-value

Get value from `alist` with proper case.

```lisp
(aya--alist-get-proper-case-value (alist))
```
<sup>function signature</sup>
- - -

### aya--beginning-of-line

Return the beginning of the line.
If `aya-trim-one-line` is non-nil return the position of the first
non-space character.  Otherwise just return the position of the first
character in the current line.

```lisp
(aya--beginning-of-line)
```
<sup>function signature</sup>
- - -

### aya--escape-snippet

Escape special yasnippet chars in the `snippet`.

```lisp
(aya--escape-snippet (snippet))
```
<sup>function signature</sup>
- - -

### aya--first-char-is-upcase

Check if first char in `str` is uppercase.

```lisp
(aya--first-char-is-upcase (str))
```
<sup>function signature</sup>
- - -

### aya--history-index-of

Get the index of `snippet` from `aya-history`.

Return the zero-based index for use with `nth`.

```lisp
(aya--history-index-of (snippet))
```
<sup>function signature</sup>
- - -

### aya--history-snippet-of

Get the snippet of `index` from `aya-history`.

```lisp
(aya--history-snippet-of (index))
```
<sup>function signature</sup>
- - -

### aya--matching-lowercase-value-exists

Verify `all` has lowercase value for idx in `alist`.

```lisp
(aya--matching-lowercase-value-exists (alist all))
```
<sup>function signature</sup>
- - -

### aya--maybe-append-newline

Append newline to `str` if `aya-create-with-newline` is non-nil.

```lisp
(aya--maybe-append-newline (str))
```
<sup>function signature</sup>
- - -

### aya--maybe-downcase-first-char

Set first char in `str` to lowercase.

```lisp
(aya--maybe-downcase-first-char (str))
```
<sup>function signature</sup>
- - -

### aya--parse

Parse `str`.

```lisp
(aya--parse (str))
```
<sup>function signature</sup>
- - -

### aya--set-current

Wrap setq `aya-current` to `snippet`.
Also append the new value of `aya-current` to `aya-history`.

```lisp
(aya--set-current (snippet))
```
<sup>function signature</sup>
- - -

### aya--upcase-first-char

Set first char in `str` to uppercase.

```lisp
(aya--upcase-first-char (str))
```
<sup>function signature</sup>
- - -

### aya-create

Create a snippet from the text between `beg` and `end`.
When the bounds are not given, use either the current region or line.

Remove `aya-marker` prefixes, write the corresponding snippet to
`aya-current`, with words prefixed by `aya-marker` as fields, and
mirrors properly set up.

```lisp
(aya-create (&optional beg end))
```
<sup>function signature</sup>
- - -

### aya-delete-from-history

Select and delete a yasnippet from the `aya-history`.
The if the selected snippet is also `aya-current`,
it will be replaced by the next snippet in history,
or blank if no other history items are available.

```lisp
(aya-delete-from-history)
```
<sup>function signature</sup>
- - -

### aya-expand

Insert the last yasnippet created by `aya-create`.

Optionally use `prefix` to set field as $0 for region wrapping
or cursor position when expansion has ended.

For example say the second field in a snippet is where you want to
wrap the currently selected region or begin editing after expansion.

Use `M-2` `M-x aya-expand`.

If we use this text as a snippet:

```~lang
~code
`````

and assume the selected region as:

`let somePrettyComplexCode = "Hello World!"`

we'd do `M-2` `M-x aya-expand` which allows us to
fill in `~lang` as `javascript` and wraps our
code into the code-fences like this.

```javascript
let somePrettyComplexCode = "Hello World!"
```

Hint: if you view the current snippet(s) in history
with `M-x aya-expand-from-history` (or `M-x describe-variable `ret` aya-history`)
the snippets are shown with their fields.

In our example the snippet looks like like this:

\`\`\`$1⤶$2⤶\`\`\`⤶

```lisp
(aya-expand (&optional prefix))
```
<sup>function signature</sup>
- - -

### aya-expand-from-history

Select and insert a yasnippet from the `aya-history`.
The selected snippet will become `aya-current`
and will be used for consecutive `aya-expand` commands.

```lisp
(aya-expand-from-history (&optional prefix))
```
<sup>function signature</sup>
- - -

### aya-insert-snippet-function-default

Insert the snippet body based on `name`.

```lisp
(aya-insert-snippet-function-default (name))
```
<sup>function signature</sup>
- - -

### aya-insert-snippet-function-extra

Insert the snippet body based on `name`.

```lisp
(aya-insert-snippet-function-extra (name))
```
<sup>function signature</sup>
- - -

### aya-next-in-history

Set `aya-current` to the next item in history.
Wraps at the end of history.

```lisp
(aya-next-in-history)
```
<sup>function signature</sup>
- - -

### aya-open-line

Call `open-line`, unless there are abbrevs or snippets at point.
In that case expand them.  If there's a snippet expansion in progress,
move to the next field.  Call `open-line` if nothing else applies.

```lisp
(aya-open-line)
```
<sup>function signature</sup>
- - -

### aya-persist-snippet

Persist the current snippet in file `name`.

The full path is `aya-persist-snippets-dir`/`major-mode`/NAME.

Make sure to configure yasnippet to scan `aya-persist-snippets-dir`
for snippets.

Use `yas/reload-all` after defining a batch of snippets,
or `yas-load-snippet-buffer` for the current one.

Customizing `aya-insert-snippet-function` affects the behavior.

```lisp
(aya-persist-snippet (name))
```
<sup>function signature</sup>
- - -

### aya-previous-in-history

Set `aya-currnet` to the previous item in history.
Wraps around at start of history.

```lisp
(aya-previous-in-history)
```
<sup>function signature</sup>
- - -

### aya-yank-snippet

Insert current snippet at point.
To save a snippet permanently, create an empty file and call this.

```lisp
(aya-yank-snippet)
```
<sup>function signature</sup>
- - -

### aya-yank-snippet-from-history

Insert snippet from history at point.

```lisp
(aya-yank-snippet-from-history)
```
<sup>function signature</sup>
- - -
