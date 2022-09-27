[![Auto-YASnippet Tests](https://github.com/abo-abo/auto-yasnippet/actions/workflows/test.yml/badge.svg?branch=master)](https://github.com/abo-abo/auto-yasnippet/actions/workflows/test.yml)

# Auto-YASnippet 1.0.0

Auto-YASnippet is a hybrid of [keyboard
macros](http://www.gnu.org/software/emacs/manual/html_node/emacs/Basic-Keyboard-Macro.html)
and [YASnippet](https://github.com/joaotavora/yasnippet). You create the snippet
on the go and it'll be ready to use immediately. Because you're not leaving the
current buffer the workflow is very fast.

All you do is enter the code you'd enter anyway but placing `~` chars where you`d
like YASnippet fields and mirrors to be.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Auto-YASnippet 1.0.0](#auto-yasnippet-100)
- [Setup/Install](#setupinstall)
- [Configuration](#configuration)
- [Examples](#examples)
  - [Multiple placeholders](#multiple-placeholders)
  - [Mixed case templates](#mixed-case-templates)
  - [Expanding around a region](#expanding-around-a-region)
- [Functions](#functions)
  - [aya-create](#aya-create)
  - [aya-expand](#aya-expand)
  - [aya-expand-from-history](#aya-expand-from-history)
  - [aya-delete-from-history](#aya-delete-from-history)
  - [aya-next-in-history & aya-previous-in-history](#aya-next-in-history--aya-previous-in-history)
  - [aya-open-line](#aya-open-line)
  - [aya-persist-snippet](#aya-persist-snippet)
  - [aya-persist-snippet-from-history](#aya-persist-snippet-from-history)

<!-- markdown-toc end -->

## Setup/Install

It's easiest/recommended to install from [MELPA](http://melpa.org/).
Here's a minimal MELPA configuration for your `~/.emacs`:

```lisp
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
```

Afterwards, <kbd>M-x</kbd> `package-install RET auto-yasnippet` <kbd>RET</kbd> (you might
want to <kbd>M-x</kbd> `package-refresh-contents` <kbd>RET</kbd> beforehand if
you haven't done so recently).

## Configuration

In your Emacs init file set keys for the `aya` commands.

For example:

```lisp
(global-set-key (kbd "C-c C-y w")   #'aya-create)
(global-set-key (kbd "C-c C-y TAB") #'aya-expand)
(global-set-key (kbd "C-c C-y SPC") #'aya-expand-from-history)
(global-set-key (kbd "C-c C-y d")   #'aya-delete-from-history)
(global-set-key (kbd "C-c C-y c")   #'aya-clear-history)
(global-set-key (kbd "C-c C-y n")   #'aya-next-in-history)
(global-set-key (kbd "C-c C-y p")   #'aya-previous-in-history)
(global-set-key (kbd "C-c C-y s")   #'aya-persist-snippet)
(global-set-key (kbd "C-c C-y o")   #'aya-open-line)
```

## Examples

If we need to write some repetitive code in an expression:

```c
count_of_red = get_total("red");
count_of_blue = get_total("blue");
count_of_green = get_total("green");
```

We can write a template, using `~` to represent text we want to replace:

```c
count_of_~red = get_total("~red");
```

With the cursor on this line, or with selected text, call <kbd>M-x</kbd> `aya-create` <kbd>RET</kbd>.
An auto-snippet is created and the text is converted to remove the `~` marker:

```c
count_of_red = get_total("red");
```

Now we can call `aya-expand` and we can insert text at each marker in the
template, note, because both words are the same, we just type it one.

Yasnippet controls all the interaction while expanding so refer to the yasnippet
docs. Basic interaction is to enter text at a marker and `TAB` to the next one.

```c
count_of_[CURSOR] = get_total("");
```

Say we enter `blue` once at `count_of_`` the result will be.

```c
count_of_blue = get_total("blue");
```

###  Multiple placeholders

You can replace multiple values in a template, just like normal
yasnippet.

In this example, our template has multiple lines, so we need to
select the relevant lines before calling `aya-create`

```java
~FooType get~Foo() {
    // Get the ~foo attribute on this.
    return this.~foo;
}
```

We fill in two placeholders in this example (the 2nd and 4th are the same as
the 3rd). Yasnippet places us at 1st, entering: Type `TAB` (yasnippet puts
us at 3rd) entering: bar `TAB` will expand to:

```java
Type getBar() {
  // Get the bar attribute on this.
  return this.foo;
}
```

### Mixed case templates

You can create mixed case templates setting `aya-case-fold` to `t`. This will result
in templates where variables that start with a character of a different case will be
treated as the same variable. The case of the first character will be preserved in the
resulting snippet.

Using the earlier example with a slight twist:

```c
count_of_~red = get_total("~Red");
```

Then calling `aya-create`, then `aya-expand`, and finally typing `blue`, the result
would be:

```c
count_of_blue = get_total("Blue");
```

Notice that `blue` was placed in both locations with proper casing.

### Expanding around a region

If you create an auto-yasnippet with one field, it's value will be filled in
from the current region. For example if we create a new snippet:

```swift
print("\(~thing)")
```

Select text:

```swift
myVar + 10
```

`aya-expand` gives us...

```swift
print("\(myVar + 10)")
```

You can also use the YASnippet built in marker `$0` in the point where you
want to finish expanding the snippet. YASnippet allows `$0` to be the region,
by setting `yas-wrap-around-region` to `t`.

# Functions

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

Set `aya-current` to the next or previous in `aya-history`.

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

## aya-persist-snippet-from-history

Functionally equivalent to `aya-persist-snippet` but using a snippet selected
from `aya-history`
