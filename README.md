# Auto-YASnippet

This is a hybrid of
[keyboard macro](http://www.gnu.org/software/emacs/manual/html_node/emacs/Basic-Keyboard-Macro.html)
and [yasnippet](http://code.google.com/p/yasnippet/).  You create the
snippet on the go, usually to be used just in the one place.  It's
fast, because you're not leaving the current buffer, and all you do is
enter the code you'd enter anyway, just placing `~` where you'd like
yasnippet fields and mirrors to be.

## Functions

### aya-create

Removes "~" from current line or region (if mark is active)
yielding valid code.
The created snippet is recorded into `aya-current`.

### aya-expand

Expands whatever is currently in `aya-current`

### aya-open-line

Generic expansion function. It will either expand or move
to the next field depending on the context.

### aya-persist-snippet

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
with it's **key** trigger.

# Installation instructions

It's easiest/recommended to install from [MELPA](http://melpa.org/).
Here's a minimal MELPA configuration for your `~/.emacs`:

```cl
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
```

Afterwards, <kbd>M-x package-install RET auto-yasnippet RET</kbd> (you might
want to <kbd>M-x package-refresh-contents RET</kbd> beforehand if
you haven't done so recently).

You will also want to setup the key bindings. Here's what I recommend:

```cl
(global-set-key (kbd "H-w") #'aya-create)
(global-set-key (kbd "H-y") #'aya-expand)
```

I also like to bind this, instead of using <kbd>TAB</kbd> to expand yasnippets:

```cl
(global-set-key (kbd "C-o") #'aya-open-line)
```

# Usage examples

## JavaScript

```js
field~1 = document.getElementById("field~1");
```

Since this just one line,
just call `aya-create` (from anywhere on this line).
The `~` chars disappear, yielding valid code.

`aya-current` becomes:

```cl
"field$1 = document.getElementById(\"field$1\");"
```

Now by calling `aya-expand` multiple times, you get:

```js
field1 = document.getElementById("field1");
field2 = document.getElementById("field2");
field3 = document.getElementById("field3");
fieldFinal = document.getElementById("fieldFinal");
```

## Java

```java
class Light~On implements Runnable {
  public Light~On() {}
  public void run() {
    System.out.println("Turning ~on lights");
    light = ~true;
  }
}
```

This differs from the code that you wanted to write only by 4 `~` chars.
Since it's more than one line, select the region and call `aya-create`.
Again, the `~` chars disappear, yielding valid code.

`aya-current` becomes:

```cl
"class Light$1 implements Runnable {
  public Light$1() {}
  public void run() {
    System.out.println(\"Turning $2 lights\");
    light = $3;
  }
}"
```

Now by calling `aya-expand`, you can quickly fill in:

```java
class LightOff implements Runnable {
  public LightOff() {}
  public void run() {
    System.out.println("Turning off lights");
    light = false;
  }
}
```

## C++

```c++
const Point<3> curl(grad[~2][~1] - grad[~1][~2],
```

Select the region between the paren and the comma and call `aya-create`.
You can easily obtain the final code:

```c++
const Point<3> curl(grad[2][1] - grad[1][2],
                    grad[0][2] - grad[2][0],
                    grad[1][0] - grad[0][1]);
```

Note how annoying it would be to triple check that the indices match.
Now you just have to check for one line.

## JavaScript - `aya-one-line`:

`aya-one-line` works as a combination of `aya-create` and `aya-expand`
for one-line snippets. It's invoked by `aya-create` in case
there's no `aya-marker` (default `~`) on the line, but there's
`aya-marker-one-line` (default `$`). Or you can invoke it on its own.

```js
field$ = document.getElementById("");
```

call `aya-create` and the rest is as before:

```js
field1 = document.getElementById("field1");
field2 = document.getElementById("field2");
field3 = document.getElementById("field3");
fieldFinal = document.getElementById("fieldFinal");
```

## Generating comments

Here's a yasnippet that makes use of `aya-tab-position`. You need to call
`aya-open-line` if you want to use it.


    # -*- mode: snippet -*-
    # name: short comment
    # key: sc
    # --
    //———$1${1:$(make-string (- 47 aya-tab-position (length yas-text)) ?—)}$0

Comments generated with this will always end in same column position,
no matter from which indentation level they were invoked from.
