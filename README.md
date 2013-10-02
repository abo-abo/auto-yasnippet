# Main idea
  This is a hybrid of [keyboard macro](http://www.gnu.org/software/emacs/manual/html_node/emacs/Basic-Keyboard-Macro.html)
  and [yasnippet](http://code.google.com/p/yasnippet/).
  You create the snippet on the go, usually to be used just in the one place.
  It's fast, because you're not leaving the current buffer, and
  all you do is enter the code you'd enter anyway, just placing "~" where you'd
  like yasnippet fields and mirrors to be.
## Functions
### aya-create
    * removes "~" from current line or region(if mark is active), yielding valid code
    * the created snippet is recorded into `aya-current'
### aya-expand
    * expands whatever is currently in `aya-current'
# Setup
1. Download yasnippet from https://github.com/capitaomorte/yasnippet and set it up.
2. Put `auto-yasnippet.el' into your elisp folder.
3. In your .emacs file:

```Lisp
     (require 'auto-yasnippet)
     (global-set-key (kbd "H-w") 'aya-create)
     (global-set-key (kbd "H-y") 'aya-expand)
```

# Usage examples
## JavaScript
```JavaScript
     field~1 = document.getElementById("field~1");
     // Since this just one line,
     // just call `aya-create' (from anywhere on this line).
     // The ~ chars disappear, yielding valid code.
     // `aya-current' becomes:
     // "field$1 = document.getElementById(\"field$1\");"
     // Now by calling `expand-auto-snippet' multiple times, you get:

     field1 = document.getElementById("field1");
     field2 = document.getElementById("field2");
     field3 = document.getElementById("field3");
     fieldFinal = document.getElementById("fieldFinal");
```
## Java
```Java
     class Light~On implements Runnable {
       public Light~On() {}
       public void run() {
         System.out.println("Turning ~on lights");
         light = ~true;
       }
     }
     // This differs from the code that you wanted to write only by 4 ~ chars.
     // Since it's more than one line, select the region and call `aya-create'.
     // Again, the ~ chars disappear, yielding valid code.
     // `aya-current' becomes:
     // "class Light$1 implements Runnable {
     //   public Light$1() {}
     //   public void run() {
     //     System.out.println(\"Turning $2 lights\");
     //     light = $3;
     //   }
     // }"

     // Now by calling `expand-auto-snippet', you can quickly fill in:

     class LightOff implements Runnable {
       public LightOff() {}
       public void run() {
         System.out.println("Turning off lights");
         light = false;
       }
     }
```
## C++
```C++
    const Point<3> curl(grad[~2][~1] - grad[~1][~2],
```

Select the region between the paren and the comma and call `aya-create'.
You can easily obtain the final code:
```C++
    const Point<3> curl(grad[2][1] - grad[1][2],
                        grad[0][2] - grad[2][0],
                        grad[1][0] - grad[0][1]);
```
Note how annoying it would be to triple check that the indices match.
Now you just have to check for one line.

## JavaScript - `aya-one-line`:
```JavaScript
     // `aya-one-line' works as a combination of `aya-create' and `aya-expand'
     // for one-line snippets. It's invoked by `aya-create' in case
     // there's no `aya-marker' (default ~) on the line, but there's
     // `aya-marker-one-line' (default $). Or you can invoke it on its own.
     field$ = document.getElementById("");
     // call `aya-create' and the rest is as before:

     field1 = document.getElementById("field1");
     field2 = document.getElementById("field2");
     field3 = document.getElementById("field3");
     fieldFinal = document.getElementById("fieldFinal");
```
