# Main idea
  This is a hybrid of [keyboard macro](http://www.gnu.org/software/emacs/manual/html_node/emacs/Basic-Keyboard-Macro.html)
  and [yasnippet](http://code.google.com/p/yasnippet/).
  You create the snippet on the go, usually to be used just in the one place.
  It's fast, because you're not leaving the current buffer, and
  all you do is enter the code you'd enter anyway, just placing "~" where you'd
  like yasnippet fields and mirrors to be.
## Functions
### create-auto-yasnippet
    * removes "~" from current line or region(if mark is active), yielding valid code
    * the created snippet is recorded into `*current-auto-yasnippet-template*'
### expand-auto-yasnippet
    * expands whatever is currently in `*current-auto-yasnippet-template*'
# Setup
1. Download yasnippet http://code.google.com/p/yasnippet/ and set it up.
2. Put `auto-yasnippet.el' into your elisp folder.
3. In your .emacs file:
       (require 'auto-yasnippet)
       (global-set-key (kbd "H-w") 'create-auto-yasnippet)
       (global-set-key (kbd "H-y") 'expand-auto-yasnippet)
# Usage examples
## JavaScript
```JavaScript
     field~1 = document.getElementById("field~1");
     // Since this just one line, just call `create-auto-yasnippet' (from anywhere on this line).
     // The ~ chars disappear, yielding valid code.
     // `*current-auto-yasnippet-template*' becomes:
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
     // Since it's more than one line, select the region and call `create-auto-yasnippet'.
     // Again, the ~ chars disappear, yielding valid code.
     // `*current-auto-yasnippet-template* becomes:
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
   
  
  
