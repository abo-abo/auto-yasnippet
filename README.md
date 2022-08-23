# Ayah

Ayah (or Auto-yasnippet history) see below...

## Setup/Install

Install on Doom! Emacs.

```emacs
(package! ayah :recipe (:host github :repo "emacsfodder/ayah"))
```

## Configuration

In your Emacs init file bind keys to the `ayah` commands.

For example:

```emacs
(bind-key "C-c C-y SPC" #'ayah-expand-from-history)
(bind-key "C-c C-y d"   #'ayah-delete-from-history)
(bind-key "C-c C-a n"   #'ayah-next-in-history)
(bind-key "C-c C-a p"   #'ayah-previous-in-history)
(bind-key "C-c C-y s"   #'ayah-persist-snippet)
```

# Functions

## ayah-expand-from-history

Select and expand from snippets in `ayah-history`. The selected
snippet will become `aya-current`.

## ayah-delete-from-history

Select and delete a snippet from `ayah-history`. The next available
snippet will become `aya-current`. When there are no other snippets
available `aya-current` will be set to `""`.

## ayah-next-in-history & ayah-previous-in-history

Set `aya-current' to the next or previous in `ayah-history'.

## ayah-persist-snippet-from-history

Functionally equivalent to `aya-persist-snippet' in but using a snippet selected
from `ayah-history'
