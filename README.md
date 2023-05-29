# emcn.el
A nextcloud notes client for emacs.

## Install

```bash
git clone https://github.com/hugot/emcn.el.git ~/projects/emcn.el
```

```elisp
(add-to-list 'load-path "~/projects/emcn.el")
(require 'emcn)
```

## Usage
To name a few functionalities:

- `M-x emcn-sync` to sync local and server notes.
- `M-x emcn` to create a new note.
- `M-x emcn-open` to open a note.
- `M-x emcn-set-name` to set note name.

See all of emcn's `interactive` functions for more things it can do. Also take a
look at `M-x customize-group emcn` for available configuration options.
