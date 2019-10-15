[![MELPA](https://melpa.org/packages/vterm-badge.svg)](https://melpa.org/#/vterm)

# Introduction

This emacs module implements a bridge to
[libvterm](https://github.com/neovim/libvterm) to display a terminal in an emacs
buffer.

## Warning

This is an **alpha release**, so it will crash your emacs. If it does, please
[report a bug](https://github.com/akermu/emacs-libvterm/issues/new)!

# Installation

## Manual

Clone the repository:

```sh
git clone https://github.com/akermu/emacs-libvterm.git
```

Before installing emacs-libvterm, you need to make sure you have
installed
 1. Emacs with [module
    support](https://www.gnu.org/software/emacs/manual/html_node/elisp/Dynamic-Modules.html).
    You can check that, by verifying that `module-file-suffix` is not `nil`.
 2. cmake (>=3.11)
 3. libtool-bin (related issues:
    [#66](https://github.com/akermu/emacs-libvterm/issues/66)
    [#85](https://github.com/akermu/emacs-libvterm/issues/85#issuecomment-491845136))
 4. If you compile vterm with `-DUSE_SYSTEM_LIBVTERM` make sure you have the
    library from https://github.com/neovim/libvterm

Run the build:

``` sh
mkdir -p build
cd build
cmake ..
make
```

And add this to your `init.el`:

``` elisp
(add-to-list 'load-path "path/to/emacs-libvterm")
(require 'vterm)
```

Or, with `use-package`:

```elisp
(use-package vterm
  :load-path  "path/to/emacs-libvterm/"
)
```

## From MELPA

`vterm` is available on [MELPA](https://melpa.org/), and it can be installed as
a normal package. If the requirements are satisfied (mainly, Emacs was built
with support for modules), `vterm` will take care of the compilation of all its
components.

`vterm` can be install with MELPA with `use-package` by adding the following
lines to your `init.el`:

```elisp
(use-package vterm
    :ensure t
)
```

# Debugging and testing

If you have successfully built the module, you can test it by executing the
following command in the `build` directory:

```sh
make run
```

# Usage

## `vterm`

Open a terminal in the current window.

## `vterm-other-window`

Open a terminal in another window.

## `vterm-copy-mode`

When you enable `vterm-copy-mode`, the terminal buffer behaves like a normal
`read-only` text buffer: you can search, copy text, etc. The default keybinding
to toggle `vterm-copy-mode` is `C-c C-t`. When a region is selected, it is
possible to copy the text and leave `vterm-copy-mode` with the enter key.

# Customization

## `vterm-shell`

Shell to run in a new vterm. It defaults to `$SHELL`.

## `vterm-term-environment-variable`

Value for the `TERM` environment variable. It defaults to `xterm-256color`. If
[eterm-256color](https://github.com/dieggsy/eterm-256color) is installed,
setting `vterm-term-environment-variable` to `eterm-color` improves the
rendering of colors in some systems.

## Keybindings

If you want a key to be sent to the terminal, bind it to `vterm--self-insert`,
or remove it from `vterm-mode-map`. By default, `vterm.el` binds most of the
`C-<char>` and `M-<char>` keys, `<f1>` through `<f12>` and some special keys
like `<backspace>` and `<return>`. Sending a keyboard interrupt is bound to `C-c
C-c`.

## Colors

Set the `:foreground` and `:background` attributes of the following faces to a
color you like. The `:foreground` is ansi color 0-7, the `:background` attribute
is ansi color 8-15.

- vterm-color-default
- vterm-color-black
- vterm-color-red
- vterm-color-green
- vterm-color-yellow
- vterm-color-blue
- vterm-color-magenta
- vterm-color-cyan
- vterm-color-white

## Directory tracking

For `zsh` put this in your `.zshrc`:

```zsh
function chpwd() {
    print -Pn "\e]51;A$(pwd)\e\\";
}
```

For bash there's no real change directory hook, so you have to rewrite the `cd`
command (please have a look at the answers
[here](https://unix.stackexchange.com/q/170279)):

```bash
cd() {
  builtin cd "$@" || return
  [ "$OLDPWD" = "$PWD" ] || echo -e "\e]51;A$(pwd)\e\\"
}
```

## Remote directory tracking

Put this in your *remote* `.zshrc`:

```zsh
function chpwd() {
    print -Pn "\e]51;A$(whoami)@$(hostname):$(pwd)\e\\"
}
```

## Related packages

- [vterm-toggle](https://github.com/jixiuf/vterm-toggle): Toggles between a vterm and the current buffer
- [multi-libvterm](https://github.com/suonlight/multi-libvterm): Multiterm for emacs-libvterm
