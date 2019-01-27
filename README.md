# Introduction

This emacs module implements a bridge to libvterm to display a terminal in a
emacs buffer.

## Warning

This is a **alpha-release**, so it will crash your emacs. If it does, please
report a bug!

# Installation

Clone the repository:

```
git clone https://github.com/akermu/emacs-libvterm.git
```

Run the build:

```
mkdir -p build
cd build
cmake ..
make
```

And add this to your `init.el`:

```
(add-to-list 'load-path "path/to/emacs-libvterm")
(require 'vterm)
```

If you want to have the module compiled, wrap the call to `require` as follows:

```
(add-to-list 'load-path "path/to/emacs-libvterm")
(let (vterm-install)
  (require 'vterm))
```

# Debugging and testing

If you have successfully build the module, you can test the module by executing
the following command in the `build` directory:

```
make run
```

# Usage

## `vterm`

Open a terminal in the current window.

## `vterm-other-window`

Open a terminal in another window.

# Customization

## `vterm-shell`

Shell to run in a new vterm. Defaults to `$SHELL`.

## Keybindings

If you want a key to be send to the terminal bind it to `vterm--self-insert`,
otherwise remove it from `vterm-mode-map`. By default vterm.el binds most of the
`C-<char>` and `M-<char>` keys, `<f1>` through `<f12>` and some special keys
like `<backspace>` and `<return>`. Sending a keyboard interrupt is by default
bound to `C-c C-c`.

## Colors

Set the `:foreground` and `:background` attributes of the following faces to a
color you like:

- vterm-color-default-fg
- vterm-color-default-bg
- vterm-color-black-fg
- vterm-color-black-bg
- vterm-color-red-fg
- vterm-color-green-bg
- vterm-color-green-fg
- vterm-color-yellow-bg
- vterm-color-blue-fg
- vterm-color-blue-bg
- vterm-color-magenta-fg
- vterm-color-magenta-bg
- vterm-color-cyan-fg
- vterm-color-cyan-bg
- vterm-color-white-fg
- vterm-color-white-bg
