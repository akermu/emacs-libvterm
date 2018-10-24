# Introduction

This emacs module implements a bridge to libvterm to display a terminal in a
emacs buffer.

## Warning

This is a **alpha-release**, so it will crash your emacs. If it does, please
report a bug!

# Installation

```
git clone https://github.com/akermu/emacs-libvterm.git
```

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

## `vterm-keymap-exceptions`

List of keys, which should be processed by emacs and not by the terminal.

## Colors

Set the `:foreground` and `:background` attributes of the following faces to a
color you like:

- vterm
- vterm-color-black
- vterm-color-red
- vterm-color-green
- vterm-color-yellow
- vterm-color-blue
- vterm-color-magenta
- vterm-color-cyan
- vterm-color-white

# Limitations

- No support for scrolling (But you can use tmux/screen to emulate scrolling) 
- Mouse support is non-existing
