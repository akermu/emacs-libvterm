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

And add this to your `init.el`

```
(add-to-list 'load-path "path/to/emacs-libvterm")
```

# Usage

```
M-x vterm-create
```

# Customization

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
- Hijacks SIGUSR1
- Mouse support is non-existing
