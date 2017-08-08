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

# Limitations

    - No support for scrolling
