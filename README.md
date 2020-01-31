[![MELPA](https://melpa.org/packages/vterm-badge.svg)](https://melpa.org/#/vterm)

# Introduction

This emacs module implements a bridge to
[libvterm](https://github.com/neovim/libvterm) to display a terminal in an Emacs
buffer.

## Warning

This is an **alpha release**, so it will crash your Emacs. If it does, please
[report a bug](https://github.com/akermu/emacs-libvterm/issues/new)!

# Installation

## Manual

Clone the repository:

```sh
git clone https://github.com/akermu/emacs-libvterm.git
```

Before installing emacs-libvterm, you need to make sure you have
installed
 1. GNU Emacs (>= 25.1) with [module
    support](https://www.gnu.org/software/emacs/manual/html_node/elisp/Dynamic-Modules.html).
    You can check that, by verifying that `module-file-suffix` is not `nil`.
 2. cmake (>= 3.11)
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

## vterm and Ubuntu

Using `vterm` on Ubuntu requires additional steps. The latest LTS version
(18.04) ships with a version of CMake that is too old for `vterm` and GNU
Emacs is not compiled with support for dynamical module loading.

It is possible to install GNU Emacs with module support from Kevin Kelley's PPA
In case Emacs is already on the system, you need to purge it before proceeding
with the following commands.
```sh
sudo add-apt-repository ppa:kelleyk/emacs
sudo apt update
sudo apt-get install emacs26
```

A way to install a recent version of CMake (>= 3.11) is with linuxbrew.
```sh
brew install cmake
```

In some cases, `/bin/sh` needs to be relinked to `/bin/bash` for the compilation
to work (see,
https://github.com/akermu/emacs-libvterm/issues/216#issuecomment-575934593).

Pull requests are welcome to improve support for Ubuntu (e.g., simplyfing the
installation).

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

## `vterm-clear-scrollback`

`vterm-clear-scrollback` does exactly what the name suggests: it clears the
current buffer from the data that it is not currently visible.
`vterm-clear-scrollback` is bound to `C-c C-l`. This function is typically used
with the `clear` function provided by the shell to clear both screen and
scrollback. In order to achieve this behavior, you need to add a new shell alias.

For `bash` or `zsh`, put this in your `.zshrc` or `.bashrc`
```bash
function vterm_printf(){
    if [ -n "$TMUX" ]; then
        # tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}
```

For `fish`   put this in your `~/.config/fish/config.fish`:
```bash
function vterm_printf;
    if [ -n "$TMUX" ]
        # tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
    else if string match -q -- "screen*" "$TERM"
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$argv"
    else
        printf "\e]%s\e\\" "$argv"
    end
end
```

For `zsh`, put this in your `.zshrc`:
```zsh

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi
```
For `bash`, put this in your `.bashrc`:
```bash
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    function clear(){
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
    }
fi
```

These aliases take advantage of the fact that `vterm` can execute `elisp`
commands, as explained below.

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

`vterm` supports _directory tracking_. If this feature is enabled, the default
directory in Emacs and the current working directory in `vterm` are synced. As a
result, interactive functions that ask for a path or a file (e.g., `dired` or
`find-file`) will do so starting from the current location.

Directory tracking requires some configuration, as the shell has to be instructed
to share the relevant information with Emacs.

For `zsh`, put this at the end of your `.zshrc`:

```zsh

vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";
}
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
```

For `bash`, put this at the end of your `.bashrc`:

```bash
vterm_prompt_end(){
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}
PS1=$PS1'\[$(vterm_prompt_end)\]'
```

For `fish`, put this in your `~/.config/fish/config.fish`:

```fish
function fish_vterm_prompt_end;
    vterm_printf '51;A'(whoami)'@'(hostname)':'(pwd)
end
function track_directories --on-event fish_prompt; fish_vterm_prompt_end; end
```

Directory tracking works on remote servers too. In case the hostname of your
remote machine does not match the actual hostname needed to connect to that
server, change `$(hostname)` with the correct one.

## Message passing

`vterm` can read and execute commands. At the moment, a command is
passed by providing a specific escape sequence. For example, to evaluate
``` elisp
(message "Hello!")
```
use
``` sh
printf "\e]51;Emessage \"Hello\!\"\e\\"

# or
vterm_printf "51;Emessage \"Hello\!\""
```

The commands that are understood are defined in the setting `vterm-eval-cmds`.

As `split-string-and-unquote` is used the parse the passed string, double quotes
and backslashes need to be escaped via backslash. For instance, bash can replace
strings internally.

```sh
vterm_cmd() {
    if [ -n "$TMUX" ]; then
        # tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]51;E"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]51;E"
    else
        printf "\e]51;E"
    fi

    printf "\e]51;E"
    local r
    while [[ $# -gt 0 ]]; do
        r="${1//\\/\\\\}"
        r="${r//\"/\\\"}"
        printf '"%s" ' "$r"
        shift
    done
    if [ -n "$TMUX" ]; then
        # tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\007\e\\"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\007\e\\"
    else
        printf "\e\\"
    fi
}
```

However if you are using dash and need a pure POSIX implementation:

```sh
vterm_cmd() {
    if [ -n "$TMUX" ]; then
        # tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]51;E"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]51;E"
    else
        printf "\e]51;E"
    fi
    while [ $# -gt 0 ]; do
        printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')"
        shift
    done
    if [ -n "$TMUX" ]; then
        # tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\007\e\\"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\007\e\\"
    else
        printf "\e\\"
    fi
}
```

Now we can write shell functions to call the ones defined in `vterm-eval-cmds`.

```sh
find_file() {
    vterm_cmd find-file "$(realpath "$@")"
}

say() {
    vterm_cmd message "%s" "$*"
}
```

This can be used inside `vterm` as

```sh
find_file name_of_file_in_local_directory
```

As an example, say you like having files opened below the current window. You
could add the command to do it on the lisp side like so:

``` elisp
(push (list "find-file-below"
            (lambda (path)
              (if-let* ((buf (find-file-noselect path))
                        (window (display-buffer-below-selected buf nil)))
                  (select-window window)
                (message "Failed to open file: %s" path))))
      vterm-eval-cmds)
```

Then add the command in your `.bashrc` file.

```sh
open_file_below() {
    vterm_cmd find-file-below "$(realpath "$@")"
}
```

Then you can open any file from inside your shell.

```sh
open_file_below ~/Documents
```

## Related packages

- [vterm-toggle](https://github.com/jixiuf/vterm-toggle): Toggles between a vterm and the current buffer
- [multi-libvterm](https://github.com/suonlight/multi-libvterm): Multiterm for emacs-libvterm
