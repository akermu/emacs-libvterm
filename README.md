[![MELPA](https://melpa.org/packages/vterm-badge.svg)](https://melpa.org/#/vterm)

# Introduction

Emacs-libvterm (_vterm_) is fully-fledged terminal emulator inside GNU Emacs
based on [libvterm](https://github.com/neovim/libvterm), a C library. As a
result of using compiled code (instead of elisp), emacs-libvterm is fully
capable, fast, and it can seamlessly handle large outputs.

## Warning

This package is in active development and, while being stable enough to be used
as a daily-driver, it is currently in **alpha** stage. This means that
occasionally the public interface will change (for example names of options or
functions). A list of recent breaking changes is in
[appendix](#breaking-changes). Moreover, emacs-libvterm deals directly with some
low-level operations, hence, bugs can lead to segmentation faults and crashes.
If that happens, please [report the
problem](https://github.com/akermu/emacs-libvterm/issues/new).

## Given that eshell, shell, and (ansi-)term are Emacs built-in, why should I use vterm?

The short answer is: unparalleled performance and compatibility with standard
command-line tools.

For the long answer, let us discuss the differences between `eshell`, `shell`,
`term` and `vterm`:
- `eshell`: it is a shell completely implemented in Emacs Lisp. It is
  well-integrated in Emacs and it runs on Windows. It does not support command line
  tools that require terminal manipulation capabilities (e.g., `ncdu`, `nmtui`,
  ...).
- `shell`: it interfaces with a standard shell (e.g., `bash`). It reads an input
  from Emacs, sends it to the shell, and reports back the output from the shell.
  As such, like `eshell`, it does not support interactive commands, especially
  those that directly handle how the output should be displayed (e.g., `htop`).
- `term`: it is a terminal emulator written in elisp. `term` runs a shell
  (similarly to other terminal emulators like Gnome Terminal) and programs can
  directly manipulate the output using escape codes. Hence, many interactive
  applications (like the one aforementioned) work with `term`. However, `term`
  and `ansi-term` do not implement all the escapes codes needed, so some
  programs do not work properly. Moreover, `term` has inferior performance
  compared to standalone terminals, especially with large bursts of output.
- `vterm`: like `term` it is a terminal emulator. Unlike `term`, the core of
  `vterm` is an external library written in C, `libvterm`. For this reason,
  `vterm` outperforms `term` and has a nearly universal compatibility with
  terminal applications.

Vterm is not for you if you are using Windows, or if you cannot set up Emacs
with support for modules. Otherwise, you should try vterm, as it provides a
superior terminal experience in Emacs.

Using `vterm` is like using Gnome Terminal inside Emacs: Vterm is fully-featured
and fast, but is not as well integrated in Emacs as `eshell` (yet), so some of
the editing keybinding you are used to using may not work. For example,
`evil-mode` is currently not supported (though, users can enable VI emulation in
their shells). This is because keys are sent directly to the shell. We are
constantly working to improve this.

# Installation

## Requirements

Before installing emacs-libvterm, you need to make sure you have installed
 1. GNU Emacs (>= 25.1) with [module
    support](https://www.gnu.org/software/emacs/manual/html_node/elisp/Dynamic-Modules.html).
    You can check that, by verifying that `module-file-suffix` is not `nil`.
 2. cmake (>= 3.11)
 3. libtool-bin (related issues:
    [#66](https://github.com/akermu/emacs-libvterm/issues/66)
    [#85](https://github.com/akermu/emacs-libvterm/issues/85#issuecomment-491845136))
 4. OPTIONAL: [libvterm](https://github.com/neovim/libvterm) (>= 0.1). This
    library can be found in the official repositories of most distributions
    (e.g., Arch, Debian, Fedora, Gentoo, openSUSE, Ubuntu). Typical names are
    `libvterm` (Arch, Fedora, Gentoo, openSUSE), or `libvterm-dev` (Debian,
    Ubuntu). If not available, `libvterm` will be downloaded during the
    compilation process. Some distributions (e.g. Ubuntu < 20.04, Debian < 11)
    have versions of `libvterm` that are too old. If you find compilation errors
    related to `VTERM_COLOR`, you should not use your system libvterm. See
    [FAQ](#frequently-asked-questions-and-problems) for more details.

## From MELPA

`vterm` is available on [MELPA](https://melpa.org/), and it can be installed as
a normal package. If the requirements are satisfied (mainly, Emacs was built
with support for modules), `vterm` will compile the module the first time it is
run. This is the recommended way to install `vterm`.

`vterm` can be install from MELPA with `use-package` by adding the following
lines to your `init.el`:

```elisp
(use-package vterm
    :ensure t)
```

To take full advantage of the capabilities of `vterm`, you should configure your
shell too. Read about this in the section [shell-side
configuration](#shell-side-configuration).

## Manual installation

Clone the repository:

```sh
git clone https://github.com/akermu/emacs-libvterm.git
```

By default, vterm will try to find if libvterm is installed. If it is not found,
emacs-libvterm will download the latest version available of libvterm (from
[here](https://github.com/neovim/libvterm)), compile it, and use it. If you
always want to use the vendored version as opposed to the one on you system, set
`USE_SYSTEM_LIBVTERM` to `no`. To do this, change `cmake ..` with `cmake
-DUSE_SYSTEM_LIBVTERM=no ..` in the following instructions.

Build the module with:

``` sh
cd emacs-libvterm
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
  :load-path  "path/to/emacs-libvterm/")
```

## vterm and Ubuntu
### 20.04
Using `vterm` on Ubuntu requires additional steps. The latest LTS version
(20.04) ships without CMake installed and Emacs27 is not yet available from Ubuntu's package repository.

The basic steps for getting vterm to work on Ubuntu 20.04 are:
* Ensure Emacs27 is installed
* Install cmake, libtool, and libtool-bin

There are a few options for installing Emacs27 on Ubuntu 20.04:
* Compile Emacs27 from source
* Install Emacs27 from Snap
* Install Emacs27 from Kevin Kelley's PPA

In any case, if you have an older Emacs version you will need to purge it before proceeding:

#### Purge Emacs
```sh
sudo apt --purge remove emacs
sudo apt autoremove
```

#### Installing Emacs27 from Kevin Kelley PPA
```sh
sudo add-apt-repository ppa:kelleyk/emacs
sudo apt install emacs27
```

##### If you get an error about emacs27_common during the install process:
```sh
Errors were encountered while processing:
 /tmp/apt-dpkg-install-RVK8CA/064-emacs27-common_27.1~1.git86d8d76aa3-kk2+20.04_all.deb
```
run
```sh
sudo apt --purge remove emacs-common
sudo apt --fix-broken install
```

#### Installing Emacs27 from Snap
I hesitate to include SNAP here, because I ran into a number of GTK Theme parsing errors, and Fontconfig errors when I tested it, and reverted to installing from Kevin Kelley's PPA. YMMV
```sh
sudo snap install emacs --classic
```

#### Install CMake and Libtool
In Ubuntu 20.04 CMake (v3.16.3-1ubuntu1) and Libtool can be installed with
```sh
sudo apt install cmake
sudo apt install libtool
sudo apt install libtool-bin
```

### 18.04

Using `vterm` on Ubuntu 18.04 requires additional steps.
18.04 ships with a version of CMake that is too old for `vterm` and GNU
Emacs is not compiled with support for dynamical module loading.

It is possible to install GNU Emacs with module support from Kevin Kelley's PPA.
The binary in Ubuntu Emacs Lisp PPA is currently broken and leads to segmentation faults
(see [#185](https://github.com/akermu/emacs-libvterm/issues/185#issuecomment-562237077)).
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
[#216](https://github.com/akermu/emacs-libvterm/issues/216#issuecomment-575934593)).

Pull requests to improve support for Ubuntu are welcome (e.g., simplifying the
installation).

Some releases of Ubuntu (e.g., 18.04) ship with a old version of libvterm that
can lead to compilation errors. If you have this problem, see the
[FAQ](#frequently-asked-questions-and-problems) for a solution.

## GNU Guix

`vterm` and its dependencies are available in GNU Guix as
[emacs-vterm](https://guix.gnu.org/packages/emacs-vterm-0-1.7d7381f/).
The package can be installed with `guix package -i emacs-vterm`.

## Shell-side configuration

Some of the most useful features in `vterm` (e.g., [directory-tracking and
prompt-tracking](#directory-tracking-and-prompt-tracking) or [message
passing](#message-passing)) require shell-side configurations. The main goal of
these additional functions is to enable the shell to send information to `vterm`
via properly escaped sequences. A function that helps in this task,
`vterm_printf`, is defined below. This function is widely used throughout this
readme.

For `bash` or `zsh`, put this in your `.zshrc` or `.bashrc`
```bash
vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}
```
This works also for `dash`.

For `fish` put this in your `~/.config/fish/config.fish`:
```bash
function vterm_printf;
    if begin; [  -n "$TMUX" ]  ; and  string match -q -r "screen|tmux" "$TERM"; end 
        # tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
    else if string match -q -- "screen*" "$TERM"
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$argv"
    else
        printf "\e]%s\e\\" "$argv"
    end
end
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

If no region is selected when the enter key is pressed it will copy the current
line from start to end. If `vterm-copy-exclude-prompt` is true it will skip
the prompt and not include it in the copy.

## `vterm-clear-scrollback`

`vterm-clear-scrollback` does exactly what the name suggests: it clears the
current buffer from the data that it is not currently visible.
`vterm-clear-scrollback` is bound to `C-c C-l`. This function is typically used
with the `clear` function provided by the shell to clear both screen and
scrollback. In order to achieve this behavior, you need to add a new shell alias.

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
For `fish`:
```
if [ "$INSIDE_EMACS" = 'vterm' ]
    function clear
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
    end
end
```
These aliases take advantage of the fact that `vterm` can execute `elisp`
commands, as explained below.

If it possible to automatically clear the scrollback when the screen is cleared
by setting the variable `vterm-clear-scrollback-when-clearing`: When
`vterm-clear-scrollback-when-clearing` is non nil, `C-l` clears both the screen
and the scrollback. When is nil, `C-l` only clears the screen. The opposite
behavior can be achieved by using the universal prefix (i.e., calling `C-u C-l`).

# Customization

## `vterm-shell`

Shell to run in a new vterm. It defaults to `$SHELL`.

## `vterm-environment`

to add more environment variables there is the custom vterm-environment which has
a similar format than the internal Emacs variable process-environment.
You can check the documentation with C-h v process-environment for more details.

## `vterm-term-environment-variable`

Value for the `TERM` environment variable. It defaults to `xterm-256color`. If
[eterm-256color](https://github.com/dieggsy/eterm-256color) is installed,
setting `vterm-term-environment-variable` to `eterm-color` improves the
rendering of colors in some systems.

## `vterm-kill-buffer-on-exit`

If set to `t`, buffers are killed when the associated process is terminated (for
example, by logging out the shell). Keeping buffers around it is useful if you
need to copy or manipulate the content.

## `vterm-module-cmake-args`

Compilation flags and arguments to be given to CMake when compiling the module.
This string is directly passed to CMake, so it uses the same syntax. At the
moment, it main use is for compiling vterm using the system libvterm instead of
the one downloaded from GitHub. You can find all the arguments and flags
available with `cmake -LA` in the build directory.

## `vterm-copy-exclude-prompt`

Controls whether or not to exclude the prompt when copying a line in
`vterm-copy-mode`. Using the universal prefix before calling
`vterm-copy-mode-done` will invert the value for that call, allowing you to
temporarily override the setting. When a prompt is not found, the whole line is
copied.

## `vterm-use-vterm-prompt-detection-method`

The variable `vterm-use-vterm-prompt-detection-method` determines whether to use
the vterm prompt tracking, if false it use the regexp in
`vterm-copy-prompt-regexp` to search for the prompt.

## `vterm-enable-manipulate-selection-data-by-osc52`

Vterm support copy text to Emacs kill ring and system clipboard by using OSC 52.
See https://invisible-island.net/xterm/ctlseqs/ctlseqs.html for more info about OSC 52.
For example: send 'blabla' to kill ring: printf "\033]52;c;$(printf "%s" "blabla" | base64)\a"

tmux can share its copy buffer to terminals by supporting osc52(like iterm2 xterm),
you can enable this feature for tmux by :
set -g set-clipboard on         #osc 52 copy paste share with iterm
set -ga terminal-overrides ',xterm*:XT:Ms=\E]52;%p1%s;%p2%s\007'
set -ga terminal-overrides ',screen*:XT:Ms=\E]52;%p1%s;%p2%s\007'

The clipboard querying/clearing functionality offered by OSC 52 is not implemented here,
And for security reason, this feature is disabled by default."

This feature need the new way of handling strings with a struct `VTermStringFragment`
in libvterm. You'd better compile emacs-libvterm with `cmake -DUSE_SYSTEM_LIBVTERM=no ..`.
If you don't do that, when  the content you want to copied is too long, it would be truncated
by bug of libvterm.

## `vterm-buffer-name-string`

When `vterm-buffer-name-string` is not nil, vterm renames automatically its own
buffers with `vterm-buffer-name-string`. This string can contain the character
`%s`, which is substituted with the _title_ (as defined by the shell, see
below). A possible value for `vterm-buffer-name-string` is `vterm %s`, according
to which all the vterm buffers will be named "vterm TITLE".

This requires some shell-side configuration to print the title. For example to
set the name "HOSTNAME:PWD", use can you the following:

For `zsh`
```zsh
autoload -U add-zsh-hook
add-zsh-hook -Uz chpwd (){ print -Pn "\e]2;%m:%2~\a" }
```
For `bash`,
```bash
PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND; }"'echo -ne "\033]0;${HOSTNAME}:${PWD}\007"'
```
For `fish`,
```fish
function fish_title
    hostname
    echo ":"
    pwd
end
```
See [zsh and bash](http://tldp.org/HOWTO/Xterm-Title-4.html) and [fish
documentations](https://fishshell.com/docs/current/#programmable-title).

## `vterm-always-compile-module`

Vterm needs `vterm-module` to work. This can be compiled externally, or `vterm`
will ask the user whether to build the module when `vterm` is first called. To
avoid this question and always compile the module, set
`vterm-always-compile-module` to `t`.

## Keybindings

If you want a key to be sent to the terminal, bind it to `vterm--self-insert`,
or remove it from `vterm-mode-map`. By default, `vterm.el` binds most of the
`C-<char>` and `M-<char>` keys, `<f1>` through `<f12>` and some special keys
like `<backspace>` and `<return>`. Sending a keyboard interrupt is bound to `C-c
C-c`.

In order to send a keypress that is already recognized by Emacs, such as `C-g`,
use the interactive function `vterm-send-next-key`. This can be bound to a key
in the `vterm-mode-map` like `C-q`, in which case pressing `C-q C-g` will send a
`C-g` key to the terminal, and so on for other modified keys:

``` emacs
(define-key vterm-mode-map (kbd "C-q") #'vterm-send-next-key)
```

This can be useful for controlling an application running in the terminal, such
as Emacs or Nano.

## Fonts

You can change the font (the _face_) used in a vterm with the following code:

``` emacs
(add-hook 'vterm-mode-hook
          (lambda ()
            (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
                 (buffer-face-mode t)))
```

Where instead of `'fixed-pitch` you specify the face you want to use. The
example reported here can be used to force vterm to use a mono-spaced font (the
`fixed-pitch` face). This is useful when your default font in Emacs is a
proportional font.

In addition to that, you can disable some text properties (bold, underline,
reverse video) setting the relative option to `t` (`vterm-disable-bold`,
`vterm-disable-underline`, or `vterm-disable-inverse-video`).

## Blink cursor

When `vterm-ignore-blink-cursor` is `t`, vterm will ignore request from application to turn on or off cursor blink.

If `nil`, cursor in any window may begin to blink or not blink because `blink-cursor-mode`
is a global minor mode in Emacs, you can use `M-x blink-cursor-mode` to toggle.

## Colors

Set the `:foreground` and `:background` attributes of the following faces to a
color you like. The `:foreground` is ansi color 0-7, the `:background` attribute
is ansi color 8-15.

- vterm-color-black
- vterm-color-red
- vterm-color-green
- vterm-color-yellow
- vterm-color-blue
- vterm-color-magenta
- vterm-color-cyan
- vterm-color-white

## Directory tracking and Prompt tracking

`vterm` supports _directory tracking_. If this feature is enabled, the default
directory in Emacs and the current working directory in `vterm` are synced. As a
result, interactive functions that ask for a path or a file (e.g., `dired` or
`find-file`) will do so starting from the current location.

And `vterm` supports _prompt tracking_. If this feature is enabled, Emacs knows
where the prompt ends, you needn't  customize `term-prompt-regexp` any more.
Then you can use `vterm-next-prompt` and `vterm-previous-prompt`
moving to end of next/previous prompt. The default keybinding is `C-c C-n` and `C-c C-p`.

And `vterm-beginning-of-line` would move the point to the first character after the
shell prompt on this line. If the point is already there, move to the beginning of the line.
The default keybinding is `C-a` in `vterm-copy-mode`.

And `vterm--at-prompt-p` would check whether the cursor is at the point just after
the shell prompt.

Directory tracking and Prompt tracking requires some configuration, as the shell has to be
instructed to share the relevant information with Emacs. The following pieces of
code assume that you have the function `vterm_printf` as defined in section
[shell-side configuration](#shell-side-configuration).

For `zsh`, put this at the end of your `.zshrc`:

```zsh
vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";
}
setopt PROMPT_SUBST
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
function vterm_prompt_end;
    vterm_printf '51;A'(whoami)'@'(hostname)':'(pwd)
end
functions --copy fish_prompt vterm_old_fish_prompt
function fish_prompt --description 'Write out the prompt; do not replace this. Instead, put this at end of your file.'
    # Remove the trailing newline from the original prompt. This is done
    # using the string builtin from fish, but to make sure any escape codes
    # are correctly interpreted, use %b for printf.
    printf "%b" (string join "\n" (vterm_old_fish_prompt))
    vterm_prompt_end
end
```
Here we are using the function `vterm_printf` that we have discussed above, so make
sure that this function is defined in your configuration file.


Directory tracking works on remote servers too. In case the hostname of your
remote machine does not match the actual hostname needed to connect to that
server, change `$(hostname)` with the correct one. For example, if the correct
hostname is `foo` and the username is `bar`, you should have something like
```bash
HOSTNAME=foo
USER=baz
vterm_printf "51;A$USER@$HOSTNAME:$(pwd)"
```

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
and backslashes need to be escaped via backslash. A convenient shell function to
automate the substitution is

`bash` or `zsh`:
```sh
vterm_cmd() {
    local vterm_elisp
    vterm_elisp=""
    while [ $# -gt 0 ]; do
        vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
        shift
    done
    vterm_printf "51;E$vterm_elisp"
}
```
`fish`:
```sh
function vterm_cmd --description 'Run an Emacs command among the ones been defined in vterm-eval-cmds.'
    set -l vterm_elisp ()
    for arg in $argv
        set -a vterm_elisp (printf '"%s" ' (string replace -a -r '([\\\\"])' '\\\\\\\\$1' $arg))
    end
    vterm_printf '51;E'(string join '' $vterm_elisp)
end
```

Now we can write shell functions to call the ones defined in `vterm-eval-cmds`.

```sh
find_file() {
    vterm_cmd find-file "$(realpath "${@:-.}")"
}

say() {
    vterm_cmd message "%s" "$*"
}
```

Or for `fish`:
```fish
function find_file
    set -q argv[1]; or set argv[1] "."
    vterm_cmd find-file (realpath "$argv")
end

function say
    vterm_cmd message "%s" "$argv"
end
```

This newly defined `find_file` function can now be used inside `vterm` as

```sh
find_file name_of_file_in_local_directory
```
If you call `find_file` without specifying any file (you just execute `find_file` in your shell),
`dired` will open with the current directory.

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
    vterm_cmd find-file-below "$(realpath "${@:-.}")"
}
```

Then you can open any file from inside your shell.

```sh
open_file_below ~/Documents
```

## Shell-side configuration files

The configurations described in earlier sections are combined in
[`etc/`](./etc/). These can be appended to or loaded into your user
configuration file. Alternatively, they can be installed system-wide, for
example in `/etc/bash/bashrc.d/`, `/etc/profile.d/` (for `zsh`), or
`/etc/fish/conf.d/` for `fish`.

When using vterm Emacs sets the environment variable INSIDE_EMACS in the subshell to ‘vterm’.
Usually the programs check this variable to determine whether they are running inside Emacs.

Vterm also sets an extra variable EMACS_VTERM_PATH to the place where the vterm library is installed.
This is very useful because when vterm is installed from melpa the Shell-side configuration files are
in the EMACS_VTERM_PATH inside the /etc sub-directory. After a package update, the directory name changes,
so, a code like this in your bashrc could be enough to load always the latest version of the file
from the right location without coping any file manually.

```
if [[ "$INSIDE_EMACS" = 'vterm' ]] \
    && [[ -n ${EMACS_VTERM_PATH} ]] \
    && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh ]]; then
	source ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh
fi
```

## Frequently Asked Questions and Problems

### How can I increase the size of the scrollback?

By default, the scrollback can contain up to 1000 lines per each vterm buffer.
You can increase this up to 100000 by changing the variable
`vterm-max-scrollback`. If you want to increase it further, you have to edit the
file `vterm-module.h`, change the variable `SB_MAX`, and set the new value for
`vterm-max-scrollback`. The potential maximum memory consumption of vterm
buffers increases with `vterm-max-scrollback`, so setting `SB_MAX` to extreme
values may lead to system instabilities and crashes.
 
### How can I automatically close vterm buffers when the process is terminated?

There is an option for that: set `vterm-kill-buffer-on-exit` to `t`.

### The package does not compile, I have errors related to `VTERM_COLOR`.

The version of `libvterm` installed on your system is too old. You should let
`emacs-libvterm` download `libvterm` for you. You can either uninstall your
libvterm, or instruct Emacs to ignore the system libvterm. If you are compiling
from Emacs, you can do this by setting:
```emacs-lisp
(setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")
```
and compile again. If you are compiling with CMake, use the flag
`-DUSE_SYSTEM_LIBVTERM=no`.

### `<C-backspace>` doesn't kill previous word.

This can be fixed by rebinding the key to what `C-w` does:
```emacs-lisp
(define-key vterm-mode-map (kbd "<C-backspace>")
    (lambda () (interactive) (vterm-send-key (kbd "C-w"))))
```

### `counsel-yank-pop` doesn't work.

Add this piece of code to your configuration file to make `counsel` use
the correct function to yank in vterm buffers.
```emacs-lisp
(defun vterm-counsel-yank-pop-action (orig-fun &rest args)
  (if (equal major-mode 'vterm-mode)
      (let ((inhibit-read-only t)
            (yank-undo-function (lambda (_start _end) (vterm-undo))))
        (cl-letf (((symbol-function 'insert-for-yank)
               (lambda (str) (vterm-send-string str t))))
            (apply orig-fun args)))
    (apply orig-fun args)))

(advice-add 'counsel-yank-pop-action :around #'vterm-counsel-yank-pop-action)
```

### How can I get the local directory without shell-side configuration?

We recommend that you set up shell-side configuration for reliable directory
tracking. If you cannot do it, a possible workaround is the following.

On most GNU/Linux systems, you can read current directory from `/proc`:
```emacs-lisp
(defun vterm-directory-sync ()
  "Synchronize current working directory."
  (interactive)
  (when vterm--process
    (let* ((pid (process-id vterm--process))
           (dir (file-truename (format "/proc/%d/cwd/" pid))))
      (setq default-directory dir))))
```
A possible application of this function is in combination with `find-file`:
```emacs-lisp
(advice-add #'find-file :before #'vterm-directory-sync)
```
This method does not work on remote machines.

### How can I get the directory tracking in a more understandable way?

If you looked at the recommended way to set-up directory tracking, you will have
noticed that it requires printing obscure code like `\e]2;%m:%2~\a` (unless you
are using `fish`).

There is another way to achieve this behavior. Define a shell function, on a
local host you can simply use

``` sh
vterm_set_directory() {
    vterm_cmd update-pwd "$PWD/"
}
```
On a remote one, use instead
``` sh
vterm_set_directory() {
    vterm_cmd update-pwd "/-:""$USER""@""$HOSTNAME"":""$PWD/"
}
```
Then, for `zsh`, add this function to the `chpwd` hook:

``` sh
autoload -U add-zsh-hook
add-zsh-hook -Uz chpwd (){ vterm_set_directory }
```
For `bash`, append it to the prompt:

``` sh
PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND; }vterm_set_directory"
```
Finally, add `update-pwd` to the list of commands that Emacs
is allowed to execute from vterm:

``` emacs-lisp
(add-to-list 'vterm-eval-cmds '("update-pwd" (lambda (path) (setq default-directory path))))
```

### When evil-mode is enabled, the cursor moves back in normal state, and this messes directory tracking

`evil-collection` provides a solution for this problem. If you do not want to
use `evil-collection`, you can add the following code:

```emacs-lisp
(defun evil-collection-vterm-escape-stay ()
"Go back to normal state but don't move
cursor backwards. Moving cursor backwards is the default vim behavior but it is
not appropriate in some cases like terminals."
(setq-local evil-move-cursor-back nil))

(add-hook 'vterm-mode-hook #'evil-collection-vterm-escape-stay)
```


## Related packages

- [vterm-toggle](https://github.com/jixiuf/vterm-toggle): Toggles between a
  vterm and the current buffer
- [multi-libvterm](https://github.com/suonlight/multi-libvterm): Multiterm for emacs-libvterm

## Appendix

### Breaking changes

Obsolete variables will be removed in version 0.1.

#### October 2020

* `vterm-disable-bold-font` was renamed to `vterm-disable-bold` to uniform it
   with the other similar options.

#### July 2020

* `vterm-use-vterm-prompt` was renamed to `vterm-use-vterm-prompt-detection-method`.
* `vterm-kill-buffer-on-exit` is set to `t` by default.

#### April 2020

* `vterm-clear-scrollback` was renamed to `vterm-clear-scrollback-when-clearning`.
* `vterm-set-title-functions` was removed. In its place, there is a new custom
  option `vterm-buffer-name-string`. See
  [vterm-buffer-name-string](vterm-buffer-name-string) for documentation.
