# path to the emacs source dir
# (you can provide it here or on the command line)
EMACS-SRC = /home/lukas/.local/aur/emacs/emacs-25.2
LIBVTERM  = /usr/include
CC	  = gcc
LD	  = gcc
CFLAGS	  = -ggdb3 -Wall
LDFLAGS	  =

ROOT := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))

all: vterm-module.so

# make shared library out of the object file
%.so: %.o
	$(LD) -shared $(LDFLAGS) -lvterm -lutil -o $@ $<

# compile source file to object file
%.o: %.c
	$(CC) $(CFLAGS) -I$(EMACS-SRC)/src -I/ -fPIC -c $<

run: vterm-module.so
	emacs -L $(ROOT) --eval "(require 'vterm)" # --eval '(insert "(vterm-new)")'
