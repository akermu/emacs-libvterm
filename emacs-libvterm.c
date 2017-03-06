#include <emacs-module.h>
#include <string.h>
#include <vterm.h>
#include <vterm_keycodes.h>
#include <pty.h>
#include <unistd.h>

/* Declare mandatory GPL symbol.  */
int plugin_is_GPL_compatible;

/* Bind NAME to FUN.  */
static void
bind_function (emacs_env *env, const char *name, emacs_value Sfun)
{
  /* Set the function cell of the symbol named NAME to SFUN using
     the 'fset' function.  */

  /* Convert the strings to symbols by interning them */
  emacs_value Qfset = env->intern (env, "fset");
  emacs_value Qsym = env->intern (env, name);

  /* Prepare the arguments array */
  emacs_value args[] = { Qsym, Sfun };

  /* Make the call (2 == nb of arguments) */
  env->funcall (env, Qfset, 2, args);
}

/* Provide FEATURE to Emacs.  */
static void
provide (emacs_env *env, const char *feature)
{
  /* call 'provide' with FEATURE converted to a symbol */

  emacs_value Qfeat = env->intern (env, feature);
  emacs_value Qprovide = env->intern (env, "provide");
  emacs_value args[] = { Qfeat };

  env->funcall (env, Qprovide, 1, args);
}

static void
insert(emacs_env *env, const char *string)
{
  emacs_value Qinsert = env->intern(env, "insert");
  emacs_value Qstring = env->make_string(env, string, strlen(string));
  emacs_value args[] = { Qstring };

  env->funcall(env, Qinsert, 1, args);
}

/* New emacs lisp function. All function exposed to Emacs must have this prototype. */
static emacs_value
Fvterm_new (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  int rows = 24;
  int columns = 80;

  int master;
  struct winsize size = { rows, columns, 0, 0};

  struct termios termios = {
    .c_iflag = ICRNL|IXON,
    .c_oflag = OPOST|ONLCR
#ifdef TAB0
      |TAB0
#endif
    ,
    .c_cflag = CS8|CREAD,
    .c_lflag = ISIG|ICANON|IEXTEN|ECHO|ECHOE|ECHOK,
    /* c_cc later */
  };

#ifdef IUTF8
  termios.c_iflag |= IUTF8;
#endif
#ifdef NL0
  termios.c_oflag |= NL0;
#endif
#ifdef CR0
  termios.c_oflag |= CR0;
#endif
#ifdef BS0
  termios.c_oflag |= BS0;
#endif
#ifdef VT0
  termios.c_oflag |= VT0;
#endif
#ifdef FF0
  termios.c_oflag |= FF0;
#endif
#ifdef ECHOCTL
  termios.c_lflag |= ECHOCTL;
#endif
#ifdef ECHOKE
  termios.c_lflag |= ECHOKE;
#endif

  cfsetspeed(&termios, 38400);

  termios.c_cc[VINTR]    = 0x1f & 'C';
  termios.c_cc[VQUIT]    = 0x1f & '\\';
  termios.c_cc[VERASE]   = 0x7f;
  termios.c_cc[VKILL]    = 0x1f & 'U';
  termios.c_cc[VEOF]     = 0x1f & 'D';
  termios.c_cc[VEOL]     = _POSIX_VDISABLE;
  termios.c_cc[VEOL2]    = _POSIX_VDISABLE;
  termios.c_cc[VSTART]   = 0x1f & 'Q';
  termios.c_cc[VSTOP]    = 0x1f & 'S';
  termios.c_cc[VSUSP]    = 0x1f & 'Z';
  termios.c_cc[VREPRINT] = 0x1f & 'R';
  termios.c_cc[VWERASE]  = 0x1f & 'W';
  termios.c_cc[VLNEXT]   = 0x1f & 'V';
  termios.c_cc[VMIN]     = 1;
  termios.c_cc[VTIME]    = 0;

  pid_t pid = forkpty(&master, NULL, &termios, &size);
  if (pid == 0) {
    char *shell = getenv("SHELL");
    char *args[2] = { shell, NULL };
    execvp(shell, args);
    exit(1);
  }
  char buffer[1920];

  VTerm *vt = vterm_new(rows, columns);
  vterm_set_utf8(vt, 1);

  vterm_input_write(vt, "ls -l\n ", 6);

  int len;
  while ((len = vterm_output_read(vt, buffer, 1920)) > 0) {
    insert(env, buffer);
  }

  vterm_free(vt);

  return env->make_integer(env, 0);
}

int
emacs_module_init (struct emacs_runtime *ert)
{
  emacs_env *env = ert->get_environment (ert);

  /* create a lambda (returns an emacs_value) */
  emacs_value fun = env->make_function (env,
              0,            /* min. number of arguments */
              0,            /* max. number of arguments */
              Fvterm_new,   /* actual function pointer */
              "doc",        /* docstring */
              NULL          /* user pointer of your choice (data param in Fmymod_test) */
  );

  bind_function (env, "vterm_new", fun);
  provide (env, "emacs-libvterm");

  /* loaded successfully */
  return 0;
}
