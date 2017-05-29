#include <emacs-module.h>
#include <string.h>
#include <vterm.h>
#include <vterm_keycodes.h>
#include <pty.h>
#include <unistd.h>
#include <pthread.h>
#include <poll.h>
#include <semaphore.h>
#include <signal.h>

/* Declare mandatory GPL symbol.  */
int plugin_is_GPL_compatible;

struct Term {
  int masterfd;
  struct VTerm *vt;
  sem_t mutex_cells;
  char cells[24][80];
  char buffer[4096];
  int read_size;
};

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
insert(emacs_env *env, const char c)
{
  char string[1] = {c};
  emacs_value Qinsert = env->intern(env, "insert");
  emacs_value Qstring = env->make_string(env, string, 1);
  emacs_value args[] = { Qstring };

  env->funcall(env, Qinsert, 1, args);
}

static void *io_loop(void *arg) {
  struct Term *t = (struct Term *)arg;

  int i, j;
  for (i = 0; i < 24; i++) {
    for (j = 0; j < 80; j++) {
      t->cells[i][j] = '0';
    }
  }

  while (1) {
    /* struct pollfd p = { t->masterfd, (0 | POLLIN), 0}; */
    /* poll(&p, 1, 10); */
    /* if ((p.revents & POLLIN) == POLLIN) { */
      ssize_t size;
      char buffer[4096];
      while ((size = read(t->masterfd, buffer, sizeof buffer)) > 0) {
        sem_wait(&t->mutex_cells);
        vterm_input_write(t->vt, buffer, size);
        sem_post(&t->mutex_cells);
      }

    /* } */

    size_t bufflen = vterm_output_get_buffer_current(t->vt);
    if(bufflen) {
      char buffer[bufflen];
      bufflen = vterm_output_read(t->vt, buffer, bufflen);
      write(t->masterfd, buffer, bufflen);
    }
  }
}

static emacs_value
Fvterm_get_output (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) {
  struct Term *t = (struct Term*) data;
  sem_wait(&t->mutex_cells);

  int i, j;
  int rows, cols;
  VTermScreen *screen = vterm_obtain_screen(t->vt);
  vterm_get_size(t->vt, &rows, &cols);
  insert(env, rows);
  insert(env, '\n');
  insert(env, cols);
  insert(env, '\n');
  insert(env, t->read_size);
  insert(env, '\n');
  for (i = 0; i < rows; i++) {
    for (j = 0; j < cols; j++) {
      VTermPos pos = { .row = i, .col = j};
      VTermScreenCell cell;
      vterm_screen_get_cell(screen, pos, &cell);
      insert(env, cell.chars[0]);
    }
  }
  sem_post(&t->mutex_cells);

  return env->make_integer(env, 0);
}

/* New emacs lisp function. All function exposed to Emacs must have this prototype. */
static emacs_value
Fvterm_new (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  int rows = 24;
  int columns = 80;

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

  VTerm *vt = vterm_new(rows, columns);
  vterm_set_utf8(vt, 1);

/*   VTermScreen *screen = vterm_obtain_screen(vt); */
/* /\*   VTermScreenCallbacks callbacks = { *\/ */
/* /\*     . *\/ */
/* /\* } *\/ */
/*   vterm_screen_set_callbacks(screen, callbacks, NULL); */

  struct Term *t = malloc(sizeof(struct Term));
  t->vt = vt;
  t->read_size = 0;
  sem_init(&t->mutex_cells, 0, 1);

  pid_t pid = forkpty(&t->masterfd, NULL, &termios, &size);
  if (pid == 0) {
    signal(SIGINT,  SIG_DFL);
    signal(SIGQUIT, SIG_DFL);
    signal(SIGSTOP, SIG_DFL);
    signal(SIGCONT, SIG_DFL);

    putenv("TERM=xterm");

    char *shell = getenv("SHELL");
    char *args[2] = { shell, NULL };
    execvp(shell, args);
    exit(1);
  }

  char buffer[4096];
  ssize_t bytes = read(t->masterfd, buffer, sizeof buffer);
  vterm_input_write(vt, buffer, bytes);
  insert(env, '\n');
  insert(env, bytes);
  insert(env, '\n');

  emacs_value fun = env->make_function (env,
                                        0,                 /* min. number of arguments */
                                        0,                 /* max. number of arguments */
                                        Fvterm_get_output, /* actual function pointer */
                                        "doc",             /* docstring */
                                        t                  /* user pointer of your choice (data param in Fmymod_test) */
                                        );
  bind_function (env, "vterm_get_output", fun);

  pthread_t thread;
  // TODO: Error handling
  int ret = pthread_create(&thread, NULL, io_loop, t);

  return env->make_integer(env, ret);
}

int
emacs_module_init (struct emacs_runtime *ert)
{
  emacs_env *env = ert->get_environment (ert);

  emacs_value fun;
  fun = env->make_function (env,
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
