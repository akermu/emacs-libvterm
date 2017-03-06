#include <emacs-module.h>
#include <string.h>
#include <vterm.h>
#include <vterm_keycodes.h>

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
