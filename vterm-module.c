#include <emacs-module.h>
#include <vterm.h>

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

static int
string_len (emacs_env *env, emacs_value string)
{
  ptrdiff_t size = 0;
  env->copy_string_contents (env, string, NULL, &size);
  return size;
}

static emacs_value
Fvterm_input_write (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  int rows = 24;
  int columns = 80;

  VTerm *vt = vterm_new(rows, columns);
  vterm_set_utf8(vt, 1);

  ptrdiff_t len = string_len(env, args[0]);
  char buffer[len];
  int ret = env->copy_string_contents(env, args[0], buffer, &len);

  vterm_input_write(vt, buffer, len);

  return env->make_integer(env, len);
}

int
emacs_module_init (struct emacs_runtime *ert)
{
  emacs_env *env = ert->get_environment (ert);

  emacs_value fun;
  fun = env->make_function (env,
                            1,
                            1,
                            Fvterm_input_write,
                            "Writes input to libvterm",
                            NULL
                            );
  bind_function (env, "vterm-input-write", fun);

  provide (env, "vterm-module");

  return 0;
}
