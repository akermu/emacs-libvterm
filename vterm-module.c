#include <emacs-module.h>
#include <vterm.h>
#include <string.h>

#define MAX(x, y) (((x) > (y)) ? (x) : (y))

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

static void
message (emacs_env *env, char *message) {
  emacs_value Fmessage = env->intern(env, "message");
  emacs_value string = env->make_string(env, message, strlen(message));
  env->funcall(env, Fmessage, 1, (emacs_value[]){string});
}

static void
erase_buffer (emacs_env *env) {
  emacs_value Ferase_buffer = env->intern(env, "erase-buffer");
  env->funcall(env, Ferase_buffer, 0, NULL);
}

static void
insert_line (emacs_env *env, char *line) {
  emacs_value Finsert = env->intern(env, "insert");
  emacs_value Sline = env->make_string(env, line, strlen(line));
  env->funcall(env, Finsert, 1, (emacs_value[]){Sline});

  emacs_value Snew_line = env->make_string(env, "\n", 1);
  env->funcall(env, Finsert, 1, (emacs_value[]){Snew_line});
}

static void
goto_char (emacs_env *env, int pos) {
  emacs_value Fgoto_char = env->intern(env, "goto-char");
  emacs_value point = env->make_integer(env, pos);
  env->funcall(env, Fgoto_char, 1, (emacs_value[]){point});
}

static emacs_value
vterm_refresh (VTerm *vt, emacs_env *env) {
  int i, j;
  int rows, cols;
  VTermScreen *screen = vterm_obtain_screen(vt);
  vterm_get_size(vt, &rows, &cols);

  erase_buffer(env);

  char line[cols + 1];
  for (i = 0; i < rows; i++) {
    for (j = 0; j < cols; j++) {
      VTermPos pos = { .row = i, .col = j};
      VTermScreenCell cell;
      vterm_screen_get_cell(screen, pos, &cell);
      if (cell.chars[0] == '\0')
        line[j] = ' ';
      else
        line[j] = cell.chars[0];
    }
    line[cols] = '\0';
    insert_line(env, line);
  }

  VTermState *state = vterm_obtain_state(vt);
  VTermPos pos;
  vterm_state_get_cursorpos(state, &pos);

  // row * (width + 1) because of newline character
  // col + 1 because (goto-char 1) sets point to first position
  int point = (pos.row * 81) + pos.col + 1;
  goto_char(env, point);

  return env->make_integer(env, 0);
}

static void
vterm_write_stdin(emacs_env *env, char *buffer, ptrdiff_t len) {
  emacs_value Fvterm_write_stdin = env->intern(env, "vterm-write-stdin");
  emacs_value string = env->make_string(env, buffer, len);
  emacs_value Fbase64_encode_string = env->intern(env, "base64-encode-string");
  string = env->funcall(env, Fbase64_encode_string, 1, (emacs_value[]){string});
  env->funcall(env, Fvterm_write_stdin, 1, (emacs_value[]){string});
}

static void
vterm_flush_output (VTerm *vt, emacs_env *env) {
  size_t bufflen = vterm_output_get_buffer_current(vt);
  if(bufflen) {
    char buffer[bufflen];
    bufflen = vterm_output_read(vt, buffer, bufflen);
    vterm_write_stdin(env, buffer, bufflen);
  }
}

static void
vterm_finalize (void *vt) {
  vterm_free(vt);
}

static emacs_value
Fvterm_new (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  int rows = 24;
  int cols = 80;

  VTerm *vt = vterm_new(rows, cols);
  vterm_set_utf8(vt, 1);

  VTermScreen *screen = vterm_obtain_screen(vt);
  vterm_screen_reset(screen, 1);

  return env->make_user_ptr(env, vterm_finalize, vt);
}

static emacs_value
Fvterm_input_write (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  VTerm *vt = env->get_user_ptr(env, args[0]);
  emacs_value input = args[1];
  ptrdiff_t len = string_len(env, input);
  char buffer[len];
  env->copy_string_contents(env, input, buffer, &len);

  vterm_input_write(vt, buffer, len - 1);

  vterm_refresh(vt, env);

  return env->make_integer(env, len - 1);
}

static emacs_value
Fvterm_send_key (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) {
  VTerm *vt = env->get_user_ptr(env, args[0]);
  emacs_value key = args[1];
  ptrdiff_t len = string_len(env, key);
  char buffer[len];
  env->copy_string_contents(env, key, buffer, &len);

  if (strcmp(buffer, "ENTER") == 0) {
    vterm_keyboard_key(vt, VTERM_KEY_ENTER, VTERM_MOD_NONE);
  } else {
    vterm_keyboard_unichar(vt, buffer[0], VTERM_MOD_NONE);
  }

  vterm_flush_output(vt, env);

  return env->make_integer(env, 0);
}

int
emacs_module_init (struct emacs_runtime *ert)
{
  emacs_env *env = ert->get_environment (ert);
  emacs_value fun;

  fun = env->make_function (env,
                            0,
                            0,
                            Fvterm_new,
                            "Allocates a new vterm.",
                            NULL
                            );
  bind_function (env, "vterm-new", fun);

  fun = env->make_function (env,
                            2,
                            2,
                            Fvterm_input_write,
                            "Writes input to libvterm.",
                            NULL
                            );
  bind_function (env, "vterm-input-write", fun);

  fun = env->make_function (env,
                            2,
                            2,
                            Fvterm_send_key,
                            "Writes a key to libvterm.",
                            NULL
                            );
  bind_function (env, "vterm-send-key", fun);


  provide (env, "vterm-module");

  return 0;
}
