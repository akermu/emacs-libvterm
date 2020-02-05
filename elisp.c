#include "elisp.h"
#include <stdio.h>

/* Set the function cell of the symbol named NAME to SFUN using
   the 'fset' function.  */
void bind_function(emacs_env *env, const char *name, emacs_value Sfun) {
  emacs_value Qfset = env->intern(env, "fset");
  emacs_value Qsym = env->intern(env, name);

  env->funcall(env, Qfset, 2, (emacs_value[]){Qsym, Sfun});
}

/* Provide FEATURE to Emacs.  */
void provide(emacs_env *env, const char *feature) {
  emacs_value Qfeat = env->intern(env, feature);
  emacs_value Qprovide = env->intern(env, "provide");

  env->funcall(env, Qprovide, 1, (emacs_value[]){Qfeat});
}

emacs_value symbol_value(emacs_env *env, emacs_value symbol) {
  return env->funcall(env, Fsymbol_value, 1, (emacs_value[]){symbol});
}

int string_bytes(emacs_env *env, emacs_value string) {
  ptrdiff_t size = 0;
  env->copy_string_contents(env, string, NULL, &size);
  return size;
}

emacs_value length(emacs_env *env, emacs_value string) {
  return env->funcall(env, Flength, 1, (emacs_value[]){string});
}

emacs_value list(emacs_env *env, emacs_value elements[], ptrdiff_t len) {
  return env->funcall(env, Flist, len, elements);
}
emacs_value nth(emacs_env *env, int idx, emacs_value list) {
  emacs_value eidx = env->make_integer(env, idx);
  return env->funcall(env, Fnth, 2, (emacs_value[]){eidx, list});
}

void put_text_property(emacs_env *env, emacs_value string, emacs_value property,
                       emacs_value value) {
  emacs_value start = env->make_integer(env, 0);
  emacs_value end = length(env, string);

  env->funcall(env, Fput_text_property, 5,
               (emacs_value[]){start, end, property, value, string});
}

void erase_buffer(emacs_env *env) { env->funcall(env, Ferase_buffer, 0, NULL); }

void insert(emacs_env *env, emacs_value string) {
  env->funcall(env, Finsert, 1, (emacs_value[]){string});
}

void goto_char(emacs_env *env, int pos) {
  emacs_value point = env->make_integer(env, pos);
  env->funcall(env, Fgoto_char, 1, (emacs_value[]){point});
}

void forward_line(emacs_env *env, int n) {
  emacs_value nline = env->make_integer(env, n);
  env->funcall(env, Fforward_line, 1, (emacs_value[]){nline});
}
void goto_line(emacs_env *env, int n) {
  emacs_value nline = env->make_integer(env, n);
  env->funcall(env, Fgoto_line, 1, (emacs_value[]){nline});
}
void delete_lines(emacs_env *env, int linenum, int count, bool del_whole_line) {
  emacs_value Qlinenum = env->make_integer(env, linenum);
  emacs_value Qcount = env->make_integer(env, count);
  if (del_whole_line) {
    env->funcall(env, Fdelete_lines, 3, (emacs_value[]){Qlinenum, Qcount, Qt});
  } else {
    env->funcall(env, Fdelete_lines, 3,
                 (emacs_value[]){Qlinenum, Qcount, Qnil});
  }
}
void recenter(emacs_env *env, emacs_value pos) {
  env->funcall(env, Frecenter, 1, (emacs_value[]){pos});
}
emacs_value point(emacs_env *env) { return env->funcall(env, Fpoint, 0, NULL); }

void set_window_point(emacs_env *env, emacs_value win, emacs_value point) {
  env->funcall(env, Fset_window_point, 2, (emacs_value[]){win, point});
}
emacs_value window_body_height(emacs_env *env, emacs_value win) {
  return env->funcall(env, Fwindow_body_height, 1, (emacs_value[]){win});
}

bool eq(emacs_env *env, emacs_value e1, emacs_value e2) {
  emacs_value Qeq = env->funcall(env, Feq, 2, (emacs_value[]){e1, e2});
  return env->is_not_nil(env, Qeq);
}

void forward_char(emacs_env *env, emacs_value n) {
  env->funcall(env, Fforward_char, 1, (emacs_value[]){n});
}

emacs_value get_buffer_window_list(emacs_env *env) {
  return env->funcall(env, Fget_buffer_window_list, 3,
                      (emacs_value[]){Qnil, Qnil, Qt});
}

emacs_value selected_window(emacs_env *env) {
  return env->funcall(env, Fselected_window, 0, (emacs_value[]){});
}

void set_cursor_type(emacs_env *env, emacs_value QCursorType) {
  env->funcall(env, Fset, 2, (emacs_value[]){Qcursor_type, QCursorType});
}

emacs_value vterm_get_color(emacs_env *env, int index) {
  emacs_value idx = env->make_integer(env, index);
  return env->funcall(env, Fvterm_get_color, 1, (emacs_value[]){idx});
}

void set_title(emacs_env *env, emacs_value string) {
  env->funcall(env, Fvterm_set_title, 1, (emacs_value[]){string});
}

void set_directory(emacs_env *env, emacs_value string) {
  env->funcall(env, Fvterm_set_directory, 1, (emacs_value[]){string});
}

void vterm_invalidate(emacs_env *env) {
  env->funcall(env, Fvterm_invalidate, 0, NULL);
}
emacs_value vterm_eval(emacs_env *env, emacs_value string) {
  return env->funcall(env, Fvterm_eval, 1, (emacs_value[]){string});
}
