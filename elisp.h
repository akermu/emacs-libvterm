#ifndef ELISP_H
#define ELISP_H

#include "emacs-module.h"
#include "vterm.h"

// Emacs symbols
emacs_value Qt;
emacs_value Qnil;
emacs_value Qnormal;
emacs_value Qbold;
emacs_value Qitalic;
emacs_value Qforeground;
emacs_value Qbackground;
emacs_value Qweight;
emacs_value Qunderline;
emacs_value Qslant;
emacs_value Qreverse;
emacs_value Qstrike;
emacs_value Qextend;
emacs_value Qface;
emacs_value Qbox;
emacs_value Qbar;
emacs_value Qhbar;
emacs_value Qcursor_type;
emacs_value Qemacs_major_version;
emacs_value Qvterm_line_wrap;
emacs_value Qrear_nonsticky;
emacs_value Qvterm_prompt;

// Emacs functions
emacs_value Fsymbol_value;
emacs_value Flength;
emacs_value Flist;
emacs_value Fnth;
emacs_value Ferase_buffer;
emacs_value Finsert;
emacs_value Fgoto_char;
emacs_value Fforward_char;
emacs_value Fforward_line;
emacs_value Fgoto_line;
emacs_value Fdelete_lines;
emacs_value Frecenter;
emacs_value Fset_window_point;
emacs_value Fwindow_body_height;
emacs_value Fpoint;

emacs_value Fput_text_property;
emacs_value Fadd_text_properties;
emacs_value Fset;
emacs_value Fvterm_flush_output;
emacs_value Fget_buffer_window_list;
emacs_value Fselected_window;
emacs_value Fvterm_set_title;
emacs_value Fvterm_set_directory;
emacs_value Fvterm_invalidate;
emacs_value Feq;
emacs_value Fvterm_get_color;
emacs_value Fvterm_eval;

// Utils
void bind_function(emacs_env *env, const char *name, emacs_value Sfun);
void provide(emacs_env *env, const char *feature);
emacs_value symbol_value(emacs_env *env, emacs_value symbol);
int string_bytes(emacs_env *env, emacs_value string);
emacs_value length(emacs_env *env, emacs_value string);
emacs_value list(emacs_env *env, emacs_value elements[], ptrdiff_t len);
emacs_value nth(emacs_env *env, int idx, emacs_value list);
void put_text_property(emacs_env *env, emacs_value string, emacs_value property,
                       emacs_value value);
void add_text_properties(emacs_env *env, emacs_value string,
                         emacs_value property);
void erase_buffer(emacs_env *env);
void insert(emacs_env *env, emacs_value string);
void goto_char(emacs_env *env, int pos);
void forward_line(emacs_env *env, int n);
void goto_line(emacs_env *env, int n);
void set_cursor_type(emacs_env *env, emacs_value cursor_type);
void delete_lines(emacs_env *env, int linenum, int count, bool del_whole_line);
void recenter(emacs_env *env, emacs_value pos);
void set_window_point(emacs_env *env, emacs_value win, emacs_value point);
emacs_value window_body_height(emacs_env *env, emacs_value win);
emacs_value point(emacs_env *env);
bool eq(emacs_env *env, emacs_value e1, emacs_value e2);
void forward_char(emacs_env *env, emacs_value n);
emacs_value get_buffer_window_list(emacs_env *env);
emacs_value selected_window(emacs_env *env);
void set_title(emacs_env *env, emacs_value string);
void set_directory(emacs_env *env, emacs_value string);
void vterm_invalidate(emacs_env *env);
emacs_value vterm_get_color(emacs_env *env, int index);
emacs_value vterm_eval(emacs_env *env, emacs_value string);

#endif /* ELISP_H */
