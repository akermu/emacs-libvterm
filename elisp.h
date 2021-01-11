#ifndef ELISP_H
#define ELISP_H

#include "emacs-module.h"
#include "vterm.h"

// Emacs symbols
extern emacs_value Qt;
extern emacs_value Qnil;
extern emacs_value Qnormal;
extern emacs_value Qbold;
extern emacs_value Qitalic;
extern emacs_value Qforeground;
extern emacs_value Qbackground;
extern emacs_value Qweight;
extern emacs_value Qunderline;
extern emacs_value Qslant;
extern emacs_value Qreverse;
extern emacs_value Qstrike;
extern emacs_value Qextend;
extern emacs_value Qface;
extern emacs_value Qbox;
extern emacs_value Qbar;
extern emacs_value Qhbar;
extern emacs_value Qcursor_type;
extern emacs_value Qemacs_major_version;
extern emacs_value Qvterm_line_wrap;
extern emacs_value Qrear_nonsticky;
extern emacs_value Qvterm_prompt;

// Emacs functions
extern emacs_value Fblink_cursor_mode;
extern emacs_value Fsymbol_value;
extern emacs_value Flength;
extern emacs_value Flist;
extern emacs_value Fnth;
extern emacs_value Ferase_buffer;
extern emacs_value Finsert;
extern emacs_value Fgoto_char;
extern emacs_value Fforward_char;
extern emacs_value Fforward_line;
extern emacs_value Fgoto_line;
extern emacs_value Fdelete_lines;
extern emacs_value Frecenter;
extern emacs_value Fset_window_point;
extern emacs_value Fwindow_body_height;
extern emacs_value Fpoint;

extern emacs_value Fput_text_property;
extern emacs_value Fadd_text_properties;
extern emacs_value Fset;
extern emacs_value Fvterm_flush_output;
extern emacs_value Fget_buffer_window_list;
extern emacs_value Fselected_window;
extern emacs_value Fvterm_set_title;
extern emacs_value Fvterm_set_directory;
extern emacs_value Fvterm_invalidate;
extern emacs_value Feq;
extern emacs_value Fvterm_get_color;
extern emacs_value Fvterm_eval;
extern emacs_value Fvterm_selection;

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
void set_cursor_blink(emacs_env *env, bool blink);
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
emacs_value vterm_selection(emacs_env *env, emacs_value selection_target,
                            emacs_value selection_data);

#endif /* ELISP_H */
