#include <emacs-module.h>
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
emacs_value Qface;
emacs_value Qcursor_type;

// Emacs functions
emacs_value Flength;
emacs_value Flist;
emacs_value Ferase_buffer;
emacs_value Finsert;
emacs_value Fgoto_char;
emacs_value Fput_text_property;
emacs_value Fset;

// Utils
void bind_function(emacs_env *env, const char *name, emacs_value Sfun);
void provide(emacs_env *env, const char *feature);
int string_bytes(emacs_env *env, emacs_value string);
emacs_value string_length(emacs_env *env, emacs_value string);
emacs_value list(emacs_env *env, emacs_value *elements, ptrdiff_t len);
void put_text_property(emacs_env *env, emacs_value string, emacs_value property,
                       emacs_value value);
emacs_value render_text(emacs_env *env, char *string, int len,
                        VTermScreenCell *cell);
void byte_to_hex(uint8_t byte, char *hex);
emacs_value color_to_rgb_string(emacs_env *env, VTermColor color);
void erase_buffer(emacs_env *env);
void insert(emacs_env *env, emacs_value string);
void goto_char(emacs_env *env, int pos);
void toggle_cursor(emacs_env *env, bool visible);
