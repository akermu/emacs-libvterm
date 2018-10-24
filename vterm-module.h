#ifndef VTERM_MODULE_H
#define VTERM_MODULE_H

#include "emacs-module.h"
#include <inttypes.h>
#include <stdbool.h>
#include <vterm.h>

int plugin_is_GPL_compatible;

struct Term {
  VTerm *vt;
};

// Faces
emacs_value Qterm;
emacs_value Qterm_color_black;
emacs_value Qterm_color_red;
emacs_value Qterm_color_green;
emacs_value Qterm_color_yellow;
emacs_value Qterm_color_blue;
emacs_value Qterm_color_magenta;
emacs_value Qterm_color_cyan;
emacs_value Qterm_color_white;

static bool compare_cells(VTermScreenCell *a, VTermScreenCell *b);
static bool is_key(unsigned char *key, size_t len, char *key_description);
static emacs_value render_text(emacs_env *env, char *string, int len,
                               VTermScreenCell *cell);

static int set_term_prop_cb(VTermProp prop, VTermValue *val, void *user_data);

static void term_redraw(struct Term *term, emacs_env *env);
static void term_setup_colors(struct Term *term, emacs_env *env);
static void term_flush_output(struct Term *term, emacs_env *env);
static void term_process_key(struct Term *term, unsigned char *key, size_t len,
                             VTermModifier modifier);
static void term_put_caret(struct Term *term, emacs_env *env, int row, int col,
                           int offset);
static void term_finalize(void *object);

static emacs_value Fvterm_new(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data);
static emacs_value Fvterm_update(emacs_env *env, ptrdiff_t nargs,
                                 emacs_value args[], void *data);
int emacs_module_init(struct emacs_runtime *ert);

#endif /* VTERM_MODULE_H */
