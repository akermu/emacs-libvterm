#ifndef VTERM_MODULE_H
#define VTERM_MODULE_H

#include "emacs-module.h"
#include <inttypes.h>
#include <stdbool.h>
#include <vterm.h>

int plugin_is_GPL_compatible;


#define SB_MAX 100000 // Maximum 'scrollback' value.

#ifndef MIN
#define MIN(X, Y) ((X) < (Y) ? (X) : (Y))
#endif
#ifndef MAX
#define MAX(X, Y) ((X) > (Y) ? (X) : (Y))
#endif

typedef struct ScrollbackLine {
  size_t cols;
  VTermScreenCell cells[];
} ScrollbackLine;

typedef struct Cursor {
  int row, col;
  bool blinking;
  bool visible;
} Cursor;

typedef struct Term {
  VTerm *vt;
  VTermScreen *vts;
  // buffer used to:
  //  - convert VTermScreen cell arrays into utf8 strings
  //  - receive data from libvterm as a result of key presses.
  ScrollbackLine **sb_buffer; // Scrollback buffer storage for libvterm
  size_t sb_current;          // number of rows pushed to sb_buffer
  size_t sb_size;             // sb_buffer size
  // "virtual index" that points to the first sb_buffer row that we need to
  // push to the terminal buffer when refreshing the scrollback. When negative,
  // it actually points to entries that are no longer in sb_buffer (because the
  // window height has increased) and must be deleted from the terminal buffer
  int sb_pending;

  int invalid_start, invalid_end; // invalid rows in libvterm screen
  bool is_invalidated;

  Cursor cursor;
  char *title;
  bool is_title_changed;

  int width, height;
} Term;

static bool compare_cells(VTermScreenCell *a, VTermScreenCell *b);
static bool is_key(unsigned char *key, size_t len, char *key_description);
static emacs_value render_text(emacs_env *env, Term *term, char *string, int len,
                               VTermScreenCell *cell);
static emacs_value cell_to_face(emacs_env *env, Term *term, VTermScreenCell *cell);
static emacs_value color_to_face(emacs_env *env, VTermColor *color, emacs_value palette);
static emacs_value color_to_rgb_string(emacs_env *env, Term *term, VTermColor *color);

static int term_settermprop(VTermProp prop, VTermValue *val, void *user_data);

static void term_redraw(Term *term, emacs_env *env);
static void term_flush_output(Term *term, emacs_env *env);
static void term_process_key(Term *term, unsigned char *key, size_t len,
                             VTermModifier modifier);
static void term_put_caret(Term *term, emacs_env *env, int row, int col,
                           int offset);
static void invalidate_terminal(Term *term, int start_row, int end_row);
static void refresh_size(Term *term);
static void term_finalize(void *object);

static emacs_value Fvterm_new(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data);
static emacs_value Fvterm_update(emacs_env *env, ptrdiff_t nargs,
                                 emacs_value args[], void *data);
static emacs_value Fvterm_redraw(emacs_env *env, ptrdiff_t nargs,
                                 emacs_value args[], void *data);
int emacs_module_init(struct emacs_runtime *ert);

#endif /* VTERM_MODULE_H */
