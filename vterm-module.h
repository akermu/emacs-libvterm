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

typedef struct LineInfo {
  char *directory; /* working directory */

  int prompt_col; /* end column of the prompt, if the current line contains the
                   * prompt */
} LineInfo;

typedef struct ScrollbackLine {
  size_t cols;
  LineInfo *info;
  VTermScreenCell cells[];
} ScrollbackLine;

typedef struct ElispCodeListNode {
  char *code;
  size_t code_len;
  struct ElispCodeListNode *next;
} ElispCodeListNode;

/*  c , p , q , s , 0 , 1 , 2 , 3 , 4 , 5 , 6 , and 7  */
/* clipboard, primary, secondary, select, or cut buffers 0 through 7 */
#define SELECTION_TARGET_MAX 12

typedef struct Cursor {
  int row, col;
  int cursor_type;
  bool cursor_visible;
  bool cursor_blink;
  bool cursor_type_changed;
  bool cursor_blink_changed;
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
  int sb_pending_by_height_decr;
  long linenum;
  long linenum_added;

  int invalid_start, invalid_end; // invalid rows in libvterm screen
  bool is_invalidated;

  Cursor cursor;
  char *title;
  bool title_changed;

  char *directory;
  bool directory_changed;

  // Single-linked list of elisp_code.
  // Newer commands are added at the tail.
  ElispCodeListNode *elisp_code_first;
  ElispCodeListNode **elisp_code_p_insert; // pointer to the position where new
                                           // node should be inserted

  /*  c , p , q , s , 0 , 1 , 2 , 3 , 4 , 5 , 6 , and 7  */
  /* clipboard, primary, secondary, select, or cut buffers 0 through 7 */
  char selection_target[SELECTION_TARGET_MAX];
  char *selection_data;

  /* the size of dirs almost = window height, value = directory of that line */
  LineInfo **lines;
  int lines_len;

  int width, height;
  int height_resize;
  bool resizing;
  bool disable_bold_font;
  bool disable_underline;
  bool disable_inverse_video;
  bool ignore_blink_cursor;

  char *cmd_buffer;

  int pty_fd;
} Term;

static bool compare_cells(VTermScreenCell *a, VTermScreenCell *b);
static bool is_key(unsigned char *key, size_t len, char *key_description);
static emacs_value render_text(emacs_env *env, Term *term, char *string,
                               int len, VTermScreenCell *cell);
static emacs_value render_fake_newline(emacs_env *env, Term *term);
static emacs_value render_prompt(emacs_env *env, emacs_value text);
static emacs_value cell_rgb_color(emacs_env *env, Term *term,
                                  VTermScreenCell *cell, bool is_foreground);

static int term_settermprop(VTermProp prop, VTermValue *val, void *user_data);

static void term_redraw(Term *term, emacs_env *env);
static void term_flush_output(Term *term, emacs_env *env);
static void term_process_key(Term *term, emacs_env *env, unsigned char *key,
                             size_t len, VTermModifier modifier);
static void invalidate_terminal(Term *term, int start_row, int end_row);

void term_finalize(void *object);

emacs_value Fvterm_new(emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                       void *data);
emacs_value Fvterm_update(emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                          void *data);
emacs_value Fvterm_redraw(emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                          void *data);
emacs_value Fvterm_write_input(emacs_env *env, ptrdiff_t nargs,
                               emacs_value args[], void *data);
emacs_value Fvterm_set_size(emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                            void *data);
emacs_value Fvterm_set_pty_name(emacs_env *env, ptrdiff_t nargs,
                                emacs_value args[], void *data);
emacs_value Fvterm_get_icrnl(emacs_env *env, ptrdiff_t nargs,
                             emacs_value args[], void *data);

emacs_value Fvterm_get_pwd(emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                           void *data);

emacs_value Fvterm_get_prompt_point(emacs_env *env, ptrdiff_t nargs,
                                    emacs_value args[], void *data);
emacs_value Fvterm_reset_cursor_point(emacs_env *env, ptrdiff_t nargs,
                                      emacs_value args[], void *data);

int emacs_module_init(struct emacs_runtime *ert);

#endif /* VTERM_MODULE_H */
