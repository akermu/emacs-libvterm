#include <emacs-module.h>
#include <inttypes.h>
#include <stdbool.h>
#include <vterm.h>

int plugin_is_GPL_compatible;

struct Term {
  VTerm *vt;
  int masterfd;
  pid_t pid;
  pthread_t thread;
};

static bool compare_cells(VTermScreenCell *a, VTermScreenCell *b);

static void term_redraw(struct Term *term, emacs_env *env);
static void term_flush_output(struct Term *term);
static void term_process_key(struct Term *term, unsigned char *key, size_t len,
                             VTermModifier modifier);
static void term_put_caret(struct Term *term, emacs_env *env, int row, int col,
                           int offset);
static void term_finalize(void *object);

static emacs_value Fvterm_new(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data);
static emacs_value Fvterm_update(emacs_env *env, ptrdiff_t nargs,
                                 emacs_value args[], void *data);
static emacs_value Fvterm_kill(emacs_env *env, ptrdiff_t nargs,
                               emacs_value args[], void *data);
static emacs_value Fvterm_set_size(emacs_env *env, ptrdiff_t nargs,
                                   emacs_value args[], void *data);

static void *event_loop(void *arg);
int emacs_module_init(struct emacs_runtime *ert);
