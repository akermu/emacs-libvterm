#include <emacs-module.h>
#include <inttypes.h>
#include <semaphore.h>
#include <stdbool.h>
#include <vterm.h>

int plugin_is_GPL_compatible;

struct Term {
  VTerm *vt;
  int masterfd;
  pid_t pid;
  pthread_t thread;
};

// Emacs symbols
static emacs_value Qt;
static emacs_value Qnil;
static emacs_value Qnormal;
static emacs_value Qbold;
static emacs_value Qitalic;
static emacs_value Qforeground;
static emacs_value Qbackground;
static emacs_value Qweight;
static emacs_value Qunderline;
static emacs_value Qslant;
static emacs_value Qreverse;
static emacs_value Qstrike;
static emacs_value Qface;

// Emacs functions
static emacs_value Flength;
static emacs_value Flist;
static emacs_value Ferase_buffer;
static emacs_value Finsert;
static emacs_value Fgoto_char;

static size_t codepoint_to_utf8(const uint32_t codepoint,
                                unsigned char buffer[4]);
static bool utf8_to_codepoint(const unsigned char buffer[4], const size_t len,
                              uint32_t *codepoint);
static void bind_function(emacs_env *env, const char *name, emacs_value Sfun);
static void provide(emacs_env *env, const char *feature);
static void message(emacs_env *env, char *message);
static void message_value(emacs_env *env, emacs_value value);
static int string_bytes(emacs_env *env, emacs_value string);
static emacs_value string_length(emacs_env *env, emacs_value string);
static emacs_value list(emacs_env *env, emacs_value *elements, ptrdiff_t len);
static void put_text_property(emacs_env *env, emacs_value string,
                              emacs_value property, emacs_value value);
static emacs_value render_text(emacs_env *env, char *string, int len,
                               VTermScreenCell *cell);
static void byte_to_hex(uint8_t byte, char *hex);
static emacs_value color_to_rgb_string(emacs_env *env, VTermColor color);
static void erase_buffer(emacs_env *env);
static void insert(emacs_env *env, emacs_value string);
static void goto_char(emacs_env *env, int pos);

static void vterm_redraw(VTerm *vt, emacs_env *env);
static void vterm_flush_output(struct Term *term);
static void term_finalize(void *object);
static emacs_value Fvterm_new(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data);
static void *event_loop(void *arg);
static void process_key(struct Term *term, unsigned char *key, size_t len,
                        VTermModifier modifier);
static emacs_value Fvterm_update(emacs_env *env, ptrdiff_t nargs,
                                 emacs_value args[], void *data);
static emacs_value Fvterm_kill(emacs_env *env, ptrdiff_t nargs,
                               emacs_value args[], void *data);
static emacs_value Fvterm_set_size(emacs_env *env, ptrdiff_t nargs,
                                   emacs_value args[], void *data);
int emacs_module_init(struct emacs_runtime *ert);
