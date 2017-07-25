#include <emacs-module.h>
#include <vterm.h>

int plugin_is_GPL_compatible;

struct Term {
  VTerm *vt;
  int masterfd;
};

static void bind_function(emacs_env *env, const char *name, emacs_value Sfun);
static void provide(emacs_env *env, const char *feature);
static void message(emacs_env *env, char *message);
static void message_value(emacs_env *env, emacs_value value);
static int string_bytes(emacs_env *env, emacs_value string);
static emacs_value string_length(emacs_env *env, emacs_value string);
static emacs_value list(emacs_env *env, emacs_value *elements, ptrdiff_t len);
static emacs_value propertize(emacs_env *env, emacs_value string,
                              emacs_value prop, emacs_value properties);
static emacs_value color_text(emacs_env *env, emacs_value string,
                              emacs_value fg, emacs_value bg);
static void byte_to_hex(uint8_t byte, char *hex);
static emacs_value color_to_rgb_string(emacs_env *env, VTermColor color);
static void erase_buffer(emacs_env *env);
static void insert(emacs_env *env, emacs_value string);
static void goto_char(emacs_env *env, int pos);

static void vterm_redraw(VTerm *vt, emacs_env *env);
static void vterm_flush_output(struct Term *term);
static void term_finalize(void *term);
static emacs_value Fvterm_new(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data);
static void process_key(struct Term *term, char *key, VTermModifier modifier);
static emacs_value Fvterm_update(emacs_env *env, ptrdiff_t nargs,
                                 emacs_value args[], void *data);
int emacs_module_init(struct emacs_runtime *ert);
