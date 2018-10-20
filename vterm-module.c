#include "vterm-module.h"
#include "elisp.h"
#include "utf8.h"
#include <string.h>
#include <unistd.h>
#include <vterm.h>

static bool compare_cells(VTermScreenCell *a, VTermScreenCell *b) {
  bool equal = true;
  equal = equal && (a->fg.red == b->fg.red);
  equal = equal && (a->fg.green == b->fg.green);
  equal = equal && (a->fg.blue == b->fg.blue);
  equal = equal && (a->bg.red == b->bg.red);
  equal = equal && (a->bg.green == b->bg.green);
  equal = equal && (a->bg.blue == b->bg.blue);
  equal = equal && (a->attrs.bold == b->attrs.bold);
  equal = equal && (a->attrs.underline == b->attrs.underline);
  equal = equal && (a->attrs.italic == b->attrs.italic);
  equal = equal && (a->attrs.reverse == b->attrs.reverse);
  equal = equal && (a->attrs.strike == b->attrs.strike);
  return equal;
}

static bool is_key(unsigned char *key, size_t len, char *key_description) {
  return (len == strlen(key_description) &&
          memcmp(key, key_description, len) == 0);
}

static int set_term_prop_cb(VTermProp prop, VTermValue *val, void *user_data) {
  emacs_env *env = (emacs_env *)user_data;
  switch (prop) {
  case VTERM_PROP_CURSORVISIBLE:
    toggle_cursor(env, val->boolean);
    break;
  default:
    return 0;
  }

  return 1;
}

static emacs_value render_text(emacs_env *env, char *buffer, int len,
                               VTermScreenCell *cell) {
  emacs_value text = env->make_string(env, buffer, len);

  emacs_value foreground = color_to_rgb_string(env, cell->fg);
  emacs_value background = color_to_rgb_string(env, cell->bg);
  emacs_value bold = cell->attrs.bold ? Qbold : Qnormal;
  emacs_value underline = cell->attrs.underline ? Qt : Qnil;
  emacs_value italic = cell->attrs.italic ? Qitalic : Qnormal;
  emacs_value reverse = cell->attrs.reverse ? Qt : Qnil;
  emacs_value strike = cell->attrs.strike ? Qt : Qnil;

  // TODO: Blink, font, dwl, dhl is missing
  emacs_value properties =
      list(env,
           (emacs_value[]){Qforeground, foreground, Qbackground, background,
                           Qweight, bold, Qunderline, underline, Qslant, italic,
                           Qreverse, reverse, Qstrike, strike},
           14);

  put_text_property(env, text, Qface, properties);

  return text;
}

static void term_redraw(Term *term, emacs_env *env) {
  int i, j;
  int rows, cols;
  VTermScreen *screen = vterm_obtain_screen(term->vt);
  vterm_get_size(term->vt, &rows, &cols);

  erase_buffer(env);

  char buffer[((rows + 1) * cols) * 4];
  int length = 0;
  VTermScreenCell cell;
  VTermScreenCell lastCell;
  VTermPos first = {.row = 0, .col = 0};
  vterm_screen_get_cell(screen, first, &lastCell);

  int offset = 0;
  for (i = 0; i < rows; i++) {
    for (j = 0; j < cols; j++) {
      VTermPos pos = {.row = i, .col = j};
      vterm_screen_get_cell(screen, pos, &cell);

      if (!compare_cells(&cell, &lastCell)) {
        emacs_value text = render_text(env, buffer, length, &lastCell);
        insert(env, text);
        length = 0;
      }

      lastCell = cell;
      if (cell.chars[0] == 0) {
        buffer[length] = ' ';
        length++;
      } else {
        unsigned char bytes[4];
        size_t count = codepoint_to_utf8(cell.chars[0], bytes);
        for (int k = 0; k < count; k++) {
          buffer[length] = bytes[k];
          length++;
        }
      }

      if (cell.width > 1) {
        int w = cell.width - 1;
        offset += w;
        j = j + w;
      }
    }

    buffer[length] = '\n';
    length++;
  }
  emacs_value text = render_text(env, buffer, length, &lastCell);
  insert(env, text);

  VTermState *state = vterm_obtain_state(term->vt);
  VTermPos pos;
  vterm_state_get_cursorpos(state, &pos);
  term_put_caret(term, env, pos.row, pos.col, -offset);
}

static void term_setup_colors(Term *term, emacs_env *env) {
  VTermColor fg, bg;
  VTermState *state = vterm_obtain_state(term->vt);

  fg = rgb_string_to_color(env, get_hex_color_fg(env, Qterm));
  bg = rgb_string_to_color(env, get_hex_color_bg(env, Qterm));
  vterm_state_set_default_colors(state, &fg, &bg);

  fg = rgb_string_to_color(env, get_hex_color_fg(env, Qterm_color_black));
  vterm_state_set_palette_color(state, 0, &fg);
  bg = rgb_string_to_color(env, get_hex_color_bg(env, Qterm_color_black));
  vterm_state_set_palette_color(state, 8, &bg);

  fg = rgb_string_to_color(env, get_hex_color_fg(env, Qterm_color_red));
  vterm_state_set_palette_color(state, 1, &fg);
  bg = rgb_string_to_color(env, get_hex_color_bg(env, Qterm_color_red));
  vterm_state_set_palette_color(state, 9, &bg);

  fg = rgb_string_to_color(env, get_hex_color_fg(env, Qterm_color_green));
  vterm_state_set_palette_color(state, 2, &fg);
  bg = rgb_string_to_color(env, get_hex_color_bg(env, Qterm_color_green));
  vterm_state_set_palette_color(state, 10, &bg);

  fg = rgb_string_to_color(env, get_hex_color_fg(env, Qterm_color_yellow));
  vterm_state_set_palette_color(state, 3, &fg);
  bg = rgb_string_to_color(env, get_hex_color_bg(env, Qterm_color_yellow));
  vterm_state_set_palette_color(state, 11, &bg);

  fg = rgb_string_to_color(env, get_hex_color_fg(env, Qterm_color_blue));
  vterm_state_set_palette_color(state, 4, &fg);
  bg = rgb_string_to_color(env, get_hex_color_bg(env, Qterm_color_blue));
  vterm_state_set_palette_color(state, 12, &bg);

  fg = rgb_string_to_color(env, get_hex_color_fg(env, Qterm_color_magenta));
  vterm_state_set_palette_color(state, 5, &fg);
  bg = rgb_string_to_color(env, get_hex_color_bg(env, Qterm_color_magenta));
  vterm_state_set_palette_color(state, 13, &bg);

  fg = rgb_string_to_color(env, get_hex_color_fg(env, Qterm_color_cyan));
  vterm_state_set_palette_color(state, 6, &fg);
  bg = rgb_string_to_color(env, get_hex_color_bg(env, Qterm_color_cyan));
  vterm_state_set_palette_color(state, 14, &bg);

  fg = rgb_string_to_color(env, get_hex_color_fg(env, Qterm_color_white));
  vterm_state_set_palette_color(state, 7, &fg);
  bg = rgb_string_to_color(env, get_hex_color_bg(env, Qterm_color_white));
  vterm_state_set_palette_color(state, 15, &bg);
}

static void term_flush_output(Term *term, emacs_env *env) {
  size_t len = vterm_output_get_buffer_current(term->vt);
  if (len) {
    char buffer[len];
    len = vterm_output_read(term->vt, buffer, len);

    emacs_value output = env->make_string(env, buffer, len);
    env->funcall(env, Fvterm_flush_output, 1, (emacs_value[]){output});
  }
}

static void term_process_key(Term *term, unsigned char *key, size_t len,
                             VTermModifier modifier) {
  if (is_key(key, len, "<return>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_ENTER, modifier);
  } else if (is_key(key, len, "<tab>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_TAB, modifier);
  } else if (is_key(key, len, "<backspace>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_BACKSPACE, modifier);
  } else if (is_key(key, len, "<escape>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_ESCAPE, modifier);
  } else if (is_key(key, len, "<up>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_UP, modifier);
  } else if (is_key(key, len, "<down>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_DOWN, modifier);
  } else if (is_key(key, len, "<left>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_LEFT, modifier);
  } else if (is_key(key, len, "<right>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_RIGHT, modifier);
  } else if (is_key(key, len, "<insert>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_INS, modifier);
  } else if (is_key(key, len, "<delete>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_DEL, modifier);
  } else if (is_key(key, len, "<home>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_HOME, modifier);
  } else if (is_key(key, len, "<end>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_END, modifier);
  } else if (is_key(key, len, "<prior>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_PAGEUP, modifier);
  } else if (is_key(key, len, "<f0>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_FUNCTION(0), modifier);
  } else if (is_key(key, len, "<f1>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_FUNCTION(1), modifier);
  } else if (is_key(key, len, "<f2>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_FUNCTION(2), modifier);
  } else if (is_key(key, len, "<f3>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_FUNCTION(3), modifier);
  } else if (is_key(key, len, "<f4>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_FUNCTION(4), modifier);
  } else if (is_key(key, len, "<f5>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_FUNCTION(5), modifier);
  } else if (is_key(key, len, "<f6>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_FUNCTION(6), modifier);
  } else if (is_key(key, len, "<f7>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_FUNCTION(7), modifier);
  } else if (is_key(key, len, "<f8>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_FUNCTION(8), modifier);
  } else if (is_key(key, len, "<f9>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_FUNCTION(9), modifier);
  } else if (is_key(key, len, "<f10>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_FUNCTION(10), modifier);
  } else if (is_key(key, len, "<f11>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_FUNCTION(11), modifier);
  } else if (is_key(key, len, "<f12>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_FUNCTION(12), modifier);
  } else if (is_key(key, len, "SPC")) {
    vterm_keyboard_unichar(term->vt, ' ', modifier);
  } else if (len <= 4) {
    uint32_t codepoint;
    if (utf8_to_codepoint(key, len, &codepoint)) {
      vterm_keyboard_unichar(term->vt, codepoint, modifier);
    }
  }
}

static void term_put_caret(Term *term, emacs_env *env, int row, int col,
                           int offset) {
  int rows, cols;
  vterm_get_size(term->vt, &rows, &cols);
  // row * (cols + 1) because of newline character
  // col + 1 because (goto-char 1) sets point to first position
  int point = (row * (cols + 1)) + col + 1 + offset;
  goto_char(env, point);
}

static void term_finalize(void *object) {
  Term *term = (Term *)object;
  vterm_free(term->vt);
  free(term);
}

static emacs_value Fvterm_new(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data) {
  Term *term = malloc(sizeof(Term));

  int rows = env->extract_integer(env, args[0]);
  int cols = env->extract_integer(env, args[1]);

  term->vt = vterm_new(rows, cols);
  vterm_set_utf8(term->vt, 1);

  term_setup_colors(term, env);

  VTermScreen *screen = vterm_obtain_screen(term->vt);
  vterm_screen_reset(screen, 1);

  return env->make_user_ptr(env, term_finalize, term);
}

static emacs_value Fvterm_update(emacs_env *env, ptrdiff_t nargs,
                                 emacs_value args[], void *data) {
  Term *term = env->get_user_ptr(env, args[0]);

  // Process keys
  if (nargs > 1) {
    ptrdiff_t len = string_bytes(env, args[1]);
    unsigned char key[len];
    env->copy_string_contents(env, args[1], (char *)key, &len);
    VTermModifier modifier = VTERM_MOD_NONE;
    if (env->is_not_nil(env, args[2]))
      modifier = modifier | VTERM_MOD_SHIFT;
    if (env->is_not_nil(env, args[3]))
      modifier = modifier | VTERM_MOD_ALT;
    if (env->is_not_nil(env, args[4]))
      modifier = modifier | VTERM_MOD_CTRL;

    // Ignore the final zero byte
    term_process_key(term, key, len - 1, modifier);
  }

  // Flush output
  term_flush_output(term, env);

  term_redraw(term, env);

  return env->make_integer(env, 0);
}

static emacs_value Fvterm_write_input(emacs_env *env, ptrdiff_t nargs,
                                      emacs_value args[], void *data) {
  Term *term = env->get_user_ptr(env, args[0]);
  ptrdiff_t len = string_bytes(env, args[1]);
  char bytes[len];

  env->copy_string_contents(env, args[1], bytes, &len);

  vterm_input_write(term->vt, bytes, len);

  return env->make_integer(env, 0);
}

static emacs_value Fvterm_set_size(emacs_env *env, ptrdiff_t nargs,
                                   emacs_value args[], void *data) {
  Term *term = env->get_user_ptr(env, args[0]);
  int rows = env->extract_integer(env, args[1]);
  int cols = env->extract_integer(env, args[2]);

  int old_rows, old_cols;
  vterm_get_size(term->vt, &old_rows, &old_cols);

  if (cols != old_cols || rows != old_rows) {
    vterm_set_size(term->vt, rows, cols);
  }

  return Qnil;
}

int emacs_module_init(struct emacs_runtime *ert) {
  emacs_env *env = ert->get_environment(ert);

  // Symbols;
  Qt = env->make_global_ref(env, env->intern(env, "t"));
  Qnil = env->make_global_ref(env, env->intern(env, "nil"));
  Qnormal = env->make_global_ref(env, env->intern(env, "normal"));
  Qbold = env->make_global_ref(env, env->intern(env, "bold"));
  Qitalic = env->make_global_ref(env, env->intern(env, "italic"));
  Qforeground = env->make_global_ref(env, env->intern(env, ":foreground"));
  Qbackground = env->make_global_ref(env, env->intern(env, ":background"));
  Qweight = env->make_global_ref(env, env->intern(env, ":weight"));
  Qunderline = env->make_global_ref(env, env->intern(env, ":underline"));
  Qslant = env->make_global_ref(env, env->intern(env, ":slant"));
  Qreverse = env->make_global_ref(env, env->intern(env, ":inverse-video"));
  Qstrike = env->make_global_ref(env, env->intern(env, ":strike-through"));
  Qface = env->make_global_ref(env, env->intern(env, "font-lock-face"));
  Qcursor_type = env->make_global_ref(env, env->intern(env, "cursor-type"));

  // Functions
  Flength = env->make_global_ref(env, env->intern(env, "length"));
  Flist = env->make_global_ref(env, env->intern(env, "list"));
  Ferase_buffer = env->make_global_ref(env, env->intern(env, "erase-buffer"));
  Finsert = env->make_global_ref(env, env->intern(env, "insert"));
  Fgoto_char = env->make_global_ref(env, env->intern(env, "goto-char"));
  Fput_text_property = env->make_global_ref(env, env->intern(env, "put-text-property"));
  Fset = env->make_global_ref(env, env->intern(env, "set"));
  Fvterm_face_color_hex = env->make_global_ref(env, env->intern(env, "vterm--face-color-hex"));
  Fvterm_flush_output = env->make_global_ref(env, env->intern(env, "vterm--flush-output"));

  // Faces
  Qterm = env->make_global_ref(env, env->intern(env, "vterm"));
  Qterm_color_black = env->make_global_ref(env, env->intern(env, "vterm-color-black"));
  Qterm_color_red = env->make_global_ref(env, env->intern(env, "vterm-color-red"));
  Qterm_color_green = env->make_global_ref(env, env->intern(env, "vterm-color-green"));
  Qterm_color_yellow = env->make_global_ref(env, env->intern(env, "vterm-color-yellow"));
  Qterm_color_blue = env->make_global_ref(env, env->intern(env, "vterm-color-blue"));
  Qterm_color_magenta = env->make_global_ref(env, env->intern(env, "vterm-color-magenta"));
  Qterm_color_cyan = env->make_global_ref(env, env->intern(env, "vterm-color-cyan"));
  Qterm_color_white = env->make_global_ref(env, env->intern(env, "vterm-color-white"));

  // Exported functions
  emacs_value fun;
  fun =
      env->make_function(env, 2, 2, Fvterm_new, "Allocates a new vterm.", NULL);
  bind_function(env, "vterm--new", fun);

  fun = env->make_function(env, 1, 5, Fvterm_update,
                           "Process io and update the screen.", NULL);
  bind_function(env, "vterm--update", fun);

  fun = env->make_function(env, 2, 2, Fvterm_write_input,
                           "Write input to vterm.", NULL);
  bind_function(env, "vterm--write-input", fun);

  fun = env->make_function(env, 3, 3, Fvterm_set_size,
                           "Sets the size of the terminal.", NULL);
  bind_function(env, "vterm--set-size", fun);

  provide(env, "vterm-module");

  return 0;
}
