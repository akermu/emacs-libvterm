#include "vterm-module.h"
#include "elisp.h"
#include <fcntl.h>
#ifdef __APPLE__
#include <util.h>
#else
#include <pty.h>
#endif
#include "utf8.h"
#include <pthread.h>
#include <signal.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/select.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <termios.h>
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

static void term_redraw(struct Term *term, emacs_env *env) {
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

static void term_setup_colors(struct Term *term, emacs_env *env) {
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

static void term_flush_output(struct Term *term) {
  size_t bufflen = vterm_output_get_buffer_current(term->vt);
  if (bufflen) {
    char buffer[bufflen];
    bufflen = vterm_output_read(term->vt, buffer, bufflen);

    // TODO: Make work with NON-Blocking io. (buffer in term)
    fcntl(term->masterfd, F_SETFL,
          fcntl(term->masterfd, F_GETFL) & ~O_NONBLOCK);
    write(term->masterfd, buffer, bufflen);
    fcntl(term->masterfd, F_SETFL, fcntl(term->masterfd, F_GETFL) | O_NONBLOCK);
  }
}

static void term_process_key(struct Term *term, unsigned char *key, size_t len,
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

static void term_put_caret(struct Term *term, emacs_env *env, int row, int col,
                           int offset) {
  int rows, cols;
  vterm_get_size(term->vt, &rows, &cols);
  // row * (cols + 1) because of newline character
  // col + 1 because (goto-char 1) sets point to first position
  int point = (row * (cols + 1)) + col + 1 + offset;
  goto_char(env, point);
}

static void term_finalize(void *object) {
  struct Term *term = (struct Term *)object;
  pthread_cancel(term->thread);
  pthread_join(term->thread, NULL);
  vterm_free(term->vt);
  free(term);
}

static emacs_value Fvterm_new(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data) {
  struct Term *term = malloc(sizeof(struct Term));

  ptrdiff_t len = string_bytes(env, args[0]);
  char shell[len];
  env->copy_string_contents(env, args[0], shell, &len);

  int rows = env->extract_integer(env, args[1]);
  int cols = env->extract_integer(env, args[2]);

  struct winsize size = {rows, cols, 0, 0};

  // Taken almost verbatim from https://bazaar.launchpad.net/~leonerd/pangoterm
  struct termios termios = {
      .c_iflag = ICRNL | IXON,
      .c_oflag = OPOST | ONLCR,
      .c_cflag = CS8 | CREAD,
      .c_lflag = ISIG | ICANON | IEXTEN | ECHO | ECHOE | ECHOK,
      /* c_cc later */
  };

  termios.c_iflag |= IUTF8;
  termios.c_oflag |= NL0;
  termios.c_oflag |= CR0;
  termios.c_oflag |= BS0;
  termios.c_oflag |= VT0;
  termios.c_oflag |= FF0;
  termios.c_lflag |= ECHOCTL;
  termios.c_lflag |= ECHOKE;

  cfsetspeed(&termios, 38400);

  termios.c_cc[VINTR] = 0x1f & 'C';
  termios.c_cc[VQUIT] = 0x1f & '\\';
  termios.c_cc[VERASE] = 0x7f;
  termios.c_cc[VKILL] = 0x1f & 'U';
  termios.c_cc[VEOF] = 0x1f & 'D';
  termios.c_cc[VEOL] = _POSIX_VDISABLE;
  termios.c_cc[VEOL2] = _POSIX_VDISABLE;
  termios.c_cc[VSTART] = 0x1f & 'Q';
  termios.c_cc[VSTOP] = 0x1f & 'S';
  termios.c_cc[VSUSP] = 0x1f & 'Z';
  termios.c_cc[VREPRINT] = 0x1f & 'R';
  termios.c_cc[VWERASE] = 0x1f & 'W';
  termios.c_cc[VLNEXT] = 0x1f & 'V';
  termios.c_cc[VMIN] = 1;
  termios.c_cc[VTIME] = 0;

  term->pid = forkpty(&term->masterfd, NULL, &termios, &size);
  fcntl(term->masterfd, F_SETFL, fcntl(term->masterfd, F_GETFL) | O_NONBLOCK);

  if (term->pid == 0) {
    setenv("TERM", "xterm", 1);
    char *args[2] = {shell, NULL};
    execvp(shell, args);
    exit(1);
  }

  term->vt = vterm_new(rows, cols);
  vterm_set_utf8(term->vt, 1);

  term_setup_colors(term, env);

  VTermScreen *screen = vterm_obtain_screen(term->vt);
  vterm_screen_reset(screen, 1);

  pthread_create(&term->thread, NULL, &event_loop, term);

  return env->make_user_ptr(env, term_finalize, term);
}

static emacs_value Fvterm_update(emacs_env *env, ptrdiff_t nargs,
                                 emacs_value args[], void *data) {
  struct Term *term = env->get_user_ptr(env, args[0]);

  // Check if exited
  int status;
  if (waitpid(term->pid, &status, WNOHANG) > 0) {
    return Qnil;
  }

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

    // Flush output
    term_flush_output(term);
  }

  VTermScreenCallbacks cb = {
      .settermprop = set_term_prop_cb,
  };
  VTermScreen *screen = vterm_obtain_screen(term->vt);
  vterm_screen_set_callbacks(screen, &cb, env);

  // Read input from masterfd
  char bytes[4096];
  int len;
  struct timeval start, end;

  gettimeofday(&start, NULL);
  while ((len = read(term->masterfd, bytes, 4096)) > 0) {
    vterm_input_write(term->vt, bytes, len);

    // Break after 40 milliseconds
    gettimeofday(&end, NULL);
    if (end.tv_usec - start.tv_usec > 40000) {
      break;
    }
  }
  vterm_screen_set_callbacks(screen, NULL, NULL);

  term_redraw(term, env);

  return env->make_integer(env, 0);
}

static emacs_value Fvterm_kill(emacs_env *env, ptrdiff_t nargs,
                               emacs_value args[], void *data) {
  struct Term *term = env->get_user_ptr(env, args[0]);
  kill(term->pid, SIGKILL);
  int status;
  waitpid(term->pid, &status, 0);
  return Qnil;
}

static emacs_value Fvterm_set_size(emacs_env *env, ptrdiff_t nargs,
                                   emacs_value args[], void *data) {
  struct Term *term = env->get_user_ptr(env, args[0]);
  int rows = env->extract_integer(env, args[1]);
  int cols = env->extract_integer(env, args[2]);

  int old_rows, old_cols;
  vterm_get_size(term->vt, &old_rows, &old_cols);

  if (cols != old_cols || rows != old_rows) {
    struct winsize size = {rows, cols, 0, 0};
    ioctl(term->masterfd, TIOCSWINSZ, &size);
    vterm_set_size(term->vt, rows, cols);
  }

  return Qnil;
}

static void *event_loop(void *arg) {
  struct Term *term = arg;
  fd_set rfds;

  while (1) {
    FD_ZERO(&rfds);
    FD_SET(term->masterfd, &rfds);
    if (select(term->masterfd + 1, &rfds, NULL, NULL, NULL) == 1) {
      kill(getpid(), SIGUSR1);
      usleep(20000);
    }
  }

  return NULL;
}

int emacs_module_init(struct emacs_runtime *ert) {
  emacs_env *env = ert->get_environment(ert);

  // Symbols;
  Qt = env->intern(env, "t");
  Qnil = env->intern(env, "nil");
  Qnormal = env->intern(env, "normal");
  Qbold = env->intern(env, "bold");
  Qitalic = env->intern(env, "italic");
  Qforeground = env->intern(env, ":foreground");
  Qbackground = env->intern(env, ":background");
  Qweight = env->intern(env, ":weight");
  Qunderline = env->intern(env, ":underline");
  Qslant = env->intern(env, ":slant");
  Qreverse = env->intern(env, ":inverse-video");
  Qstrike = env->intern(env, ":strike-through");
  Qface = env->intern(env, "font-lock-face");
  Qcursor_type = env->intern(env, "cursor-type");

  // Functions
  Flength = env->intern(env, "length");
  Flist = env->intern(env, "list");
  Ferase_buffer = env->intern(env, "erase-buffer");
  Finsert = env->intern(env, "insert");
  Fgoto_char = env->intern(env, "goto-char");
  Fput_text_property = env->intern(env, "put-text-property");
  Fset = env->intern(env, "set");
  Fvterm_face_color_hex = env->intern(env, "vterm--face-color-hex");

  // Faces
  Qterm = env->intern(env, "vterm");
  Qterm_color_black = env->intern(env, "vterm-color-black");
  Qterm_color_red = env->intern(env, "vterm-color-red");
  Qterm_color_green = env->intern(env, "vterm-color-green");
  Qterm_color_yellow = env->intern(env, "vterm-color-yellow");
  Qterm_color_blue = env->intern(env, "vterm-color-blue");
  Qterm_color_magenta = env->intern(env, "vterm-color-magenta");
  Qterm_color_cyan = env->intern(env, "vterm-color-cyan");
  Qterm_color_white = env->intern(env, "vterm-color-white");

  // Exported functions
  emacs_value fun;
  fun =
      env->make_function(env, 3, 3, Fvterm_new, "Allocates a new vterm.", NULL);
  bind_function(env, "vterm-new", fun);

  fun = env->make_function(env, 1, 5, Fvterm_update,
                           "Process io and update the screen.", NULL);
  bind_function(env, "vterm-update", fun);

  fun = env->make_function(env, 1, 1, Fvterm_kill,
                           "Kill the the shell process.", NULL);
  bind_function(env, "vterm-kill", fun);

  fun = env->make_function(env, 3, 3, Fvterm_set_size,
                           "Sets the size of the terminal.", NULL);
  bind_function(env, "vterm-set-size", fun);

  provide(env, "vterm-module");

  return 0;
}
