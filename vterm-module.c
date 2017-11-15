#include "vterm-module.h"
#include <fcntl.h>
#ifdef __APPLE__
#  include <util.h>
#else
#  include <pty.h>
#endif
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/select.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <termios.h>
#include <unistd.h>
#include <vterm.h>

#define MAX(x, y) (((x) > (y)) ? (x) : (y))

static size_t codepoint_to_utf8(const uint32_t codepoint,
                                unsigned char buffer[4]) {
  if (codepoint <= 0x7F) {
    buffer[0] = codepoint;
    return 1;
  }
  if (codepoint >= 0x80 && codepoint <= 0x07FF) {
    buffer[0] = 0xC0 | (codepoint >> 6);
    buffer[1] = 0x80 | (codepoint & 0x3F);
    return 2;
  }
  if (codepoint >= 0x0800 && codepoint <= 0xFFFF) {
    buffer[0] = 0xE0 | (codepoint >> 12);
    buffer[1] = 0x80 | ((codepoint >> 6) & 0x3F);
    buffer[2] = 0x80 | (codepoint & 0x3F);
    return 3;
  }

  if (codepoint >= 0x10000 && codepoint <= 0x10FFFF) {
    buffer[0] = 0xF0 | (codepoint >> 18);
    buffer[1] = 0x80 | ((codepoint >> 12) & 0x3F);
    buffer[2] = 0x80 | ((codepoint >> 6) & 0x3F);
    buffer[3] = 0x80 | (codepoint & 0x3F);
    return 4;
  }
  return 0;
}

static bool utf8_to_codepoint(const unsigned char buffer[4], const size_t len,
                              uint32_t *codepoint) {
  *codepoint = 0;
  if (len == 1 && buffer[0] <= 0x7F) {
    *codepoint = buffer[0];
    return true;
  }
  if (len == 2 && (buffer[0] >= 0xC0 && buffer[0] <= 0xDF) &&
      (buffer[1] >= 0x80 && buffer[1] <= 0xBF)) {
    *codepoint = buffer[0] & 0x1F;
    *codepoint = *codepoint << 6;
    *codepoint = *codepoint | (buffer[1] & 0x3F);
    return true;
  }
  if (len == 3 && (buffer[0] >= 0xE0 && buffer[0] <= 0xEF) &&
      (buffer[1] >= 0x80 && buffer[1] <= 0xBF) &&
      (buffer[2] >= 0x80 && buffer[2] <= 0xBF)) {
    *codepoint = buffer[0] & 0xF;
    *codepoint = *codepoint << 6;
    *codepoint = *codepoint | (buffer[1] & 0x3F);
    *codepoint = *codepoint << 6;
    *codepoint = *codepoint | (buffer[2] & 0x3F);
    return true;
  }
  if (len == 4 && (buffer[0] >= 0xF0 && buffer[0] <= 0xF7) &&
      (buffer[1] >= 0x80 && buffer[1] <= 0xBF) &&
      (buffer[2] >= 0x80 && buffer[2] <= 0xBF) &&
      (buffer[3] >= 0x80 && buffer[3] <= 0xBF)) {
    *codepoint = buffer[0] & 7;
    *codepoint = *codepoint << 6;
    *codepoint = *codepoint | (buffer[1] & 0x3F);
    *codepoint = *codepoint << 6;
    *codepoint = *codepoint | (buffer[2] & 0x3F);
    *codepoint = *codepoint << 6;
    *codepoint = *codepoint | (buffer[3] & 0x3F);
    return true;
  }

  return false;
}

/* Bind NAME to FUN.  */
static void bind_function(emacs_env *env, const char *name, emacs_value Sfun) {
  /* Set the function cell of the symbol named NAME to SFUN using
     the 'fset' function.  */

  /* Convert the strings to symbols by interning them */
  emacs_value Qfset = env->intern(env, "fset");
  emacs_value Qsym = env->intern(env, name);

  /* Prepare the arguments array */
  emacs_value args[] = {Qsym, Sfun};

  /* Make the call (2 == nb of arguments) */
  env->funcall(env, Qfset, 2, args);
}

/* Provide FEATURE to Emacs.  */
static void provide(emacs_env *env, const char *feature) {
  /* call 'provide' with FEATURE converted to a symbol */

  emacs_value Qfeat = env->intern(env, feature);
  emacs_value Qprovide = env->intern(env, "provide");
  emacs_value args[] = {Qfeat};

  env->funcall(env, Qprovide, 1, args);
}

static int string_bytes(emacs_env *env, emacs_value string) {
  ptrdiff_t size = 0;
  env->copy_string_contents(env, string, NULL, &size);
  return size;
}

static emacs_value string_length(emacs_env *env, emacs_value string) {
  return env->funcall(env, Flength, 1, (emacs_value[]){string});
}

static emacs_value list(emacs_env *env, emacs_value *elements, ptrdiff_t len) {
  return env->funcall(env, Flist, len, elements);
}

static void put_text_property(emacs_env *env, emacs_value string,
                              emacs_value property, emacs_value value) {
  emacs_value start = env->make_integer(env, 0);
  emacs_value end = string_length(env, string);

  env->funcall(env, Fput_text_property, 5,
               (emacs_value[]){start, end, property, value, string});
}

/*
 * Color must be a string #RGB
 */
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
  emacs_value properties = list(
      env, (emacs_value[]){Qforeground, foreground, Qbackground, background,
                           Qweight, bold, Qunderline, underline, Qslant, italic,
                           Qreverse, reverse, Qstrike, strike},
      14);

  put_text_property(env, text, Qface, properties);

  return text;
}

static void byte_to_hex(uint8_t byte, char *hex) {
  snprintf(hex, 3, "%.2X", byte);
}

static emacs_value color_to_rgb_string(emacs_env *env, VTermColor color) {
  char buffer[8];
  buffer[0] = '#';
  buffer[7] = '\0';
  byte_to_hex(color.red, buffer + 1);
  byte_to_hex(color.green, buffer + 3);
  byte_to_hex(color.blue, buffer + 5);

  return env->make_string(env, buffer, 7);
};

static void erase_buffer(emacs_env *env) {
  env->funcall(env, Ferase_buffer, 0, NULL);
}

static void insert(emacs_env *env, emacs_value string) {
  env->funcall(env, Finsert, 1, (emacs_value[]){string});
}

static void goto_char(emacs_env *env, int pos) {
  emacs_value point = env->make_integer(env, pos);
  env->funcall(env, Fgoto_char, 1, (emacs_value[]){point});
}

static void vterm_put_caret(VTerm *vt, emacs_env *env, int row, int col, int offset) {
  int rows, cols;
  vterm_get_size(vt, &rows, &cols);
  // row * (cols + 1) because of newline character
  // col + 1 because (goto-char 1) sets point to first position
  int point = (row * (cols + 1)) + col + 1 + offset;
  goto_char(env, point);
}

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

static void vterm_redraw(VTerm *vt, emacs_env *env) {
  int i, j;
  int rows, cols;
  VTermScreen *screen = vterm_obtain_screen(vt);
  vterm_get_size(vt, &rows, &cols);

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

  VTermState *state = vterm_obtain_state(vt);
  VTermPos pos;
  vterm_state_get_cursorpos(state, &pos);
  vterm_put_caret(vt, env, pos.row, pos.col, -offset);
}

static void vterm_flush_output(struct Term *term) {
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

static void term_finalize(void *object) {
  struct Term *term = (struct Term*)object;
  pthread_cancel(term->thread);
  pthread_join(term->thread, NULL);
  vterm_free(term->vt);
  free(term);
}

static emacs_value Fvterm_new(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data) {
  struct Term *term = malloc(sizeof(struct Term));

  int rows = env->extract_integer(env, args[0]);
  int cols = env->extract_integer(env, args[1]);

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
    char *shell = getenv("SHELL");
    char *args[2] = {shell, NULL};
    execvp(shell, args);
    exit(1);
  }

  term->vt = vterm_new(rows, cols);
  vterm_set_utf8(term->vt, 1);

  VTermScreen *screen = vterm_obtain_screen(term->vt);
  vterm_screen_reset(screen, 1);

  pthread_create(&term->thread, NULL, &event_loop, term);

  return env->make_user_ptr(env, term_finalize, term);
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

static void process_key(struct Term *term, unsigned char *key, size_t len,
                        VTermModifier modifier) {
  if (len == 8 && memcmp(key, "<return>", len) == 0) {
    vterm_keyboard_key(term->vt, VTERM_KEY_ENTER, modifier);
  } else if (len == 11 && memcmp(key, "<backspace>", len) == 0) {
    vterm_keyboard_key(term->vt, VTERM_KEY_BACKSPACE, modifier);
  } else if (len == 5 && memcmp(key, "<tab>", len) == 0) {
    vterm_keyboard_key(term->vt, VTERM_KEY_TAB, modifier);
  } else if (len == 3 && memcmp(key, "SPC", len) == 0) {
    vterm_keyboard_unichar(term->vt, ' ', modifier);
  } else if (len <= 4) {
    uint32_t codepoint;
    if (utf8_to_codepoint(key, len, &codepoint)) {
      vterm_keyboard_unichar(term->vt, codepoint, modifier);
    }
  }

  vterm_flush_output(term);
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
    env->copy_string_contents(env, args[1], key, &len);
    VTermModifier modifier = VTERM_MOD_NONE;
    if (env->is_not_nil(env, args[2]))
      modifier = modifier | VTERM_MOD_SHIFT;
    if (env->is_not_nil(env, args[3]))
      modifier = modifier | VTERM_MOD_ALT;
    if (env->is_not_nil(env, args[4]))
      modifier = modifier | VTERM_MOD_CTRL;

    // Ignore the final zero byte
    process_key(term, key, len - 1, modifier);
  }

  // Read input from masterfd
  char bytes[4096];
  int len;
  struct timeval start, end;
  gettimeofday(&start, NULL);

  while ((len = read(term->masterfd, bytes, 4096)) > 0) {
    vterm_input_write(term->vt, bytes, len);
    vterm_redraw(term->vt, env);

    // Break after 40 milliseconds
    gettimeofday(&end, NULL);
    if (end.tv_usec - start.tv_usec > 40000) {
      break;
    }
  }

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

  // Functions
  Flength = env->intern(env, "length");
  Flist = env->intern(env, "list");
  Ferase_buffer = env->intern(env, "erase-buffer");
  Finsert = env->intern(env, "insert");
  Fgoto_char = env->intern(env, "goto-char");
  Fput_text_property = env->intern(env, "put-text-property");

  // Exported functions
  emacs_value fun;
  fun =
      env->make_function(env, 2, 2, Fvterm_new, "Allocates a new vterm.", NULL);
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
