#include "vterm-module.h"
#include "elisp.h"
#include "utf8.h"
#include <assert.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>
#include <vterm.h>

static LineInfo *alloc_lineinfo() {
  LineInfo *info = malloc(sizeof(LineInfo));
  info->directory = NULL;
  info->prompt_col = -1;
  return info;
}
void free_lineinfo(LineInfo *line) {
  if (line == NULL) {
    return;
  }
  if (line->directory != NULL) {
    free(line->directory);
    line->directory = NULL;
  }
  free(line);
}
static int term_sb_push(int cols, const VTermScreenCell *cells, void *data) {
  Term *term = (Term *)data;

  if (!term->sb_size) {
    return 0;
  }

  // copy vterm cells into sb_buffer
  size_t c = (size_t)cols;
  ScrollbackLine *sbrow = NULL;
  if (term->sb_current == term->sb_size) {
    if (term->sb_buffer[term->sb_current - 1]->cols == c) {
      // Recycle old row if it's the right size
      sbrow = term->sb_buffer[term->sb_current - 1];
    } else {
      if (term->sb_buffer[term->sb_current - 1]->info != NULL) {
        free_lineinfo(term->sb_buffer[term->sb_current - 1]->info);
        term->sb_buffer[term->sb_current - 1]->info = NULL;
      }
      free(term->sb_buffer[term->sb_current - 1]);
    }

    // Make room at the start by shifting to the right.
    memmove(term->sb_buffer + 1, term->sb_buffer,
            sizeof(term->sb_buffer[0]) * (term->sb_current - 1));

  } else if (term->sb_current > 0) {
    // Make room at the start by shifting to the right.
    memmove(term->sb_buffer + 1, term->sb_buffer,
            sizeof(term->sb_buffer[0]) * term->sb_current);
  }

  if (!sbrow) {
    sbrow = malloc(sizeof(ScrollbackLine) + c * sizeof(sbrow->cells[0]));
    sbrow->cols = c;
    sbrow->info = NULL;
  }
  if (sbrow->info != NULL) {
    free_lineinfo(sbrow->info);
  }
  sbrow->info = term->lines[0];
  memmove(term->lines, term->lines + 1,
          sizeof(term->lines[0]) * (term->lines_len - 1));
  if (term->resizing) {
    /* pushed by window height decr */
    if (term->lines[term->lines_len - 1] != NULL) {
      /* do not need free here ,it is reused ,we just need set null */
      term->lines[term->lines_len - 1] = NULL;
    }
    term->lines_len--;
  } else {
    LineInfo *lastline = term->lines[term->lines_len - 1];
    if (lastline != NULL) {
      LineInfo *line = alloc_lineinfo();
      if (lastline->directory != NULL) {
        line->directory = malloc(1 + strlen(lastline->directory));
        strcpy(line->directory, lastline->directory);
      }
      term->lines[term->lines_len - 1] = line;
    }
  }

  // New row is added at the start of the storage buffer.
  term->sb_buffer[0] = sbrow;
  if (term->sb_current < term->sb_size) {
    term->sb_current++;
  }

  if (term->sb_pending < term->sb_size) {
    term->sb_pending++;
    /* when window height decreased */
    if (term->height_resize < 0 &&
        term->sb_pending_by_height_decr < -term->height_resize) {
      term->sb_pending_by_height_decr++;
    }
  }

  memcpy(sbrow->cells, cells, c * sizeof(cells[0]));

  return 1;
}
/// Scrollback pop handler (from pangoterm).
///
/// @param cols
/// @param cells  VTerm state to update.
/// @param data   Term
static int term_sb_pop(int cols, VTermScreenCell *cells, void *data) {
  Term *term = (Term *)data;

  if (!term->sb_current) {
    return 0;
  }

  if (term->sb_pending) {
    term->sb_pending--;
  }

  ScrollbackLine *sbrow = term->sb_buffer[0];
  term->sb_current--;
  // Forget the "popped" row by shifting the rest onto it.
  memmove(term->sb_buffer, term->sb_buffer + 1,
          sizeof(term->sb_buffer[0]) * (term->sb_current));

  size_t cols_to_copy = (size_t)cols;
  if (cols_to_copy > sbrow->cols) {
    cols_to_copy = sbrow->cols;
  }

  // copy to vterm state
  memcpy(cells, sbrow->cells, sizeof(cells[0]) * cols_to_copy);
  size_t col;
  for (col = cols_to_copy; col < (size_t)cols; col++) {
    cells[col].chars[0] = 0;
    cells[col].width = 1;
  }

  LineInfo **lines = malloc(sizeof(LineInfo *) * (term->lines_len + 1));

  memmove(lines + 1, term->lines, sizeof(term->lines[0]) * term->lines_len);
  lines[0] = sbrow->info;
  free(sbrow);
  term->lines_len += 1;
  free(term->lines);
  term->lines = lines;

  return 1;
}

static int row_to_linenr(Term *term, int row) {
  return row != INT_MAX ? row + (int)term->sb_current + 1 : INT_MAX;
}

static int linenr_to_row(Term *term, int linenr) {
  return linenr - (int)term->sb_current - 1;
}

static void fetch_cell(Term *term, int row, int col, VTermScreenCell *cell) {
  if (row < 0) {
    ScrollbackLine *sbrow = term->sb_buffer[-row - 1];
    if ((size_t)col < sbrow->cols) {
      *cell = sbrow->cells[col];
    } else {
      // fill the pointer with an empty cell
      VTermColor fg, bg;
      VTermState *state = vterm_obtain_state(term->vt);
      vterm_state_get_default_colors(state, &fg, &bg);

      *cell = (VTermScreenCell){.chars = {0}, .width = 1, .bg = bg};
    }
  } else {
    vterm_screen_get_cell(term->vts, (VTermPos){.row = row, .col = col}, cell);
  }
}

static char *get_row_directory(Term *term, int row) {
  if (row < 0) {
    ScrollbackLine *sbrow = term->sb_buffer[-row - 1];
    return sbrow->info->directory;
    /* return term->dirs[0]; */
  } else {
    LineInfo *line = term->lines[row];
    return line ? line->directory : NULL;
  }
}
static LineInfo *get_lineinfo(Term *term, int row) {
  if (row < 0) {
    ScrollbackLine *sbrow = term->sb_buffer[-row - 1];
    return sbrow->info;
    /* return term->dirs[0]; */
  } else {
    return term->lines[row];
  }
}
static bool is_eol(Term *term, int end_col, int row, int col) {
  /* This cell is EOL if this and every cell to the right is black */
  if (row >= 0) {
    VTermPos pos = {.row = row, .col = col};
    return vterm_screen_is_eol(term->vts, pos);
  }

  ScrollbackLine *sbrow = term->sb_buffer[-row - 1];
  int c;
  for (c = col; c < end_col && c < sbrow->cols;) {
    if (sbrow->cells[c].chars[0]) {
      return 0;
    }
    c += sbrow->cells[c].width;
  }
  return 1;
}

static size_t get_col_offset(Term *term, int row, int end_col) {
  int col = 0;
  size_t offset = 0;
  unsigned char buf[4];
  int height;
  int width;
  vterm_get_size(term->vt, &height, &width);

  while (col < end_col) {
    VTermScreenCell cell;
    fetch_cell(term, row, col, &cell);
    if (cell.chars[0]) {
      if (cell.width > 1) {
        offset += cell.width - 1;
      }
    } else {
      if (is_eol(term, term->width, row, col)) {
        offset += cell.width;
      }
    }
    col += cell.width;
  }
  return offset;
}

static void refresh_lines(Term *term, emacs_env *env, int start_row,
                          int end_row, int end_col) {
  if (end_row < start_row) {
    return;
  }
  int i, j;

  char buffer[((end_row - start_row + 1) * end_col) * 4];
  int length = 0;
  VTermScreenCell cell;
  VTermScreenCell lastCell;
  fetch_cell(term, start_row, 0, &lastCell);

  int offset = 0;
  for (i = start_row; i < end_row; i++) {
    for (j = 0; j < end_col; j++) {
      fetch_cell(term, i, j, &cell);

      if (!compare_cells(&cell, &lastCell)) {
        emacs_value text = render_text(env, term, buffer, length, &lastCell);
        insert(env, text);
        length = 0;
      }

      lastCell = cell;
      if (cell.chars[0] == 0) {
        if (is_eol(term, end_col, i, j)) {
          /* This cell is EOL if this and every cell to the right is black */
          break;
        }
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
  emacs_value text = render_text(env, term, buffer, length, &lastCell);
  insert(env, text);

  return;
}
// Refresh the screen (visible part of the buffer when the terminal is
// focused) of a invalidated terminal
static void refresh_screen(Term *term, emacs_env *env) {
  // Term height may have decreased before `invalid_end` reflects it.
  term->invalid_end = MIN(term->invalid_end, term->height);

  if (term->invalid_end >= term->invalid_start) {
    int startrow = -(term->height - term->invalid_start - term->linenum_added);
    /* startrow is negative,so we backward  -startrow lines from end of buffer
       then delete lines there.
     */
    goto_line(env, startrow);
    delete_lines(env, startrow, term->invalid_end - term->invalid_start, true);
    refresh_lines(term, env, term->invalid_start, term->invalid_end,
                  term->width);

    /* term->linenum_added is lines added  by window height increased */
    term->linenum += term->linenum_added;
    term->linenum_added = 0;
  }

  term->invalid_start = INT_MAX;
  term->invalid_end = -1;
}

static int term_resize(int rows, int cols, void *user_data) {
  /* can not use invalidate_terminal here */
  /* when the window height decreased, */
  /*  the value of term->invalid_end can't bigger than window height */
  Term *term = (Term *)user_data;
  term->invalid_start = 0;
  term->invalid_end = rows;

  /* if rows=term->lines_len, that means term_sb_pop already resize term->lines
   */
  /* if rows<term->lines_len, term_sb_push would resize term->lines there */
  /* we noly need to take care of rows>term->height */

  if (rows > term->height) {
    if (rows > term->lines_len) {
      LineInfo **infos = term->lines;
      term->lines = malloc(sizeof(LineInfo *) * rows);
      memmove(term->lines, infos, sizeof(infos[0]) * term->lines_len);

      LineInfo *lastline = term->lines[term->lines_len - 1];
      for (int i = term->lines_len; i < rows; i++) {
        if (lastline != NULL) {
          LineInfo *line = alloc_lineinfo();
          if (lastline->directory != NULL) {
            line->directory =
                malloc(1 + strlen(term->lines[term->lines_len - 1]->directory));
            strcpy(line->directory,
                   term->lines[term->lines_len - 1]->directory);
          }
          term->lines[i] = line;
        } else {
          term->lines[i] = NULL;
        }
      }
      term->lines_len = rows;
      free(infos);
    }
  }

  term->width = cols;
  term->height = rows;

  invalidate_terminal(term, -1, -1);
  term->resizing = false;

  return 1;
}

// Refresh the scrollback of an invalidated terminal.
static void refresh_scrollback(Term *term, emacs_env *env) {
  int max_line_count = (int)term->sb_current + term->height;
  int del_cnt = 0;
  if (term->sb_pending > 0) {
    // This means that either the window height has decreased or the screen
    // became full and libvterm had to push all rows up. Convert the first
    // pending scrollback row into a string and append it just above the visible
    // section of the buffer

    del_cnt = term->linenum - term->height - (int)term->sb_size +
              term->sb_pending - term->sb_pending_by_height_decr;
    if (del_cnt > 0) {
      delete_lines(env, 1, del_cnt, true);
      term->linenum -= del_cnt;
    }

    term->linenum += term->sb_pending;
    del_cnt = term->linenum - max_line_count; /* extra lines at the bottom */
    /* buf_index is negative,so we move to end of buffer,then backward
       -buf_index lines. goto lines backward is effectively when
       vterm-max-scrollback is a large number.
     */
    int buf_index = -(term->height + del_cnt);
    goto_line(env, buf_index);
    refresh_lines(term, env, -term->sb_pending, 0, term->width);

    term->sb_pending = 0;
  }

  // Remove extra lines at the bottom
  del_cnt = term->linenum - max_line_count;
  if (del_cnt > 0) {
    term->linenum -= del_cnt;
    /* -del_cnt is negative,so we delete_lines from end of buffer.
       this line means: delete del_cnt count of lines at end of buffer.
     */
    delete_lines(env, -del_cnt, del_cnt, true);
  }

  term->sb_pending_by_height_decr = 0;
  term->height_resize = 0;
}

static void adjust_topline(Term *term, emacs_env *env) {
  VTermState *state = vterm_obtain_state(term->vt);
  VTermPos pos;
  vterm_state_get_cursorpos(state, &pos);

  /* pos.row-term->height is negative,so we backward term->height-pos.row
   * lines from end of buffer
   */

  goto_line(env, pos.row - term->height);
  size_t offset = get_col_offset(term, pos.row, pos.col);
  forward_char(env, env->make_integer(env, pos.col - offset));

  bool following = term->height == 1 + pos.row;

  emacs_value windows = get_buffer_window_list(env);
  emacs_value swindow = selected_window(env);
  int winnum = env->extract_integer(env, length(env, windows));
  for (int i = 0; i < winnum; i++) {
    emacs_value window = nth(env, i, windows);
    if (eq(env, window, swindow)) {
      int win_body_height =
          env->extract_integer(env, window_body_height(env, window));

      /* recenter:If ARG is negative, it counts up from the bottom of the
       * window.  (ARG should be less than the height of the window ) */
      if (term->height - pos.row <= win_body_height) {
        recenter(env, env->make_integer(env, pos.row - term->height));
      } else {
        recenter(env, env->make_integer(env, pos.row));
      }
    } else {
      if (env->is_not_nil(env, window)) {
        set_window_point(env, window, point(env));
      }
    }
  }
}

static void invalidate_terminal(Term *term, int start_row, int end_row) {
  if (start_row != -1 && end_row != -1) {
    term->invalid_start = MIN(term->invalid_start, start_row);
    term->invalid_end = MAX(term->invalid_end, end_row);
  }
  term->is_invalidated = true;
}

static int term_damage(VTermRect rect, void *data) {
  invalidate_terminal(data, rect.start_row, rect.end_row);
  return 1;
}

static int term_moverect(VTermRect dest, VTermRect src, void *data) {
  invalidate_terminal(data, MIN(dest.start_row, src.start_row),
                      MAX(dest.end_row, src.end_row));
  return 1;
}

static int term_movecursor(VTermPos new, VTermPos old, int visible,
                           void *data) {
  Term *term = data;
  term->cursor.row = new.row;
  term->cursor.col = new.col;
  invalidate_terminal(term, old.row, old.row + 1);
  invalidate_terminal(term, new.row, new.row + 1);

  return 1;
}

static void term_redraw_cursor(Term *term, emacs_env *env) {
  if (term->cursor.cursor_type_changed) {
    term->cursor.cursor_type_changed = false;
    switch (term->cursor.cursor_type) {
    case VTERM_PROP_CURSOR_VISIBLE:
      set_cursor_type(env, Qt);
      break;
    case VTERM_PROP_CURSOR_NOT_VISIBLE:
      set_cursor_type(env, Qnil);
      break;
    case VTERM_PROP_CURSOR_BLOCK:
      set_cursor_type(env, Qbox);
      break;
    case VTERM_PROP_CURSOR_UNDERLINE:
      set_cursor_type(env, Qhbar);
      break;
    case VTERM_PROP_CURSOR_BAR_LEFT:
      set_cursor_type(env, Qbar);
      break;
    default:
      return;
    }
  }
}

static void term_redraw(Term *term, emacs_env *env) {
  term_redraw_cursor(term, env);

  if (term->is_invalidated) {
    int oldlinenum = term->linenum;
    refresh_scrollback(term, env);
    refresh_screen(term, env);
    term->linenum_added = term->linenum - oldlinenum;
    adjust_topline(term, env);
    term->linenum_added = 0;
  }

  if (term->title_changed) {
    set_title(env, env->make_string(env, term->title, strlen(term->title)));
    term->title_changed = false;
  }

  if (term->directory_changed) {
    set_directory(
        env, env->make_string(env, term->directory, strlen(term->directory)));
    term->directory_changed = false;
  }
  if (term->elisp_code_changed) {
    term->elisp_code_changed = false;
    emacs_value elisp_code =
        env->make_string(env, term->elisp_code, strlen(term->elisp_code));
    vterm_eval(env, elisp_code);
    free(term->elisp_code);
    term->elisp_code = NULL;
  }

  term->is_invalidated = false;
}

static VTermScreenCallbacks vterm_screen_callbacks = {
    .damage = term_damage,
    .moverect = term_moverect,
    .movecursor = term_movecursor,
    .settermprop = term_settermprop,
    .resize = term_resize,
    .sb_pushline = term_sb_push,
    .sb_popline = term_sb_pop,
};

static bool compare_cells(VTermScreenCell *a, VTermScreenCell *b) {
  bool equal = true;
  equal = equal && vterm_color_is_equal(&a->fg, &b->fg);
  equal = equal && vterm_color_is_equal(&a->bg, &b->bg);
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

static void term_set_title(Term *term, char *title) {
  size_t len = strlen(title);
  if (term->title) {
    free(term->title);
  }
  term->title = malloc(sizeof(char) * (len + 1));
  strncpy(term->title, title, len);
  term->title[len] = 0;
  term->title_changed = true;
  return;
}

static int term_settermprop(VTermProp prop, VTermValue *val, void *user_data) {
  Term *term = (Term *)user_data;
  switch (prop) {
  case VTERM_PROP_CURSORVISIBLE:
    invalidate_terminal(term, term->cursor.row, term->cursor.row + 1);
    if (val->boolean) {
      term->cursor.cursor_type = VTERM_PROP_CURSOR_VISIBLE;
    } else {
      term->cursor.cursor_type = VTERM_PROP_CURSOR_NOT_VISIBLE;
    }
    term->cursor.cursor_type_changed = true;
    break;
  case VTERM_PROP_CURSORSHAPE:
    invalidate_terminal(term, term->cursor.row, term->cursor.row + 1);
    term->cursor.cursor_type = val->number;
    term->cursor.cursor_type_changed = true;

    break;
  case VTERM_PROP_TITLE:
    term_set_title(term, val->string);
    break;
  case VTERM_PROP_ALTSCREEN:
    invalidate_terminal(term, 0, term->height);
    break;
  default:
    return 0;
  }

  return 1;
}

static emacs_value render_text(emacs_env *env, Term *term, char *buffer,
                               int len, VTermScreenCell *cell) {
  emacs_value text;
  if (len == 0) {
    text = env->make_string(env, "", 0);
    return text;
  } else {
    text = env->make_string(env, buffer, len);
  }

  emacs_value fg = color_to_rgb_string(env, term, &cell->fg);
  emacs_value bg = color_to_rgb_string(env, term, &cell->bg);
  emacs_value bold =
      cell->attrs.bold && !term->disable_bold_font ? Qbold : Qnormal;
  emacs_value underline = cell->attrs.underline ? Qt : Qnil;
  emacs_value italic = cell->attrs.italic ? Qitalic : Qnormal;
  emacs_value reverse = cell->attrs.reverse ? Qt : Qnil;
  emacs_value strike = cell->attrs.strike ? Qt : Qnil;

  // TODO: Blink, font, dwl, dhl is missing
  emacs_value properties =
      list(env,
           (emacs_value[]){Qforeground, fg, Qbackground, bg, Qweight, bold,
                           Qunderline, underline, Qslant, italic, Qreverse,
                           reverse, Qstrike, strike},
           14);

  put_text_property(env, text, Qface, properties);

  return text;
}

static emacs_value color_to_rgb_string(emacs_env *env, Term *term,
                                       VTermColor *color) {
  if (VTERM_COLOR_IS_DEFAULT_FG(color)) {
    return vterm_get_color(env, -1);
  }
  if (VTERM_COLOR_IS_DEFAULT_BG(color)) {
    return vterm_get_color(env, -2);
  }
  if (VTERM_COLOR_IS_INDEXED(color)) {
    if (color->indexed.idx < 16) {
      return vterm_get_color(env, color->indexed.idx);
    } else {
      VTermState *state = vterm_obtain_state(term->vt);
      vterm_state_get_palette_color(state, color->indexed.idx, color);
    }
  } else if (VTERM_COLOR_IS_RGB(color)) {
    /* do nothing just use the argument color directly */
  }

  char buffer[8];
  snprintf(buffer, 8, "#%02X%02X%02X", color->rgb.red, color->rgb.green,
           color->rgb.blue);
  return env->make_string(env, buffer, 7);
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

static void term_clear_scrollback(Term *term, emacs_env *env) {
  vterm_screen_flush_damage(term->vts);
  term_redraw(term, env);
  if (term->sb_pending > 0) { // Pending rows must be processed first.
    return;
  }
  for (int i = 0; i < term->sb_current; i++) {
    if (term->sb_buffer[i]->info != NULL) {
      free_lineinfo(term->sb_buffer[i]->info);
      term->sb_buffer[i]->info = NULL;
    }
    free(term->sb_buffer[i]);
  }
  free(term->sb_buffer);
  term->sb_buffer = malloc(sizeof(ScrollbackLine *) * term->sb_size);
  delete_lines(env, 1, term->sb_current, true);
  term->linenum -= term->sb_current;
  term->sb_current = 0;
}
static void term_process_key(Term *term, emacs_env *env, unsigned char *key,
                             size_t len, VTermModifier modifier) {
  if (is_key(key, len, "<clear_scrollback>")) {
    term_clear_scrollback(term, env);
  } else if (is_key(key, len, "<start>")) {
    tcflow(term->pty_fd, TCOON);
  } else if (is_key(key, len, "<stop>")) {
    tcflow(term->pty_fd, TCOOFF);
  } else if (is_key(key, len, "<start_paste>")) {
    vterm_keyboard_start_paste(term->vt);
  } else if (is_key(key, len, "<end_paste>")) {
    vterm_keyboard_end_paste(term->vt);
  } else if (is_key(key, len, "<tab>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_TAB, modifier);
  } else if (is_key(key, len, "<backtab>") ||
             is_key(key, len, "<iso-lefttab>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_TAB, VTERM_MOD_SHIFT);
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
  } else if (is_key(key, len, "<next>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_PAGEDOWN, modifier);
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
  } else if (is_key(key, len, "<kp-0>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_KP_0, modifier);
  } else if (is_key(key, len, "<kp-1>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_KP_1, modifier);
  } else if (is_key(key, len, "<kp-2>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_KP_2, modifier);
  } else if (is_key(key, len, "<kp-3>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_KP_3, modifier);
  } else if (is_key(key, len, "<kp-4>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_KP_4, modifier);
  } else if (is_key(key, len, "<kp-5>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_KP_5, modifier);
  } else if (is_key(key, len, "<kp-6>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_KP_6, modifier);
  } else if (is_key(key, len, "<kp-7>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_KP_7, modifier);
  } else if (is_key(key, len, "<kp-8>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_KP_8, modifier);
  } else if (is_key(key, len, "<kp-9>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_KP_9, modifier);
  } else if (is_key(key, len, "<kp-decimal>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_KP_PERIOD, modifier);
  } else if (is_key(key, len, "j") && (modifier == VTERM_MOD_CTRL)) {
    vterm_keyboard_unichar(term->vt, '\n', 0);
  } else if (is_key(key, len, "SPC")) {
    vterm_keyboard_unichar(term->vt, ' ', modifier);
  } else if (len <= 4) {
    uint32_t codepoint;
    if (utf8_to_codepoint(key, len, &codepoint)) {
      vterm_keyboard_unichar(term->vt, codepoint, modifier);
    }
  }
}

void term_finalize(void *object) {
  Term *term = (Term *)object;
  for (int i = 0; i < term->sb_current; i++) {
    if (term->sb_buffer[i]->info != NULL) {
      free_lineinfo(term->sb_buffer[i]->info);
      term->sb_buffer[i]->info = NULL;
    }
    free(term->sb_buffer[i]);
  }
  if (term->title) {
    free(term->title);
    term->title = NULL;
  }

  if (term->directory) {
    free(term->directory);
    term->directory = NULL;
  }
  if (term->elisp_code) {
    free(term->elisp_code);
    term->elisp_code = NULL;
  }
  for (int i = 0; i < term->lines_len; i++) {
    if (term->lines[i] != NULL) {
      free_lineinfo(term->lines[i]);
      term->lines[i] = NULL;
    }
  }

  if (term->pty_fd > 0) {
    close(term->pty_fd);
  }

  free(term->sb_buffer);
  vterm_free(term->vt);
  free(term);
}

static int osc_callback(const char *command, size_t cmdlen, void *user) {
  Term *term = (Term *)user;
  char buffer[cmdlen + 1];

  buffer[cmdlen] = '\0';
  memcpy(buffer, command, cmdlen);

  if (cmdlen > 4 && buffer[0] == '5' && buffer[1] == '1' && buffer[2] == ';' &&
      buffer[3] == 'A') {
    if (term->directory != NULL) {
      free(term->directory);
      term->directory = NULL;
    }
    term->directory = malloc(cmdlen - 4 + 1);
    strcpy(term->directory, &buffer[4]);
    term->directory_changed = true;

    for (int i = term->cursor.row; i < term->lines_len; i++) {
      if (term->lines[i] == NULL) {
        term->lines[i] = alloc_lineinfo();
      }

      if (term->lines[i]->directory != NULL) {
        free(term->lines[i]->directory);
      }
      term->lines[i]->directory = malloc(cmdlen - 4 + 1);
      strcpy(term->lines[i]->directory, &buffer[4]);
      if (i == term->cursor.row) {
        term->lines[i]->prompt_col = term->cursor.col;
      } else {
        term->lines[i]->prompt_col = -1;
      }
    }
    return 1;
  } else if (cmdlen > 4 && buffer[0] == '5' && buffer[1] == '1' &&
             buffer[2] == ';' && buffer[3] == 'E') {
    term->elisp_code = malloc(cmdlen - 4 + 1);
    strcpy(term->elisp_code, &buffer[4]);
    term->elisp_code_changed = true;
    return 1;
  }
  return 0;
}

static VTermParserCallbacks parser_callbacks = {
    .text = NULL,
    .control = NULL,
    .escape = NULL,
    .csi = NULL,
    .osc = &osc_callback,
    .dcs = NULL,
};

emacs_value Fvterm_new(emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                       void *data) {
  Term *term = malloc(sizeof(Term));

  int rows = env->extract_integer(env, args[0]);
  int cols = env->extract_integer(env, args[1]);
  int sb_size = env->extract_integer(env, args[2]);
  int disable_bold_font = env->is_not_nil(env, args[3]);

  term->vt = vterm_new(rows, cols);
  vterm_set_utf8(term->vt, 1);

  term->vts = vterm_obtain_screen(term->vt);

  VTermState *state = vterm_obtain_state(term->vt);
  vterm_state_set_unrecognised_fallbacks(state, &parser_callbacks, term);

  vterm_screen_reset(term->vts, 1);
  vterm_screen_set_callbacks(term->vts, &vterm_screen_callbacks, term);
  vterm_screen_set_damage_merge(term->vts, VTERM_DAMAGE_SCROLL);
  vterm_screen_enable_altscreen(term->vts, true);
  term->sb_size = MIN(SB_MAX, sb_size);
  term->sb_current = 0;
  term->sb_pending = 0;
  term->sb_pending_by_height_decr = 0;
  term->sb_buffer = malloc(sizeof(ScrollbackLine *) * term->sb_size);
  term->invalid_start = 0;
  term->invalid_end = rows;
  term->width = cols;
  term->height = rows;
  term->height_resize = 0;
  term->disable_bold_font = disable_bold_font;
  emacs_value newline = env->make_string(env, "\n", 1);
  for (int i = 0; i < term->height; i++) {
    insert(env, newline);
  }
  term->linenum = term->height;
  term->linenum_added = 0;
  term->resizing = false;

  term->pty_fd = -1;

  term->title = NULL;
  term->title_changed = false;

  term->cursor.row = 0;
  term->cursor.col = 0;
  term->directory = NULL;
  term->directory_changed = false;
  term->elisp_code = NULL;
  term->elisp_code_changed = false;

  term->lines = malloc(sizeof(LineInfo *) * rows);
  term->lines_len = rows;
  for (int i = 0; i < rows; i++) {
    term->lines[i] = NULL;
  }

  return env->make_user_ptr(env, term_finalize, term);
}

emacs_value Fvterm_update(emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                          void *data) {
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
    term_process_key(term, env, key, len - 1, modifier);
  }

  // Flush output
  term_flush_output(term, env);
  if (term->is_invalidated) {
    vterm_invalidate(env);
  }

  return env->make_integer(env, 0);
}

emacs_value Fvterm_redraw(emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                          void *data) {
  Term *term = env->get_user_ptr(env, args[0]);
  term_redraw(term, env);
  return env->make_integer(env, 0);
}

emacs_value Fvterm_write_input(emacs_env *env, ptrdiff_t nargs,
                               emacs_value args[], void *data) {
  Term *term = env->get_user_ptr(env, args[0]);
  ptrdiff_t len = string_bytes(env, args[1]);
  char bytes[len];

  env->copy_string_contents(env, args[1], bytes, &len);

  vterm_input_write(term->vt, bytes, len);
  vterm_screen_flush_damage(term->vts);

  return env->make_integer(env, 0);
}

emacs_value Fvterm_set_size(emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                            void *data) {
  Term *term = env->get_user_ptr(env, args[0]);
  int rows = env->extract_integer(env, args[1]);
  int cols = env->extract_integer(env, args[2]);

  if (cols != term->width || rows != term->height) {
    term->height_resize = rows - term->height;
    if (rows > term->height) {
      if (rows - term->height > term->sb_current) {
        term->linenum_added = rows - term->height - term->sb_current;
      }
    }
    term->resizing = true;
    vterm_set_size(term->vt, rows, cols);
    vterm_screen_flush_damage(term->vts);

    term_redraw(term, env);
  }

  return Qnil;
}

emacs_value Fvterm_set_pty_name(emacs_env *env, ptrdiff_t nargs,
                                emacs_value args[], void *data) {
  Term *term = env->get_user_ptr(env, args[0]);

  if (nargs > 1) {
    ptrdiff_t len = string_bytes(env, args[1]);
    char filename[len];

    env->copy_string_contents(env, args[1], filename, &len);

    term->pty_fd = open(filename, O_RDONLY);
  }
  return Qnil;
}
emacs_value Fvterm_get_pwd(emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                           void *data) {
  Term *term = env->get_user_ptr(env, args[0]);
  int linenum = env->extract_integer(env, args[1]);
  int row = linenr_to_row(term, linenum);
  char *dir = get_row_directory(term, row);

  return dir ? env->make_string(env, dir, strlen(dir)) : Qnil;
}

emacs_value Fvterm_get_icrnl(emacs_env *env, ptrdiff_t nargs,
                             emacs_value args[], void *data) {
  Term *term = env->get_user_ptr(env, args[0]);

  if (term->pty_fd > 0) {
    struct termios keys;
    tcgetattr(term->pty_fd, &keys);

    if (keys.c_iflag & ICRNL)
      return Qt;
    else
      return Qnil;
  }
  return Qnil;
}

emacs_value Fvterm_get_prompt_point(emacs_env *env, ptrdiff_t nargs,
                                    emacs_value args[], void *data) {
  Term *term = env->get_user_ptr(env, args[0]);
  int linenum = env->extract_integer(env, args[1]);
  if (linenum >= term->linenum) {
    linenum = term->linenum;
  }
  for (int l = linenum; l >= 1; l--) {
    int cur_row = linenr_to_row(term, l);
    LineInfo *info = get_lineinfo(term, cur_row);
    if (info != NULL && info->prompt_col >= 0) {
      goto_line(env, l);
      size_t offset = get_col_offset(term, cur_row, info->prompt_col);
      forward_char(env, env->make_integer(env, info->prompt_col - offset));

      return point(env);
    }
  }
  return Qnil;
}

emacs_value Fvterm_reset_cursor_point(emacs_env *env, ptrdiff_t nargs,
                                      emacs_value args[], void *data) {
  Term *term = env->get_user_ptr(env, args[0]);
  int line = row_to_linenr(term, term->cursor.row);
  goto_line(env, line);
  size_t offset = get_col_offset(term, term->cursor.row, term->cursor.col);
  forward_char(env, env->make_integer(env, term->cursor.col - offset));
  return point(env);
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
  Qbox = env->make_global_ref(env, env->intern(env, "box"));
  Qbar = env->make_global_ref(env, env->intern(env, "bar"));
  Qhbar = env->make_global_ref(env, env->intern(env, "hbar"));
  Qcursor_type = env->make_global_ref(env, env->intern(env, "cursor-type"));

  // Functions
  Fsymbol_value = env->make_global_ref(env, env->intern(env, "symbol-value"));
  Flength = env->make_global_ref(env, env->intern(env, "length"));
  Flist = env->make_global_ref(env, env->intern(env, "list"));
  Fnth = env->make_global_ref(env, env->intern(env, "nth"));
  Ferase_buffer = env->make_global_ref(env, env->intern(env, "erase-buffer"));
  Finsert = env->make_global_ref(env, env->intern(env, "insert"));
  Fgoto_char = env->make_global_ref(env, env->intern(env, "goto-char"));
  Fput_text_property =
      env->make_global_ref(env, env->intern(env, "put-text-property"));
  Fset = env->make_global_ref(env, env->intern(env, "set"));
  Fvterm_flush_output =
      env->make_global_ref(env, env->intern(env, "vterm--flush-output"));
  Fforward_line = env->make_global_ref(env, env->intern(env, "forward-line"));
  Fgoto_line = env->make_global_ref(env, env->intern(env, "vterm--goto-line"));
  Fdelete_lines =
      env->make_global_ref(env, env->intern(env, "vterm--delete-lines"));
  Frecenter = env->make_global_ref(env, env->intern(env, "recenter"));
  Fset_window_point =
      env->make_global_ref(env, env->intern(env, "set-window-point"));
  Fwindow_body_height =
      env->make_global_ref(env, env->intern(env, "window-body-height"));

  Fpoint = env->make_global_ref(env, env->intern(env, "point"));
  Fforward_char = env->make_global_ref(env, env->intern(env, "forward-char"));
  Fget_buffer_window_list =
      env->make_global_ref(env, env->intern(env, "get-buffer-window-list"));
  Fselected_window =
      env->make_global_ref(env, env->intern(env, "selected-window"));

  Fvterm_set_title =
      env->make_global_ref(env, env->intern(env, "vterm--set-title"));
  Fvterm_set_directory =
      env->make_global_ref(env, env->intern(env, "vterm--set-directory"));
  Fvterm_invalidate =
      env->make_global_ref(env, env->intern(env, "vterm--invalidate"));
  Feq = env->make_global_ref(env, env->intern(env, "eq"));
  Fvterm_get_color =
      env->make_global_ref(env, env->intern(env, "vterm--get-color"));
  Fvterm_eval = env->make_global_ref(env, env->intern(env, "vterm--eval"));

  // Exported functions
  emacs_value fun;
  fun =
      env->make_function(env, 4, 4, Fvterm_new, "Allocates a new vterm.", NULL);
  bind_function(env, "vterm--new", fun);

  fun = env->make_function(env, 1, 5, Fvterm_update,
                           "Process io and update the screen.", NULL);
  bind_function(env, "vterm--update", fun);

  fun =
      env->make_function(env, 1, 1, Fvterm_redraw, "Redraw the screen.", NULL);
  bind_function(env, "vterm--redraw", fun);

  fun = env->make_function(env, 2, 2, Fvterm_write_input,
                           "Write input to vterm.", NULL);
  bind_function(env, "vterm--write-input", fun);

  fun = env->make_function(env, 3, 3, Fvterm_set_size,
                           "Sets the size of the terminal.", NULL);
  bind_function(env, "vterm--set-size", fun);

  fun = env->make_function(env, 2, 2, Fvterm_set_pty_name,
                           "Sets the name of the pty.", NULL);
  bind_function(env, "vterm--set-pty-name", fun);
  fun = env->make_function(env, 2, 2, Fvterm_get_pwd,
                           "Get the working directory of at line n.", NULL);
  bind_function(env, "vterm--get-pwd-raw", fun);
  fun = env->make_function(env, 2, 2, Fvterm_get_prompt_point,
                           "Get the end postion of current prompt.", NULL);
  bind_function(env, "vterm--get-prompt-point-internal", fun);
  fun = env->make_function(env, 1, 1, Fvterm_reset_cursor_point,
                           "Reset cursor postion.", NULL);
  bind_function(env, "vterm--reset-point", fun);


  fun = env->make_function(env, 1, 1, Fvterm_get_icrnl,
                           "Gets the icrnl state of the pty", NULL);
  bind_function(env, "vterm--get-icrnl", fun);

  provide(env, "vterm-module");

  return 0;
}
