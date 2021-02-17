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
static int is_end_of_prompt(Term *term, int end_col, int row, int col) {
  LineInfo *info = get_lineinfo(term, row);
  if (info == NULL) {
    return 0;
  }
  if (info->prompt_col < 0) {
    return 0;
  }
  if (info->prompt_col == col) {
    return 1;
  }
  if (is_eol(term, end_col, row, col) && info->prompt_col >= col) {
    return 1;
  }
  return 0;
}

static void goto_col(Term *term, emacs_env *env, int row, int end_col) {
  int col = 0;
  size_t offset = 0;
  size_t beyond_eol = 0;

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
        beyond_eol += cell.width;
      }
    }
    col += cell.width;
  }

  forward_char(env, env->make_integer(env, end_col - offset));
  emacs_value space = env->make_string(env, " ", 1);
  for (int i = 0; i < beyond_eol; i += 1)
    insert(env, space);
}

static void refresh_lines(Term *term, emacs_env *env, int start_row,
                          int end_row, int end_col) {
  if (end_row < start_row) {
    return;
  }
  int i, j;

#define PUSH_BUFFER(c)                                                         \
  do {                                                                         \
    if (length == capacity) {                                                  \
      capacity += end_col * 4;                                                 \
      buffer = realloc(buffer, capacity * sizeof(char));                       \
    }                                                                          \
    buffer[length] = (c);                                                      \
    length++;                                                                  \
  } while (0)

  int capacity = ((end_row - start_row + 1) * end_col) * 4;
  int length = 0;
  char *buffer = malloc(capacity * sizeof(char));
  VTermScreenCell cell;
  VTermScreenCell lastCell;
  fetch_cell(term, start_row, 0, &lastCell);

  for (i = start_row; i < end_row; i++) {

    int newline = 0;
    int isprompt = 0;
    for (j = 0; j < end_col; j++) {
      fetch_cell(term, i, j, &cell);
      if (isprompt && length > 0) {
        emacs_value text = render_text(env, term, buffer, length, &lastCell);
        insert(env, render_prompt(env, text));
        length = 0;
      }

      isprompt = is_end_of_prompt(term, end_col, i, j);
      if (isprompt && length > 0) {
        insert(env, render_text(env, term, buffer, length, &lastCell));
        length = 0;
      }

      if (!compare_cells(&cell, &lastCell)) {
        emacs_value text = render_text(env, term, buffer, length, &lastCell);
        insert(env, text);
        length = 0;
      }

      lastCell = cell;
      if (cell.chars[0] == 0) {
        if (is_eol(term, end_col, i, j)) {
          /* This cell is EOL if this and every cell to the right is black */
          PUSH_BUFFER('\n');
          newline = 1;
          break;
        }
        PUSH_BUFFER(' ');
      } else {
        for (int k = 0; k < VTERM_MAX_CHARS_PER_CELL && cell.chars[k]; ++k) {
          unsigned char bytes[4];
          size_t count = codepoint_to_utf8(cell.chars[k], bytes);
          for (int l = 0; l < count; l++) {
            PUSH_BUFFER(bytes[l]);
          }
        }
      }

      if (cell.width > 1) {
        int w = cell.width - 1;
        j = j + w;
      }
    }
    if (isprompt && length > 0) {
      emacs_value text = render_text(env, term, buffer, length, &lastCell);
      insert(env, render_prompt(env, text));
      length = 0;
      isprompt = 0;
    }

    if (!newline) {
      emacs_value text = render_text(env, term, buffer, length, &lastCell);
      insert(env, text);
      length = 0;
      text = render_fake_newline(env, term);
      insert(env, text);
    }
  }
  emacs_value text = render_text(env, term, buffer, length, &lastCell);
  insert(env, text);

#undef PUSH_BUFFER
  free(buffer);

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
  goto_col(term, env, pos.row, pos.col);

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
  if (term->cursor.cursor_blink_changed) {
    term->cursor.cursor_blink_changed = false;
    set_cursor_blink(env, term->cursor.cursor_blink);
  }

  if (term->cursor.cursor_type_changed) {
    term->cursor.cursor_type_changed = false;

    if (!term->cursor.cursor_visible) {
      set_cursor_type(env, Qnil);
      return;
    }

    switch (term->cursor.cursor_type) {
    case VTERM_PROP_CURSORSHAPE_BLOCK:
      set_cursor_type(env, Qbox);
      break;
    case VTERM_PROP_CURSORSHAPE_UNDERLINE:
      set_cursor_type(env, Qhbar);
      break;
    case VTERM_PROP_CURSORSHAPE_BAR_LEFT:
      set_cursor_type(env, Qbar);
      break;
    default:
      set_cursor_type(env, Qt);
      break;
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

  while (term->elisp_code_first) {
    ElispCodeListNode *node = term->elisp_code_first;
    term->elisp_code_first = node->next;
    emacs_value elisp_code = env->make_string(env, node->code, node->code_len);
    vterm_eval(env, elisp_code);

    free(node->code);
    free(node);
  }
  term->elisp_code_p_insert = &term->elisp_code_first;

  if (term->selection_data) {
    emacs_value selection_target = env->make_string(
        env, &term->selection_target[0], strlen(&term->selection_target[0]));
    emacs_value selection_data = env->make_string(env, term->selection_data,
                                                  strlen(term->selection_data));
    vterm_selection(env, selection_target, selection_data);
    free(term->selection_data);
    term->selection_data = NULL;
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

/* str1=concat(str1,str2,str2_len,true); */
/* str1 can be NULL */
static char *concat(char *str1, const char *str2, size_t str2_len,
                    bool free_str1) {
  if (str1 == NULL) {
    str1 = malloc(str2_len + 1);
    memcpy(str1, str2, str2_len);
    str1[str2_len] = '\0';
    return str1;
  }
  size_t str1_len = strlen(str1);
  char *buf = malloc(str1_len + str2_len + 1);
  memcpy(buf, str1, str1_len);
  memcpy(&buf[str1_len], str2, str2_len);
  buf[str1_len + str2_len] = '\0';
  if (free_str1) {
    free(str1);
  }
  return buf;
}
static void term_set_title(Term *term, const char *title, size_t len,
                           bool initial, bool final) {
  if (term->title && initial) {
    free(term->title);
    term->title = NULL;
    term->title_changed = false;
  }
  term->title = concat(term->title, title, len, true);
  if (final) {
    term->title_changed = true;
  }
  return;
}

static int term_settermprop(VTermProp prop, VTermValue *val, void *user_data) {
  Term *term = (Term *)user_data;
  switch (prop) {
  case VTERM_PROP_CURSORVISIBLE:
    invalidate_terminal(term, term->cursor.row, term->cursor.row + 1);
    term->cursor.cursor_visible = val->boolean;
    term->cursor.cursor_type_changed = true;
    break;
  case VTERM_PROP_CURSORBLINK:
    if (term->ignore_blink_cursor)
      break;
    invalidate_terminal(term, term->cursor.row, term->cursor.row + 1);
    term->cursor.cursor_blink = val->boolean;
    term->cursor.cursor_blink_changed = true;
    break;
  case VTERM_PROP_CURSORSHAPE:
    invalidate_terminal(term, term->cursor.row, term->cursor.row + 1);
    term->cursor.cursor_type = val->number;
    term->cursor.cursor_type_changed = true;
    break;
  case VTERM_PROP_TITLE:
#ifdef VTermStringFragmentNotExists
    term_set_title(term, val->string, strlen(val->string), true, true);
#else
    term_set_title(term, val->string.str, val->string.len, val->string.initial,
                   val->string.final);
#endif
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

  emacs_value fg = cell_rgb_color(env, term, cell, true);
  emacs_value bg = cell_rgb_color(env, term, cell, false);
  /* With vterm-disable-bold-font, vterm-disable-underline,
   * vterm-disable-inverse-video, users can disable some text properties.
   * Here, we check whether the text would require adding such properties.
   * In case it does, and the user does not disable the attribute, we later
   * append the property to the list props.  If the text does not require
   * such property, or the user disable it, we set the variable to nil.
   * Properties that are marked as nil are not added to the text. */
  emacs_value bold =
      cell->attrs.bold && !term->disable_bold_font ? Qbold : Qnil;
  emacs_value underline =
      cell->attrs.underline && !term->disable_underline ? Qt : Qnil;
  emacs_value italic = cell->attrs.italic ? Qitalic : Qnil;
  emacs_value reverse =
      cell->attrs.reverse && !term->disable_inverse_video ? Qt : Qnil;
  emacs_value strike = cell->attrs.strike ? Qt : Qnil;

  // TODO: Blink, font, dwl, dhl is missing
  int emacs_major_version =
      env->extract_integer(env, symbol_value(env, Qemacs_major_version));
  emacs_value properties;
  emacs_value props[64];
  int props_len = 0;
  if (env->is_not_nil(env, fg))
    props[props_len++] = Qforeground, props[props_len++] = fg;
  if (env->is_not_nil(env, bg))
    props[props_len++] = Qbackground, props[props_len++] = bg;
  if (bold != Qnil)
    props[props_len++] = Qweight, props[props_len++] = bold;
  if (underline != Qnil)
    props[props_len++] = Qunderline, props[props_len++] = underline;
  if (italic != Qnil)
    props[props_len++] = Qslant, props[props_len++] = italic;
  if (reverse != Qnil)
    props[props_len++] = Qreverse, props[props_len++] = reverse;
  if (strike != Qnil)
    props[props_len++] = Qstrike, props[props_len++] = strike;
  if (emacs_major_version >= 27)
    props[props_len++] = Qextend, props[props_len++] = Qt;

  properties = list(env, props, props_len);

  if (props_len)
    put_text_property(env, text, Qface, properties);

  return text;
}
static emacs_value render_prompt(emacs_env *env, emacs_value text) {

  emacs_value properties;

  properties =
      list(env, (emacs_value[]){Qvterm_prompt, Qt, Qrear_nonsticky, Qt}, 4);

  add_text_properties(env, text, properties);

  return text;
}

static emacs_value render_fake_newline(emacs_env *env, Term *term) {

  emacs_value text;
  text = env->make_string(env, "\n", 1);

  emacs_value properties;

  properties =
      list(env, (emacs_value[]){Qvterm_line_wrap, Qt, Qrear_nonsticky, Qt}, 4);

  add_text_properties(env, text, properties);

  return text;
}

static emacs_value cell_rgb_color(emacs_env *env, Term *term,
                                  VTermScreenCell *cell, bool is_foreground) {
  VTermColor *color = is_foreground ? &cell->fg : &cell->bg;

  /** NOTE: -10 is used as index offset for special indexes,
   * see C-h f vterm--get-color RET
   */
  if (VTERM_COLOR_IS_DEFAULT_FG(color)) {
    return vterm_get_color(env, -1 + (cell->attrs.underline ? -10 : 0));
  }
  if (VTERM_COLOR_IS_DEFAULT_BG(color)) {
    return vterm_get_color(env, -2 + (cell->attrs.reverse ? -10 : 0));
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
  } else if (is_key(key, len, "<kp-add>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_KP_PLUS, modifier);
  } else if (is_key(key, len, "<kp-subtract>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_KP_MINUS, modifier);
  } else if (is_key(key, len, "<kp-multiply>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_KP_MULT, modifier);
  } else if (is_key(key, len, "<kp-divide>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_KP_DIVIDE, modifier);
  } else if (is_key(key, len, "<kp-equal>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_KP_EQUAL, modifier);
  } else if (is_key(key, len, "<kp-decimal>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_KP_PERIOD, modifier);
  } else if (is_key(key, len, "<kp-separator>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_KP_COMMA, modifier);
  } else if (is_key(key, len, "<kp-enter>")) {
    vterm_keyboard_key(term->vt, VTERM_KEY_KP_ENTER, modifier);
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

  while (term->elisp_code_first) {
    ElispCodeListNode *node = term->elisp_code_first;
    term->elisp_code_first = node->next;
    free(node->code);
    free(node);
  }
  term->elisp_code_p_insert = &term->elisp_code_first;

  if (term->cmd_buffer) {
    free(term->cmd_buffer);
    term->cmd_buffer = NULL;
  }
  if (term->selection_data) {
    free(term->selection_data);
    term->selection_data = NULL;
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
  free(term->lines);
  vterm_free(term->vt);
  free(term);
}

static int handle_osc_cmd_51(Term *term, char subCmd, char *buffer) {
  if (subCmd == 'A') {
    /* "51;A" sets the current directory */
    /* "51;A" has also the role of identifying the end of the prompt */
    if (term->directory != NULL) {
      free(term->directory);
      term->directory = NULL;
    }
    term->directory = malloc(strlen(buffer) + 1);
    strcpy(term->directory, buffer);
    term->directory_changed = true;

    for (int i = term->cursor.row; i < term->lines_len; i++) {
      if (term->lines[i] == NULL) {
        term->lines[i] = alloc_lineinfo();
      }

      if (term->lines[i]->directory != NULL) {
        free(term->lines[i]->directory);
      }
      term->lines[i]->directory = malloc(strlen(buffer) + 1);
      strcpy(term->lines[i]->directory, buffer);
      if (i == term->cursor.row) {
        term->lines[i]->prompt_col = term->cursor.col;
      } else {
        term->lines[i]->prompt_col = -1;
      }
    }
    return 1;
  } else if (subCmd == 'E') {
    /* "51;E" executes elisp code */
    /* The elisp code is executed in term_redraw */
    ElispCodeListNode *node = malloc(sizeof(ElispCodeListNode));
    node->code_len = strlen(buffer);
    node->code = malloc(node->code_len + 1);
    strcpy(node->code, buffer);
    node->next = NULL;

    *(term->elisp_code_p_insert) = node;
    term->elisp_code_p_insert = &(node->next);
    return 1;
  }
  return 0;
}
static int handle_osc_cmd_52(Term *term, char *buffer) {
  /* OSC 52 ; Pc ; Pd BEL */
  /* Manipulate Selection Data  */
  /* https://invisible-island.net/xterm/ctlseqs/ctlseqs.html */
  /* test by printf "\033]52;c;$(printf "%s" "blabla" | base64)\a" */

  for (int i = 0; i < SELECTION_TARGET_MAX; i++) { /* reset Pc */
    term->selection_target[i] = 0;
  }
  int selection_target_idx = 0;
  size_t cmdlen = strlen(buffer);

  for (int i = 0; i < cmdlen; i++) {
    /* OSC 52 ; Pc ; Pd BEL */
    if (buffer[i] == ';') { /* find the second ";" */
      term->selection_data = malloc(cmdlen - i);
      strcpy(term->selection_data, &buffer[i + 1]);
      break;
    }
    if (selection_target_idx < SELECTION_TARGET_MAX) {
      /* c , p , q , s , 0 , 1 , 2 , 3 , 4 , 5 , 6 , and 7 */
      /* for clipboard, primary, secondary, select, or cut buffers 0 through 7
       * respectively */
      term->selection_target[selection_target_idx] = buffer[i];
      selection_target_idx++;
    } else { /* len of Pc should not >12 just ignore this cmd,am I wrong? */
      return 0;
    }
  }
  return 1;
}
static int handle_osc_cmd(Term *term, int cmd, char *buffer) {
  if (cmd == 51) {
    char subCmd = '0';
    if (strlen(buffer) == 0) {
      return 0;
    }
    subCmd = buffer[0];
    /* ++ skip the subcmd char */
    return handle_osc_cmd_51(term, subCmd, ++buffer);
  } else if (cmd == 52) {
    return handle_osc_cmd_52(term, buffer);
  }
  return 0;
}
#ifdef VTermStringFragmentNotExists
static int osc_callback(const char *command, size_t cmdlen, void *user) {
  Term *term = (Term *)user;
  char buffer[cmdlen + 1];
  buffer[cmdlen] = '\0';
  memcpy(buffer, command, cmdlen);

  if (cmdlen > 4 && buffer[0] == '5' && buffer[1] == '1' && buffer[2] == ';' &&
      buffer[3] == 'A') {
    return handle_osc_cmd_51(term, 'A', &buffer[4]);
  } else if (cmdlen > 4 && buffer[0] == '5' && buffer[1] == '1' &&
             buffer[2] == ';' && buffer[3] == 'E') {
    return handle_osc_cmd_51(term, 'E', &buffer[4]);
  } else if (cmdlen > 4 && buffer[0] == '5' && buffer[1] == '2' &&
             buffer[2] == ';') {
    /* OSC 52 ; Pc ; Pd BEL */
    return handle_osc_cmd_52(term, &buffer[3]);
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
#else

static int osc_callback(int cmd, VTermStringFragment frag, void *user) {
  /* osc_callback (OSC = Operating System Command) */

  /* We interpret escape codes that start with "51;" */
  /* "51;A" sets the current directory */
  /* "51;A" has also the role of identifying the end of the prompt */
  /* "51;E" executes elisp code */
  /* The elisp code is executed in term_redraw */

  /* "52;[cpqs01234567];data" Manipulate Selection Data */
  /* I think libvterm has bug ,sometimes when the data is long enough ,the final
   * fragment is missed */
  /* printf "\033]52;c;$(printf "%s" $(ruby -e 'print "x"*999999')|base64)\a"
   */

  Term *term = (Term *)user;

  if (frag.initial) {
    /* drop old fragment,because this is a initial fragment */
    if (term->cmd_buffer) {
      free(term->cmd_buffer);
      term->cmd_buffer = NULL;
    }
  }

  if (!frag.initial && !frag.final && frag.len == 0) {
    return 0;
  }

  term->cmd_buffer = concat(term->cmd_buffer, frag.str, frag.len, true);
  if (frag.final) {
    handle_osc_cmd(term, cmd, term->cmd_buffer);
    free(term->cmd_buffer);
    term->cmd_buffer = NULL;
  }
  return 0;
}
static VTermStateFallbacks parser_callbacks = {
    .control = NULL,
    .csi = NULL,
    .osc = &osc_callback,
    .dcs = NULL,
};

#endif

emacs_value Fvterm_new(emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                       void *data) {
  Term *term = malloc(sizeof(Term));

  int rows = env->extract_integer(env, args[0]);
  int cols = env->extract_integer(env, args[1]);
  int sb_size = env->extract_integer(env, args[2]);
  int disable_bold_font = env->is_not_nil(env, args[3]);
  int disable_underline = env->is_not_nil(env, args[4]);
  int disable_inverse_video = env->is_not_nil(env, args[5]);
  int ignore_blink_cursor = env->is_not_nil(env, args[6]);

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
  term->disable_underline = disable_underline;
  term->disable_inverse_video = disable_inverse_video;
  term->ignore_blink_cursor = ignore_blink_cursor;
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
  term->cursor.cursor_type = -1;
  term->cursor.cursor_visible = true;
  term->cursor.cursor_type_changed = false;
  term->cursor.cursor_blink = false;
  term->cursor.cursor_blink_changed = false;
  term->directory = NULL;
  term->directory_changed = false;
  term->elisp_code_first = NULL;
  term->elisp_code_p_insert = &term->elisp_code_first;
  term->selection_data = NULL;

  term->cmd_buffer = NULL;

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
    if (nargs > 2 && env->is_not_nil(env, args[2]))
      modifier = modifier | VTERM_MOD_SHIFT;
    if (nargs > 3 && env->is_not_nil(env, args[3]))
      modifier = modifier | VTERM_MOD_ALT;
    if (nargs > 4 && env->is_not_nil(env, args[4]))
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

emacs_value Fvterm_reset_cursor_point(emacs_env *env, ptrdiff_t nargs,
                                      emacs_value args[], void *data) {
  Term *term = env->get_user_ptr(env, args[0]);
  int line = row_to_linenr(term, term->cursor.row);
  goto_line(env, line);
  goto_col(term, env, term->cursor.row, term->cursor.col);
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
  Qextend = env->make_global_ref(env, env->intern(env, ":extend"));
  Qemacs_major_version =
      env->make_global_ref(env, env->intern(env, "emacs-major-version"));
  Qvterm_line_wrap =
      env->make_global_ref(env, env->intern(env, "vterm-line-wrap"));
  Qrear_nonsticky =
      env->make_global_ref(env, env->intern(env, "rear-nonsticky"));
  Qvterm_prompt = env->make_global_ref(env, env->intern(env, "vterm-prompt"));

  Qface = env->make_global_ref(env, env->intern(env, "font-lock-face"));
  Qbox = env->make_global_ref(env, env->intern(env, "box"));
  Qbar = env->make_global_ref(env, env->intern(env, "bar"));
  Qhbar = env->make_global_ref(env, env->intern(env, "hbar"));
  Qcursor_type = env->make_global_ref(env, env->intern(env, "cursor-type"));

  // Functions
  Fblink_cursor_mode =
      env->make_global_ref(env, env->intern(env, "blink-cursor-mode"));
  Fsymbol_value = env->make_global_ref(env, env->intern(env, "symbol-value"));
  Flength = env->make_global_ref(env, env->intern(env, "length"));
  Flist = env->make_global_ref(env, env->intern(env, "list"));
  Fnth = env->make_global_ref(env, env->intern(env, "nth"));
  Ferase_buffer = env->make_global_ref(env, env->intern(env, "erase-buffer"));
  Finsert = env->make_global_ref(env, env->intern(env, "vterm--insert"));
  Fgoto_char = env->make_global_ref(env, env->intern(env, "goto-char"));
  Fput_text_property =
      env->make_global_ref(env, env->intern(env, "put-text-property"));
  Fadd_text_properties =
      env->make_global_ref(env, env->intern(env, "add-text-properties"));
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
  Fvterm_selection =
      env->make_global_ref(env, env->intern(env, "vterm--selection"));

  // Exported functions
  emacs_value fun;
  fun =
      env->make_function(env, 4, 7, Fvterm_new, "Allocate a new vterm.", NULL);
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
                           "Set the size of the terminal.", NULL);
  bind_function(env, "vterm--set-size", fun);

  fun = env->make_function(env, 2, 2, Fvterm_set_pty_name,
                           "Set the name of the pty.", NULL);
  bind_function(env, "vterm--set-pty-name", fun);
  fun = env->make_function(env, 2, 2, Fvterm_get_pwd,
                           "Get the working directory of at line n.", NULL);
  bind_function(env, "vterm--get-pwd-raw", fun);
  fun = env->make_function(env, 1, 1, Fvterm_reset_cursor_point,
                           "Reset cursor postion.", NULL);
  bind_function(env, "vterm--reset-point", fun);

  fun = env->make_function(env, 1, 1, Fvterm_get_icrnl,
                           "Get the icrnl state of the pty", NULL);
  bind_function(env, "vterm--get-icrnl", fun);

  provide(env, "vterm-module");

  return 0;
}
