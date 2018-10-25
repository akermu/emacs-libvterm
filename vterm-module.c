#include "vterm-module.h"
#include "elisp.h"
#include "utf8.h"
#include <string.h>
#include <unistd.h>
#include <limits.h>
#include <vterm.h>
#include <assert.h>

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
  }

  // New row is added at the start of the storage buffer.
  term->sb_buffer[0] = sbrow;
  if (term->sb_current < term->sb_size) {
    term->sb_current++;
  }

  if (term->sb_pending < (int)term->sb_size) {
    term->sb_pending++;
  }

  memcpy(sbrow->cells, cells, sizeof(cells[0]) * c);

  return 1;
}
/// Scrollback pop handler (from pangoterm).
///
/// @param cols
/// @param cells  VTerm state to update.
/// @param data   Term
static int term_sb_pop(int cols, VTermScreenCell *cells, void *data){
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
  size_t col ;
  for (col = cols_to_copy; col < (size_t)cols; col++) {
    cells[col].chars[0] = 0;
    cells[col].width = 1;
  }

  free(sbrow);

  return 1;
}

static int row_to_linenr(Term *term, int row){
  return row != INT_MAX ? row + (int)term->sb_current + 1 : INT_MAX;
}

static int linenr_to_row(Term *term, int linenr){
  return linenr - (int)term->sb_current - 1;
}

static void fetch_cell(Term *term, int row, int col,
                       VTermScreenCell *cell){
  if (row < 0) {
    ScrollbackLine *sbrow = term->sb_buffer[-row - 1];
    if ((size_t)col < sbrow->cols) {
      *cell = sbrow->cells[col];
    } else {
      // fill the pointer with an empty cell
      VTermColor fg, bg;
      VTermState *state = vterm_obtain_state(term->vt);
      vterm_state_get_default_colors(state, &fg, &bg);

      *cell = (VTermScreenCell) {
                                 .chars = { 0 },
                                 .width = 1,
                                 .bg = bg
      };
    }
  } else {
    vterm_screen_get_cell(term->vts, (VTermPos){.row = row, .col = col},
                          cell);
  }
}

static size_t get_col_offset(Term *term, int row, int end_col){
  int col = 0;
  size_t offset = 0;
  unsigned char  buf[4];

  while (col < end_col) {
    VTermScreenCell cell;
    fetch_cell(term, row, col, &cell);
    if (cell.chars[0]) {
      if(cell.width>1){
        offset+=cell.width-1;
      }
    }
    col += cell.width;

  }
  return offset;
}

static size_t refresh_row(Term *term,emacs_env* env, int row,
                          int end_col,bool append_newline){
  int  j;
  char *ptr = term->textbuf;
  int length = 0;
  VTermScreenCell cell;
  VTermScreenCell lastCell;
  fetch_cell(term, row, 0, &lastCell);

  for (j = 0; j < end_col; j++) {
    VTermPos pos = {.row = row, .col = j};
    fetch_cell(term, row, j, &cell);

    if (!compare_cells(&cell, &lastCell)) {
      ptr[length]='\0';
      emacs_value text = render_text(env, ptr, length, &lastCell);
      insert(env,text);
      ptr+=length;
      length = 0;
    }

    lastCell = cell;
    if (cell.chars[0] == 0) {
      ptr[length] = ' ';
      length++;
    } else {
      unsigned char bytes[4];
      size_t count = codepoint_to_utf8(cell.chars[0], bytes);
      int k;
      for (k = 0; k < count; k++) {
        ptr[length] = bytes[k];
        length++;
      }
    }

    if (cell.width > 1) {
      int w = cell.width - 1;
      j = j + w;
    }

  }
  if(length>0){
    emacs_value text = render_text(env, ptr, length, &lastCell);
    insert(env,text);
    ptr+=length;
  }
  if(append_newline){
    *ptr='\n';
    ptr+=1;
    insert(env,env->make_string(env, "\n", 1));
  }
  *ptr=0;
  return ptr-term->textbuf;
}

// Refresh the screen (visible part of the buffer when the terminal is
// focused) of a invalidated terminal
static void refresh_screen(Term *term,emacs_env *env){
  int height;
  int width;
  vterm_get_size(term->vt, &height, &width);
  // Term height may have decreased before `invalid_end` reflects it.
  /* refresh full screen now  */
  /* TODO: only refresh invalid lines */
  term->invalid_start = 0;
  term->invalid_end = height;

  int line_start=row_to_linenr(term, term->invalid_start);
  goto_line(env,line_start-1);
  int liner ;
  int r;
  for (r = term->invalid_start; r < term->invalid_end; r++) {
    liner =row_to_linenr(term,r);
    int buffer_lnum = env->extract_integer(env, buffer_line_number(env));
    if(liner>buffer_lnum){
      goto_line(env,buffer_lnum); /* maybe should goto end of buffer */
      int i;
      for (i = 0; i < liner-buffer_lnum; i++){
        insert(env,env->make_string(env,"\n",1));
      }
    }
    delete_lines(env,liner,1,false);
    goto_line(env,liner);
    refresh_row(term,env,r,width,false);
  }
  term->invalid_start = INT_MAX;
  term->invalid_end = -1;
}


// Refresh the scrollback of an invalidated terminal.
static void refresh_scrollback(Term *term,emacs_env *env) {
  int width, height;
  int buffer_lnum ;
  vterm_get_size(term->vt, &height, &width);

  while (term->sb_pending > 0) {
    // This means that either the window height has decreased or the screen
    // became full and libvterm had to push all rows up. Convert the first
    // pending scrollback row into a string and append it just above the visible
    // section of the buffer
    buffer_lnum = env->extract_integer(env, buffer_line_number(env));

    if ((buffer_lnum- height) >= (int)term->sb_size) {
      // scrollback full, delete lines at the top
      delete_lines(env,1, 1,true);
      /* insert(env,env->make_string(env, "\n", 1)); */
    }
    buffer_lnum = env->extract_integer(env, buffer_line_number(env));
    int buf_index = buffer_lnum- height+1;
    goto_line(env,buf_index);
    size_t length=refresh_row(term,env,-term->sb_pending,width,true);
    term->sb_pending--;
  }
  // Remove extra lines at the bottom
  int max_line_count = (int)term->sb_current + height;
  buffer_lnum=env->extract_integer(env, buffer_line_number(env));

  // Remove extra lines at the bottom
  if (buffer_lnum> max_line_count) {
    delete_lines(env,max_line_count,buffer_lnum-max_line_count, true);
  }
}

static void adjust_topline(Term *term,emacs_env *env, long added){
  int height, width;
  vterm_get_size(term->vt, &height, &width);
  /* FOR_ALL_WINDOWS_IN_TAB(wp, curtab) { */
  int buffer_lnum = env->extract_integer(env, buffer_line_number(env));
  VTermState *state = vterm_obtain_state(term->vt);
  VTermPos pos;
  vterm_state_get_cursorpos(state, &pos);
  int cursor_lnum=row_to_linenr(term,pos.row);
  bool following = buffer_lnum ==cursor_lnum + added;  // cursor at end?


  if (following ) {             /* || (wp == curwin && is_focused(term)) */
    // "Follow" the terminal output
    goto_line(env,MIN(cursor_lnum,buffer_lnum));
    size_t offset=get_col_offset(term,pos.row,pos.col);
    forward_char(env,env->make_integer(env,pos.col-offset));
    recenter(env,env->make_integer(env,-1)); /* make current lien at the screen bottom */

  } else {
    goto_line(env,MIN(cursor_lnum,buffer_lnum));
    size_t offset=get_col_offset(term,pos.row,pos.col);
    forward_char(env,env->make_integer(env,pos.col-offset));
    recenter(env,env->make_integer(env,pos.row));
  }
}
static void term_redraw(Term *term,emacs_env *env) {
  long ml_before = env->extract_integer(env, buffer_line_number(env));
  refresh_scrollback(term,env);
  refresh_screen(term,env);
  long ml_added = env->extract_integer(env, buffer_line_number(env))- ml_before;
  adjust_topline(term,env, ml_added);
}

static VTermScreenCallbacks vterm_screen_callbacks = {
  /* .damage      = term_damage, */
  /* .moverect    = term_moverect, */
  /* .movecursor  = term_movecursor, */
  .settermprop = term_settermprop,
  /* .bell        = term_bell, */
  .sb_pushline = term_sb_push,
  .sb_popline  = term_sb_pop,
};


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

static int term_settermprop(VTermProp prop, VTermValue *val, void *user_data) {
  Term *term = (Term *)user_data;
  switch (prop) {
  case VTERM_PROP_CURSORVISIBLE:
    term->cursor_visible = val->boolean;
    break;
  default:
    return 0;
  }

  return 1;
}

static emacs_value render_text(emacs_env *env, char *buffer, int len,
                               VTermScreenCell *cell) {
  emacs_value text;
  if(len==0){
     text= env->make_string(env, "", 0);
     return text;
     /* return NULL; */
  }else{
     text= env->make_string(env, buffer, len);
  }

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
  int point = ((row+term->sb_current) * (cols + 1)) + col + 1 + offset;
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
  int sb_size = env->extract_integer(env, args[2]);

  term->vt = vterm_new(rows, cols);
  vterm_set_utf8(term->vt, 1);

  term_setup_colors(term, env);


  term->vts = vterm_obtain_screen(term->vt);
  vterm_screen_reset(term->vts , 1);
  vterm_screen_set_callbacks(term->vts , &vterm_screen_callbacks, term);
  vterm_screen_set_damage_merge(term->vts , VTERM_DAMAGE_SCROLL);
  term->sb_size = MIN(SB_MAX,sb_size);
  term->sb_current = 0;
  term->sb_pending = 0;
  term->sb_buffer = malloc(sizeof(ScrollbackLine *) * term->sb_size);
  term->invalid_start=0;
  term->invalid_end=cols;



  return env->make_user_ptr(env, term_finalize, term);
}

static emacs_value Fvterm_update(emacs_env *env, ptrdiff_t nargs,
                                 emacs_value args[], void *data) {
  Term *term = env->get_user_ptr(env, args[0]);

  toggle_cursor(env, term->cursor_visible);

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
    term_redraw(term,env);
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
  Fforward_line = env->make_global_ref(env, env->intern(env, "forward-line"));
  Fgoto_line = env->make_global_ref(env, env->intern(env, "vterm--goto-line"));
  Fbuffer_line_number = env->make_global_ref(env, env->intern(env, "vterm--buffer-line-num"));
  Fdelete_lines = env->make_global_ref(env, env->intern(env, "vterm--delete-lines"));
  Frecenter = env->make_global_ref(env,env->intern(env, "vterm--recenter"));
  Fforward_char = env->make_global_ref(env,env->intern(env, "vterm--forward-char"));




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
      env->make_function(env, 3, 3, Fvterm_new, "Allocates a new vterm.", NULL);
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

