#include "utf8.h"

size_t codepoint_to_utf8(const uint32_t codepoint, unsigned char buffer[4]) {
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

bool utf8_to_codepoint(const unsigned char buffer[4], const size_t len,
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
