#ifndef UTF8_H
#define UTF8_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

size_t codepoint_to_utf8(const uint32_t codepoint, unsigned char buffer[4]);
bool utf8_to_codepoint(const unsigned char buffer[4], const size_t len,
                       uint32_t *codepoint);

#endif /* UTF8_H */
