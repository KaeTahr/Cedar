#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include "student.h"

typedef void     (*name_setter_t)(Student*, uint32_t);
typedef uint32_t (*name_getter_t)(const Student*);

// Oops didn't implement unified array accessor oh well
static const name_setter_t NAME_SET[16] = {
  d_set_name__0,  d_set_name__1,  d_set_name__2,  d_set_name__3,
  d_set_name__4,  d_set_name__5,  d_set_name__6,  d_set_name__7,
  d_set_name__8,  d_set_name__9,  d_set_name__10, d_set_name__11,
  d_set_name__12, d_set_name__13, d_set_name__14, d_set_name__15
};
static const name_getter_t NAME_GET[16] = {
  d_get_name__0,  d_get_name__1,  d_get_name__2,  d_get_name__3,
  d_get_name__4,  d_get_name__5,  d_get_name__6,  d_get_name__7,
  d_get_name__8,  d_get_name__9,  d_get_name__10, d_get_name__11,
  d_get_name__12, d_get_name__13, d_get_name__14, d_get_name__15
};

static void set_name(Student *s, const unsigned char bytes[16]) {
  for (int i = 0; i < 16; i++) NAME_SET[i](s, bytes[i]);
}
static void get_name(const Student *s, unsigned char out[16]) {
  for (int i = 0; i < 16; i++) out[i] = (unsigned char) NAME_GET[i](s);
}

static void hexdump_student(const Student *s) {
  // The backing storage is 24 bytes (6 * 4). Print bytewise.
  const unsigned char *p = (const unsigned char*) s->data;
  for (int i = 0; i < 24; i++) {
    printf("%02X%s", p[i], ((i+1)%8==0) ? "  " : " ");
  }
  printf("\n");
}

int main(void) {
  Student s = {0};

  // Fill name with 00..0F
  unsigned char name[16];
  for (int i = 0; i < 16; i++) name[i] = (unsigned char)i;
  set_name(&s, name);

  // id at 16–19 (LE)
  d_set_id(&s, 0x11223344u);  // expect bytes 44 33 22 11

  // grades at 20–23 (big-endian 16-bit each)
  // maths = 0xABCD -> bytes AB CD
  // physics = 0x0123 -> bytes 01 23
  d_set_grades__maths(&s, 0xABCDu);
  d_set_grades__physics(&s, 0x0123u);

  // Show backing bytes
  hexdump_student(&s);

  // Quick round-trip check for name
  unsigned char out[16] = {0};
  get_name(&s, out);
  int ok = memcmp(name, out, 16) == 0;
  printf("name round-trip %s\n", ok ? "OK" : "MISMATCH");

  // Show logical reads too
  printf("id      = 0x%08X\n", d_get_id(&s));
  printf("maths   = 0x%04X\n", (unsigned)d_get_grades__maths(&s));
  printf("physics = 0x%04X\n", (unsigned)d_get_grades__physics(&s));

  return ok ? 0 : 1;
}
