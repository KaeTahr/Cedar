#include <assert.h>
#include <stdio.h>
#include "student.h"

int main(void) {
  Student s = {0};

  // 1) Round-trip tests
  d_set_Student_Number(&s, 0x12345678U);
  assert(d_get_Student_Number(&s) == 0x12345678U);

  d_set_DOB(&s, 0xA1B2C3D4U);
  assert(d_get_DOB(&s) == 0xA1B2C3D4U);

  d_set_Grades__Maths(&s, 0x01020304U);
  assert(d_get_Grades__Maths(&s) == 0x01020304U);

  d_set_Grades__Physics(&s, 0xFEEDBEEFU);
  assert(d_get_Grades__Physics(&s) == 0xFEEDBEEFU);

  // 2) Spot-check lane bits (optional)
 printf("%08x %08x %08x %08x %08x\n", s.data[0], s.data[1], s.data[2], s.data[3], s.data[4]);

  puts("OK");
  return 0;
}
