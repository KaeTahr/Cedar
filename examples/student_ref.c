#include <stdint.h>

struct Grades {
  uint16_t maths;
  uint16_t physics;
};

struct Student {
  uint8_t  name[16];
  uint32_t id;
  struct Grades grades;
};

volatile struct Student _force_student;
int force_use(void) { return sizeof(struct Student) + _force_student.id; }
