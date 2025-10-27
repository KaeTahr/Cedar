type Student = record {
  "Student Number": U32 @ 0B endian LE,
  DOB:               U32 @ 12B,
  Grades: record {
    Maths:   U32 @ 8B  endian BE,
    Physics: U32 @ 12B endian BE
  } @ 8B
}
