layout Before = record {
  a : 1B @ 2B,
  b : 1B before a,
  c : 1B before a 1B
} @ 0B;
