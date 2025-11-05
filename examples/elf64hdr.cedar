layout Elf64_Ehdr = record {
  e_ident     : U8[16] @ 0B,

  e_type      : U16    @ 16B endian LE,
  e_machine   : U16    @ 18B endian LE,
  e_version   : U32    @ 20B endian LE,

  e_entry     : U64    @ 24B endian LE,
  e_phoff     : U64    @ 32B endian LE,
  e_shoff     : U64    @ 40B endian LE,

  e_flags     : U32    @ 48B endian LE,

  e_ehsize    : U16    @ 52B endian LE,
  e_phentsize : U16    @ 54B endian LE,
  e_phnum     : U16    @ 56B endian LE,
  e_shentsize : U16    @ 58B endian LE,
  e_shnum     : U16    @ 60B endian LE,
  e_shstrndx  : U16    @ 62B endian LE
} @ 0B;
