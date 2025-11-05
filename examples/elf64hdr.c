// Adapted from the ELF specification: https://refspecs.linuxfoundation.org/elf/gabi4+/ch4.eheader.html
// types are simplified for the example
#include <stdint.h>

typedef struct Mirror_Elf64_Ehdr {
  uint8_t   e_ident[16];
  uint16_t  e_type;
  uint16_t  e_machine;
  uint32_t  e_version;
  uint64_t  e_entry;
  uint64_t  e_phoff;
  uint64_t  e_shoff;
  uint32_t  e_flags;
  uint16_t  e_ehsize;
  uint16_t  e_phentsize;
  uint16_t  e_phnum;
  uint16_t  e_shentsize;
  uint16_t  e_shnum;
  uint16_t  e_shstrndx;
} Mirror_Elf64_Ehdr;

volatile Mirror_Elf64_Ehdr _force_elf64_ehdr;
int force_use(void) {
  return sizeof(Mirror_Elf64_Ehdr)
       + _force_elf64_ehdr.e_ident[0]
       + _force_elf64_ehdr.e_type;
}