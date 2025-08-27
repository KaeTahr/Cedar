#include <string.h>
#include <inttypes.h>
#include "student_cedar.h"

/* Bit helpers (LE). For BE fields, byte-swap at the call sites. */
static inline uint64_t load_bits_le(const uint8_t* b, size_t bitOff, size_t bitLen) {
    size_t byteOff = bitOff >> 3;
    size_t endBit  = bitOff + bitLen;
    size_t endByte = (endBit + 7) >> 3;
    uint64_t acc = 0;
    for (size_t i = 0; i < endByte - byteOff && i < 8; ++i) {
        acc |= ((uint64_t)b[byteOff + i]) << (8*i);
    }
    acc >>= (bitOff & 7);
    if (bitLen >= 64) return acc;
    return acc & ((1ULL << bitLen) - 1ULL);
}

static inline void store_bits_le(uint8_t* b, size_t bitOff, size_t bitLen, uint64_t value) {
    size_t byteOff = bitOff >> 3;
    size_t endBit  = bitOff + bitLen;
    size_t endByte = (endBit + 7) >> 3;
    uint64_t v = value << (bitOff & 7);
    for (size_t i = 0; i < endByte - byteOff && i < 8; ++i) {
        uint8_t cur = b[byteOff + i];
        uint8_t newb = (uint8_t)(v >> (8*i));
        uint8_t mask = 0xFF;
        size_t bitStart = (byteOff + i) * 8;
        size_t bitEnd   = bitStart + 8;
        size_t coverS   = (bitOff > bitStart) ? (bitOff - bitStart) : 0;
        size_t coverE   = (endBit < bitEnd) ? (bitEnd - endBit) : 0;
        if (bitLen < 64) {
            uint8_t left  = (coverS >= 8) ? 0 : (uint8_t)(0xFF << coverS);
            uint8_t right = (coverE >= 8) ? 0 : (uint8_t)(0xFF >> coverE);
            mask = (uint8_t)(left & right);
        }
        cur &= ~mask;
        cur |= (newb & mask);
        b[byteOff + i] = cur;
    }
}


void* Student_get_name(const Student* s)
{
    uint64_t u = load_bits_le(s->_b, 0, 64);
    return (void*)(uintptr_t)u;
}

void Student_set_name(Student* s, const void* v)
{
    uintptr_t u = (uintptr_t)v;
    store_bits_le(s->_b, 0, 64, (uint64_t)u);
}

uint8_t Student_get_age(const Student* s)
{
    return (uint8_t)load_bits_le(s->_b, 64, 8);
}

void Student_set_age(Student* s, uint8_t v)
{
    store_bits_le(s->_b, 64, 8, (uint64_t)v);
}

uint8_t Student_get_maths(const Student* s)
{
    return (uint8_t)load_bits_le(s->_b, 72, 8);
}

void Student_set_maths(Student* s, uint8_t v)
{
    store_bits_le(s->_b, 72, 8, (uint64_t)v);
}

uint16_t Student_get_physics(const Student* s)
{
    return (uint16_t)(uint16_t)load_bits_le(s->_b, 80, 16);
}

void Student_set_physics(Student* s, uint16_t v)
{
    store_bits_le(s->_b, 80, 16, (uint64_t)v);
}

