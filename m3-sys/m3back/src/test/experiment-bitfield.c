#include <stdio.h>

unsigned extract_out_bounds(void);

#define BITS_OF(T) ((sizeof(T) * 8))
#define EXTRACT(T, VALUE, OFFSET, SIZE)   \
    (((OFFSET + SIZE) > BITS_OF(T)) ? extract_out_bounds() : \
    (OFFSET ? (((struct { T : OFFSET; unsigned T value: SIZE; }*)&VALUE)->value) \
            : (((struct { unsigned T value: SIZE; }*)&VALUE)->value)))
#define X(type, value, offset, size) do {               \
result = EXTRACT(type, value, offset, size);            \
if (result)                                             \
    printf("%s %llX[%X:%X] => %llX\n", #type,           \
           (unsigned long long)value, (unsigned)offset, (unsigned)size, \
           result); \
} while(0)
#define Size8(type, value, offset, initial_size)  do { \
    if ((offset + initial_size) <= BITS_OF(type)) { X(type, value, offset, initial_size); \
      if ((offset + initial_size + 1) <= BITS_OF(type)) { X(type, value, offset, initial_size + 1); \
        if ((offset + initial_size + 2) <= BITS_OF(type)) { X(type, value, offset, initial_size + 2); \
          if ((offset + initial_size + 3) <= BITS_OF(type)) { X(type, value, offset, initial_size + 3); \
            if ((offset + initial_size + 4) <= BITS_OF(type)) { X(type, value, offset, initial_size + 4); \
              if ((offset + initial_size + 5) <= BITS_OF(type)) { X(type, value, offset, initial_size + 5); \
                if ((offset + initial_size + 6) <= BITS_OF(type)) { X(type, value, offset, initial_size + 6); \
                  if ((offset + initial_size + 7) <= BITS_OF(type)) { X(type, value, offset, initial_size + 7); \
    } } } } } } } } } while(0)
#define Size16(type, value, offset, initial_size)  do { \
    Size8(type, value, offset, initial_size); \
    Size8(type, value, offset, initial_size + 8); \
} while(0)
#define Size32(type, value, offset, initial_size)  do { \
    Size16(type, value, offset, initial_size); \
    Size16(type, value, offset, initial_size + 16); \
} while(0)
#define Size64(type, value, offset, initial_size)  do { \
    Size32(type, value, offset, initial_size); \
    Size32(type, value, offset, initial_size + 32); \
} while(0)
#define Offset8(type, expand_size_macro, value, initial_offset, initial_size)  do { \
    expand_size_macro(type, value, initial_offset, initial_size); \
    expand_size_macro(type, value, initial_offset + 1, initial_size); \
    expand_size_macro(type, value, initial_offset + 2, initial_size); \
    expand_size_macro(type, value, initial_offset + 3, initial_size); \
    expand_size_macro(type, value, initial_offset + 4, initial_size); \
    expand_size_macro(type, value, initial_offset + 5, initial_size); \
    expand_size_macro(type, value, initial_offset + 6, initial_size); \
    expand_size_macro(type, value, initial_offset + 7, initial_size); \
} while(0)
#define Offset16(type, expand_size_macro, value, initial_offset, initial_size)  do { \
    Offset8(type, expand_size_macro, value, initial_offset, initial_size); \
    Offset8(type, expand_size_macro, value, initial_offset + 8, initial_size); \
} while(0)
#define Offset32(type, expand_size_macro, value, initial_offset, initial_size)  do { \
    Offset16(type, expand_size_macro, value, initial_offset, initial_size); \
    Offset16(type, expand_size_macro, value, initial_offset + 16, initial_size); \
} while(0)
#define Offset64(type, expand_size_macro, value, initial_offset, initial_size)  do { \
    Offset32(type, expand_size_macro, value, initial_offset, initial_size); \
    Offset32(type, expand_size_macro, value, initial_offset + 32, initial_size); \
} while(0)

int main()
{
    unsigned long long result = { 0 };
    long long value64 = { 0 };
    int value32 = { 0 };
    short value16 = { 0 };
    unsigned char value8 = { 0 };

#if 0
    for (value64 = 1; value64 < 10; ++value64)
    {
        Offset64(long long, Size64, value64, 0, 1);
        Offset32(int, Size32, value64, 0, 1);
        Offset16(short, Size16, value64, 0, 1);
        Offset8(char, Size8, value64, 0, 1);
    }
    for (value64 = -1; value64; value64 <<= 1)
    {
        Offset64(long long, Size64, value64, 0, 1);
        Offset32(int, Size32, value64, 0, 1);
        Offset16(short, Size16, value64, 0, 1);
        Offset8(char, Size8, value64, 0, 1);
    }
#endif
    for (value64 = 1; value64; value64 <<= 1)
    {
        Offset64(long long, Size64, value64, 0, 1);
        Offset32(int, Size32, value64, 0, 1);
        Offset16(short, Size16, value64, 0, 1);
        Offset8(char, Size8, value64, 0, 1);
    }
    for (value64 = 3; value64; value64 <<= 1)
    {
        Offset64(long long, Size64, value64, 0, 1);
        Offset32(int, Size32, value64, 0, 1);
        Offset16(short, Size16, value64, 0, 1);
        Offset8(char, Size8, value64, 0, 1);
    }
    for (value64 = 5; value64; value64 <<= 1)
    {
        Offset64(long long, Size64, value64, 0, 1);
        Offset32(int, Size32, value64, 0, 1);
        Offset16(short, Size16, value64, 0, 1);
        Offset8(char, Size8, value64, 0, 1);
    }
#if 0
    for (value64 = -1; value64 > -10; --value64)
    {
        Offset64(long long, Size64, value64, 0, 1);
        Offset32(int, Size32, value64, 0, 1);
        Offset16(short, Size16, value64, 0, 1);
        Offset8(char, Size8, value64, 0, 1);
    }
    for (value32 = 1; value32 < 10; ++value32)
    {
        Offset32(int, Size32, value32, 0, 1);
        Offset16(short, Size16, value32, 0, 1);
        Offset8(char, Size8, value32, 0, 1);
    }
#endif
    for (value32 = -1; value32; value32 <<= 1)
    {
        Offset32(int, Size32, value32, 0, 1);
        Offset16(short, Size16, value32, 0, 1);
        Offset8(char, Size8, value32, 0, 1);
    }
    for (value32 = 1; value32; value32 <<= 1)
    {
        Offset32(int, Size32, value32, 0, 1);
        Offset16(short, Size16, value32, 0, 1);
        Offset8(char, Size8, value32, 0, 1);
    }
    for (value32 = 3; value32; value32 <<= 1)
    {
        Offset32(int, Size32, value32, 0, 1);
        Offset16(short, Size16, value32, 0, 1);
        Offset8(char, Size8, value32, 0, 1);
    }
    for (value32 = 5; value32; value32 <<= 1)
    {
        Offset32(int, Size32, value32, 0, 1);
        Offset16(short, Size16, value32, 0, 1);
        Offset8(char, Size8, value32, 0, 1);
    }
#if 0
    for (value32 = -1; value32 > -10; --value32)
    {
        Offset32(int, Size32, value32, 0, 1);
        Offset16(short, Size16, value32, 0, 1);
        Offset8(char, Size8, value32, 0, 1);
    }
    for (value16 = 1; value16 < 10; ++value16)
    {
        Offset16(short, Size16, value16, 0, 1);
        Offset8(char, Size8, value16, 0, 1);
    }
    for (value16 = -1; value16 > -10; --value16)
    {
        Offset16(short, Size16, value16, 0, 1);
        Offset8(char, Size8, value16, 0, 1);
    }
#endif
    for (value16 = -1; value16; value16 <<= 1)
    {
        Offset16(short, Size16, value16, 0, 1);
        Offset8(char, Size8, value16, 0, 1);
    }
    for (value16 = 1; value16; value16 <<= 1)
    {
        Offset16(short, Size16, value16, 0, 1);
        Offset8(char, Size8, value16, 0, 1);
    }
    for (value16 = 3; value16; value16 <<= 1)
    {
        Offset16(short, Size16, value16, 0, 1);
        Offset8(char, Size8, value16, 0, 1);
    }
    for (value16 = 5; value16; value16 <<= 1)
    {
        Offset16(short, Size16, value16, 0, 1);
        Offset8(char, Size8, value16, 0, 1);
    }
    for (value8 = 1; value8 < 255; ++value8)
        Offset8(char, Size8, value8, 0, 1);
    for (value8 = 1; value8; value8 <<= 1)
        Offset8(char, Size8, value8, 0, 1);
    for (value8 = 3; value8; value8 <<= 1)
        Offset8(char, Size8, value8, 0, 1);
    for (value8 = 5; value8; value8 <<= 1)
        Offset8(char, Size8, value8, 0, 1);

    value16 = 0x1234;
    X(short, value16, 0, 4);
    X(short, value16, 4, 4);
    X(short, value16, 8, 4);
    X(short, value16, 12, 4);

    value32 = 0x12345678;
    X(int, value32, 0, 4);
    X(int, value32, 4, 4);
    X(int, value32, 8, 4);
    X(int, value32, 12, 4);
    X(int, value32, 12, 4);
    X(int, value32, 12, 4);
    X(int, value32, 12, 4);

    return 0;
}
