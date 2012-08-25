
#include <stdio.h>

#define EXTRACT(T, VALUE, OFFSET, SIZE) \
 (OFFSET ? (((struct { T : OFFSET; unsigned T value: SIZE; }*)&VALUE)->value) \
         : (((struct { unsigned T value: SIZE; }*)&VALUE)->value))

#define X(type, offset, size) printf("%s %llX[%X:%X] => %X\n", #type, (unsigned long long)value[2], (unsigned)offset, (unsigned)size, (unsigned)EXTRACT(type, value[2], offset, size))
#define Size8(type, offset, initial_size) \
        X(type, offset, initial_size); \
        X(type, offset, initial_size + 1); \
        X(type, offset, initial_size + 2); \
        X(type, offset, initial_size + 3); \
        X(type, offset, initial_size + 4); \
        X(type, offset, initial_size + 5); \
        X(type, offset, initial_size + 6); \
        X(type, offset, initial_size + 7)
#define Size16(type, offset, initial_size) \
        Size8(type, offset, initial_size); \
        Size8(type, offset, initial_size + 8)
#define Size32(type, offset, initial_size) \
        Size16(type, offset, initial_size); \
        Size16(type, offset, initial_size + 16)
#define Size64(type, offset, initial_size) \
        Size32(type, offset, initial_size); \
        Size32(type, offset, initial_size + 32)
#define Offset8(type, expand_size_macro, initial_offset, initial_size) \
        expand_size_macro(type, initial_offset, initial_size); \
        expand_size_macro(type, initial_offset + 1, initial_size); \
        expand_size_macro(type, initial_offset + 2, initial_size); \
        expand_size_macro(type, initial_offset + 3, initial_size); \
        expand_size_macro(type, initial_offset + 4, initial_size); \
        expand_size_macro(type, initial_offset + 5, initial_size); \
        expand_size_macro(type, initial_offset + 6, initial_size); \
        expand_size_macro(type, initial_offset + 7, initial_size)
#define Offset16(type, expand_size_macro, initial_offset, initial_size) \
        Offset8(type, expand_size_macro, initial_offset, initial_size); \
        Offset8(type, expand_size_macro, initial_offset + 8, initial_size)
#define Offset32(type, expand_size_macro, initial_offset, initial_size) \
        Offset16(type, expand_size_macro, initial_offset, initial_size); \
        Offset16(type, expand_size_macro, initial_offset + 16, initial_size)
#define Offset64(type, expand_size_macro, initial_offset, initial_size) \
        Offset32(type, expand_size_macro, initial_offset, initial_size); \
        Offset32(type, expand_size_macro, initial_offset + 32, initial_size)

int main()
{
 {
   volatile long long value[5] = {0};
   for (value[2] = 1; value[2] < 1024; ++value[2])
   {
     Offset64(long long, Size64, 1, 1);
     Offset32(int, Size32, 1, 1);
     Offset16(short, Size16, 1, 1);
     Offset8(char, Size8, 1, 1);
   }
   for (value[2] = 1; value[2]; value[2] <<= 1)
   {
     Offset64(long long, Size64, 1, 1);
     Offset32(int, Size32, 1, 1);
     Offset16(short, Size16, 1, 1);
     Offset8(char, Size8, 1, 1);
   }
   for (value[2] = 3; value[2]; value[2] <<= 1)
   {
     Offset64(long long, Size64, 1, 1);
     Offset32(int, Size32, 1, 1);
     Offset16(short, Size16, 1, 1);
     Offset8(char, Size8, 1, 1);
   }
   for (value[2] = 5; value[2]; value[2] <<= 1)
   {
     Offset64(long long, Size64, 1, 1);
     Offset32(int, Size32, 1, 1);
     Offset16(short, Size16, 1, 1);
     Offset8(char, Size8, 1, 1);
   }
   for (value[2] = -1; value[2] > -1024; --value[2])
   {
     Offset64(long long, Size64, 1, 1);
     Offset32(int, Size32, 1, 1);
     Offset16(short, Size16, 1, 1);
     Offset8(char, Size8, 1, 1);
   }
 }
 {
   volatile int value[5] = {0};
   for (value[2] = 1; value[2] < 1024; ++value[2])
   {
     Offset32(int, Size32, 1, 1);
     Offset16(short, Size16, 1, 1);
     Offset8(char, Size8, 1, 1);
   }
   for (value[2] = 1; value[2]; value[2] <<= 1)
   {
     Offset32(int, Size32, 1, 1);
     Offset16(short, Size16, 1, 1);
     Offset8(char, Size8, 1, 1);
   }
   for (value[2] = 3; value[2]; value[2] <<= 1)
   {
     Offset32(int, Size32, 1, 1);
     Offset16(short, Size16, 1, 1);
     Offset8(char, Size8, 1, 1);
   }
   for (value[2] = 5; value[2]; value[2] <<= 1)
   {
     Offset32(int, Size32, 1, 1);
     Offset16(short, Size16, 1, 1);
     Offset8(char, Size8, 1, 1);
   }
   for (value[2] = -1; value[2] > -1024; --value[2])
   {
     Offset32(int, Size32, 1, 1);
     Offset16(short, Size16, 1, 1);
     Offset8(char, Size8, 1, 1);
   }
 }
 {
   volatile unsigned short value[5] = {0};
   for (value[2] = 1; value[2] < 0xFFFF; ++value[2])
   {
     Offset16(short, Size16, 1, 1);
     Offset8(char, Size8, 1, 1);
   }
   for (value[2] = 1; value[2]; value[2] <<= 1)
   {
     Offset16(short, Size16, 1, 1);
     Offset8(char, Size8, 1, 1);
   }
   for (value[2] = 3; value[2]; value[2] <<= 1)
   {
     Offset16(short, Size16, 1, 1);
     Offset8(char, Size8, 1, 1);
   }
   for (value[2] = 5; value[2]; value[2] <<= 1)
   {
     Offset16(short, Size16, 1, 1);
     Offset8(char, Size8, 1, 1);
   }
 }
 {
   volatile unsigned char value[5] = {0};
   for (value[2] = 1; value[2] < 255; ++value[2])
   {
     Offset8(char, Size8, 1, 1);
   }
   for (value[2] = 1; value[2]; value[2] <<= 1)
   {
     Offset8(char, Size8, 1, 1);
   }
   for (value[2] = 3; value[2]; value[2] <<= 1)
   {
     Offset8(char, Size8, 1, 1);
   }
   for (value[2] = 5; value[2]; value[2] <<= 1)
   {
     Offset8(char, Size8, 1, 1);
   }
 }
 return 0;
}
