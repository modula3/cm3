
#include <stdio.h>

#define EXTRACT(T, VALUE, OFFSET, SIZE) \
 (OFFSET ? (((struct { T : OFFSET; unsigned T value: SIZE; }*)&VALUE)->value) \
         : (((struct { unsigned T value: SIZE; }*)&VALUE)->value))

int main()
{
 {
   long long value;
   for (value = 1; value < 1024; ++value)
   {
#define X(type, offset, size) printf("%s %llX[%X:%X] => %X\n", #type, (long long)value, (int)offset, size, (unsigned)EXTRACT(type, value, offset, size))
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
     Offset64(long long, Size64, 1, 1);
     Offset32(int, Size32, 1, 1);
     Offset16(short, Size16, 1, 1);
     Offset8(char, Size8, 1, 1);
   }
 }
{
   int  value;
   for (value = 1; value < 1024; ++value)
   {
     Offset32(int, Size32, 1, 1);
     Offset16(short, Size16, 1, 1);
     Offset8(char, Size8, 1, 1);
   }
 }
{
   short value;
   for (value = 1; value < 30000; ++value)
   {
     Offset16(short, Size16, 1, 1);
     Offset8(char, Size8, 1, 1);
   }
 }
{
   unsigned char value;
   for (value = 1; value < 255; ++value)
   {
     Offset8(char, Size8, 1, 1);
   }
 }
 return 0;
}
