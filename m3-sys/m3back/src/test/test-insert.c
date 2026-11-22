#include <stdlib.h>
#include <stdio.h>

typedef unsigned int UINT32;
#if defined(_MSC_VER) || defined(__DECC) || defined(__int64)
typedef unsigned __int64 UINT64;
#else
typedef unsigned long long UINT64;
#endif
#define m3_extract_T(T) static T m3_extract_##T(T value,unsigned offset,unsigned count){return((value>>offset)&~(((~(T)0))<<count));}
#define m3_insert_T(T) static T m3_insert_##T(T x,T y,unsigned offset,unsigned count){T mask=(~((~(T)0)<<count))<<offset;return(((y<<offset)&mask)|(x&~mask));}
m3_extract_T(UINT32)
m3_extract_T(UINT64)
m3_insert_T(UINT32)
m3_insert_T(UINT64)

char* reverse(char* a, size_t b)
{
  size_t c = 0;
  if (b > 1)
  {
    b -= 1;
    while (c < b)
    {
      char d = a[c];
      a[c] = a[b];
      a[b] = d;
      c += 1;
      b -= 1;
    }
  }
  return a;
}

char* format_binary(char* buffer, UINT64 x)
{
  size_t i = { 0 };
  size_t n = { 0 };
  do
  {
    if (n && (n % 8) == 0)
      buffer[i++] = '`';
    buffer[i++] = "01"[x & 1];
    n += 1;
    x >>= 1;
  } while(x);
  buffer[i] = 0;
  return reverse(buffer, i);
}
#define FORMAT_BINARY(x) format_binary((char*)alloca(65*2), x)

int main()
{
    UINT64 x = { 0 };
    UINT64 y = { 0 };
    UINT64 z = { 0 };

#define X(insert, extract, a, b, o, c) do {         \
  x = insert(a, b, o, c);                           \
  printf("%s(%llX,%llX,%llX,%llX):%llX\n", #insert, (UINT64)a, (UINT64)b, (UINT64)o, (UINT64)c, (UINT64)x); \
  y = extract(x, o, c);                             \
  z = extract(b, 0, c);                             \
  if (y != z)                                       \
  {                                                 \
    printf("extract(0x%llX %llu 0b%s,0x%llX %llu 0b%s,0x%llX %llu 0b%s):0x%llX %llu 0b%s expected 0x%llX %llu 0b%s\n", \
        (UINT64)x, (UINT64)x, FORMAT_BINARY(x),         \
        (UINT64)o, (UINT64)o, FORMAT_BINARY(o),         \
        (UINT64)c, (UINT64)c, FORMAT_BINARY(c),         \
        x, x, FORMAT_BINARY(x),                         \
        (UINT64)z, (UINT64)z, FORMAT_BINARY(z));        \
    if ((o + c) > (8 * sizeof(extract(0,0,0))))         \
      printf("but offset+size is out of range\n");      \
    else                                                \
      abort();                                          \
  }                                                     \
} while(0)

    X(m3_insert_UINT64, m3_extract_UINT64, 0, 0x12345678, 0, 4);
    X(m3_insert_UINT64, m3_extract_UINT64, 0, 0x12345678, 4, 4);
    X(m3_insert_UINT64, m3_extract_UINT64, 0, 0x12345678, 8, 4);
    X(m3_insert_UINT64, m3_extract_UINT64, 0, 0x12345678, 12, 4);
    X(m3_insert_UINT64, m3_extract_UINT64, 0, 0x12345678, 16, 4);
    X(m3_insert_UINT64, m3_extract_UINT64, 0, 0x12345678, 20, 4);
    X(m3_insert_UINT64, m3_extract_UINT64, 0, 0x12345678, 24, 4);
    X(m3_insert_UINT64, m3_extract_UINT64, 0, 0x12345678, 28, 4);

    X(m3_insert_UINT64, m3_extract_UINT64, 0, (((UINT64)0x90ABCDEF) << 32) | 0x12345678, 32, 4);
    X(m3_insert_UINT64, m3_extract_UINT64, 0, (((UINT64)0x90ABCDEF) << 32) | 0x12345678, 36, 4);
    X(m3_insert_UINT64, m3_extract_UINT64, 0, (((UINT64)0x90ABCDEF) << 32) | 0x12345678, 40, 4);
    X(m3_insert_UINT64, m3_extract_UINT64, 0, (((UINT64)0x90ABCDEF) << 32) | 0x12345678, 44, 4);
    X(m3_insert_UINT64, m3_extract_UINT64, 0, (((UINT64)0x90ABCDEF) << 32) | 0x12345678, 48, 4);
    X(m3_insert_UINT64, m3_extract_UINT64, 0, (((UINT64)0x90ABCDEF) << 32) | 0x12345678, 52, 4);
    X(m3_insert_UINT64, m3_extract_UINT64, 0, (((UINT64)0x90ABCDEF) << 32) | 0x12345678, 56, 4);
    X(m3_insert_UINT64, m3_extract_UINT64, 0, (((UINT64)0x90ABCDEF) << 32) | 0x12345678, 60, 4);
    X(m3_insert_UINT64, m3_extract_UINT64, 0, (((UINT64)0x90ABCDEF) << 32) | 0x12345678, 60, 1);
    X(m3_insert_UINT64, m3_extract_UINT64, 0, (((UINT64)0x90ABCDEF) << 32) | 0x12345678, 61, 1);
    X(m3_insert_UINT64, m3_extract_UINT64, 0, (((UINT64)0x90ABCDEF) << 32) | 0x12345678, 62, 1);
    X(m3_insert_UINT64, m3_extract_UINT64, 0, (((UINT64)0x90ABCDEF) << 32) | 0x12345678, 63, 1);
    X(m3_insert_UINT64, m3_extract_UINT64, 0, (((UINT64)0x90ABCDEF) << 32) | 0x12345678, 62, 2);
    X(m3_insert_UINT64, m3_extract_UINT64, 0, (((UINT64)0x90ABCDEF) << 32) | 0x12345678, 61, 3);

    X(m3_insert_UINT32, m3_extract_UINT32, 0, 0x12345678, 0, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, 0, 0x12345678, 4, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, 0, 0x12345678, 8, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, 0, 0x12345678, 12, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, 0, 0x12345678, 16, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, 0, 0x12345678, 20, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, 0, 0x12345678, 24, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, 0, 0x12345678, 28, 4);

    X(m3_insert_UINT32, m3_extract_UINT32, 0, 0x12345678, 0, 5);
    X(m3_insert_UINT32, m3_extract_UINT32, 0, 0x12345678, 4, 5);
    X(m3_insert_UINT32, m3_extract_UINT32, 0, 0x12345678, 8, 5);
    X(m3_insert_UINT32, m3_extract_UINT32, 0, 0x12345678, 12, 5);
    X(m3_insert_UINT32, m3_extract_UINT32, 0, 0x12345678, 16, 5);
    X(m3_insert_UINT32, m3_extract_UINT32, 0, 0x12345678, 20, 5);
    X(m3_insert_UINT32, m3_extract_UINT32, 0, 0x12345678, 24, 5);
    X(m3_insert_UINT32, m3_extract_UINT32, 0, 0x12345678, 28, 5);

    X(m3_insert_UINT32, m3_extract_UINT32, 0, 0x12345678, 0, 8);
    X(m3_insert_UINT32, m3_extract_UINT32, 0, 0x12345678, 0, 10);
    X(m3_insert_UINT32, m3_extract_UINT32, 0, 0x12345678, 0, 12);
    X(m3_insert_UINT32, m3_extract_UINT32, 0, 0x12345678, 0, 20);
    X(m3_insert_UINT32, m3_extract_UINT32, 0, 0x12345678, 4, 1);
    X(m3_insert_UINT32, m3_extract_UINT32, 0, 0x12345678, 8, 1);
    X(m3_insert_UINT32, m3_extract_UINT32, 0, 0x12345678, 16, 8);
    X(m3_insert_UINT32, m3_extract_UINT32, 0, 0x12345678, 16, 16);
    X(m3_insert_UINT32, m3_extract_UINT32, 0, 0x12345678, 16, 20);
    X(m3_insert_UINT32, m3_extract_UINT32, 0, 0x12345678, 24, 1);
    X(m3_insert_UINT32, m3_extract_UINT32, 0, 0x12345678, 28, 1);






    X(m3_insert_UINT64, m3_extract_UINT64, -1, 0x12345678, 0, 4);
    X(m3_insert_UINT64, m3_extract_UINT64, -1, 0x12345678, 4, 4);
    X(m3_insert_UINT64, m3_extract_UINT64, -1, 0x12345678, 8, 4);
    X(m3_insert_UINT64, m3_extract_UINT64, -1, 0x12345678, 12, 4);
    X(m3_insert_UINT64, m3_extract_UINT64, -1, 0x12345678, 16, 4);
    X(m3_insert_UINT64, m3_extract_UINT64, -1, 0x12345678, 2, 4);
    X(m3_insert_UINT64, m3_extract_UINT64, -1, 0x12345678, 24, 4);
    X(m3_insert_UINT64, m3_extract_UINT64, -1, 0x12345678, 28, 4);

    X(m3_insert_UINT64, m3_extract_UINT64, -1, (((UINT64)0x90ABCDEF) << 32) | 0x12345678, 32, 4);
    X(m3_insert_UINT64, m3_extract_UINT64, -1, (((UINT64)0x90ABCDEF) << 32) | 0x12345678, 36, 4);
    X(m3_insert_UINT64, m3_extract_UINT64, -1, (((UINT64)0x90ABCDEF) << 32) | 0x12345678, 40, 4);
    X(m3_insert_UINT64, m3_extract_UINT64, -1, (((UINT64)0x90ABCDEF) << 32) | 0x12345678, 44, 4);
    X(m3_insert_UINT64, m3_extract_UINT64, -1, (((UINT64)0x90ABCDEF) << 32) | 0x12345678, 48, 4);
    X(m3_insert_UINT64, m3_extract_UINT64, -1, (((UINT64)0x90ABCDEF) << 32) | 0x12345678, 52, 4);
    X(m3_insert_UINT64, m3_extract_UINT64, -1, (((UINT64)0x90ABCDEF) << 32) | 0x12345678, 56, 4);
    X(m3_insert_UINT64, m3_extract_UINT64, -1, (((UINT64)0x90ABCDEF) << 32) | 0x12345678, 60, 4);
    X(m3_insert_UINT64, m3_extract_UINT64, -1, (((UINT64)0x90ABCDEF) << 32) | 0x12345678, 60, 1);
    X(m3_insert_UINT64, m3_extract_UINT64, -1, (((UINT64)0x90ABCDEF) << 32) | 0x12345678, 61, 1);
    X(m3_insert_UINT64, m3_extract_UINT64, -1, (((UINT64)0x90ABCDEF) << 32) | 0x12345678, 62, 1);
    X(m3_insert_UINT64, m3_extract_UINT64, -1, (((UINT64)0x90ABCDEF) << 32) | 0x12345678, 63, 1);
    X(m3_insert_UINT64, m3_extract_UINT64, -1, (((UINT64)0x90ABCDEF) << 32) | 0x12345678, 62, 2);
    X(m3_insert_UINT64, m3_extract_UINT64, -1, (((UINT64)0x90ABCDEF) << 32) | 0x12345678, 61, 3);

    X(m3_insert_UINT32, m3_extract_UINT32, -1, 0x12345678, 0, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, -1, 0x12345678, 4, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, -1, 0x12345678, 8, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, -1, 0x12345678, 12, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, -1, 0x12345678, 16, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, -1, 0x12345678, 20, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, -1, 0x12345678, 24, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, -1, 0x12345678, 28, 4);

    X(m3_insert_UINT32, m3_extract_UINT32, -1, 0x12345678, 0, 5);
    X(m3_insert_UINT32, m3_extract_UINT32, -1, 0x12345678, 4, 5);
    X(m3_insert_UINT32, m3_extract_UINT32, -1, 0x12345678, 8, 5);
    X(m3_insert_UINT32, m3_extract_UINT32, -1, 0x12345678, 12, 5);
    X(m3_insert_UINT32, m3_extract_UINT32, -1, 0x12345678, 16, 5);
    X(m3_insert_UINT32, m3_extract_UINT32, -1, 0x12345678, 20, 5);
    X(m3_insert_UINT32, m3_extract_UINT32, -1, 0x12345678, 24, 5);
    X(m3_insert_UINT32, m3_extract_UINT32, -1, 0x12345678, 28, 5);

    X(m3_insert_UINT32, m3_extract_UINT32, -1, 0x12345678, 0, 8);
    X(m3_insert_UINT32, m3_extract_UINT32, -1, 0x12345678, 0, 10);
    X(m3_insert_UINT32, m3_extract_UINT32, -1, 0x12345678, 0, 12);
    X(m3_insert_UINT32, m3_extract_UINT32, -1, 0x12345678, 0, 20);
    X(m3_insert_UINT32, m3_extract_UINT32, -1, 0x12345678, 4, 1);
    X(m3_insert_UINT32, m3_extract_UINT32, -1, 0x12345678, 8, 1);
    X(m3_insert_UINT32, m3_extract_UINT32, -1, 0x12345678, 16, 8);
    X(m3_insert_UINT32, m3_extract_UINT32, -1, 0x12345678, 16, 16);
    X(m3_insert_UINT32, m3_extract_UINT32, -1, 0x12345678, 16, 20);
    X(m3_insert_UINT32, m3_extract_UINT32, -1, 0x12345678, 24, 1);
    X(m3_insert_UINT32, m3_extract_UINT32, -1, 0x12345678, 28, 1);




    X(m3_insert_UINT32, m3_extract_UINT32, -1, 1, 0, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, -1, 2, 4, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, -1, 3, 8, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, -1, 4, 12, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, -1, 5, 16, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, -1, 6, 20, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, -1, 7, 24, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, -1, 8, 28, 4);


    X(m3_insert_UINT32, m3_extract_UINT32, 0, 1, 0, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, 0, 2, 4, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, 0, 3, 8, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, 0, 4, 12, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, 0, 5, 16, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, 0, 6, 20, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, 0, 7, 24, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, 0, 8, 28, 4);


    X(m3_insert_UINT32, m3_extract_UINT32, 0x12345678, 1, 0, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, 0x12345678, 2, 4, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, 0x12345678, 3, 8, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, 0x12345678, 4, 12, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, 0x12345678, 5, 16, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, 0x12345678, 6, 20, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, 0x12345678, 7, 24, 4);
    X(m3_insert_UINT32, m3_extract_UINT32, 0x12345678, 8, 28, 4);


    return 0;
}
