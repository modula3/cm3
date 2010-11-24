#include <stdio.h>
#include <TargetConditionals.h>

typedef union {
  unsigned u;
  struct {
    unsigned char b[4];
  } s1;
  struct {
    unsigned a : 1;
    unsigned b : 8;
    unsigned c : 8;
    unsigned d : 8;
    unsigned e : 7;
  } s2;
} T1;

unsigned bit_pattern(unsigned initial, unsigned loop, unsigned or, unsigned left_shift)
{
 unsigned a = initial;
 unsigned b = { 0 };
 for (b = 0; b < loop; ++b)
   a = ((a << left_shift) | or);
 return a;
}

char* reverse(char* buffer, unsigned n)
{
  unsigned i = { 0 };
  if (n < 2)
    return buffer;
  n -= 1;
  while (i < n)
  {
    char t = buffer[i];
    buffer[i] = buffer[n];
    buffer[n] = t;
    i += 1;
    n -= 1;
  }
  return buffer;
}

char* binary(unsigned u, char* buffer)
{
  unsigned a = { 0 };
  unsigned b = { 0 };
  for (; a < 32; ++a)
  {
    buffer[b++] = "01"[u & 1];
    u >>= 1;
    if (((a + 1) % 4) == 0)
      buffer[b++] = '_';
  }
  b -= 1;
  buffer[b] = 0;
  return reverse(buffer, b);
}

unsigned get_bits(unsigned char* a, unsigned offset, unsigned count)
{
  unsigned value = { 0 };

  if (TARGET_RT_LITTLE_ENDIAN)
    return (((*(unsigned*)a) >> offset) & ~(~0u << count));
    
  while (offset > 8)
  {
    a += 1;
    offset -= 8;
  }
  while (count)
  {
    unsigned new_bits = { 0 };
    if (count > 8)
    {
      new_bits = *a;
    }
    else
    {    
      new_bits = (*a >> offset);
    }
    offset -= 8;
    count -= 8;
    value |= new_bits;
  }
}

int main()
{
  T1 t1 = { 0 };
  unsigned i, j = { 0 };
  char buffer[64] = { 0 };
  
  unsigned values[] = {1,0x80000000,0x55555555,0xAAAAAAAA,0xA5A5A5A5,0x5A5A5A5A,0};
  unsigned counts[] = {1,8,8,8,7};
  unsigned offsets[] = {0,1,9,17,25};
  
  for (i = 0; t1.u = values[i]; ++i)
  {
    unsigned offset = { 0 };
    printf("u: %08X %s\n", t1.u, binary(t1.u, buffer));
    printf("u.b: %02X%02X%02X%02X\n", t1.s1.b[0], t1.s1.b[1], t1.s1.b[2], t1.s1.b[3]);
    printf("u.a,b,c,d,e: %08X %08X %08X %08X %08X\n", t1.s2.a, t1.s2.b, t1.s2.c, t1.s2.d, t1.s2.e);
    
    for (j = 0; j < 5; ++j)
    {
      if (TARGET_RT_LITTLE_ENDIAN)
        printf("%c: %08X\n", 'a' + j, (t1.u >> offsets[j]) & ~(~0u << counts[j]));
     else
     {
        printf("%c: %08X\n", 'a' + j, (t1.u >> (32 - offsets[j])) & ~(~0u << counts[j]));
     }     
    }
  }

  return 0;
}
