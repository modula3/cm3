#include <math.h>
#include <stdio.h>

int Floor(float f) { return floor(f); }
int Ceil(float f) { return ceil(f); }
int Trunc(float f) { return (int)f; }
int Round(float f) { return (int)llroundl(f); }

int main()
{
  static const float data[] = { 0, 1, 1.1, 1.4, 1.5, 1.6 };
  static int (* const functions[])(float) = { Floor, Ceil, Trunc, Round };
  static char const * const names[] = {"Floor", "Ceil", "Trunc", "Round" };
  unsigned i = { 0 };
  unsigned j = { 0 };
  unsigned k = { 0 };
  for (i = 0; i <= 3; ++i)
  {
    for (j = 0; j < sizeof(data) / sizeof(data[0]); ++j)
    {
      float f = data[j];
      if (i & 1) f += 1;
      if (i & 2) f *= -1;
      for (k = 0; k < sizeof(functions) / sizeof(functions[0]); ++k)
      {
        printf("%s(%f) => %d\n", names[k], f, functions[k](f));
      }
      printf("\n");
    }
  }
  return 0;
}
