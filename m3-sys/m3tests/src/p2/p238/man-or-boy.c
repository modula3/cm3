/* man-or-boy.c */
#include <stdio.h>
#include <stdlib.h>

typedef struct frame
{
  int (*fn)(struct frame*);
  int* k;
  struct frame* x1;
  struct frame* x2;
  struct frame* x3;
  struct frame* x4;
  struct frame* x5;
} FRAME;

int Fn1 (FRAME* a) { return -1; }
int F0  (FRAME* a) { return  0; }
int F1  (FRAME* a) { return  1; }

int eval(FRAME* a) { return a->fn(a); }

int A(FRAME*);

int B(FRAME* a)
{
  int k = (*a->k -= 1);
  FRAME b = { 0 };
  b.fn = B;
  b.k = &k;
  b.x1 = a;
  b.x2 = a->x1;
  b.x3 = a->x2;
  b.x4 = a->x3;
  b.x5 = a->x4;
  return A(&b);
}

int A(FRAME* a)
{
  return *a->k <= 0 ? eval(a->x4) + eval(a->x5) : B(a);
}

int main(int argc, char** argv)
{
  int k = argc == 2 ? strtol(argv[1],0,0) : 10;
  FRAME fn1 = { 0 };
  FRAME f1 = { 0 };
  FRAME f0 = { 0 };
  FRAME a = { 0 };

  fn1.k = &k;
  fn1.fn = Fn1;
  f1.k = &k;
  f1.fn = F1;
  f0.k = &k;
  f0.fn = F0;

  a.k = &k;
  a.fn = B;
  a.x1 = &f1;
  a.x2 = &fn1;
  a.x3 = &fn1;
  a.x4 = &f1;
  a.x5 = &f0;

  printf("%d\n", A(&a));
  return 0;
}
