#include <stdio.h>

typedef  int (*PROC)();

int add_one (p)
  PROC p;
{
  int i;

  printf ("in add_one, p = 0x%x\n", p);

  i = p ();
  printf ("   p() => %d\n", i);
  
  return i+1;
}

PROC m3_proc;

int add_one_again ()
{
  int i;

  printf ("in add_one_again, m3_proc = 0x%x\n", m3_proc);

  i = m3_proc ();
  printf ("   m3_proc () => %d\n", i);
  
  return i+1;
}
