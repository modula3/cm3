#include <stdio.h>

#ifdef __cplusplus
extern "C" {

typedef  int (*PROC)();

int add_one (PROC p)
{
  int i;

  printf ("in add_one, p = %p\n", p);

  i = p ();
  printf ("   p() => %d\n", i);
  
  return i+1;
}

PROC m3_proc;

int add_one_again ()
{
  int i;

  printf ("in add_one_again, m3_proc = %p\n", m3_proc);

  i = m3_proc ();
  printf ("   m3_proc () => %d\n", i);
  
  return i+1;
}

}
#endif
