#include "m3core.h"

#define M3MODULE FPU

/* Formerly, we "implemented" scaleb with
PROCEDURE scalb (x: LONGREAL; n: INTEGER): LONGREAL =
  BEGIN
    <*ASSERT FALSE*>
   END scalb;

Now we're equating it to the externally-defined (ANSI C math library)
procedure ldexp--see FPU.i3. */

M3WRAP2(LONGREAL, ldexp, LONGREAL, INTEGER)
M3WRAP1(LONGREAL, sqrt, LONGREAL)
