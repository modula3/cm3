/* Copyright (C) 1995, Digital Equipment Corporation         */
/* All rights reserved.                                      */
/* See the file COPYRIGHT for a full description.            */
/*                                                           */
/* Last modified on Thu Feb  9 07:56:00 PST 1995 by kalsow   */

#include <stdio.h>

main ()
{
     int i, x, y, xvaries;
     int inc = 30;

     x = y = 0;
     xvaries = 1;
     for (i = 0; i < 30; i++) {
       fprintf (stdout, "V%d %d %d\n", i, x, y);
       if (xvaries) {
	 x += inc;
	 if (x > 1000) { x = 1000; y = 1000 + inc; xvaries = 0;}
	 if (x < 0) { y = 1000; x = 0; xvaries = 0; inc = -30;}
       } else {
	 y += inc;
	 if (y > 1000) { y = 1000; x = 1000; xvaries = 1; inc = -30;}
	 if (y < 0) { y = 0; x = 0; xvaries = 1; inc = 30;}
       }
     }

     for (i = 0; i < 20; i++)
       fprintf (stdout, "V%d -> V%d %d.%d\n",i, i+10, 1 + i/30, i/10);

     for (i = 0; i < 23; i++)
       fprintf (stdout, "V%d -> V%d %d.%d\n",i, i+7, 1 + i/30, i/10);

     for (i = 0; i < 19; i++)
       fprintf (stdout, "V%d -> V%d %d.%d\n",i, i+11, 1 + i/30, i/10);

     fprintf (stdout, ".\n");
}
