/*
*+---------------------------------------------------------------------------+
*|            Copyright (C) 1992, Digital Equipment Corporation              |
*| 		          All rights reserved.                               |
*|                  See the file COPYRIGHT for a full description.           |
*+---------------------------------------------------------------------------+
*/

#include <stdio.h>
 
#define N 15


main()
{
  int r, c, ct;

  printf ("%04d\n", N*N);
  for (r = 0; r < N; r++) {
    for (c = 0; c < N; c++) {
      ct = 0;
      printf ("v%d\n", r*N+c);
      if (c < 3 || c > 7) {
         printf ("%.2f %.2f 4.0\n", c*10.0, r*10.0);
         ct += edge (r-1, c, ct);
         ct += edge (r+1, c, ct);
         ct += edge (r, c-1, ct);
         ct += edge (r, c+1, ct);
      }
      else if (c == 3) {
         printf ("%.2f %.2f 2.0\n", c*10.0, r*10.0);
         ct += edge (r, c-1, ct);
         ct += edge (r+1, c+1, ct);
      }
      else if (c == 4) {
         printf ("%.2f %.2f 2.0\n", c*10.0, r*10.0);
         ct += edge (r+1, c+1, ct);
         ct += edge (r-1, c-1, ct);
      }
      else if (c == 5) {
         printf ("%.2f %.2f 3.0\n", c*10.0, r*10.0);
         ct += edge (r+1, c+1, ct);
         ct += edge (r-1, c+1, ct);
         ct += edge (r-1, c-1, ct);
      }
      else if (c == 7) {
         printf ("%.2f %.2f 3.0\n", c*10.0, r*10.0);
         ct += edge (r, c+1, ct);
         ct += edge (r-1, c-1, ct);
         ct += edge (r+1, c-1, ct);
      }
      else if (c == 6) {
         printf ("%.2f %.2f 4.0\n", c*10.0, r*10.0);
         ct += edge (r+1, c+1, ct);
         ct += edge (r-1, c-1, ct);
         ct += edge (r+1, c-1, ct);
         ct += edge (r-1, c+1, ct);
      }
    
      printf ("\n");
    }
  }
}

int edge(row, col, id)
{
  if (row < 0 || row >= N || col < 0 || col >= N) 
    return 0;
  else {
    printf ("Edge\n");
    printf ("%04d %02d\n", row*N+col, id);
    return 1;
  }
}
