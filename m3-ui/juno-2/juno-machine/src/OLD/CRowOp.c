/* Copyright (C) 1992, Digital Equipment Corporation                         */
/* All rights reserved.                                                      */
/* See the file COPYRIGHT for a full description.                            */
/*                                                                           */
/* Last modified on Mon Oct  3 17:17:45 PDT 1994 by heydon                   */

/* C implementation of the procedure "CRowOp.P". */

#include <stdio.h>

rowop(len, target, src, factor)
  long len;
  float *target, *src;
  float factor;
{
    int i, i2;
    float maxAbs = 0.0;
    float abs;
    float t0, t1, t2, t3;
    int maxCol = -1;

    for (i=0; i<len-2; i+=2) {
        t0 = src[i];
        t2 = (t0 * factor);
        i2 = i + 1;
        t1 = src[i2];
        t3 = (t1 * factor);
	target[i] -= t2;
	target[i2] -= t3;
	abs = target[i];
	if (abs < 0) abs = -(abs);
	if (abs > maxAbs) {
	    maxAbs = abs;
	    maxCol = i;
	}
	abs = target[i2];
	if (abs < 0) abs = -(abs);
	if (abs > maxAbs) {
	    maxAbs = abs;
	    maxCol = i2;
	}
    }
    for (i=0; i<len-1; i++) {
	target[i] -= (src[i] * factor);
	abs = target[i];
	if (abs < 0) abs = -(abs);
	if (abs > maxAbs) {
	    maxAbs = abs;
	    maxCol = i;
	}
    }
    target[len-1] -= (src[len-1] * factor);
    return maxCol;
}
