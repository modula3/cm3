/* calfun.f -- translated by f2c (version 20100827).
   You must link the resulting object file with libf2c:
	on Microsoft Windows system, link with libf2c.lib;
	on Linux or Unix systems, link with .../path/to/libf2c.a -lm
	or, if you install libf2c.a in a standard place, with -lf2c -lm
	-- in that order, at the end of the command line, as in
		cc *.o -lf2c -lm
	Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

		http://www.netlib.org/f2c/libf2c.zip
*/

#include "f2c.h"

doublereal calfun_(integer *n, doublereal *x)
{
    /* System generated locals */
    integer i__1, i__2;
    real ret_val;

    /* Local variables */
    static doublereal f;
    static integer i__, j;
    static doublereal y[100]	/* was [10][10] */;
    static integer np, iw;
    static doublereal sum;

    /* Parameter adjustments */
    --x;

    /* Function Body */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	y[j * 10 - 10] = 1.;
/* L10: */
	y[j * 10 - 9] = x[j] * 2. - 1.;
    }
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
/* L20: */
	    y[i__ + 1 + j * 10 - 11] = y[j * 10 - 9] * 2. * y[i__ + j * 10 - 
		    11] - y[i__ - 1 + j * 10 - 11];
	}
    }
    f = 0.;
    np = *n + 1;
    iw = 1;
    i__2 = np;
    for (i__ = 1; i__ <= i__2; ++i__) {
	sum = 0.;
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
/* L30: */
	    sum += y[i__ + j * 10 - 11];
	}
	sum /= (doublereal) (*n);
	if (iw > 0) {
	    sum += 1. / (doublereal) (i__ * i__ - (i__ << 1));
	}
	iw = -iw;
/* L40: */
	f += sum * sum;
    }
    ret_val = f;
    return ret_val;
} /* calfun_ */

