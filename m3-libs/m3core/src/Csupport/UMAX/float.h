/* float.h */
/* Produced by hard-params version 4.1, CWI, Amsterdam */

#define FLT_RADIX 2
#define FLT_MANT_DIG 24
#define FLT_DIG 6
#define FLT_ROUNDS 1
#define FLT_EPSILON ((float)1.19209290e-07)
#define FLT_MIN_EXP (-125)
#define FLT_MIN ((float)1.17549435e-38)
#define FLT_MIN_10_EXP (-37)
#define FLT_MAX_EXP 128
#define FLT_MAX ((float)3.40282347e+38)
#define FLT_MAX_10_EXP 38

#define DBL_MANT_DIG 53
#define DBL_DIG 15
#define DBL_EPSILON 2.2204460492503131e-16
#define DBL_MIN_EXP (-1021)
#define DBL_MIN 2.2250738585072010e-308

/* *** WARNING: Possibly bad output from printf above */
/*     expected value around 2.2250738585072010e-308, bit pattern:
    00000000 00000000 00000000 00000000 00000000 00000000 00010000 00000000 */
/*     sscanf gave           0.0000000000000000e+00, bit pattern:
    00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 */
/*     difference= 2.2250738585072010e-308 */

#define DBL_MIN_10_EXP (-307)
#define DBL_MAX_EXP 1024
#define DBL_MAX 1.7976931348623455e+308

/* *** WARNING: sscanf returned an unusable number */
/*     scanning: 1.7976931348623455e+308 with format: %le */

#define DBL_MAX_10_EXP 308

