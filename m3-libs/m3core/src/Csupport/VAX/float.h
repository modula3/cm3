/* float.h */
/* Produced by hard-params version 4.1, CWI, Amsterdam */

#define FLT_RADIX 2
#define FLT_MANT_DIG 24
#define FLT_DIG 6
#define FLT_ROUNDS 1
#define FLT_EPSILON ((float)5.96046448e-08)
#define FLT_MIN_EXP (-127)
#define FLT_MIN ((float)2.93873588e-39)
#define FLT_MIN_10_EXP (-38)
#define FLT_MAX_EXP 127
#define FLT_MAX ((float)1.70141173e+38)
#define FLT_MAX_10_EXP 38

#define DBL_MANT_DIG 56
#define DBL_DIG 16
#define DBL_EPSILON 1.38777878078144570e-17

/* *** WARNING: Possibly bad output from printf above */
/*     expected value around 1.38777878078144570e-17, bit pattern:
    10000000 00100100 00000000 00000000 00000000 00000000 00000000 00000000 */
/*     sscanf gave           1.38777878078144570e-17, bit pattern:
    10000000 00100100 00000000 00000000 00000000 00000000 00000001 00000000 */
/*     difference= -3.85185988877447170e-34 */

#define DBL_MIN_EXP (-127)
#define DBL_MIN 2.93873587705571880e-39
#define DBL_MIN_10_EXP (-38)
#define DBL_MAX_EXP 127
#define DBL_MAX 1.70141183460469230e+38
#define DBL_MAX_10_EXP 38

#define   HUGE_VAL        ((float)FLT_MAX)        /* error value returned by Math lib */
