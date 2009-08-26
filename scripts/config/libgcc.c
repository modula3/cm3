/*
reverse engineer libgcc
*/
typedef unsigned long unsignedlong;
typedef unsigned long long unsignedlonglong;
typedef long long longlong;

#define PASTEx(x, y) x ## y
#define PASTE(x, y) PASTEx(x, y)
#define PASTE3(x, y, z) PASTE(PASTE(x, y), z)

#define CONVERT(a, b) b PASTE3(a, _to_, b) (a c) { return c; } \
                      a PASTE3(b, _to_, a) (b c) { return c; }

#define NEGATE(a) a PASTE(negate_, a) (a c) { return -c; }

#define ADD(a) a PASTE(add_, a) (a c, a d) { return c + d; }
#define SUB(a) a PASTE(sub_, a) (a c, a d) { return c - d; }
#define MULT(a) a PASTE(mult_, a) (a c, a d) { return c * d; }
#define DIV(a) a PASTE(div_, a) (a c, a d) { return c / d; }
#define MOD(a) a PASTE(mod_, a) (a c, a d) { return c % d; }
#define COMPARE(a) int PASTE(compare_, a) (a c, a d) { return c < d ? 0 : c == d ? 1 : 2; }
#define Fx(a) ADD(a) SUB(a) MULT(a) DIV(a) MOD(a) NEGATE(a) COMPARE(a) CONVERT(double, a) CONVERT(float, a)
#define F(a) Fx(a) Fx(PASTE(unsigned, a))

F(long)
F(longlong)
