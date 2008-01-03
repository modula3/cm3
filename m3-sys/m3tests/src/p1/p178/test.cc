
#include <stdio.h>

#define IMODXP(n,m)   (((n) >= 0) ? (n)%(m) : (m)-1-(((-(n))-1)%(m)))

typedef struct {
  unsigned int a: 18;
  unsigned int b: 14;
} Packed;

typedef struct {
  int a: 18;
  int b: 14;
} IPacked;

int i = 16380;
unsigned int u = 16380;
Packed p = { 27, 16380 };
Packed q = { 27, 0x3fff };
IPacked r = { 27, 16380 };

main () {
   printf ("integer  i => %d\n", i);
   printf ("         i * (-1) => %d\n", i * (-1));
   printf ("         (i * (-1)) >= 0 => %d\n", (i * (-1)) >= 0);
   printf ("         (i * (-1)) MOD 16381) => %d\n", IMODXP(i * (-1), 16381));

   printf ("unsigned u => %d\n", u);
   printf ("         u * (-1) => %d\n", u * (-1));
   printf ("         (u * (-1)) >= 0 => %d\n", (u * (-1)) >= 0);
   printf ("         (u * (-1)) MOD 16381 => %d\n", IMODXP(u * (-1), 16381));

   printf ("(int)unsigned u => %d\n", (int)u);
   printf ("         u * (-1) => %d\n", ((int)u) * (-1));
   printf ("         (u * (-1)) >= 0 => %d\n", (((int)u) * (-1)) >= 0);
   printf ("         (u * (-1)) MOD 16381 => %d\n", IMODXP(((int)u) * (-1), 16381));

   printf ("packed p.b => %d\n", p.b);
   printf ("         p.b * (-1) => %d\n", (p.b) * (-1));
   printf ("         (p.b * (-1)) >= 0 => %d\n", ((p.b) * (-1)) >= 0);
   printf ("         (p.b * (-1)) MOD 16381 => %d\n", IMODXP((p.b) * (-1), 16381));

   printf ("(int)packed p.b => %d\n", (int)p.b);
   printf ("         p.b * (-1) => %d\n", ((int)p.b) * (-1));
   printf ("         (p.b * (-1)) >= 0 => %d\n", (((int)p.b) * (-1)) >= 0);
   printf ("         (p.b * (-1)) MOD 16381 => %d\n", IMODXP(((int)p.b) * (-1), 16381));

   printf ("packed q.b => %d\n", q.b);
   printf ("         q.b * (-1) => %d\n", (q.b) * (-1));
   printf ("         (q.b * (-1)) >= 0 => %d\n", ((q.b) * (-1)) >= 0);
   printf ("         (q.b * (-1)) MOD 16381 => %d\n", IMODXP((q.b) * (-1), 16381));

   printf ("(int)packed q.b => %d\n", (int)q.b);
   printf ("         q.b * (-1) => %d\n", ((int)q.b) * (-1));
   printf ("         (q.b * (-1)) >= 0 => %d\n", (((int)q.b) * (-1)) >= 0);
   printf ("         (q.b * (-1)) MOD 16381 => %d\n", IMODXP(((int)q.b) * (-1), 16381));

   i = q.b;
   printf ("integer  i => %d\n", i);
   printf ("         i * (-1) => %d\n", i * (-1));
   printf ("         (i * (-1)) >= 0 => %d\n", (i * (-1)) >= 0);
   printf ("         (i * (-1)) MOD 16381) => %d\n", IMODXP(i * (-1), 16381));

   printf ("packed r.b => %d\n", r.b);
   printf ("         r.b * (-1) => %d\n", (r.b) * (-1));
   printf ("         (r.b * (-1)) >= 0 => %d\n", ((r.b) * (-1)) >= 0);
   printf ("         (r.b * (-1)) MOD 16381 => %d\n", IMODXP((r.b) * (-1), 16381));

};
