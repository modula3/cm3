/* Carsten Weich, June 1995
*
* "stat" and "fstat" are declared as "__inline__" in
* "/usr/include/sys/stat.h". These inline calls are not
* accessible to the linker, so Modula-3 routines cannot
* call them.
*
* This hack replicates the inline calls as regular functions.
*/

int stat (p1, p2)
char *p1, *p2;
{
   return _xstat(1, p1, p2);
}

int fstat (f, p)
int f;
char *p;
{
   return _fxstat(1, f, p);
}
