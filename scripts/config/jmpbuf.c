/*
Jay Krell
jay.krell@cornell.edu
May 2009
*/

#include <stdio.h>
#include <setjmp.h>
#define ALIGN_OF_TYPE(x) (sizeof(struct {char a; x b;}) - sizeof(x))
typedef unsigned U;

int main()
{
    jmp_buf jb;
    sigjmp_buf sjb;

#ifdef __CYGWIN__
    printf("cygwin: don't believe it\n");
#endif
    printf("jmpbuf size: %u\n", (U)sizeof(jb));
    printf("sigjmpbuf size: %u\n", (U)sizeof(sjb));
    printf("alignment: %u %u\n", (U)ALIGN_OF_TYPE(jmp_buf), (U)ALIGN_OF_TYPE(sigjmp_buf));
    return 0;
}
