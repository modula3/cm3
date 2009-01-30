/*
see http://www.opengroup.org/onlinepubs/009695399/functions/swapcontext.html
*/

#include <stdio.h>
#include "context.h"

static ucontext_t ctx[4];

static void
f1 (int a, int b, int c)
{
    printf("start f1(%d, %d, %d)\n", a, b, c);
    swapcontext(&ctx[1], &ctx[3]);
    puts("finish f1");
}

static void
f2 ()
{
    printf("start f2\n");
    swapcontext(&ctx[2], &ctx[1]);
    puts("finish f2");
}

static void
f3 (int a, int b)
{
    printf("start f3(%d, %d)\n", a, b);
    swapcontext(&ctx[3], &ctx[2]);
    puts("finish f3");
}


int
main (void)
{
    char st[3][8192];
    unsigned i;

    for (i = 0 ; i != 4 ; ++i)
    {
        getcontext(&ctx[i]);
        if (i != 0)
        {
            ctx[i].uc_stack.ss_sp = st[i - 1];
            ctx[i].uc_stack.ss_size = sizeof st[i - 1];
            ctx[i].uc_link = &ctx[i - 1];
        }
    }

    makecontext(&ctx[1], f1, 3, 1, 2, 3);
    makecontext(&ctx[2], f2, 0);
    makecontext(&ctx[3], f3, 2, 4, 5);
    swapcontext(&ctx[0], &ctx[3]);

    return 0;
}
