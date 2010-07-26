/* Cygwin defines its jmpbuf incorrectly. */
#ifdef __CYGWIN__
typedef unsigned char _JBTYPE __attribute__((aligned(4)));
#define _JBTYPE _JBTYPE
#endif

#include <stdio.h>
#include <signal.h>
#include <stddef.h>
#include <setjmp.h>
#include <stdlib.h>
#include <string.h>
typedef unsigned U;

typedef size_t JB[sizeof(sigjmp_buf) / sizeof(size_t)];

void find(JB jb1, size_t a)
{
    U i;
    for (i = 0; i < (sizeof(JB) / sizeof(size_t)); ++i)
    {
        if (jb1[i] == a)
        {
            printf("find: jb1[%u] = %p\n", i, (void*)jb1[i]);
        }
    }
}

void diff(JB jb1, JB jb2)
{
    U i;
    for (i = 0; i < (sizeof(JB) / sizeof(size_t)); ++i)
    {
        if (jb1[i] != jb2[i])
        {
            printf("diff: jb1[%u] = %p, jb2[%u] = %p\n", i, (void*)jb1[i], i, (void*)jb2[i]);
        }
    }
}

void config2(sigjmp_buf* jb1)
{
    sigjmp_buf jb2;

    memset(&jb2, 0, sizeof(jb2));
    printf("looking for program counter and stack pointer and frame pointer\n");
    sigsetjmp(jb2, 1);
    diff(*(JB*)jb1, *(JB*)jb2);
}

void (*pconfig2)(sigjmp_buf* jb1) = config2;

void config(volatile size_t a)
{
    sigjmp_buf jb1;
    sigjmp_buf jb2;
    void* volatile p;
    void* volatile q;

    memset(&jb1, 0, sizeof(jb1));
    memset(&jb2, 0, sizeof(jb2));

    printf("config is at %p\n", &config);
    printf("parameter %p is at %p\n", (void*)a, &a);
    printf("local jb1 is at %p\n", &jb1);
    printf("local jb2 is at %p\n", &jb2);

    printf("looking for first parameter\n");
    sigsetjmp(jb1, 1);
    find(*(JB*)&jb1, a);

    printf("looking for program counter\n");
    sigsetjmp(jb1, 1);
    sigsetjmp(jb2, 1);
    diff(*(JB*)&jb1, *(JB*)&jb2);

    printf("looking for program counter and stack pointer\n");
    printf("alloca(123):%p\n", p = alloca(0x123));
    sigsetjmp(jb1, 1);
    printf("alloca(123):%p\n", q = alloca(0x123));
    sigsetjmp(jb2, 1);
    diff(*(JB*)&jb1, *(JB*)&jb2);

    config2(&jb1);
    pconfig2(&jb1);
}

void stack_grow(char* a)
{
    char b;
    
    printf("stack grows %s\n", (a < &b ? "up" : "down"));
}

int main()
{
    char a;
    
    printf("alignof(sigjmp_buf) %u\n", (U)__alignof(sigjmp_buf));
    printf("sizeof(sigjmp_buf) %u\n", (U)sizeof(sigjmp_buf));
    
    stack_grow(&a);

    if (sizeof(sigjmp_buf) % sizeof(size_t))
    {
        printf("sigjmp_buf size not multiple of size_t\n");
        exit(1);
    }
    config(0xdeadbeef * 16);
    config(0xcafebabe * 16);
    config(0xfeedface * 16);
    return 0;
}
