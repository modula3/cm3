#include <stdio.h>
#include <assert.h>

static void F(char* a)
{
    char b;
    assert(a != &b);
    printf("stack grows %s\n", (&b < a) ? "down" : "up");
}

int main()
{
    char a;
    F(&a);
    return 0;
}
