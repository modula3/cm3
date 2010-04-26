#include <stdio.h>
#include <assert.h>
#include <stdlib.h>

int get_stack_direction_helper(char* a)
{
    char b;
    assert(a != &b);
    return (&b < a) ? -1 : 1;
}

int get_stack_direction()
{
    char a;
    return get_stack_direction_helper(&a);
}

int main()
{
    printf("stack grows %s\n", (get_stack_direction() < 0) ? "down" : "up"));
    return EXIT_SUCCESS;
}
