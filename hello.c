#include <stdio.h>
#include <stdlib.h>

int main()
{
 printf("%d\n", (int)sizeof(void*));
 printf("%s\n", getenv("PROCESSOR_ARCHITECTURE"));
}
