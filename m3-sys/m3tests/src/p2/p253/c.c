#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

void
put_adr(const char* s, void* p)
{
    printf("%s %p\n", s, p);
}

#ifdef __cplusplus
} /* extern "C" { */
#endif
