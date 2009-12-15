#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

void finally__F2(void);

void finally__F1(void)
{
#ifdef _MSC_VER
    __try
    {
        finally__F2();
    }
    __finally
#endif
    {
        printf("finally\n");
    }
}

#ifdef __cplusplus
} /* extern "C" */
#endif
