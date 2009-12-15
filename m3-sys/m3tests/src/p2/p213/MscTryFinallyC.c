#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

void MscTryFinally__F2(void);

void MscTryFinally__F1(void)
{
#ifdef _MSC_VER
    __try
    {
        MscTryFinally__F2();
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
