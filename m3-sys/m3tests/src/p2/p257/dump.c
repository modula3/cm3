#include <assert.h>
#include <stddef.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

void Dump(
    int type,
    int offset,
    int count,
    int bitsize,
    int bytesize,
    const unsigned char* address)
{
    int i;
    char newline[] = "\n\0";
    int inewline = 0;
    
    // Do not dump all zeros.
    i = bytesize;
    while (i > 0 && address[i - 1] == 0) i -= 1;
    if (i == 0)
        return;
 
    //assert(bytesize == 4 || bytesize == 8);
    //assert(bitsize == 32 || bitsize == 64);
    printf("T%d offset:%d count:%d ",
        (int)type, (int)offset, (int)count);
    // reduce some difference between little endian 32bit and 64bit
    while (bytesize > 0 && address[bytesize - 1] == 0) bytesize -= 1;
    for (i = 0; i < bytesize; ++i)
    {
        printf("%02X ", address[i]);
        if (i > 0 && (i % 30) == 0)
            fputs(&newline[inewline = 1] - 1, stdout);
        else
            inewline = 0;
    }
    fputs(&newline[inewline], stdout);
    fflush(stdout);
}

#ifdef __cplusplus
} /* extern "C" */
#endif
