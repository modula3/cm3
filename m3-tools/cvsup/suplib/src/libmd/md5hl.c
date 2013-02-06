/* md5hl.c
 * ----------------------------------------------------------------------------
 * "THE BEER-WARE LICENSE" (Revision 42):
 * <phk@login.dkuug.dk> wrote this file.  As long as you retain this notice you
 * can do whatever you want with this stuff. If we meet some day, and you think
 * this stuff is worth it, you can buy me a beer in return.   Poul-Henning Kamp
 * ----------------------------------------------------------------------------
 */

#include "m3core.h"

#include "md5.h"

#ifdef __cplusplus
extern "C" {
#endif

char *
__cdecl
MD5End(MD5_CTX *ctx, char *buf)
{
    int i = { 0 };
    unsigned char digest[16] = { 0 };
    static const char hex[]="0123456789abcdef";

    if (!buf)
        buf = (char*)malloc(33);
    if (!buf)
        return 0;
    MD5Final(digest,ctx);
    for (i=0;i<16;i++) {
        buf[i+i] = hex[digest[i] >> 4];
        buf[i+i+1] = hex[digest[i] & 0x0f];
    }
    buf[i+i] = '\0';
    return buf;
}

char *
__cdecl
MD5File (char *filename, char *buf)
{
    unsigned char buffer[BUFSIZ] = { 0 };
    MD5_CTX ctx = { 0 };
    int f = { 0 };
    int i = { 0 };
    int j = { 0 };

    MD5Init(&ctx);
    f = open(filename,O_RDONLY);
    if (f < 0) return 0;
    while ((i = read(f,buffer,sizeof buffer)) > 0) {
        MD5Update(&ctx,buffer,i);
    }
    j = errno;
    close(f);
    errno = j;
    if (i < 0) return 0;
    return MD5End(&ctx, buf);
}

char *
__cdecl
MD5Data (const unsigned char *data, unsigned int len, char *buf)
{
    MD5_CTX ctx = { 0 };

    MD5Init(&ctx);
    MD5Update(&ctx,data,len);
    return MD5End(&ctx, buf);
}

#ifdef __cplusplus
} /* extern "C" */
#endif
