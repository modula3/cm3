#include "m3unix.h"

#ifdef __cplusplus
extern "C" {
#endif

UINT32 Uin__ntohl(UINT32 x)
{
    return ntohl(x);
}

UINT16 Uin__ntohs(UINT16 x)
{
    return ntohs(x);
}

UINT32 Uin__htonl(UINT32 x)
{
    return htonl(x);
}

UINT16 Uin__htons(UINT16 x)
{
    return htons(x);
}

#ifdef __cplusplus
}
#endif
