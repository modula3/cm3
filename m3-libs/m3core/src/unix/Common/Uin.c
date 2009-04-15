#ifdef _WIN32
#include <winsock2.h>
#else
#include "m3unix.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

UINT32 Uin_ntohl(UINT32 x)
{
    return ntohl(x);
}

UINT16 Uin_ntohs(UINT16 x)
{
    return ntohs(x);
}

UINT32 Uin_htonl(UINT32 x)
{
    return htonl(x);
}

UINT16 Uin_htons(UINT16 x)
{
    return htons(x);
}

#ifdef __cplusplus
}
#endif
