#include "m3unix.h"
#include <arpa/inet.h>

#ifdef __cplusplus
extern "C" {
#endif

#define M3MODULE Uin
M3WRAP1(UINT32, ntohl, UINT32)
M3WRAP1(UINT16, ntohs, UINT16)
M3WRAP1(UINT32, htonl, UINT32)
M3WRAP1(UINT16, htons, UINT16)

#ifdef __cplusplus
}
#endif
