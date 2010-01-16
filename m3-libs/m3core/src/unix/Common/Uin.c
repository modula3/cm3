#ifdef _MSC_VER
#pragma optimize("gt", on)
#pragma optimize("y", off)
#undef _DLL
#ifndef _MT
#define _MT
#endif
#endif

#include "m3core.h"
#ifndef _WIN32
#include <arpa/inet.h>
#endif

#define M3MODULE Uin
M3WRAP1(UINT32, ntohl, UINT32)
M3WRAP1(UINT16, ntohs, UINT16)
M3WRAP1(UINT32, htonl, UINT32)
M3WRAP1(UINT16, htons, UINT16)
