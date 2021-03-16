/* see IP.i3 */

#ifdef __cplusplus
extern "C" {
#endif

#define STRUCT_AND_TYPEDEF(x) struct x; typedef struct x x

STRUCT_AND_TYPEDEF(addrinfo);
STRUCT_AND_TYPEDEF(hostent);
STRUCT_AND_TYPEDEF(sockaddr);
STRUCT_AND_TYPEDEF(sockaddr_storage);
STRUCT_AND_TYPEDEF(sockaddr_un);
STRUCT_AND_TYPEDEF(sockaddr_in);
STRUCT_AND_TYPEDEF(sockaddr_in6);
STRUCT_AND_TYPEDEF(in_addr);
STRUCT_AND_TYPEDEF(in6_addr);
STRUCT_AND_TYPEDEF(IP__Endpoint);

/* functions */
#define GetAddress          IPInternal__GetAddress
//#define GetAddrInfo       IPInternal__GetAddrInfo // Clashes with Windows.h.
#define GetCanonicalByAddr  IPInternal__GetCanonicalByAddr
#define GetCanonicalByName  IPInternal__GetCanonicalByName
#define GetHostAddr         IPInternal__GetHostAddr
#define GetHostByName       IPInternal__GetHostByName
//#define GetNameInfo       IPInternal__GetNameInfo // Clashes with Windows.h.

int
__cdecl
GetHostByName(const char* s, int* res, hostent** h);

int
__cdecl
GetCanonicalByName(const char* s, TEXT* text, hostent** h);

int
__cdecl
GetCanonicalByAddr(const int* addr, TEXT* result, hostent** h);

int
__cdecl
GetHostAddr(int* address, hostent** h);

int
__cdecl
IPInternal__GetAddrInfo(IP__Endpoint** endpoint, const char* node, const char* port);

int
__cdecl
IPInternal__GetNameInfo(
    int family,
    int port,
    const void* addr,
    TEXT* hostText,
    TEXT* serviceText);

void
__cdecl
IPInternal__CopyStoT(const char* name, TEXT* text);

void
__cdecl
IPInternal__Init(void);

void
__cdecl
IPInternal__NewEndpoint4(IP__Endpoint** endpoint, int port, const in_addr* address);

void
__cdecl
IPInternal__NewEndpoint6(IP__Endpoint** endpoint, int port, const in6_addr* address);

void
__cdecl
IPError__Die(void);

INTEGER
__cdecl
IPInternal__getsockname(INTEGER fd, char* address, INTEGER* port);

#ifdef __cplusplus
} /* extern "C" */
#endif
