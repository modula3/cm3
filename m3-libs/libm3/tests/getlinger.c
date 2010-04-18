#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#ifdef _WIN32
#include <winsock2.h>
#include <stdlib.h>
#else
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <arpa/inet.h>
#endif

int main(int argc, char** argv)
{
    struct linger linger;
    int optlen = sizeof(linger);
    int fd =  { 0 };
    struct sockaddr_in in;
    int r = { 0 };
    int err = { 0 };
    char errorMessage[1024] = { 0 };
#ifdef _WIN32
    WSADATA wsaData = { 0 };
    WORD wsVersion = MAKEWORD(2, 2);

    if (argc > 3 && argv[3]) wsVersion = strtoul(argv[3], 0, 0);
    printf("wsVersion %x\n", wsVersion);
    WSAStartup(wsVersion, &wsaData);
#endif

    memset(&in, 0, sizeof(in));
    memset(&linger, 0, sizeof(linger));
    in.sin_family = AF_INET;
    in.sin_addr.s_addr = (**(unsigned**)gethostbyname(argv[1])->h_addr_list);
    in.sin_port = htons(atoi(argv[2]));

    printf("address %u.%u.%u.%u port %u\n",
           ((unsigned char*)&in.sin_addr.s_addr)[0],
           ((unsigned char*)&in.sin_addr.s_addr)[1],
           ((unsigned char*)&in.sin_addr.s_addr)[2],
           ((unsigned char*)&in.sin_addr.s_addr)[3],
           in.sin_port);

    fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    optlen = sizeof(linger);
    getsockopt(fd, SOL_SOCKET, SO_LINGER, (char*)&linger, &optlen);
    printf("fd:%d onoff:%d linger:%d\n", fd, linger.l_onoff, linger.l_linger);

    r = connect(fd, (struct sockaddr*)&in, sizeof(in));
#ifdef _WIN32
    if (r == -1)
    {
        err = WSAGetLastError();
        FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, 0, err, 0, errorMessage, sizeof(errorMessage), 0);
    }
#endif
    printf("connect returned %d %d %s\n", r, err, errorMessage);

    optlen = sizeof(linger);
    getsockopt(fd, SOL_SOCKET, SO_LINGER, (char*)&linger, &optlen);
    printf("fd:%d onoff:%d linger:%d\n", fd, linger.l_onoff, linger.l_linger);

    linger.l_onoff = 1;
    linger.l_linger = 0;
    setsockopt(fd, SOL_SOCKET, SO_LINGER, (char*)&linger, sizeof(linger));
    optlen = sizeof(linger);
    getsockopt(fd, SOL_SOCKET, SO_LINGER, (char*)&linger, &optlen);
    printf("fd:%d onoff:%d linger:%d\n", fd, linger.l_onoff, linger.l_linger);

    linger.l_onoff = 1;
    linger.l_linger = 1;
    setsockopt(fd, SOL_SOCKET, SO_LINGER, (char*)&linger, sizeof(linger));
    optlen = sizeof(linger);
    getsockopt(fd, SOL_SOCKET, SO_LINGER, (char*)&linger, &optlen);
    printf("fd:%d onoff:%d linger:%d\n", fd, linger.l_onoff, linger.l_linger);

    return 0;
}
