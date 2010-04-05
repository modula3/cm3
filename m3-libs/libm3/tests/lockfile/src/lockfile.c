#include <stdio.h>
#include <windows.h>

int main(int argc, char** argv)
{
    HANDLE h = { 0 };
    BOOL success = { 0 };

    h = CreateFile(argv[1],
                  GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE,
                  NULL,
                  OPEN_EXISTING,
                  FILE_ATTRIBUTE_NORMAL,
                  NULL);
    printf("file handle %p, error %u\n", h, GetLastError());

    success = LockFile(h, 0, 0, -1, -1);
    printf("lock file success %u, error %u\n", success, GetLastError());

    return 0;
}
