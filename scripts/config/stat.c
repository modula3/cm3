void* fopen(const char*, const char*);
int printf(const char*, ...);
void* memset(void*, unsigned int, unsigned int);

/*
iphone headers give struct stat wrong?
*/

int main()
{
    int buf[100] = { 0 };
    int fd = { 0 };
    void* f = { 0 };
    int i = { 0 };

    memset(buf, 0x12, sizeof(buf));

    /* experiment some here with a.out, ., /bin/ls, etc., correlate
    with out out of ls -ai, and the various possibilities the xnu headers provide. */

    f = fopen("a.out", "rb");
    fd = fileno(f);
    fstat(fd, buf);

    while (i < 40)
    {
        printf("%08x %08x %08x %08x\n", buf[i], buf[i + 1], buf[i + 2], buf[i + 3]);
        i += 4;
    }

    return 0;
}
