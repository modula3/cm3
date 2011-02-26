#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#endif

int main(int argc, char** argv)
{
  int seconds = { 0 };
  if (argc != 2 || strspn(argv[1], "0123456789") != strlen(argv[1]))
  {
    fprintf(stderr, "usage: %s seconds-to-sleep\n", argv[0]);
    return EXIT_FAILURE;
  }
  seconds = atoi(argv[1]);
#ifdef _WIN32
  Sleep(seconds * 1000);
#else
  sleep(seconds);
#endif
  return EXIT_SUCCESS;
}
