#include "wrap.h"
#include <netdb.h>

struct hostent *
m3_gethostbyaddr(const char *addr, int len, int type)
{
  return gethostbyaddr(addr, len, type);
}
