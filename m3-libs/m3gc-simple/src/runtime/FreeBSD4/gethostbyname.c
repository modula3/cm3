#include "wrap.h"
#include <netdb.h>

struct hostent *
m3_gethostbyname(const char *name)
{
  struct hostent *result;

  ENTER_CRITICAL;
  result = gethostbyname(name);
  EXIT_CRITICAL;
  return result;
}
