#include <sys/types.h>
#include <sys/wait.h>

pid_t waitpidwrap(pid_t pid, int *status, int options)
{
  pid_t r;
  r = waitpid(pid,status,options);
  *status = WEXITSTATUS(*status) | ((*status & 0xff) << 8);
  return r;
}

