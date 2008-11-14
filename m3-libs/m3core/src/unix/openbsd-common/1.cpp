#include <stdio.h>
#include <pthread.h>
#include <setjmp.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/resource.h>
typedef struct stat stat_t;

const char* F1(int) { return "int"; }
const char* F1(long) { return "long"; }
const char* F1(unsigned) { return "unsigned"; }
const char* F1(unsigned long) { return "unsigned long"; }
const char* F1(unsigned char) { return "unsigned char"; }
const char* F1(signed char) { return "signed char"; }
const char* F1(short) { return "short"; }
const char* F1(unsigned short) { return "unsigned short"; }
const char* F1(long long) { return "long long"; }
const char* F1(unsigned long long) { return "unsigned long long"; }

typedef struct __jmp_buf_tag jmp_buf2;

int main(int argc, char** argv)
{

#define A(a) printf("%s 0x%x bytes %u bytes 0x%0x 'addresses' %u 'addresses' 0x%x bits %u bits\n", #a, (unsigned) sizeof(a), (unsigned) sizeof(a), (unsigned) sizeof(a) / sizeof(void*), (unsigned) sizeof(a) / sizeof(void*), (unsigned) sizeof(a) * 8, (unsigned) sizeof(a) * 8)

    A(jmp_buf);
    //A(jmp_buf2);
    A(pthread_t);
    A(pthread_attr_t);
    A(pthread_mutex_t);
    A(pthread_mutexattr_t);
    A(pthread_cond_t);
    A(pthread_condattr_t);
    A(pthread_key_t);
    A(pthread_once_t);
    A(pthread_rwlock_t);
    A(pthread_rwlockattr_t);
    //A(pthread_barrier_t);
    //A(pthread_barrierattr_t);
    //A(sched_param);

#undef A

#define STRING(a) #a
#define A(a) printf(STRING(a) " %s\n", F1(a()))

    A(clock_t);
    A(dev_t);
    A(gid_t);
    //A(in_addr_t);
    //A(in_port_t);
    A(ino_t);
    A(mode_t);
    A(nlink_t);
    A(off_t);
    A(pid_t);
    A(rlim_t);
    A(uid_t);
    A(size_t);
    A(socklen_t);
    A(time_t);
    A(uid_t);

  return 0;
}
