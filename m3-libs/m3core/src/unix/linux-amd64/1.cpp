#include <stdio.h>
#include <pthread.h>
#include <setjmp.h>

typedef struct __jmp_buf_tag jmp_buf2;

/* use globals so the assembly is easier to read */

#define A(a) unsigned size_ ## a = (printf("%s %u\n", #a, sizeof(a)), sizeof(a))

    A(jmp_buf);
    A(jmp_buf2);
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
    A(pthread_barrier_t);
    A(pthread_barrierattr_t);
    //A(sched_param);

int main()
{
    return 0;
}
