/*
Test how Cygwin behavis.
1) Does zero initialization work? Not in general. It isn't guaranteed
2) Does static initialization work? Yes.
*/

#include <stdio.h>
#include <pthread.h>
#include <errno.h>

void once_callback_z(void)
{
    printf("once_callback_z\n");
}

void once_callback_s(void)
{
    printf("once_callback_s\n");
}

int main()
{
    union
    {
        pthread_mutex_t m;
        pthread_cond_t c;
        pthread_rwlock_t rw;
        pthread_once_t o;
    } z = { 0 };
    struct timespec t = { 0 };
    pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;
    pthread_mutex_t m2 = PTHREAD_MUTEX_INITIALIZER;
    pthread_mutex_t m3 = PTHREAD_MUTEX_INITIALIZER;
    pthread_mutex_t m4 = PTHREAD_MUTEX_INITIALIZER;
    pthread_mutex_t m5 = PTHREAD_MUTEX_INITIALIZER;
    pthread_mutex_t m6 = PTHREAD_MUTEX_INITIALIZER;
    pthread_mutex_t m7 = PTHREAD_MUTEX_INITIALIZER;
    pthread_mutex_t m8 = PTHREAD_MUTEX_INITIALIZER;
    pthread_mutex_t m9 = PTHREAD_MUTEX_INITIALIZER;
    pthread_cond_t c = PTHREAD_COND_INITIALIZER;
    pthread_cond_t c2 = PTHREAD_COND_INITIALIZER;
    pthread_cond_t c3 = PTHREAD_COND_INITIALIZER;
    pthread_cond_t c4 = PTHREAD_COND_INITIALIZER;
    pthread_cond_t c5 = PTHREAD_COND_INITIALIZER;
    pthread_cond_t c6 = PTHREAD_COND_INITIALIZER;
    pthread_cond_t c7 = PTHREAD_COND_INITIALIZER;
    pthread_cond_t c8 = PTHREAD_COND_INITIALIZER;
    pthread_once_t o = PTHREAD_ONCE_INIT;
    pthread_rwlock_t rw = PTHREAD_RWLOCK_INITIALIZER;
    int a = { 0 };

#define A(b) a = b; printf("%s:%d\n", #b, a);

    memset(&z, 0, sizeof(z));
    A(pthread_mutex_lock(&z.m)); /* This does not work. Not great but ok. */
    A(pthread_mutex_lock(&m)); /* This works. */

    memset(&z, 0, sizeof(z));
    A(pthread_rwlock_rdlock(&z.rw)); /* This does not work. Not great but ok. */
    A(pthread_rwlock_rdlock(&rw)); /* This works. */

    memset(&z, 0, sizeof(z));
    A(pthread_once(&z.o, once_callback_z)); /* This works. */
    A(pthread_once(&o, once_callback_s)); /* This works. */

    memset(&z, 0, sizeof(z));
    A(pthread_cond_wait(&z.c, &m2)); /* This does not work. It isn't supposed to. */
    A(pthread_cond_wait(&c2, &m3)); /* This does not work. It isn't supposed to. */
    A(pthread_cond_wait(&c3, &m4)); /* This does not work. It isn't supposed to. */
    A(pthread_cond_init(&c4, 0)); /* This works. */
    A(pthread_cond_wait(&c4, &m5)); /* This does not work. It isn't supposed to. */

    memset(&z, 0, sizeof(z));
    A(pthread_mutex_lock(&m6));
    A(pthread_mutex_lock(&m7));
    A(pthread_mutex_lock(&m8));
    A(pthread_mutex_lock(&m9));

    A(ETIMEDOUT);
    A(EINVAL);
    A(pthread_cond_init(&c5, 0)); /* This works. */
    A(pthread_cond_timedwait(&c5, &m7, &t)); /* This works. */
    A(pthread_cond_timedwait(&c5, &m8, &t)); /* This works. */
    A(pthread_cond_timedwait(&c5, &m9, &t)); /* This works. */

    return 0;
}
