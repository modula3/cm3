/*
Test how Cygwin behaves.
1) Does zero initialization work? Not in general. It isn't guaranteed
2) Does static initialization work? Yes.
*/

#include <stdio.h>
#include <pthread.h>
#include <errno.h>
#include <sys/stat.h>

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
    struct stat st;
    int fd = { 0 };

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

    stat("/dev/null", &st);

    printf("st_dev %x\n", (int) st.st_dev);
    printf("st_ino %x\n", (int) st.st_ino);
    printf("st_mode %x\n", (int) st.st_mode);
    printf("st_nlink %x\n", (int) st.st_nlink);
    printf("st_uid %x\n", (int) st.st_uid);
    printf("st_gid %x\n", (int) st.st_gid);
    printf("st_rdev %x\n", (int) st.st_rdev);

    printf("offsets:\n");
    printf("st_dev %x\n", offsetof(struct stat, st_dev));
    printf("st_ino %x\n", offsetof(struct stat, st_ino));
    printf("st_mode %x\n", offsetof(struct stat, st_mode));
    printf("st_nlink %x\n", offsetof(struct stat, st_nlink));
    printf("st_uid %x\n", offsetof(struct stat, st_uid));
    printf("st_gid %x\n", offsetof(struct stat, st_gid));
    printf("st_rdev %x\n", offsetof(struct stat, st_rdev));
    printf("st_size %x\n", offsetof(struct stat, st_size));
    printf("st_atim %x\n", offsetof(struct stat, st_atim));
    printf("st_mtim %x\n", offsetof(struct stat, st_mtim));
    printf("st_ctim %x\n", offsetof(struct stat, st_ctim));
    printf("st_blksize %x\n", offsetof(struct stat, st_blksize));
    printf("st_blocks %x\n", offsetof(struct stat, st_blocks));
    printf("st_spare4 %x\n", offsetof(struct stat, st_spare4));
    printf("sizeof(stat) %x\n", sizeof(struct stat));


    fd = open("/dev/null", 0);
    printf("fd %x\n", fd);
    close(fd);

    fd = open("/dev/null", 0);
    printf("fd %x\n", fd);

    fd = open("/dev/null", 0);
    printf("fd %x\n", fd);

    fd = open("/dev/null", 0);
    printf("fd %x\n", fd);

    fd = open("foo", 0);
    printf("fd %x\n", fd);

    return 0;
}
