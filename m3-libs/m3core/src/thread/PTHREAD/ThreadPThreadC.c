/* This file exists because
 - Modula-3 static initialization does not happen early enough.
 - Cygwin's static initialization of pthreads objects is not all zeros.
*/

#include <pthread.h>

pthread_mutex_t ThreadPThread__activeMu = PTHREAD_MUTEX_INITIALIZER; /* global lock for list of active threads */
pthread_mutex_t ThreadPThread__slotMu = PTHREAD_MUTEX_INITIALIZER; /* global lock for thread slot table */
pthread_mutex_t ThreadPThread__initMu = PTHREAD_MUTEX_INITIALIZER; /* global lock for initializers */
pthread_mutex_t ThreadPThread__perfMu = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t ThreadPThread__lockMu = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t ThreadPThread__lockCond = PTHREAD_COND_INITIALIZER;
