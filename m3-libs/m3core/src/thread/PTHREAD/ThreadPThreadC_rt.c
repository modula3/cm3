/* Copyright (C) 2024 Peter McKinna. All rights reserved. */
/* See file COPYRIGHT-BSD for details. */

#include "m3core.h"

M3_EXTERNC_BEGIN

// real time thread support. only defined for Linux so far.

int
__cdecl
ThreadPThread__pthread_setschedparam(pthread_t t, 
                                     int policy, 
                                     const struct sched_param *param)
{
#if defined(__linux__)  
  return pthread_setschedparam(t, policy, param);
#else
  return -1;
#endif
}

int
__cdecl
ThreadPThread__pthread_getschedparam(pthread_t t, 
                                     int *policy, 
                                     struct sched_param *param)
{
#if defined(__linux__)  
  return pthread_getschedparam(t, policy, param);
#else
  return -1;
#endif
}

int
__cdecl
ThreadPThread__pthread_setschedprio(pthread_t t, 
                                    int priority) 
{
#if defined(__linux__)  
  return pthread_setschedprio(t, priority);
#else
  return -1;
#endif
}
   
// mutex support 

void *
__cdecl
ThreadPThread__pthread_mutex_rt(int protocol)
{
#if defined(__linux__)  
int ret;

  //debug
  //fprintf(stderr, "pthread_mutex_rt protocol:%d\n", protocol); 
    
  pthread_mutexattr_t attr;
  pthread_mutex_t *mu;
    
  mu = (pthread_mutex_t *) calloc(1, sizeof(pthread_mutex_t));    

  ret = pthread_mutexattr_init(&attr);
  if (ret) fprintf(stderr, "ERROR: pthread_mutex_rt mutextattr_init:%d\n", ret); 

  if (protocol >= 0) {
    ret = pthread_mutexattr_setprotocol(&attr, protocol);      
    if (ret) fprintf(stderr, "ERROR: pthread_mutex_rt setprotocol:%d\n", ret);
  }
    
  ret = pthread_mutex_init(mu, &attr);
  if (ret) fprintf(stderr, "ERROR: pthread_mutex_rt init:%d\n", ret); 

  return mu;
#else
  return NULL;
#endif
}

// priority ceiling support

int
__cdecl
ThreadPThread__pthread_mutex_getprioceiling(pthread_mutex_t *mutex, 
                                            int *prioceiling) 
{
#if defined(__linux__)  
  return pthread_mutex_getprioceiling(mutex, prioceiling);
#else
  return -1;
#endif
}

int
__cdecl
ThreadPThread__pthread_mutex_setprioceiling(pthread_mutex_t *mutex, 
                                            int prioceiling,
                                            int *oldCeiling) 
{
#if defined(__linux__)  
  return pthread_mutex_setprioceiling(mutex, prioceiling, oldCeiling);
#else
  return -1;
#endif
}

// Affinity 
/* affinity is non portable (with the np sufix) */

#if defined(__linux__)
#define cpuSetType cpu_set_t
#else
#define cpuSetType int 
#endif

int
__cdecl
ThreadPThread__pthread_setaffinity_np(pthread_t t, 
                                      int cpuSetSize,
                                      cpuSetType *cpuSet) 
{
#if defined(__linux__)  
  return pthread_setaffinity_np(t, cpuSetSize, cpuSet);
#else
  return -1;
#endif
}

int
__cdecl
ThreadPThread__pthread_getaffinity_np(pthread_t t, 
                                      int cpuSetSize,
                                      cpuSetType *cpuSet) 
{
#if defined(__linux__)  
  return pthread_getaffinity_np(t, cpuSetSize, cpuSet);
#else
  return -1;
#endif
}

// set functions for affinity support

cpuSetType *
__cdecl
ThreadPThread__alloc_cpuset(int num_cpus, int *size)
{
#if defined(__linux__)  
  *size = CPU_ALLOC_SIZE(num_cpus);
  return CPU_ALLOC(num_cpus);
#else
  return NULL;
#endif
}

void
__cdecl
ThreadPThread__free_cpuset(cpuSetType *cpuset)
{
#if defined(__linux__)  
  CPU_FREE(cpuset);
#endif
}

void
__cdecl
ThreadPThread__zero_cpuset(int size, cpuSetType *cpuset)
{
#if defined(__linux__)  
  CPU_ZERO_S(size, cpuset);
#endif
}

int
__cdecl
ThreadPThread__in_cpuset(int cpu, int size, cpuSetType *cpuset)
{
#if defined(__linux__)  
  return CPU_ISSET_S(cpu, size, cpuset);
#else
  return -1;
#endif
}

void
__cdecl
ThreadPThread__add_core_to_cpuset(int core_id, int size, cpuSetType *cpuset)
{
#if defined(__linux__)  
   CPU_SET_S(core_id, size, cpuset); 
#endif
}

// max and min priority 

int
__cdecl
ThreadPThread__max_priority(int policy)
{
#if defined(__linux__)  
  return sched_get_priority_max(policy);
#else
  return -1;
#endif
}

int
__cdecl
ThreadPThread__min_priority(int policy)
{
#if defined(__linux__)  
  return sched_get_priority_min(policy);
#else
  return -1;
#endif
}

// configured cores
/* using get_nprocs_conf() from sys/sysinfo.h would have been nice 
 * but its non portable. (as is get_nprocs() for online cores) */

int
__cdecl
ThreadPThread__get_conf_cores()
{
  int numCPU = 0;
  
#if defined(__linux__)  
  numCPU = sysconf(_SC_NPROCESSORS_CONF);
/* 
//FreeBSD, MacOS X, NetBSD, OpenBSD, etc.:
#elif defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__)  
  int mib[4];
  size_t len = sizeof(numCPU); 

  // set the mib for hw.ncpu
  mib[0] = CTL_HW;
  mib[1] = HW_NCPU;

  // get the number of CPUs from the system
  sysctl(mib, 2, &numCPU, &len, NULL, 0);
*/
#endif

  return numCPU;
}

// online cores 

int
__cdecl
ThreadPThread__get_online_cores()
{
  int numCPU = 0;
  
#if defined(__linux__) 
  numCPU = sysconf(_SC_NPROCESSORS_ONLN);

/*
//FreeBSD, MacOS X, NetBSD, OpenBSD, etc.:
#elif defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__)  
  int mib[4];
  size_t len = sizeof(numCPU); 

  // set the mib for hw.ncpu 
  mib[0] = CTL_HW;
  mib[1] = HW_AVAILCPU;  // alternatively, try HW_NCPU;

  // get the number of CPUs from the system 
  sysctl(mib, 2, &numCPU, &len, NULL, 0);

  if(numCPU < 1) {
     mib[1] = HW_NCPU;
     sysctl(mib, 2, &numCPU, &len, NULL, 0);

     if(numCPU < 1) {
          numCPU = 1;
     }
  }

#elif defined(__hpux)
  numCPU = mpctl(MPC_GETNUMSPUS_SYS, NULL, NULL);

#elif defined(__INTERIX)
  numCPU = sysconf(_SC_NPROC_ONLN);
*/
#endif

  return numCPU;
}

M3_EXTERNC_END
