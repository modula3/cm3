#include <semaphore.h>

#ifdef __FreeBSD__
#include <stdlib.h>
#else
#include <malloc.h>
#endif

#include <assert.h>
#include <stdio.h>
/* $Id$ */

/* Author : Mika Nystrom <mika@alum.mit.edu> */

#define DEBUG 0
int 
SemaphoreC__sem_wait(void *sem)           
{ 
  int r;

  if(DEBUG)fprintf(stderr, "sem_wait(0x%x)--->\n", sem);
  r = sem_wait(sem); 
  if(DEBUG)fprintf(stderr, "<---sem_wait(0x%x)\n", sem);

  return r;
}

int 
SemaphoreC__sem_post(void * sem)           
{ 
  int r;
  if(DEBUG)fprintf(stderr, "sem_post(0x%x)\n", sem);
  r = sem_post(sem); 
  return r;
}

int 
SemaphoreC__sem_getvalue(void *sem, int *value) 
{ 
  int r = sem_getvalue(sem, value); 
  return r;
}

void *
SemaphoreC__sem_alloc(void) { 
  void *a=malloc(sizeof(sem_t)); 
  assert(a);
  return a;
}

int 
SemaphoreC__sem_init(void *sem)           
{ 
  return sem_init(sem, 0, 0); 
}

int 
SemaphoreC__sem_destroy(void *sem)           
{ 
  int r = sem_destroy(sem); 
  free(sem);
  return r;
}
