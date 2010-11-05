/* POSIX threads dummy routines for systems without weak definitions.  */
/* Compile this one with gcc.  */
/* Copyright (C) 2003, 2004, 2005, 2007, 2008, 2009 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "tconfig.h"
#include "tm.h"
# define __gthrw_pragma(pragma) _Pragma (#pragma)
/* Define so we provide weak definitions of functions used by libobjc only.  */
#define _LIBOBJC_WEAK
#include "gthr.h"

int
pthread_once (pthread_once_t *ARG_UNUSED(once),
	      void (*func) (void) ATTRIBUTE_UNUSED)
{
  return -1;
}

int
pthread_key_create (pthread_key_t *ARG_UNUSED(key),
		    void (*dtor) (void *) ATTRIBUTE_UNUSED)
{
  return -1;
}

int
pthread_key_delete (pthread_key_t ARG_UNUSED(key))
{
  return 0;
}

void *
pthread_getspecific (pthread_key_t ARG_UNUSED(key))
{
  return 0;
}

int
pthread_setspecific (pthread_key_t ARG_UNUSED(key),
		     const void *ARG_UNUSED(ptr))
{
  return 0;
}

int
pthread_create (pthread_t *ARG_UNUSED(thread),
		const pthread_attr_t *ARG_UNUSED(attr),
		void *(*start_routine) (void *) ATTRIBUTE_UNUSED,
		void *ARG_UNUSED(arg))
{
  return 0;
}

int
pthread_join (pthread_t ARG_UNUSED(thread),
	      void **ARG_UNUSED(value_ptr))
{
  return 0;
}

void
pthread_exit (void *ARG_UNUSED(value_ptr))
{
}

int
pthread_detach (pthread_t ARG_UNUSED(thread))
{
  return 0;
}

int
pthread_cancel (pthread_t ARG_UNUSED(thread))
{
  return 0;
}

int
pthread_mutex_lock (pthread_mutex_t *ARG_UNUSED(mutex))
{
  return 0;
}

int
pthread_mutex_trylock (pthread_mutex_t *ARG_UNUSED(mutex))
{
  return 0;
}

#ifdef _POSIX_TIMEOUTS
#if _POSIX_TIMEOUTS >= 0
int
pthread_mutex_timedlock (pthread_mutex_t *ARG_UNUSED(mutex),
			 const struct timespec *ARG_UNUSED(abs_timeout))
{
  return 0;
}
#endif
#endif /* _POSIX_TIMEOUTS */

int
pthread_mutex_unlock (pthread_mutex_t *ARG_UNUSED(mutex))
{
  return 0;
}

int
pthread_mutexattr_init (pthread_mutexattr_t *ARG_UNUSED(attr))
{
  return 0;
}

int
pthread_mutexattr_settype (pthread_mutexattr_t *ARG_UNUSED(attr),
			   int ARG_UNUSED(type))
{
  return 0;
}

int
pthread_mutexattr_destroy (pthread_mutexattr_t *ARG_UNUSED(attr))
{
  return 0;
}

int
pthread_cond_broadcast (pthread_cond_t *ARG_UNUSED(cond))
{
  return 0;
}

int
pthread_cond_destroy (pthread_cond_t *ARG_UNUSED(cond))
{
  return 0;
}

int
pthread_cond_init (pthread_cond_t *ARG_UNUSED(cond),
		   const pthread_condattr_t *ARG_UNUSED(attr))
{
  return 0;
}

int
pthread_cond_signal (pthread_cond_t *ARG_UNUSED(cond))
{
  return 0;
}

int
pthread_cond_wait (pthread_cond_t *ARG_UNUSED(cond),
		   pthread_mutex_t *ARG_UNUSED(mutex))
{
  return 0;
}

int
pthread_cond_timedwait (pthread_cond_t *ARG_UNUSED(cond), 
			pthread_mutex_t *ARG_UNUSED(mutex),
			const struct timespec *ARG_UNUSED(abstime))
{
  return 0;
}

int
pthread_mutex_init (pthread_mutex_t *ARG_UNUSED(mutex),
		    const pthread_mutexattr_t *ARG_UNUSED(attr))
{
  return 0;
}

int
pthread_mutex_destroy (pthread_mutex_t *ARG_UNUSED(mutex))
{
  return 0;
}

pthread_t
pthread_self (void)
{
  return (pthread_t) 0;
}

#ifdef _POSIX_PRIORITY_SCHEDULING
#ifdef _POSIX_THREAD_PRIORITY_SCHEDULING
int
sched_get_priority_max (int ARG_UNUSED(policy))
{
  return 0;
}

int
sched_get_priority_min (int ARG_UNUSED(policy))
{
  return 0;
}
#endif /* _POSIX_THREAD_PRIORITY_SCHEDULING */
#endif /* _POSIX_PRIORITY_SCHEDULING */

int
sched_yield (void)
{
  return 0;
}

int
pthread_attr_destroy (pthread_attr_t *ARG_UNUSED(attr))
{
  return 0;
}

int
pthread_attr_init (pthread_attr_t *ARG_UNUSED(attr))
{
  return 0;
}

int
pthread_attr_setdetachstate (pthread_attr_t *ARG_UNUSED(attr),
			     int ARG_UNUSED(detachstate))
{
  return 0;
}

#ifdef _POSIX_THREAD_PRIORITY_SCHEDULING
int
pthread_getschedparam (pthread_t ARG_UNUSED(thread),
		       int *ARG_UNUSED(policy),
		       struct sched_param *ARG_UNUSED(param))
{
  return 0;
}

int
pthread_setschedparam (pthread_t ARG_UNUSED(thread),
		       int ARG_UNUSED(policy),
		       const struct sched_param *ARG_UNUSED(param))
{
  return 0;
}
#endif /* _POSIX_THREAD_PRIORITY_SCHEDULING */

