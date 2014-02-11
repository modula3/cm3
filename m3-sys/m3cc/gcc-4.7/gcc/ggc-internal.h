/* Modula-3: modified */

/* Garbage collection for the GNU compiler.  Internal definitions
   for ggc-*.c and stringpool.c.

   Copyright (C) 2009, 2010 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_GGC_INTERNAL_H
#define GCC_GGC_INTERNAL_H

#include "ggc.h"

/* Call ggc_set_mark on all the roots.  */
extern void ggc_mark_roots (void);

/* Stringpool.  */

/* Mark the entries in the string pool.  */
extern void ggc_mark_stringpool	(void);

/* Purge the entries in the string pool.  */
extern void ggc_purge_stringpool (void);

/* Allocation and collection.  */

/* When set, ggc_collect will do collection.  */
extern bool ggc_force_collect;

extern void ggc_record_overhead (size_t, size_t, void * MEM_STAT_DECL);

extern void ggc_free_overhead (void *);

extern void ggc_prune_overhead_list (void);

/* Return the number of bytes allocated at the indicated address.  */
extern size_t ggc_get_size (const void *);


/* Statistics.  */

/* This structure contains the statistics common to all collectors.
   Particular collectors can extend this structure.  */
typedef struct ggc_statistics
{
  /* At present, we don't really gather any interesting statistics.  */
  int unused;
} ggc_statistics;

/* Used by the various collectors to gather and print statistics that
   do not depend on the collector in use.  */
extern void ggc_print_common_statistics (FILE *, ggc_statistics *);

#endif
