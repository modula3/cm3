/* Modula-3: modified */

/* Simple garbage collection for the GNU compiler.
   Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008,
   2009, 2010 Free Software Foundation, Inc.

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

/* Generic garbage collection (GC) functions and data, not specific to
   any particular GC implementation.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "hashtab.h"
#include "ggc.h"
#include "ggc-internal.h"
#include "diagnostic-core.h"
#include "params.h"
#include "hosthooks.h"
#include "hosthooks-def.h"
#include "plugin.h"
#include "vec.h"
#include "timevar.h"

/* When set, ggc_collect will do collection.  */
bool ggc_force_collect;

/* When true, protect the contents of the identifier hash table.  */
bool ggc_protect_identifiers = true;

/* Statistics about the allocation.  */
static ggc_statistics *ggc_stats;

struct traversal_state;

static int ggc_htab_delete (void **, void *);

/* Maintain global roots that are preserved during GC.  */

/* Process a slot of an htab by deleting it if it has not been marked.  */

static int
ggc_htab_delete (void **slot, void *info)
{
  const struct ggc_cache_tab *r = (const struct ggc_cache_tab *) info;

  if (! (*r->marked_p) (*slot))
    htab_clear_slot (*r->base, slot);
  else
    (*r->cb) (*slot);

  return 1;
}


/* This extra vector of dynamically registered root_tab-s is used by
   ggc_mark_roots and gives the ability to dynamically add new GGC root
   tables, for instance from some plugins; this vector is on the heap
   since it is used by GGC internally.  */
typedef const struct ggc_root_tab *const_ggc_root_tab_t;
DEF_VEC_P(const_ggc_root_tab_t);
DEF_VEC_ALLOC_P(const_ggc_root_tab_t, heap);
static VEC(const_ggc_root_tab_t, heap) *extra_root_vec;

/* Dynamically register a new GGC root table RT. This is useful for
   plugins. */

void
ggc_register_root_tab (const struct ggc_root_tab* rt)
{
  if (rt)
    VEC_safe_push (const_ggc_root_tab_t, heap, extra_root_vec, rt);
}

/* This extra vector of dynamically registered cache_tab-s is used by
   ggc_mark_roots and gives the ability to dynamically add new GGC cache
   tables, for instance from some plugins; this vector is on the heap
   since it is used by GGC internally.  */
typedef const struct ggc_cache_tab *const_ggc_cache_tab_t;
DEF_VEC_P(const_ggc_cache_tab_t);
DEF_VEC_ALLOC_P(const_ggc_cache_tab_t, heap);
static VEC(const_ggc_cache_tab_t, heap) *extra_cache_vec;

/* Dynamically register a new GGC cache table CT. This is useful for
   plugins. */

void
ggc_register_cache_tab (const struct ggc_cache_tab* ct)
{
  if (ct)
    VEC_safe_push (const_ggc_cache_tab_t, heap, extra_cache_vec, ct);
}

/* Scan a hash table that has objects which are to be deleted if they are not
   already marked.  */

static void
ggc_scan_cache_tab (const_ggc_cache_tab_t ctp)
{
  const struct ggc_cache_tab *cti;

  for (cti = ctp; cti->base != NULL; cti++)
    if (*cti->base)
      {
        ggc_set_mark (*cti->base);
        htab_traverse_noresize (*cti->base, ggc_htab_delete,
                                CONST_CAST (void *, (const void *)cti));
        ggc_set_mark ((*cti->base)->entries);
      }
}

/* Mark all the roots in the table RT.  */

static void
ggc_mark_root_tab (const_ggc_root_tab_t rt)
{
  size_t i;

  for ( ; rt->base != NULL; rt++)
    for (i = 0; i < rt->nelt; i++)
      (*rt->cb) (*(void **) ((char *)rt->base + rt->stride * i));
}

/* Iterate through all registered roots and mark each element.  */

void
ggc_mark_roots (void)
{
  const struct ggc_root_tab *const *rt;
  const_ggc_root_tab_t rtp, rti;
  const struct ggc_cache_tab *const *ct;
  const_ggc_cache_tab_t ctp;
  size_t i;

  for (rt = gt_ggc_deletable_rtab; *rt; rt++)
    for (rti = *rt; rti->base != NULL; rti++)
      memset (rti->base, 0, rti->stride);

  for (rt = gt_ggc_rtab; *rt; rt++)
    ggc_mark_root_tab (*rt);

  FOR_EACH_VEC_ELT (const_ggc_root_tab_t, extra_root_vec, i, rtp)
    ggc_mark_root_tab (rtp);

  if (ggc_protect_identifiers)
    ggc_mark_stringpool ();

  /* Now scan all hash tables that have objects which are to be deleted if
     they are not already marked.  */
  for (ct = gt_ggc_cache_rtab; *ct; ct++)
    ggc_scan_cache_tab (*ct);

  FOR_EACH_VEC_ELT (const_ggc_cache_tab_t, extra_cache_vec, i, ctp)
    ggc_scan_cache_tab (ctp);

  if (! ggc_protect_identifiers)
    ggc_purge_stringpool ();

  /* Some plugins may call ggc_set_mark from here.  */
  invoke_plugin_callbacks (PLUGIN_GGC_MARKING, NULL);
}

/* Allocate a block of memory, then clear it.  */
void *
ggc_internal_cleared_alloc_stat (size_t size MEM_STAT_DECL)
{
  void *buf = ggc_internal_alloc_stat (size PASS_MEM_STAT);
  memset (buf, 0, size);
  return buf;
}

/* Resize a block of memory, possibly re-allocating it.  */
void *
ggc_realloc_stat (void *x, size_t size MEM_STAT_DECL)
{
  void *r;
  size_t old_size;

  if (x == NULL)
    return ggc_internal_alloc_stat (size PASS_MEM_STAT);

  old_size = ggc_get_size (x);

  if (size <= old_size)
    {
      /* Mark the unwanted memory as unaccessible.  We also need to make
	 the "new" size accessible, since ggc_get_size returns the size of
	 the pool, not the size of the individually allocated object, the
	 size which was previously made accessible.  Unfortunately, we
	 don't know that previously allocated size.  Without that
	 knowledge we have to lose some initialization-tracking for the
	 old parts of the object.  An alternative is to mark the whole
	 old_size as reachable, but that would lose tracking of writes
	 after the end of the object (by small offsets).  Discard the
	 handle to avoid handle leak.  */
      VALGRIND_DISCARD (VALGRIND_MAKE_MEM_NOACCESS ((char *) x + size,
						    old_size - size));
      VALGRIND_DISCARD (VALGRIND_MAKE_MEM_DEFINED (x, size));
      return x;
    }

  r = ggc_internal_alloc_stat (size PASS_MEM_STAT);

  /* Since ggc_get_size returns the size of the pool, not the size of the
     individually allocated object, we'd access parts of the old object
     that were marked invalid with the memcpy below.  We lose a bit of the
     initialization-tracking since some of it may be uninitialized.  */
  VALGRIND_DISCARD (VALGRIND_MAKE_MEM_DEFINED (x, old_size));

  memcpy (r, x, old_size);

  /* The old object is not supposed to be used anymore.  */
  ggc_free (x);

  return r;
}

void *
ggc_cleared_alloc_htab_ignore_args (size_t c ATTRIBUTE_UNUSED,
				    size_t n ATTRIBUTE_UNUSED)
{
  gcc_assert (c * n == sizeof (struct htab));
  return ggc_alloc_cleared_htab ();
}

/* TODO: once we actually use type information in GGC, create a new tag
   gt_gcc_ptr_array and use it for pointer arrays.  */
void *
ggc_cleared_alloc_ptr_array_two_args (size_t c, size_t n)
{
  gcc_assert (sizeof (PTR *) == n);
  return ggc_internal_cleared_vec_alloc (sizeof (PTR *), c);
}

/* These are for splay_tree_new_ggc.  */
void *
ggc_splay_alloc (enum gt_types_enum obj_type ATTRIBUTE_UNUSED, int sz,
		 void *nl)
{
  gcc_assert (!nl);
  return ggc_internal_alloc (sz);
}

void
ggc_splay_dont_free (void * x ATTRIBUTE_UNUSED, void *nl)
{
  gcc_assert (!nl);
}

/* Print statistics that are independent of the collector in use.  */
#define SCALE(x) ((unsigned long) ((x) < 1024*10 \
		  ? (x) \
		  : ((x) < 1024*1024*10 \
		     ? (x) / 1024 \
		     : (x) / (1024*1024))))
#define LABEL(x) ((x) < 1024*10 ? ' ' : ((x) < 1024*1024*10 ? 'k' : 'M'))

void
ggc_print_common_statistics (FILE *stream ATTRIBUTE_UNUSED,
			     ggc_statistics *stats)
{
  /* Set the pointer so that during collection we will actually gather
     the statistics.  */
  ggc_stats = stats;

  /* Then do one collection to fill in the statistics.  */
  ggc_collect ();

  /* At present, we don't really gather any interesting statistics.  */

  /* Don't gather statistics any more.  */
  ggc_stats = NULL;
}

#if !defined ENABLE_GC_CHECKING && !defined ENABLE_GC_ALWAYS_COLLECT

/* Modify the bound based on rlimits.  */
static double
ggc_rlimit_bound (double limit)
{
#if defined(HAVE_GETRLIMIT)
  struct rlimit rlim;
# if defined (RLIMIT_AS)
  /* RLIMIT_AS is what POSIX says is the limit on mmap.  Presumably
     any OS which has RLIMIT_AS also has a working mmap that GCC will use.  */
  if (getrlimit (RLIMIT_AS, &rlim) == 0
      && rlim.rlim_cur != (rlim_t) RLIM_INFINITY
      && rlim.rlim_cur < limit)
    limit = rlim.rlim_cur;
# elif defined (RLIMIT_DATA)
  /* ... but some older OSs bound mmap based on RLIMIT_DATA, or we
     might be on an OS that has a broken mmap.  (Others don't bound
     mmap at all, apparently.)  */
  if (getrlimit (RLIMIT_DATA, &rlim) == 0
      && rlim.rlim_cur != (rlim_t) RLIM_INFINITY
      && rlim.rlim_cur < limit
      /* Darwin has this horribly bogus default setting of
	 RLIMIT_DATA, to 6144Kb.  No-one notices because RLIMIT_DATA
	 appears to be ignored.  Ignore such silliness.  If a limit
	 this small was actually effective for mmap, GCC wouldn't even
	 start up.  */
      && rlim.rlim_cur >= 8 * 1024 * 1024)
    limit = rlim.rlim_cur;
# endif /* RLIMIT_AS or RLIMIT_DATA */
#endif /* HAVE_GETRLIMIT */

  return limit;
}

/* Heuristic to set a default for GGC_MIN_EXPAND.  */
static int
ggc_min_expand_heuristic (void)
{
  double min_expand = physmem_total();

  /* Adjust for rlimits.  */
  min_expand = ggc_rlimit_bound (min_expand);

  /* The heuristic is a percentage equal to 30% + 70%*(RAM/1GB), yielding
     a lower bound of 30% and an upper bound of 100% (when RAM >= 1GB).  */
  min_expand /= 1024*1024*1024;
  min_expand *= 70;
  min_expand = MIN (min_expand, 70);
  min_expand += 30;

  return min_expand;
}

/* Heuristic to set a default for GGC_MIN_HEAPSIZE.  */
static int
ggc_min_heapsize_heuristic (void)
{
  double phys_kbytes = physmem_total();
  double limit_kbytes = ggc_rlimit_bound (phys_kbytes * 2);

  phys_kbytes /= 1024; /* Convert to Kbytes.  */
  limit_kbytes /= 1024;

  /* The heuristic is RAM/8, with a lower bound of 4M and an upper
     bound of 128M (when RAM >= 1GB).  */
  phys_kbytes /= 8;

#if defined(HAVE_GETRLIMIT) && defined (RLIMIT_RSS)
  /* Try not to overrun the RSS limit while doing garbage collection.
     The RSS limit is only advisory, so no margin is subtracted.  */
 {
   struct rlimit rlim;
   if (getrlimit (RLIMIT_RSS, &rlim) == 0
       && rlim.rlim_cur != (rlim_t) RLIM_INFINITY)
     phys_kbytes = MIN (phys_kbytes, rlim.rlim_cur / 1024);
 }
# endif

  /* Don't blindly run over our data limit; do GC at least when the
     *next* GC would be within 20Mb of the limit or within a quarter of
     the limit, whichever is larger.  If GCC does hit the data limit,
     compilation will fail, so this tries to be conservative.  */
  limit_kbytes = MAX (0, limit_kbytes - MAX (limit_kbytes / 4, 20 * 1024));
  limit_kbytes = (limit_kbytes * 100) / (110 + ggc_min_expand_heuristic ());
  phys_kbytes = MIN (phys_kbytes, limit_kbytes);

  phys_kbytes = MAX (phys_kbytes, 4 * 1024);
  phys_kbytes = MIN (phys_kbytes, 128 * 1024);

  return phys_kbytes;
}
#endif

void
init_ggc_heuristics (void)
{
#if !defined ENABLE_GC_CHECKING && !defined ENABLE_GC_ALWAYS_COLLECT
  set_default_param_value (GGC_MIN_EXPAND, ggc_min_expand_heuristic ());
  set_default_param_value (GGC_MIN_HEAPSIZE, ggc_min_heapsize_heuristic ());
#endif
}

#ifdef GATHER_STATISTICS

/* Datastructure used to store per-call-site statistics.  */
struct loc_descriptor
{
  const char *file;
  int line;
  const char *function;
  int times;
  size_t allocated;
  size_t overhead;
  size_t freed;
  size_t collected;
};

/* Hashtable used for statistics.  */
static htab_t loc_hash;

/* Hash table helpers functions.  */
static hashval_t
hash_descriptor (const void *p)
{
  const struct loc_descriptor *const d = (const struct loc_descriptor *) p;

  return htab_hash_pointer (d->function) | d->line;
}

static int
eq_descriptor (const void *p1, const void *p2)
{
  const struct loc_descriptor *const d = (const struct loc_descriptor *) p1;
  const struct loc_descriptor *const d2 = (const struct loc_descriptor *) p2;

  return (d->file == d2->file && d->line == d2->line
	  && d->function == d2->function);
}

/* Hashtable converting address of allocated field to loc descriptor.  */
static htab_t ptr_hash;
struct ptr_hash_entry
{
  void *ptr;
  struct loc_descriptor *loc;
  size_t size;
};

/* Hash table helpers functions.  */
static hashval_t
hash_ptr (const void *p)
{
  const struct ptr_hash_entry *const d = (const struct ptr_hash_entry *) p;

  return htab_hash_pointer (d->ptr);
}

static int
eq_ptr (const void *p1, const void *p2)
{
  const struct ptr_hash_entry *const p = (const struct ptr_hash_entry *) p1;

  return (p->ptr == p2);
}

/* Return descriptor for given call site, create new one if needed.  */
static struct loc_descriptor *
loc_descriptor (const char *name, int line, const char *function)
{
  struct loc_descriptor loc;
  struct loc_descriptor **slot;

  loc.file = name;
  loc.line = line;
  loc.function = function;
  if (!loc_hash)
    loc_hash = htab_create (10, hash_descriptor, eq_descriptor, NULL);

  slot = (struct loc_descriptor **) htab_find_slot (loc_hash, &loc, INSERT);
  if (*slot)
    return *slot;
  *slot = XCNEW (struct loc_descriptor);
  (*slot)->file = name;
  (*slot)->line = line;
  (*slot)->function = function;
  return *slot;
}

/* Record ALLOCATED and OVERHEAD bytes to descriptor NAME:LINE (FUNCTION).  */
void
ggc_record_overhead (size_t allocated, size_t overhead, void *ptr,
		     const char *name, int line, const char *function)
{
  struct loc_descriptor *loc = loc_descriptor (name, line, function);
  struct ptr_hash_entry *p = XNEW (struct ptr_hash_entry);
  PTR *slot;

  p->ptr = ptr;
  p->loc = loc;
  p->size = allocated + overhead;
  if (!ptr_hash)
    ptr_hash = htab_create (10, hash_ptr, eq_ptr, NULL);
  slot = htab_find_slot_with_hash (ptr_hash, ptr, htab_hash_pointer (ptr), INSERT);
  gcc_assert (!*slot);
  *slot = p;

  loc->times++;
  loc->allocated+=allocated;
  loc->overhead+=overhead;
}

/* Helper function for prune_overhead_list.  See if SLOT is still marked and
   remove it from hashtable if it is not.  */
static int
ggc_prune_ptr (void **slot, void *b ATTRIBUTE_UNUSED)
{
  struct ptr_hash_entry *p = (struct ptr_hash_entry *) *slot;
  if (!ggc_marked_p (p->ptr))
    {
      p->loc->collected += p->size;
      htab_clear_slot (ptr_hash, slot);
      free (p);
    }
  return 1;
}

/* After live values has been marked, walk all recorded pointers and see if
   they are still live.  */
void
ggc_prune_overhead_list (void)
{
  htab_traverse (ptr_hash, ggc_prune_ptr, NULL);
}

/* Notice that the pointer has been freed.  */
void
ggc_free_overhead (void *ptr)
{
  PTR *slot = htab_find_slot_with_hash (ptr_hash, ptr, htab_hash_pointer (ptr),
					NO_INSERT);
  struct ptr_hash_entry *p;
  /* The pointer might be not found if a PCH read happened between allocation
     and ggc_free () call.  FIXME: account memory properly in the presence of
     PCH. */
  if (!slot)
      return;
  p = (struct ptr_hash_entry *) *slot;
  p->loc->freed += p->size;
  htab_clear_slot (ptr_hash, slot);
  free (p);
}

/* Helper for qsort; sort descriptors by amount of memory consumed.  */
static int
final_cmp_statistic (const void *loc1, const void *loc2)
{
  const struct loc_descriptor *const l1 =
    *(const struct loc_descriptor *const *) loc1;
  const struct loc_descriptor *const l2 =
    *(const struct loc_descriptor *const *) loc2;
  long diff;
  diff = ((long)(l1->allocated + l1->overhead - l1->freed) -
	  (l2->allocated + l2->overhead - l2->freed));
  return diff > 0 ? 1 : diff < 0 ? -1 : 0;
}

/* Helper for qsort; sort descriptors by amount of memory consumed.  */
static int
cmp_statistic (const void *loc1, const void *loc2)
{
  const struct loc_descriptor *const l1 =
    *(const struct loc_descriptor *const *) loc1;
  const struct loc_descriptor *const l2 =
    *(const struct loc_descriptor *const *) loc2;
  long diff;

  diff = ((long)(l1->allocated + l1->overhead - l1->freed - l1->collected) -
	  (l2->allocated + l2->overhead - l2->freed - l2->collected));
  if (diff)
    return diff > 0 ? 1 : diff < 0 ? -1 : 0;
  diff =  ((long)(l1->allocated + l1->overhead - l1->freed) -
	   (l2->allocated + l2->overhead - l2->freed));
  return diff > 0 ? 1 : diff < 0 ? -1 : 0;
}

/* Collect array of the descriptors from hashtable.  */
static struct loc_descriptor **loc_array;
static int
add_statistics (void **slot, void *b)
{
  int *n = (int *)b;
  loc_array[*n] = (struct loc_descriptor *) *slot;
  (*n)++;
  return 1;
}

/* Dump per-site memory statistics.  */
#endif
void
dump_ggc_loc_statistics (bool final ATTRIBUTE_UNUSED)
{
#ifdef GATHER_STATISTICS
  int nentries = 0;
  char s[4096];
  size_t collected = 0, freed = 0, allocated = 0, overhead = 0, times = 0;
  int i;

  ggc_force_collect = true;
  ggc_collect ();

  loc_array = XCNEWVEC (struct loc_descriptor *, loc_hash->n_elements);
  fprintf (stderr, "-------------------------------------------------------\n");
  fprintf (stderr, "\n%-48s %10s       %10s       %10s       %10s       %10s\n",
	   "source location", "Garbage", "Freed", "Leak", "Overhead", "Times");
  fprintf (stderr, "-------------------------------------------------------\n");
  htab_traverse (loc_hash, add_statistics, &nentries);
  qsort (loc_array, nentries, sizeof (*loc_array),
	 final ? final_cmp_statistic : cmp_statistic);
  for (i = 0; i < nentries; i++)
    {
      struct loc_descriptor *d = loc_array[i];
      allocated += d->allocated;
      times += d->times;
      freed += d->freed;
      collected += d->collected;
      overhead += d->overhead;
    }
  for (i = 0; i < nentries; i++)
    {
      struct loc_descriptor *d = loc_array[i];
      if (d->allocated)
	{
	  const char *s1 = d->file;
	  const char *s2;
	  while ((s2 = strstr (s1, "gcc/")))
	    s1 = s2 + 4;
	  sprintf (s, "%s:%i (%s)", s1, d->line, d->function);
	  s[48] = 0;
	  fprintf (stderr, "%-48s %10li:%4.1f%% %10li:%4.1f%% %10li:%4.1f%% %10li:%4.1f%% %10li\n", s,
		   (long)d->collected,
		   (d->collected) * 100.0 / collected,
		   (long)d->freed,
		   (d->freed) * 100.0 / freed,
		   (long)(d->allocated + d->overhead - d->freed - d->collected),
		   (d->allocated + d->overhead - d->freed - d->collected) * 100.0
		   / (allocated + overhead - freed - collected),
		   (long)d->overhead,
		   d->overhead * 100.0 / overhead,
		   (long)d->times);
	}
    }
  fprintf (stderr, "%-48s %10ld       %10ld       %10ld       %10ld       %10ld\n",
	   "Total", (long)collected, (long)freed,
	   (long)(allocated + overhead - freed - collected), (long)overhead,
	   (long)times);
  fprintf (stderr, "%-48s %10s       %10s       %10s       %10s       %10s\n",
	   "source location", "Garbage", "Freed", "Leak", "Overhead", "Times");
  fprintf (stderr, "-------------------------------------------------------\n");
  ggc_force_collect = false;
#endif
}
