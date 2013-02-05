/* Modula-3: modified */

/* Map logical line numbers to (source file, line number) pairs.
   Copyright (C) 2001, 2003, 2004, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.

 In other words, you are welcome to use, share and improve this program.
 You are forbidden to forbid anyone else to use, share and improve
 what you give them.   Help stamp out software-hoarding!  */

#include "config.h"
#include "system.h"
#include "line-map.h"
#include "cpplib.h"

static void trace_include (const struct line_maps *, const struct line_map *);
static const struct line_map * linemap_ordinary_map_lookup (struct line_maps *,
							    source_location);
static const struct line_map* linemap_macro_map_lookup (struct line_maps *,
							source_location);
static source_location linemap_macro_map_loc_to_def_point
(const struct line_map*, source_location);
static source_location linemap_macro_map_loc_unwind_toward_spelling
(const struct line_map*, source_location);
static source_location linemap_macro_map_loc_to_exp_point
(const struct line_map*, source_location);
static source_location linemap_macro_loc_to_spelling_point
(struct line_maps *, source_location, const struct line_map **);
static source_location linemap_macro_loc_to_def_point (struct line_maps *,
						       source_location,
						       const struct line_map **);
static source_location linemap_macro_loc_to_exp_point (struct line_maps *,
						       source_location,
						       const struct line_map **);

/* Counters defined in macro.c.  */
extern unsigned num_expanded_macros_counter;
extern unsigned num_macro_tokens_counter;

/* Initialize a line map set.  */

void
linemap_init (struct line_maps *set)
{
  memset (set, 0, sizeof (struct line_maps));
  set->highest_location = RESERVED_LOCATION_COUNT - 1;
  set->highest_line = RESERVED_LOCATION_COUNT - 1;
}

/* Check for and warn about line_maps entered but not exited.  */

void
linemap_check_files_exited (struct line_maps *set)
{
  struct line_map *map;
  /* Depending upon whether we are handling preprocessed input or
     not, this can be a user error or an ICE.  */
  for (map = LINEMAPS_LAST_ORDINARY_MAP (set);
       ! MAIN_FILE_P (map);
       map = INCLUDED_FROM (set, map))
    fprintf (stderr, "line-map.c: file \"%s\" entered but not left\n",
	     ORDINARY_MAP_FILE_NAME (map));
}

/* Create a new line map in the line map set SET, and return it.
   REASON is the reason of creating the map. It determines the type
   of map created (ordinary or macro map). Note that ordinary maps and
   macro maps are allocated in different memory location.  */

static struct line_map *
new_linemap (struct line_maps *set,
	     enum lc_reason reason)
{
  /* Depending on this variable, a macro map would be allocated in a
     different memory location than an ordinary map.  */
  bool macro_map_p = (reason == LC_ENTER_MACRO);
  struct line_map *result;

  if (LINEMAPS_USED (set, macro_map_p) == LINEMAPS_ALLOCATED (set, macro_map_p))
    {
      /* We ran out of allocated line maps. Let's allocate more.  */
      unsigned alloc_size;

      line_map_realloc reallocator
	= set->reallocator ? set->reallocator : xrealloc;
      line_map_round_alloc_size_func round_alloc_size =
	set->round_alloc_size;

      /* We are going to execute some dance to try to reduce the
	 overhead of the memory allocator, in case we are using the
	 ggc-page.c one.
	 
	 The actual size of memory we are going to get back from the
	 allocator is the smallest power of 2 that is greater than the
	 size we requested.  So let's consider that size then.  */

      alloc_size =
	(2 * LINEMAPS_ALLOCATED (set, macro_map_p) +  256)
	* sizeof (struct line_map);

      /* Get the actual size of memory that is going to be allocated
	 by the allocator.  */
      alloc_size = round_alloc_size (alloc_size);

      /* Now alloc_size contains the exact memory size we would get if
	 we have asked for the initial alloc_size amount of memory.
	 Let's get back to the number of macro map that amounts
	 to.  */
      LINEMAPS_ALLOCATED (set, macro_map_p) =
	alloc_size / (sizeof (struct line_map));

      /* And now let's really do the re-allocation.  */
      LINEMAPS_MAPS (set, macro_map_p) =
	(struct line_map *) (*reallocator)
	(LINEMAPS_MAPS (set, macro_map_p),
	 (LINEMAPS_ALLOCATED (set, macro_map_p)
	  * sizeof (struct line_map)));

      result =
	&LINEMAPS_MAPS (set, macro_map_p)[LINEMAPS_USED (set, macro_map_p)];
      memset (result, 0,
	      ((LINEMAPS_ALLOCATED (set, macro_map_p)
		- LINEMAPS_USED (set, macro_map_p))
	       * sizeof (struct line_map)));
    }
  else
    result =
      &LINEMAPS_MAPS (set, macro_map_p)[LINEMAPS_USED (set, macro_map_p)];

  LINEMAPS_USED (set, macro_map_p)++;

  result->reason = reason;
  return result;
}

/* Add a mapping of logical source line to physical source file and
   line number.

   The text pointed to by TO_FILE must have a lifetime
   at least as long as the final call to lookup_line ().  An empty
   TO_FILE means standard input.  If reason is LC_LEAVE, and
   TO_FILE is NULL, then TO_FILE, TO_LINE and SYSP are given their
   natural values considering the file we are returning to.

   FROM_LINE should be monotonic increasing across calls to this
   function.  A call to this function can relocate the previous set of
   maps, so any stored line_map pointers should not be used.  */

const struct line_map *
linemap_add (struct line_maps *set, enum lc_reason reason,
	     unsigned int sysp, const char *to_file, linenum_type to_line)
{
  struct line_map *map;
  source_location start_location = set->highest_location + 1;

  linemap_assert (!(LINEMAPS_ORDINARY_USED (set)
		    && (start_location
			< MAP_START_LOCATION (LINEMAPS_LAST_ORDINARY_MAP (set)))));

  /* When we enter the file for the first time reason cannot be
     LC_RENAME.  */
  linemap_assert (!(set->depth == 0 && reason == LC_RENAME));

  /* If we are leaving the main file, return a NULL map.  */
  if (reason == LC_LEAVE
      && MAIN_FILE_P (LINEMAPS_LAST_ORDINARY_MAP (set))
      && to_file == NULL)
    {
      set->depth--;
      return NULL;
    }

  map = new_linemap (set, reason);

  if (to_file && *to_file == '\0' && reason != LC_RENAME_VERBATIM)
    to_file = "<stdin>";

  if (reason == LC_RENAME_VERBATIM)
    reason = LC_RENAME;

  if (reason == LC_LEAVE)
    {
      /* When we are just leaving an "included" file, and jump to the next
	 location inside the "includer" right after the #include
	 "included", this variable points the map in use right before the
	 #include "included", inside the same "includer" file.  */
      struct line_map *from;
      bool error;

      if (MAIN_FILE_P (map - 1))
	{
	  /* So this _should_ means we are leaving the main file --
	     effectively ending the compilation unit. But to_file not
	     being NULL means the caller thinks we are leaving to
	     another file. This is an erroneous behaviour but we'll
	     try to recover from it. Let's pretend we are not leaving
	     the main file.  */
	  error = true;
          reason = LC_RENAME;
          from = map - 1;
	}
      else
	{
	  /* (MAP - 1) points to the map we are leaving. The
	     map from which (MAP - 1) got included should be the map
	     that comes right before MAP in the same file.  */
	  from = INCLUDED_FROM (set, map - 1);
	  error = to_file && filename_cmp (ORDINARY_MAP_FILE_NAME (from),
					   to_file);
	}

      /* Depending upon whether we are handling preprocessed input or
	 not, this can be a user error or an ICE.  */
      if (error)
	fprintf (stderr, "line-map.c: file \"%s\" left but not entered\n",
		 to_file);

      /* A TO_FILE of NULL is special - we use the natural values.  */
      if (error || to_file == NULL)
	{
	  to_file = ORDINARY_MAP_FILE_NAME (from);
	  to_line = SOURCE_LINE (from, from[1].start_location);
	  sysp = ORDINARY_MAP_IN_SYSTEM_HEADER_P (from);
	}
    }

  linemap_assert (reason != LC_ENTER_MACRO);
  ORDINARY_MAP_IN_SYSTEM_HEADER_P (map) = sysp;
  MAP_START_LOCATION (map) = start_location;
  ORDINARY_MAP_FILE_NAME (map) = to_file;
  ORDINARY_MAP_STARTING_LINE_NUMBER (map) = to_line;
  LINEMAPS_ORDINARY_CACHE (set) = LINEMAPS_ORDINARY_USED (set) - 1;
  ORDINARY_MAP_NUMBER_OF_COLUMN_BITS (map) = 0;
  set->highest_location = start_location;
  set->highest_line = start_location;
  set->max_column_hint = 0;

  if (reason == LC_ENTER)
    {
      ORDINARY_MAP_INCLUDER_FILE_INDEX (map) = 
	set->depth == 0 ? -1 : (int) (LINEMAPS_ORDINARY_USED (set) - 2);
      set->depth++;
      if (set->trace_includes)
	trace_include (set, map);
    }
  else if (reason == LC_RENAME)
    ORDINARY_MAP_INCLUDER_FILE_INDEX (map) =
      ORDINARY_MAP_INCLUDER_FILE_INDEX (&map[-1]);
  else if (reason == LC_LEAVE)
    {
      set->depth--;
      ORDINARY_MAP_INCLUDER_FILE_INDEX (map) =
	ORDINARY_MAP_INCLUDER_FILE_INDEX (INCLUDED_FROM (set, map - 1));
    }

  return map;
}

/* Returns TRUE if the line table set tracks token locations accross
   macro expansion, FALSE otherwise.  */

bool
linemap_tracks_macro_expansion_locs_p (struct line_maps *set)
{
  return LINEMAPS_MACRO_MAPS (set) != NULL;
}

/* Return a source_location for the start (i.e. column==0) of
   (physical) line TO_LINE in the current source file (as in the
   most recent linemap_add).   MAX_COLUMN_HINT is the highest column
   number we expect to use in this line (but it does not change
   the highest_location).  */

source_location
linemap_line_start (struct line_maps *set, linenum_type to_line,
		    unsigned int max_column_hint)
{
  struct line_map *map = LINEMAPS_LAST_ORDINARY_MAP (set);
  source_location highest = set->highest_location;
  source_location r;
  linenum_type last_line =
    SOURCE_LINE (map, set->highest_line);
  int line_delta = to_line - last_line;
  bool add_map = false;

  if (line_delta < 0
      || (line_delta > 10
	  && line_delta * ORDINARY_MAP_NUMBER_OF_COLUMN_BITS (map) > 1000)
      || (max_column_hint >= (1U << ORDINARY_MAP_NUMBER_OF_COLUMN_BITS (map)))
      || (max_column_hint <= 80
	  && ORDINARY_MAP_NUMBER_OF_COLUMN_BITS (map) >= 10))
    {
      add_map = true;
    }
  else
    max_column_hint = set->max_column_hint;
  if (add_map)
    {
      int column_bits;
      if (max_column_hint > 100000 || highest > 0xC0000000)
	{
	  /* If the column number is ridiculous or we've allocated a huge
	     number of source_locations, give up on column numbers. */
	  max_column_hint = 0;
	  if (highest >0xF0000000)
	    return 0;
	  column_bits = 0;
	}
      else
	{
	  column_bits = 7;
	  while (max_column_hint >= (1U << column_bits))
	    column_bits++;
	  max_column_hint = 1U << column_bits;
	}
      /* Allocate the new line_map.  However, if the current map only has a
	 single line we can sometimes just increase its column_bits instead. */
      if (line_delta < 0
	  || last_line != ORDINARY_MAP_STARTING_LINE_NUMBER (map)
	  || SOURCE_COLUMN (map, highest) >= (1U << column_bits))
	map = (struct line_map *) linemap_add (set, LC_RENAME,
					       ORDINARY_MAP_IN_SYSTEM_HEADER_P
					       (map),
					       ORDINARY_MAP_FILE_NAME (map),
					       to_line);
      ORDINARY_MAP_NUMBER_OF_COLUMN_BITS (map) = column_bits;
      r = (MAP_START_LOCATION (map)
	   + ((to_line - ORDINARY_MAP_STARTING_LINE_NUMBER (map))
	      << column_bits));
    }
  else
    r = highest - SOURCE_COLUMN (map, highest)
      + (line_delta << ORDINARY_MAP_NUMBER_OF_COLUMN_BITS (map));

  /* Locations of ordinary tokens are always lower than locations of
     macro tokens.  */
  if (r >= LINEMAPS_MACRO_LOWEST_LOCATION (set))
    return 0;

  set->highest_line = r;
  if (r > set->highest_location)
    set->highest_location = r;
  set->max_column_hint = max_column_hint;
  return r;
}

/* Given a virtual source location yielded by a map (either an
   ordinary or a macro map), returns that map.  */

const struct line_map*
linemap_lookup (struct line_maps *set, source_location line)
{
  if (linemap_location_from_macro_expansion_p (set, line))
    return linemap_macro_map_lookup (set, line);
  return linemap_ordinary_map_lookup (set, line);
}

/* Given a source location yielded by an ordinary map, returns that
   map.  Since the set is built chronologically, the logical lines are
   monotonic increasing, and so the list is sorted and we can use a
   binary search.  */

static const struct line_map *
linemap_ordinary_map_lookup (struct line_maps *set, source_location line)
{
  unsigned int md, mn, mx;
  const struct line_map *cached, *result;

  if (set ==  NULL || line < RESERVED_LOCATION_COUNT)
    return NULL;

  mn = LINEMAPS_ORDINARY_CACHE (set);
  mx = LINEMAPS_ORDINARY_USED (set);
  
  cached = LINEMAPS_ORDINARY_MAP_AT (set, mn);
  /* We should get a segfault if no line_maps have been added yet.  */
  if (line >= MAP_START_LOCATION (cached))
    {
      if (mn + 1 == mx || line < MAP_START_LOCATION (&cached[1]))
	return cached;
    }
  else
    {
      mx = mn;
      mn = 0;
    }

  while (mx - mn > 1)
    {
      md = (mn + mx) / 2;
      if (MAP_START_LOCATION (LINEMAPS_ORDINARY_MAP_AT (set, md)) > line)
	mx = md;
      else
	mn = md;
    }

  LINEMAPS_ORDINARY_CACHE (set) = mn;
  result = LINEMAPS_ORDINARY_MAP_AT (set, mn);
  linemap_assert (line >= MAP_START_LOCATION (result));
  return result;
}

/* Given a source location yielded by a macro map, returns that map.
   Since the set is built chronologically, the logical lines are
   monotonic decreasing, and so the list is sorted and we can use a
   binary search.  */

static const struct line_map*
linemap_macro_map_lookup (struct line_maps *set, source_location line)
{
  unsigned int md, mn, mx;
  const struct line_map *cached, *result;

  linemap_assert (line >= LINEMAPS_MACRO_LOWEST_LOCATION (set));

  if (set ==  NULL)
    return NULL;

  mn = LINEMAPS_MACRO_CACHE (set);
  mx = LINEMAPS_MACRO_USED (set);
  cached = LINEMAPS_MACRO_MAP_AT (set, mn);
  
  if (line >= MAP_START_LOCATION (cached))
    {
      if (mn == 0 || line < MAP_START_LOCATION (&cached[-1]))
	return cached;
      mx = mn - 1;
      mn = 0;
    }

  while (mn < mx)
    {
      md = (mx + mn) / 2;
      if (MAP_START_LOCATION (LINEMAPS_MACRO_MAP_AT (set, md)) > line)
	mn = md + 1;
      else
	mx = md;
    }

  LINEMAPS_MACRO_CACHE (set) = mx;
  result = LINEMAPS_MACRO_MAP_AT (set, LINEMAPS_MACRO_CACHE (set));
  linemap_assert (MAP_START_LOCATION (result) <= line);

  return result;
}

/* Return TRUE if MAP encodes locations coming from a macro
   replacement-list at macro expansion point.  */

bool
linemap_macro_expansion_map_p (const struct line_map *map)
{
  if (!map)
    return false;
  return (map->reason == LC_ENTER_MACRO);
}

/* If LOCATION is the locus of a token in a replacement-list of a
   macro expansion return the location of the macro expansion point.

   Read the comments of struct line_map and struct line_map_macro in
   line-map.h to understand what a macro expansion point is.  */

static source_location
linemap_macro_map_loc_to_exp_point (const struct line_map *map,
				    source_location location ATTRIBUTE_UNUSED)
{
  linemap_assert (linemap_macro_expansion_map_p (map)
		  && location >= MAP_START_LOCATION (map));

  /* Make sure LOCATION is correct.  */
  linemap_assert ((location - MAP_START_LOCATION (map))
		  <  MACRO_MAP_NUM_MACRO_TOKENS (map));

  return MACRO_MAP_EXPANSION_POINT_LOCATION (map);
}

/* If LOCATION is the source location of a token that belongs to a
   macro replacement-list -- as part of a macro expansion -- then
   return the location of the token at the definition point of the
   macro.  Otherwise, return LOCATION.  SET is the set of maps
   location come from.  ORIGINAL_MAP is an output parm. If non NULL,
   the function sets *ORIGINAL_MAP to the ordinary (non-macro) map the
   returned location comes from.  */

source_location
linemap_macro_map_loc_to_def_point (const struct line_map *map,
				    source_location location)
{
  unsigned token_no;

  linemap_assert (linemap_macro_expansion_map_p (map)
		  && location >= MAP_START_LOCATION (map));
  linemap_assert (location >= RESERVED_LOCATION_COUNT);

  token_no = location - MAP_START_LOCATION (map);
  linemap_assert (token_no < MACRO_MAP_NUM_MACRO_TOKENS (map));

  location = MACRO_MAP_LOCATIONS (map)[2 * token_no + 1];

  return location;
}

/* If LOCATION is the locus of a token that is an argument of a
   function-like macro M and appears in the expansion of M, return the
   locus of that argument in the context of the caller of M.

   In other words, this returns the xI location presented in the
   comments of line_map_macro above.  */
source_location
linemap_macro_map_loc_unwind_toward_spelling (const struct line_map* map,
					      source_location location)
{
  unsigned token_no;

  linemap_assert (linemap_macro_expansion_map_p (map)
		  && location >= MAP_START_LOCATION (map));
  linemap_assert (location >= RESERVED_LOCATION_COUNT);

  token_no = location - MAP_START_LOCATION (map);
  linemap_assert (token_no < MACRO_MAP_NUM_MACRO_TOKENS (map));

  location = MACRO_MAP_LOCATIONS (map)[2 * token_no];
  
  return location;
}

/* Return the name of the macro associated to MACRO_MAP.  */

const char*
linemap_map_get_macro_name (const struct line_map* macro_map)
{
  linemap_assert (macro_map && linemap_macro_expansion_map_p (macro_map));
  return (const char*) NODE_NAME (MACRO_MAP_MACRO (macro_map));
}

/* Return a positive value if LOCATION is the locus of a token that is
   located in a system header, O otherwise. It returns 1 if LOCATION
   is the locus of a token that is located in a system header, and 2
   if LOCATION is the locus of a token located in a C system header
   that therefore needs to be extern "C" protected in C++.

   Note that this function returns 1 if LOCATION belongs to a token
   that is part of a macro replacement-list defined in a system
   header, but expanded in a non-system file.  */

int
linemap_location_in_system_header_p (struct line_maps *set,
				     source_location location)
{
  const struct line_map *map = NULL;

  location =
    linemap_resolve_location (set, location, LRK_SPELLING_LOCATION, &map);

  if (location < RESERVED_LOCATION_COUNT)
    return false;

  return LINEMAP_SYSP (map);
}

/* Return TRUE if LOCATION is a source code location of a token coming
   from a macro replacement-list at a macro expansion point, FALSE
   otherwise.  */

bool
linemap_location_from_macro_expansion_p (struct line_maps *set,
					 source_location location)
{
  linemap_assert (location <= MAX_SOURCE_LOCATION
		  && (set->highest_location
		      < LINEMAPS_MACRO_LOWEST_LOCATION (set)));
  if (set == NULL)
    return false;
  return (location > set->highest_location);
}

/* Print an include trace, for e.g. the -H option of the preprocessor.  */

static void
trace_include (const struct line_maps *set, const struct line_map *map)
{
  unsigned int i = set->depth;

  while (--i)
    putc ('.', stderr);

  fprintf (stderr, " %s\n", ORDINARY_MAP_FILE_NAME (map));
}

/* Return the spelling location of the token wherever it comes from,
   whether part of a macro definition or not.

   This is a subroutine for linemap_resolve_location.  */

static source_location
linemap_macro_loc_to_spelling_point (struct line_maps *set,
				     source_location location,
				     const struct line_map **original_map)
{
  struct line_map *map;

  linemap_assert (set && location >= RESERVED_LOCATION_COUNT);

  while (true)
    {
      map = (struct line_map*) linemap_lookup (set, location);
      if (!linemap_macro_expansion_map_p (map))
	break;

      location =
	linemap_macro_map_loc_unwind_toward_spelling (map, location);
    }

  if (original_map)
    *original_map = map;
  return location;
}

/* If LOCATION is the source location of a token that belongs to a
   macro replacement-list -- as part of a macro expansion -- then
   return the location of the token at the definition point of the
   macro.  Otherwise, return LOCATION.  SET is the set of maps
   location come from.  ORIGINAL_MAP is an output parm. If non NULL,
   the function sets *ORIGINAL_MAP to the ordinary (non-macro) map the
   returned location comes from. 

   This is a subroutine of linemap_resolve_location.  */

static source_location
linemap_macro_loc_to_def_point (struct line_maps *set,
				source_location location,
				const struct line_map **original_map)
{
  struct line_map *map;

  linemap_assert (set && location >= RESERVED_LOCATION_COUNT);

  while (true)
    {
      map = (struct line_map*) linemap_lookup (set, location);
      if (!linemap_macro_expansion_map_p (map))
	break;

      location =
	linemap_macro_map_loc_to_def_point (map, location);
    }

  if (original_map)
    *original_map = map;
  return location;
}

/* If LOCATION is the source location of a token that belongs to a
   macro replacement-list -- at a macro expansion point -- then return
   the location of the topmost expansion point of the macro.  We say
   topmost because if we are in the context of a nested macro
   expansion, the function returns the source location of the first
   macro expansion that triggered the nested expansions.

   Otherwise, return LOCATION.  SET is the set of maps location come
   from.  ORIGINAL_MAP is an output parm. If non NULL, the function
   sets *ORIGINAL_MAP to the ordinary (non-macro) map the returned
   location comes from.

   This is a subroutine of linemap_resolve_location.  */

static source_location
linemap_macro_loc_to_exp_point (struct line_maps *set,
				source_location location,
				const struct line_map **original_map)
{
  struct line_map *map;

  linemap_assert (set && location >= RESERVED_LOCATION_COUNT);

  while (true)
    {
      map = (struct line_map*) linemap_lookup (set, location);
      if (!linemap_macro_expansion_map_p (map))
	break;
      location = linemap_macro_map_loc_to_exp_point (map, location);
    }

  if (original_map)
    *original_map = map;
  return location;
}

/* Resolve a virtual location into either a spelling location, an
   expansion point location or a token argument replacement point
   location.  Return the map that encodes the virtual location as well
   as the resolved location.

   If LOC is *NOT* the location of a token resulting from the
   expansion of a macro, then the parameter LRK (which stands for
   Location Resolution Kind) is ignored and the resulting location
   just equals the one given in argument.

   Now if LOC *IS* the location of a token resulting from the
   expansion of a macro, this is what happens.

   * If LRK is set to LRK_MACRO_EXPANSION_POINT
   -------------------------------

   The virtual location is resolved to the location to the locus of
   the expansion point of the macro.

   * If LRK is set to LRK_SPELLING_LOCATION
   -------------------------------------

   The virtual location is resolved to the location to the locus where
   the token has been spelled in the source. This can follow through
   all the macro expansions that led to the token.

   * If LRK is set to LRK_MACRO_PARM_REPLACEMENT_POINT
   --------------------------------------

   If LOC is the locus of a token that is an argument of a
   function-like macro [replacing a parameter in the replacement list
   of the macro] the virtual location is resolved to the locus of the
   parameter that is replaced, in the context of the definition of the
   macro.

   If LOC is the locus of a token that is not an argument of a
   function-like macro, then the function behaves as if LRK was set to
   LRK_SPELLING_LOCATION.

   If MAP is non-NULL, *MAP is set to the map of the resolved
   location.  Note that if the resturned location wasn't originally
   encoded by a map, the *MAP is set to NULL.  This can happen if LOC
   resolves to a location reserved for the client code, like
   UNKNOWN_LOCATION or BUILTINS_LOCATION in GCC.  */

source_location
linemap_resolve_location (struct line_maps *set,
			  source_location loc,
			  enum location_resolution_kind lrk,
			  const struct line_map **map)
{
  if (loc < RESERVED_LOCATION_COUNT)
    {
      /* A reserved location wasn't encoded in a map.  Let's return a
	 NULL map here, just like what linemap_ordinary_map_lookup
	 does.  */
      if (map)
	*map = NULL;
      return loc;
    }

  switch (lrk)
    {
    case LRK_MACRO_EXPANSION_POINT:
      loc = linemap_macro_loc_to_exp_point (set, loc, map);
      break;
    case LRK_SPELLING_LOCATION:
      loc = linemap_macro_loc_to_spelling_point (set, loc, map);
      break;
    case LRK_MACRO_DEFINITION_LOCATION:
      loc = linemap_macro_loc_to_def_point (set, loc, map);
      break;
    default:
      abort ();
    }
  return loc;
}

/* 
   Suppose that LOC is the virtual location of a token T coming from
   the expansion of a macro M.  This function then steps up to get the
   location L of the point where M got expanded.  If L is a spelling
   location inside a macro expansion M', then this function returns
   the locus of the point where M' was expanded.  Said otherwise, this
   function returns the location of T in the context that triggered
   the expansion of M. 

   *LOC_MAP must be set to the map of LOC.  This function then sets it
   to the map of the returned location.  */

source_location
linemap_unwind_toward_expansion (struct line_maps *set,
				 source_location loc,
				 const struct line_map **map)
{
  source_location resolved_location;
  const struct line_map *resolved_map;

  resolved_location =
    linemap_macro_map_loc_unwind_toward_spelling (*map, loc);
  resolved_map = linemap_lookup (set, resolved_location);

  if (!linemap_macro_expansion_map_p (resolved_map))
    {
      resolved_location = linemap_macro_map_loc_to_exp_point (*map, loc);
      resolved_map = linemap_lookup (set, resolved_location);
    }

  *map = resolved_map;
  return resolved_location;
}

/* Expand source code location LOC and return a user readable source
   code location.  LOC must be a spelling (non-virtual) location.  If
   it's a location < RESERVED_LOCATION_COUNT a zeroed expanded source
   location is returned.  */

expanded_location
linemap_expand_location (struct line_maps *set,
			 const struct line_map *map,
			 source_location loc)

{
  expanded_location xloc;

  memset (&xloc, 0, sizeof (xloc));

  if (loc < RESERVED_LOCATION_COUNT)
    /* The location for this token wasn't generated from a line map.
       It was probably a location for a builtin token, chosen by some
       client code.  Let's not try to expand the location in that
       case.  */;
  else if (map == NULL)
    /* We shouldn't be getting a NULL map with a location that is not
       reserved by the client code.  */
    abort ();
  else
    {
      /* MAP must be an ordinary map and LOC must be non-virtual,
	 encoded into this map, obviously; the accessors used on MAP
	 below ensure it is ordinary.  Let's just assert the
	 non-virtualness of LOC here.  */
      if (linemap_location_from_macro_expansion_p (set, loc))
	abort ();

      xloc.file = LINEMAP_FILE (map);
      xloc.line = SOURCE_LINE (map, loc);
      xloc.column = SOURCE_COLUMN (map, loc);
      xloc.sysp = LINEMAP_SYSP (map) != 0;
    }

  return xloc;
}


/* Dump line map at index IX in line table SET to STREAM.  If STREAM
   is NULL, use stderr.  IS_MACRO is true if the caller wants to
   dump a macro map, false otherwise.  */

void
linemap_dump (FILE *stream, struct line_maps *set, unsigned ix, bool is_macro)
{
  const char *lc_reasons_v[LC_ENTER_MACRO + 1]
      = { "LC_ENTER", "LC_LEAVE", "LC_RENAME", "LC_RENAME_VERBATIM",
	  "LC_ENTER_MACRO" };
  const char *reason;
  struct line_map *map;

  if (stream == NULL)
    stream = stderr;

  if (!is_macro)
    map = LINEMAPS_ORDINARY_MAP_AT (set, ix);
  else
    map = LINEMAPS_MACRO_MAP_AT (set, ix);

  reason = (map->reason <= LC_ENTER_MACRO) ? lc_reasons_v[map->reason] : "???";

  fprintf (stream, "Map #%u [%p] - LOC: %u - REASON: %s - SYSP: %s\n",
	   ix, (void *) map, map->start_location, reason,
	   (!is_macro && ORDINARY_MAP_IN_SYSTEM_HEADER_P (map)) ? "yes" : "no");
  if (!is_macro)
    {
      unsigned includer_ix;
      struct line_map *includer_map;

      includer_ix = ORDINARY_MAP_INCLUDER_FILE_INDEX (map);
      includer_map = includer_ix < LINEMAPS_ORDINARY_USED (set)
		     ? LINEMAPS_ORDINARY_MAP_AT (set, includer_ix)
		     : NULL;

      fprintf (stream, "File: %s:%d\n", ORDINARY_MAP_FILE_NAME (map),
	       ORDINARY_MAP_STARTING_LINE_NUMBER (map));
      fprintf (stream, "Included from: [%d] %s\n", includer_ix,
	       includer_map ? ORDINARY_MAP_FILE_NAME (includer_map) : "None");
    }
  else
    fprintf (stream, "Macro: %s (%u tokens)\n",
	     linemap_map_get_macro_name (map),
	     MACRO_MAP_NUM_MACRO_TOKENS (map));

  fprintf (stream, "\n");
}


/* Dump debugging information about source location LOC into the file
   stream STREAM. SET is the line map set LOC comes from.  */

void
linemap_dump_location (struct line_maps *set,
		       source_location loc,
		       FILE *stream)
{
  const struct line_map *map;
  source_location location;
  const char *path = "", *from = "";
  int l = -1, c = -1, s = -1, e = -1;

  if (loc == 0)
    return;

  location =
    linemap_resolve_location (set, loc, LRK_MACRO_DEFINITION_LOCATION, &map);

  if (map == NULL)
    /* Only reserved locations can be tolerated in this case.  */
    linemap_assert (location < RESERVED_LOCATION_COUNT);
  else
    {
      path = LINEMAP_FILE (map);
      l = SOURCE_LINE (map, location);
      c = SOURCE_COLUMN (map, location);
      s = LINEMAP_SYSP (map) != 0;
      e = location != loc;
      if (e)
	from = "N/A";
      else
	from = (INCLUDED_FROM (set, map))
	  ? LINEMAP_FILE (INCLUDED_FROM (set, map))
	  : "<NULL>";
    }

  /* P: path, L: line, C: column, S: in-system-header, M: map address,
     E: macro expansion?, LOC: original location, R: resolved location   */
  fprintf (stream, "{P:%s;F:%s;L:%d;C:%d;S:%d;M:%p;E:%d,LOC:%d,R:%d}",
	   path, from, l, c, s, (void*)map, e, loc, location);
}
