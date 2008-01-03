/* Modula-3 language support definitions for GDB, the GNU debugger.
   Copyright 2006 Free Software Foundation, Inc.

This file is part of GDB.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

  /* This file contains debugging support for Modula-3 threads. */ 

#include "defs.h"
#include "gdbtypes.h"
#include "frame.h" 
#include "regcache.h" 
#include "target.h"

#include "m3-threads.h" 
#include "m3-util.h" 
#include "m3-valprint.h" 

#define eval(x) evaluate_expression (parse_expression (x))

static struct type *thread__t = 0;
static int thread__t__id_size, thread__t__id_offset;
static int thread__t__state_size, thread__t__state_offset;
static int thread__t__next_size, thread__t__next_offset;
static int thread__t__cond_size, thread__t__cond_offset;
static struct type * thread__t__cond_type;
static int thread__t__mutex_size, thread__t__mutex_offset;
static struct type * thread__t__mutex_type;
static int thread__t__time_size, thread__t__time_offset;
static struct type * thread__t__time_type;
static int thread__t__context_size, thread__t__context_offset;
static struct type * thread__t__context_type;
static int thread__t__buf_size, thread__t__buf_offset;
static struct type * thread__t__buf_type;

static void 
ensure_init_thread_constants ( void )
{
  int thread__t__context_size, thread__t__context_offset;
  struct type * thread__t__context_type;
  int dataOffset = TARGET_PTR_BIT;

  if (thread__t) return;

  m3_unit_name_globals_symbol ('I', "Thread", NULL );
  m3_unit_name_globals_symbol ('M', "ThreadPosix", NULL );

  thread__t = find_m3_type_named ("Thread.T",1);

  m3_find_rec_field (thread__t, "id", 
		     &thread__t__id_size, &thread__t__id_offset, 0);
  m3_find_rec_field (thread__t, "state", 
		     &thread__t__state_size, &thread__t__state_offset, 0);
  m3_find_rec_field (thread__t, "next", 
		     &thread__t__next_size, &thread__t__next_offset, 0);
  m3_find_rec_field (thread__t, "waitingForCondition", 
		     &thread__t__cond_size, &thread__t__cond_offset, 
		     &thread__t__cond_type);
  m3_find_rec_field (thread__t, "waitingForMutex", 
		     &thread__t__mutex_size, &thread__t__mutex_offset, 
		     &thread__t__mutex_type);
  m3_find_rec_field (thread__t, "waitingForTime", 
		     &thread__t__time_size, &thread__t__time_offset, 
		     &thread__t__time_type);
  m3_find_rec_field (thread__t, "context",
		     &thread__t__context_size, &thread__t__context_offset,
		     &thread__t__context_type);
  m3_find_rec_field (thread__t__context_type, "buf",
		     &thread__t__buf_size, &thread__t__buf_offset, 0);

  /* skip past the method pointer */
#if defined(sparc)
  /* deal with sparc realignment */
  dataOffset += TARGET_PTR_BIT;
#endif
  thread__t__id_offset    += dataOffset;
  thread__t__state_offset += dataOffset;
  thread__t__next_offset  += dataOffset;
  thread__t__cond_offset  += dataOffset;
  thread__t__mutex_offset += dataOffset;
  thread__t__time_offset  += dataOffset;
  thread__t__buf_offset   += dataOffset + thread__t__context_offset;
} /* ensure_init_thread_constants */ 

/*--------------------------------------------------------- jmpbuf layout ---*/

#if defined(mips)
/* see config/mips/tm-mips.h/REGISTER_NAMES */
#define HAVE_REGISTER_MAP
static int regno_to_jmpbuf [] = {
   3,  4,  5,  6,  7,  8,  9, 10,
  11, 12, 13, 14, 15, 16, 17, 18,
  19, 20, 21, 22, 23, 24, 25, 26,
  27, 28, 29, 30, 31, 32, 33, 34,
   3,  3,  3,  3,  3,  2,
  38, 39, 40, 41, 42, 43, 44, 45,
  46, 47, 48, 49, 50, 51, 52, 53,
  54, 55, 56, 57, 58, 59, 60, 61,
  62, 63, 64, 65, 66, 67, 68, 69,
   3,  3,  3,  3,  3,  3,  3,  3,
   3,  3};
#endif

#if defined(__alpha)
/* see config/alpha/tm-alpha.h/REGISTER_NAMES */
#define HAVE_REGISTER_MAP
static int regno_to_jmpbuf [] = {
   4,  5,  6,  7,  8,  9, 10, 11,
  12, 13, 14, 15, 16, 17, 18, 19,
  20, 21, 22, 23, 24, 25, 26, 27,
  28, 29, 30, 31, 32, 33, 34, 35,
  37, 38, 39, 40, 41, 42, 43, 44,
  45, 46, 47, 48, 49, 50, 51, 52,
  53, 54, 55, 56, 57, 58, 59, 60,
  61, 62, 63, 64, 65, 66, 67, 68,
   2, 35};
#endif

#if defined(linux) && defined(i386)

/* This tells where each register in array REGISTER_NAMES in 
   config/i386/tm-i386.h is found in a jump_buf <jmp_buf.h>.
   Registers which dont appear are set to 0? The fourth register
   below is ebx and happens to be the first entry (0) in jmp_buf. */

#define HAVE_REGISTER_MAP
static int regno_to_jmpbuf [] = {
  0 /* eax */,   0 /* ecx */,    0 /* edx */,   0 /* ebx */, \
  4 /* esp */,   3 /* ebp */,    1 /* esi */,   2 /* edi */, \
  5 /* eip */,   0 /* eflags */, 0 /* cs */,    0 /* ss */, \
  0 /* ds */,    0 /* es */,     0 /* fs */,    0 /* gs */, \
  0 /* st0 */,   0 /* st1 */,    0 /* st2 */,   0 /* st3 */, \
  0 /* st4 */,   0 /* st5 */,    0 /* st6 */,   0 /* st7 */, \
};
#endif

/*---------------------------------------------------- thread enumeration ---*/

struct m3_thread {
  LONGEST ref;   /* the Thread.T value */
  int     id;    /* the thread's internal ID */
  char   *bits;  /* the pointer to the Thread.T's data fields */
} ;

static void
get_m3_thread ( LONGEST ref, struct m3_thread  * t ) 
{
    CORE_ADDR tmpref = ref;

    /* in case we get stuck */
    t->ref   = ref;
    t->id    = 0;
    t->bits  = 0;

    if (!ref) return;

    t->bits = m3_read_object_fields_bits (tmpref);
    if (!t->bits) return;

    t->id = m3_extract_ord (t->bits, thread__t__id_offset,thread__t__id_size,0);
} /* get_m3_thread */ 

static void
first_m3_thread ( struct m3_thread * t )
{
    struct value * v = eval ("ThreadPosix.self");
    LONGEST ref = m3_extract_address (value_contents (v), 0);
    get_m3_thread (ref, t);
} /* first_m3_thread */ 

static void 
next_m3_thread ( struct m3_thread *t ) 
{
    LONGEST ref;

    if (!t) return;
    if (!(t->bits)) return;
    ref = m3_extract_address (t->bits, thread__t__next_offset);
    get_m3_thread (ref, t);
} /* next_m3_thread */ 

/*---------------------------------------------------------------- switch ---*/

/* the "current" thread for the user */
static struct m3_thread cur_m3_thread = { 0, 0, 0 };

static void
look_in_thread ( int regno ) 
{
#ifdef HAVE_REGISTER_MAP
  int reg_offset;

  if (cur_m3_thread.ref == 0) { first_m3_thread (&cur_m3_thread); }

  if (cur_m3_thread.bits) {
    int first_reg = regno;
    int last_reg  = regno+1;
    if (regno < 0) { first_reg = 0; last_reg = NUM_REGS; }
    for (regno = first_reg; regno < last_reg; regno++) {
      reg_offset = thread__t__buf_offset / HOST_CHAR_BIT
	+ regno_to_jmpbuf [regno] * TARGET_PTR_BIT / HOST_CHAR_BIT;
      regcache_raw_supply 
        (current_regcache, regno, cur_m3_thread.bits + reg_offset);
    }
  }
#else
  if ((regno >= 0) && (regno < NUM_REGS)) {
    error ( "%s, line %d: don't know where to find register \"%s\" "
            "in stopped thread",
	    __FILE__, __LINE__, REGISTER_NAME ( regno )
          );
  } else {
    error ("%s, line %d: don't know where to find registers in stopped thread",
	   __FILE__, __LINE__);
  }
#endif
} /* look_in_thread */ 

void
switch_command ( char *args, int from_tty ) 
{
  static void (*saved_to_fetch_registers) PARAMS ((int)) = 0;
  struct m3_thread first, cur;
  int  target_id;

  ensure_init_thread_constants ( );

  if (!args) { error ("You must specify a thread id to switch to."); }
  sscanf (args, "%d", &target_id);

  first_m3_thread (&first);

  /* scan for a match */
  cur = first;
  while ((cur.bits) && (cur.id != target_id)) {
    next_m3_thread (&cur);
  }

  if (!cur.bits) {
    error ("Unable to find thread with id = %d", target_id);
    return; /* forget it */
  }

  /* record the thread for "look_in_thread"'s use */
  cur_m3_thread = cur;

  /* update the register locating function */
  if (cur.id == first.id) {
    /* we're looking at the primary thread that gdb knows about */
    if (current_target.to_fetch_registers == look_in_thread) {
      current_target.to_fetch_registers = saved_to_fetch_registers;
    }
  } else {
    /* we're looking at a secondary thread */
    if (current_target.to_fetch_registers != look_in_thread) {
      saved_to_fetch_registers = current_target.to_fetch_registers;
      current_target.to_fetch_registers = look_in_thread;
    }
  }

  registers_changed ( );
  reinit_frame_cache ( );
} /* switch_command */ 

void
threads_command ( char *args, int from_tty ) 
{
  int first_id, state;
  struct m3_thread cur;

  ensure_init_thread_constants ( );

  first_m3_thread (&cur);
  first_id = cur.id;

  printf_filtered ("-id-   -Thread.T-  -state-\n");
  while (cur.bits) {
    printf_filtered ("%4d  ", cur.id);
    print_longest (gdb_stdout, 'x', 1, cur.ref);
    printf_filtered ("  ");

    state = m3_extract_ord (cur.bits, thread__t__state_offset,
			   thread__t__state_size, 0);
    switch (state) {
      case 0 /* alive */:
	printf_filtered ("alive");
	break; 
      case 1 /* waiting */:
	printf_filtered ("waiting for condition 16_%lx",
		  m3_extract_address (cur.bits, thread__t__cond_offset));
	break;
      case 2 /* locking */:
	printf_filtered ("waiting for mutex 16_%lx",
		  m3_extract_address (cur.bits, thread__t__mutex_offset));
	break;
      case 3 /* pausing */:
	printf_filtered ("waiting until ");
	m3_val_print2 (thread__t__time_type, cur.bits, thread__t__time_offset, 
		       thread__t__time_size, gdb_stdout, 0, 0, 1);
	break;
      case 4 /* blocking */:
	printf_filtered ("waiting for I/O");
	break;
      case 5 /* dying */:
	printf_filtered ("waiting for somebody to join");
	break;
      case 6 /* dead */:
	printf_filtered ("dead");
	break;
      default:
	printf_filtered ("<unknown state = %d>", state);
	break;
      }
    puts_filtered ("\n");

    /* advance to the next thread */
    next_m3_thread (&cur);
    if (cur.id == first_id) break;
  }
} /* threads_command */ 

/* End of file m3-threads.c */ 
