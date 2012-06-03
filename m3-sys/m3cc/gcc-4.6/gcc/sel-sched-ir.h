/* Modula-3: modified */

/* Instruction scheduling pass.  This file contains definitions used
   internally in the scheduler.
   Copyright (C) 2006, 2007, 2008, 2009, 2010 Free Software Foundation, Inc.

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

#ifndef GCC_SEL_SCHED_IR_H
#define GCC_SEL_SCHED_IR_H

/* For state_t.  */
#include "insn-attr.h"
#include "regset.h"
#include "basic-block.h"
/* For reg_note.  */
#include "rtl.h"
#include "ggc.h"
#include "bitmap.h"
#include "vecprim.h"
#include "sched-int.h"
#include "cfgloop.h"

EXTERN_C_START

/* tc_t is a short for target context.  This is a state of the target
   backend.  */
typedef void *tc_t;

/* List data types used for av sets, fences, paths, and boundaries.  */

/* Forward declarations for types that are part of some list nodes.  */
struct _list_node;

/* List backend.  */
typedef struct _list_node *_list_t;
#define _LIST_NEXT(L) ((L)->next)

/* Instruction data that is part of vinsn type.  */
struct idata_def;
typedef struct idata_def *idata_t;

/* A virtual instruction, i.e. an instruction as seen by the scheduler.  */
struct vinsn_def;
typedef struct vinsn_def *vinsn_t;

/* RTX list.
   This type is the backend for ilist.  */
typedef _list_t _xlist_t;
#define _XLIST_X(L) ((L)->u.x)
#define _XLIST_NEXT(L) (_LIST_NEXT (L))

/* Instruction.  */
typedef rtx insn_t;

/* List of insns.  */
typedef _xlist_t ilist_t;
#define ILIST_INSN(L) (_XLIST_X (L))
#define ILIST_NEXT(L) (_XLIST_NEXT (L))

/* This lists possible transformations that done locally, i.e. in
   moveup_expr.  */
enum local_trans_type
  {
    TRANS_SUBSTITUTION,
    TRANS_SPECULATION
  };

/* This struct is used to record the history of expression's
   transformations.  */
struct expr_history_def_1
{
  /* UID of the insn.  */
  unsigned uid;

  /* How the expression looked like.  */
  vinsn_t old_expr_vinsn;

  /* How the expression looks after the transformation.  */
  vinsn_t new_expr_vinsn;

  /* And its speculative status.  */
  ds_t spec_ds;

  /* Type of the transformation.  */
  enum local_trans_type type;
};

typedef struct expr_history_def_1 expr_history_def;

DEF_VEC_O (expr_history_def);
DEF_VEC_ALLOC_O (expr_history_def, heap);

/* Expression information.  */
struct _expr
{
  /* Insn description.  */
  vinsn_t vinsn;

  /* SPEC is the degree of speculativeness.
     FIXME: now spec is increased when an rhs is moved through a
     conditional, thus showing only control speculativeness.  In the
     future we'd like to count data spec separately to allow a better
     control on scheduling.  */
  int spec;

  /* Degree of speculativeness measured as probability of executing
     instruction's original basic block given relative to
     the current scheduling point.  */
  int usefulness;

  /* A priority of this expression.  */
  int priority;

  /* A priority adjustment of this expression.  */
  int priority_adj;

  /* Number of times the insn was scheduled.  */
  int sched_times;

  /* A basic block index this was originated from.  Zero when there is
     more than one originator.  */
  int orig_bb_index;

  /* Instruction should be of SPEC_DONE_DS type in order to be moved to this
     point.  */
  ds_t spec_done_ds;

  /* SPEC_TO_CHECK_DS hold speculation types that should be checked
     (used only during move_op ()).  */
  ds_t spec_to_check_ds;

  /* Cycle on which original insn was scheduled.  Zero when it has not yet
     been scheduled or more than one originator.  */
  int orig_sched_cycle;

  /* This vector contains the history of insn's transformations.  */
  VEC(expr_history_def, heap) *history_of_changes;

  /* True (1) when original target (register or memory) of this instruction
     is available for scheduling, false otherwise.  -1 means we're not sure;
     please run find_used_regs to clarify.  */
  signed char target_available;

  /* True when this expression needs a speculation check to be scheduled.
     This is used during find_used_regs.  */
  BOOL_BITFIELD needs_spec_check_p : 1;

  /* True when the expression was substituted.  Used for statistical
     purposes.  */
  BOOL_BITFIELD was_substituted : 1;

  /* True when the expression was renamed.  */
  BOOL_BITFIELD was_renamed : 1;

  /* True when expression can't be moved.  */
  BOOL_BITFIELD cant_move : 1;
};

typedef struct _expr expr_def;
typedef expr_def *expr_t;

#define EXPR_VINSN(EXPR) ((EXPR)->vinsn)
#define EXPR_INSN_RTX(EXPR) (VINSN_INSN_RTX (EXPR_VINSN (EXPR)))
#define EXPR_PATTERN(EXPR) (VINSN_PATTERN (EXPR_VINSN (EXPR)))
#define EXPR_LHS(EXPR) (VINSN_LHS (EXPR_VINSN (EXPR)))
#define EXPR_RHS(EXPR) (VINSN_RHS (EXPR_VINSN (EXPR)))
#define EXPR_TYPE(EXPR) (VINSN_TYPE (EXPR_VINSN (EXPR)))
#define EXPR_SEPARABLE_P(EXPR) (VINSN_SEPARABLE_P (EXPR_VINSN (EXPR)))

#define EXPR_SPEC(EXPR) ((EXPR)->spec)
#define EXPR_USEFULNESS(EXPR) ((EXPR)->usefulness)
#define EXPR_PRIORITY(EXPR) ((EXPR)->priority)
#define EXPR_PRIORITY_ADJ(EXPR) ((EXPR)->priority_adj)
#define EXPR_SCHED_TIMES(EXPR) ((EXPR)->sched_times)
#define EXPR_ORIG_BB_INDEX(EXPR) ((EXPR)->orig_bb_index)
#define EXPR_ORIG_SCHED_CYCLE(EXPR) ((EXPR)->orig_sched_cycle)
#define EXPR_SPEC_DONE_DS(EXPR) ((EXPR)->spec_done_ds)
#define EXPR_SPEC_TO_CHECK_DS(EXPR) ((EXPR)->spec_to_check_ds)
#define EXPR_HISTORY_OF_CHANGES(EXPR) ((EXPR)->history_of_changes)
#define EXPR_TARGET_AVAILABLE(EXPR) ((EXPR)->target_available)
#define EXPR_NEEDS_SPEC_CHECK_P(EXPR) ((EXPR)->needs_spec_check_p)
#define EXPR_WAS_SUBSTITUTED(EXPR) ((EXPR)->was_substituted)
#define EXPR_WAS_RENAMED(EXPR) ((EXPR)->was_renamed)
#define EXPR_CANT_MOVE(EXPR) ((EXPR)->cant_move)

#define EXPR_WAS_CHANGED(EXPR) (VEC_length (expr_history_def, \
                                            EXPR_HISTORY_OF_CHANGES (EXPR)) > 0)

/* Insn definition for list of original insns in find_used_regs.  */
struct _def
{
  insn_t orig_insn;

  /* FIXME: Get rid of CROSSES_CALL in each def, since if we're moving up
     rhs from two different places, but only one of the code motion paths
     crosses a call, we can't use any of the call_used_regs, no matter which
     path or whether all paths crosses a call.  Thus we should move CROSSES_CALL
     to static params.  */
  bool crosses_call;
};
typedef struct _def *def_t;


/* Availability sets are sets of expressions we're scheduling.  */
typedef _list_t av_set_t;
#define _AV_SET_EXPR(L) (&(L)->u.expr)
#define _AV_SET_NEXT(L) (_LIST_NEXT (L))


/* Boundary of the current fence group.  */
struct _bnd
{
  /* The actual boundary instruction.  */
  insn_t to;

  /* Its path to the fence.  */
  ilist_t ptr;

  /* Availability set at the boundary.  */
  av_set_t av;

  /* This set moved to the fence.  */
  av_set_t av1;

  /* Deps context at this boundary.  As long as we have one boundary per fence,
     this is just a pointer to the same deps context as in the corresponding
     fence.  */
  deps_t dc;
};
typedef struct _bnd *bnd_t;
#define BND_TO(B) ((B)->to)

/* PTR stands not for pointer as you might think, but as a Path To Root of the
   current instruction group from boundary B.  */
#define BND_PTR(B) ((B)->ptr)
#define BND_AV(B) ((B)->av)
#define BND_AV1(B) ((B)->av1)
#define BND_DC(B) ((B)->dc)

/* List of boundaries.  */
typedef _list_t blist_t;
#define BLIST_BND(L) (&(L)->u.bnd)
#define BLIST_NEXT(L) (_LIST_NEXT (L))


/* Fence information.  A fence represents current scheduling point and also
   blocks code motion through it when pipelining.  */
struct _fence
{
  /* Insn before which we gather an instruction group.*/
  insn_t insn;

  /* Modeled state of the processor pipeline.  */
  state_t state;

  /* Current cycle that is being scheduled on this fence.  */
  int cycle;

  /* Number of insns that were scheduled on the current cycle.
     This information has to be local to a fence.  */
  int cycle_issued_insns;

  /* At the end of fill_insns () this field holds the list of the instructions
     that are inner boundaries of the scheduled parallel group.  */
  ilist_t bnds;

  /* Deps context at this fence.  It is used to model dependencies at the
     fence so that insn ticks can be properly evaluated.  */
  deps_t dc;

  /* Target context at this fence.  Used to save and load any local target
     scheduling information when changing fences.  */
  tc_t tc;

  /* A vector of insns that are scheduled but not yet completed.  */
  VEC (rtx,gc) *executing_insns;

  /* A vector indexed by UIDs that caches the earliest cycle on which
     an insn can be scheduled on this fence.  */
  int *ready_ticks;

  /* Its size.  */
  int ready_ticks_size;

  /* Insn, which has been scheduled last on this fence.  */
  rtx last_scheduled_insn;

  /* The last value of can_issue_more variable on this fence.  */
  int issue_more;

  /* If non-NULL force the next scheduled insn to be SCHED_NEXT.  */
  rtx sched_next;

  /* True if fill_insns processed this fence.  */
  BOOL_BITFIELD processed_p : 1;

  /* True if fill_insns actually scheduled something on this fence.  */
  BOOL_BITFIELD scheduled_p : 1;

  /* True when the next insn scheduled here would start a cycle.  */
  BOOL_BITFIELD starts_cycle_p : 1;

  /* True when the next insn scheduled here would be scheduled after a stall.  */
  BOOL_BITFIELD after_stall_p : 1;
};
typedef struct _fence *fence_t;

#define FENCE_INSN(F) ((F)->insn)
#define FENCE_STATE(F) ((F)->state)
#define FENCE_BNDS(F) ((F)->bnds)
#define FENCE_PROCESSED_P(F) ((F)->processed_p)
#define FENCE_SCHEDULED_P(F) ((F)->scheduled_p)
#define FENCE_ISSUED_INSNS(F) ((F)->cycle_issued_insns)
#define FENCE_CYCLE(F) ((F)->cycle)
#define FENCE_STARTS_CYCLE_P(F) ((F)->starts_cycle_p)
#define FENCE_AFTER_STALL_P(F) ((F)->after_stall_p)
#define FENCE_DC(F) ((F)->dc)
#define FENCE_TC(F) ((F)->tc)
#define FENCE_LAST_SCHEDULED_INSN(F) ((F)->last_scheduled_insn)
#define FENCE_ISSUE_MORE(F) ((F)->issue_more)
#define FENCE_EXECUTING_INSNS(F) ((F)->executing_insns)
#define FENCE_READY_TICKS(F) ((F)->ready_ticks)
#define FENCE_READY_TICKS_SIZE(F) ((F)->ready_ticks_size)
#define FENCE_SCHED_NEXT(F) ((F)->sched_next)

/* List of fences.  */
typedef _list_t flist_t;
#define FLIST_FENCE(L) (&(L)->u.fence)
#define FLIST_NEXT(L) (_LIST_NEXT (L))

/* List of fences with pointer to the tail node.  */
struct flist_tail_def
{
  flist_t head;
  flist_t *tailp;
};

typedef struct flist_tail_def *flist_tail_t;
#define FLIST_TAIL_HEAD(L) ((L)->head)
#define FLIST_TAIL_TAILP(L) ((L)->tailp)

/* List node information.  A list node can be any of the types above.  */
struct _list_node
{
  _list_t next;

  union
  {
    rtx x;
    struct _bnd bnd;
    expr_def expr;
    struct _fence fence;
    struct _def def;
    void *data;
  } u;
};


/* _list_t functions.
   All of _*list_* functions are used through accessor macros, thus
   we can't move them in sel-sched-ir.c.  */
extern alloc_pool sched_lists_pool;

static inline _list_t
_list_alloc (void)
{
  return (_list_t) pool_alloc (sched_lists_pool);
}

static inline void
_list_add (_list_t *lp)
{
  _list_t l = _list_alloc ();

  _LIST_NEXT (l) = *lp;
  *lp = l;
}

static inline void
_list_remove_nofree (_list_t *lp)
{
  _list_t n = *lp;

  *lp = _LIST_NEXT (n);
}

static inline void
_list_remove (_list_t *lp)
{
  _list_t n = *lp;

  *lp = _LIST_NEXT (n);
  pool_free (sched_lists_pool, n);
}

static inline void
_list_clear (_list_t *l)
{
  while (*l)
    _list_remove (l);
}


/* List iterator backend.  */
typedef struct
{
  /* The list we're iterating.  */
  _list_t *lp;

  /* True when this iterator supprts removing.  */
  bool can_remove_p;

  /* True when we've actually removed something.  */
  bool removed_p;
} _list_iterator;

static inline void
_list_iter_start (_list_iterator *ip, _list_t *lp, bool can_remove_p)
{
  ip->lp = lp;
  ip->can_remove_p = can_remove_p;
  ip->removed_p = false;
}

static inline void
_list_iter_next (_list_iterator *ip)
{
  if (!ip->removed_p)
    ip->lp = &_LIST_NEXT (*ip->lp);
  else
    ip->removed_p = false;
}

static inline void
_list_iter_remove (_list_iterator *ip)
{
  gcc_assert (!ip->removed_p && ip->can_remove_p);
  _list_remove (ip->lp);
  ip->removed_p = true;
}

static inline void
_list_iter_remove_nofree (_list_iterator *ip)
{
  gcc_assert (!ip->removed_p && ip->can_remove_p);
  _list_remove_nofree (ip->lp);
  ip->removed_p = true;
}

/* General macros to traverse a list.  FOR_EACH_* interfaces are
   implemented using these.  */
#define _FOR_EACH(TYPE, ELEM, I, L)				\
  for (_list_iter_start (&(I), &(L), false);			\
       _list_iter_cond_##TYPE (*(I).lp, &(ELEM));		\
       _list_iter_next (&(I)))

#define _FOR_EACH_1(TYPE, ELEM, I, LP)                              \
  for (_list_iter_start (&(I), (LP), true);                         \
       _list_iter_cond_##TYPE (*(I).lp, &(ELEM));                   \
       _list_iter_next (&(I)))


/* _xlist_t functions.  */

static inline void
_xlist_add (_xlist_t *lp, rtx x)
{
  _list_add (lp);
  _XLIST_X (*lp) = x;
}

#define _xlist_remove(LP) (_list_remove (LP))
#define _xlist_clear(LP) (_list_clear (LP))

static inline bool
_xlist_is_in_p (_xlist_t l, rtx x)
{
  while (l)
    {
      if (_XLIST_X (l) == x)
        return true;
      l = _XLIST_NEXT (l);
    }

  return false;
}

/* Used through _FOR_EACH.  */
static inline bool
_list_iter_cond_x (_xlist_t l, rtx *xp)
{
  if (l)
    {
      *xp = _XLIST_X (l);
      return true;
    }

  return false;
}

#define _xlist_iter_remove(IP) (_list_iter_remove (IP))

typedef _list_iterator _xlist_iterator;
#define _FOR_EACH_X(X, I, L) _FOR_EACH (x, (X), (I), (L))
#define _FOR_EACH_X_1(X, I, LP) _FOR_EACH_1 (x, (X), (I), (LP))


/* ilist_t functions.  Instruction lists are simply RTX lists.  */

#define ilist_add(LP, INSN) (_xlist_add ((LP), (INSN)))
#define ilist_remove(LP) (_xlist_remove (LP))
#define ilist_clear(LP) (_xlist_clear (LP))
#define ilist_is_in_p(L, INSN) (_xlist_is_in_p ((L), (INSN)))
#define ilist_iter_remove(IP) (_xlist_iter_remove (IP))

typedef _xlist_iterator ilist_iterator;
#define FOR_EACH_INSN(INSN, I, L) _FOR_EACH_X (INSN, I, L)
#define FOR_EACH_INSN_1(INSN, I, LP) _FOR_EACH_X_1 (INSN, I, LP)


/* Av set iterators.  */
typedef _list_iterator av_set_iterator;
#define FOR_EACH_EXPR(EXPR, I, AV) _FOR_EACH (expr, (EXPR), (I), (AV))
#define FOR_EACH_EXPR_1(EXPR, I, AV) _FOR_EACH_1 (expr, (EXPR), (I), (AV))

static bool
_list_iter_cond_expr (av_set_t av, expr_t *exprp)
{
  if (av)
    {
      *exprp = _AV_SET_EXPR (av);
      return true;
    }

  return false;
}


/* Def list iterators.  */
typedef _list_t def_list_t;
typedef _list_iterator def_list_iterator;

#define DEF_LIST_NEXT(L) (_LIST_NEXT (L))
#define DEF_LIST_DEF(L) (&(L)->u.def)

#define FOR_EACH_DEF(DEF, I, DEF_LIST) _FOR_EACH (def, (DEF), (I), (DEF_LIST))

static inline bool
_list_iter_cond_def (def_list_t def_list, def_t *def)
{
  if (def_list)
    {
      *def = DEF_LIST_DEF (def_list);
      return true;
    }

  return false;
}


/* InstructionData.  Contains information about insn pattern.  */
struct idata_def
{
  /* Type of the insn.
     o CALL_INSN - Call insn
     o JUMP_INSN - Jump insn
     o INSN - INSN that cannot be cloned
     o USE - INSN that can be cloned
     o SET - INSN that can be cloned and separable into lhs and rhs
     o PC - simplejump.  Insns that simply redirect control flow should not
     have any dependencies.  Sched-deps.c, though, might consider them as
     producers or consumers of certain registers.  To avoid that we handle
     dependency for simple jumps ourselves.  */
  int type;

  /* If insn is a SET, this is its left hand side.  */
  rtx lhs;

  /* If insn is a SET, this is its right hand side.  */
  rtx rhs;

  /* Registers that are set/used by this insn.  This info is now gathered
     via sched-deps.c.  The downside of this is that we also use live info
     from flow that is accumulated in the basic blocks.  These two infos
     can be slightly inconsistent, hence in the beginning we make a pass
     through CFG and calculating the conservative solution for the info in
     basic blocks.  When this scheduler will be switched to use dataflow,
     this can be unified as df gives us both per basic block and per
     instruction info.  Actually, we don't do that pass and just hope
     for the best.  */
  regset reg_sets;

  regset reg_clobbers;

  regset reg_uses;
};

#define IDATA_TYPE(ID) ((ID)->type)
#define IDATA_LHS(ID) ((ID)->lhs)
#define IDATA_RHS(ID) ((ID)->rhs)
#define IDATA_REG_SETS(ID) ((ID)->reg_sets)
#define IDATA_REG_USES(ID) ((ID)->reg_uses)
#define IDATA_REG_CLOBBERS(ID) ((ID)->reg_clobbers)

/* Type to represent all needed info to emit an insn.
   This is a virtual equivalent of the insn.
   Every insn in the stream has an associated vinsn.  This is used
   to reduce memory consumption basing on the fact that many insns
   don't change through the scheduler.

   vinsn can be either normal or unique.
   * Normal vinsn is the one, that can be cloned multiple times and typically
   corresponds to normal instruction.

   * Unique vinsn derivates from CALL, ASM, JUMP (for a while) and other
   unusual stuff.  Such a vinsn is described by its INSN field, which is a
   reference to the original instruction.  */
struct vinsn_def
{
  /* Associated insn.  */
  rtx insn_rtx;

  /* Its description.  */
  struct idata_def id;

  /* Hash of vinsn.  It is computed either from pattern or from rhs using
     hash_rtx.  It is not placed in ID for faster compares.  */
  unsigned hash;

  /* Hash of the insn_rtx pattern.  */
  unsigned hash_rtx;

  /* Smart pointer counter.  */
  int count;

  /* Cached cost of the vinsn.  To access it please use vinsn_cost ().  */
  int cost;

  /* Mark insns that may trap so we don't move them through jumps.  */
  bool may_trap_p;
};

#define VINSN_INSN_RTX(VI) ((VI)->insn_rtx)
#define VINSN_PATTERN(VI) (PATTERN (VINSN_INSN_RTX (VI)))

#define VINSN_ID(VI) (&((VI)->id))
#define VINSN_HASH(VI) ((VI)->hash)
#define VINSN_HASH_RTX(VI) ((VI)->hash_rtx)
#define VINSN_TYPE(VI) (IDATA_TYPE (VINSN_ID (VI)))
#define VINSN_SEPARABLE_P(VI) (VINSN_TYPE (VI) == SET)
#define VINSN_CLONABLE_P(VI) (VINSN_SEPARABLE_P (VI) || VINSN_TYPE (VI) == USE)
#define VINSN_UNIQUE_P(VI) (!VINSN_CLONABLE_P (VI))
#define VINSN_LHS(VI) (IDATA_LHS (VINSN_ID (VI)))
#define VINSN_RHS(VI) (IDATA_RHS (VINSN_ID (VI)))
#define VINSN_REG_SETS(VI) (IDATA_REG_SETS (VINSN_ID (VI)))
#define VINSN_REG_USES(VI) (IDATA_REG_USES (VINSN_ID (VI)))
#define VINSN_REG_CLOBBERS(VI) (IDATA_REG_CLOBBERS (VINSN_ID (VI)))
#define VINSN_COUNT(VI) ((VI)->count)
#define VINSN_MAY_TRAP_P(VI) ((VI)->may_trap_p)


/* An entry of the hashtable describing transformations happened when
   moving up through an insn.  */
struct transformed_insns
{
  /* Previous vinsn.  Used to find the proper element.  */
  vinsn_t vinsn_old;

  /* A new vinsn.  */
  vinsn_t vinsn_new;

  /* Speculative status.  */
  ds_t ds;

  /* Type of transformation happened.  */
  enum local_trans_type type;

  /* Whether a conflict on the target register happened.  */
  BOOL_BITFIELD was_target_conflict : 1;

  /* Whether a check was needed.  */
  BOOL_BITFIELD needs_check : 1;
};

/* Indexed by INSN_LUID, the collection of all data associated with
   a single instruction that is in the stream.  */
struct _sel_insn_data
{
  /* The expression that contains vinsn for this insn and some
     flow-sensitive data like priority.  */
  expr_def expr;

  /* If (WS_LEVEL == GLOBAL_LEVEL) then AV is empty.  */
  int ws_level;

  /* A number that helps in defining a traversing order for a region.  */
  int seqno;

  /* A liveness data computed above this insn.  */
  regset live;

  /* An INSN_UID bit is set when deps analysis result is already known.  */
  bitmap analyzed_deps;

  /* An INSN_UID bit is set when a hard dep was found, not set when
     no dependence is found.  This is meaningful only when the analyzed_deps
     bitmap has its bit set.  */
  bitmap found_deps;

  /* An INSN_UID bit is set when this is a bookkeeping insn generated from
     a parent with this uid.  If a parent is a bookkeeping copy, all its
     originators are transitively included in this set.  */
  bitmap originators;

  /* A hashtable caching the result of insn transformations through this one.  */
  htab_t transformed_insns;

  /* A context incapsulating this insn.  */
  struct deps_desc deps_context;

  /* This field is initialized at the beginning of scheduling and is used
     to handle sched group instructions.  If it is non-null, then it points
     to the instruction, which should be forced to schedule next.  Such
     instructions are unique.  */
  insn_t sched_next;

  /* Cycle at which insn was scheduled.  It is greater than zero if insn was
     scheduled.  This is used for bundling.  */
  int sched_cycle;

  /* Cycle at which insn's data will be fully ready.  */
  int ready_cycle;

  /* Speculations that are being checked by this insn.  */
  ds_t spec_checked_ds;

  /* Whether the live set valid or not.  */
  BOOL_BITFIELD live_valid_p : 1;
  /* Insn is an ASM.  */
  BOOL_BITFIELD asm_p : 1;

  /* True when an insn is scheduled after we've determined that a stall is
     required.
     This is used when emulating the Haifa scheduler for bundling.  */
  BOOL_BITFIELD after_stall_p : 1;
};

typedef struct _sel_insn_data sel_insn_data_def;
typedef sel_insn_data_def *sel_insn_data_t;

DEF_VEC_O (sel_insn_data_def);
DEF_VEC_ALLOC_O (sel_insn_data_def, heap);
extern VEC (sel_insn_data_def, heap) *s_i_d;

/* Accessor macros for s_i_d.  */
#define SID(INSN) (VEC_index (sel_insn_data_def, s_i_d,	INSN_LUID (INSN)))
#define SID_BY_UID(UID) (VEC_index (sel_insn_data_def, s_i_d,	LUID_BY_UID (UID)))

extern sel_insn_data_def insn_sid (insn_t);

#define INSN_ASM_P(INSN) (SID (INSN)->asm_p)
#define INSN_SCHED_NEXT(INSN) (SID (INSN)->sched_next)
#define INSN_ANALYZED_DEPS(INSN) (SID (INSN)->analyzed_deps)
#define INSN_FOUND_DEPS(INSN) (SID (INSN)->found_deps)
#define INSN_DEPS_CONTEXT(INSN) (SID (INSN)->deps_context)
#define INSN_ORIGINATORS(INSN) (SID (INSN)->originators)
#define INSN_ORIGINATORS_BY_UID(UID) (SID_BY_UID (UID)->originators)
#define INSN_TRANSFORMED_INSNS(INSN) (SID (INSN)->transformed_insns)

#define INSN_EXPR(INSN) (&SID (INSN)->expr)
#define INSN_LIVE(INSN) (SID (INSN)->live)
#define INSN_LIVE_VALID_P(INSN) (SID (INSN)->live_valid_p)
#define INSN_VINSN(INSN) (EXPR_VINSN (INSN_EXPR (INSN)))
#define INSN_TYPE(INSN) (VINSN_TYPE (INSN_VINSN (INSN)))
#define INSN_SIMPLEJUMP_P(INSN) (INSN_TYPE (INSN) == PC)
#define INSN_LHS(INSN) (VINSN_LHS (INSN_VINSN (INSN)))
#define INSN_RHS(INSN) (VINSN_RHS (INSN_VINSN (INSN)))
#define INSN_REG_SETS(INSN) (VINSN_REG_SETS (INSN_VINSN (INSN)))
#define INSN_REG_CLOBBERS(INSN) (VINSN_REG_CLOBBERS (INSN_VINSN (INSN)))
#define INSN_REG_USES(INSN) (VINSN_REG_USES (INSN_VINSN (INSN)))
#define INSN_SCHED_TIMES(INSN) (EXPR_SCHED_TIMES (INSN_EXPR (INSN)))
#define INSN_SEQNO(INSN) (SID (INSN)->seqno)
#define INSN_AFTER_STALL_P(INSN) (SID (INSN)->after_stall_p)
#define INSN_SCHED_CYCLE(INSN) (SID (INSN)->sched_cycle)
#define INSN_READY_CYCLE(INSN) (SID (INSN)->ready_cycle)
#define INSN_SPEC_CHECKED_DS(INSN) (SID (INSN)->spec_checked_ds)

/* A global level shows whether an insn is valid or not.  */
extern int global_level;

#define INSN_WS_LEVEL(INSN) (SID (INSN)->ws_level)

extern av_set_t get_av_set (insn_t);
extern int get_av_level (insn_t);

#define AV_SET(INSN) (get_av_set (INSN))
#define AV_LEVEL(INSN) (get_av_level (INSN))
#define AV_SET_VALID_P(INSN) (AV_LEVEL (INSN) == global_level)

/* A list of fences currently in the works.  */
extern flist_t fences;

/* A NOP pattern used as a placeholder for real insns.  */
extern rtx nop_pattern;

/* An insn that 'contained' in EXIT block.  */
extern rtx exit_insn;

/* Provide a separate luid for the insn.  */
#define INSN_INIT_TODO_LUID (1)

/* Initialize s_s_i_d.  */
#define INSN_INIT_TODO_SSID (2)

/* Initialize data for simplejump.  */
#define INSN_INIT_TODO_SIMPLEJUMP (4)

/* Return true if INSN is a local NOP.  The nop is local in the sense that
   it was emitted by the scheduler as a temporary insn and will soon be
   deleted.  These nops are identified by their pattern.  */
#define INSN_NOP_P(INSN) (PATTERN (INSN) == nop_pattern)

/* Return true if INSN is linked into instruction stream.
   NB: It is impossible for INSN to have one field null and the other not
   null: gcc_assert ((PREV_INSN (INSN) == NULL_RTX)
   == (NEXT_INSN (INSN) == NULL_RTX)) is valid.  */
#define INSN_IN_STREAM_P(INSN) (PREV_INSN (INSN) && NEXT_INSN (INSN))

/* Return true if INSN is in current fence.  */
#define IN_CURRENT_FENCE_P(INSN) (flist_lookup (fences, INSN) != NULL)

/* Marks loop as being considered for pipelining.  */
#define MARK_LOOP_FOR_PIPELINING(LOOP) ((LOOP)->aux = (void *)(size_t)(1))
#define LOOP_MARKED_FOR_PIPELINING_P(LOOP) ((size_t)((LOOP)->aux))

/* Saved loop preheader to transfer when scheduling the loop.  */
#define LOOP_PREHEADER_BLOCKS(LOOP) ((size_t)((LOOP)->aux) == 1         \
                                     ? NULL                             \
                                     : ((VEC(basic_block, heap) *) (LOOP)->aux))
#define SET_LOOP_PREHEADER_BLOCKS(LOOP,BLOCKS) ((LOOP)->aux             \
                                                = (BLOCKS != NULL       \
                                                   ? BLOCKS             \
                                                   : (LOOP)->aux))

extern bitmap blocks_to_reschedule;


/* A variable to track which part of rtx we are scanning in
   sched-deps.c: sched_analyze_insn ().  */
enum deps_where_def
  {
    DEPS_IN_INSN,
    DEPS_IN_LHS,
    DEPS_IN_RHS,
    DEPS_IN_NOWHERE
  };
typedef enum deps_where_def deps_where_t;


/* Per basic block data for the whole CFG.  */
typedef struct
{
  /* For each bb header this field contains a set of live registers.
     For all other insns this field has a NULL.
     We also need to know LV sets for the instructions, that are immediatly
     after the border of the region.  */
  regset lv_set;

  /* Status of LV_SET.
     true - block has usable LV_SET.
     false - block's LV_SET should be recomputed.  */
  bool lv_set_valid_p;
} sel_global_bb_info_def;

typedef sel_global_bb_info_def *sel_global_bb_info_t;

DEF_VEC_O (sel_global_bb_info_def);
DEF_VEC_ALLOC_O (sel_global_bb_info_def, heap);

/* Per basic block data.  This array is indexed by basic block index.  */
extern VEC (sel_global_bb_info_def, heap) *sel_global_bb_info;

extern void sel_extend_global_bb_info (void);
extern void sel_finish_global_bb_info (void);

/* Get data for BB.  */
#define SEL_GLOBAL_BB_INFO(BB)					\
  (VEC_index (sel_global_bb_info_def, sel_global_bb_info, (BB)->index))

/* Access macros.  */
#define BB_LV_SET(BB) (SEL_GLOBAL_BB_INFO (BB)->lv_set)
#define BB_LV_SET_VALID_P(BB) (SEL_GLOBAL_BB_INFO (BB)->lv_set_valid_p)

/* Per basic block data for the region.  */
typedef struct
{
  /* This insn stream is constructed in such a way that it should be
     traversed by PREV_INSN field - (*not* NEXT_INSN).  */
  rtx note_list;

  /* Cached availability set at the beginning of a block.
     See also AV_LEVEL () for conditions when this av_set can be used.  */
  av_set_t av_set;

  /* If (AV_LEVEL == GLOBAL_LEVEL) then AV is valid.  */
  int av_level;
} sel_region_bb_info_def;

typedef sel_region_bb_info_def *sel_region_bb_info_t;

DEF_VEC_O (sel_region_bb_info_def);
DEF_VEC_ALLOC_O (sel_region_bb_info_def, heap);

/* Per basic block data.  This array is indexed by basic block index.  */
extern VEC (sel_region_bb_info_def, heap) *sel_region_bb_info;

/* Get data for BB.  */
#define SEL_REGION_BB_INFO(BB) (VEC_index (sel_region_bb_info_def,	\
					   sel_region_bb_info, (BB)->index))

/* Get BB's note_list.
   A note_list is a list of various notes that was scattered across BB
   before scheduling, and will be appended at the beginning of BB after
   scheduling is finished.  */
#define BB_NOTE_LIST(BB) (SEL_REGION_BB_INFO (BB)->note_list)

#define BB_AV_SET(BB) (SEL_REGION_BB_INFO (BB)->av_set)
#define BB_AV_LEVEL(BB) (SEL_REGION_BB_INFO (BB)->av_level)
#define BB_AV_SET_VALID_P(BB) (BB_AV_LEVEL (BB) == global_level)

/* Used in bb_in_ebb_p.  */
extern bitmap_head *forced_ebb_heads;

/* The loop nest being pipelined.  */
extern struct loop *current_loop_nest;

/* Saves pipelined blocks.  Bitmap is indexed by bb->index.  */
extern sbitmap bbs_pipelined;

/* Various flags.  */
extern bool enable_moveup_set_path_p;
extern bool pipelining_p;
extern bool bookkeeping_p;
extern int max_insns_to_rename;
extern bool preheader_removed;

/* Software lookahead window size.
   According to the results in Nakatani and Ebcioglu [1993], window size of 16
   is enough to extract most ILP in integer code.  */
#define MAX_WS (PARAM_VALUE (PARAM_SELSCHED_MAX_LOOKAHEAD))

extern regset sel_all_regs;


/* Successor iterator backend.  */
typedef struct
{
  /* True if we're at BB end.  */
  bool bb_end;

  /* An edge on which we're iterating.  */
  edge e1;

  /* The previous edge saved after skipping empty blocks.  */
  edge e2;

  /* Edge iterator used when there are successors in other basic blocks.  */
  edge_iterator ei;

  /* Successor block we're traversing.  */
  basic_block bb;

  /* Flags that are passed to the iterator.  We return only successors
     that comply to these flags.  */
  short flags;

  /* When flags include SUCCS_ALL, this will be set to the exact type
     of the sucessor we're traversing now.  */
  short current_flags;

  /* If skip to loop exits, save here information about loop exits.  */
  int current_exit;
  VEC (edge, heap) *loop_exits;
} succ_iterator;

/* A structure returning all successor's information.  */
struct succs_info
{
  /* Flags that these succcessors were computed with.  */
  short flags;

  /* Successors that correspond to the flags.  */
  insn_vec_t succs_ok;

  /* Their probabilities.  As of now, we don't need this for other
     successors.  */
  VEC(int,heap) *probs_ok;

  /* Other successors.  */
  insn_vec_t succs_other;

  /* Probability of all successors.  */
  int all_prob;

  /* The number of all successors.  */
  int all_succs_n;

  /* The number of good successors.  */
  int succs_ok_n;
};

/* Some needed definitions.  */
extern basic_block after_recovery;

extern insn_t sel_bb_head (basic_block);
extern insn_t sel_bb_end (basic_block);
extern bool sel_bb_empty_p (basic_block);
extern bool in_current_region_p (basic_block);

/* Flags to pass to compute_succs_info and FOR_EACH_SUCC.
   Any successor will fall into exactly one category.   */

/* Include normal successors.  */
#define SUCCS_NORMAL (1)

/* Include back-edge successors.  */
#define SUCCS_BACK (2)

/* Include successors that are outside of the current region.  */
#define SUCCS_OUT (4)

/* When pipelining of the outer loops is enabled, skip innermost loops
   to their exits.  */
#define SUCCS_SKIP_TO_LOOP_EXITS (8)

/* Include all successors.  */
#define SUCCS_ALL (SUCCS_NORMAL | SUCCS_BACK | SUCCS_OUT)

/* Functions that are used in sel-sched.c.  */

/* List functions.  */
extern ilist_t ilist_copy (ilist_t);
extern ilist_t ilist_invert (ilist_t);
extern void blist_add (blist_t *, insn_t, ilist_t, deps_t);
extern void blist_remove (blist_t *);
extern void flist_tail_init (flist_tail_t);

extern fence_t flist_lookup (flist_t, insn_t);
extern void flist_clear (flist_t *);
extern void def_list_add (def_list_t *, insn_t, bool);

EXTERN_C_END

#endif /* GCC_SEL_SCHED_IR_H */
