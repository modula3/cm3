/* Modula-3: modified */

/* Alias analysis for GNU C
   Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006,
   2007, 2008, 2009, 2010 Free Software Foundation, Inc.
   Contributed by John Carr (jfc@mit.edu).

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "tm_p.h"
#include "function.h"
#include "alias.h"
#include "emit-rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "flags.h"
#include "output.h"
#include "diagnostic-core.h"
#include "cselib.h"
#include "splay-tree.h"
#include "ggc.h"
#include "langhooks.h"
#include "timevar.h"
#include "target.h"
#include "cgraph.h"
#include "tree-pass.h"
#include "df.h"
#include "tree-ssa-alias.h"
#include "pointer-set.h"
#include "tree-flow.h"

EXTERN_C_START

/* The aliasing API provided here solves related but different problems:

   Say there exists (in c)

   struct X {
     struct Y y1;
     struct Z z2;
   } x1, *px1,  *px2;

   struct Y y2, *py;
   struct Z z2, *pz;


   py = &px1.y1;
   px2 = &x1;

   Consider the four questions:

   Can a store to x1 interfere with px2->y1?
   Can a store to x1 interfere with px2->z2?
   (*px2).z2
   Can a store to x1 change the value pointed to by with py?
   Can a store to x1 change the value pointed to by with pz?

   The answer to these questions can be yes, yes, yes, and maybe.

   The first two questions can be answered with a simple examination
   of the type system.  If structure X contains a field of type Y then
   a store thru a pointer to an X can overwrite any field that is
   contained (recursively) in an X (unless we know that px1 != px2).

   The last two of the questions can be solved in the same way as the
   first two questions but this is too conservative.  The observation
   is that in some cases analysis we can know if which (if any) fields
   are addressed and if those addresses are used in bad ways.  This
   analysis may be language specific.  In C, arbitrary operations may
   be applied to pointers.  However, there is some indication that
   this may be too conservative for some C++ types.

   The pass ipa-type-escape does this analysis for the types whose
   instances do not escape across the compilation boundary.

   Historically in GCC, these two problems were combined and a single
   data structure was used to represent the solution to these
   problems.  We now have two similar but different data structures,
   The data structure to solve the last two question is similar to the
   first, but does not contain have the fields in it whose address are
   never taken.  For types that do escape the compilation unit, the
   data structures will have identical information.
*/

/* The alias sets assigned to MEMs assist the back-end in determining
   which MEMs can alias which other MEMs.  In general, two MEMs in
   different alias sets cannot alias each other, with one important
   exception.  Consider something like:

     struct S { int i; double d; };

   a store to an `S' can alias something of either type `int' or type
   `double'.  (However, a store to an `int' cannot alias a `double'
   and vice versa.)  We indicate this via a tree structure that looks
   like:
	   struct S
	    /   \
	   /     \
	 |/_     _\|
	 int    double

   (The arrows are directed and point downwards.)
    In this situation we say the alias set for `struct S' is the
   `superset' and that those for `int' and `double' are `subsets'.

   To see whether two alias sets can point to the same memory, we must
   see if either alias set is a subset of the other. We need not trace
   past immediate descendants, however, since we propagate all
   grandchildren up one level.

   Alias set zero is implicitly a superset of all other alias sets.
   However, this is no actual entry for alias set zero.  It is an
   error to attempt to explicitly construct a subset of zero.  */

struct GTY(()) alias_set_entry_d {
  /* The alias set number, as stored in MEM_ALIAS_SET.  */
  alias_set_type alias_set;

  /* Nonzero if would have a child of zero: this effectively makes this
     alias set the same as alias set zero.  */
  int has_zero_child;

  /* The children of the alias set.  These are not just the immediate
     children, but, in fact, all descendants.  So, if we have:

       struct T { struct S s; float f; }

     continuing our example above, the children here will be all of
     `int', `double', `float', and `struct S'.  */
  splay_tree GTY((param1_is (int), param2_is (int))) children;
};
typedef struct alias_set_entry_d *alias_set_entry;

static int rtx_equal_for_memref_p (const_rtx, const_rtx);
static void record_set (rtx, const_rtx, void *);
static rtx find_base_value (rtx);
static int mems_in_disjoint_alias_sets_p (const_rtx, const_rtx);
static int insert_subset_children (splay_tree_node, void*);
static alias_set_entry get_alias_set_entry (alias_set_type);
static int write_dependence_p (const_rtx, const_rtx, int);

static void memory_modified_1 (rtx, const_rtx, void *);

/* Set up all info needed to perform alias analysis on memory references.  */

/* Returns the size in bytes of the mode of X.  */
#define SIZE_FOR_MODE(X) (GET_MODE_SIZE (GET_MODE (X)))

/* Returns nonzero if MEM1 and MEM2 do not alias because they are in
   different alias sets.  We ignore alias sets in functions making use
   of variable arguments because the va_arg macros on some systems are
   not legal ANSI C.  */
#define DIFFERENT_ALIAS_SETS_P(MEM1, MEM2)			\
  mems_in_disjoint_alias_sets_p (MEM1, MEM2)

/* Cap the number of passes we make over the insns propagating alias
   information through set chains.   10 is a completely arbitrary choice.  */
#define MAX_ALIAS_LOOP_PASSES 10

/* reg_base_value[N] gives an address to which register N is related.
   If all sets after the first add or subtract to the current value
   or otherwise modify it so it does not point to a different top level
   object, reg_base_value[N] is equal to the address part of the source
   of the first set.

   A base address can be an ADDRESS, SYMBOL_REF, or LABEL_REF.  ADDRESS
   expressions represent certain special values: function arguments and
   the stack, frame, and argument pointers.

   The contents of an ADDRESS is not normally used, the mode of the
   ADDRESS determines whether the ADDRESS is a function argument or some
   other special value.  Pointer equality, not rtx_equal_p, determines whether
   two ADDRESS expressions refer to the same base address.

   The only use of the contents of an ADDRESS is for determining if the
   current function performs nonlocal memory memory references for the
   purposes of marking the function as a constant function.  */

static GTY(()) VEC(rtx,gc) *reg_base_value;
static rtx *new_reg_base_value;

/* We preserve the copy of old array around to avoid amount of garbage
   produced.  About 8% of garbage produced were attributed to this
   array.  */
static GTY((deletable)) VEC(rtx,gc) *old_reg_base_value;

#define static_reg_base_value \
  (this_target_rtl->x_static_reg_base_value)

#define REG_BASE_VALUE(X)				\
  (REGNO (X) < VEC_length (rtx, reg_base_value)		\
   ? VEC_index (rtx, reg_base_value, REGNO (X)) : 0)

/* Vector indexed by N giving the initial (unchanging) value known for
   pseudo-register N.  This array is initialized in init_alias_analysis,
   and does not change until end_alias_analysis is called.  */
static GTY((length("reg_known_value_size"))) rtx *reg_known_value;

/* Indicates number of valid entries in reg_known_value.  */
static GTY(()) unsigned int reg_known_value_size;

/* Vector recording for each reg_known_value whether it is due to a
   REG_EQUIV note.  Future passes (viz., reload) may replace the
   pseudo with the equivalent expression and so we account for the
   dependences that would be introduced if that happens.

   The REG_EQUIV notes created in assign_parms may mention the arg
   pointer, and there are explicit insns in the RTL that modify the
   arg pointer.  Thus we must ensure that such insns don't get
   scheduled across each other because that would invalidate the
   REG_EQUIV notes.  One could argue that the REG_EQUIV notes are
   wrong, but solving the problem in the scheduler will likely give
   better code, so we do it here.  */
static bool *reg_known_equiv_p;

/* True when scanning insns from the start of the rtl to the
   NOTE_INSN_FUNCTION_BEG note.  */
static bool copying_arguments;

DEF_VEC_P(alias_set_entry);
DEF_VEC_ALLOC_P(alias_set_entry,gc);

/* The splay-tree used to store the various alias set entries.  */
static GTY (()) VEC(alias_set_entry,gc) *alias_sets;

/* Build a decomposed reference object for querying the alias-oracle
   from the MEM rtx and store it in *REF.
   Returns false if MEM is not suitable for the alias-oracle.  */

static bool
ao_ref_from_mem (ao_ref *ref, const_rtx mem)
{
  tree expr = MEM_EXPR (mem);
  tree base;

  if (!expr)
    return false;

  ao_ref_init (ref, expr);

  /* Get the base of the reference and see if we have to reject or
     adjust it.  */
  base = ao_ref_base (ref);
  if (base == NULL_TREE)
    return false;

  /* The tree oracle doesn't like to have these.  */
  if (TREE_CODE (base) == FUNCTION_DECL
      || TREE_CODE (base) == LABEL_DECL)
    return false;

  /* If this is a pointer dereference of a non-SSA_NAME punt.
     ???  We could replace it with a pointer to anything.  */
  if ((INDIRECT_REF_P (base)
       || TREE_CODE (base) == MEM_REF)
      && TREE_CODE (TREE_OPERAND (base, 0)) != SSA_NAME)
    return false;
  if (TREE_CODE (base) == TARGET_MEM_REF
      && TMR_BASE (base)
      && TREE_CODE (TMR_BASE (base)) != SSA_NAME)
    return false;

  /* If this is a reference based on a partitioned decl replace the
     base with an INDIRECT_REF of the pointer representative we
     created during stack slot partitioning.  */
  if (TREE_CODE (base) == VAR_DECL
      && ! TREE_STATIC (base)
      && cfun->gimple_df->decls_to_pointers != NULL)
    {
      void *namep;
      namep = pointer_map_contains (cfun->gimple_df->decls_to_pointers, base);
      if (namep)
	ref->base = build_simple_mem_ref (*(tree *)namep);
    }
  else if (TREE_CODE (base) == TARGET_MEM_REF
	   && TREE_CODE (TMR_BASE (base)) == ADDR_EXPR
	   && TREE_CODE (TREE_OPERAND (TMR_BASE (base), 0)) == VAR_DECL
	   && ! TREE_STATIC (TREE_OPERAND (TMR_BASE (base), 0))
	   && cfun->gimple_df->decls_to_pointers != NULL)
    {
      void *namep;
      namep = pointer_map_contains (cfun->gimple_df->decls_to_pointers,
				    TREE_OPERAND (TMR_BASE (base), 0));
      if (namep)
	ref->base = build_simple_mem_ref (*(tree *)namep);
    }

  ref->ref_alias_set = MEM_ALIAS_SET (mem);

  /* If MEM_OFFSET or MEM_SIZE are NULL we have to punt.
     Keep points-to related information though.  */
  if (!MEM_OFFSET (mem)
      || !MEM_SIZE (mem))
    {
      ref->ref = NULL_TREE;
      ref->offset = 0;
      ref->size = -1;
      ref->max_size = -1;
      return true;
    }

  /* If the base decl is a parameter we can have negative MEM_OFFSET in
     case of promoted subregs on bigendian targets.  Trust the MEM_EXPR
     here.  */
  if (INTVAL (MEM_OFFSET (mem)) < 0
      && ((INTVAL (MEM_SIZE (mem)) + INTVAL (MEM_OFFSET (mem)))
	  * BITS_PER_UNIT) == ref->size)
    return true;

  ref->offset += INTVAL (MEM_OFFSET (mem)) * BITS_PER_UNIT;
  ref->size = INTVAL (MEM_SIZE (mem)) * BITS_PER_UNIT;

  /* The MEM may extend into adjacent fields, so adjust max_size if
     necessary.  */
  if (ref->max_size != -1
      && ref->size > ref->max_size)
    ref->max_size = ref->size;

  /* If MEM_OFFSET and MEM_SIZE get us outside of the base object of
     the MEM_EXPR punt.  This happens for STRICT_ALIGNMENT targets a lot.  */
  if (MEM_EXPR (mem) != get_spill_slot_decl (false)
      && (ref->offset < 0
	  || (DECL_P (ref->base)
	      && (!host_integerp (DECL_SIZE (ref->base), 1)
		  || (TREE_INT_CST_LOW (DECL_SIZE ((ref->base)))
		      < (unsigned HOST_WIDE_INT)(ref->offset + ref->size))))))
    return false;

  return true;
}

/* Query the alias-oracle on whether the two memory rtx X and MEM may
   alias.  If TBAA_P is set also apply TBAA.  Returns true if the
   two rtxen may alias, false otherwise.  */

static bool
rtx_refs_may_alias_p (const_rtx x, const_rtx mem, bool tbaa_p)
{
    return true;
}

/* Returns a pointer to the alias set entry for ALIAS_SET, if there is
   such an entry, or NULL otherwise.  */

static inline alias_set_entry
get_alias_set_entry (alias_set_type alias_set)
{
  return VEC_index (alias_set_entry, alias_sets, alias_set);
}

/* Returns nonzero if the alias sets for MEM1 and MEM2 are such that
   the two MEMs cannot alias each other.  */

static inline int
mems_in_disjoint_alias_sets_p (const_rtx mem1, const_rtx mem2)
{
/* Perform a basic sanity check.  Namely, that there are no alias sets
   if we're not using strict aliasing.  This helps to catch bugs
   whereby someone uses PUT_CODE, but doesn't clear MEM_ALIAS_SET, or
   where a MEM is allocated in some way other than by the use of
   gen_rtx_MEM, and the MEM_ALIAS_SET is not cleared.  If we begin to
   use alias sets to indicate that spilled registers cannot alias each
   other, we might need to remove this check.  */
  gcc_assert (flag_strict_aliasing
	      || (!MEM_ALIAS_SET (mem1) && !MEM_ALIAS_SET (mem2)));

  return ! alias_sets_conflict_p (MEM_ALIAS_SET (mem1), MEM_ALIAS_SET (mem2));
}

/* Insert the NODE into the splay tree given by DATA.  Used by
   record_alias_subset via splay_tree_foreach.  */

static int
insert_subset_children (splay_tree_node node, void *data)
{
  splay_tree_insert ((splay_tree) data, node->key, node->value);

  return 0;
}

/* Return true if the first alias set is a subset of the second.  */

bool
alias_set_subset_of (alias_set_type set1, alias_set_type set2)
{
  alias_set_entry ase;

  /* Everything is a subset of the "aliases everything" set.  */
  if (set2 == 0)
    return true;

  /* Otherwise, check if set1 is a subset of set2.  */
  ase = get_alias_set_entry (set2);
  if (ase != 0
      && (ase->has_zero_child
	  || splay_tree_lookup (ase->children,
			        (splay_tree_key) set1)))
    return true;
  return false;
}

/* Return 1 if the two specified alias sets may conflict.  */

int
alias_sets_conflict_p (alias_set_type set1, alias_set_type set2)
{
  alias_set_entry ase;

  /* The easy case.  */
  if (alias_sets_must_conflict_p (set1, set2))
    return 1;

  /* See if the first alias set is a subset of the second.  */
  ase = get_alias_set_entry (set1);
  if (ase != 0
      && (ase->has_zero_child
	  || splay_tree_lookup (ase->children,
				(splay_tree_key) set2)))
    return 1;

  /* Now do the same, but with the alias sets reversed.  */
  ase = get_alias_set_entry (set2);
  if (ase != 0
      && (ase->has_zero_child
	  || splay_tree_lookup (ase->children,
				(splay_tree_key) set1)))
    return 1;

  /* The two alias sets are distinct and neither one is the
     child of the other.  Therefore, they cannot conflict.  */
  return 0;
}

/* Return 1 if the two specified alias sets will always conflict.  */

int
alias_sets_must_conflict_p (alias_set_type set1, alias_set_type set2)
{
  if (set1 == 0 || set2 == 0 || set1 == set2)
    return 1;

  return 0;
}

/* Return 1 if any MEM object of type T1 will always conflict (using the
   dependency routines in this file) with any MEM object of type T2.
   This is used when allocating temporary storage.  If T1 and/or T2 are
   NULL_TREE, it means we know nothing about the storage.  */

int
objects_must_conflict_p (tree t1, tree t2)
{
  alias_set_type set1, set2;

  /* If neither has a type specified, we don't know if they'll conflict
     because we may be using them to store objects of various types, for
     example the argument and local variables areas of inlined functions.  */
  if (t1 == 0 && t2 == 0)
    return 0;

  /* If they are the same type, they must conflict.  */
  if (t1 == t2
      /* Likewise if both are volatile.  */
      || (t1 != 0 && TYPE_VOLATILE (t1) && t2 != 0 && TYPE_VOLATILE (t2)))
    return 1;

  set1 = t1 ? get_alias_set (t1) : 0;
  set2 = t2 ? get_alias_set (t2) : 0;

  /* We can't use alias_sets_conflict_p because we must make sure
     that every subtype of t1 will conflict with every subtype of
     t2 for which a pair of subobjects of these respective subtypes
     overlaps on the stack.  */
  return alias_sets_must_conflict_p (set1, set2);
}

/* Return true if all nested component references handled by
   get_inner_reference in T are such that we should use the alias set
   provided by the object at the heart of T.

   This is true for non-addressable components (which don't have their
   own alias set), as well as components of objects in alias set zero.
   This later point is a special case wherein we wish to override the
   alias set used by the component, but we don't have per-FIELD_DECL
   assignable alias sets.  */

bool
component_uses_parent_alias_set (const_tree t)
{
  while (1)
    {
      /* If we're at the end, it vacuously uses its own alias set.  */
      if (!handled_component_p (t))
	return false;

      switch (TREE_CODE (t))
	{
	case COMPONENT_REF:
	  if (DECL_NONADDRESSABLE_P (TREE_OPERAND (t, 1)))
	    return true;
	  break;

	case ARRAY_REF:
	case ARRAY_RANGE_REF:
	  if (TYPE_NONALIASED_COMPONENT (TREE_TYPE (TREE_OPERAND (t, 0))))
	    return true;
	  break;

	case REALPART_EXPR:
	case IMAGPART_EXPR:
	  break;

	default:
	  /* Bitfields and casts are never addressable.  */
	  return true;
	}

      t = TREE_OPERAND (t, 0);
      if (get_alias_set (TREE_TYPE (t)) == 0)
	return true;
    }
}

/* Return the alias set for the memory pointed to by T, which may be
   either a type or an expression.  Return -1 if there is nothing
   special about dereferencing T.  */

static alias_set_type
get_deref_alias_set_1 (tree t)
{
  /* If we're not doing any alias analysis, just assume everything
     aliases everything else.  */
  if (!flag_strict_aliasing)
    return 0;

  /* All we care about is the type.  */
  if (! TYPE_P (t))
    t = TREE_TYPE (t);

  /* If we have an INDIRECT_REF via a void pointer, we don't
     know anything about what that might alias.  Likewise if the
     pointer is marked that way.  */
  if (TREE_CODE (TREE_TYPE (t)) == VOID_TYPE
      || TYPE_REF_CAN_ALIAS_ALL (t))
    return 0;

  return -1;
}

/* Return the alias set for the memory pointed to by T, which may be
   either a type or an expression.  */

alias_set_type
get_deref_alias_set (tree t)
{
  alias_set_type set = get_deref_alias_set_1 (t);

  /* Fall back to the alias-set of the pointed-to type.  */
  if (set == -1)
    {
      if (! TYPE_P (t))
	t = TREE_TYPE (t);
      set = get_alias_set (TREE_TYPE (t));
    }

  return set;
}

/* Return the alias set for T, which may be either a type or an
   expression.  Call language-specific routine for help, if needed.  */

alias_set_type
get_alias_set (tree t)
{
  alias_set_type set;

  /* If we're not doing any alias analysis, just assume everything
     aliases everything else.  Also return 0 if this or its type is
     an error.  */
  if (! flag_strict_aliasing || t == error_mark_node
      || (! TYPE_P (t)
	  && (TREE_TYPE (t) == 0 || TREE_TYPE (t) == error_mark_node)))
    return 0;

  /* We can be passed either an expression or a type.  This and the
     language-specific routine may make mutually-recursive calls to each other
     to figure out what to do.  At each juncture, we see if this is a tree
     that the language may need to handle specially.  First handle things that
     aren't types.  */
  if (! TYPE_P (t))
    {
      tree inner;

      /* Give the language a chance to do something with this tree
	 before we look at it.  */
      STRIP_NOPS (t);
      set = lang_hooks.get_alias_set (t);
      if (set != -1)
	return set;

      /* Get the base object of the reference.  */
      inner = t;
      while (handled_component_p (inner))
	{
	  /* If there is a VIEW_CONVERT_EXPR in the chain we cannot use
	     the type of any component references that wrap it to
	     determine the alias-set.  */
	  if (TREE_CODE (inner) == VIEW_CONVERT_EXPR)
	    t = TREE_OPERAND (inner, 0);
	  inner = TREE_OPERAND (inner, 0);
	}

      /* Handle pointer dereferences here, they can override the
	 alias-set.  */
      if (INDIRECT_REF_P (inner))
	{
	  set = get_deref_alias_set_1 (TREE_OPERAND (inner, 0));
	  if (set != -1)
	    return set;
	}
      else if (TREE_CODE (inner) == TARGET_MEM_REF)
	return get_deref_alias_set (TMR_OFFSET (inner));
      else if (TREE_CODE (inner) == MEM_REF)
	{
	  set = get_deref_alias_set_1 (TREE_OPERAND (inner, 1));
	  if (set != -1)
	    return set;
	}

      /* If the innermost reference is a MEM_REF that has a
	 conversion embedded treat it like a VIEW_CONVERT_EXPR above,
	 using the memory access type for determining the alias-set.  */
     if (TREE_CODE (inner) == MEM_REF
	 && TYPE_MAIN_VARIANT (TREE_TYPE (inner))
	    != TYPE_MAIN_VARIANT
	       (TREE_TYPE (TREE_TYPE (TREE_OPERAND (inner, 1)))))
       return get_deref_alias_set (TREE_OPERAND (inner, 1));

      /* Otherwise, pick up the outermost object that we could have a pointer
	 to, processing conversions as above.  */
      while (component_uses_parent_alias_set (t))
	{
	  t = TREE_OPERAND (t, 0);
	  STRIP_NOPS (t);
	}

      /* If we've already determined the alias set for a decl, just return
	 it.  This is necessary for C++ anonymous unions, whose component
	 variables don't look like union members (boo!).  */
      if (TREE_CODE (t) == VAR_DECL
	  && DECL_RTL_SET_P (t) && MEM_P (DECL_RTL (t)))
	return MEM_ALIAS_SET (DECL_RTL (t));

      /* Now all we care about is the type.  */
      t = TREE_TYPE (t);
    }

  /* Variant qualifiers don't affect the alias set, so get the main
     variant.  */
  t = TYPE_MAIN_VARIANT (t);

  /* Always use the canonical type as well.  If this is a type that
     requires structural comparisons to identify compatible types
     use alias set zero.  */
  if (TYPE_STRUCTURAL_EQUALITY_P (t))
    {
      /* Allow the language to specify another alias set for this
	 type.  */
      set = lang_hooks.get_alias_set (t);
      if (set != -1)
	return set;
      return 0;
    }

  t = TYPE_CANONICAL (t);

  /* Canonical types shouldn't form a tree nor should the canonical
     type require structural equality checks.  */
  gcc_checking_assert (TYPE_CANONICAL (t) == t
		       && !TYPE_STRUCTURAL_EQUALITY_P (t));

  /* If this is a type with a known alias set, return it.  */
  if (TYPE_ALIAS_SET_KNOWN_P (t))
    return TYPE_ALIAS_SET (t);

  /* We don't want to set TYPE_ALIAS_SET for incomplete types.  */
  if (!COMPLETE_TYPE_P (t))
    {
      /* For arrays with unknown size the conservative answer is the
	 alias set of the element type.  */
      if (TREE_CODE (t) == ARRAY_TYPE)
	return get_alias_set (TREE_TYPE (t));

      /* But return zero as a conservative answer for incomplete types.  */
      return 0;
    }

  /* See if the language has special handling for this type.  */
  set = lang_hooks.get_alias_set (t);
  if (set != -1)
    return set;

  /* There are no objects of FUNCTION_TYPE, so there's no point in
     using up an alias set for them.  (There are, of course, pointers
     and references to functions, but that's different.)  */
  else if (TREE_CODE (t) == FUNCTION_TYPE || TREE_CODE (t) == METHOD_TYPE)
    set = 0;

  /* Unless the language specifies otherwise, let vector types alias
     their components.  This avoids some nasty type punning issues in
     normal usage.  And indeed lets vectors be treated more like an
     array slice.  */
  else if (TREE_CODE (t) == VECTOR_TYPE)
    set = get_alias_set (TREE_TYPE (t));

  /* Unless the language specifies otherwise, treat array types the
     same as their components.  This avoids the asymmetry we get
     through recording the components.  Consider accessing a
     character(kind=1) through a reference to a character(kind=1)[1:1].
     Or consider if we want to assign integer(kind=4)[0:D.1387] and
     integer(kind=4)[4] the same alias set or not.
     Just be pragmatic here and make sure the array and its element
     type get the same alias set assigned.  */
  else if (TREE_CODE (t) == ARRAY_TYPE && !TYPE_NONALIASED_COMPONENT (t))
    set = get_alias_set (TREE_TYPE (t));

  /* From the former common C and C++ langhook implementation:

     Unfortunately, there is no canonical form of a pointer type.
     In particular, if we have `typedef int I', then `int *', and
     `I *' are different types.  So, we have to pick a canonical
     representative.  We do this below.

     Technically, this approach is actually more conservative that
     it needs to be.  In particular, `const int *' and `int *'
     should be in different alias sets, according to the C and C++
     standard, since their types are not the same, and so,
     technically, an `int **' and `const int **' cannot point at
     the same thing.

     But, the standard is wrong.  In particular, this code is
     legal C++:

     int *ip;
     int **ipp = &ip;
     const int* const* cipp = ipp;
     And, it doesn't make sense for that to be legal unless you
     can dereference IPP and CIPP.  So, we ignore cv-qualifiers on
     the pointed-to types.  This issue has been reported to the
     C++ committee.

     In addition to the above canonicalization issue, with LTO
     we should also canonicalize `T (*)[]' to `T *' avoiding
     alias issues with pointer-to element types and pointer-to
     array types.

     Likewise we need to deal with the situation of incomplete
     pointed-to types and make `*(struct X **)&a' and
     `*(struct X {} **)&a' alias.  Otherwise we will have to
     guarantee that all pointer-to incomplete type variants
     will be replaced by pointer-to complete type variants if
     they are available.

     With LTO the convenient situation of using `void *' to
     access and store any pointer type will also become
     more apparent (and `void *' is just another pointer-to
     incomplete type).  Assigning alias-set zero to `void *'
     and all pointer-to incomplete types is a not appealing
     solution.  Assigning an effective alias-set zero only
     affecting pointers might be - by recording proper subset
     relationships of all pointer alias-sets.

     Pointer-to function types are another grey area which
     needs caution.  Globbing them all into one alias-set
     or the above effective zero set would work.

     For now just assign the same alias-set to all pointers.
     That's simple and avoids all the above problems.  */
  else if (POINTER_TYPE_P (t)
	   && t != ptr_type_node)
    return get_alias_set (ptr_type_node);

  /* Otherwise make a new alias set for this type.  */
  else
    set = new_alias_set ();

  TYPE_ALIAS_SET (t) = set;

  /* If this is an aggregate type or a complex type, we must record any
     component aliasing information.  */
  if (AGGREGATE_TYPE_P (t) || TREE_CODE (t) == COMPLEX_TYPE)
    record_component_aliases (t);

  return set;
}

/* Return a brand-new alias set.  */

alias_set_type
new_alias_set (void)
{
  if (flag_strict_aliasing)
    {
      if (alias_sets == 0)
	VEC_safe_push (alias_set_entry, gc, alias_sets, 0);
      VEC_safe_push (alias_set_entry, gc, alias_sets, 0);
      return VEC_length (alias_set_entry, alias_sets) - 1;
    }
  else
    return 0;
}

/* Indicate that things in SUBSET can alias things in SUPERSET, but that
   not everything that aliases SUPERSET also aliases SUBSET.  For example,
   in C, a store to an `int' can alias a load of a structure containing an
   `int', and vice versa.  But it can't alias a load of a 'double' member
   of the same structure.  Here, the structure would be the SUPERSET and
   `int' the SUBSET.  This relationship is also described in the comment at
   the beginning of this file.

   This function should be called only once per SUPERSET/SUBSET pair.

   It is illegal for SUPERSET to be zero; everything is implicitly a
   subset of alias set zero.  */

void
record_alias_subset (alias_set_type superset, alias_set_type subset)
{
  alias_set_entry superset_entry;
  alias_set_entry subset_entry;

  /* It is possible in complex type situations for both sets to be the same,
     in which case we can ignore this operation.  */
  if (superset == subset)
    return;

  gcc_assert (superset);

  superset_entry = get_alias_set_entry (superset);
  if (superset_entry == 0)
    {
      /* Create an entry for the SUPERSET, so that we have a place to
	 attach the SUBSET.  */
      superset_entry = ggc_alloc_cleared_alias_set_entry_d ();
      superset_entry->alias_set = superset;
      superset_entry->children
	= splay_tree_new_ggc (splay_tree_compare_ints,
			      ggc_alloc_splay_tree_scalar_scalar_splay_tree_s,
			      ggc_alloc_splay_tree_scalar_scalar_splay_tree_node_s);
      superset_entry->has_zero_child = 0;
      VEC_replace (alias_set_entry, alias_sets, superset, superset_entry);
    }

  if (subset == 0)
    superset_entry->has_zero_child = 1;
  else
    {
      subset_entry = get_alias_set_entry (subset);
      /* If there is an entry for the subset, enter all of its children
	 (if they are not already present) as children of the SUPERSET.  */
      if (subset_entry)
	{
	  if (subset_entry->has_zero_child)
	    superset_entry->has_zero_child = 1;

	  splay_tree_foreach (subset_entry->children, insert_subset_children,
			      superset_entry->children);
	}

      /* Enter the SUBSET itself as a child of the SUPERSET.  */
      splay_tree_insert (superset_entry->children,
			 (splay_tree_key) subset, 0);
    }
}

/* Record that component types of TYPE, if any, are part of that type for
   aliasing purposes.  For record types, we only record component types
   for fields that are not marked non-addressable.  For array types, we
   only record the component type if it is not marked non-aliased.  */

void
record_component_aliases (tree type)
{
  alias_set_type superset = get_alias_set (type);
  tree field;

  if (superset == 0)
    return;

  switch (TREE_CODE (type))
    {
    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      /* Recursively record aliases for the base classes, if there are any.  */
      if (TYPE_BINFO (type))
	{
	  int i;
	  tree binfo, base_binfo;

	  for (binfo = TYPE_BINFO (type), i = 0;
	       BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
	    record_alias_subset (superset,
				 get_alias_set (BINFO_TYPE (base_binfo)));
	}
      for (field = TYPE_FIELDS (type); field != 0; field = DECL_CHAIN (field))
	if (TREE_CODE (field) == FIELD_DECL && !DECL_NONADDRESSABLE_P (field))
	  record_alias_subset (superset, get_alias_set (TREE_TYPE (field)));
      break;

    case COMPLEX_TYPE:
      record_alias_subset (superset, get_alias_set (TREE_TYPE (type)));
      break;

    /* VECTOR_TYPE and ARRAY_TYPE share the alias set with their
       element type.  */

    default:
      break;
    }
}

/* Allocate an alias set for use in storing and reading from the varargs
   spill area.  */

static GTY(()) alias_set_type varargs_set = -1;

alias_set_type
get_varargs_alias_set (void)
{
#if 1
  /* We now lower VA_ARG_EXPR, and there's currently no way to attach the
     varargs alias set to an INDIRECT_REF (FIXME!), so we can't
     consistently use the varargs alias set for loads from the varargs
     area.  So don't use it anywhere.  */
  return 0;
#else
  if (varargs_set == -1)
    varargs_set = new_alias_set ();

  return varargs_set;
#endif
}

/* Likewise, but used for the fixed portions of the frame, e.g., register
   save areas.  */

static GTY(()) alias_set_type frame_set = -1;

alias_set_type
get_frame_alias_set (void)
{
  if (frame_set == -1)
    frame_set = new_alias_set ();

  return frame_set;
}

/* Inside SRC, the source of a SET, find a base address.  */

static rtx
find_base_value (rtx src)
{
  unsigned int regno;

#if defined (FIND_BASE_TERM)
  /* Try machine-dependent ways to find the base term.  */
  src = FIND_BASE_TERM (src);
#endif

  switch (GET_CODE (src))
    {
    case SYMBOL_REF:
    case LABEL_REF:
      return src;

    case REG:
      regno = REGNO (src);
      /* At the start of a function, argument registers have known base
	 values which may be lost later.  Returning an ADDRESS
	 expression here allows optimization based on argument values
	 even when the argument registers are used for other purposes.  */
      if (regno < FIRST_PSEUDO_REGISTER && copying_arguments)
	return new_reg_base_value[regno];

      /* If a pseudo has a known base value, return it.  Do not do this
	 for non-fixed hard regs since it can result in a circular
	 dependency chain for registers which have values at function entry.

	 The test above is not sufficient because the scheduler may move
	 a copy out of an arg reg past the NOTE_INSN_FUNCTION_BEGIN.  */
      if ((regno >= FIRST_PSEUDO_REGISTER || fixed_regs[regno])
	  && regno < VEC_length (rtx, reg_base_value))
	{
	  /* If we're inside init_alias_analysis, use new_reg_base_value
	     to reduce the number of relaxation iterations.  */
	  if (new_reg_base_value && new_reg_base_value[regno]
	      && DF_REG_DEF_COUNT (regno) == 1)
	    return new_reg_base_value[regno];

	  if (VEC_index (rtx, reg_base_value, regno))
	    return VEC_index (rtx, reg_base_value, regno);
	}

      return 0;

    case MEM:
      /* Check for an argument passed in memory.  Only record in the
	 copying-arguments block; it is too hard to track changes
	 otherwise.  */
      if (copying_arguments
	  && (XEXP (src, 0) == arg_pointer_rtx
	      || (GET_CODE (XEXP (src, 0)) == PLUS
		  && XEXP (XEXP (src, 0), 0) == arg_pointer_rtx)))
	return gen_rtx_ADDRESS (VOIDmode, src);
      return 0;

    case CONST:
      src = XEXP (src, 0);
      if (GET_CODE (src) != PLUS && GET_CODE (src) != MINUS)
	break;

      /* ... fall through ...  */

    case PLUS:
    case MINUS:
      {
	rtx temp, src_0 = XEXP (src, 0), src_1 = XEXP (src, 1);

	/* If either operand is a REG that is a known pointer, then it
	   is the base.  */
	if (REG_P (src_0) && REG_POINTER (src_0))
	  return find_base_value (src_0);
	if (REG_P (src_1) && REG_POINTER (src_1))
	  return find_base_value (src_1);

	/* If either operand is a REG, then see if we already have
	   a known value for it.  */
	if (REG_P (src_0))
	  {
	    temp = find_base_value (src_0);
	    if (temp != 0)
	      src_0 = temp;
	  }

	if (REG_P (src_1))
	  {
	    temp = find_base_value (src_1);
	    if (temp!= 0)
	      src_1 = temp;
	  }

	/* If either base is named object or a special address
	   (like an argument or stack reference), then use it for the
	   base term.  */
	if (src_0 != 0
	    && (GET_CODE (src_0) == SYMBOL_REF
		|| GET_CODE (src_0) == LABEL_REF
		|| (GET_CODE (src_0) == ADDRESS
		    && GET_MODE (src_0) != VOIDmode)))
	  return src_0;

	if (src_1 != 0
	    && (GET_CODE (src_1) == SYMBOL_REF
		|| GET_CODE (src_1) == LABEL_REF
		|| (GET_CODE (src_1) == ADDRESS
		    && GET_MODE (src_1) != VOIDmode)))
	  return src_1;

	/* Guess which operand is the base address:
	   If either operand is a symbol, then it is the base.  If
	   either operand is a CONST_INT, then the other is the base.  */
	if (CONST_INT_P (src_1) || CONSTANT_P (src_0))
	  return find_base_value (src_0);
	else if (CONST_INT_P (src_0) || CONSTANT_P (src_1))
	  return find_base_value (src_1);

	return 0;
      }

    case LO_SUM:
      /* The standard form is (lo_sum reg sym) so look only at the
	 second operand.  */
      return find_base_value (XEXP (src, 1));

    case AND:
      /* If the second operand is constant set the base
	 address to the first operand.  */
      if (CONST_INT_P (XEXP (src, 1)) && INTVAL (XEXP (src, 1)) != 0)
	return find_base_value (XEXP (src, 0));
      return 0;

    case TRUNCATE:
      /* As we do not know which address space the pointer is refering to, we can
	 handle this only if the target does not support different pointer or
	 address modes depending on the address space.  */
      if (!target_default_pointer_address_modes_p ())
	break;
      if (GET_MODE_SIZE (GET_MODE (src)) < GET_MODE_SIZE (Pmode))
	break;
      /* Fall through.  */
    case HIGH:
    case PRE_INC:
    case PRE_DEC:
    case POST_INC:
    case POST_DEC:
    case PRE_MODIFY:
    case POST_MODIFY:
      return find_base_value (XEXP (src, 0));

    case ZERO_EXTEND:
    case SIGN_EXTEND:	/* used for NT/Alpha pointers */
      /* As we do not know which address space the pointer is refering to, we can
	 handle this only if the target does not support different pointer or
	 address modes depending on the address space.  */
      if (!target_default_pointer_address_modes_p ())
	break;

      {
	rtx temp = find_base_value (XEXP (src, 0));

	if (temp != 0 && CONSTANT_P (temp))
	  temp = convert_memory_address (Pmode, temp);

	return temp;
      }

    default:
      break;
    }

  return 0;
}

/* Called from init_alias_analysis indirectly through note_stores.  */

/* While scanning insns to find base values, reg_seen[N] is nonzero if
   register N has been set in this function.  */
static char *reg_seen;

/* Addresses which are known not to alias anything else are identified
   by a unique integer.  */
static int unique_id;

static void
record_set (rtx dest, const_rtx set, void *data ATTRIBUTE_UNUSED)
{
  unsigned regno;
  rtx src;
  int n;

  if (!REG_P (dest))
    return;

  regno = REGNO (dest);

  gcc_checking_assert (regno < VEC_length (rtx, reg_base_value));

  /* If this spans multiple hard registers, then we must indicate that every
     register has an unusable value.  */
  if (regno < FIRST_PSEUDO_REGISTER)
    n = hard_regno_nregs[regno][GET_MODE (dest)];
  else
    n = 1;
  if (n != 1)
    {
      while (--n >= 0)
	{
	  reg_seen[regno + n] = 1;
	  new_reg_base_value[regno + n] = 0;
	}
      return;
    }

  if (set)
    {
      /* A CLOBBER wipes out any old value but does not prevent a previously
	 unset register from acquiring a base address (i.e. reg_seen is not
	 set).  */
      if (GET_CODE (set) == CLOBBER)
	{
	  new_reg_base_value[regno] = 0;
	  return;
	}
      src = SET_SRC (set);
    }
  else
    {
      if (reg_seen[regno])
	{
	  new_reg_base_value[regno] = 0;
	  return;
	}
      reg_seen[regno] = 1;
      new_reg_base_value[regno] = gen_rtx_ADDRESS (Pmode,
						   GEN_INT (unique_id++));
      return;
    }

  /* If this is not the first set of REGNO, see whether the new value
     is related to the old one.  There are two cases of interest:

	(1) The register might be assigned an entirely new value
	    that has the same base term as the original set.

	(2) The set might be a simple self-modification that
	    cannot change REGNO's base value.

     If neither case holds, reject the original base value as invalid.
     Note that the following situation is not detected:

	 extern int x, y;  int *p = &x; p += (&y-&x);

     ANSI C does not allow computing the difference of addresses
     of distinct top level objects.  */
  if (new_reg_base_value[regno] != 0
      && find_base_value (src) != new_reg_base_value[regno])
    switch (GET_CODE (src))
      {
      case LO_SUM:
      case MINUS:
	if (XEXP (src, 0) != dest && XEXP (src, 1) != dest)
	  new_reg_base_value[regno] = 0;
	break;
      case PLUS:
	/* If the value we add in the PLUS is also a valid base value,
	   this might be the actual base value, and the original value
	   an index.  */
	{
	  rtx other = NULL_RTX;

	  if (XEXP (src, 0) == dest)
	    other = XEXP (src, 1);
	  else if (XEXP (src, 1) == dest)
	    other = XEXP (src, 0);

	  if (! other || find_base_value (other))
	    new_reg_base_value[regno] = 0;
	  break;
	}
      case AND:
	if (XEXP (src, 0) != dest || !CONST_INT_P (XEXP (src, 1)))
	  new_reg_base_value[regno] = 0;
	break;
      default:
	new_reg_base_value[regno] = 0;
	break;
      }
  /* If this is the first set of a register, record the value.  */
  else if ((regno >= FIRST_PSEUDO_REGISTER || ! fixed_regs[regno])
	   && ! reg_seen[regno] && new_reg_base_value[regno] == 0)
    new_reg_base_value[regno] = find_base_value (src);

  reg_seen[regno] = 1;
}

/* Return REG_BASE_VALUE for REGNO.  Selective scheduler uses this to avoid
   using hard registers with non-null REG_BASE_VALUE for renaming.  */
rtx
get_reg_base_value (unsigned int regno)
{
  return VEC_index (rtx, reg_base_value, regno);
}

/* If a value is known for REGNO, return it.  */

rtx
get_reg_known_value (unsigned int regno)
{
  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      regno -= FIRST_PSEUDO_REGISTER;
      if (regno < reg_known_value_size)
	return reg_known_value[regno];
    }
  return NULL;
}

/* Set it.  */

static void
set_reg_known_value (unsigned int regno, rtx val)
{
  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      regno -= FIRST_PSEUDO_REGISTER;
      if (regno < reg_known_value_size)
	reg_known_value[regno] = val;
    }
}

/* Similarly for reg_known_equiv_p.  */

bool
get_reg_known_equiv_p (unsigned int regno)
{
  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      regno -= FIRST_PSEUDO_REGISTER;
      if (regno < reg_known_value_size)
	return reg_known_equiv_p[regno];
    }
  return false;
}

static void
set_reg_known_equiv_p (unsigned int regno, bool val)
{
  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      regno -= FIRST_PSEUDO_REGISTER;
      if (regno < reg_known_value_size)
	reg_known_equiv_p[regno] = val;
    }
}


/* Returns a canonical version of X, from the point of view alias
   analysis.  (For example, if X is a MEM whose address is a register,
   and the register has a known value (say a SYMBOL_REF), then a MEM
   whose address is the SYMBOL_REF is returned.)  */

rtx
canon_rtx (rtx x)
{
  /* Recursively look for equivalences.  */
  if (REG_P (x) && REGNO (x) >= FIRST_PSEUDO_REGISTER)
    {
      rtx t = get_reg_known_value (REGNO (x));
      if (t == x)
	return x;
      if (t)
	return canon_rtx (t);
    }

  if (GET_CODE (x) == PLUS)
    {
      rtx x0 = canon_rtx (XEXP (x, 0));
      rtx x1 = canon_rtx (XEXP (x, 1));

      if (x0 != XEXP (x, 0) || x1 != XEXP (x, 1))
	{
	  if (CONST_INT_P (x0))
	    return plus_constant (x1, INTVAL (x0));
	  else if (CONST_INT_P (x1))
	    return plus_constant (x0, INTVAL (x1));
	  return gen_rtx_PLUS (GET_MODE (x), x0, x1);
	}
    }

  /* This gives us much better alias analysis when called from
     the loop optimizer.   Note we want to leave the original
     MEM alone, but need to return the canonicalized MEM with
     all the flags with their original values.  */
  else if (MEM_P (x))
    x = replace_equiv_address_nv (x, canon_rtx (XEXP (x, 0)));

  return x;
}

/* Return 1 if X and Y are identical-looking rtx's.
   Expect that X and Y has been already canonicalized.

   We use the data in reg_known_value above to see if two registers with
   different numbers are, in fact, equivalent.  */

static int
rtx_equal_for_memref_p (const_rtx x, const_rtx y)
{
  int i;
  int j;
  enum rtx_code code;
  const char *fmt;

  if (x == 0 && y == 0)
    return 1;
  if (x == 0 || y == 0)
    return 0;

  if (x == y)
    return 1;

  code = GET_CODE (x);
  /* Rtx's of different codes cannot be equal.  */
  if (code != GET_CODE (y))
    return 0;

  /* (MULT:SI x y) and (MULT:HI x y) are NOT equivalent.
     (REG:SI x) and (REG:HI x) are NOT equivalent.  */

  if (GET_MODE (x) != GET_MODE (y))
    return 0;

  /* Some RTL can be compared without a recursive examination.  */
  switch (code)
    {
    case REG:
      return REGNO (x) == REGNO (y);

    case LABEL_REF:
      return XEXP (x, 0) == XEXP (y, 0);

    case SYMBOL_REF:
      return XSTR (x, 0) == XSTR (y, 0);

    case VALUE:
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_FIXED:
      /* There's no need to compare the contents of CONST_DOUBLEs or
	 CONST_INTs because pointer equality is a good enough
	 comparison for these nodes.  */
      return 0;

    default:
      break;
    }

  /* canon_rtx knows how to handle plus.  No need to canonicalize.  */
  if (code == PLUS)
    return ((rtx_equal_for_memref_p (XEXP (x, 0), XEXP (y, 0))
	     && rtx_equal_for_memref_p (XEXP (x, 1), XEXP (y, 1)))
	    || (rtx_equal_for_memref_p (XEXP (x, 0), XEXP (y, 1))
		&& rtx_equal_for_memref_p (XEXP (x, 1), XEXP (y, 0))));
  /* For commutative operations, the RTX match if the operand match in any
     order.  Also handle the simple binary and unary cases without a loop.  */
  if (COMMUTATIVE_P (x))
    {
      rtx xop0 = canon_rtx (XEXP (x, 0));
      rtx yop0 = canon_rtx (XEXP (y, 0));
      rtx yop1 = canon_rtx (XEXP (y, 1));

      return ((rtx_equal_for_memref_p (xop0, yop0)
	       && rtx_equal_for_memref_p (canon_rtx (XEXP (x, 1)), yop1))
	      || (rtx_equal_for_memref_p (xop0, yop1)
		  && rtx_equal_for_memref_p (canon_rtx (XEXP (x, 1)), yop0)));
    }
  else if (NON_COMMUTATIVE_P (x))
    {
      return (rtx_equal_for_memref_p (canon_rtx (XEXP (x, 0)),
				      canon_rtx (XEXP (y, 0)))
	      && rtx_equal_for_memref_p (canon_rtx (XEXP (x, 1)),
					 canon_rtx (XEXP (y, 1))));
    }
  else if (UNARY_P (x))
    return rtx_equal_for_memref_p (canon_rtx (XEXP (x, 0)),
				   canon_rtx (XEXP (y, 0)));

  /* Compare the elements.  If any pair of corresponding elements
     fail to match, return 0 for the whole things.

     Limit cases to types which actually appear in addresses.  */

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      switch (fmt[i])
	{
	case 'i':
	  if (XINT (x, i) != XINT (y, i))
	    return 0;
	  break;

	case 'E':
	  /* Two vectors must have the same length.  */
	  if (XVECLEN (x, i) != XVECLEN (y, i))
	    return 0;

	  /* And the corresponding elements must match.  */
	  for (j = 0; j < XVECLEN (x, i); j++)
	    if (rtx_equal_for_memref_p (canon_rtx (XVECEXP (x, i, j)),
					canon_rtx (XVECEXP (y, i, j))) == 0)
	      return 0;
	  break;

	case 'e':
	  if (rtx_equal_for_memref_p (canon_rtx (XEXP (x, i)),
				      canon_rtx (XEXP (y, i))) == 0)
	    return 0;
	  break;

	  /* This can happen for asm operands.  */
	case 's':
	  if (strcmp (XSTR (x, i), XSTR (y, i)))
	    return 0;
	  break;

	/* This can happen for an asm which clobbers memory.  */
	case '0':
	  break;

	  /* It is believed that rtx's at this level will never
	     contain anything but integers and other rtx's,
	     except for within LABEL_REFs and SYMBOL_REFs.  */
	default:
	  gcc_unreachable ();
	}
    }
  return 1;
}

rtx
find_base_term (rtx x)
{
  cselib_val *val;
  struct elt_loc_list *l;

#if defined (FIND_BASE_TERM)
  /* Try machine-dependent ways to find the base term.  */
  x = FIND_BASE_TERM (x);
#endif

  switch (GET_CODE (x))
    {
    case REG:
      return REG_BASE_VALUE (x);

    case TRUNCATE:
      /* As we do not know which address space the pointer is refering to, we can
	 handle this only if the target does not support different pointer or
	 address modes depending on the address space.  */
      if (!target_default_pointer_address_modes_p ())
	return 0;
      if (GET_MODE_SIZE (GET_MODE (x)) < GET_MODE_SIZE (Pmode))
	return 0;
      /* Fall through.  */
    case HIGH:
    case PRE_INC:
    case PRE_DEC:
    case POST_INC:
    case POST_DEC:
    case PRE_MODIFY:
    case POST_MODIFY:
      return find_base_term (XEXP (x, 0));

    case ZERO_EXTEND:
    case SIGN_EXTEND:	/* Used for Alpha/NT pointers */
      /* As we do not know which address space the pointer is refering to, we can
	 handle this only if the target does not support different pointer or
	 address modes depending on the address space.  */
      if (!target_default_pointer_address_modes_p ())
	return 0;

      {
	rtx temp = find_base_term (XEXP (x, 0));

	if (temp != 0 && CONSTANT_P (temp))
	  temp = convert_memory_address (Pmode, temp);

	return temp;
      }

    case VALUE:
      val = CSELIB_VAL_PTR (x);
      if (!val)
	return 0;
      for (l = val->locs; l; l = l->next)
	if ((x = find_base_term (l->loc)) != 0)
	  return x;
      return 0;

    case LO_SUM:
      /* The standard form is (lo_sum reg sym) so look only at the
         second operand.  */
      return find_base_term (XEXP (x, 1));

    case CONST:
      x = XEXP (x, 0);
      if (GET_CODE (x) != PLUS && GET_CODE (x) != MINUS)
	return 0;
      /* Fall through.  */
    case PLUS:
    case MINUS:
      {
	rtx tmp1 = XEXP (x, 0);
	rtx tmp2 = XEXP (x, 1);

	/* This is a little bit tricky since we have to determine which of
	   the two operands represents the real base address.  Otherwise this
	   routine may return the index register instead of the base register.

	   That may cause us to believe no aliasing was possible, when in
	   fact aliasing is possible.

	   We use a few simple tests to guess the base register.  Additional
	   tests can certainly be added.  For example, if one of the operands
	   is a shift or multiply, then it must be the index register and the
	   other operand is the base register.  */

	if (tmp1 == pic_offset_table_rtx && CONSTANT_P (tmp2))
	  return find_base_term (tmp2);

	/* If either operand is known to be a pointer, then use it
	   to determine the base term.  */
	if (REG_P (tmp1) && REG_POINTER (tmp1))
	  {
	    rtx base = find_base_term (tmp1);
	    if (base)
	      return base;
	  }

	if (REG_P (tmp2) && REG_POINTER (tmp2))
	  {
	    rtx base = find_base_term (tmp2);
	    if (base)
	      return base;
	  }

	/* Neither operand was known to be a pointer.  Go ahead and find the
	   base term for both operands.  */
	tmp1 = find_base_term (tmp1);
	tmp2 = find_base_term (tmp2);

	/* If either base term is named object or a special address
	   (like an argument or stack reference), then use it for the
	   base term.  */
	if (tmp1 != 0
	    && (GET_CODE (tmp1) == SYMBOL_REF
		|| GET_CODE (tmp1) == LABEL_REF
		|| (GET_CODE (tmp1) == ADDRESS
		    && GET_MODE (tmp1) != VOIDmode)))
	  return tmp1;

	if (tmp2 != 0
	    && (GET_CODE (tmp2) == SYMBOL_REF
		|| GET_CODE (tmp2) == LABEL_REF
		|| (GET_CODE (tmp2) == ADDRESS
		    && GET_MODE (tmp2) != VOIDmode)))
	  return tmp2;

	/* We could not determine which of the two operands was the
	   base register and which was the index.  So we can determine
	   nothing from the base alias check.  */
	return 0;
      }

    case AND:
      if (CONST_INT_P (XEXP (x, 1)) && INTVAL (XEXP (x, 1)) != 0)
	return find_base_term (XEXP (x, 0));
      return 0;

    case SYMBOL_REF:
    case LABEL_REF:
      return x;

    default:
      return 0;
    }
}

/* Convert the address X into something we can use.  This is done by returning
   it unchanged unless it is a value; in the latter case we call cselib to get
   a more useful rtx.  */

rtx
get_addr (rtx x)
{
  cselib_val *v;
  struct elt_loc_list *l;

  if (GET_CODE (x) != VALUE)
    return x;
  v = CSELIB_VAL_PTR (x);
  if (v)
    {
      for (l = v->locs; l; l = l->next)
	if (CONSTANT_P (l->loc))
	  return l->loc;
      for (l = v->locs; l; l = l->next)
	if (!REG_P (l->loc) && !MEM_P (l->loc))
	  return l->loc;
      if (v->locs)
	return v->locs->loc;
    }
  return x;
}

/* Functions to compute memory dependencies.

   Since we process the insns in execution order, we can build tables
   to keep track of what registers are fixed (and not aliased), what registers
   are varying in known ways, and what registers are varying in unknown
   ways.

   If both memory references are volatile, then there must always be a
   dependence between the two references, since their order can not be
   changed.  A volatile and non-volatile reference can be interchanged
   though.

   A MEM_IN_STRUCT reference at a non-AND varying address can never
   conflict with a non-MEM_IN_STRUCT reference at a fixed address.  We
   also must allow AND addresses, because they may generate accesses
   outside the object being referenced.  This is used to generate
   aligned addresses from unaligned addresses, for instance, the alpha
   storeqi_unaligned pattern.  */

/* Read dependence: X is read after read in MEM takes place.  There can
   only be a dependence here if both reads are volatile.  */

int
read_dependence (const_rtx mem, const_rtx x)
{
  return 1;
}

/* Return nonzero if we can determine the exprs corresponding to memrefs
   X and Y and they do not overlap. 
   If LOOP_VARIANT is set, skip offset-based disambiguation */

int
nonoverlapping_memrefs_p (const_rtx x, const_rtx y, bool loop_invariant)
{
  return 0;
}

/* True dependence: X is read after store in MEM takes place.  */

int
true_dependence (const_rtx mem, enum machine_mode mem_mode, const_rtx x,
		 bool (*varies) (const_rtx, bool))
{
  return 1;
}

/* Canonical true dependence: X is read after store in MEM takes place.
   Variant of true_dependence which assumes MEM has already been
   canonicalized (hence we no longer do that here).
   The mem_addr argument has been added, since true_dependence_1 computed
   this value prior to canonicalizing.  */

int
canon_true_dependence (const_rtx mem, enum machine_mode mem_mode, rtx mem_addr,
		       const_rtx x, rtx x_addr, bool (*varies) (const_rtx, bool))
{
  return 1;
}

/* Anti dependence: X is written after read in MEM takes place.  */

int
anti_dependence (const_rtx mem, const_rtx x)
{
  return 1;
}

/* Output dependence: X is written after store in MEM takes place.  */

int
output_dependence (const_rtx mem, const_rtx x)
{
  return 1;
}

/* Check whether X may be aliased with MEM.  Don't do offset-based
  memory disambiguation & TBAA.  */
int
may_alias_p (const_rtx mem, const_rtx x)
{
  return 1;
}

void
init_alias_target (void)
{
  int i;

  memset (static_reg_base_value, 0, sizeof static_reg_base_value);

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    /* Check whether this register can hold an incoming pointer
       argument.  FUNCTION_ARG_REGNO_P tests outgoing register
       numbers, so translate if necessary due to register windows.  */
    if (FUNCTION_ARG_REGNO_P (OUTGOING_REGNO (i))
	&& HARD_REGNO_MODE_OK (i, Pmode))
      static_reg_base_value[i]
	= gen_rtx_ADDRESS (VOIDmode, gen_rtx_REG (Pmode, i));

  static_reg_base_value[STACK_POINTER_REGNUM]
    = gen_rtx_ADDRESS (Pmode, stack_pointer_rtx);
  static_reg_base_value[ARG_POINTER_REGNUM]
    = gen_rtx_ADDRESS (Pmode, arg_pointer_rtx);
  static_reg_base_value[FRAME_POINTER_REGNUM]
    = gen_rtx_ADDRESS (Pmode, frame_pointer_rtx);
#if !HARD_FRAME_POINTER_IS_FRAME_POINTER
  static_reg_base_value[HARD_FRAME_POINTER_REGNUM]
    = gen_rtx_ADDRESS (Pmode, hard_frame_pointer_rtx);
#endif
}

/* Set MEMORY_MODIFIED when X modifies DATA (that is assumed
   to be memory reference.  */
static bool memory_modified;
static void
memory_modified_1 (rtx x, const_rtx pat ATTRIBUTE_UNUSED, void *data)
{
  if (MEM_P (x))
    {
      if (anti_dependence (x, (const_rtx)data) || output_dependence (x, (const_rtx)data))
	memory_modified = true;
    }
}


/* Return true when INSN possibly modify memory contents of MEM
   (i.e. address can be modified).  */
bool
memory_modified_in_insn_p (const_rtx mem, const_rtx insn)
{
  if (!INSN_P (insn))
    return false;
  memory_modified = false;
  note_stores (PATTERN (insn), memory_modified_1, CONST_CAST_RTX(mem));
  return memory_modified;
}

/* Initialize the aliasing machinery.  Initialize the REG_KNOWN_VALUE
   array.  */

void
init_alias_analysis (void)
{
  unsigned int maxreg = max_reg_num ();
  int changed, pass;
  int i;
  unsigned int ui;
  rtx insn;

  timevar_push (TV_ALIAS_ANALYSIS);

  reg_known_value_size = maxreg - FIRST_PSEUDO_REGISTER;
  reg_known_value = ggc_alloc_cleared_vec_rtx (reg_known_value_size);
  reg_known_equiv_p = XCNEWVEC (bool, reg_known_value_size);

  /* If we have memory allocated from the previous run, use it.  */
  if (old_reg_base_value)
    reg_base_value = old_reg_base_value;

  if (reg_base_value)
    VEC_truncate (rtx, reg_base_value, 0);

  VEC_safe_grow_cleared (rtx, gc, reg_base_value, maxreg);

  new_reg_base_value = XNEWVEC (rtx, maxreg);
  reg_seen = XNEWVEC (char, maxreg);

  /* The basic idea is that each pass through this loop will use the
     "constant" information from the previous pass to propagate alias
     information through another level of assignments.

     This could get expensive if the assignment chains are long.  Maybe
     we should throttle the number of iterations, possibly based on
     the optimization level or flag_expensive_optimizations.

     We could propagate more information in the first pass by making use
     of DF_REG_DEF_COUNT to determine immediately that the alias information
     for a pseudo is "constant".

     A program with an uninitialized variable can cause an infinite loop
     here.  Instead of doing a full dataflow analysis to detect such problems
     we just cap the number of iterations for the loop.

     The state of the arrays for the set chain in question does not matter
     since the program has undefined behavior.  */

  pass = 0;
  do
    {
      /* Assume nothing will change this iteration of the loop.  */
      changed = 0;

      /* We want to assign the same IDs each iteration of this loop, so
	 start counting from zero each iteration of the loop.  */
      unique_id = 0;

      /* We're at the start of the function each iteration through the
	 loop, so we're copying arguments.  */
      copying_arguments = true;

      /* Wipe the potential alias information clean for this pass.  */
      memset (new_reg_base_value, 0, maxreg * sizeof (rtx));

      /* Wipe the reg_seen array clean.  */
      memset (reg_seen, 0, maxreg);

      /* Mark all hard registers which may contain an address.
	 The stack, frame and argument pointers may contain an address.
	 An argument register which can hold a Pmode value may contain
	 an address even if it is not in BASE_REGS.

	 The address expression is VOIDmode for an argument and
	 Pmode for other registers.  */

      memcpy (new_reg_base_value, static_reg_base_value,
	      FIRST_PSEUDO_REGISTER * sizeof (rtx));

      /* Walk the insns adding values to the new_reg_base_value array.  */
      for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
	{
	  if (INSN_P (insn))
	    {
	      rtx note, set;

#if defined (HAVE_prologue) || defined (HAVE_epilogue)
	      /* The prologue/epilogue insns are not threaded onto the
		 insn chain until after reload has completed.  Thus,
		 there is no sense wasting time checking if INSN is in
		 the prologue/epilogue until after reload has completed.  */
	      if (reload_completed
		  && prologue_epilogue_contains (insn))
		continue;
#endif

	      /* If this insn has a noalias note, process it,  Otherwise,
		 scan for sets.  A simple set will have no side effects
		 which could change the base value of any other register.  */

	      if (GET_CODE (PATTERN (insn)) == SET
		  && REG_NOTES (insn) != 0
		  && find_reg_note (insn, REG_NOALIAS, NULL_RTX))
		record_set (SET_DEST (PATTERN (insn)), NULL_RTX, NULL);
	      else
		note_stores (PATTERN (insn), record_set, NULL);

	      set = single_set (insn);

	      if (set != 0
		  && REG_P (SET_DEST (set))
		  && REGNO (SET_DEST (set)) >= FIRST_PSEUDO_REGISTER)
		{
		  unsigned int regno = REGNO (SET_DEST (set));
		  rtx src = SET_SRC (set);
		  rtx t;

		  note = find_reg_equal_equiv_note (insn);
		  if (note && REG_NOTE_KIND (note) == REG_EQUAL
		      && DF_REG_DEF_COUNT (regno) != 1)
		    note = NULL_RTX;

		  if (note != NULL_RTX
		      && GET_CODE (XEXP (note, 0)) != EXPR_LIST
		      && ! rtx_varies_p (XEXP (note, 0), 1)
		      && ! reg_overlap_mentioned_p (SET_DEST (set),
						    XEXP (note, 0)))
		    {
		      set_reg_known_value (regno, XEXP (note, 0));
		      set_reg_known_equiv_p (regno,
			REG_NOTE_KIND (note) == REG_EQUIV);
		    }
		  else if (DF_REG_DEF_COUNT (regno) == 1
			   && GET_CODE (src) == PLUS
			   && REG_P (XEXP (src, 0))
			   && (t = get_reg_known_value (REGNO (XEXP (src, 0))))
			   && CONST_INT_P (XEXP (src, 1)))
		    {
		      t = plus_constant (t, INTVAL (XEXP (src, 1)));
		      set_reg_known_value (regno, t);
		      set_reg_known_equiv_p (regno, 0);
		    }
		  else if (DF_REG_DEF_COUNT (regno) == 1
			   && ! rtx_varies_p (src, 1))
		    {
		      set_reg_known_value (regno, src);
		      set_reg_known_equiv_p (regno, 0);
		    }
		}
	    }
	  else if (NOTE_P (insn)
		   && NOTE_KIND (insn) == NOTE_INSN_FUNCTION_BEG)
	    copying_arguments = false;
	}

      /* Now propagate values from new_reg_base_value to reg_base_value.  */
      gcc_assert (maxreg == (unsigned int) max_reg_num ());

      for (ui = 0; ui < maxreg; ui++)
	{
	  if (new_reg_base_value[ui]
	      && new_reg_base_value[ui] != VEC_index (rtx, reg_base_value, ui)
	      && ! rtx_equal_p (new_reg_base_value[ui],
				VEC_index (rtx, reg_base_value, ui)))
	    {
	      VEC_replace (rtx, reg_base_value, ui, new_reg_base_value[ui]);
	      changed = 1;
	    }
	}
    }
  while (changed && ++pass < MAX_ALIAS_LOOP_PASSES);

  /* Fill in the remaining entries.  */
  for (i = 0; i < (int)reg_known_value_size; i++)
    if (reg_known_value[i] == 0)
      reg_known_value[i] = regno_reg_rtx[i + FIRST_PSEUDO_REGISTER];

  /* Clean up.  */
  free (new_reg_base_value);
  new_reg_base_value = 0;
  free (reg_seen);
  reg_seen = 0;
  timevar_pop (TV_ALIAS_ANALYSIS);
}

/* Equate REG_BASE_VALUE (reg1) to REG_BASE_VALUE (reg2).
   Special API for var-tracking pass purposes.  */

void
vt_equate_reg_base_value (const_rtx reg1, const_rtx reg2)
{
  VEC_replace (rtx, reg_base_value, REGNO (reg1), REG_BASE_VALUE (reg2));
}

void
end_alias_analysis (void)
{
  old_reg_base_value = reg_base_value;
  ggc_free (reg_known_value);
  reg_known_value = 0;
  reg_known_value_size = 0;
  free (reg_known_equiv_p);
  reg_known_equiv_p = 0;
}

EXTERN_C_END

#include "gt-alias.h"
