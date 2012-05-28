/* Modula-3: modified */

/* Scalar Replacement of Aggregates (SRA) converts some structure
   references into scalar references, exposing them to the scalar
   optimizers.
   Copyright (C) 2008, 2009, 2010 Free Software Foundation, Inc.
   Contributed by Martin Jambor <mjambor@suse.cz>

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

/* This file implements Scalar Reduction of Aggregates (SRA).  SRA is run
   twice, once in the early stages of compilation (early SRA) and once in the
   late stages (late SRA).  The aim of both is to turn references to scalar
   parts of aggregates into uses of independent scalar variables.

   The two passes are nearly identical, the only difference is that early SRA
   does not scalarize unions which are used as the result in a GIMPLE_RETURN
   statement because together with inlining this can lead to weird type
   conversions.

   Both passes operate in four stages:

   1. The declarations that have properties which make them candidates for
      scalarization are identified in function find_var_candidates().  The
      candidates are stored in candidate_bitmap.

   2. The function body is scanned.  In the process, declarations which are
      used in a manner that prevent their scalarization are removed from the
      candidate bitmap.  More importantly, for every access into an aggregate,
      an access structure (struct access) is created by create_access() and
      stored in a vector associated with the aggregate.  Among other
      information, the aggregate declaration, the offset and size of the access
      and its type are stored in the structure.

      On a related note, assign_link structures are created for every assign
      statement between candidate aggregates and attached to the related
      accesses.

   3. The vectors of accesses are analyzed.  They are first sorted according to
      their offset and size and then scanned for partially overlapping accesses
      (i.e. those which overlap but one is not entirely within another).  Such
      an access disqualifies the whole aggregate from being scalarized.

      If there is no such inhibiting overlap, a representative access structure
      is chosen for every unique combination of offset and size.  Afterwards,
      the pass builds a set of trees from these structures, in which children
      of an access are within their parent (in terms of offset and size).

      Then accesses  are propagated  whenever possible (i.e.  in cases  when it
      does not create a partially overlapping access) across assign_links from
      the right hand side to the left hand side.

      Then the set of trees for each declaration is traversed again and those
      accesses which should be replaced by a scalar are identified.

   4. The function is traversed again, and for every reference into an
      aggregate that has some component which is about to be scalarized,
      statements are amended and new statements are created as necessary.
      Finally, if a parameter got scalarized, the scalar replacements are
      initialized with values from respective parameter aggregates.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "alloc-pool.h"
#include "tm.h"
#include "tree.h"
#include "gimple.h"
#include "cgraph.h"
#include "tree-flow.h"
#include "ipa-prop.h"
#include "tree-pretty-print.h"
#include "statistics.h"
#include "tree-dump.h"
#include "timevar.h"
#include "params.h"
#include "target.h"
#include "flags.h"
#include "dbgcnt.h"
#include "tree-inline.h"
#include "gimple-pretty-print.h"

EXTERN_C_START

/* Enumeration of all aggregate reductions we can do.  */
enum sra_mode { SRA_MODE_EARLY_IPA,   /* early call regularization */
		SRA_MODE_EARLY_INTRA, /* early intraprocedural SRA */
		SRA_MODE_INTRA };     /* late intraprocedural SRA */

/* Global variable describing which aggregate reduction we are performing at
   the moment.  */
static enum sra_mode sra_mode;

struct assign_link;

/* ACCESS represents each access to an aggregate variable (as a whole or a
   part).  It can also represent a group of accesses that refer to exactly the
   same fragment of an aggregate (i.e. those that have exactly the same offset
   and size).  Such representatives for a single aggregate, once determined,
   are linked in a linked list and have the group fields set.

   Moreover, when doing intraprocedural SRA, a tree is built from those
   representatives (by the means of first_child and next_sibling pointers), in
   which all items in a subtree are "within" the root, i.e. their offset is
   greater or equal to offset of the root and offset+size is smaller or equal
   to offset+size of the root.  Children of an access are sorted by offset.

   Note that accesses to parts of vector and complex number types always
   represented by an access to the whole complex number or a vector.  It is a
   duty of the modifying functions to replace them appropriately.  */

struct access
{
  /* Values returned by  `get_ref_base_and_extent' for each component reference
     If EXPR isn't a component reference  just set `BASE = EXPR', `OFFSET = 0',
     `SIZE = TREE_SIZE (TREE_TYPE (expr))'.  */
  HOST_WIDE_INT offset;
  HOST_WIDE_INT size;
  tree base;

  /* Expression.  It is context dependent so do not use it to create new
     expressions to access the original aggregate.  See PR 42154 for a
     testcase.  */
  tree expr;
  /* Type.  */
  tree type;

  /* The statement this access belongs to.  */
  gimple stmt;

  /* Next group representative for this aggregate. */
  struct access *next_grp;

  /* Pointer to the group representative.  Pointer to itself if the struct is
     the representative.  */
  struct access *group_representative;

  /* If this access has any children (in terms of the definition above), this
     points to the first one.  */
  struct access *first_child;

  /* In intraprocedural SRA, pointer to the next sibling in the access tree as
     described above.  In IPA-SRA this is a pointer to the next access
     belonging to the same group (having the same representative).  */
  struct access *next_sibling;

  /* Pointers to the first and last element in the linked list of assign
     links.  */
  struct assign_link *first_link, *last_link;

  /* Pointer to the next access in the work queue.  */
  struct access *next_queued;

  /* Replacement variable for this access "region."  Never to be accessed
     directly, always only by the means of get_access_replacement() and only
     when grp_to_be_replaced flag is set.  */
  tree replacement_decl;

  /* Is this particular access write access? */
  unsigned write : 1;

  /* Is this access an artificial one created to scalarize some record
     entirely? */
  unsigned total_scalarization : 1;

  /* Is this access an access to a non-addressable field? */
  unsigned non_addressable : 1;

  /* Is this access currently in the work queue?  */
  unsigned grp_queued : 1;

  /* Does this group contain a write access?  This flag is propagated down the
     access tree.  */
  unsigned grp_write : 1;

  /* Does this group contain a read access?  This flag is propagated down the
     access tree.  */
  unsigned grp_read : 1;

  /* Does this group contain a read access that comes from an assignment
     statement?  This flag is propagated down the access tree.  */
  unsigned grp_assignment_read : 1;

  /* Does this group contain a write access that comes from an assignment
     statement?  This flag is propagated down the access tree.  */
  unsigned grp_assignment_write : 1;

  /* Does this group contain a read access through a scalar type?  This flag is
     not propagated in the access tree in any direction.  */
  unsigned grp_scalar_read : 1;

  /* Does this group contain a write access through a scalar type?  This flag
     is not propagated in the access tree in any direction.  */
  unsigned grp_scalar_write : 1;

  /* Other passes of the analysis use this bit to make function
     analyze_access_subtree create scalar replacements for this group if
     possible.  */
  unsigned grp_hint : 1;

  /* Is the subtree rooted in this access fully covered by scalar
     replacements?  */
  unsigned grp_covered : 1;

  /* If set to true, this access and all below it in an access tree must not be
     scalarized.  */
  unsigned grp_unscalarizable_region : 1;

  /* Whether data have been written to parts of the aggregate covered by this
     access which is not to be scalarized.  This flag is propagated up in the
     access tree.  */
  unsigned grp_unscalarized_data : 1;

  /* Does this access and/or group contain a write access through a
     BIT_FIELD_REF?  */
  unsigned grp_partial_lhs : 1;

  /* Set when a scalar replacement should be created for this variable.  We do
     the decision and creation at different places because create_tmp_var
     cannot be called from within FOR_EACH_REFERENCED_VAR. */
  unsigned grp_to_be_replaced : 1;

  /* Should TREE_NO_WARNING of a replacement be set?  */
  unsigned grp_no_warning : 1;

  /* Is it possible that the group refers to data which might be (directly or
     otherwise) modified?  */
  unsigned grp_maybe_modified : 1;

  /* Set when this is a representative of a pointer to scalar (i.e. by
     reference) parameter which we consider for turning into a plain scalar
     (i.e. a by value parameter).  */
  unsigned grp_scalar_ptr : 1;

  /* Set when we discover that this pointer is not safe to dereference in the
     caller.  */
  unsigned grp_not_necessarilly_dereferenced : 1;
};

typedef struct access *access_p;

DEF_VEC_P (access_p);
DEF_VEC_ALLOC_P (access_p, heap);

/* Alloc pool for allocating access structures.  */
static alloc_pool access_pool;

/* A structure linking lhs and rhs accesses from an aggregate assignment.  They
   are used to propagate subaccesses from rhs to lhs as long as they don't
   conflict with what is already there.  */
struct assign_link
{
  struct access *lacc, *racc;
  struct assign_link *next;
};

/* Alloc pool for allocating assign link structures.  */
static alloc_pool link_pool;

/* Base (tree) -> Vector (VEC(access_p,heap) *) map.  */
static struct pointer_map_t *base_access_vec;

/* Bitmap of candidates.  */
static bitmap candidate_bitmap;

/* Bitmap of candidates which we should try to entirely scalarize away and
   those which cannot be (because they are and need be used as a whole).  */
static bitmap should_scalarize_away_bitmap, cannot_scalarize_away_bitmap;

/* Obstack for creation of fancy names.  */
static struct obstack name_obstack;

/* Head of a linked list of accesses that need to have its subaccesses
   propagated to their assignment counterparts. */
static struct access *work_queue_head;

/* Number of parameters of the analyzed function when doing early ipa SRA.  */
static int func_param_count;

/* scan_function sets the following to true if it encounters a call to
   __builtin_apply_args.  */
static bool encountered_apply_args;

/* Set by scan_function when it finds a recursive call.  */
static bool encountered_recursive_call;

/* Set by scan_function when it finds a recursive call with less actual
   arguments than formal parameters..  */
static bool encountered_unchangable_recursive_call;

/* This is a table in which for each basic block and parameter there is a
   distance (offset + size) in that parameter which is dereferenced and
   accessed in that BB.  */
static HOST_WIDE_INT *bb_dereferences;
/* Bitmap of BBs that can cause the function to "stop" progressing by
   returning, throwing externally, looping infinitely or calling a function
   which might abort etc.. */
static bitmap final_bbs;

/* Representative of no accesses at all. */
static struct access  no_accesses_representant;

/* Predicate to test the special value.  */

static inline bool
no_accesses_p (struct access *access)
{
  return access == &no_accesses_representant;
}

/* Dump contents of ACCESS to file F in a human friendly way.  If GRP is true,
   representative fields are dumped, otherwise those which only describe the
   individual access are.  */

static struct
{
  /* Number of processed aggregates is readily available in
     analyze_all_variable_accesses and so is not stored here.  */

  /* Number of created scalar replacements.  */
  int replacements;

  /* Number of times sra_modify_expr or sra_modify_assign themselves changed an
     expression.  */
  int exprs;

  /* Number of statements created by generate_subtree_copies.  */
  int subtree_copies;

  /* Number of statements created by load_assign_lhs_subreplacements.  */
  int subreplacements;

  /* Number of times sra_modify_assign has deleted a statement.  */
  int deleted;

  /* Number of times sra_modify_assign has to deal with subaccesses of LHS and
     RHS reparately due to type conversions or nonexistent matching
     references.  */
  int separate_lhs_rhs_handling;

  /* Number of parameters that were removed because they were unused.  */
  int deleted_unused_parameters;

  /* Number of scalars passed as parameters by reference that have been
     converted to be passed by value.  */
  int scalar_by_ref_to_by_val;

  /* Number of aggregate parameters that were replaced by one or more of their
     components.  */
  int aggregate_params_reduced;

  /* Numbber of components created when splitting aggregate parameters.  */
  int param_reductions_created;
} sra_stats;

static void
dump_access (FILE *f, struct access *access, bool grp)
{
  fprintf (f, "access { ");
  fprintf (f, "base = (%d)'", DECL_UID (access->base));
  print_generic_expr (f, access->base, 0);
  fprintf (f, "', offset = " HOST_WIDE_INT_PRINT_DEC, access->offset);
  fprintf (f, ", size = " HOST_WIDE_INT_PRINT_DEC, access->size);
  fprintf (f, ", expr = ");
  print_generic_expr (f, access->expr, 0);
  fprintf (f, ", type = ");
  print_generic_expr (f, access->type, 0);
  if (grp)
    fprintf (f, ", total_scalarization = %d, grp_read = %d, grp_write = %d, "
	     "grp_assignment_read = %d, grp_assignment_write = %d, "
	     "grp_scalar_read = %d, grp_scalar_write = %d, "
	     "grp_hint = %d, grp_covered = %d, "
	     "grp_unscalarizable_region = %d, grp_unscalarized_data = %d, "
	     "grp_partial_lhs = %d, grp_to_be_replaced = %d, "
	     "grp_maybe_modified = %d, "
	     "grp_not_necessarilly_dereferenced = %d\n",
	     access->total_scalarization, access->grp_read, access->grp_write,
	     access->grp_assignment_read, access->grp_assignment_write,
	     access->grp_scalar_read, access->grp_scalar_write,
	     access->grp_hint, access->grp_covered,
	     access->grp_unscalarizable_region, access->grp_unscalarized_data,
	     access->grp_partial_lhs, access->grp_to_be_replaced,
	     access->grp_maybe_modified,
	     access->grp_not_necessarilly_dereferenced);
  else
    fprintf (f, ", write = %d, total_scalarization = %d, "
	     "grp_partial_lhs = %d\n",
	     access->write, access->total_scalarization,
	     access->grp_partial_lhs);
}

/* Dump a subtree rooted in ACCESS to file F, indent by LEVEL.  */

static void
dump_access_tree_1 (FILE *f, struct access *access, int level)
{
  do
    {
      int i;

      for (i = 0; i < level; i++)
	fputs ("* ", dump_file);

      dump_access (f, access, true);

      if (access->first_child)
	dump_access_tree_1 (f, access->first_child, level + 1);

      access = access->next_sibling;
    }
  while (access);
}

/* Dump all access trees for a variable, given the pointer to the first root in
   ACCESS.  */

static void
dump_access_tree (FILE *f, struct access *access)
{
  for (; access; access = access->next_grp)
    dump_access_tree_1 (f, access, 0);
}

/* Return true iff ACC is non-NULL and has subaccesses.  */

static inline bool
access_has_children_p (struct access *acc)
{
  return acc && acc->first_child;
}

/* Return a vector of pointers to accesses for the variable given in BASE or
   NULL if there is none.  */

static VEC (access_p, heap) *
get_base_access_vector (tree base)
{
  void **slot;

  slot = pointer_map_contains (base_access_vec, base);
  if (!slot)
    return NULL;
  else
    return *(VEC (access_p, heap) **) slot;
}

/* Find an access with required OFFSET and SIZE in a subtree of accesses rooted
   in ACCESS.  Return NULL if it cannot be found.  */

static struct access *
find_access_in_subtree (struct access *access, HOST_WIDE_INT offset,
			HOST_WIDE_INT size)
{
  while (access && (access->offset != offset || access->size != size))
    {
      struct access *child = access->first_child;

      while (child && (child->offset + child->size <= offset))
	child = child->next_sibling;
      access = child;
    }

  return access;
}

/* Return the first group representative for DECL or NULL if none exists.  */

static struct access *
get_first_repr_for_decl (tree base)
{
  VEC (access_p, heap) *access_vec;

  access_vec = get_base_access_vector (base);
  if (!access_vec)
    return NULL;

  return VEC_index (access_p, access_vec, 0);
}

/* Find an access representative for the variable BASE and given OFFSET and
   SIZE.  Requires that access trees have already been built.  Return NULL if
   it cannot be found.  */

static struct access *
get_var_base_offset_size_access (tree base, HOST_WIDE_INT offset,
				 HOST_WIDE_INT size)
{
  struct access *access;

  access = get_first_repr_for_decl (base);
  while (access && (access->offset + access->size <= offset))
    access = access->next_grp;
  if (!access)
    return NULL;

  return find_access_in_subtree (access, offset, size);
}

/* Add LINK to the linked list of assign links of RACC.  */
static void
add_link_to_rhs (struct access *racc, struct assign_link *link)
{
  gcc_assert (link->racc == racc);

  if (!racc->first_link)
    {
      gcc_assert (!racc->last_link);
      racc->first_link = link;
    }
  else
    racc->last_link->next = link;

  racc->last_link = link;
  link->next = NULL;
}

/* Move all link structures in their linked list in OLD_RACC to the linked list
   in NEW_RACC.  */
static void
relink_to_new_repr (struct access *new_racc, struct access *old_racc)
{
  if (!old_racc->first_link)
    {
      gcc_assert (!old_racc->last_link);
      return;
    }

  if (new_racc->first_link)
    {
      gcc_assert (!new_racc->last_link->next);
      gcc_assert (!old_racc->last_link || !old_racc->last_link->next);

      new_racc->last_link->next = old_racc->first_link;
      new_racc->last_link = old_racc->last_link;
    }
  else
    {
      gcc_assert (!new_racc->last_link);

      new_racc->first_link = old_racc->first_link;
      new_racc->last_link = old_racc->last_link;
    }
  old_racc->first_link = old_racc->last_link = NULL;
}

/* Add ACCESS to the work queue (which is actually a stack).  */

static void
add_access_to_work_queue (struct access *access)
{
  if (!access->grp_queued)
    {
      gcc_assert (!access->next_queued);
      access->next_queued = work_queue_head;
      access->grp_queued = 1;
      work_queue_head = access;
    }
}

/* Pop an access from the work queue, and return it, assuming there is one.  */

static struct access *
pop_access_from_work_queue (void)
{
  struct access *access = work_queue_head;

  work_queue_head = access->next_queued;
  access->next_queued = NULL;
  access->grp_queued = 0;
  return access;
}


/* Allocate necessary structures.  */

static void
sra_initialize (void)
{
  candidate_bitmap = BITMAP_ALLOC (NULL);
  should_scalarize_away_bitmap = BITMAP_ALLOC (NULL);
  cannot_scalarize_away_bitmap = BITMAP_ALLOC (NULL);
  gcc_obstack_init (&name_obstack);
  access_pool = create_alloc_pool ("SRA accesses", sizeof (struct access), 16);
  link_pool = create_alloc_pool ("SRA links", sizeof (struct assign_link), 16);
  base_access_vec = pointer_map_create ();
  memset (&sra_stats, 0, sizeof (sra_stats));
  encountered_apply_args = false;
  encountered_recursive_call = false;
  encountered_unchangable_recursive_call = false;
}

/* Hook fed to pointer_map_traverse, deallocate stored vectors.  */

static bool
delete_base_accesses (const void *key ATTRIBUTE_UNUSED, void **value,
		     void *data ATTRIBUTE_UNUSED)
{
  VEC (access_p, heap) *access_vec;
  access_vec = (VEC (access_p, heap) *) *value;
  VEC_free (access_p, heap, access_vec);

  return true;
}

/* Deallocate all general structures.  */

static void
sra_deinitialize (void)
{
  BITMAP_FREE (candidate_bitmap);
  BITMAP_FREE (should_scalarize_away_bitmap);
  BITMAP_FREE (cannot_scalarize_away_bitmap);
  free_alloc_pool (access_pool);
  free_alloc_pool (link_pool);
  obstack_free (&name_obstack, NULL);

  pointer_map_traverse (base_access_vec, delete_base_accesses, NULL);
  pointer_map_destroy (base_access_vec);
}

/* Remove DECL from candidates for SRA and write REASON to the dump file if
   there is one.  */
static void
disqualify_candidate (tree decl, const char *reason)
{
  bitmap_clear_bit (candidate_bitmap, DECL_UID (decl));

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "! Disqualifying ");
      print_generic_expr (dump_file, decl, 0);
      fprintf (dump_file, " - %s\n", reason);
    }
}

/* Return true iff the type contains a field or an element which does not allow
   scalarization.  */

static bool
type_internals_preclude_sra_p (tree type)
{
  tree fld;
  tree et;

  switch (TREE_CODE (type))
    {
    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      for (fld = TYPE_FIELDS (type); fld; fld = DECL_CHAIN (fld))
	if (TREE_CODE (fld) == FIELD_DECL)
	  {
	    tree ft = TREE_TYPE (fld);

	    if (TREE_THIS_VOLATILE (fld)
		|| !DECL_FIELD_OFFSET (fld) || !DECL_SIZE (fld)
		|| !host_integerp (DECL_FIELD_OFFSET (fld), 1)
		|| !host_integerp (DECL_SIZE (fld), 1)
		|| (AGGREGATE_TYPE_P (ft)
		    && int_bit_position (fld) % BITS_PER_UNIT != 0))
	      return true;

	    if (AGGREGATE_TYPE_P (ft)
		&& type_internals_preclude_sra_p (ft))
	      return true;
	  }

      return false;

    case ARRAY_TYPE:
      et = TREE_TYPE (type);

      if (AGGREGATE_TYPE_P (et))
	return type_internals_preclude_sra_p (et);
      else
	return false;

    default:
      return false;
    }
}

/* If T is an SSA_NAME, return NULL if it is not a default def or return its
   base variable if it is.  Return T if it is not an SSA_NAME.  */

static tree
get_ssa_base_param (tree t)
{
  if (TREE_CODE (t) == SSA_NAME)
    {
      if (SSA_NAME_IS_DEFAULT_DEF (t))
	return SSA_NAME_VAR (t);
      else
	return NULL_TREE;
    }
  return t;
}

/* Mark a dereference of BASE of distance DIST in a basic block tht STMT
   belongs to, unless the BB has already been marked as a potentially
   final.  */

static void
mark_parm_dereference (tree base, HOST_WIDE_INT dist, gimple stmt)
{
  basic_block bb = gimple_bb (stmt);
  int idx, parm_index = 0;
  tree parm;

  if (bitmap_bit_p (final_bbs, bb->index))
    return;

  for (parm = DECL_ARGUMENTS (current_function_decl);
       parm && parm != base;
       parm = DECL_CHAIN (parm))
    parm_index++;

  gcc_assert (parm_index < func_param_count);

  idx = bb->index * func_param_count + parm_index;
  if (bb_dereferences[idx] < dist)
    bb_dereferences[idx] = dist;
}

/* Allocate an access structure for BASE, OFFSET and SIZE, clear it, fill in
   the three fields.  Also add it to the vector of accesses corresponding to
   the base.  Finally, return the new access.  */

static struct access *
create_access_1 (tree base, HOST_WIDE_INT offset, HOST_WIDE_INT size)
{
  VEC (access_p, heap) *vec;
  struct access *access;
  void **slot;

  access = (struct access *) pool_alloc (access_pool);
  memset (access, 0, sizeof (struct access));
  access->base = base;
  access->offset = offset;
  access->size = size;

  slot = pointer_map_contains (base_access_vec, base);
  if (slot)
    vec = (VEC (access_p, heap) *) *slot;
  else
    vec = VEC_alloc (access_p, heap, 32);

  VEC_safe_push (access_p, heap, vec, access);

  *((struct VEC (access_p,heap) **)
	pointer_map_insert (base_access_vec, base)) = vec;

  return access;
}

/* Create and insert access for EXPR. Return created access, or NULL if it is
   not possible.  */

static struct access *
create_access (tree expr, gimple stmt, bool write)
{
  struct access *access;
  HOST_WIDE_INT offset, size, max_size;
  tree base = expr;
  bool ptr, unscalarizable_region = false;

  base = get_ref_base_and_extent (expr, &offset, &size, &max_size);

  if (sra_mode == SRA_MODE_EARLY_IPA
      && TREE_CODE (base) == MEM_REF)
    {
      base = get_ssa_base_param (TREE_OPERAND (base, 0));
      if (!base)
	return NULL;
      ptr = true;
    }
  else
    ptr = false;

  if (!DECL_P (base) || !bitmap_bit_p (candidate_bitmap, DECL_UID (base)))
    return NULL;

  if (sra_mode == SRA_MODE_EARLY_IPA)
    {
      if (size < 0 || size != max_size)
	{
	  disqualify_candidate (base, "Encountered a variable sized access.");
	  return NULL;
	}
      if (TREE_CODE (expr) == COMPONENT_REF
	  && DECL_BIT_FIELD (TREE_OPERAND (expr, 1)))
	{
	  disqualify_candidate (base, "Encountered a bit-field access.");
	  return NULL;
	}
      gcc_checking_assert ((offset % BITS_PER_UNIT) == 0);

      if (ptr)
	mark_parm_dereference (base, offset + size, stmt);
    }
  else
    {
      if (size != max_size)
	{
	  size = max_size;
	  unscalarizable_region = true;
	}
      if (size < 0)
	{
	  disqualify_candidate (base, "Encountered an unconstrained access.");
	  return NULL;
	}
    }

  access = create_access_1 (base, offset, size);
  access->expr = expr;
  access->type = TREE_TYPE (expr);
  access->write = write;
  access->grp_unscalarizable_region = unscalarizable_region;
  access->stmt = stmt;

  if (TREE_CODE (expr) == COMPONENT_REF
      && DECL_NONADDRESSABLE_P (TREE_OPERAND (expr, 1)))
    access->non_addressable = 1;

  return access;
}


/* Return true iff TYPE is a RECORD_TYPE with fields that are either of gimple
   register types or (recursively) records with only these two kinds of fields.
   It also returns false if any of these records contains a bit-field.  */

static bool
type_consists_of_records_p (tree type)
{
  tree fld;

  if (TREE_CODE (type) != RECORD_TYPE)
    return false;

  for (fld = TYPE_FIELDS (type); fld; fld = DECL_CHAIN (fld))
    if (TREE_CODE (fld) == FIELD_DECL)
      {
	tree ft = TREE_TYPE (fld);

	if (DECL_BIT_FIELD (fld))
	  return false;

	if (!is_gimple_reg_type (ft)
	    && !type_consists_of_records_p (ft))
	  return false;
      }

  return true;
}

/* Create total_scalarization accesses for all scalar type fields in DECL that
   must be of a RECORD_TYPE conforming to type_consists_of_records_p.  BASE
   must be the top-most VAR_DECL representing the variable, OFFSET must be the
   offset of DECL within BASE.  REF must be the memory reference expression for
   the given decl.  */

static void
completely_scalarize_record (tree base, tree decl, HOST_WIDE_INT offset,
			     tree ref)
{
  tree fld, decl_type = TREE_TYPE (decl);

  for (fld = TYPE_FIELDS (decl_type); fld; fld = DECL_CHAIN (fld))
    if (TREE_CODE (fld) == FIELD_DECL)
      {
	HOST_WIDE_INT pos = offset + int_bit_position (fld);
	tree ft = TREE_TYPE (fld);
	tree nref = build3 (COMPONENT_REF, TREE_TYPE (fld), ref, fld,
			    NULL_TREE);

	if (is_gimple_reg_type (ft))
	  {
	    struct access *access;
	    HOST_WIDE_INT size;

	    size = tree_low_cst (DECL_SIZE (fld), 1);
	    access = create_access_1 (base, pos, size);
	    access->expr = nref;
	    access->type = ft;
	    access->total_scalarization = 1;
	    /* Accesses for intraprocedural SRA can have their stmt NULL.  */
	  }
	else
	  completely_scalarize_record (base, fld, pos, nref);
      }
}


/* Search the given tree for a declaration by skipping handled components and
   exclude it from the candidates.  */

static void
disqualify_base_of_expr (tree t, const char *reason)
{
  t = get_base_address (t);
  if (sra_mode == SRA_MODE_EARLY_IPA
      && TREE_CODE (t) == MEM_REF)
    t = get_ssa_base_param (TREE_OPERAND (t, 0));

  if (t && DECL_P (t))
    disqualify_candidate (t, reason);
}

/* Scan expression EXPR and create access structures for all accesses to
   candidates for scalarization.  Return the created access or NULL if none is
   created.  */

static struct access *
build_access_from_expr_1 (tree expr, gimple stmt, bool write)
{
  struct access *ret = NULL;
  bool partial_ref;

  if (TREE_CODE (expr) == BIT_FIELD_REF
      || TREE_CODE (expr) == IMAGPART_EXPR
      || TREE_CODE (expr) == REALPART_EXPR)
    {
      expr = TREE_OPERAND (expr, 0);
      partial_ref = true;
    }
  else
    partial_ref = false;

  /* We need to dive through V_C_Es in order to get the size of its parameter
     and not the result type.  Ada produces such statements.  We are also
     capable of handling the topmost V_C_E but not any of those buried in other
     handled components.  */
  if (TREE_CODE (expr) == VIEW_CONVERT_EXPR)
    expr = TREE_OPERAND (expr, 0);

  if (contains_view_convert_expr_p (expr))
    {
      disqualify_base_of_expr (expr, "V_C_E under a different handled "
			       "component.");
      return NULL;
    }

  switch (TREE_CODE (expr))
    {
    case MEM_REF:
      if (TREE_CODE (TREE_OPERAND (expr, 0)) != ADDR_EXPR
	  && sra_mode != SRA_MODE_EARLY_IPA)
	return NULL;
      /* fall through */
    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
    case COMPONENT_REF:
    case ARRAY_REF:
    case ARRAY_RANGE_REF:
      ret = create_access (expr, stmt, write);
      break;

    default:
      break;
    }

  if (write && partial_ref && ret)
    ret->grp_partial_lhs = 1;

  return ret;
}

/* Scan expression EXPR and create access structures for all accesses to
   candidates for scalarization.  Return true if any access has been inserted.
   STMT must be the statement from which the expression is taken, WRITE must be
   true if the expression is a store and false otherwise. */

static bool
build_access_from_expr (tree expr, gimple stmt, bool write)
{
  struct access *access;

  access = build_access_from_expr_1 (expr, stmt, write);
  if (access)
    {
      /* This means the aggregate is accesses as a whole in a way other than an
	 assign statement and thus cannot be removed even if we had a scalar
	 replacement for everything.  */
      if (cannot_scalarize_away_bitmap)
	bitmap_set_bit (cannot_scalarize_away_bitmap, DECL_UID (access->base));
      return true;
    }
  return false;
}

/* Disqualify LHS and RHS for scalarization if STMT must end its basic block in
   modes in which it matters, return true iff they have been disqualified.  RHS
   may be NULL, in that case ignore it.  If we scalarize an aggregate in
   intra-SRA we may need to add statements after each statement.  This is not
   possible if a statement unconditionally has to end the basic block.  */
static bool
disqualify_ops_if_throwing_stmt (gimple stmt, tree lhs, tree rhs)
{
  if ((sra_mode == SRA_MODE_EARLY_INTRA || sra_mode == SRA_MODE_INTRA)
      && (stmt_can_throw_internal (stmt) || stmt_ends_bb_p (stmt)))
    {
      disqualify_base_of_expr (lhs, "LHS of a throwing stmt.");
      if (rhs)
	disqualify_base_of_expr (rhs, "RHS of a throwing stmt.");
      return true;
    }
  return false;
}

/* Scan expressions occuring in STMT, create access structures for all accesses
   to candidates for scalarization and remove those candidates which occur in
   statements or expressions that prevent them from being split apart.  Return
   true if any access has been inserted.  */

static bool
build_accesses_from_assign (gimple stmt)
{
  tree lhs, rhs;
  struct access *lacc, *racc;

  if (!gimple_assign_single_p (stmt))
    return false;

  lhs = gimple_assign_lhs (stmt);
  rhs = gimple_assign_rhs1 (stmt);

  if (disqualify_ops_if_throwing_stmt (stmt, lhs, rhs))
    return false;

  racc = build_access_from_expr_1 (rhs, stmt, false);
  lacc = build_access_from_expr_1 (lhs, stmt, true);

  if (lacc)
    lacc->grp_assignment_write = 1;

  if (racc)
    {
      racc->grp_assignment_read = 1;
      if (should_scalarize_away_bitmap && !gimple_has_volatile_ops (stmt)
	  && !is_gimple_reg_type (racc->type))
	bitmap_set_bit (should_scalarize_away_bitmap, DECL_UID (racc->base));
    }

  if (lacc && racc
      && (sra_mode == SRA_MODE_EARLY_INTRA || sra_mode == SRA_MODE_INTRA)
      && !lacc->grp_unscalarizable_region
      && !racc->grp_unscalarizable_region
      && AGGREGATE_TYPE_P (TREE_TYPE (lhs))
      /* FIXME: Turn the following line into an assert after PR 40058 is
	 fixed.  */
      && lacc->size == racc->size
      && useless_type_conversion_p (lacc->type, racc->type))
    {
      struct assign_link *link;

      link = (struct assign_link *) pool_alloc (link_pool);
      memset (link, 0, sizeof (struct assign_link));

      link->lacc = lacc;
      link->racc = racc;

      add_link_to_rhs (racc, link);
    }

  return lacc || racc;
}

/* Callback of walk_stmt_load_store_addr_ops visit_addr used to determine
   GIMPLE_ASM operands with memory constrains which cannot be scalarized.  */

static bool
asm_visit_addr (gimple stmt ATTRIBUTE_UNUSED, tree op,
		void *data ATTRIBUTE_UNUSED)
{
  op = get_base_address (op);
  if (op
      && DECL_P (op))
    disqualify_candidate (op, "Non-scalarizable GIMPLE_ASM operand.");

  return false;
}

/* Return true iff callsite CALL has at least as many actual arguments as there
   are formal parameters of the function currently processed by IPA-SRA.  */

static inline bool
callsite_has_enough_arguments_p (gimple call)
{
  return gimple_call_num_args (call) >= (unsigned) func_param_count;
}

/* Scan function and look for interesting expressions and create access
   structures for them.  Return true iff any access is created.  */

static bool
scan_function (void)
{
  basic_block bb;
  bool ret = false;

  FOR_EACH_BB (bb)
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple stmt = gsi_stmt (gsi);
	  tree t;
	  unsigned i;

	  if (final_bbs && stmt_can_throw_external (stmt))
	    bitmap_set_bit (final_bbs, bb->index);
	  switch (gimple_code (stmt))
	    {
	    case GIMPLE_RETURN:
	      t = gimple_return_retval (stmt);
	      if (t != NULL_TREE)
		ret |= build_access_from_expr (t, stmt, false);
	      if (final_bbs)
		bitmap_set_bit (final_bbs, bb->index);
	      break;

	    case GIMPLE_ASSIGN:
	      ret |= build_accesses_from_assign (stmt);
	      break;

	    case GIMPLE_CALL:
	      for (i = 0; i < gimple_call_num_args (stmt); i++)
		ret |= build_access_from_expr (gimple_call_arg (stmt, i),
					       stmt, false);

	      if (sra_mode == SRA_MODE_EARLY_IPA)
		{
		  tree dest = gimple_call_fndecl (stmt);
		  int flags = gimple_call_flags (stmt);

		  if (dest)
		    {
		      if (DECL_BUILT_IN_CLASS (dest) == BUILT_IN_NORMAL
			  && DECL_FUNCTION_CODE (dest) == BUILT_IN_APPLY_ARGS)
			encountered_apply_args = true;
		      if (cgraph_get_node (dest)
			  == cgraph_get_node (current_function_decl))
			{
			  encountered_recursive_call = true;
			  if (!callsite_has_enough_arguments_p (stmt))
			    encountered_unchangable_recursive_call = true;
			}
		    }

		  if (final_bbs
		      && (flags & (ECF_CONST | ECF_PURE)) == 0)
		    bitmap_set_bit (final_bbs, bb->index);
		}

	      t = gimple_call_lhs (stmt);
	      if (t && !disqualify_ops_if_throwing_stmt (stmt, t, NULL))
		ret |= build_access_from_expr (t, stmt, true);
	      break;

	    case GIMPLE_ASM:
	      walk_stmt_load_store_addr_ops (stmt, NULL, NULL, NULL,
					     asm_visit_addr);
	      if (final_bbs)
		bitmap_set_bit (final_bbs, bb->index);

	      for (i = 0; i < gimple_asm_ninputs (stmt); i++)
		{
		  t = TREE_VALUE (gimple_asm_input_op (stmt, i));
		  ret |= build_access_from_expr (t, stmt, false);
		}
	      for (i = 0; i < gimple_asm_noutputs (stmt); i++)
		{
		  t = TREE_VALUE (gimple_asm_output_op (stmt, i));
		  ret |= build_access_from_expr (t, stmt, true);
		}
	      break;

	    default:
	      break;
	    }
	}
    }

  return ret;
}

/* Helper of QSORT function. There are pointers to accesses in the array.  An
   access is considered smaller than another if it has smaller offset or if the
   offsets are the same but is size is bigger. */

static int
compare_access_positions (const void *a, const void *b)
{
  const access_p *fp1 = (const access_p *) a;
  const access_p *fp2 = (const access_p *) b;
  const access_p f1 = *fp1;
  const access_p f2 = *fp2;

  if (f1->offset != f2->offset)
    return f1->offset < f2->offset ? -1 : 1;

  if (f1->size == f2->size)
    {
      if (f1->type == f2->type)
	return 0;
      /* Put any non-aggregate type before any aggregate type.  */
      else if (!is_gimple_reg_type (f1->type)
	  && is_gimple_reg_type (f2->type))
	return 1;
      else if (is_gimple_reg_type (f1->type)
	       && !is_gimple_reg_type (f2->type))
	return -1;
      /* Put any complex or vector type before any other scalar type.  */
      else if (TREE_CODE (f1->type) != COMPLEX_TYPE
	       && TREE_CODE (f1->type) != VECTOR_TYPE
	       && (TREE_CODE (f2->type) == COMPLEX_TYPE
		   || TREE_CODE (f2->type) == VECTOR_TYPE))
	return 1;
      else if ((TREE_CODE (f1->type) == COMPLEX_TYPE
		|| TREE_CODE (f1->type) == VECTOR_TYPE)
	       && TREE_CODE (f2->type) != COMPLEX_TYPE
	       && TREE_CODE (f2->type) != VECTOR_TYPE)
	return -1;
      /* Put the integral type with the bigger precision first.  */
      else if (INTEGRAL_TYPE_P (f1->type)
	       && INTEGRAL_TYPE_P (f2->type))
	return TYPE_PRECISION (f2->type) - TYPE_PRECISION (f1->type);
      /* Put any integral type with non-full precision last.  */
      else if (INTEGRAL_TYPE_P (f1->type)
	       && (TREE_INT_CST_LOW (TYPE_SIZE (f1->type))
		   != TYPE_PRECISION (f1->type)))
	return 1;
      else if (INTEGRAL_TYPE_P (f2->type)
	       && (TREE_INT_CST_LOW (TYPE_SIZE (f2->type))
		   != TYPE_PRECISION (f2->type)))
	return -1;
      /* Stabilize the sort.  */
      return TYPE_UID (f1->type) - TYPE_UID (f2->type);
    }

  /* We want the bigger accesses first, thus the opposite operator in the next
     line: */
  return f1->size > f2->size ? -1 : 1;
}


/* Append a name of the declaration to the name obstack.  A helper function for
   make_fancy_name.  */

static void
make_fancy_decl_name (tree decl)
{
  char buffer[32];

  tree name = DECL_NAME (decl);
  if (name)
    obstack_grow (&name_obstack, IDENTIFIER_POINTER (name),
		  IDENTIFIER_LENGTH (name));
  else
    {
      sprintf (buffer, "D%u", DECL_UID (decl));
      obstack_grow (&name_obstack, buffer, strlen (buffer));
    }
}

/* Helper for make_fancy_name.  */

static void
make_fancy_name_1 (tree expr)
{
  char buffer[32];
  tree index;

  if (DECL_P (expr))
    {
      make_fancy_decl_name (expr);
      return;
    }

  switch (TREE_CODE (expr))
    {
    case COMPONENT_REF:
      make_fancy_name_1 (TREE_OPERAND (expr, 0));
      obstack_1grow (&name_obstack, '$');
      make_fancy_decl_name (TREE_OPERAND (expr, 1));
      break;

    case ARRAY_REF:
      make_fancy_name_1 (TREE_OPERAND (expr, 0));
      obstack_1grow (&name_obstack, '$');
      /* Arrays with only one element may not have a constant as their
	 index. */
      index = TREE_OPERAND (expr, 1);
      if (TREE_CODE (index) != INTEGER_CST)
	break;
      sprintf (buffer, HOST_WIDE_INT_PRINT_DEC, TREE_INT_CST_LOW (index));
      obstack_grow (&name_obstack, buffer, strlen (buffer));
      break;

    case ADDR_EXPR:
      make_fancy_name_1 (TREE_OPERAND (expr, 0));
      break;

    case MEM_REF:
      make_fancy_name_1 (TREE_OPERAND (expr, 0));
      if (!integer_zerop (TREE_OPERAND (expr, 1)))
	{
	  obstack_1grow (&name_obstack, '$');
	  sprintf (buffer, HOST_WIDE_INT_PRINT_DEC,
		   TREE_INT_CST_LOW (TREE_OPERAND (expr, 1)));
	  obstack_grow (&name_obstack, buffer, strlen (buffer));
	}
      break;

    case BIT_FIELD_REF:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
      gcc_unreachable (); 	/* we treat these as scalars.  */
      break;
    default:
      break;
    }
}

/* Create a human readable name for replacement variable of ACCESS.  */

static char *
make_fancy_name (tree expr)
{
  make_fancy_name_1 (expr);
  obstack_1grow (&name_obstack, '\0');
  return XOBFINISH (&name_obstack, char *);
}

/* Construct a MEM_REF that would reference a part of aggregate BASE of type
   EXP_TYPE at the given OFFSET.  If BASE is something for which
   get_addr_base_and_unit_offset returns NULL, gsi must be non-NULL and is used
   to insert new statements either before or below the current one as specified
   by INSERT_AFTER.  This function is not capable of handling bitfields.  */

tree
build_ref_for_offset (location_t loc, tree base, HOST_WIDE_INT offset,
		      tree exp_type, gimple_stmt_iterator *gsi,
		      bool insert_after)
{
  tree prev_base = base;
  tree off;
  HOST_WIDE_INT base_offset;

  gcc_checking_assert (offset % BITS_PER_UNIT == 0);

  base = get_addr_base_and_unit_offset (base, &base_offset);

  /* get_addr_base_and_unit_offset returns NULL for references with a variable
     offset such as array[var_index].  */
  if (!base)
    {
      gimple stmt;
      tree tmp, addr;

      gcc_checking_assert (gsi);
      tmp = create_tmp_reg (build_pointer_type (TREE_TYPE (prev_base)), NULL);
      add_referenced_var (tmp);
      tmp = make_ssa_name (tmp, NULL);
      addr = build_fold_addr_expr (unshare_expr (prev_base));
      stmt = gimple_build_assign (tmp, addr);
      gimple_set_location (stmt, loc);
      SSA_NAME_DEF_STMT (tmp) = stmt;
      if (insert_after)
	gsi_insert_after (gsi, stmt, GSI_NEW_STMT);
      else
	gsi_insert_before (gsi, stmt, GSI_SAME_STMT);
      update_stmt (stmt);

      off = build_int_cst (reference_alias_ptr_type (prev_base),
			   offset / BITS_PER_UNIT);
      base = tmp;
    }
  else if (TREE_CODE (base) == MEM_REF)
    {
      off = build_int_cst (TREE_TYPE (TREE_OPERAND (base, 1)),
			   base_offset + offset / BITS_PER_UNIT);
      off = int_const_binop (PLUS_EXPR, TREE_OPERAND (base, 1), off, 0);
      base = unshare_expr (TREE_OPERAND (base, 0));
    }
  else
    {
      off = build_int_cst (reference_alias_ptr_type (base),
			   base_offset + offset / BITS_PER_UNIT);
      base = build_fold_addr_expr (unshare_expr (base));
    }

  return fold_build2_loc (loc, MEM_REF, exp_type, base, off);
}

/* Construct a memory reference to a part of an aggregate BASE at the given
   OFFSET and of the same type as MODEL.  In case this is a reference to a
   component, the function will replicate the last COMPONENT_REF of model's
   expr to access it.  GSI and INSERT_AFTER have the same meaning as in
   build_ref_for_offset.  */

static tree
build_ref_for_model (location_t loc, tree base, HOST_WIDE_INT offset,
		     struct access *model, gimple_stmt_iterator *gsi,
		     bool insert_after)
{
  if (TREE_CODE (model->expr) == COMPONENT_REF)
    {
      tree t, exp_type;
      offset -= int_bit_position (TREE_OPERAND (model->expr, 1));
      exp_type = TREE_TYPE (TREE_OPERAND (model->expr, 0));
      t = build_ref_for_offset (loc, base, offset, exp_type, gsi, insert_after);
      return fold_build3_loc (loc, COMPONENT_REF, model->type, t,
			      TREE_OPERAND (model->expr, 1), NULL_TREE);
    }
  else
    return build_ref_for_offset (loc, base, offset, model->type,
				 gsi, insert_after);
}

/* Construct a memory reference consisting of component_refs and array_refs to
   a part of an aggregate *RES (which is of type TYPE).  The requested part
   should have type EXP_TYPE at be the given OFFSET.  This function might not
   succeed, it returns true when it does and only then *RES points to something
   meaningful.  This function should be used only to build expressions that we
   might need to present to user (e.g. in warnings).  In all other situations,
   build_ref_for_model or build_ref_for_offset should be used instead.  */

static bool
build_user_friendly_ref_for_offset (tree *res, tree type, HOST_WIDE_INT offset,
				    tree exp_type)
{
  while (1)
    {
      tree fld;
      tree tr_size, index, minidx;
      HOST_WIDE_INT el_size;

      if (offset == 0 && exp_type
	  && types_compatible_p (exp_type, type))
	return true;

      switch (TREE_CODE (type))
	{
	case UNION_TYPE:
	case QUAL_UNION_TYPE:
	case RECORD_TYPE:
	  for (fld = TYPE_FIELDS (type); fld; fld = DECL_CHAIN (fld))
	    {
	      HOST_WIDE_INT pos, size;
	      tree expr, *expr_ptr;

	      if (TREE_CODE (fld) != FIELD_DECL)
		continue;

	      pos = int_bit_position (fld);
	      gcc_assert (TREE_CODE (type) == RECORD_TYPE || pos == 0);
	      tr_size = DECL_SIZE (fld);
	      if (!tr_size || !host_integerp (tr_size, 1))
		continue;
	      size = tree_low_cst (tr_size, 1);
	      if (size == 0)
		{
		  if (pos != offset)
		    continue;
		}
	      else if (pos > offset || (pos + size) <= offset)
		continue;

	      expr = build3 (COMPONENT_REF, TREE_TYPE (fld), *res, fld,
			     NULL_TREE);
	      expr_ptr = &expr;
	      if (build_user_friendly_ref_for_offset (expr_ptr, TREE_TYPE (fld),
						      offset - pos, exp_type))
		{
		  *res = expr;
		  return true;
		}
	    }
	  return false;

	case ARRAY_TYPE:
	  tr_size = TYPE_SIZE (TREE_TYPE (type));
	  if (!tr_size || !host_integerp (tr_size, 1))
	    return false;
	  el_size = tree_low_cst (tr_size, 1);

	  minidx = TYPE_MIN_VALUE (TYPE_DOMAIN (type));
	  if (TREE_CODE (minidx) != INTEGER_CST || el_size == 0)
	    return false;
	  index = build_int_cst (TYPE_DOMAIN (type), offset / el_size);
	  if (!integer_zerop (minidx))
	    index = int_const_binop (PLUS_EXPR, index, minidx, 0);
	  *res = build4 (ARRAY_REF, TREE_TYPE (type), *res, index,
			 NULL_TREE, NULL_TREE);
	  offset = offset % el_size;
	  type = TREE_TYPE (type);
	  break;

	default:
	  if (offset != 0)
	    return false;

	  if (exp_type)
	    return false;
	  else
	    return true;
	}
    }
}

/* Return true iff TYPE is stdarg va_list type.  */

static inline bool
is_va_list_type (tree type)
{
  return TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (va_list_type_node);
}

/* The very first phase of intraprocedural SRA.  It marks in candidate_bitmap
   those with type which is suitable for scalarization.  */

static bool
find_var_candidates (void)
{
  tree var, type;
  referenced_var_iterator rvi;
  bool ret = false;

  FOR_EACH_REFERENCED_VAR (cfun, var, rvi)
    {
      if (TREE_CODE (var) != VAR_DECL && TREE_CODE (var) != PARM_DECL)
        continue;
      type = TREE_TYPE (var);

      if (!AGGREGATE_TYPE_P (type)
	  || needs_to_live_in_memory (var)
	  || TREE_THIS_VOLATILE (var)
	  || !COMPLETE_TYPE_P (type)
	  || !host_integerp (TYPE_SIZE (type), 1)
          || tree_low_cst (TYPE_SIZE (type), 1) == 0
	  || type_internals_preclude_sra_p (type)
	  /* Fix for PR 41089.  tree-stdarg.c needs to have va_lists intact but
	      we also want to schedule it rather late.  Thus we ignore it in
	      the early pass. */
	  || (sra_mode == SRA_MODE_EARLY_INTRA
	      && is_va_list_type (type)))
	continue;

      bitmap_set_bit (candidate_bitmap, DECL_UID (var));

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Candidate (%d): ", DECL_UID (var));
	  print_generic_expr (dump_file, var, 0);
	  fprintf (dump_file, "\n");
	}
      ret = true;
    }

  return ret;
}

/* Sort all accesses for the given variable, check for partial overlaps and
   return NULL if there are any.  If there are none, pick a representative for
   each combination of offset and size and create a linked list out of them.
   Return the pointer to the first representative and make sure it is the first
   one in the vector of accesses.  */

static struct access *
sort_and_splice_var_accesses (tree var)
{
  int i, j, access_count;
  struct access *res, **prev_acc_ptr = &res;
  VEC (access_p, heap) *access_vec;
  bool first = true;
  HOST_WIDE_INT low = -1, high = 0;

  access_vec = get_base_access_vector (var);
  if (!access_vec)
    return NULL;
  access_count = VEC_length (access_p, access_vec);

  /* Sort by <OFFSET, SIZE>.  */
  VEC_qsort (access_p, access_vec, compare_access_positions);

  i = 0;
  while (i < access_count)
    {
      struct access *access = VEC_index (access_p, access_vec, i);
      bool grp_write = access->write;
      bool grp_read = !access->write;
      bool grp_scalar_write = access->write
	&& is_gimple_reg_type (access->type);
      bool grp_scalar_read = !access->write
	&& is_gimple_reg_type (access->type);
      bool grp_assignment_read = access->grp_assignment_read;
      bool grp_assignment_write = access->grp_assignment_write;
      bool multiple_scalar_reads = false;
      bool total_scalarization = access->total_scalarization;
      bool grp_partial_lhs = access->grp_partial_lhs;
      bool first_scalar = is_gimple_reg_type (access->type);
      bool unscalarizable_region = access->grp_unscalarizable_region;

      if (first || access->offset >= high)
	{
	  first = false;
	  low = access->offset;
	  high = access->offset + access->size;
	}
      else if (access->offset > low && access->offset + access->size > high)
	return NULL;
      else
	gcc_assert (access->offset >= low
		    && access->offset + access->size <= high);

      j = i + 1;
      while (j < access_count)
	{
	  struct access *ac2 = VEC_index (access_p, access_vec, j);
	  if (ac2->offset != access->offset || ac2->size != access->size)
	    break;
	  if (ac2->write)
	    {
	      grp_write = true;
	      grp_scalar_write = (grp_scalar_write
				  || is_gimple_reg_type (ac2->type));
	    }
	  else
	    {
	      grp_read = true;
	      if (is_gimple_reg_type (ac2->type))
		{
		  if (grp_scalar_read)
		    multiple_scalar_reads = true;
		  else
		    grp_scalar_read = true;
		}
	    }
	  grp_assignment_read |= ac2->grp_assignment_read;
	  grp_assignment_write |= ac2->grp_assignment_write;
	  grp_partial_lhs |= ac2->grp_partial_lhs;
	  unscalarizable_region |= ac2->grp_unscalarizable_region;
	  total_scalarization |= ac2->total_scalarization;
	  relink_to_new_repr (access, ac2);

	  /* If there are both aggregate-type and scalar-type accesses with
	     this combination of size and offset, the comparison function
	     should have put the scalars first.  */
	  gcc_assert (first_scalar || !is_gimple_reg_type (ac2->type));
	  ac2->group_representative = access;
	  j++;
	}

      i = j;

      access->group_representative = access;
      access->grp_write = grp_write;
      access->grp_read = grp_read;
      access->grp_scalar_read = grp_scalar_read;
      access->grp_scalar_write = grp_scalar_write;
      access->grp_assignment_read = grp_assignment_read;
      access->grp_assignment_write = grp_assignment_write;
      access->grp_hint = multiple_scalar_reads || total_scalarization;
      access->grp_partial_lhs = grp_partial_lhs;
      access->grp_unscalarizable_region = unscalarizable_region;
      if (access->first_link)
	add_access_to_work_queue (access);

      *prev_acc_ptr = access;
      prev_acc_ptr = &access->next_grp;
    }

  gcc_assert (res == VEC_index (access_p, access_vec, 0));
  return res;
}

/* Create a variable for the given ACCESS which determines the type, name and a
   few other properties.  Return the variable declaration and store it also to
   ACCESS->replacement.  */

static tree
create_access_replacement (struct access *access, bool rename)
{
  tree repl;

  repl = create_tmp_var (access->type, "SR");
  get_var_ann (repl);
  add_referenced_var (repl);
  if (rename)
    mark_sym_for_renaming (repl);

  if (!access->grp_partial_lhs
      && (TREE_CODE (access->type) == COMPLEX_TYPE
	  || TREE_CODE (access->type) == VECTOR_TYPE))
    DECL_GIMPLE_REG_P (repl) = 1;

  DECL_SOURCE_LOCATION (repl) = DECL_SOURCE_LOCATION (access->base);
  DECL_ARTIFICIAL (repl) = 1;
  DECL_IGNORED_P (repl) = DECL_IGNORED_P (access->base);

  if (DECL_NAME (access->base)
      && !DECL_IGNORED_P (access->base)
      && !DECL_ARTIFICIAL (access->base))
    {
      char *pretty_name = make_fancy_name (access->expr);
      tree debug_expr = unshare_expr (access->expr), d;

      DECL_NAME (repl) = get_identifier (pretty_name);
      obstack_free (&name_obstack, pretty_name);

      /* Get rid of any SSA_NAMEs embedded in debug_expr,
	 as DECL_DEBUG_EXPR isn't considered when looking for still
	 used SSA_NAMEs and thus they could be freed.  All debug info
	 generation cares is whether something is constant or variable
	 and that get_ref_base_and_extent works properly on the
	 expression.  */
      for (d = debug_expr; handled_component_p (d); d = TREE_OPERAND (d, 0))
	switch (TREE_CODE (d))
	  {
	  case ARRAY_REF:
	  case ARRAY_RANGE_REF:
	    if (TREE_OPERAND (d, 1)
		&& TREE_CODE (TREE_OPERAND (d, 1)) == SSA_NAME)
	      TREE_OPERAND (d, 1) = SSA_NAME_VAR (TREE_OPERAND (d, 1));
	    if (TREE_OPERAND (d, 3)
		&& TREE_CODE (TREE_OPERAND (d, 3)) == SSA_NAME)
	      TREE_OPERAND (d, 3) = SSA_NAME_VAR (TREE_OPERAND (d, 3));
	    /* FALLTHRU */
	  case COMPONENT_REF:
	    if (TREE_OPERAND (d, 2)
		&& TREE_CODE (TREE_OPERAND (d, 2)) == SSA_NAME)
	      TREE_OPERAND (d, 2) = SSA_NAME_VAR (TREE_OPERAND (d, 2));
	    break;
	  default:
	    break;
	  }
      SET_DECL_DEBUG_EXPR (repl, debug_expr);
      DECL_DEBUG_EXPR_IS_FROM (repl) = 1;
      if (access->grp_no_warning)
	TREE_NO_WARNING (repl) = 1;
      else
	TREE_NO_WARNING (repl) = TREE_NO_WARNING (access->base);
    }
  else
    TREE_NO_WARNING (repl) = 1;

  if (dump_file)
    {
      fprintf (dump_file, "Created a replacement for ");
      print_generic_expr (dump_file, access->base, 0);
      fprintf (dump_file, " offset: %u, size: %u: ",
	       (unsigned) access->offset, (unsigned) access->size);
      print_generic_expr (dump_file, repl, 0);
      fprintf (dump_file, "\n");
    }
  sra_stats.replacements++;

  return repl;
}

/* Return ACCESS scalar replacement, create it if it does not exist yet.  */

static inline tree
get_access_replacement (struct access *access)
{
  gcc_assert (access->grp_to_be_replaced);

  if (!access->replacement_decl)
    access->replacement_decl = create_access_replacement (access, true);
  return access->replacement_decl;
}

/* Return ACCESS scalar replacement, create it if it does not exist yet but do
   not mark it for renaming.  */

static inline tree
get_unrenamed_access_replacement (struct access *access)
{
  gcc_assert (!access->grp_to_be_replaced);

  if (!access->replacement_decl)
    access->replacement_decl = create_access_replacement (access, false);
  return access->replacement_decl;
}


/* Build a subtree of accesses rooted in *ACCESS, and move the pointer in the
   linked list along the way.  Stop when *ACCESS is NULL or the access pointed
   to it is not "within" the root.  Return false iff some accesses partially
   overlap.  */

static bool
build_access_subtree (struct access **access)
{
  struct access *root = *access, *last_child = NULL;
  HOST_WIDE_INT limit = root->offset + root->size;

  *access = (*access)->next_grp;
  while  (*access && (*access)->offset + (*access)->size <= limit)
    {
      if (!last_child)
	root->first_child = *access;
      else
	last_child->next_sibling = *access;
      last_child = *access;

      if (!build_access_subtree (access))
	return false;
    }

  if (*access && (*access)->offset < limit)
    return false;

  return true;
}

/* Build a tree of access representatives, ACCESS is the pointer to the first
   one, others are linked in a list by the next_grp field.  Return false iff
   some accesses partially overlap.  */

static bool
build_access_trees (struct access *access)
{
  while (access)
    {
      struct access *root = access;

      if (!build_access_subtree (&access))
	return false;
      root->next_grp = access;
    }
  return true;
}

/* Return true if expr contains some ARRAY_REFs into a variable bounded
   array.  */

static bool
expr_with_var_bounded_array_refs_p (tree expr)
{
  while (handled_component_p (expr))
    {
      if (TREE_CODE (expr) == ARRAY_REF
	  && !host_integerp (array_ref_low_bound (expr), 0))
	return true;
      expr = TREE_OPERAND (expr, 0);
    }
  return false;
}

enum mark_rw_status { SRA_MRRW_NOTHING, SRA_MRRW_DIRECT, SRA_MRRW_ASSIGN};

/* Analyze the subtree of accesses rooted in ROOT, scheduling replacements when
   both seeming beneficial and when ALLOW_REPLACEMENTS allows it.  Also set all
   sorts of access flags appropriately along the way, notably always set
   grp_read and grp_assign_read according to MARK_READ and grp_write when
   MARK_WRITE is true.

   Creating a replacement for a scalar access is considered beneficial if its
   grp_hint is set (this means we are either attempting total scalarization or
   there is more than one direct read access) or according to the following
   table:

   Access written to through a scalar type (once or more times)
   |
   |	Written to in an assignment statement
   |	|
   |	|	Access read as scalar _once_
   |	|	|
   |   	|	|	Read in an assignment statement
   |	|	|	|
   |   	|	|	|	Scalarize	Comment
-----------------------------------------------------------------------------
   0	0	0	0			No access for the scalar
   0	0	0	1			No access for the scalar
   0	0	1	0	No		Single read - won't help
   0	0	1	1	No		The same case
   0	1	0	0			No access for the scalar
   0	1	0	1			No access for the scalar
   0	1	1	0	Yes		s = *g; return s.i;
   0	1	1	1       Yes		The same case as above
   1	0	0	0	No		Won't help
   1	0	0	1	Yes		s.i = 1; *g = s;
   1	0	1	0	Yes		s.i = 5; g = s.i;
   1	0	1	1	Yes		The same case as above
   1	1	0	0	No		Won't help.
   1	1	0	1	Yes		s.i = 1; *g = s;
   1	1	1	0	Yes		s = *g; return s.i;
   1	1	1	1	Yes		Any of the above yeses  */

static bool
analyze_access_subtree (struct access *root, bool allow_replacements,
			enum mark_rw_status mark_read,
			enum mark_rw_status mark_write)
{
  struct access *child;
  HOST_WIDE_INT limit = root->offset + root->size;
  HOST_WIDE_INT covered_to = root->offset;
  bool scalar = is_gimple_reg_type (root->type);
  bool hole = false, sth_created = false;

  if (root->grp_assignment_read)
    mark_read = SRA_MRRW_ASSIGN;
  else if (mark_read == SRA_MRRW_ASSIGN)
    {
      root->grp_read = 1;
      root->grp_assignment_read = 1;
    }
  else if (mark_read == SRA_MRRW_DIRECT)
    root->grp_read = 1;
  else if (root->grp_read)
    mark_read = SRA_MRRW_DIRECT;

  if (root->grp_assignment_write)
    mark_write = SRA_MRRW_ASSIGN;
  else if (mark_write == SRA_MRRW_ASSIGN)
    {
      root->grp_write = 1;
      root->grp_assignment_write = 1;
    }
  else if (mark_write == SRA_MRRW_DIRECT)
    root->grp_write = 1;
  else if (root->grp_write)
    mark_write = SRA_MRRW_DIRECT;

  if (root->grp_unscalarizable_region)
    allow_replacements = false;

  if (allow_replacements && expr_with_var_bounded_array_refs_p (root->expr))
    allow_replacements = false;

  for (child = root->first_child; child; child = child->next_sibling)
    {
      if (!hole && child->offset < covered_to)
	hole = true;
      else
	covered_to += child->size;

      sth_created |= analyze_access_subtree (child,
					     allow_replacements && !scalar,
					     mark_read, mark_write);

      root->grp_unscalarized_data |= child->grp_unscalarized_data;
      hole |= !child->grp_covered;
    }

  if (allow_replacements && scalar && !root->first_child
      && (root->grp_hint
	  || ((root->grp_scalar_read || root->grp_assignment_read)
	      && (root->grp_scalar_write || root->grp_assignment_write))))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Marking ");
	  print_generic_expr (dump_file, root->base, 0);
	  fprintf (dump_file, " offset: %u, size: %u: ",
		   (unsigned) root->offset, (unsigned) root->size);
	  fprintf (dump_file, " to be replaced.\n");
	}

      root->grp_to_be_replaced = 1;
      sth_created = true;
      hole = false;
    }
  else if (covered_to < limit)
    hole = true;

  if (sth_created && !hole)
    {
      root->grp_covered = 1;
      return true;
    }
  if (root->grp_write || TREE_CODE (root->base) == PARM_DECL)
    root->grp_unscalarized_data = 1; /* not covered and written to */
  if (sth_created)
    return true;
  return false;
}

/* Analyze all access trees linked by next_grp by the means of
   analyze_access_subtree.  */
static bool
analyze_access_trees (struct access *access)
{
  bool ret = false;

  while (access)
    {
      if (analyze_access_subtree (access, true,
				  SRA_MRRW_NOTHING, SRA_MRRW_NOTHING))
	ret = true;
      access = access->next_grp;
    }

  return ret;
}

/* Return true iff a potential new child of LACC at offset OFFSET and with size
   SIZE would conflict with an already existing one.  If exactly such a child
   already exists in LACC, store a pointer to it in EXACT_MATCH.  */

static bool
child_would_conflict_in_lacc (struct access *lacc, HOST_WIDE_INT norm_offset,
			      HOST_WIDE_INT size, struct access **exact_match)
{
  struct access *child;

  for (child = lacc->first_child; child; child = child->next_sibling)
    {
      if (child->offset == norm_offset && child->size == size)
	{
	  *exact_match = child;
	  return true;
	}

      if (child->offset < norm_offset + size
	  && child->offset + child->size > norm_offset)
	return true;
    }

  return false;
}

/* Create a new child access of PARENT, with all properties just like MODEL
   except for its offset and with its grp_write false and grp_read true.
   Return the new access or NULL if it cannot be created.  Note that this access
   is created long after all splicing and sorting, it's not located in any
   access vector and is automatically a representative of its group.  */

static struct access *
create_artificial_child_access (struct access *parent, struct access *model,
				HOST_WIDE_INT new_offset)
{
  struct access *access;
  struct access **child;
  tree expr = parent->base;

  gcc_assert (!model->grp_unscalarizable_region);

  access = (struct access *) pool_alloc (access_pool);
  memset (access, 0, sizeof (struct access));
  if (!build_user_friendly_ref_for_offset (&expr, TREE_TYPE (expr), new_offset,
					   model->type))
    {
      access->grp_no_warning = true;
      expr = build_ref_for_model (EXPR_LOCATION (parent->base), parent->base,
				  new_offset, model, NULL, false);
    }

  access->base = parent->base;
  access->expr = expr;
  access->offset = new_offset;
  access->size = model->size;
  access->type = model->type;
  access->grp_write = true;
  access->grp_read = false;

  child = &parent->first_child;
  while (*child && (*child)->offset < new_offset)
    child = &(*child)->next_sibling;

  access->next_sibling = *child;
  *child = access;

  return access;
}


/* Propagate all subaccesses of RACC across an assignment link to LACC. Return
   true if any new subaccess was created.  Additionally, if RACC is a scalar
   access but LACC is not, change the type of the latter, if possible.  */

static bool
propagate_subaccesses_across_link (struct access *lacc, struct access *racc)
{
  struct access *rchild;
  HOST_WIDE_INT norm_delta = lacc->offset - racc->offset;
  bool ret = false;

  if (is_gimple_reg_type (lacc->type)
      || lacc->grp_unscalarizable_region
      || racc->grp_unscalarizable_region)
    return false;

  if (!lacc->first_child && !racc->first_child
      && is_gimple_reg_type (racc->type))
    {
      tree t = lacc->base;

      lacc->type = racc->type;
      if (build_user_friendly_ref_for_offset (&t, TREE_TYPE (t), lacc->offset,
					      racc->type))
	lacc->expr = t;
      else
	{
	  lacc->expr = build_ref_for_model (EXPR_LOCATION (lacc->base),
					    lacc->base, lacc->offset,
					    racc, NULL, false);
	  lacc->grp_no_warning = true;
	}
      return false;
    }

  for (rchild = racc->first_child; rchild; rchild = rchild->next_sibling)
    {
      struct access *new_acc = NULL;
      HOST_WIDE_INT norm_offset = rchild->offset + norm_delta;

      if (rchild->grp_unscalarizable_region)
	continue;

      if (child_would_conflict_in_lacc (lacc, norm_offset, rchild->size,
					&new_acc))
	{
	  if (new_acc)
	    {
	      rchild->grp_hint = 1;
	      new_acc->grp_hint |= new_acc->grp_read;
	      if (rchild->first_child)
		ret |= propagate_subaccesses_across_link (new_acc, rchild);
	    }
	  continue;
	}

      rchild->grp_hint = 1;
      new_acc = create_artificial_child_access (lacc, rchild, norm_offset);
      if (new_acc)
	{
	  ret = true;
	  if (racc->first_child)
	    propagate_subaccesses_across_link (new_acc, rchild);
	}
    }

  return ret;
}

/* Propagate all subaccesses across assignment links.  */

static void
propagate_all_subaccesses (void)
{
  while (work_queue_head)
    {
      struct access *racc = pop_access_from_work_queue ();
      struct assign_link *link;

      gcc_assert (racc->first_link);

      for (link = racc->first_link; link; link = link->next)
	{
	  struct access *lacc = link->lacc;

	  if (!bitmap_bit_p (candidate_bitmap, DECL_UID (lacc->base)))
	    continue;
	  lacc = lacc->group_representative;
	  if (propagate_subaccesses_across_link (lacc, racc)
	      && lacc->first_link)
	    add_access_to_work_queue (lacc);
	}
    }
}

/* Go through all accesses collected throughout the (intraprocedural) analysis
   stage, exclude overlapping ones, identify representatives and build trees
   out of them, making decisions about scalarization on the way.  Return true
   iff there are any to-be-scalarized variables after this stage. */

static bool
analyze_all_variable_accesses (void)
{
  int res = 0;
  bitmap tmp = BITMAP_ALLOC (NULL);
  bitmap_iterator bi;
  unsigned i, max_total_scalarization_size;

  max_total_scalarization_size = UNITS_PER_WORD * BITS_PER_UNIT
    * MOVE_RATIO (optimize_function_for_speed_p (cfun));

  EXECUTE_IF_SET_IN_BITMAP (candidate_bitmap, 0, i, bi)
    if (bitmap_bit_p (should_scalarize_away_bitmap, i)
	&& !bitmap_bit_p (cannot_scalarize_away_bitmap, i))
      {
	tree var = referenced_var (i);

	if (TREE_CODE (var) == VAR_DECL
	    && ((unsigned) tree_low_cst (TYPE_SIZE (TREE_TYPE (var)), 1)
		<= max_total_scalarization_size)
	    && type_consists_of_records_p (TREE_TYPE (var)))
	  {
	    completely_scalarize_record (var, var, 0, var);
	    if (dump_file && (dump_flags & TDF_DETAILS))
	      {
		fprintf (dump_file, "Will attempt to totally scalarize ");
		print_generic_expr (dump_file, var, 0);
		fprintf (dump_file, " (UID: %u): \n", DECL_UID (var));
	      }
	  }
      }

  bitmap_copy (tmp, candidate_bitmap);
  EXECUTE_IF_SET_IN_BITMAP (tmp, 0, i, bi)
    {
      tree var = referenced_var (i);
      struct access *access;

      access = sort_and_splice_var_accesses (var);
      if (!access || !build_access_trees (access))
	disqualify_candidate (var,
			      "No or inhibitingly overlapping accesses.");
    }

  propagate_all_subaccesses ();

  bitmap_copy (tmp, candidate_bitmap);
  EXECUTE_IF_SET_IN_BITMAP (tmp, 0, i, bi)
    {
      tree var = referenced_var (i);
      struct access *access = get_first_repr_for_decl (var);

      if (analyze_access_trees (access))
	{
	  res++;
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "\nAccess trees for ");
	      print_generic_expr (dump_file, var, 0);
	      fprintf (dump_file, " (UID: %u): \n", DECL_UID (var));
	      dump_access_tree (dump_file, access);
	      fprintf (dump_file, "\n");
	    }
	}
      else
	disqualify_candidate (var, "No scalar replacements to be created.");
    }

  BITMAP_FREE (tmp);

  if (res)
    {
      statistics_counter_event (cfun, "Scalarized aggregates", res);
      return true;
    }
  else
    return false;
}

/* Generate statements copying scalar replacements of accesses within a subtree
   into or out of AGG.  ACCESS, all its children, siblings and their children
   are to be processed.  AGG is an aggregate type expression (can be a
   declaration but does not have to be, it can for example also be a mem_ref or
   a series of handled components).  TOP_OFFSET is the offset of the processed
   subtree which has to be subtracted from offsets of individual accesses to
   get corresponding offsets for AGG.  If CHUNK_SIZE is non-null, copy only
   replacements in the interval <start_offset, start_offset + chunk_size>,
   otherwise copy all.  GSI is a statement iterator used to place the new
   statements.  WRITE should be true when the statements should write from AGG
   to the replacement and false if vice versa.  if INSERT_AFTER is true, new
   statements will be added after the current statement in GSI, they will be
   added before the statement otherwise.  */

static void
generate_subtree_copies (struct access *access, tree agg,
			 HOST_WIDE_INT top_offset,
			 HOST_WIDE_INT start_offset, HOST_WIDE_INT chunk_size,
			 gimple_stmt_iterator *gsi, bool write,
			 bool insert_after, location_t loc)
{
  do
    {
      if (chunk_size && access->offset >= start_offset + chunk_size)
	return;

      if (access->grp_to_be_replaced
	  && (chunk_size == 0
	      || access->offset + access->size > start_offset))
	{
	  tree expr, repl = get_access_replacement (access);
	  gimple stmt;

	  expr = build_ref_for_model (loc, agg, access->offset - top_offset,
				      access, gsi, insert_after);

	  if (write)
	    {
	      if (access->grp_partial_lhs)
		expr = force_gimple_operand_gsi (gsi, expr, true, NULL_TREE,
						 !insert_after,
						 insert_after ? GSI_NEW_STMT
						 : GSI_SAME_STMT);
	      stmt = gimple_build_assign (repl, expr);
	    }
	  else
	    {
	      TREE_NO_WARNING (repl) = 1;
	      if (access->grp_partial_lhs)
		repl = force_gimple_operand_gsi (gsi, repl, true, NULL_TREE,
						 !insert_after,
						 insert_after ? GSI_NEW_STMT
						 : GSI_SAME_STMT);
	      stmt = gimple_build_assign (expr, repl);
	    }
	  gimple_set_location (stmt, loc);

	  if (insert_after)
	    gsi_insert_after (gsi, stmt, GSI_NEW_STMT);
	  else
	    gsi_insert_before (gsi, stmt, GSI_SAME_STMT);
	  update_stmt (stmt);
	  sra_stats.subtree_copies++;
	}

      if (access->first_child)
	generate_subtree_copies (access->first_child, agg, top_offset,
				 start_offset, chunk_size, gsi,
				 write, insert_after, loc);

      access = access->next_sibling;
    }
  while (access);
}

/* Assign zero to all scalar replacements in an access subtree.  ACCESS is the
   the root of the subtree to be processed.  GSI is the statement iterator used
   for inserting statements which are added after the current statement if
   INSERT_AFTER is true or before it otherwise.  */

static void
init_subtree_with_zero (struct access *access, gimple_stmt_iterator *gsi,
			bool insert_after, location_t loc)

{
  struct access *child;

  if (access->grp_to_be_replaced)
    {
      gimple stmt;

      stmt = gimple_build_assign (get_access_replacement (access),
				  build_zero_cst (access->type));
      if (insert_after)
	gsi_insert_after (gsi, stmt, GSI_NEW_STMT);
      else
	gsi_insert_before (gsi, stmt, GSI_SAME_STMT);
      update_stmt (stmt);
      gimple_set_location (stmt, loc);
    }

  for (child = access->first_child; child; child = child->next_sibling)
    init_subtree_with_zero (child, gsi, insert_after, loc);
}

/* Search for an access representative for the given expression EXPR and
   return it or NULL if it cannot be found.  */

static struct access *
get_access_for_expr (tree expr)
{
  HOST_WIDE_INT offset, size, max_size;
  tree base;

  /* FIXME: This should not be necessary but Ada produces V_C_Es with a type of
     a different size than the size of its argument and we need the latter
     one.  */
  if (TREE_CODE (expr) == VIEW_CONVERT_EXPR)
    expr = TREE_OPERAND (expr, 0);

  base = get_ref_base_and_extent (expr, &offset, &size, &max_size);
  if (max_size == -1 || !DECL_P (base))
    return NULL;

  if (!bitmap_bit_p (candidate_bitmap, DECL_UID (base)))
    return NULL;

  return get_var_base_offset_size_access (base, offset, max_size);
}

/* Replace the expression EXPR with a scalar replacement if there is one and
   generate other statements to do type conversion or subtree copying if
   necessary.  GSI is used to place newly created statements, WRITE is true if
   the expression is being written to (it is on a LHS of a statement or output
   in an assembly statement).  */

static bool
sra_modify_expr (tree *expr, gimple_stmt_iterator *gsi, bool write)
{
  location_t loc;
  struct access *access;
  tree type, bfr;

  if (TREE_CODE (*expr) == BIT_FIELD_REF)
    {
      bfr = *expr;
      expr = &TREE_OPERAND (*expr, 0);
    }
  else
    bfr = NULL_TREE;

  if (TREE_CODE (*expr) == REALPART_EXPR || TREE_CODE (*expr) == IMAGPART_EXPR)
    expr = &TREE_OPERAND (*expr, 0);
  access = get_access_for_expr (*expr);
  if (!access)
    return false;
  type = TREE_TYPE (*expr);

  loc = gimple_location (gsi_stmt (*gsi));
  if (access->grp_to_be_replaced)
    {
      tree repl = get_access_replacement (access);
      /* If we replace a non-register typed access simply use the original
         access expression to extract the scalar component afterwards.
	 This happens if scalarizing a function return value or parameter
	 like in gcc.c-torture/execute/20041124-1.c, 20050316-1.c and
	 gcc.c-torture/compile/20011217-1.c.

         We also want to use this when accessing a complex or vector which can
         be accessed as a different type too, potentially creating a need for
         type conversion (see PR42196) and when scalarized unions are involved
         in assembler statements (see PR42398).  */
      if (!useless_type_conversion_p (type, access->type))
	{
	  tree ref;

	  ref = build_ref_for_model (loc, access->base, access->offset, access,
				     NULL, false);

	  if (write)
	    {
	      gimple stmt;

	      if (access->grp_partial_lhs)
		ref = force_gimple_operand_gsi (gsi, ref, true, NULL_TREE,
						 false, GSI_NEW_STMT);
	      stmt = gimple_build_assign (repl, ref);
	      gimple_set_location (stmt, loc);
	      gsi_insert_after (gsi, stmt, GSI_NEW_STMT);
	    }
	  else
	    {
	      gimple stmt;

	      if (access->grp_partial_lhs)
		repl = force_gimple_operand_gsi (gsi, repl, true, NULL_TREE,
						 true, GSI_SAME_STMT);
	      stmt = gimple_build_assign (ref, repl);
	      gimple_set_location (stmt, loc);
	      gsi_insert_before (gsi, stmt, GSI_SAME_STMT);
	    }
	}
      else
	*expr = repl;
      sra_stats.exprs++;
    }

  if (access->first_child)
    {
      HOST_WIDE_INT start_offset, chunk_size;
      if (bfr
	  && host_integerp (TREE_OPERAND (bfr, 1), 1)
	  && host_integerp (TREE_OPERAND (bfr, 2), 1))
	{
	  chunk_size = tree_low_cst (TREE_OPERAND (bfr, 1), 1);
	  start_offset = access->offset
	    + tree_low_cst (TREE_OPERAND (bfr, 2), 1);
	}
      else
	start_offset = chunk_size = 0;

      generate_subtree_copies (access->first_child, access->base, 0,
			       start_offset, chunk_size, gsi, write, write,
			       loc);
    }
  return true;
}

/* Where scalar replacements of the RHS have been written to when a replacement
   of a LHS of an assigments cannot be direclty loaded from a replacement of
   the RHS. */
enum unscalarized_data_handling { SRA_UDH_NONE,  /* Nothing done so far. */
				  SRA_UDH_RIGHT, /* Data flushed to the RHS. */
				  SRA_UDH_LEFT }; /* Data flushed to the LHS. */

/* Store all replacements in the access tree rooted in TOP_RACC either to their
   base aggregate if there are unscalarized data or directly to LHS of the
   statement that is pointed to by GSI otherwise.  */

static enum unscalarized_data_handling
handle_unscalarized_data_in_subtree (struct access *top_racc,
				     gimple_stmt_iterator *gsi)
{
  if (top_racc->grp_unscalarized_data)
    {
      generate_subtree_copies (top_racc->first_child, top_racc->base, 0, 0, 0,
			       gsi, false, false,
			       gimple_location (gsi_stmt (*gsi)));
      return SRA_UDH_RIGHT;
    }
  else
    {
      tree lhs = gimple_assign_lhs (gsi_stmt (*gsi));
      generate_subtree_copies (top_racc->first_child, lhs, top_racc->offset,
			       0, 0, gsi, false, false,
			       gimple_location (gsi_stmt (*gsi)));
      return SRA_UDH_LEFT;
    }
}


/* Try to generate statements to load all sub-replacements in an access subtree
   formed by children of LACC from scalar replacements in the TOP_RACC subtree.
   If that is not possible, refresh the TOP_RACC base aggregate and load the
   accesses from it.  LEFT_OFFSET is the offset of the left whole subtree being
   copied. NEW_GSI is stmt iterator used for statement insertions after the
   original assignment, OLD_GSI is used to insert statements before the
   assignment.  *REFRESHED keeps the information whether we have needed to
   refresh replacements of the LHS and from which side of the assignments this
   takes place.  */

static void
load_assign_lhs_subreplacements (struct access *lacc, struct access *top_racc,
				 HOST_WIDE_INT left_offset,
				 gimple_stmt_iterator *old_gsi,
				 gimple_stmt_iterator *new_gsi,
				 enum unscalarized_data_handling *refreshed)
{
  location_t loc = gimple_location (gsi_stmt (*old_gsi));
  for (lacc = lacc->first_child; lacc; lacc = lacc->next_sibling)
    {
      if (lacc->grp_to_be_replaced)
	{
	  struct access *racc;
	  HOST_WIDE_INT offset = lacc->offset - left_offset + top_racc->offset;
	  gimple stmt;
	  tree rhs;

	  racc = find_access_in_subtree (top_racc, offset, lacc->size);
	  if (racc && racc->grp_to_be_replaced)
	    {
	      rhs = get_access_replacement (racc);
	      if (!useless_type_conversion_p (lacc->type, racc->type))
		rhs = fold_build1_loc (loc, VIEW_CONVERT_EXPR, lacc->type, rhs);
	    }
	  else
	    {
	      /* No suitable access on the right hand side, need to load from
		 the aggregate.  See if we have to update it first... */
	      if (*refreshed == SRA_UDH_NONE)
		*refreshed = handle_unscalarized_data_in_subtree (top_racc,
								  old_gsi);

	      if (*refreshed == SRA_UDH_LEFT)
		rhs = build_ref_for_model (loc, lacc->base, lacc->offset, lacc,
					    new_gsi, true);
	      else
		rhs = build_ref_for_model (loc, top_racc->base, offset, lacc,
					    new_gsi, true);
	    }

	  stmt = gimple_build_assign (get_access_replacement (lacc), rhs);
	  gsi_insert_after (new_gsi, stmt, GSI_NEW_STMT);
	  gimple_set_location (stmt, loc);
	  update_stmt (stmt);
	  sra_stats.subreplacements++;
	}
      else if (*refreshed == SRA_UDH_NONE
	       && lacc->grp_read && !lacc->grp_covered)
	*refreshed = handle_unscalarized_data_in_subtree (top_racc,
							  old_gsi);

      if (lacc->first_child)
	load_assign_lhs_subreplacements (lacc, top_racc, left_offset,
					 old_gsi, new_gsi, refreshed);
    }
}

/* Result code for SRA assignment modification.  */
enum assignment_mod_result { SRA_AM_NONE,       /* nothing done for the stmt */
			     SRA_AM_MODIFIED,  /* stmt changed but not
						  removed */
			     SRA_AM_REMOVED };  /* stmt eliminated */

/* Modify assignments with a CONSTRUCTOR on their RHS.  STMT contains a pointer
   to the assignment and GSI is the statement iterator pointing at it.  Returns
   the same values as sra_modify_assign.  */

static enum assignment_mod_result
sra_modify_constructor_assign (gimple *stmt, gimple_stmt_iterator *gsi)
{
  tree lhs = gimple_assign_lhs (*stmt);
  struct access *acc;
  location_t loc;

  acc = get_access_for_expr (lhs);
  if (!acc)
    return SRA_AM_NONE;

  loc = gimple_location (*stmt);
  if (VEC_length (constructor_elt,
		  CONSTRUCTOR_ELTS (gimple_assign_rhs1 (*stmt))) > 0)
    {
      /* I have never seen this code path trigger but if it can happen the
	 following should handle it gracefully.  */
      if (access_has_children_p (acc))
	generate_subtree_copies (acc->first_child, acc->base, 0, 0, 0, gsi,
				 true, true, loc);
      return SRA_AM_MODIFIED;
    }

  if (acc->grp_covered)
    {
      init_subtree_with_zero (acc, gsi, false, loc);
      unlink_stmt_vdef (*stmt);
      gsi_remove (gsi, true);
      return SRA_AM_REMOVED;
    }
  else
    {
      init_subtree_with_zero (acc, gsi, true, loc);
      return SRA_AM_MODIFIED;
    }
}

/* Create and return a new suitable default definition SSA_NAME for RACC which
   is an access describing an uninitialized part of an aggregate that is being
   loaded.  */

static tree
get_repl_default_def_ssa_name (struct access *racc)
{
  tree repl, decl;

  decl = get_unrenamed_access_replacement (racc);

  repl = gimple_default_def (cfun, decl);
  if (!repl)
    {
      repl = make_ssa_name (decl, gimple_build_nop ());
      set_default_def (decl, repl);
    }

  return repl;
}

/* Return true if REF has a COMPONENT_REF with a bit-field field declaration
   somewhere in it.  */

static inline bool
contains_bitfld_comp_ref_p (const_tree ref)
{
  while (handled_component_p (ref))
    {
      if (TREE_CODE (ref) == COMPONENT_REF
          && DECL_BIT_FIELD (TREE_OPERAND (ref, 1)))
        return true;
      ref = TREE_OPERAND (ref, 0);
    }

  return false;
}

/* Return true if REF has an VIEW_CONVERT_EXPR or a COMPONENT_REF with a
   bit-field field declaration somewhere in it.  */

static inline bool
contains_vce_or_bfcref_p (const_tree ref)
{
  while (handled_component_p (ref))
    {
      if (TREE_CODE (ref) == VIEW_CONVERT_EXPR
	  || (TREE_CODE (ref) == COMPONENT_REF
	      && DECL_BIT_FIELD (TREE_OPERAND (ref, 1))))
	return true;
      ref = TREE_OPERAND (ref, 0);
    }

  return false;
}

/* Examine both sides of the assignment statement pointed to by STMT, replace
   them with a scalare replacement if there is one and generate copying of
   replacements if scalarized aggregates have been used in the assignment.  GSI
   is used to hold generated statements for type conversions and subtree
   copying.  */

static enum assignment_mod_result
sra_modify_assign (gimple *stmt, gimple_stmt_iterator *gsi)
{
  struct access *lacc, *racc;
  tree lhs, rhs;
  bool modify_this_stmt = false;
  bool force_gimple_rhs = false;
  location_t loc;
  gimple_stmt_iterator orig_gsi = *gsi;

  if (!gimple_assign_single_p (*stmt))
    return SRA_AM_NONE;
  lhs = gimple_assign_lhs (*stmt);
  rhs = gimple_assign_rhs1 (*stmt);

  if (TREE_CODE (rhs) == CONSTRUCTOR)
    return sra_modify_constructor_assign (stmt, gsi);

  if (TREE_CODE (rhs) == REALPART_EXPR || TREE_CODE (lhs) == REALPART_EXPR
      || TREE_CODE (rhs) == IMAGPART_EXPR || TREE_CODE (lhs) == IMAGPART_EXPR
      || TREE_CODE (rhs) == BIT_FIELD_REF || TREE_CODE (lhs) == BIT_FIELD_REF)
    {
      modify_this_stmt = sra_modify_expr (gimple_assign_rhs1_ptr (*stmt),
					  gsi, false);
      modify_this_stmt |= sra_modify_expr (gimple_assign_lhs_ptr (*stmt),
					   gsi, true);
      return modify_this_stmt ? SRA_AM_MODIFIED : SRA_AM_NONE;
    }

  lacc = get_access_for_expr (lhs);
  racc = get_access_for_expr (rhs);
  if (!lacc && !racc)
    return SRA_AM_NONE;

  loc = gimple_location (*stmt);
  if (lacc && lacc->grp_to_be_replaced)
    {
      lhs = get_access_replacement (lacc);
      gimple_assign_set_lhs (*stmt, lhs);
      modify_this_stmt = true;
      if (lacc->grp_partial_lhs)
	force_gimple_rhs = true;
      sra_stats.exprs++;
    }

  if (racc && racc->grp_to_be_replaced)
    {
      rhs = get_access_replacement (racc);
      modify_this_stmt = true;
      if (racc->grp_partial_lhs)
	force_gimple_rhs = true;
      sra_stats.exprs++;
    }

  if (modify_this_stmt)
    {
      if (!useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (rhs)))
	{
	  /* If we can avoid creating a VIEW_CONVERT_EXPR do so.
	     ???  This should move to fold_stmt which we simply should
	     call after building a VIEW_CONVERT_EXPR here.  */
	  if (AGGREGATE_TYPE_P (TREE_TYPE (lhs))
	      && !contains_bitfld_comp_ref_p (lhs)
	      && !access_has_children_p (lacc))
	    {
	      lhs = build_ref_for_model (loc, lhs, 0, racc, gsi, false);
	      gimple_assign_set_lhs (*stmt, lhs);
	    }
	  else if (AGGREGATE_TYPE_P (TREE_TYPE (rhs))
		   && !contains_vce_or_bfcref_p (rhs)
		   && !access_has_children_p (racc))
	    rhs = build_ref_for_model (loc, rhs, 0, lacc, gsi, false);

	  if (!useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (rhs)))
	    {
	      rhs = fold_build1_loc (loc, VIEW_CONVERT_EXPR, TREE_TYPE (lhs),
				     rhs);
	      if (is_gimple_reg_type (TREE_TYPE (lhs))
		  && TREE_CODE (lhs) != SSA_NAME)
		force_gimple_rhs = true;
	    }
	}
    }

  /* From this point on, the function deals with assignments in between
     aggregates when at least one has scalar reductions of some of its
     components.  There are three possible scenarios: Both the LHS and RHS have
     to-be-scalarized components, 2) only the RHS has or 3) only the LHS has.

     In the first case, we would like to load the LHS components from RHS
     components whenever possible.  If that is not possible, we would like to
     read it directly from the RHS (after updating it by storing in it its own
     components).  If there are some necessary unscalarized data in the LHS,
     those will be loaded by the original assignment too.  If neither of these
     cases happen, the original statement can be removed.  Most of this is done
     by load_assign_lhs_subreplacements.

     In the second case, we would like to store all RHS scalarized components
     directly into LHS and if they cover the aggregate completely, remove the
     statement too.  In the third case, we want the LHS components to be loaded
     directly from the RHS (DSE will remove the original statement if it
     becomes redundant).

     This is a bit complex but manageable when types match and when unions do
     not cause confusion in a way that we cannot really load a component of LHS
     from the RHS or vice versa (the access representing this level can have
     subaccesses that are accessible only through a different union field at a
     higher level - different from the one used in the examined expression).
     Unions are fun.

     Therefore, I specially handle a fourth case, happening when there is a
     specific type cast or it is impossible to locate a scalarized subaccess on
     the other side of the expression.  If that happens, I simply "refresh" the
     RHS by storing in it is scalarized components leave the original statement
     there to do the copying and then load the scalar replacements of the LHS.
     This is what the first branch does.  */

  if (gimple_has_volatile_ops (*stmt)
      || contains_vce_or_bfcref_p (rhs)
      || contains_vce_or_bfcref_p (lhs))
    {
      if (access_has_children_p (racc))
	generate_subtree_copies (racc->first_child, racc->base, 0, 0, 0,
				 gsi, false, false, loc);
      if (access_has_children_p (lacc))
	generate_subtree_copies (lacc->first_child, lacc->base, 0, 0, 0,
				 gsi, true, true, loc);
      sra_stats.separate_lhs_rhs_handling++;
    }
  else
    {
      if (access_has_children_p (lacc) && access_has_children_p (racc))
	{
	  gimple_stmt_iterator orig_gsi = *gsi;
	  enum unscalarized_data_handling refreshed;

	  if (lacc->grp_read && !lacc->grp_covered)
	    refreshed = handle_unscalarized_data_in_subtree (racc, gsi);
	  else
	    refreshed = SRA_UDH_NONE;

	  load_assign_lhs_subreplacements (lacc, racc, lacc->offset,
					   &orig_gsi, gsi, &refreshed);
	  if (refreshed != SRA_UDH_RIGHT)
	    {
	      gsi_next (gsi);
	      unlink_stmt_vdef (*stmt);
	      gsi_remove (&orig_gsi, true);
	      sra_stats.deleted++;
	      return SRA_AM_REMOVED;
	    }
	}
      else
	{
	  if (racc)
	    {
	      if (!racc->grp_to_be_replaced && !racc->grp_unscalarized_data)
		{
		  if (dump_file)
		    {
		      fprintf (dump_file, "Removing load: ");
		      print_gimple_stmt (dump_file, *stmt, 0, 0);
		    }

		  if (TREE_CODE (lhs) == SSA_NAME)
		    {
		      rhs = get_repl_default_def_ssa_name (racc);
		      if (!useless_type_conversion_p (TREE_TYPE (lhs),
						      TREE_TYPE (rhs)))
			rhs = fold_build1_loc (loc, VIEW_CONVERT_EXPR,
					       TREE_TYPE (lhs), rhs);
		    }
		  else
		    {
		      if (racc->first_child)
			generate_subtree_copies (racc->first_child, lhs,
						 racc->offset, 0, 0, gsi,
						 false, false, loc);

		      gcc_assert (*stmt == gsi_stmt (*gsi));
		      unlink_stmt_vdef (*stmt);
		      gsi_remove (gsi, true);
		      sra_stats.deleted++;
		      return SRA_AM_REMOVED;
		    }
		}
	      else if (racc->first_child)
		generate_subtree_copies (racc->first_child, lhs, racc->offset,
					 0, 0, gsi, false, true, loc);
	    }
	  if (access_has_children_p (lacc))
	    generate_subtree_copies (lacc->first_child, rhs, lacc->offset,
				     0, 0, gsi, true, true, loc);
	}
    }

  /* This gimplification must be done after generate_subtree_copies, lest we
     insert the subtree copies in the middle of the gimplified sequence.  */
  if (force_gimple_rhs)
    rhs = force_gimple_operand_gsi (&orig_gsi, rhs, true, NULL_TREE,
				    true, GSI_SAME_STMT);
  if (gimple_assign_rhs1 (*stmt) != rhs)
    {
      modify_this_stmt = true;
      gimple_assign_set_rhs_from_tree (&orig_gsi, rhs);
      gcc_assert (*stmt == gsi_stmt (orig_gsi));
    }

  return modify_this_stmt ? SRA_AM_MODIFIED : SRA_AM_NONE;
}

/* Traverse the function body and all modifications as decided in
   analyze_all_variable_accesses.  Return true iff the CFG has been
   changed.  */

static bool
sra_modify_function_body (void)
{
  bool cfg_changed = false;
  basic_block bb;

  FOR_EACH_BB (bb)
    {
      gimple_stmt_iterator gsi = gsi_start_bb (bb);
      while (!gsi_end_p (gsi))
	{
	  gimple stmt = gsi_stmt (gsi);
	  enum assignment_mod_result assign_result;
	  bool modified = false, deleted = false;
	  tree *t;
	  unsigned i;

	  switch (gimple_code (stmt))
	    {
	    case GIMPLE_RETURN:
	      t = gimple_return_retval_ptr (stmt);
	      if (*t != NULL_TREE)
		modified |= sra_modify_expr (t, &gsi, false);
	      break;

	    case GIMPLE_ASSIGN:
	      assign_result = sra_modify_assign (&stmt, &gsi);
	      modified |= assign_result == SRA_AM_MODIFIED;
	      deleted = assign_result == SRA_AM_REMOVED;
	      break;

	    case GIMPLE_CALL:
	      /* Operands must be processed before the lhs.  */
	      for (i = 0; i < gimple_call_num_args (stmt); i++)
		{
		  t = gimple_call_arg_ptr (stmt, i);
		  modified |= sra_modify_expr (t, &gsi, false);
		}

	      if (gimple_call_lhs (stmt))
		{
		  t = gimple_call_lhs_ptr (stmt);
		  modified |= sra_modify_expr (t, &gsi, true);
		}
	      break;

	    case GIMPLE_ASM:
	      for (i = 0; i < gimple_asm_ninputs (stmt); i++)
		{
		  t = &TREE_VALUE (gimple_asm_input_op (stmt, i));
		  modified |= sra_modify_expr (t, &gsi, false);
		}
	      for (i = 0; i < gimple_asm_noutputs (stmt); i++)
		{
		  t = &TREE_VALUE (gimple_asm_output_op (stmt, i));
		  modified |= sra_modify_expr (t, &gsi, true);
		}
	      break;

	    default:
	      break;
	    }

	  if (modified)
	    {
	      update_stmt (stmt);
	      if (maybe_clean_eh_stmt (stmt)
		  && gimple_purge_dead_eh_edges (gimple_bb (stmt)))
		cfg_changed = true;
	    }
	  if (!deleted)
	    gsi_next (&gsi);
	}
    }

  return cfg_changed;
}

/* Generate statements initializing scalar replacements of parts of function
   parameters.  */

static void
initialize_parameter_reductions (void)
{
  gimple_stmt_iterator gsi;
  gimple_seq seq = NULL;
  tree parm;

  for (parm = DECL_ARGUMENTS (current_function_decl);
       parm;
       parm = DECL_CHAIN (parm))
    {
      VEC (access_p, heap) *access_vec;
      struct access *access;

      if (!bitmap_bit_p (candidate_bitmap, DECL_UID (parm)))
	continue;
      access_vec = get_base_access_vector (parm);
      if (!access_vec)
	continue;

      if (!seq)
	{
	  seq = gimple_seq_alloc ();
	  gsi = gsi_start (seq);
	}

      for (access = VEC_index (access_p, access_vec, 0);
	   access;
	   access = access->next_grp)
	generate_subtree_copies (access, parm, 0, 0, 0, &gsi, true, true,
				 EXPR_LOCATION (parm));
    }

  if (seq)
    gsi_insert_seq_on_edge_immediate (single_succ_edge (ENTRY_BLOCK_PTR), seq);
}

/* The "main" function of intraprocedural SRA passes.  Runs the analysis and if
   it reveals there are components of some aggregates to be scalarized, it runs
   the required transformations.  */
static unsigned int
perform_intra_sra (void)
{
  int ret = 0;
  sra_initialize ();

  if (!find_var_candidates ())
    goto out;

  if (!scan_function ())
    goto out;

  if (!analyze_all_variable_accesses ())
    goto out;

  if (sra_modify_function_body ())
    ret = TODO_update_ssa | TODO_cleanup_cfg;
  else
    ret = TODO_update_ssa;
  initialize_parameter_reductions ();

  statistics_counter_event (cfun, "Scalar replacements created",
			    sra_stats.replacements);
  statistics_counter_event (cfun, "Modified expressions", sra_stats.exprs);
  statistics_counter_event (cfun, "Subtree copy stmts",
			    sra_stats.subtree_copies);
  statistics_counter_event (cfun, "Subreplacement stmts",
			    sra_stats.subreplacements);
  statistics_counter_event (cfun, "Deleted stmts", sra_stats.deleted);
  statistics_counter_event (cfun, "Separate LHS and RHS handling",
			    sra_stats.separate_lhs_rhs_handling);

 out:
  sra_deinitialize ();
  return ret;
}

/* Perform early intraprocedural SRA.  */
static unsigned int
early_intra_sra (void)
{
  sra_mode = SRA_MODE_EARLY_INTRA;
  return perform_intra_sra ();
}

/* Perform "late" intraprocedural SRA.  */
static unsigned int
late_intra_sra (void)
{
  sra_mode = SRA_MODE_INTRA;
  return perform_intra_sra ();
}


static bool
gate_intra_sra (void)
{
  return flag_tree_sra != 0 && dbg_cnt (tree_sra);
}


struct gimple_opt_pass pass_sra_early =
{
 {
  GIMPLE_PASS,
  "esra",	 			/* name */
  gate_intra_sra,			/* gate */
  early_intra_sra,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_SRA,				/* tv_id */
  PROP_cfg | PROP_ssa,                  /* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func
  | TODO_update_ssa
  | TODO_ggc_collect
  | TODO_verify_ssa			/* todo_flags_finish */
 }
};

struct gimple_opt_pass pass_sra =
{
 {
  GIMPLE_PASS,
  "sra",	 			/* name */
  gate_intra_sra,			/* gate */
  late_intra_sra,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_SRA,				/* tv_id */
  PROP_cfg | PROP_ssa,                  /* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  TODO_update_address_taken,		/* todo_flags_start */
  TODO_dump_func
  | TODO_update_ssa
  | TODO_ggc_collect
  | TODO_verify_ssa			/* todo_flags_finish */
 }
};


/* Return true iff PARM (which must be a parm_decl) is an unused scalar
   parameter.  */

static bool
is_unused_scalar_param (tree parm)
{
  tree name;
  return (is_gimple_reg (parm)
	  && (!(name = gimple_default_def (cfun, parm))
	      || has_zero_uses (name)));
}

/* Scan immediate uses of a default definition SSA name of a parameter PARM and
   examine whether there are any direct or otherwise infeasible ones.  If so,
   return true, otherwise return false.  PARM must be a gimple register with a
   non-NULL default definition.  */

static bool
ptr_parm_has_direct_uses (tree parm)
{
  imm_use_iterator ui;
  gimple stmt;
  tree name = gimple_default_def (cfun, parm);
  bool ret = false;

  FOR_EACH_IMM_USE_STMT (stmt, ui, name)
    {
      int uses_ok = 0;
      use_operand_p use_p;

      if (is_gimple_debug (stmt))
	continue;

      /* Valid uses include dereferences on the lhs and the rhs.  */
      if (gimple_has_lhs (stmt))
	{
	  tree lhs = gimple_get_lhs (stmt);
	  while (handled_component_p (lhs))
	    lhs = TREE_OPERAND (lhs, 0);
	  if (TREE_CODE (lhs) == MEM_REF
	      && TREE_OPERAND (lhs, 0) == name
	      && integer_zerop (TREE_OPERAND (lhs, 1))
	      && types_compatible_p (TREE_TYPE (lhs),
				     TREE_TYPE (TREE_TYPE (name))))
	    uses_ok++;
	}
      if (gimple_assign_single_p (stmt))
	{
	  tree rhs = gimple_assign_rhs1 (stmt);
	  while (handled_component_p (rhs))
	    rhs = TREE_OPERAND (rhs, 0);
	  if (TREE_CODE (rhs) == MEM_REF
	      && TREE_OPERAND (rhs, 0) == name
	      && integer_zerop (TREE_OPERAND (rhs, 1))
	      && types_compatible_p (TREE_TYPE (rhs),
				     TREE_TYPE (TREE_TYPE (name))))
	    uses_ok++;
	}
      else if (is_gimple_call (stmt))
	{
	  unsigned i;
	  for (i = 0; i < gimple_call_num_args (stmt); ++i)
	    {
	      tree arg = gimple_call_arg (stmt, i);
	      while (handled_component_p (arg))
		arg = TREE_OPERAND (arg, 0);
	      if (TREE_CODE (arg) == MEM_REF
		  && TREE_OPERAND (arg, 0) == name
		  && integer_zerop (TREE_OPERAND (arg, 1))
		  && types_compatible_p (TREE_TYPE (arg),
					 TREE_TYPE (TREE_TYPE (name))))
		uses_ok++;
	    }
	}

      /* If the number of valid uses does not match the number of
         uses in this stmt there is an unhandled use.  */
      FOR_EACH_IMM_USE_ON_STMT (use_p, ui)
	--uses_ok;

      if (uses_ok != 0)
	ret = true;

      if (ret)
	BREAK_FROM_IMM_USE_STMT (ui);
    }

  return ret;
}

/* Identify candidates for reduction for IPA-SRA based on their type and mark
   them in candidate_bitmap.  Note that these do not necessarily include
   parameter which are unused and thus can be removed.  Return true iff any
   such candidate has been found.  */

static bool
find_param_candidates (void)
{
  tree parm;
  int count = 0;
  bool ret = false;

  for (parm = DECL_ARGUMENTS (current_function_decl);
       parm;
       parm = DECL_CHAIN (parm))
    {
      tree type = TREE_TYPE (parm);

      count++;

      if (TREE_THIS_VOLATILE (parm)
	  || TREE_ADDRESSABLE (parm)
	  || (!is_gimple_reg_type (type) && is_va_list_type (type)))
	continue;

      if (is_unused_scalar_param (parm))
	{
	  ret = true;
	  continue;
	}

      if (POINTER_TYPE_P (type))
	{
	  type = TREE_TYPE (type);

	  if (TREE_CODE (type) == FUNCTION_TYPE
	      || TYPE_VOLATILE (type)
	      || (TREE_CODE (type) == ARRAY_TYPE
		  && TYPE_NONALIASED_COMPONENT (type))
	      || !is_gimple_reg (parm)
	      || is_va_list_type (type)
	      || ptr_parm_has_direct_uses (parm))
	    continue;
	}
      else if (!AGGREGATE_TYPE_P (type))
	continue;

      if (!COMPLETE_TYPE_P (type)
	  || !host_integerp (TYPE_SIZE (type), 1)
          || tree_low_cst (TYPE_SIZE (type), 1) == 0
	  || (AGGREGATE_TYPE_P (type)
	      && type_internals_preclude_sra_p (type)))
	continue;

      bitmap_set_bit (candidate_bitmap, DECL_UID (parm));
      ret = true;
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Candidate (%d): ", DECL_UID (parm));
	  print_generic_expr (dump_file, parm, 0);
	  fprintf (dump_file, "\n");
	}
    }

  func_param_count = count;
  return ret;
}

/* Callback of walk_aliased_vdefs, marks the access passed as DATA as
   maybe_modified. */

static bool
mark_maybe_modified (ao_ref *ao ATTRIBUTE_UNUSED, tree vdef ATTRIBUTE_UNUSED,
		     void *data)
{
  struct access *repr = (struct access *) data;

  repr->grp_maybe_modified = 1;
  return true;
}

/* Analyze what representatives (in linked lists accessible from
   REPRESENTATIVES) can be modified by side effects of statements in the
   current function.  */

static void
analyze_modified_params (VEC (access_p, heap) *representatives)
{
  int i;

  for (i = 0; i < func_param_count; i++)
    {
      struct access *repr;

      for (repr = VEC_index (access_p, representatives, i);
	   repr;
	   repr = repr->next_grp)
	{
	  struct access *access;
	  bitmap visited;
	  ao_ref ar;

	  if (no_accesses_p (repr))
	    continue;
	  if (!POINTER_TYPE_P (TREE_TYPE (repr->base))
	      || repr->grp_maybe_modified)
	    continue;

	  ao_ref_init (&ar, repr->expr);
	  visited = BITMAP_ALLOC (NULL);
	  for (access = repr; access; access = access->next_sibling)
	    {
	      /* All accesses are read ones, otherwise grp_maybe_modified would
		 be trivially set.  */
	      walk_aliased_vdefs (&ar, gimple_vuse (access->stmt),
				  mark_maybe_modified, repr, &visited);
	      if (repr->grp_maybe_modified)
		break;
	    }
	  BITMAP_FREE (visited);
	}
    }
}

/* Propagate distances in bb_dereferences in the opposite direction than the
   control flow edges, in each step storing the maximum of the current value
   and the minimum of all successors.  These steps are repeated until the table
   stabilizes.  Note that BBs which might terminate the functions (according to
   final_bbs bitmap) never updated in this way.  */

static void
propagate_dereference_distances (void)
{
  VEC (basic_block, heap) *queue;
  basic_block bb;

  queue = VEC_alloc (basic_block, heap, last_basic_block_for_function (cfun));
  VEC_quick_push (basic_block, queue, ENTRY_BLOCK_PTR);
  FOR_EACH_BB (bb)
    {
      VEC_quick_push (basic_block, queue, bb);
      bb->aux = bb;
    }

  while (!VEC_empty (basic_block, queue))
    {
      edge_iterator ei;
      edge e;
      bool change = false;
      int i;

      bb = VEC_pop (basic_block, queue);
      bb->aux = NULL;

      if (bitmap_bit_p (final_bbs, bb->index))
	continue;

      for (i = 0; i < func_param_count; i++)
	{
	  int idx = bb->index * func_param_count + i;
	  bool first = true;
	  HOST_WIDE_INT inh = 0;

	  FOR_EACH_EDGE (e, ei, bb->succs)
	  {
	    int succ_idx = e->dest->index * func_param_count + i;

	    if (e->src == EXIT_BLOCK_PTR)
	      continue;

	    if (first)
	      {
		first = false;
		inh = bb_dereferences [succ_idx];
	      }
	    else if (bb_dereferences [succ_idx] < inh)
	      inh = bb_dereferences [succ_idx];
	  }

	  if (!first && bb_dereferences[idx] < inh)
	    {
	      bb_dereferences[idx] = inh;
	      change = true;
	    }
	}

      if (change && !bitmap_bit_p (final_bbs, bb->index))
	FOR_EACH_EDGE (e, ei, bb->preds)
	  {
	    if (e->src->aux)
	      continue;

	    e->src->aux = e->src;
	    VEC_quick_push (basic_block, queue, e->src);
	  }
    }

  VEC_free (basic_block, heap, queue);
}

EXTERN_C_END
