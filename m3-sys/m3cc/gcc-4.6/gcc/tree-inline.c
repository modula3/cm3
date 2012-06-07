/* Modula-3: modified */

/* Tree inlining.
   Copyright 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.
   Contributed by Alexandre Oliva <aoliva@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "diagnostic-core.h"
#include "tree.h"
#include "tree-inline.h"
#include "flags.h"
#include "params.h"
#include "input.h"
#include "insn-config.h"
#include "hashtab.h"
#include "langhooks.h"
#include "basic-block.h"
#include "tree-iterator.h"
#include "cgraph.h"
#include "intl.h"
#include "tree-flow.h"
#include "function.h"
#include "tree-flow.h"
#include "tree-pretty-print.h"
#include "except.h"
#include "debug.h"
#include "pointer-set.h"
#include "value-prof.h"
#include "tree-pass.h"
#include "target.h"
#include "integrate.h"

#include "rtl.h"	/* FIXME: For asm_str_count.  */

/* I'm not real happy about this, but we need to handle gimple and
   non-gimple trees.  */
#include "gimple.h"

EXTERN_C_START

/* Inlining, Cloning, Versioning, Parallelization

   Inlining: a function body is duplicated, but the PARM_DECLs are
   remapped into VAR_DECLs, and non-void RETURN_EXPRs become
   MODIFY_EXPRs that store to a dedicated returned-value variable.
   The duplicated eh_region info of the copy will later be appended
   to the info for the caller; the eh_region info in copied throwing
   statements and RESX statements are adjusted accordingly.

   Cloning: (only in C++) We have one body for a con/de/structor, and
   multiple function decls, each with a unique parameter list.
   Duplicate the body, using the given splay tree; some parameters
   will become constants (like 0 or 1).

   Versioning: a function body is duplicated and the result is a new
   function rather than into blocks of an existing function as with
   inlining.  Some parameters will become constants.

   Parallelization: a region of a function is duplicated resulting in
   a new function.  Variables may be replaced with complex expressions
   to enable shared variable semantics.

   All of these will simultaneously lookup any callgraph edges.  If
   we're going to inline the duplicated function body, and the given
   function has some cloned callgraph nodes (one for each place this
   function will be inlined) those callgraph edges will be duplicated.
   If we're cloning the body, those callgraph edges will be
   updated to point into the new body.  (Note that the original
   callgraph node and edge list will not be altered.)

   See the CALL_EXPR handling case in copy_tree_body_r ().  */

/* To Do:

   o In order to make inlining-on-trees work, we pessimized
     function-local static constants.  In particular, they are now
     always output, even when not addressed.  Fix this by treating
     function-local static constants just like global static
     constants; the back-end already knows not to output them if they
     are not needed.

   o Provide heuristics to clamp inlining of recursive template
     calls?  */


/* Weights that estimate_num_insns uses to estimate the size of the
   produced code.  */

eni_weights eni_size_weights;

/* Weights that estimate_num_insns uses to estimate the time necessary
   to execute the produced code.  */

eni_weights eni_time_weights;

/* Prototypes.  */

static void remap_block (tree *, copy_body_data *);
static void copy_bind_expr (tree *, int *, copy_body_data *);
static tree mark_local_for_remap_r (tree *, int *, void *);
static void unsave_expr_1 (tree);
static tree unsave_r (tree *, int *, void *);
static void declare_inline_vars (tree, tree);
static void remap_save_expr (tree *, void *, int *);
static void prepend_lexical_block (tree current_block, tree new_block);
static tree copy_decl_to_var (tree, copy_body_data *);
static tree copy_result_decl_to_var (tree, copy_body_data *);
static gimple remap_gimple_stmt (gimple, copy_body_data *);

/* Insert a tree->tree mapping for ID.  Despite the name suggests
   that the trees should be variables, it is used for more than that.  */

void
insert_decl_map (copy_body_data *id, tree key, tree value)
{
  *pointer_map_insert (id->decl_map, key) = value;

  /* Always insert an identity map as well.  If we see this same new
     node again, we won't want to duplicate it a second time.  */
  if (key != value)
    *pointer_map_insert (id->decl_map, value) = value;
}

/* Insert a tree->tree mapping for ID.  This is only used for
   variables.  */

static void
insert_debug_decl_map (copy_body_data *id, tree key, tree value)
{
  if (!gimple_in_ssa_p (id->src_cfun))
    return;

  if (!MAY_HAVE_DEBUG_STMTS)
    return;

  if (!target_for_debug_bind (key))
    return;

  gcc_assert (TREE_CODE (key) == PARM_DECL);
  gcc_assert (TREE_CODE (value) == VAR_DECL);

  if (!id->debug_map)
    id->debug_map = pointer_map_create ();

  *pointer_map_insert (id->debug_map, key) = value;
}

/* If nonzero, we're remapping the contents of inlined debug
   statements.  If negative, an error has occurred, such as a
   reference to a variable that isn't available in the inlined
   context.  */
static int processing_debug_stmt = 0;

/* Construct new SSA name for old NAME. ID is the inline context.  */

static tree
remap_ssa_name (tree name, copy_body_data *id)
{
  tree new_tree;
  tree *n;

  gcc_assert (TREE_CODE (name) == SSA_NAME);

  n = (tree *) pointer_map_contains (id->decl_map, name);
  if (n)
    return unshare_expr (*n);

  if (processing_debug_stmt)
    {
      processing_debug_stmt = -1;
      return name;
    }

  /* Do not set DEF_STMT yet as statement is not copied yet. We do that
     in copy_bb.  */
  new_tree = remap_decl (SSA_NAME_VAR (name), id);

  /* We might've substituted constant or another SSA_NAME for
     the variable.

     Replace the SSA name representing RESULT_DECL by variable during
     inlining:  this saves us from need to introduce PHI node in a case
     return value is just partly initialized.  */
  if ((TREE_CODE (new_tree) == VAR_DECL || TREE_CODE (new_tree) == PARM_DECL)
      && (TREE_CODE (SSA_NAME_VAR (name)) != RESULT_DECL
	  || !id->transform_return_to_modify))
    {
      struct ptr_info_def *pi;
      new_tree = make_ssa_name (new_tree, NULL);
      insert_decl_map (id, name, new_tree);
      SSA_NAME_OCCURS_IN_ABNORMAL_PHI (new_tree)
	= SSA_NAME_OCCURS_IN_ABNORMAL_PHI (name);
      TREE_TYPE (new_tree) = TREE_TYPE (SSA_NAME_VAR (new_tree));
      /* At least IPA points-to info can be directly transferred.  */
      if (id->src_cfun->gimple_df
	  && id->src_cfun->gimple_df->ipa_pta
	  && (pi = SSA_NAME_PTR_INFO (name))
	  && !pi->pt.anything)
	{
	  struct ptr_info_def *new_pi = get_ptr_info (new_tree);
	  new_pi->pt = pi->pt;
	}
      if (gimple_nop_p (SSA_NAME_DEF_STMT (name)))
	{
	  /* By inlining function having uninitialized variable, we might
	     extend the lifetime (variable might get reused).  This cause
	     ICE in the case we end up extending lifetime of SSA name across
	     abnormal edge, but also increase register pressure.

	     We simply initialize all uninitialized vars by 0 except
	     for case we are inlining to very first BB.  We can avoid
	     this for all BBs that are not inside strongly connected
	     regions of the CFG, but this is expensive to test.  */
	  if (id->entry_bb
	      && is_gimple_reg (SSA_NAME_VAR (name))
	      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (name)
	      && TREE_CODE (SSA_NAME_VAR (name)) != PARM_DECL
	      && (id->entry_bb != EDGE_SUCC (ENTRY_BLOCK_PTR, 0)->dest
		  || EDGE_COUNT (id->entry_bb->preds) != 1))
	    {
	      gimple_stmt_iterator gsi = gsi_last_bb (id->entry_bb);
	      gimple init_stmt;
	      tree zero = build_zero_cst (TREE_TYPE (new_tree));

	      init_stmt = gimple_build_assign (new_tree, zero);
	      gsi_insert_after (&gsi, init_stmt, GSI_NEW_STMT);
	      SSA_NAME_IS_DEFAULT_DEF (new_tree) = 0;
	    }
	  else
	    {
	      SSA_NAME_DEF_STMT (new_tree) = gimple_build_nop ();
	      if (gimple_default_def (id->src_cfun, SSA_NAME_VAR (name))
		  == name)
	        set_default_def (SSA_NAME_VAR (new_tree), new_tree);
	    }
	}
    }
  else
    insert_decl_map (id, name, new_tree);
  return new_tree;
}

/* Remap DECL during the copying of the BLOCK tree for the function.  */

tree
remap_decl (tree decl, copy_body_data *id)
{
  tree *n;

  /* We only remap local variables in the current function.  */

  /* See if we have remapped this declaration.  */

  n = (tree *) pointer_map_contains (id->decl_map, decl);

  if (!n && processing_debug_stmt)
    {
      processing_debug_stmt = -1;
      return decl;
    }

  /* If we didn't already have an equivalent for this declaration,
     create one now.  */
  if (!n)
    {
      /* Make a copy of the variable or label.  */
      tree t = id->copy_decl (decl, id);

      /* Remember it, so that if we encounter this local entity again
	 we can reuse this copy.  Do this early because remap_type may
	 need this decl for TYPE_STUB_DECL.  */
      insert_decl_map (id, decl, t);

      if (!DECL_P (t))
	return t;

      /* Remap types, if necessary.  */
      TREE_TYPE (t) = remap_type (TREE_TYPE (t), id);
      if (TREE_CODE (t) == TYPE_DECL)
        DECL_ORIGINAL_TYPE (t) = remap_type (DECL_ORIGINAL_TYPE (t), id);

      /* Remap sizes as necessary.  */
      walk_tree (&DECL_SIZE (t), copy_tree_body_r, id, NULL);
      walk_tree (&DECL_SIZE_UNIT (t), copy_tree_body_r, id, NULL);

      /* If fields, do likewise for offset and qualifier.  */
      if (TREE_CODE (t) == FIELD_DECL)
	{
	  walk_tree (&DECL_FIELD_OFFSET (t), copy_tree_body_r, id, NULL);
	  if (TREE_CODE (DECL_CONTEXT (t)) == QUAL_UNION_TYPE)
	    walk_tree (&DECL_QUALIFIER (t), copy_tree_body_r, id, NULL);
	}

      if ((TREE_CODE (t) == VAR_DECL
	   || TREE_CODE (t) == RESULT_DECL
	   || TREE_CODE (t) == PARM_DECL)
	  && id->src_fn && DECL_STRUCT_FUNCTION (id->src_fn)
	  && gimple_referenced_vars (DECL_STRUCT_FUNCTION (id->src_fn))
	  /* We don't want to mark as referenced VAR_DECLs that were
	     not marked as such in the src function.  */
	  && (TREE_CODE (decl) != VAR_DECL
	      || referenced_var_lookup (DECL_STRUCT_FUNCTION (id->src_fn),
					DECL_UID (decl))))
	add_referenced_var (t);
      return t;
    }

  if (id->do_not_unshare)
    return *n;
  else
    return unshare_expr (*n);
}

static tree
remap_type_1 (tree type, copy_body_data *id)
{
  tree new_tree, t;

  /* We do need a copy.  build and register it now.  If this is a pointer or
     reference type, remap the designated type and make a new pointer or
     reference type.  */
  if (TREE_CODE (type) == POINTER_TYPE)
    {
      new_tree = build_pointer_type_for_mode (remap_type (TREE_TYPE (type), id),
					 TYPE_MODE (type),
					 TYPE_REF_CAN_ALIAS_ALL (type));
      if (TYPE_ATTRIBUTES (type) || TYPE_QUALS (type))
	new_tree = build_type_attribute_qual_variant (new_tree,
						      TYPE_ATTRIBUTES (type),
						      TYPE_QUALS (type));
      insert_decl_map (id, type, new_tree);
      return new_tree;
    }
  else if (TREE_CODE (type) == REFERENCE_TYPE)
    {
      new_tree = build_reference_type_for_mode (remap_type (TREE_TYPE (type), id),
					    TYPE_MODE (type),
					    TYPE_REF_CAN_ALIAS_ALL (type));
      if (TYPE_ATTRIBUTES (type) || TYPE_QUALS (type))
	new_tree = build_type_attribute_qual_variant (new_tree,
						      TYPE_ATTRIBUTES (type),
						      TYPE_QUALS (type));
      insert_decl_map (id, type, new_tree);
      return new_tree;
    }
  else
    new_tree = copy_node (type);

  insert_decl_map (id, type, new_tree);

  /* This is a new type, not a copy of an old type.  Need to reassociate
     variants.  We can handle everything except the main variant lazily.  */
  t = TYPE_MAIN_VARIANT (type);
  if (type != t)
    {
      t = remap_type (t, id);
      TYPE_MAIN_VARIANT (new_tree) = t;
      TYPE_NEXT_VARIANT (new_tree) = TYPE_NEXT_VARIANT (t);
      TYPE_NEXT_VARIANT (t) = new_tree;
    }
  else
    {
      TYPE_MAIN_VARIANT (new_tree) = new_tree;
      TYPE_NEXT_VARIANT (new_tree) = NULL;
    }

  if (TYPE_STUB_DECL (type))
    TYPE_STUB_DECL (new_tree) = remap_decl (TYPE_STUB_DECL (type), id);

  /* Lazily create pointer and reference types.  */
  TYPE_POINTER_TO (new_tree) = NULL;
  TYPE_REFERENCE_TO (new_tree) = NULL;

  switch (TREE_CODE (new_tree))
    {
    case INTEGER_TYPE:
    case REAL_TYPE:
    case FIXED_POINT_TYPE:
    case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:
      t = TYPE_MIN_VALUE (new_tree);
      if (t && TREE_CODE (t) != INTEGER_CST)
        walk_tree (&TYPE_MIN_VALUE (new_tree), copy_tree_body_r, id, NULL);

      t = TYPE_MAX_VALUE (new_tree);
      if (t && TREE_CODE (t) != INTEGER_CST)
        walk_tree (&TYPE_MAX_VALUE (new_tree), copy_tree_body_r, id, NULL);
      return new_tree;

    case FUNCTION_TYPE:
      TREE_TYPE (new_tree) = remap_type (TREE_TYPE (new_tree), id);
      walk_tree (&TYPE_ARG_TYPES (new_tree), copy_tree_body_r, id, NULL);
      return new_tree;

    case ARRAY_TYPE:
      TREE_TYPE (new_tree) = remap_type (TREE_TYPE (new_tree), id);
      TYPE_DOMAIN (new_tree) = remap_type (TYPE_DOMAIN (new_tree), id);
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      {
	tree f, nf = NULL;

	for (f = TYPE_FIELDS (new_tree); f ; f = DECL_CHAIN (f))
	  {
	    t = remap_decl (f, id);
	    DECL_CONTEXT (t) = new_tree;
	    DECL_CHAIN (t) = nf;
	    nf = t;
	  }
	TYPE_FIELDS (new_tree) = nreverse (nf);
      }
      break;

    case OFFSET_TYPE:
    default:
      /* Shouldn't have been thought variable sized.  */
      gcc_unreachable ();
    }

  walk_tree (&TYPE_SIZE (new_tree), copy_tree_body_r, id, NULL);
  walk_tree (&TYPE_SIZE_UNIT (new_tree), copy_tree_body_r, id, NULL);

  return new_tree;
}

tree
remap_type (tree type, copy_body_data *id)
{
  tree *node;
  tree tmp;

  if (type == NULL)
    return type;

  /* See if we have remapped this type.  */
  node = (tree *) pointer_map_contains (id->decl_map, type);
  if (node)
    return *node;

  /* The type only needs remapping if it's variably modified.  */
  if (! variably_modified_type_p (type, id->src_fn))
    {
      insert_decl_map (id, type, type);
      return type;
    }

  id->remapping_type_depth++;
  tmp = remap_type_1 (type, id);
  id->remapping_type_depth--;

  return tmp;
}

/* Return previously remapped type of TYPE in ID.  Return NULL if TYPE
   is NULL or TYPE has not been remapped before.  */

static tree
remapped_type (tree type, copy_body_data *id)
{
  tree *node;

  if (type == NULL)
    return type;

  /* See if we have remapped this type.  */
  node = (tree *) pointer_map_contains (id->decl_map, type);
  if (node)
    return *node;
  else
    return NULL;
}

  /* The type only needs remapping if it's variably modified.  */
/* Decide if DECL can be put into BLOCK_NONLOCAL_VARs.  */

static bool
can_be_nonlocal (tree decl, copy_body_data *id)
{
  /* We can not duplicate function decls.  */
  if (TREE_CODE (decl) == FUNCTION_DECL)
    return true;

  /* Local static vars must be non-local or we get multiple declaration
     problems.  */
  if (TREE_CODE (decl) == VAR_DECL
      && !auto_var_in_fn_p (decl, id->src_fn))
    return true;

  /* At the moment dwarf2out can handle only these types of nodes.  We
     can support more later.  */
  if (TREE_CODE (decl) != VAR_DECL && TREE_CODE (decl) != PARM_DECL)
    return false;

  /* We must use global type.  We call remapped_type instead of
     remap_type since we don't want to remap this type here if it
     hasn't been remapped before.  */
  if (TREE_TYPE (decl) != remapped_type (TREE_TYPE (decl), id))
    return false;

  /* Wihtout SSA we can't tell if variable is used.  */
  if (!gimple_in_ssa_p (cfun))
    return false;

  /* Live variables must be copied so we can attach DECL_RTL.  */
  if (var_ann (decl))
    return false;

  return true;
}

static tree
remap_decls (tree decls, VEC(tree,gc) **nonlocalized_list, copy_body_data *id)
{
  tree old_var;
  tree new_decls = NULL_TREE;

  /* Remap its variables.  */
  for (old_var = decls; old_var; old_var = DECL_CHAIN (old_var))
    {
      tree new_var;

      if (can_be_nonlocal (old_var, id))
	{
	  if (TREE_CODE (old_var) == VAR_DECL
	      && ! DECL_EXTERNAL (old_var)
	      && (var_ann (old_var) || !gimple_in_ssa_p (cfun)))
	    add_local_decl (cfun, old_var);
	  if ((!optimize || debug_info_level > DINFO_LEVEL_TERSE)
	      && !DECL_IGNORED_P (old_var)
	      && nonlocalized_list)
	    VEC_safe_push (tree, gc, *nonlocalized_list, old_var);
	  continue;
	}

      /* Remap the variable.  */
      new_var = remap_decl (old_var, id);

      /* If we didn't remap this variable, we can't mess with its
	 TREE_CHAIN.  If we remapped this variable to the return slot, it's
	 already declared somewhere else, so don't declare it here.  */

      if (new_var == id->retvar)
	;
      else if (!new_var)
        {
	  if ((!optimize || debug_info_level > DINFO_LEVEL_TERSE)
	      && !DECL_IGNORED_P (old_var)
	      && nonlocalized_list)
	    VEC_safe_push (tree, gc, *nonlocalized_list, old_var);
	}
      else
	{
	  gcc_assert (DECL_P (new_var));
	  DECL_CHAIN (new_var) = new_decls;
	  new_decls = new_var;
 
	  /* Also copy value-expressions.  */
	  if (TREE_CODE (new_var) == VAR_DECL
	      && DECL_HAS_VALUE_EXPR_P (new_var))
	    {
	      tree tem = DECL_VALUE_EXPR (new_var);
	      bool old_regimplify = id->regimplify;
	      id->remapping_type_depth++;
	      walk_tree (&tem, copy_tree_body_r, id, NULL);
	      id->remapping_type_depth--;
	      id->regimplify = old_regimplify;
	      SET_DECL_VALUE_EXPR (new_var, tem);
	    }
	}
    }

  return nreverse (new_decls);
}

/* Copy the BLOCK to contain remapped versions of the variables
   therein.  And hook the new block into the block-tree.  */

static void
remap_block (tree *block, copy_body_data *id)
{
  tree old_block;
  tree new_block;

  /* Make the new block.  */
  old_block = *block;
  new_block = make_node (BLOCK);
  TREE_USED (new_block) = TREE_USED (old_block);
  BLOCK_ABSTRACT_ORIGIN (new_block) = old_block;
  BLOCK_SOURCE_LOCATION (new_block) = BLOCK_SOURCE_LOCATION (old_block);
  BLOCK_NONLOCALIZED_VARS (new_block)
    = VEC_copy (tree, gc, BLOCK_NONLOCALIZED_VARS (old_block));
  *block = new_block;

  /* Remap its variables.  */
  BLOCK_VARS (new_block) = remap_decls (BLOCK_VARS (old_block),
  					&BLOCK_NONLOCALIZED_VARS (new_block),
					id);

  if (id->transform_lang_insert_block)
    id->transform_lang_insert_block (new_block);

  /* Remember the remapped block.  */
  insert_decl_map (id, old_block, new_block);
}

/* Copy the whole block tree and root it in id->block.  */
static tree
remap_blocks (tree block, copy_body_data *id)
{
  tree t;
  tree new_tree = block;

  if (!block)
    return NULL;

  remap_block (&new_tree, id);
  gcc_assert (new_tree != block);
  for (t = BLOCK_SUBBLOCKS (block); t ; t = BLOCK_CHAIN (t))
    prepend_lexical_block (new_tree, remap_blocks (t, id));
  /* Blocks are in arbitrary order, but make things slightly prettier and do
     not swap order when producing a copy.  */
  BLOCK_SUBBLOCKS (new_tree) = blocks_nreverse (BLOCK_SUBBLOCKS (new_tree));
  return new_tree;
}

static void
copy_statement_list (tree *tp)
{
  tree_stmt_iterator oi, ni;
  tree new_tree;

  new_tree = alloc_stmt_list ();
  ni = tsi_start (new_tree);
  oi = tsi_start (*tp);
  TREE_TYPE (new_tree) = TREE_TYPE (*tp);
  *tp = new_tree;

  for (; !tsi_end_p (oi); tsi_next (&oi))
    {
      tree stmt = tsi_stmt (oi);
      if (TREE_CODE (stmt) == STATEMENT_LIST)
	copy_statement_list (&stmt);
      tsi_link_after (&ni, stmt, TSI_CONTINUE_LINKING);
    }
}

static void
copy_bind_expr (tree *tp, int *walk_subtrees, copy_body_data *id)
{
  tree block = BIND_EXPR_BLOCK (*tp);
  /* Copy (and replace) the statement.  */
  copy_tree_r (tp, walk_subtrees, NULL);
  if (block)
    {
      remap_block (&block, id);
      BIND_EXPR_BLOCK (*tp) = block;
    }

  if (BIND_EXPR_VARS (*tp))
    /* This will remap a lot of the same decls again, but this should be
       harmless.  */
    BIND_EXPR_VARS (*tp) = remap_decls (BIND_EXPR_VARS (*tp), NULL, id);
}


/* Create a new gimple_seq by remapping all the statements in BODY
   using the inlining information in ID.  */

static gimple_seq
remap_gimple_seq (gimple_seq body, copy_body_data *id)
{
  gimple_stmt_iterator si;
  gimple_seq new_body = NULL;

  for (si = gsi_start (body); !gsi_end_p (si); gsi_next (&si))
    {
      gimple new_stmt = remap_gimple_stmt (gsi_stmt (si), id);
      gimple_seq_add_stmt (&new_body, new_stmt);
    }

  return new_body;
}


/* Copy a GIMPLE_BIND statement STMT, remapping all the symbols in its
   block using the mapping information in ID.  */

static gimple
copy_gimple_bind (gimple stmt, copy_body_data *id)
{
  gimple new_bind;
  tree new_block, new_vars;
  gimple_seq body, new_body;

  /* Copy the statement.  Note that we purposely don't use copy_stmt
     here because we need to remap statements as we copy.  */
  body = gimple_bind_body (stmt);
  new_body = remap_gimple_seq (body, id);

  new_block = gimple_bind_block (stmt);
  if (new_block)
    remap_block (&new_block, id);

  /* This will remap a lot of the same decls again, but this should be
     harmless.  */
  new_vars = gimple_bind_vars (stmt);
  if (new_vars)
    new_vars = remap_decls (new_vars, NULL, id);

  new_bind = gimple_build_bind (new_vars, new_body, new_block);

  return new_bind;
}


/* Remap the GIMPLE operand pointed to by *TP.  DATA is really a
   'struct walk_stmt_info *'.  DATA->INFO is a 'copy_body_data *'.
   WALK_SUBTREES is used to indicate walk_gimple_op whether to keep
   recursing into the children nodes of *TP.  */

static tree
remap_gimple_op_r (tree *tp, int *walk_subtrees, void *data)
{
  struct walk_stmt_info *wi_p = (struct walk_stmt_info *) data;
  copy_body_data *id = (copy_body_data *) wi_p->info;
  tree fn = id->src_fn;

  if (TREE_CODE (*tp) == SSA_NAME)
    {
      *tp = remap_ssa_name (*tp, id);
      *walk_subtrees = 0;
      return NULL;
    }
  else if (auto_var_in_fn_p (*tp, fn))
    {
      /* Local variables and labels need to be replaced by equivalent
	 variables.  We don't want to copy static variables; there's
	 only one of those, no matter how many times we inline the
	 containing function.  Similarly for globals from an outer
	 function.  */
      tree new_decl;

      /* Remap the declaration.  */
      new_decl = remap_decl (*tp, id);
      gcc_assert (new_decl);
      /* Replace this variable with the copy.  */
      STRIP_TYPE_NOPS (new_decl);
      /* ???  The C++ frontend uses void * pointer zero to initialize
         any other type.  This confuses the middle-end type verification.
	 As cloned bodies do not go through gimplification again the fixup
	 there doesn't trigger.  */
      if (TREE_CODE (new_decl) == INTEGER_CST
	  && !useless_type_conversion_p (TREE_TYPE (*tp), TREE_TYPE (new_decl)))
	new_decl = fold_convert (TREE_TYPE (*tp), new_decl);
      *tp = new_decl;
      *walk_subtrees = 0;
    }
  else if (TREE_CODE (*tp) == STATEMENT_LIST)
    gcc_unreachable ();
  else if (TREE_CODE (*tp) == SAVE_EXPR)
    gcc_unreachable ();
  else if (TREE_CODE (*tp) == LABEL_DECL
	   && (!DECL_CONTEXT (*tp)
	       || decl_function_context (*tp) == id->src_fn))
    /* These may need to be remapped for EH handling.  */
    *tp = remap_decl (*tp, id);
  else if (TYPE_P (*tp))
    /* Types may need remapping as well.  */
    *tp = remap_type (*tp, id);
  else if (CONSTANT_CLASS_P (*tp))
    {
      /* If this is a constant, we have to copy the node iff the type
	 will be remapped.  copy_tree_r will not copy a constant.  */
      tree new_type = remap_type (TREE_TYPE (*tp), id);

      if (new_type == TREE_TYPE (*tp))
	*walk_subtrees = 0;

      else if (TREE_CODE (*tp) == INTEGER_CST)
	*tp = build_int_cst_wide (new_type, TREE_INT_CST_LOW (*tp),
				  TREE_INT_CST_HIGH (*tp));
      else
	{
	  *tp = copy_node (*tp);
	  TREE_TYPE (*tp) = new_type;
	}
    }
  else
    {
      /* Otherwise, just copy the node.  Note that copy_tree_r already
	 knows not to copy VAR_DECLs, etc., so this is safe.  */
      if (TREE_CODE (*tp) == MEM_REF)
	{
	  tree ptr = TREE_OPERAND (*tp, 0);
	  tree old = *tp;
	  tree tem;

	  /* We need to re-canonicalize MEM_REFs from inline substitutions
	     that can happen when a pointer argument is an ADDR_EXPR.
	     Recurse here manually to allow that.  */
	  walk_tree (&ptr, remap_gimple_op_r, data, NULL);
	  if ((tem = maybe_fold_offset_to_reference (EXPR_LOCATION (*tp),
						     ptr,
						     TREE_OPERAND (*tp, 1),
						     TREE_TYPE (*tp)))
	      && TREE_THIS_VOLATILE (tem) == TREE_THIS_VOLATILE (old))
	    {
	      tree *tem_basep = &tem;
	      while (handled_component_p (*tem_basep))
		tem_basep = &TREE_OPERAND (*tem_basep, 0);
	      if (TREE_CODE (*tem_basep) == MEM_REF)
		*tem_basep
		    = build2 (MEM_REF, TREE_TYPE (*tem_basep),
			      TREE_OPERAND (*tem_basep, 0),
			      fold_convert (TREE_TYPE (TREE_OPERAND (*tp, 1)),
					    TREE_OPERAND (*tem_basep, 1)));
	      else
		*tem_basep
		    = build2 (MEM_REF, TREE_TYPE (*tem_basep),
			      build_fold_addr_expr (*tem_basep),
			      build_int_cst
			      (TREE_TYPE (TREE_OPERAND (*tp, 1)), 0));
	      *tp = tem;
	    }
	  else
	    {
	      *tp = fold_build2 (MEM_REF, TREE_TYPE (*tp),
				 ptr, TREE_OPERAND (*tp, 1));
	      TREE_THIS_VOLATILE (*tp) = TREE_THIS_VOLATILE (old);
	      TREE_THIS_NOTRAP (*tp) = TREE_THIS_NOTRAP (old);
	    }
	  TREE_NO_WARNING (*tp) = TREE_NO_WARNING (old);
	  *walk_subtrees = 0;
	  return NULL;
	}

      /* Here is the "usual case".  Copy this tree node, and then
	 tweak some special cases.  */
      copy_tree_r (tp, walk_subtrees, NULL);

      /* Global variables we haven't seen yet need to go into referenced
	 vars.  If not referenced from types only.  */
      if (gimple_in_ssa_p (cfun)
	  && TREE_CODE (*tp) == VAR_DECL
	  && id->remapping_type_depth == 0
	  && !processing_debug_stmt)
	add_referenced_var (*tp);

      /* We should never have TREE_BLOCK set on non-statements.  */
      if (EXPR_P (*tp))
	gcc_assert (!TREE_BLOCK (*tp));

      if (TREE_CODE (*tp) != OMP_CLAUSE)
	TREE_TYPE (*tp) = remap_type (TREE_TYPE (*tp), id);

      if (TREE_CODE (*tp) == TARGET_EXPR && TREE_OPERAND (*tp, 3))
	{
	  /* The copied TARGET_EXPR has never been expanded, even if the
	     original node was expanded already.  */
	  TREE_OPERAND (*tp, 1) = TREE_OPERAND (*tp, 3);
	  TREE_OPERAND (*tp, 3) = NULL_TREE;
	}
      else if (TREE_CODE (*tp) == ADDR_EXPR)
	{
	  /* Variable substitution need not be simple.  In particular,
	     the MEM_REF substitution above.  Make sure that
	     TREE_CONSTANT and friends are up-to-date.  But make sure
	     to not improperly set TREE_BLOCK on some sub-expressions.  */
	  int invariant = is_gimple_min_invariant (*tp);
	  tree block = id->block;
	  id->block = NULL_TREE;
	  walk_tree (&TREE_OPERAND (*tp, 0), remap_gimple_op_r, data, NULL);
	  id->block = block;
	  recompute_tree_invariant_for_addr_expr (*tp);

	  /* If this used to be invariant, but is not any longer,
	     then regimplification is probably needed.  */
	  if (invariant && !is_gimple_min_invariant (*tp))
	    id->regimplify = true;

	  *walk_subtrees = 0;
	}
    }

  /* Keep iterating.  */
  return NULL_TREE;
}


/* Called from copy_body_id via walk_tree.  DATA is really a
   `copy_body_data *'.  */

tree
copy_tree_body_r (tree *tp, int *walk_subtrees, void *data)
{
  copy_body_data *id = (copy_body_data *) data;
  tree fn = id->src_fn;
  tree new_block;

  /* Begin by recognizing trees that we'll completely rewrite for the
     inlining context.  Our output for these trees is completely
     different from out input (e.g. RETURN_EXPR is deleted, and morphs
     into an edge).  Further down, we'll handle trees that get
     duplicated and/or tweaked.  */

  /* When requested, RETURN_EXPRs should be transformed to just the
     contained MODIFY_EXPR.  The branch semantics of the return will
     be handled elsewhere by manipulating the CFG rather than a statement.  */
  if (TREE_CODE (*tp) == RETURN_EXPR && id->transform_return_to_modify)
    {
      tree assignment = TREE_OPERAND (*tp, 0);

      /* If we're returning something, just turn that into an
	 assignment into the equivalent of the original RESULT_DECL.
	 If the "assignment" is just the result decl, the result
	 decl has already been set (e.g. a recent "foo (&result_decl,
	 ...)"); just toss the entire RETURN_EXPR.  */
      if (assignment && TREE_CODE (assignment) == MODIFY_EXPR)
	{
	  /* Replace the RETURN_EXPR with (a copy of) the
	     MODIFY_EXPR hanging underneath.  */
	  *tp = copy_node (assignment);
	}
      else /* Else the RETURN_EXPR returns no value.  */
	{
	  *tp = NULL;
	  return (tree) (void *)1;
	}
    }
  else if (TREE_CODE (*tp) == SSA_NAME)
    {
      *tp = remap_ssa_name (*tp, id);
      *walk_subtrees = 0;
      return NULL;
    }

  /* Local variables and labels need to be replaced by equivalent
     variables.  We don't want to copy static variables; there's only
     one of those, no matter how many times we inline the containing
     function.  Similarly for globals from an outer function.  */
  else if (auto_var_in_fn_p (*tp, fn))
    {
      tree new_decl;

      /* Remap the declaration.  */
      new_decl = remap_decl (*tp, id);
      gcc_assert (new_decl);
      /* Replace this variable with the copy.  */
      STRIP_TYPE_NOPS (new_decl);
      *tp = new_decl;
      *walk_subtrees = 0;
    }
  else if (TREE_CODE (*tp) == STATEMENT_LIST)
    copy_statement_list (tp);
  else if (TREE_CODE (*tp) == SAVE_EXPR
	   || TREE_CODE (*tp) == TARGET_EXPR)
    remap_save_expr (tp, id->decl_map, walk_subtrees);
  else if (TREE_CODE (*tp) == LABEL_DECL
	   && (! DECL_CONTEXT (*tp)
	       || decl_function_context (*tp) == id->src_fn))
    /* These may need to be remapped for EH handling.  */
    *tp = remap_decl (*tp, id);
  else if (TREE_CODE (*tp) == BIND_EXPR)
    copy_bind_expr (tp, walk_subtrees, id);
  /* Types may need remapping as well.  */
  else if (TYPE_P (*tp))
    *tp = remap_type (*tp, id);

  /* If this is a constant, we have to copy the node iff the type will be
     remapped.  copy_tree_r will not copy a constant.  */
  else if (CONSTANT_CLASS_P (*tp))
    {
      tree new_type = remap_type (TREE_TYPE (*tp), id);

      if (new_type == TREE_TYPE (*tp))
	*walk_subtrees = 0;

      else if (TREE_CODE (*tp) == INTEGER_CST)
	*tp = build_int_cst_wide (new_type, TREE_INT_CST_LOW (*tp),
				  TREE_INT_CST_HIGH (*tp));
      else
	{
	  *tp = copy_node (*tp);
	  TREE_TYPE (*tp) = new_type;
	}
    }

  /* Otherwise, just copy the node.  Note that copy_tree_r already
     knows not to copy VAR_DECLs, etc., so this is safe.  */
  else
    {
      /* Here we handle trees that are not completely rewritten.
	 First we detect some inlining-induced bogosities for
	 discarding.  */
      if (TREE_CODE (*tp) == MODIFY_EXPR
	  && TREE_OPERAND (*tp, 0) == TREE_OPERAND (*tp, 1)
	  && (auto_var_in_fn_p (TREE_OPERAND (*tp, 0), fn)))
	{
	  /* Some assignments VAR = VAR; don't generate any rtl code
	     and thus don't count as variable modification.  Avoid
	     keeping bogosities like 0 = 0.  */
	  tree decl = TREE_OPERAND (*tp, 0), value;
	  tree *n;

	  n = (tree *) pointer_map_contains (id->decl_map, decl);
	  if (n)
	    {
	      value = *n;
	      STRIP_TYPE_NOPS (value);
	      if (TREE_CONSTANT (value) || TREE_READONLY (value))
		{
		  *tp = build_empty_stmt (EXPR_LOCATION (*tp));
		  return copy_tree_body_r (tp, walk_subtrees, data);
		}
	    }
	}
      else if (TREE_CODE (*tp) == INDIRECT_REF)
	{
	  /* Get rid of *& from inline substitutions that can happen when a
	     pointer argument is an ADDR_EXPR.  */
	  tree decl = TREE_OPERAND (*tp, 0);
	  tree *n;

	  n = (tree *) pointer_map_contains (id->decl_map, decl);
	  if (n)
	    {
	      tree new_tree;
	      tree old;
	      /* If we happen to get an ADDR_EXPR in n->value, strip
	         it manually here as we'll eventually get ADDR_EXPRs
		 which lie about their types pointed to.  In this case
		 build_fold_indirect_ref wouldn't strip the INDIRECT_REF,
		 but we absolutely rely on that.  As fold_indirect_ref
	         does other useful transformations, try that first, though.  */
	      tree type = TREE_TYPE (TREE_TYPE (*n));
	      if (id->do_not_unshare)
		new_tree = *n;
	      else
		new_tree = unshare_expr (*n);
	      old = *tp;
	      *tp = gimple_fold_indirect_ref (new_tree);
	      if (! *tp)
	        {
		  if (TREE_CODE (new_tree) == ADDR_EXPR)
		    {
		      *tp = fold_indirect_ref_1 (EXPR_LOCATION (new_tree),
						 type, new_tree);
		      /* ???  We should either assert here or build
			 a VIEW_CONVERT_EXPR instead of blindly leaking
			 incompatible types to our IL.  */
		      if (! *tp)
			*tp = TREE_OPERAND (new_tree, 0);
		    }
	          else
		    {
	              *tp = build1 (INDIRECT_REF, type, new_tree);
		      TREE_THIS_VOLATILE (*tp) = TREE_THIS_VOLATILE (old);
		      TREE_SIDE_EFFECTS (*tp) = TREE_SIDE_EFFECTS (old);
		      TREE_READONLY (*tp) = TREE_READONLY (old);
		      TREE_THIS_NOTRAP (*tp) = TREE_THIS_NOTRAP (old);
		    }
		}
	      *walk_subtrees = 0;
	      return NULL;
	    }
	}
      else if (TREE_CODE (*tp) == MEM_REF)
	{
	  /* We need to re-canonicalize MEM_REFs from inline substitutions
	     that can happen when a pointer argument is an ADDR_EXPR.  */
	  tree decl = TREE_OPERAND (*tp, 0);
	  tree *n;

	  n = (tree *) pointer_map_contains (id->decl_map, decl);
	  if (n)
	    {
	      tree old = *tp;
	      *tp = fold_build2 (MEM_REF, TREE_TYPE (*tp),
				 unshare_expr (*n), TREE_OPERAND (*tp, 1));
	      TREE_THIS_VOLATILE (*tp) = TREE_THIS_VOLATILE (old);
	      TREE_NO_WARNING (*tp) = TREE_NO_WARNING (old);
	      *walk_subtrees = 0;
	      return NULL;
	    }
	}

      /* Here is the "usual case".  Copy this tree node, and then
	 tweak some special cases.  */
      copy_tree_r (tp, walk_subtrees, NULL);

      /* Global variables we haven't seen yet needs to go into referenced
	 vars.  If not referenced from types or debug stmts only.  */
      if (gimple_in_ssa_p (cfun)
	  && TREE_CODE (*tp) == VAR_DECL
	  && id->remapping_type_depth == 0
	  && !processing_debug_stmt)
	add_referenced_var (*tp);

      /* If EXPR has block defined, map it to newly constructed block.
         When inlining we want EXPRs without block appear in the block
	 of function call if we are not remapping a type.  */
      if (EXPR_P (*tp))
	{
	  new_block = id->remapping_type_depth == 0 ? id->block : NULL;
	  if (TREE_BLOCK (*tp))
	    {
	      tree *n;
	      n = (tree *) pointer_map_contains (id->decl_map,
						 TREE_BLOCK (*tp));
	      gcc_assert (n || id->remapping_type_depth != 0);
	      if (n)
		new_block = *n;
	    }
	  TREE_BLOCK (*tp) = new_block;
	}

      if (TREE_CODE (*tp) != OMP_CLAUSE)
	TREE_TYPE (*tp) = remap_type (TREE_TYPE (*tp), id);

      /* The copied TARGET_EXPR has never been expanded, even if the
	 original node was expanded already.  */
      if (TREE_CODE (*tp) == TARGET_EXPR && TREE_OPERAND (*tp, 3))
	{
	  TREE_OPERAND (*tp, 1) = TREE_OPERAND (*tp, 3);
	  TREE_OPERAND (*tp, 3) = NULL_TREE;
	}

      /* Variable substitution need not be simple.  In particular, the
	 INDIRECT_REF substitution above.  Make sure that TREE_CONSTANT
	 and friends are up-to-date.  */
      else if (TREE_CODE (*tp) == ADDR_EXPR)
	{
	  int invariant = is_gimple_min_invariant (*tp);
	  walk_tree (&TREE_OPERAND (*tp, 0), copy_tree_body_r, id, NULL);

	  /* Handle the case where we substituted an INDIRECT_REF
	     into the operand of the ADDR_EXPR.  */
	  if (TREE_CODE (TREE_OPERAND (*tp, 0)) == INDIRECT_REF)
	    *tp = TREE_OPERAND (TREE_OPERAND (*tp, 0), 0);
	  else
	    recompute_tree_invariant_for_addr_expr (*tp);

	  /* If this used to be invariant, but is not any longer,
	     then regimplification is probably needed.  */
	  if (invariant && !is_gimple_min_invariant (*tp))
	    id->regimplify = true;

	  *walk_subtrees = 0;
	}
    }

  /* Keep iterating.  */
  return NULL_TREE;
}

/* Helper for remap_gimple_stmt.  Given an EH region number for the
   source function, map that to the duplicate EH region number in
   the destination function.  */

static int
remap_eh_region_nr (int old_nr, copy_body_data *id)
{
  eh_region old_r, new_r;
  void **slot;

  old_r = get_eh_region_from_number_fn (id->src_cfun, old_nr);
  slot = pointer_map_contains (id->eh_map, old_r);
  new_r = (eh_region) *slot;

  return new_r->index;
}

/* Similar, but operate on INTEGER_CSTs.  */

static tree
remap_eh_region_tree_nr (tree old_t_nr, copy_body_data *id)
{
  int old_nr, new_nr;

  old_nr = tree_low_cst (old_t_nr, 0);
  new_nr = remap_eh_region_nr (old_nr, id);

  return build_int_cst (NULL, new_nr);
}

/* Helper for copy_bb.  Remap statement STMT using the inlining
   information in ID.  Return the new statement copy.  */

static gimple
remap_gimple_stmt (gimple stmt, copy_body_data *id)
{
  gimple copy = NULL;
  struct walk_stmt_info wi;
  tree new_block;
  bool skip_first = false;

  /* Begin by recognizing trees that we'll completely rewrite for the
     inlining context.  Our output for these trees is completely
     different from out input (e.g. RETURN_EXPR is deleted, and morphs
     into an edge).  Further down, we'll handle trees that get
     duplicated and/or tweaked.  */

  /* When requested, GIMPLE_RETURNs should be transformed to just the
     contained GIMPLE_ASSIGN.  The branch semantics of the return will
     be handled elsewhere by manipulating the CFG rather than the
     statement.  */
  if (gimple_code (stmt) == GIMPLE_RETURN && id->transform_return_to_modify)
    {
      tree retval = gimple_return_retval (stmt);

      /* If we're returning something, just turn that into an
	 assignment into the equivalent of the original RESULT_DECL.
	 If RETVAL is just the result decl, the result decl has
	 already been set (e.g. a recent "foo (&result_decl, ...)");
	 just toss the entire GIMPLE_RETURN.  */
      if (retval
	  && (TREE_CODE (retval) != RESULT_DECL
	      && (TREE_CODE (retval) != SSA_NAME
		  || TREE_CODE (SSA_NAME_VAR (retval)) != RESULT_DECL)))
        {
	  copy = gimple_build_assign (id->retvar, retval);
	  /* id->retvar is already substituted.  Skip it on later remapping.  */
	  skip_first = true;
	}
      else
	return gimple_build_nop ();
    }
  else if (gimple_has_substatements (stmt))
    {
      gimple_seq s1, s2;

      /* When cloning bodies from the C++ front end, we will be handed bodies
	 in High GIMPLE form.  Handle here all the High GIMPLE statements that
	 have embedded statements.  */
      switch (gimple_code (stmt))
	{
	case GIMPLE_BIND:
	  copy = copy_gimple_bind (stmt, id);
	  break;

	case GIMPLE_CATCH:
	  s1 = remap_gimple_seq (gimple_catch_handler (stmt), id);
	  copy = gimple_build_catch (gimple_catch_types (stmt), s1);
	  break;

	case GIMPLE_EH_FILTER:
	  s1 = remap_gimple_seq (gimple_eh_filter_failure (stmt), id);
	  copy = gimple_build_eh_filter (gimple_eh_filter_types (stmt), s1);
	  break;

	case GIMPLE_TRY:
	  s1 = remap_gimple_seq (gimple_try_eval (stmt), id);
	  s2 = remap_gimple_seq (gimple_try_cleanup (stmt), id);
	  copy = gimple_build_try (s1, s2, gimple_try_kind (stmt));
	  break;

	case GIMPLE_WITH_CLEANUP_EXPR:
	  s1 = remap_gimple_seq (gimple_wce_cleanup (stmt), id);
	  copy = gimple_build_wce (s1);
	  break;

	case GIMPLE_OMP_PARALLEL:
	  s1 = remap_gimple_seq (gimple_omp_body (stmt), id);
	  copy = gimple_build_omp_parallel
	           (s1,
		    gimple_omp_parallel_clauses (stmt),
		    gimple_omp_parallel_child_fn (stmt),
		    gimple_omp_parallel_data_arg (stmt));
	  break;

	case GIMPLE_OMP_TASK:
	  s1 = remap_gimple_seq (gimple_omp_body (stmt), id);
	  copy = gimple_build_omp_task
	           (s1,
		    gimple_omp_task_clauses (stmt),
		    gimple_omp_task_child_fn (stmt),
		    gimple_omp_task_data_arg (stmt),
		    gimple_omp_task_copy_fn (stmt),
		    gimple_omp_task_arg_size (stmt),
		    gimple_omp_task_arg_align (stmt));
	  break;

	case GIMPLE_OMP_FOR:
	  s1 = remap_gimple_seq (gimple_omp_body (stmt), id);
	  s2 = remap_gimple_seq (gimple_omp_for_pre_body (stmt), id);
	  copy = gimple_build_omp_for (s1, gimple_omp_for_clauses (stmt),
				       gimple_omp_for_collapse (stmt), s2);
	  {
	    size_t i;
	    for (i = 0; i < gimple_omp_for_collapse (stmt); i++)
	      {
		gimple_omp_for_set_index (copy, i,
					  gimple_omp_for_index (stmt, i));
		gimple_omp_for_set_initial (copy, i,
					    gimple_omp_for_initial (stmt, i));
		gimple_omp_for_set_final (copy, i,
					  gimple_omp_for_final (stmt, i));
		gimple_omp_for_set_incr (copy, i,
					 gimple_omp_for_incr (stmt, i));
		gimple_omp_for_set_cond (copy, i,
					 gimple_omp_for_cond (stmt, i));
	      }
	  }
	  break;

	case GIMPLE_OMP_MASTER:
	  s1 = remap_gimple_seq (gimple_omp_body (stmt), id);
	  copy = gimple_build_omp_master (s1);
	  break;

	case GIMPLE_OMP_ORDERED:
	  s1 = remap_gimple_seq (gimple_omp_body (stmt), id);
	  copy = gimple_build_omp_ordered (s1);
	  break;

	case GIMPLE_OMP_SECTION:
	  s1 = remap_gimple_seq (gimple_omp_body (stmt), id);
	  copy = gimple_build_omp_section (s1);
	  break;

	case GIMPLE_OMP_SECTIONS:
	  s1 = remap_gimple_seq (gimple_omp_body (stmt), id);
	  copy = gimple_build_omp_sections
	           (s1, gimple_omp_sections_clauses (stmt));
	  break;

	case GIMPLE_OMP_SINGLE:
	  s1 = remap_gimple_seq (gimple_omp_body (stmt), id);
	  copy = gimple_build_omp_single
	           (s1, gimple_omp_single_clauses (stmt));
	  break;

	case GIMPLE_OMP_CRITICAL:
	  s1 = remap_gimple_seq (gimple_omp_body (stmt), id);
	  copy
	    = gimple_build_omp_critical (s1, gimple_omp_critical_name (stmt));
	  break;

	default:
	  gcc_unreachable ();
	}
    }
  else
    {
      if (gimple_assign_copy_p (stmt)
	  && gimple_assign_lhs (stmt) == gimple_assign_rhs1 (stmt)
	  && auto_var_in_fn_p (gimple_assign_lhs (stmt), id->src_fn))
	{
	  /* Here we handle statements that are not completely rewritten.
	     First we detect some inlining-induced bogosities for
	     discarding.  */

	  /* Some assignments VAR = VAR; don't generate any rtl code
	     and thus don't count as variable modification.  Avoid
	     keeping bogosities like 0 = 0.  */
	  tree decl = gimple_assign_lhs (stmt), value;
	  tree *n;

	  n = (tree *) pointer_map_contains (id->decl_map, decl);
	  if (n)
	    {
	      value = *n;
	      STRIP_TYPE_NOPS (value);
	      if (TREE_CONSTANT (value) || TREE_READONLY (value))
		return gimple_build_nop ();
	    }
	}

      if (gimple_debug_bind_p (stmt))
	{
	  copy = gimple_build_debug_bind (gimple_debug_bind_get_var (stmt),
					  gimple_debug_bind_get_value (stmt),
					  stmt);
	  VEC_safe_push (gimple, heap, id->debug_stmts, copy);
	  return copy;
	}

      /* Create a new deep copy of the statement.  */
      copy = gimple_copy (stmt);

      /* Remap the region numbers for __builtin_eh_{pointer,filter},
	 RESX and EH_DISPATCH.  */
      if (id->eh_map)
	switch (gimple_code (copy))
	  {
	  case GIMPLE_CALL:
	    {
	      tree r, fndecl = gimple_call_fndecl (copy);
	      if (fndecl && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL)
		switch (DECL_FUNCTION_CODE (fndecl))
		  {
		  case BUILT_IN_EH_COPY_VALUES:
		    r = gimple_call_arg (copy, 1);
		    r = remap_eh_region_tree_nr (r, id);
		    gimple_call_set_arg (copy, 1, r);
		    /* FALLTHRU */

		  case BUILT_IN_EH_POINTER:
		  case BUILT_IN_EH_FILTER:
		    r = gimple_call_arg (copy, 0);
		    r = remap_eh_region_tree_nr (r, id);
		    gimple_call_set_arg (copy, 0, r);
		    break;

		  default:
		    break;
		  }

	      /* Reset alias info if we didn't apply measures to
		 keep it valid over inlining by setting DECL_PT_UID.  */
	      if (!id->src_cfun->gimple_df
		  || !id->src_cfun->gimple_df->ipa_pta)
		gimple_call_reset_alias_info (copy);
	    }
	    break;

	  case GIMPLE_RESX:
	    {
	      int r = gimple_resx_region (copy);
	      r = remap_eh_region_nr (r, id);
	      gimple_resx_set_region (copy, r);
	    }
	    break;

	  case GIMPLE_EH_DISPATCH:
	    {
	      int r = gimple_eh_dispatch_region (copy);
	      r = remap_eh_region_nr (r, id);
	      gimple_eh_dispatch_set_region (copy, r);
	    }
	    break;

	  default:
	    break;
	  }
    }

  /* If STMT has a block defined, map it to the newly constructed
     block.  When inlining we want statements without a block to
     appear in the block of the function call.  */
  new_block = id->block;
  if (gimple_block (copy))
    {
      tree *n;
      n = (tree *) pointer_map_contains (id->decl_map, gimple_block (copy));
      gcc_assert (n);
      new_block = *n;
    }

  gimple_set_block (copy, new_block);

  if (gimple_debug_bind_p (copy))
    return copy;

  /* Remap all the operands in COPY.  */
  memset (&wi, 0, sizeof (wi));
  wi.info = id;
  if (skip_first)
    walk_tree (gimple_op_ptr (copy, 1), remap_gimple_op_r, &wi, NULL);
  else
    walk_gimple_op (copy, remap_gimple_op_r, &wi);

  /* Clear the copied virtual operands.  We are not remapping them here
     but are going to recreate them from scratch.  */
  if (gimple_has_mem_ops (copy))
    {
      gimple_set_vdef (copy, NULL_TREE);
      gimple_set_vuse (copy, NULL_TREE);
    }

  return copy;
}


/* Copy basic block, scale profile accordingly.  Edges will be taken care of
   later  */

static basic_block
copy_bb (copy_body_data *id, basic_block bb, int frequency_scale,
         gcov_type count_scale)
{
  gimple_stmt_iterator gsi, copy_gsi, seq_gsi;
  basic_block copy_basic_block;
  tree decl;
  gcov_type freq;
  basic_block prev;

  /* Search for previous copied basic block.  */
  prev = bb->prev_bb;
  while (!prev->aux)
    prev = prev->prev_bb;

  /* create_basic_block() will append every new block to
     basic_block_info automatically.  */
  copy_basic_block = create_basic_block (NULL, (void *) 0,
                                         (basic_block) prev->aux);
  copy_basic_block->count = bb->count * count_scale / REG_BR_PROB_BASE;

  /* We are going to rebuild frequencies from scratch.  These values
     have just small importance to drive canonicalize_loop_headers.  */
  freq = ((gcov_type)bb->frequency * frequency_scale / REG_BR_PROB_BASE);

  /* We recompute frequencies after inlining, so this is quite safe.  */
  if (freq > BB_FREQ_MAX)
    freq = BB_FREQ_MAX;
  copy_basic_block->frequency = freq;

  copy_gsi = gsi_start_bb (copy_basic_block);

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple stmt = gsi_stmt (gsi);
      gimple orig_stmt = stmt;

      id->regimplify = false;
      stmt = remap_gimple_stmt (stmt, id);
      if (gimple_nop_p (stmt))
	continue;

      gimple_duplicate_stmt_histograms (cfun, stmt, id->src_cfun, orig_stmt);
      seq_gsi = copy_gsi;

      /* With return slot optimization we can end up with
	 non-gimple (foo *)&this->m, fix that here.  */
      if (is_gimple_assign (stmt)
	  && gimple_assign_rhs_code (stmt) == NOP_EXPR
	  && !is_gimple_val (gimple_assign_rhs1 (stmt)))
	{
	  tree new_rhs;
	  new_rhs = force_gimple_operand_gsi (&seq_gsi,
					      gimple_assign_rhs1 (stmt),
					      true, NULL, false,
					      GSI_CONTINUE_LINKING);
	  gimple_assign_set_rhs1 (stmt, new_rhs);
	  id->regimplify = false;
	}

      gsi_insert_after (&seq_gsi, stmt, GSI_NEW_STMT);

      if (id->regimplify)
	gimple_regimplify_operands (stmt, &seq_gsi);

      /* If copy_basic_block has been empty at the start of this iteration,
	 call gsi_start_bb again to get at the newly added statements.  */
      if (gsi_end_p (copy_gsi))
	copy_gsi = gsi_start_bb (copy_basic_block);
      else
	gsi_next (&copy_gsi);

      /* Process the new statement.  The call to gimple_regimplify_operands
	 possibly turned the statement into multiple statements, we
	 need to process all of them.  */
      do
	{
	  tree fn;

	  stmt = gsi_stmt (copy_gsi);
	  if (is_gimple_call (stmt)
	      && gimple_call_va_arg_pack_p (stmt)
	      && id->gimple_call)
	    {
	      /* __builtin_va_arg_pack () should be replaced by
		 all arguments corresponding to ... in the caller.  */
	      tree p;
	      gimple new_call;
	      VEC(tree, heap) *argarray;
	      size_t nargs = gimple_call_num_args (id->gimple_call);
	      size_t n;

	      for (p = DECL_ARGUMENTS (id->src_fn); p; p = DECL_CHAIN (p))
		nargs--;

	      /* Create the new array of arguments.  */
	      n = nargs + gimple_call_num_args (stmt);
	      argarray = VEC_alloc (tree, heap, n);
	      VEC_safe_grow (tree, heap, argarray, n);

	      /* Copy all the arguments before '...'  */
	      memcpy (VEC_address (tree, argarray),
		      gimple_call_arg_ptr (stmt, 0),
		      gimple_call_num_args (stmt) * sizeof (tree));

	      /* Append the arguments passed in '...'  */
	      memcpy (VEC_address(tree, argarray) + gimple_call_num_args (stmt),
		      gimple_call_arg_ptr (id->gimple_call, 0)
			+ (gimple_call_num_args (id->gimple_call) - nargs),
		      nargs * sizeof (tree));

	      new_call = gimple_build_call_vec (gimple_call_fn (stmt),
						argarray);

	      VEC_free (tree, heap, argarray);

	      /* Copy all GIMPLE_CALL flags, location and block, except
		 GF_CALL_VA_ARG_PACK.  */
	      gimple_call_copy_flags (new_call, stmt);
	      gimple_call_set_va_arg_pack (new_call, false);
	      gimple_set_location (new_call, gimple_location (stmt));
	      gimple_set_block (new_call, gimple_block (stmt));
	      gimple_call_set_lhs (new_call, gimple_call_lhs (stmt));

	      gsi_replace (&copy_gsi, new_call, false);
	      stmt = new_call;
	    }
	  else if (is_gimple_call (stmt)
		   && id->gimple_call
		   && (decl = gimple_call_fndecl (stmt))
		   && DECL_BUILT_IN_CLASS (decl) == BUILT_IN_NORMAL
		   && DECL_FUNCTION_CODE (decl) == BUILT_IN_VA_ARG_PACK_LEN)
	    {
	      /* __builtin_va_arg_pack_len () should be replaced by
		 the number of anonymous arguments.  */
	      size_t nargs = gimple_call_num_args (id->gimple_call);
	      tree count, p;
	      gimple new_stmt;

	      for (p = DECL_ARGUMENTS (id->src_fn); p; p = DECL_CHAIN (p))
		nargs--;

	      count = build_int_cst (integer_type_node, nargs);
	      new_stmt = gimple_build_assign (gimple_call_lhs (stmt), count);
	      gsi_replace (&copy_gsi, new_stmt, false);
	      stmt = new_stmt;
	    }

	  /* Statements produced by inlining can be unfolded, especially
	     when we constant propagated some operands.  We can't fold
	     them right now for two reasons:
	     1) folding require SSA_NAME_DEF_STMTs to be correct
	     2) we can't change function calls to builtins.
	     So we just mark statement for later folding.  We mark
	     all new statements, instead just statements that has changed
	     by some nontrivial substitution so even statements made
	     foldable indirectly are updated.  If this turns out to be
	     expensive, copy_body can be told to watch for nontrivial
	     changes.  */
	  if (id->statements_to_fold)
	    pointer_set_insert (id->statements_to_fold, stmt);

	  /* We're duplicating a CALL_EXPR.  Find any corresponding
	     callgraph edges and update or duplicate them.  */
	  if (is_gimple_call (stmt))
	    {
	      struct cgraph_edge *edge;
	      int flags;

	      switch (id->transform_call_graph_edges)
		{
		case CB_CGE_DUPLICATE:
		  edge = cgraph_edge (id->src_node, orig_stmt);
		  if (edge)
		    {
		      int edge_freq = edge->frequency;
		      edge = cgraph_clone_edge (edge, id->dst_node, stmt,
					        gimple_uid (stmt),
					        REG_BR_PROB_BASE, CGRAPH_FREQ_BASE,
					        edge->frequency, true);
		      /* We could also just rescale the frequency, but
		         doing so would introduce roundoff errors and make
			 verifier unhappy.  */
		      edge->frequency
		        = compute_call_stmt_bb_frequency (id->dst_node->decl,
							  copy_basic_block);
		      if (dump_file
		      	  && profile_status_for_function (cfun) != PROFILE_ABSENT
			  && (edge_freq > edge->frequency + 10
			      || edge_freq < edge->frequency - 10))
			{
			  fprintf (dump_file, "Edge frequency estimated by "
			           "cgraph %i diverge from inliner's estimate %i\n",
			  	   edge_freq,
				   edge->frequency);
			  fprintf (dump_file,
			  	   "Orig bb: %i, orig bb freq %i, new bb freq %i\n",
				   bb->index,
				   bb->frequency,
				   copy_basic_block->frequency);
			}
		      stmt = cgraph_redirect_edge_call_stmt_to_callee (edge);
		    }
		  break;

		case CB_CGE_MOVE_CLONES:
		  cgraph_set_call_stmt_including_clones (id->dst_node,
							 orig_stmt, stmt);
		  edge = cgraph_edge (id->dst_node, stmt);
		  break;

		case CB_CGE_MOVE:
		  edge = cgraph_edge (id->dst_node, orig_stmt);
		  if (edge)
		    cgraph_set_call_stmt (edge, stmt);
		  break;

		default:
		  gcc_unreachable ();
		}

	      /* Constant propagation on argument done during inlining
		 may create new direct call.  Produce an edge for it.  */
	      if ((!edge
		   || (edge->indirect_inlining_edge
		       && id->transform_call_graph_edges == CB_CGE_MOVE_CLONES))
		  && (fn = gimple_call_fndecl (stmt)) != NULL)
		{
		  struct cgraph_node *dest = cgraph_node (fn);

		  /* We have missing edge in the callgraph.  This can happen
		     when previous inlining turned an indirect call into a
		     direct call by constant propagating arguments or we are
		     producing dead clone (for further cloning).  In all
		     other cases we hit a bug (incorrect node sharing is the
		     most common reason for missing edges).  */
		  gcc_assert (dest->needed || !dest->analyzed
			      || dest->address_taken
		  	      || !id->src_node->analyzed
			      || !id->dst_node->analyzed);
		  if (id->transform_call_graph_edges == CB_CGE_MOVE_CLONES)
		    cgraph_create_edge_including_clones
		      (id->dst_node, dest, orig_stmt, stmt, bb->count,
		       compute_call_stmt_bb_frequency (id->dst_node->decl,
		       				       copy_basic_block),
		       bb->loop_depth, CIF_ORIGINALLY_INDIRECT_CALL);
		  else
		    cgraph_create_edge (id->dst_node, dest, stmt,
					bb->count,
					compute_call_stmt_bb_frequency
					  (id->dst_node->decl, copy_basic_block),
					bb->loop_depth)->inline_failed
		      = CIF_ORIGINALLY_INDIRECT_CALL;
		  if (dump_file)
		    {
		      fprintf (dump_file, "Created new direct edge to %s\n",
			       cgraph_node_name (dest));
		    }
		}

	      flags = gimple_call_flags (stmt);
	      if (flags & ECF_MAY_BE_ALLOCA)
		cfun->calls_alloca = true;
	      if (flags & ECF_RETURNS_TWICE)
		cfun->calls_setjmp = true;
	    }

	  maybe_duplicate_eh_stmt_fn (cfun, stmt, id->src_cfun, orig_stmt,
				      id->eh_map, id->eh_lp_nr);

	  if (gimple_in_ssa_p (cfun) && !is_gimple_debug (stmt))
	    {
	      ssa_op_iter i;
	      tree def;

	      find_new_referenced_vars (gsi_stmt (copy_gsi));
	      FOR_EACH_SSA_TREE_OPERAND (def, stmt, i, SSA_OP_DEF)
		if (TREE_CODE (def) == SSA_NAME)
		  SSA_NAME_DEF_STMT (def) = stmt;
	    }

	  gsi_next (&copy_gsi);
	}
      while (!gsi_end_p (copy_gsi));

      copy_gsi = gsi_last_bb (copy_basic_block);
    }

  return copy_basic_block;
}

/* Inserting Single Entry Multiple Exit region in SSA form into code in SSA
   form is quite easy, since dominator relationship for old basic blocks does
   not change.

   There is however exception where inlining might change dominator relation
   across EH edges from basic block within inlined functions destinating
   to landing pads in function we inline into.

   The function fills in PHI_RESULTs of such PHI nodes if they refer
   to gimple regs.  Otherwise, the function mark PHI_RESULT of such
   PHI nodes for renaming.  For non-gimple regs, renaming is safe: the
   EH edges are abnormal and SSA_NAME_OCCURS_IN_ABNORMAL_PHI must be
   set, and this means that there will be no overlapping live ranges
   for the underlying symbol.

   This might change in future if we allow redirecting of EH edges and
   we might want to change way build CFG pre-inlining to include
   all the possible edges then.  */
static void
update_ssa_across_abnormal_edges (basic_block bb, basic_block ret_bb,
				  bool can_throw, bool nonlocal_goto)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    if (!e->dest->aux
	|| ((basic_block)e->dest->aux)->index == ENTRY_BLOCK)
      {
	gimple phi;
	gimple_stmt_iterator si;

	if (!nonlocal_goto)
	  gcc_assert (e->flags & EDGE_EH);

	if (!can_throw)
	  gcc_assert (!(e->flags & EDGE_EH));

	for (si = gsi_start_phis (e->dest); !gsi_end_p (si); gsi_next (&si))
	  {
	    edge re;

	    phi = gsi_stmt (si);

	    /* There shouldn't be any PHI nodes in the ENTRY_BLOCK.  */
	    gcc_assert (!e->dest->aux);

	    gcc_assert ((e->flags & EDGE_EH)
			|| SSA_NAME_OCCURS_IN_ABNORMAL_PHI (PHI_RESULT (phi)));

	    if (!is_gimple_reg (PHI_RESULT (phi)))
	      {
		mark_sym_for_renaming (SSA_NAME_VAR (PHI_RESULT (phi)));
		continue;
	      }

	    re = find_edge (ret_bb, e->dest);
	    gcc_assert (re);
	    gcc_assert ((re->flags & (EDGE_EH | EDGE_ABNORMAL))
			== (e->flags & (EDGE_EH | EDGE_ABNORMAL)));

	    SET_USE (PHI_ARG_DEF_PTR_FROM_EDGE (phi, e),
		     USE_FROM_PTR (PHI_ARG_DEF_PTR_FROM_EDGE (phi, re)));
	  }
      }
}


/* Copy edges from BB into its copy constructed earlier, scale profile
   accordingly.  Edges will be taken care of later.  Assume aux
   pointers to point to the copies of each BB.  Return true if any
   debug stmts are left after a statement that must end the basic block.  */

static bool
copy_edges_for_bb (basic_block bb, gcov_type count_scale, basic_block ret_bb)
{
  basic_block new_bb = (basic_block) bb->aux;
  edge_iterator ei;
  edge old_edge;
  gimple_stmt_iterator si;
  int flags;
  bool need_debug_cleanup = false;

  /* Use the indices from the original blocks to create edges for the
     new ones.  */
  FOR_EACH_EDGE (old_edge, ei, bb->succs)
    if (!(old_edge->flags & EDGE_EH))
      {
	edge new_edge;

	flags = old_edge->flags;

	/* Return edges do get a FALLTHRU flag when the get inlined.  */
	if (old_edge->dest->index == EXIT_BLOCK && !old_edge->flags
	    && old_edge->dest->aux != EXIT_BLOCK_PTR)
	  flags |= EDGE_FALLTHRU;
	new_edge = make_edge (new_bb, (basic_block) old_edge->dest->aux, flags);
	new_edge->count = old_edge->count * count_scale / REG_BR_PROB_BASE;
	new_edge->probability = old_edge->probability;
      }

  if (bb->index == ENTRY_BLOCK || bb->index == EXIT_BLOCK)
    return false;

  for (si = gsi_start_bb (new_bb); !gsi_end_p (si);)
    {
      gimple copy_stmt;
      bool can_throw, nonlocal_goto;

      copy_stmt = gsi_stmt (si);
      if (!is_gimple_debug (copy_stmt))
	{
	  update_stmt (copy_stmt);
	  if (gimple_in_ssa_p (cfun))
	    mark_symbols_for_renaming (copy_stmt);
	}

      /* Do this before the possible split_block.  */
      gsi_next (&si);

      /* If this tree could throw an exception, there are two
         cases where we need to add abnormal edge(s): the
         tree wasn't in a region and there is a "current
         region" in the caller; or the original tree had
         EH edges.  In both cases split the block after the tree,
         and add abnormal edge(s) as needed; we need both
         those from the callee and the caller.
         We check whether the copy can throw, because the const
         propagation can change an INDIRECT_REF which throws
         into a COMPONENT_REF which doesn't.  If the copy
         can throw, the original could also throw.  */
      can_throw = stmt_can_throw_internal (copy_stmt);
      nonlocal_goto = stmt_can_make_abnormal_goto (copy_stmt);

      if (can_throw || nonlocal_goto)
	{
	  if (!gsi_end_p (si))
	    {
	      while (!gsi_end_p (si) && is_gimple_debug (gsi_stmt (si)))
		gsi_next (&si);
	      if (gsi_end_p (si))
		need_debug_cleanup = true;
	    }
	  if (!gsi_end_p (si))
	    /* Note that bb's predecessor edges aren't necessarily
	       right at this point; split_block doesn't care.  */
	    {
	      edge e = split_block (new_bb, copy_stmt);

	      new_bb = e->dest;
	      new_bb->aux = e->src->aux;
	      si = gsi_start_bb (new_bb);
	    }
	}

      if (gimple_code (copy_stmt) == GIMPLE_EH_DISPATCH)
	make_eh_dispatch_edges (copy_stmt);
      else if (can_throw)
	make_eh_edges (copy_stmt);

      if (nonlocal_goto)
	make_abnormal_goto_edges (gimple_bb (copy_stmt), true);

      if ((can_throw || nonlocal_goto)
	  && gimple_in_ssa_p (cfun))
	update_ssa_across_abnormal_edges (gimple_bb (copy_stmt), ret_bb,
					  can_throw, nonlocal_goto);
    }
  return need_debug_cleanup;
}

/* Copy the PHIs.  All blocks and edges are copied, some blocks
   was possibly split and new outgoing EH edges inserted.
   BB points to the block of original function and AUX pointers links
   the original and newly copied blocks.  */

static void
copy_phis_for_bb (basic_block bb, copy_body_data *id)
{
  basic_block const new_bb = (basic_block) bb->aux;
  edge_iterator ei;
  gimple phi;
  gimple_stmt_iterator si;
  edge new_edge;
  bool inserted = false;

  for (si = gsi_start (phi_nodes (bb)); !gsi_end_p (si); gsi_next (&si))
    {
      tree res, new_res;
      gimple new_phi;

      phi = gsi_stmt (si);
      res = PHI_RESULT (phi);
      new_res = res;
      if (is_gimple_reg (res))
	{
	  walk_tree (&new_res, copy_tree_body_r, id, NULL);
	  SSA_NAME_DEF_STMT (new_res)
	    = new_phi = create_phi_node (new_res, new_bb);
	  FOR_EACH_EDGE (new_edge, ei, new_bb->preds)
	    {
	      edge old_edge = find_edge ((basic_block) new_edge->src->aux, bb);
	      tree arg;
	      tree new_arg;
	      tree block = id->block;
	      edge_iterator ei2;

	      /* When doing partial cloning, we allow PHIs on the entry block
		 as long as all the arguments are the same.  Find any input
		 edge to see argument to copy.  */
	      if (!old_edge)
		FOR_EACH_EDGE (old_edge, ei2, bb->preds)
		  if (!old_edge->src->aux)
		    break;

	      arg = PHI_ARG_DEF_FROM_EDGE (phi, old_edge);
	      new_arg = arg;
	      id->block = NULL_TREE;
	      walk_tree (&new_arg, copy_tree_body_r, id, NULL);
	      id->block = block;
	      gcc_assert (new_arg);
	      /* With return slot optimization we can end up with
	         non-gimple (foo *)&this->m, fix that here.  */
	      if (TREE_CODE (new_arg) != SSA_NAME
		  && TREE_CODE (new_arg) != FUNCTION_DECL
		  && !is_gimple_val (new_arg))
		{
		  gimple_seq stmts = NULL;
		  new_arg = force_gimple_operand (new_arg, &stmts, true, NULL);
		  gsi_insert_seq_on_edge (new_edge, stmts);
		  inserted = true;
		}
	      add_phi_arg (new_phi, new_arg, new_edge,
			   gimple_phi_arg_location_from_edge (phi, old_edge));
	    }
	}
    }

  /* Commit the delayed edge insertions.  */
  if (inserted)
    FOR_EACH_EDGE (new_edge, ei, new_bb->preds)
      gsi_commit_one_edge_insert (new_edge, NULL);
}


/* Wrapper for remap_decl so it can be used as a callback.  */

static tree
remap_decl_1 (tree decl, void *data)
{
  return remap_decl (decl, (copy_body_data *) data);
}

/* Build struct function and associated datastructures for the new clone
   NEW_FNDECL to be build.  CALLEE_FNDECL is the original */

static void
initialize_cfun (tree new_fndecl, tree callee_fndecl, gcov_type count)
{
  struct function *src_cfun = DECL_STRUCT_FUNCTION (callee_fndecl);
  gcov_type count_scale;

  if (ENTRY_BLOCK_PTR_FOR_FUNCTION (src_cfun)->count)
    count_scale = (REG_BR_PROB_BASE * count
		   / ENTRY_BLOCK_PTR_FOR_FUNCTION (src_cfun)->count);
  else
    count_scale = REG_BR_PROB_BASE;

  /* Register specific tree functions.  */
  gimple_register_cfg_hooks ();

  /* Get clean struct function.  */
  push_struct_function (new_fndecl);

  /* We will rebuild these, so just sanity check that they are empty.  */
  gcc_assert (VALUE_HISTOGRAMS (cfun) == NULL);
  gcc_assert (cfun->local_decls == NULL);
  gcc_assert (cfun->cfg == NULL);
  gcc_assert (cfun->decl == new_fndecl);

  /* Copy items we preserve during cloning.  */
  cfun->static_chain_decl = src_cfun->static_chain_decl;
  cfun->nonlocal_goto_save_area = src_cfun->nonlocal_goto_save_area;
  cfun->function_end_locus = src_cfun->function_end_locus;
  cfun->curr_properties = src_cfun->curr_properties;
  cfun->last_verified = src_cfun->last_verified;
  cfun->va_list_gpr_size = src_cfun->va_list_gpr_size;
  cfun->va_list_fpr_size = src_cfun->va_list_fpr_size;
  cfun->has_nonlocal_label = src_cfun->has_nonlocal_label;
  cfun->stdarg = src_cfun->stdarg;
  cfun->dont_save_pending_sizes_p = src_cfun->dont_save_pending_sizes_p;
  cfun->after_inlining = src_cfun->after_inlining;
  cfun->can_throw_non_call_exceptions
    = src_cfun->can_throw_non_call_exceptions;
  cfun->returns_struct = src_cfun->returns_struct;
  cfun->returns_pcc_struct = src_cfun->returns_pcc_struct;
  cfun->after_tree_profile = src_cfun->after_tree_profile;

  init_empty_tree_cfg ();

  profile_status_for_function (cfun) = profile_status_for_function (src_cfun);
  ENTRY_BLOCK_PTR->count =
    (ENTRY_BLOCK_PTR_FOR_FUNCTION (src_cfun)->count * count_scale /
     REG_BR_PROB_BASE);
  ENTRY_BLOCK_PTR->frequency
    = ENTRY_BLOCK_PTR_FOR_FUNCTION (src_cfun)->frequency;
  EXIT_BLOCK_PTR->count =
    (EXIT_BLOCK_PTR_FOR_FUNCTION (src_cfun)->count * count_scale /
     REG_BR_PROB_BASE);
  EXIT_BLOCK_PTR->frequency =
    EXIT_BLOCK_PTR_FOR_FUNCTION (src_cfun)->frequency;
  if (src_cfun->eh)
    init_eh_for_function ();

  if (src_cfun->gimple_df)
    {
      init_tree_ssa (cfun);
      cfun->gimple_df->in_ssa_p = true;
      init_ssa_operands ();
    }
  pop_cfun ();
}

/* Helper function for copy_cfg_body.  Move debug stmts from the end
   of NEW_BB to the beginning of successor basic blocks when needed.  If the
   successor has multiple predecessors, reset them, otherwise keep
   their value.  */

static void
maybe_move_debug_stmts_to_successors (copy_body_data *id, basic_block new_bb)
{
  edge e;
  edge_iterator ei;
  gimple_stmt_iterator si = gsi_last_nondebug_bb (new_bb);

  if (gsi_end_p (si)
      || gsi_one_before_end_p (si)
      || !(stmt_can_throw_internal (gsi_stmt (si))
	   || stmt_can_make_abnormal_goto (gsi_stmt (si))))
    return;

  FOR_EACH_EDGE (e, ei, new_bb->succs)
    {
      gimple_stmt_iterator ssi = gsi_last_bb (new_bb);
      gimple_stmt_iterator dsi = gsi_after_labels (e->dest);
      while (is_gimple_debug (gsi_stmt (ssi)))
	{
	  gimple stmt = gsi_stmt (ssi), new_stmt;
	  tree var;
	  tree value;

	  /* For the last edge move the debug stmts instead of copying
	     them.  */
	  if (ei_one_before_end_p (ei))
	    {
	      si = ssi;
	      gsi_prev (&ssi);
	      if (!single_pred_p (e->dest))
		gimple_debug_bind_reset_value (stmt);
	      gsi_remove (&si, false);
	      gsi_insert_before (&dsi, stmt, GSI_SAME_STMT);
	      continue;
	    }

	  var = gimple_debug_bind_get_var (stmt);
	  if (single_pred_p (e->dest))
	    {
	      value = gimple_debug_bind_get_value (stmt);
	      value = unshare_expr (value);
	    }
	  else
	    value = NULL_TREE;
	  new_stmt = gimple_build_debug_bind (var, value, stmt);
	  gsi_insert_before (&dsi, new_stmt, GSI_SAME_STMT);
	  VEC_safe_push (gimple, heap, id->debug_stmts, new_stmt);
	  gsi_prev (&ssi);
	}
    }
}

/* Make a copy of the body of FN so that it can be inserted inline in
   another function.  Walks FN via CFG, returns new fndecl.  */

static tree
copy_cfg_body (copy_body_data * id, gcov_type count, int frequency_scale,
	       basic_block entry_block_map, basic_block exit_block_map,
	       bitmap blocks_to_copy, basic_block new_entry)
{
  tree callee_fndecl = id->src_fn;
  /* Original cfun for the callee, doesn't change.  */
  struct function *src_cfun = DECL_STRUCT_FUNCTION (callee_fndecl);
  struct function *cfun_to_copy;
  basic_block bb;
  tree new_fndecl = NULL;
  bool need_debug_cleanup = false;
  gcov_type count_scale;
  int last;
  int incoming_frequency = 0;
  gcov_type incoming_count = 0;

  if (ENTRY_BLOCK_PTR_FOR_FUNCTION (src_cfun)->count)
    count_scale = (REG_BR_PROB_BASE * count
		   / ENTRY_BLOCK_PTR_FOR_FUNCTION (src_cfun)->count);
  else
    count_scale = REG_BR_PROB_BASE;

  /* Register specific tree functions.  */
  gimple_register_cfg_hooks ();

  /* If we are inlining just region of the function, make sure to connect new entry
     to ENTRY_BLOCK_PTR.  Since new entry can be part of loop, we must compute
     frequency and probability of ENTRY_BLOCK_PTR based on the frequencies and
     probabilities of edges incoming from nonduplicated region.  */
  if (new_entry)
    {
      edge e;
      edge_iterator ei;

      FOR_EACH_EDGE (e, ei, new_entry->preds)
	if (!e->src->aux)
	  {
	    incoming_frequency += EDGE_FREQUENCY (e);
	    incoming_count += e->count;
	  }
      incoming_count = incoming_count * count_scale / REG_BR_PROB_BASE;
      incoming_frequency
	= incoming_frequency * frequency_scale / REG_BR_PROB_BASE;
      ENTRY_BLOCK_PTR->count = incoming_count;
      ENTRY_BLOCK_PTR->frequency = incoming_frequency;
    }

  /* Must have a CFG here at this point.  */
  gcc_assert (ENTRY_BLOCK_PTR_FOR_FUNCTION
	      (DECL_STRUCT_FUNCTION (callee_fndecl)));

  cfun_to_copy = id->src_cfun = DECL_STRUCT_FUNCTION (callee_fndecl);

  ENTRY_BLOCK_PTR_FOR_FUNCTION (cfun_to_copy)->aux = entry_block_map;
  EXIT_BLOCK_PTR_FOR_FUNCTION (cfun_to_copy)->aux = exit_block_map;
  entry_block_map->aux = ENTRY_BLOCK_PTR_FOR_FUNCTION (cfun_to_copy);
  exit_block_map->aux = EXIT_BLOCK_PTR_FOR_FUNCTION (cfun_to_copy);

  /* Duplicate any exception-handling regions.  */
  if (cfun->eh)
    id->eh_map = duplicate_eh_regions (cfun_to_copy, NULL, id->eh_lp_nr,
				       remap_decl_1, id);

  /* Use aux pointers to map the original blocks to copy.  */
  FOR_EACH_BB_FN (bb, cfun_to_copy)
    if (!blocks_to_copy || bitmap_bit_p (blocks_to_copy, bb->index))
      {
	basic_block new_bb = copy_bb (id, bb, frequency_scale, count_scale);
	bb->aux = new_bb;
	new_bb->aux = bb;
      }

  last = last_basic_block;

  /* Now that we've duplicated the blocks, duplicate their edges.  */
  FOR_ALL_BB_FN (bb, cfun_to_copy)
    if (!blocks_to_copy
        || (bb->index > 0 && bitmap_bit_p (blocks_to_copy, bb->index)))
      need_debug_cleanup |= copy_edges_for_bb (bb, count_scale, exit_block_map);

  if (new_entry)
    {
      edge e = make_edge (entry_block_map, (basic_block)new_entry->aux, EDGE_FALLTHRU);
      e->probability = REG_BR_PROB_BASE;
      e->count = incoming_count;
    }

  if (gimple_in_ssa_p (cfun))
    FOR_ALL_BB_FN (bb, cfun_to_copy)
      if (!blocks_to_copy
	  || (bb->index > 0 && bitmap_bit_p (blocks_to_copy, bb->index)))
	copy_phis_for_bb (bb, id);

  FOR_ALL_BB_FN (bb, cfun_to_copy)
    if (bb->aux)
      {
	if (need_debug_cleanup
	    && bb->index != ENTRY_BLOCK
	    && bb->index != EXIT_BLOCK)
	  maybe_move_debug_stmts_to_successors (id, (basic_block) bb->aux);
	((basic_block)bb->aux)->aux = NULL;
	bb->aux = NULL;
      }

  /* Zero out AUX fields of newly created block during EH edge
     insertion. */
  for (; last < last_basic_block; last++)
    {
      if (need_debug_cleanup)
	maybe_move_debug_stmts_to_successors (id, BASIC_BLOCK (last));
      BASIC_BLOCK (last)->aux = NULL;
    }
  entry_block_map->aux = NULL;
  exit_block_map->aux = NULL;

  if (id->eh_map)
    {
      pointer_map_destroy (id->eh_map);
      id->eh_map = NULL;
    }

  return new_fndecl;
}

/* Copy the debug STMT using ID.  We deal with these statements in a
   special way: if any variable in their VALUE expression wasn't
   remapped yet, we won't remap it, because that would get decl uids
   out of sync, causing codegen differences between -g and -g0.  If
   this arises, we drop the VALUE expression altogether.  */

static void
copy_debug_stmt (gimple stmt, copy_body_data *id)
{
  tree t, *n;
  struct walk_stmt_info wi;

  t = id->block;
  if (gimple_block (stmt))
    {
      tree *n;
      n = (tree *) pointer_map_contains (id->decl_map, gimple_block (stmt));
      if (n)
	t = *n;
    }
  gimple_set_block (stmt, t);

  /* Remap all the operands in COPY.  */
  memset (&wi, 0, sizeof (wi));
  wi.info = id;

  processing_debug_stmt = 1;

  t = gimple_debug_bind_get_var (stmt);

  if (TREE_CODE (t) == PARM_DECL && id->debug_map
      && (n = (tree *) pointer_map_contains (id->debug_map, t)))
    {
      gcc_assert (TREE_CODE (*n) == VAR_DECL);
      t = *n;
    }
  else if (TREE_CODE (t) == VAR_DECL
	   && !TREE_STATIC (t)
	   && gimple_in_ssa_p (cfun)
	   && !pointer_map_contains (id->decl_map, t)
	   && !var_ann (t))
    /* T is a non-localized variable.  */;
  else
    walk_tree (&t, remap_gimple_op_r, &wi, NULL);

  gimple_debug_bind_set_var (stmt, t);

  if (gimple_debug_bind_has_value_p (stmt))
    walk_tree (gimple_debug_bind_get_value_ptr (stmt),
	       remap_gimple_op_r, &wi, NULL);

  /* Punt if any decl couldn't be remapped.  */
  if (processing_debug_stmt < 0)
    gimple_debug_bind_reset_value (stmt);

  processing_debug_stmt = 0;

  update_stmt (stmt);
  if (gimple_in_ssa_p (cfun))
    mark_symbols_for_renaming (stmt);
}

/* Process deferred debug stmts.  In order to give values better odds
   of being successfully remapped, we delay the processing of debug
   stmts until all other stmts that might require remapping are
   processed.  */

static void
copy_debug_stmts (copy_body_data *id)
{
  size_t i;
  gimple stmt;

  if (!id->debug_stmts)
    return;

  FOR_EACH_VEC_ELT (gimple, id->debug_stmts, i, stmt)
    copy_debug_stmt (stmt, id);

  VEC_free (gimple, heap, id->debug_stmts);
}

/* Make a copy of the body of SRC_FN so that it can be inserted inline in
   another function.  */

static tree
copy_tree_body (copy_body_data *id)
{
  tree fndecl = id->src_fn;
  tree body = DECL_SAVED_TREE (fndecl);

  walk_tree (&body, copy_tree_body_r, id, NULL);

  return body;
}

/* Make a copy of the body of FN so that it can be inserted inline in
   another function.  */

static tree
copy_body (copy_body_data *id, gcov_type count, int frequency_scale,
	   basic_block entry_block_map, basic_block exit_block_map,
	   bitmap blocks_to_copy, basic_block new_entry)
{
  tree fndecl = id->src_fn;
  tree body;

  /* If this body has a CFG, walk CFG and copy.  */
  gcc_assert (ENTRY_BLOCK_PTR_FOR_FUNCTION (DECL_STRUCT_FUNCTION (fndecl)));
  body = copy_cfg_body (id, count, frequency_scale, entry_block_map, exit_block_map,
		        blocks_to_copy, new_entry);
  copy_debug_stmts (id);

  return body;
}

/* Estimate the cost of a memory move.  Use machine dependent
   word size and take possible memcpy call into account.  */

int
estimate_move_cost (tree type)
{
  HOST_WIDE_INT size;

  gcc_assert (!VOID_TYPE_P (type));

  if (TREE_CODE (type) == VECTOR_TYPE)
    {
      enum machine_mode inner = TYPE_MODE (TREE_TYPE (type));
      enum machine_mode simd
	= targetm.vectorize.preferred_simd_mode (inner);
      int simd_mode_size = GET_MODE_SIZE (simd);
      return ((GET_MODE_SIZE (TYPE_MODE (type)) + simd_mode_size - 1)
	      / simd_mode_size);
    }

  size = int_size_in_bytes (type);

  if (size < 0 || size > MOVE_MAX_PIECES * MOVE_RATIO (!optimize_size))
    /* Cost of a memcpy call, 3 arguments and the call.  */
    return 4;
  else
    return ((size + MOVE_MAX_PIECES - 1) / MOVE_MAX_PIECES);
}

/* Returns cost of operation CODE, according to WEIGHTS  */

static int
estimate_operator_cost (enum tree_code code, eni_weights *weights,
			tree op1 ATTRIBUTE_UNUSED, tree op2)
{
  switch (code)
    {
    /* These are "free" conversions, or their presumed cost
       is folded into other operations.  */
    case RANGE_EXPR:
    CASE_CONVERT:
    case COMPLEX_EXPR:
    case PAREN_EXPR:
    case VIEW_CONVERT_EXPR:
      return 0;

    /* Assign cost of 1 to usual operations.
       ??? We may consider mapping RTL costs to this.  */
    case COND_EXPR:
    case VEC_COND_EXPR:

    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case FMA_EXPR:

    case ADDR_SPACE_CONVERT_EXPR:
    case FIXED_CONVERT_EXPR:
    case FIX_TRUNC_EXPR:

    case NEGATE_EXPR:
    case FLOAT_EXPR:
    case MIN_EXPR:
    case MAX_EXPR:
    case ABS_EXPR:

    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
    case VEC_LSHIFT_EXPR:
    case VEC_RSHIFT_EXPR:

    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_AND_EXPR:
    case BIT_NOT_EXPR:

    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
    case TRUTH_NOT_EXPR:

    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case ORDERED_EXPR:
    case UNORDERED_EXPR:

    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:
    case UNGE_EXPR:
    case UNEQ_EXPR:
    case LTGT_EXPR:

    case CONJ_EXPR:

    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:

    case REALIGN_LOAD_EXPR:

    case REDUC_MAX_EXPR:
    case REDUC_MIN_EXPR:
    case REDUC_PLUS_EXPR:
    case WIDEN_SUM_EXPR:
    case WIDEN_MULT_EXPR:
    case DOT_PROD_EXPR:
    case WIDEN_MULT_PLUS_EXPR:
    case WIDEN_MULT_MINUS_EXPR:

    case VEC_WIDEN_MULT_HI_EXPR:
    case VEC_WIDEN_MULT_LO_EXPR:
    case VEC_UNPACK_HI_EXPR:
    case VEC_UNPACK_LO_EXPR:
    case VEC_UNPACK_FLOAT_HI_EXPR:
    case VEC_UNPACK_FLOAT_LO_EXPR:
    case VEC_PACK_TRUNC_EXPR:
    case VEC_PACK_SAT_EXPR:
    case VEC_PACK_FIX_TRUNC_EXPR:
    case VEC_EXTRACT_EVEN_EXPR:
    case VEC_EXTRACT_ODD_EXPR:
    case VEC_INTERLEAVE_HIGH_EXPR:
    case VEC_INTERLEAVE_LOW_EXPR:

      return 1;

    /* Few special cases of expensive operations.  This is useful
       to avoid inlining on functions having too many of these.  */
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case EXACT_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case RDIV_EXPR:
      if (TREE_CODE (op2) != INTEGER_CST)
        return weights->div_mod_cost;
      return 1;

    default:
      /* We expect a copy assignment with no operator.  */
      gcc_assert (get_gimple_rhs_class (code) == GIMPLE_SINGLE_RHS);
      return 0;
    }
}


/* Estimate number of instructions that will be created by expanding
   the statements in the statement sequence STMTS.
   WEIGHTS contains weights attributed to various constructs.  */

static
int estimate_num_insns_seq (gimple_seq stmts, eni_weights *weights)
{
  int cost;
  gimple_stmt_iterator gsi;

  cost = 0;
  for (gsi = gsi_start (stmts); !gsi_end_p (gsi); gsi_next (&gsi))
    cost += estimate_num_insns (gsi_stmt (gsi), weights);

  return cost;
}


/* Estimate number of instructions that will be created by expanding STMT.
   WEIGHTS contains weights attributed to various constructs.  */

int
estimate_num_insns (gimple stmt, eni_weights *weights)
{
  unsigned cost, i;
  enum gimple_code code = gimple_code (stmt);
  tree lhs;
  tree rhs;

  switch (code)
    {
    case GIMPLE_ASSIGN:
      /* Try to estimate the cost of assignments.  We have three cases to
	 deal with:
	 1) Simple assignments to registers;
	 2) Stores to things that must live in memory.  This includes
	    "normal" stores to scalars, but also assignments of large
	    structures, or constructors of big arrays;

	 Let us look at the first two cases, assuming we have "a = b + C":
	 <GIMPLE_ASSIGN <var_decl "a">
	        <plus_expr <var_decl "b"> <constant C>>
	 If "a" is a GIMPLE register, the assignment to it is free on almost
	 any target, because "a" usually ends up in a real register.  Hence
	 the only cost of this expression comes from the PLUS_EXPR, and we
	 can ignore the GIMPLE_ASSIGN.
	 If "a" is not a GIMPLE register, the assignment to "a" will most
	 likely be a real store, so the cost of the GIMPLE_ASSIGN is the cost
	 of moving something into "a", which we compute using the function
	 estimate_move_cost.  */
      lhs = gimple_assign_lhs (stmt);
      rhs = gimple_assign_rhs1 (stmt);

      if (is_gimple_reg (lhs))
	cost = 0;
      else
	cost = estimate_move_cost (TREE_TYPE (lhs));

      if (!is_gimple_reg (rhs) && !is_gimple_min_invariant (rhs))
	cost += estimate_move_cost (TREE_TYPE (rhs));

      cost += estimate_operator_cost (gimple_assign_rhs_code (stmt), weights,
      				      gimple_assign_rhs1 (stmt),
				      get_gimple_rhs_class (gimple_assign_rhs_code (stmt))
				      == GIMPLE_BINARY_RHS
				      ? gimple_assign_rhs2 (stmt) : NULL);
      break;

    case GIMPLE_COND:
      cost = 1 + estimate_operator_cost (gimple_cond_code (stmt), weights,
      				         gimple_op (stmt, 0),
				         gimple_op (stmt, 1));
      break;

    case GIMPLE_SWITCH:
      /* Take into account cost of the switch + guess 2 conditional jumps for
         each case label.

	 TODO: once the switch expansion logic is sufficiently separated, we can
	 do better job on estimating cost of the switch.  */
      if (weights->time_based)
        cost = floor_log2 (gimple_switch_num_labels (stmt)) * 2;
      else
        cost = gimple_switch_num_labels (stmt) * 2;
      break;

    case GIMPLE_CALL:
      {
	tree decl = gimple_call_fndecl (stmt);
	tree addr = gimple_call_fn (stmt);
	tree funtype = TREE_TYPE (addr);
	bool stdarg = false;

	if (POINTER_TYPE_P (funtype))
	  funtype = TREE_TYPE (funtype);

	/* Do not special case builtins where we see the body.
	   This just confuse inliner.  */
	if (!decl || cgraph_node (decl)->analyzed)
	  ;
	/* For buitins that are likely expanded to nothing or
	   inlined do not account operand costs.  */
	else if (is_simple_builtin (decl))
	  return 0;
	else if (is_inexpensive_builtin (decl))
	  return weights->target_builtin_call_cost;
	else if (DECL_BUILT_IN_CLASS (decl) == BUILT_IN_NORMAL)
	  {
	    /* We canonicalize x * x to pow (x, 2.0) with -ffast-math, so
	       specialize the cheap expansion we do here.
	       ???  This asks for a more general solution.  */
	    switch (DECL_FUNCTION_CODE (decl))
	      {
		case BUILT_IN_POW:
		case BUILT_IN_POWF:
		case BUILT_IN_POWL:
		  if (TREE_CODE (gimple_call_arg (stmt, 1)) == REAL_CST
		      && REAL_VALUES_EQUAL
			   (TREE_REAL_CST (gimple_call_arg (stmt, 1)), dconst2))
		    return estimate_operator_cost (MULT_EXPR, weights,
						   gimple_call_arg (stmt, 0),
						   gimple_call_arg (stmt, 0));
		  break;

		default:
		  break;
	      }
	  }

	cost = weights->call_cost;
	if (decl)
	  funtype = TREE_TYPE (decl);

	if (!VOID_TYPE_P (TREE_TYPE (funtype)))
	  cost += estimate_move_cost (TREE_TYPE (funtype));

	if (funtype)
	  stdarg = stdarg_p (funtype);

	/* Our cost must be kept in sync with
	   cgraph_estimate_size_after_inlining that does use function
	   declaration to figure out the arguments.

	   For functions taking variable list of arguments we must
	   look into call statement intself.  This is safe because
	   we will get only higher costs and in most cases we will
	   not inline these anyway.  */
	if (decl && DECL_ARGUMENTS (decl) && !stdarg)
	  {
	    tree arg;
	    for (arg = DECL_ARGUMENTS (decl); arg; arg = DECL_CHAIN (arg))
	      if (!VOID_TYPE_P (TREE_TYPE (arg)))
	        cost += estimate_move_cost (TREE_TYPE (arg));
	  }
	else if (funtype && prototype_p (funtype) && !stdarg)
	  {
	    tree t;
	    for (t = TYPE_ARG_TYPES (funtype); t && t != void_list_node;
	    	 t = TREE_CHAIN (t))
	      if (!VOID_TYPE_P (TREE_VALUE (t)))
	        cost += estimate_move_cost (TREE_VALUE (t));
	  }
	else
	  {
	    for (i = 0; i < gimple_call_num_args (stmt); i++)
	      {
		tree arg = gimple_call_arg (stmt, i);
	        if (!VOID_TYPE_P (TREE_TYPE (arg)))
		  cost += estimate_move_cost (TREE_TYPE (arg));
	      }
	  }

	break;
      }

    case GIMPLE_RETURN:
      return weights->return_cost;

    case GIMPLE_GOTO:
    case GIMPLE_LABEL:
    case GIMPLE_NOP:
    case GIMPLE_PHI:
    case GIMPLE_PREDICT:
    case GIMPLE_DEBUG:
      return 0;

    case GIMPLE_ASM:
      return asm_str_count (gimple_asm_string (stmt));

    case GIMPLE_RESX:
      /* This is either going to be an external function call with one
	 argument, or two register copy statements plus a goto.  */
      return 2;

    case GIMPLE_EH_DISPATCH:
      /* ??? This is going to turn into a switch statement.  Ideally
	 we'd have a look at the eh region and estimate the number of
	 edges involved.  */
      return 10;

    case GIMPLE_BIND:
      return estimate_num_insns_seq (gimple_bind_body (stmt), weights);

    case GIMPLE_EH_FILTER:
      return estimate_num_insns_seq (gimple_eh_filter_failure (stmt), weights);

    case GIMPLE_CATCH:
      return estimate_num_insns_seq (gimple_catch_handler (stmt), weights);

    case GIMPLE_TRY:
      return (estimate_num_insns_seq (gimple_try_eval (stmt), weights)
              + estimate_num_insns_seq (gimple_try_cleanup (stmt), weights));

    /* OpenMP directives are generally very expensive.  */

    case GIMPLE_OMP_RETURN:
    case GIMPLE_OMP_SECTIONS_SWITCH:
    case GIMPLE_OMP_ATOMIC_STORE:
    case GIMPLE_OMP_CONTINUE:
      /* ...except these, which are cheap.  */
      return 0;

    case GIMPLE_OMP_ATOMIC_LOAD:
      return weights->omp_cost;

    case GIMPLE_OMP_FOR:
      return (weights->omp_cost
              + estimate_num_insns_seq (gimple_omp_body (stmt), weights)
              + estimate_num_insns_seq (gimple_omp_for_pre_body (stmt), weights));

    case GIMPLE_OMP_PARALLEL:
    case GIMPLE_OMP_TASK:
    case GIMPLE_OMP_CRITICAL:
    case GIMPLE_OMP_MASTER:
    case GIMPLE_OMP_ORDERED:
    case GIMPLE_OMP_SECTION:
    case GIMPLE_OMP_SECTIONS:
    case GIMPLE_OMP_SINGLE:
      return (weights->omp_cost
              + estimate_num_insns_seq (gimple_omp_body (stmt), weights));

    default:
      gcc_unreachable ();
    }

  return cost;
}

/* Estimate number of instructions that will be created by expanding
   function FNDECL.  WEIGHTS contains weights attributed to various
   constructs.  */

int
estimate_num_insns_fn (tree fndecl, eni_weights *weights)
{
  struct function *my_function = DECL_STRUCT_FUNCTION (fndecl);
  gimple_stmt_iterator bsi;
  basic_block bb;
  int n = 0;

  gcc_assert (my_function && my_function->cfg);
  FOR_EACH_BB_FN (bb, my_function)
    {
      for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	n += estimate_num_insns (gsi_stmt (bsi), weights);
    }

  return n;
}


/* Initializes weights used by estimate_num_insns.  */

void
init_inline_once (void)
{
  eni_size_weights.call_cost = 1;
  eni_size_weights.target_builtin_call_cost = 1;
  eni_size_weights.div_mod_cost = 1;
  eni_size_weights.omp_cost = 40;
  eni_size_weights.time_based = false;
  eni_size_weights.return_cost = 1;

  /* Estimating time for call is difficult, since we have no idea what the
     called function does.  In the current uses of eni_time_weights,
     underestimating the cost does less harm than overestimating it, so
     we choose a rather small value here.  */
  eni_time_weights.call_cost = 10;
  eni_time_weights.target_builtin_call_cost = 1;
  eni_time_weights.div_mod_cost = 10;
  eni_time_weights.omp_cost = 40;
  eni_time_weights.time_based = true;
  eni_time_weights.return_cost = 2;
}

/* Estimate the number of instructions in a gimple_seq. */

int
count_insns_seq (gimple_seq seq, eni_weights *weights)
{
  gimple_stmt_iterator gsi;
  int n = 0;
  for (gsi = gsi_start (seq); !gsi_end_p (gsi); gsi_next (&gsi))
    n += estimate_num_insns (gsi_stmt (gsi), weights);

  return n;
}


/* Install new lexical TREE_BLOCK underneath 'current_block'.  */

static void
prepend_lexical_block (tree current_block, tree new_block)
{
  BLOCK_CHAIN (new_block) = BLOCK_SUBBLOCKS (current_block);
  BLOCK_SUBBLOCKS (current_block) = new_block;
  BLOCK_SUPERCONTEXT (new_block) = current_block;
}

/* Passed to walk_tree.  Copies the node pointed to, if appropriate.  */

tree
copy_tree_r (tree *tp, int *walk_subtrees, void *data ATTRIBUTE_UNUSED)
{
  enum tree_code code = TREE_CODE (*tp);
  enum tree_code_class cl = TREE_CODE_CLASS (code);

  /* We make copies of most nodes.  */
  if (IS_EXPR_CODE_CLASS (cl)
      || code == TREE_LIST
      || code == TREE_VEC
      || code == TYPE_DECL
      || code == OMP_CLAUSE)
    {
      /* Because the chain gets clobbered when we make a copy, we save it
	 here.  */
      tree chain = NULL_TREE, new_tree;

      chain = TREE_CHAIN (*tp);

      /* Copy the node.  */
      new_tree = copy_node (*tp);

      *tp = new_tree;

      /* Now, restore the chain, if appropriate.  That will cause
	 walk_tree to walk into the chain as well.  */
      if (code == PARM_DECL
	  || code == TREE_LIST
	  || code == OMP_CLAUSE)
	TREE_CHAIN (*tp) = chain;

      /* For now, we don't update BLOCKs when we make copies.  So, we
	 have to nullify all BIND_EXPRs.  */
      if (TREE_CODE (*tp) == BIND_EXPR)
	BIND_EXPR_BLOCK (*tp) = NULL_TREE;
    }
  else if (code == CONSTRUCTOR)
    {
      /* CONSTRUCTOR nodes need special handling because
         we need to duplicate the vector of elements.  */
      tree new_tree;

      new_tree = copy_node (*tp);

      CONSTRUCTOR_ELTS (new_tree) = VEC_copy (constructor_elt, gc,
					 CONSTRUCTOR_ELTS (*tp));
      *tp = new_tree;
    }
  else if (TREE_CODE_CLASS (code) == tcc_type)
    *walk_subtrees = 0;
  else if (TREE_CODE_CLASS (code) == tcc_declaration)
    *walk_subtrees = 0;
  else if (TREE_CODE_CLASS (code) == tcc_constant)
    *walk_subtrees = 0;
  else
    gcc_assert (code != STATEMENT_LIST);
  return NULL_TREE;
}

/* The SAVE_EXPR pointed to by TP is being copied.  If ST contains
   information indicating to what new SAVE_EXPR this one should be mapped,
   use that one.  Otherwise, create a new node and enter it in ST.  FN is
   the function into which the copy will be placed.  */

static void
remap_save_expr (tree *tp, void *st_, int *walk_subtrees)
{
  struct pointer_map_t *st = (struct pointer_map_t *) st_;
  tree *n;
  tree t;

  /* See if we already encountered this SAVE_EXPR.  */
  n = (tree *) pointer_map_contains (st, *tp);

  /* If we didn't already remap this SAVE_EXPR, do so now.  */
  if (!n)
    {
      t = copy_node (*tp);

      /* Remember this SAVE_EXPR.  */
      *pointer_map_insert (st, *tp) = t;
      /* Make sure we don't remap an already-remapped SAVE_EXPR.  */
      *pointer_map_insert (st, t) = t;
    }
  else
    {
      /* We've already walked into this SAVE_EXPR; don't do it again.  */
      *walk_subtrees = 0;
      t = *n;
    }

  /* Replace this SAVE_EXPR with the copy.  */
  *tp = t;
}

/* Called via walk_tree.  If *TP points to a DECL_STMT for a local label,
   copies the declaration and enters it in the splay_tree in DATA (which is
   really an `copy_body_data *').  */

static tree
mark_local_for_remap_r (tree *tp, int *walk_subtrees ATTRIBUTE_UNUSED,
			void *data)
{
  copy_body_data *id = (copy_body_data *) data;

  /* Don't walk into types.  */
  if (TYPE_P (*tp))
    *walk_subtrees = 0;

  else if (TREE_CODE (*tp) == LABEL_EXPR)
    {
      tree decl = TREE_OPERAND (*tp, 0);

      /* Copy the decl and remember the copy.  */
      insert_decl_map (id, decl, id->copy_decl (decl, id));
    }

  return NULL_TREE;
}

/* Perform any modifications to EXPR required when it is unsaved.  Does
   not recurse into EXPR's subtrees.  */

static void
unsave_expr_1 (tree expr)
{
  switch (TREE_CODE (expr))
    {
    case TARGET_EXPR:
      /* Don't mess with a TARGET_EXPR that hasn't been expanded.
         It's OK for this to happen if it was part of a subtree that
         isn't immediately expanded, such as operand 2 of another
         TARGET_EXPR.  */
      if (TREE_OPERAND (expr, 1))
	break;

      TREE_OPERAND (expr, 1) = TREE_OPERAND (expr, 3);
      TREE_OPERAND (expr, 3) = NULL_TREE;
      break;

    default:
      break;
    }
}

/* Called via walk_tree when an expression is unsaved.  Using the
   splay_tree pointed to by ST (which is really a `splay_tree'),
   remaps all local declarations to appropriate replacements.  */

static tree
unsave_r (tree *tp, int *walk_subtrees, void *data)
{
  copy_body_data *id = (copy_body_data *) data;
  struct pointer_map_t *st = id->decl_map;
  tree *n;

  /* Only a local declaration (variable or label).  */
  if ((TREE_CODE (*tp) == VAR_DECL && !TREE_STATIC (*tp))
      || TREE_CODE (*tp) == LABEL_DECL)
    {
      /* Lookup the declaration.  */
      n = (tree *) pointer_map_contains (st, *tp);

      /* If it's there, remap it.  */
      if (n)
	*tp = *n;
    }

  else if (TREE_CODE (*tp) == STATEMENT_LIST)
    gcc_unreachable ();
  else if (TREE_CODE (*tp) == BIND_EXPR)
    copy_bind_expr (tp, walk_subtrees, id);
  else if (TREE_CODE (*tp) == SAVE_EXPR
	   || TREE_CODE (*tp) == TARGET_EXPR)
    remap_save_expr (tp, st, walk_subtrees);
  else
    {
      copy_tree_r (tp, walk_subtrees, NULL);

      /* Do whatever unsaving is required.  */
      unsave_expr_1 (*tp);
    }

  /* Keep iterating.  */
  return NULL_TREE;
}

/* Copies everything in EXPR and replaces variables, labels
   and SAVE_EXPRs local to EXPR.  */

tree
unsave_expr_now (tree expr)
{
  copy_body_data id;

  /* There's nothing to do for NULL_TREE.  */
  if (expr == 0)
    return expr;

  /* Set up ID.  */
  memset (&id, 0, sizeof (id));
  id.src_fn = current_function_decl;
  id.dst_fn = current_function_decl;
  id.decl_map = pointer_map_create ();
  id.debug_map = NULL;

  id.copy_decl = copy_decl_no_change;
  id.transform_call_graph_edges = CB_CGE_DUPLICATE;
  id.transform_new_cfg = false;
  id.transform_return_to_modify = false;
  id.transform_lang_insert_block = NULL;

  /* Walk the tree once to find local labels.  */
  walk_tree_without_duplicates (&expr, mark_local_for_remap_r, &id);

  /* Walk the tree again, copying, remapping, and unsaving.  */
  walk_tree (&expr, unsave_r, &id, NULL);

  /* Clean up.  */
  pointer_map_destroy (id.decl_map);
  if (id.debug_map)
    pointer_map_destroy (id.debug_map);

  return expr;
}

/* Called via walk_gimple_seq.  If *GSIP points to a GIMPLE_LABEL for a local
   label, copies the declaration and enters it in the splay_tree in DATA (which
   is really a 'copy_body_data *'.  */

static tree
mark_local_labels_stmt (gimple_stmt_iterator *gsip,
		        bool *handled_ops_p ATTRIBUTE_UNUSED,
		        struct walk_stmt_info *wi)
{
  copy_body_data *id = (copy_body_data *) wi->info;
  gimple stmt = gsi_stmt (*gsip);

  if (gimple_code (stmt) == GIMPLE_LABEL)
    {
      tree decl = gimple_label_label (stmt);

      /* Copy the decl and remember the copy.  */
      insert_decl_map (id, decl, id->copy_decl (decl, id));
    }

  return NULL_TREE;
}


/* Called via walk_gimple_seq by copy_gimple_seq_and_replace_local.
   Using the splay_tree pointed to by ST (which is really a `splay_tree'),
   remaps all local declarations to appropriate replacements in gimple
   operands. */

static tree
replace_locals_op (tree *tp, int *walk_subtrees, void *data)
{
  struct walk_stmt_info *wi = (struct walk_stmt_info*) data;
  copy_body_data *id = (copy_body_data *) wi->info;
  struct pointer_map_t *st = id->decl_map;
  tree *n;
  tree expr = *tp;

  /* Only a local declaration (variable or label).  */
  if ((TREE_CODE (expr) == VAR_DECL
       && !TREE_STATIC (expr))
      || TREE_CODE (expr) == LABEL_DECL)
    {
      /* Lookup the declaration.  */
      n = (tree *) pointer_map_contains (st, expr);

      /* If it's there, remap it.  */
      if (n)
	*tp = *n;
      *walk_subtrees = 0;
    }
  else if (TREE_CODE (expr) == STATEMENT_LIST
	   || TREE_CODE (expr) == BIND_EXPR
	   || TREE_CODE (expr) == SAVE_EXPR)
    gcc_unreachable ();
  else if (TREE_CODE (expr) == TARGET_EXPR)
    {
      /* Don't mess with a TARGET_EXPR that hasn't been expanded.
         It's OK for this to happen if it was part of a subtree that
         isn't immediately expanded, such as operand 2 of another
         TARGET_EXPR.  */
      if (!TREE_OPERAND (expr, 1))
	{
	  TREE_OPERAND (expr, 1) = TREE_OPERAND (expr, 3);
	  TREE_OPERAND (expr, 3) = NULL_TREE;
	}
    }

  /* Keep iterating.  */
  return NULL_TREE;
}


/* Called via walk_gimple_seq by copy_gimple_seq_and_replace_local.
   Using the splay_tree pointed to by ST (which is really a `splay_tree'),
   remaps all local declarations to appropriate replacements in gimple
   statements. */

static tree
replace_locals_stmt (gimple_stmt_iterator *gsip,
		     bool *handled_ops_p ATTRIBUTE_UNUSED,
		     struct walk_stmt_info *wi)
{
  copy_body_data *id = (copy_body_data *) wi->info;
  gimple stmt = gsi_stmt (*gsip);

  if (gimple_code (stmt) == GIMPLE_BIND)
    {
      tree block = gimple_bind_block (stmt);

      if (block)
	{
	  remap_block (&block, id);
	  gimple_bind_set_block (stmt, block);
	}

      /* This will remap a lot of the same decls again, but this should be
	 harmless.  */
      if (gimple_bind_vars (stmt))
	gimple_bind_set_vars (stmt, remap_decls (gimple_bind_vars (stmt), NULL, id));
    }

  /* Keep iterating.  */
  return NULL_TREE;
}


/* Copies everything in SEQ and replaces variables and labels local to
   current_function_decl.  */

gimple_seq
copy_gimple_seq_and_replace_locals (gimple_seq seq)
{
  copy_body_data id;
  struct walk_stmt_info wi;
  struct pointer_set_t *visited;
  gimple_seq copy;

  /* There's nothing to do for NULL_TREE.  */
  if (seq == NULL)
    return seq;

  /* Set up ID.  */
  memset (&id, 0, sizeof (id));
  id.src_fn = current_function_decl;
  id.dst_fn = current_function_decl;
  id.decl_map = pointer_map_create ();
  id.debug_map = NULL;

  id.copy_decl = copy_decl_no_change;
  id.transform_call_graph_edges = CB_CGE_DUPLICATE;
  id.transform_new_cfg = false;
  id.transform_return_to_modify = false;
  id.transform_lang_insert_block = NULL;

  /* Walk the tree once to find local labels.  */
  memset (&wi, 0, sizeof (wi));
  visited = pointer_set_create ();
  wi.info = &id;
  wi.pset = visited;
  walk_gimple_seq (seq, mark_local_labels_stmt, NULL, &wi);
  pointer_set_destroy (visited);

  copy = gimple_seq_copy (seq);

  /* Walk the copy, remapping decls.  */
  memset (&wi, 0, sizeof (wi));
  wi.info = &id;
  walk_gimple_seq (copy, replace_locals_stmt, replace_locals_op, &wi);

  /* Clean up.  */
  pointer_map_destroy (id.decl_map);
  if (id.debug_map)
    pointer_map_destroy (id.debug_map);

  return copy;
}


/* Allow someone to determine if SEARCH is a child of TOP from gdb.  */

static tree
debug_find_tree_1 (tree *tp, int *walk_subtrees ATTRIBUTE_UNUSED, void *data)
{
  if (*tp == data)
    return (tree) data;
  else
    return NULL;
}

DEBUG_FUNCTION bool
debug_find_tree (tree top, tree search)
{
  return walk_tree_without_duplicates (&top, debug_find_tree_1, search) != 0;
}


/* Declare the variables created by the inliner.  Add all the variables in
   VARS to BIND_EXPR.  */

static void
declare_inline_vars (tree block, tree vars)
{
  tree t;
  for (t = vars; t; t = DECL_CHAIN (t))
    {
      DECL_SEEN_IN_BIND_EXPR_P (t) = 1;
      gcc_assert (!TREE_STATIC (t) && !TREE_ASM_WRITTEN (t));
      add_local_decl (cfun, t);
    }

  if (block)
    BLOCK_VARS (block) = chainon (BLOCK_VARS (block), vars);
}

/* Copy NODE (which must be a DECL).  The DECL originally was in the FROM_FN,
   but now it will be in the TO_FN.  PARM_TO_VAR means enable PARM_DECL to
   VAR_DECL translation.  */

static tree
copy_decl_for_dup_finish (copy_body_data *id, tree decl, tree copy)
{
  /* Don't generate debug information for the copy if we wouldn't have
     generated it for the copy either.  */
  DECL_ARTIFICIAL (copy) = DECL_ARTIFICIAL (decl);
  DECL_IGNORED_P (copy) = DECL_IGNORED_P (decl);

  /* Set the DECL_ABSTRACT_ORIGIN so the debugging routines know what
     declaration inspired this copy.  */
  DECL_ABSTRACT_ORIGIN (copy) = DECL_ORIGIN (decl);

  /* The new variable/label has no RTL, yet.  */
  if (CODE_CONTAINS_STRUCT (TREE_CODE (copy), TS_DECL_WRTL)
      && !TREE_STATIC (copy) && !DECL_EXTERNAL (copy))
    SET_DECL_RTL (copy, 0);

  /* These args would always appear unused, if not for this.  */
  TREE_USED (copy) = 1;

  /* Set the context for the new declaration.  */
  if (!DECL_CONTEXT (decl))
    /* Globals stay global.  */
    ;
  else if (DECL_CONTEXT (decl) != id->src_fn)
    /* Things that weren't in the scope of the function we're inlining
       from aren't in the scope we're inlining to, either.  */
    ;
  else if (TREE_STATIC (decl))
    /* Function-scoped static variables should stay in the original
       function.  */
    ;
  else
    /* Ordinary automatic local variables are now in the scope of the
       new function.  */
    DECL_CONTEXT (copy) = id->dst_fn;

  if (TREE_CODE (decl) == VAR_DECL
      /* C++ clones functions during parsing, before
	 referenced_vars.  */
      && gimple_referenced_vars (DECL_STRUCT_FUNCTION (id->src_fn))
      && referenced_var_lookup (DECL_STRUCT_FUNCTION (id->src_fn),
				DECL_UID (decl)))
    add_referenced_var (copy);

  return copy;
}

static tree
copy_decl_to_var (tree decl, copy_body_data *id)
{
  tree copy, type;

  gcc_assert (TREE_CODE (decl) == PARM_DECL
	      || TREE_CODE (decl) == RESULT_DECL);

  type = TREE_TYPE (decl);

  copy = build_decl (DECL_SOURCE_LOCATION (id->dst_fn),
		     VAR_DECL, DECL_NAME (decl), type);
  if (DECL_PT_UID_SET_P (decl))
    SET_DECL_PT_UID (copy, DECL_PT_UID (decl));
  TREE_ADDRESSABLE (copy) = TREE_ADDRESSABLE (decl);
  TREE_READONLY (copy) = TREE_READONLY (decl);
  TREE_THIS_VOLATILE (copy) = TREE_THIS_VOLATILE (decl);
  DECL_GIMPLE_REG_P (copy) = DECL_GIMPLE_REG_P (decl);

  return copy_decl_for_dup_finish (id, decl, copy);
}

/* Like copy_decl_to_var, but create a return slot object instead of a
   pointer variable for return by invisible reference.  */

static tree
copy_result_decl_to_var (tree decl, copy_body_data *id)
{
  tree copy, type;

  gcc_assert (TREE_CODE (decl) == PARM_DECL
	      || TREE_CODE (decl) == RESULT_DECL);

  type = TREE_TYPE (decl);
  if (DECL_BY_REFERENCE (decl))
    type = TREE_TYPE (type);

  copy = build_decl (DECL_SOURCE_LOCATION (id->dst_fn),
		     VAR_DECL, DECL_NAME (decl), type);
  if (DECL_PT_UID_SET_P (decl))
    SET_DECL_PT_UID (copy, DECL_PT_UID (decl));
  TREE_READONLY (copy) = TREE_READONLY (decl);
  TREE_THIS_VOLATILE (copy) = TREE_THIS_VOLATILE (decl);
  if (!DECL_BY_REFERENCE (decl))
    {
      TREE_ADDRESSABLE (copy) = TREE_ADDRESSABLE (decl);
      DECL_GIMPLE_REG_P (copy) = DECL_GIMPLE_REG_P (decl);
    }

  return copy_decl_for_dup_finish (id, decl, copy);
}

tree
copy_decl_no_change (tree decl, copy_body_data *id)
{
  tree copy;

  copy = copy_node (decl);

  /* The COPY is not abstract; it will be generated in DST_FN.  */
  DECL_ABSTRACT (copy) = 0;
  lang_hooks.dup_lang_specific_decl (copy);

  /* TREE_ADDRESSABLE isn't used to indicate that a label's address has
     been taken; it's for internal bookkeeping in expand_goto_internal.  */
  if (TREE_CODE (copy) == LABEL_DECL)
    {
      TREE_ADDRESSABLE (copy) = 0;
      LABEL_DECL_UID (copy) = -1;
    }

  return copy_decl_for_dup_finish (id, decl, copy);
}

/* EXP is CALL_EXPR present in a GENERIC expression tree.  Try to integrate
   the callee and return the inlined body on success.  */

tree
maybe_inline_call_in_expr (tree exp)
{
  tree fn = get_callee_fndecl (exp);

  /* We can only try to inline "const" functions.  */
  if (fn && TREE_READONLY (fn) && DECL_SAVED_TREE (fn))
    {
      struct pointer_map_t *decl_map = pointer_map_create ();
      call_expr_arg_iterator iter;
      copy_body_data id;
      tree param, arg, t;

      /* Remap the parameters.  */
      for (param = DECL_ARGUMENTS (fn), arg = first_call_expr_arg (exp, &iter);
	   param;
	   param = DECL_CHAIN (param), arg = next_call_expr_arg (&iter))
	*pointer_map_insert (decl_map, param) = arg;

      memset (&id, 0, sizeof (id));
      id.src_fn = fn;
      id.dst_fn = current_function_decl;
      id.src_cfun = DECL_STRUCT_FUNCTION (fn);
      id.decl_map = decl_map;

      id.copy_decl = copy_decl_no_change;
      id.transform_call_graph_edges = CB_CGE_DUPLICATE;
      id.transform_new_cfg = false;
      id.transform_return_to_modify = true;
      id.transform_lang_insert_block = false;

      /* Make sure not to unshare trees behind the front-end's back
	 since front-end specific mechanisms may rely on sharing.  */
      id.regimplify = false;
      id.do_not_unshare = true;

      /* We're not inside any EH region.  */
      id.eh_lp_nr = 0;

      t = copy_tree_body (&id);
      pointer_map_destroy (decl_map);

      /* We can only return something suitable for use in a GENERIC
	 expression tree.  */
      if (TREE_CODE (t) == MODIFY_EXPR)
	return TREE_OPERAND (t, 1);
    }

   return NULL_TREE;
}

EXTERN_C_END
