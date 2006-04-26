#include "config.h"
#include "system.h"
#include "tree.h"
#include "rtl.h"
#include "toplev.h"
#include "flags.h"
#include "m3-tree.h"
#include "function.h"
#include "expr.h"
#include "libfuncs.h"
#include "except.h"
#include "output.h"
#include "ggc.h"

extern int  m3cg_lineno;

static tree declare_external_proc PARAMS ((const char *, tree));
static tree name_type PARAMS ((tree, const char *));

void
m3_init_decl_processing ()
{
  /* Build the the common tree nodes.  Then build our M3 integer types,
     and adjust the common integer types correspondingly. */
  build_common_tree_nodes (1);
  t_int_8 = name_type (make_signed_type (8), "int_8");
  t_word_8 = name_type (make_unsigned_type (8), "word_8");
  t_int_16 = name_type (make_signed_type (16), "int_16");
  t_word_16 = name_type (make_unsigned_type (16), "word_16");
  t_int_32 = name_type (make_signed_type (32), "int_32");
  t_word_32 = name_type (make_unsigned_type (32), "word_32");
  t_int_64 = name_type (make_signed_type (64), "int_64");
  t_word_64 = name_type (make_unsigned_type (64), "word_64");
  if (BITS_PER_WORD == 32)
    {
      t_int_32d = t_int_32;
      t_word_32d = t_word_32;
    }
  else if (BITS_PER_WORD == 64)
    {
      t_int_32d = t_int_64;
      t_word_32d = t_word_64;
    }
  else
    {
      t_int_32d = name_type (make_signed_type (BITS_PER_WORD), "int_32d");
      t_word_32d = name_type (make_unsigned_type (BITS_PER_WORD), "word_32d");
    }
  t_int = t_int_32d;
  t_word = t_word_32d;
  integer_type_node = t_int;
  unsigned_type_node = t_word;
  char_type_node = t_int_8; 
  /* FIXME: do we need this, too? 
   * It seems we do; otherwise the compiler will abort when generating
   * stabs debugging information (ow 2003-01-19)
   */

  /* Set the type used for sizes and build the remaining common nodes. */
  set_sizetype (integer_type_node);
  build_common_tree_nodes_2 (0);

  /* Build the remaining M3-specific type and value nodes. */
  t_addr = name_type (ptr_type_node, "addr");
  t_reel = name_type (float_type_node, "reel");
  t_lreel = name_type (double_type_node, "lreel");
  /* XXX The M3 front end (m3middle/src/Target.m3) seems to treat extended
     reals the same as LONGREAL.  That may be due to limitations in other
     parts of the front end.  I don't know yet.  For now we likewise treat
     the xreel type as if it were lreel. */
#if 0
  t_xreel = name_type (long_double_type_node, "xreel");
#else
  t_xreel = name_type (double_type_node, "xreel");
#endif
  t_void = void_type_node;
  v_zero = integer_zero_node;
  v_one = integer_one_node;
  v_null = null_pointer_node;

  /* Register nodes with the garbage collector. */
  ggc_add_tree_root (m3_global_trees, sizeof m3_global_trees / sizeof (tree));
}

static tree
name_type (type, name)
     tree type;
     const char *name;
{
  TYPE_NAME (type) = build_decl (TYPE_DECL, get_identifier (name), type);
  return type;
}

tree
m3_build_type (t, s, a)
     m3_type t;
     int s;
     int a;
{
  switch (t)
    {

    case T_word:
      {
	switch (s)
	  {
	  case 0:
	    return t_word_32d;
	  case 8:
	    return t_word_8;
	  case 16:
	    return t_word_16;
	  case 32:
	    return t_word_32;
	  case 64:
	    return t_word_64;
	  default:
	    if (s == BITS_PER_WORD)
	      return t_word_32d;
	  }
	break;
      }

    case T_int:
      {
	switch (s)
	  {
	  case 0:
	    return t_int_32d;
	  case 8:
	    return t_int_8;
	  case 16:
	    return t_int_16;
	  case 32:
	    return t_int_32;
	  case 64:
	    return t_int_64;
	  default:
	    if (s == BITS_PER_WORD)
	      return t_int_32d;
	  }
	break;
      }

    case T_addr:
      return t_addr;
    case T_reel:
      return t_reel;
    case T_lreel:
      return t_lreel;
    case T_xreel:
      return t_xreel;
    case T_int_8:
      return t_int_8;
    case T_int_16:
      return t_int_16;
    case T_int_32:
      return t_int_32;
    case T_int_32d:
      return t_int_32d;
    case T_int_64:
      return t_int_64;
    case T_word_8:
      return t_word_8;
    case T_word_16:
      return t_word_16;
    case T_word_32:
      return t_word_32;
    case T_word_32d:
      return t_word_32d;
    case T_word_64:
      return t_word_64;
    case T_void:
      return t_void;

    case T_struct:
      {
	tree ts = make_node (RECORD_TYPE);
	TYPE_NAME (ts) = NULL_TREE;
	TYPE_FIELDS (ts) = NULL_TREE;
	TYPE_SIZE (ts) = bitsize_int (s);
	TYPE_SIZE_UNIT (ts) = size_int (s / BITS_PER_UNIT);

	TYPE_ALIGN (ts) = a;
	TYPE_MODE (ts) = mode_for_size (s, MODE_INT, 1);
	/* If structure's known alignment is less than
	   what the scalar mode would need, and it matters,
	   then stick with BLKmode.  */
	if (STRICT_ALIGNMENT && !(a >= BIGGEST_ALIGNMENT || (a >= s)))
	  {
	    if (TYPE_MODE (ts) != BLKmode)
	      /* If this is the only reason this type is BLKmode,
		 then don't force containing types to be BLKmode.  */
	      TYPE_NO_FORCE_BLK (ts) = 1;
	    TYPE_MODE (ts) = BLKmode;
	  }
	return ts;
      }

    default:
      break;
    } /*switch*/

  fatal_error ("Cannot build this type, m3cg_lineno %d", m3cg_lineno);
  /*NOTREACHED*/
}

/*===================================================== RUNTIME FUNCTIONS ===*/

void
m3_declare_runtime_functions ()
{
  tree t;

  t = build_function_type (t_addr,
			   tree_cons (NULL_TREE, t_addr,
				      tree_cons (NULL_TREE, t_addr,
						 tree_cons (NULL_TREE, t_int,
							    tree_cons (NULL_TREE, t_void, NULL_TREE)))));

  memcpy_proc = declare_external_proc ("memcpy", t);
  DECL_BUILT_IN_CLASS (memcpy_proc) = BUILT_IN_NORMAL;
  DECL_FUNCTION_CODE (memcpy_proc) = BUILT_IN_MEMCPY;

  memmove_proc = declare_external_proc ("memmove", t);

  t = build_function_type (t_addr,
			   tree_cons (NULL_TREE, t_addr,
				      tree_cons (NULL_TREE, t_int,
						 tree_cons (NULL_TREE, t_int,
							    tree_cons (NULL_TREE, t_void, NULL_TREE)))));
  memset_proc = declare_external_proc ("memset", t);
  DECL_BUILT_IN_CLASS (memset_proc) = BUILT_IN_NORMAL;
  DECL_FUNCTION_CODE (memset_proc) = BUILT_IN_MEMSET;

  t = build_function_type (t_int,
			   tree_cons (NULL_TREE, t_int,
				      tree_cons (NULL_TREE, t_int,
						 tree_cons (NULL_TREE, t_void, NULL_TREE))));
  div_proc = declare_external_proc ("m3_div", t);
  mod_proc = declare_external_proc ("m3_mod", t);

  t = build_function_type (t_void, NULL_TREE);
  set_union_proc  = declare_external_proc ("set_union", t);
  set_diff_proc   = declare_external_proc ("set_difference", t);
  set_inter_proc  = declare_external_proc ("set_intersection", t);
  set_sdiff_proc  = declare_external_proc ("set_sym_difference", t);
  set_sing_proc   = declare_external_proc ("set_singleton", t);
  set_range_proc  = declare_external_proc ("set_range", t);
  
  t = build_function_type (t_int, NULL_TREE);
  set_member_proc = declare_external_proc ("set_member", t);
  set_eq_proc = declare_external_proc ("set_eq", t);
  set_ne_proc = declare_external_proc ("set_ne", t);
  set_gt_proc = declare_external_proc ("set_gt", t);
  set_ge_proc = declare_external_proc ("set_ge", t);
  set_lt_proc = declare_external_proc ("set_lt", t);
  set_le_proc = declare_external_proc ("set_le", t);
}

static tree
declare_external_proc (name, typ)
     const char *name;
     tree typ;
{
  tree decl = build_decl (FUNCTION_DECL, get_identifier (name), typ);
  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  make_decl_rtl (decl, NULL);
  assemble_external (decl);
  TREE_USED (decl) = 1;
  return decl;
}
