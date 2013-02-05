/* Modula-3: modified */

/* Expand builtin functions.
   Copyright (C) 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
   2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011,
   2012 Free Software Foundation, Inc.

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
#include "machmode.h"
#include "rtl.h"
#include "tree.h"
#include "real.h"
#include "gimple.h"
#include "flags.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "except.h"
#include "function.h"
#include "insn-config.h"
#include "expr.h"
#include "optabs.h"
#include "libfuncs.h"
#include "recog.h"
#include "output.h"
#include "typeclass.h"
#include "predict.h"
#include "tm_p.h"
#include "target.h"
#include "langhooks.h"
#include "basic-block.h"
#include "tree-mudflap.h"
#include "tree-flow.h"
#include "value-prof.h"
#include "diagnostic-core.h"
#include "builtins.h"


#ifndef PAD_VARARGS_DOWN
#define PAD_VARARGS_DOWN BYTES_BIG_ENDIAN
#endif

struct target_builtins default_target_builtins;
#if SWITCHABLE_TARGET
struct target_builtins *this_target_builtins = &default_target_builtins;
#endif

/* Define the names of the builtin function types and codes.  */
const char *const built_in_class_names[4]
  = {"NOT_BUILT_IN", "BUILT_IN_FRONTEND", "BUILT_IN_MD", "BUILT_IN_NORMAL"};

#define DEF_BUILTIN(X, N, C, T, LT, B, F, NA, AT, IM, COND) #X,
const char * built_in_names[(int) END_BUILTINS] =
{
#include "builtins.def"
};
#undef DEF_BUILTIN

/* Setup an array of _DECL trees, make sure each element is
   initialized to NULL_TREE.  */
builtin_info_type builtin_info;

static const char *c_getstr (tree);
static rtx c_readstr (const char *, enum machine_mode);
static int target_char_cast (tree, char *);
static rtx get_memory_rtx (tree, tree);
static int apply_args_size (void);
static int apply_result_size (void);
#if defined (HAVE_untyped_call) || defined (HAVE_untyped_return)
static rtx result_vector (int, rtx);
#endif
static void expand_builtin_update_setjmp_buf (rtx);
static void expand_builtin_prefetch (tree);
static rtx expand_builtin_apply_args (void);
static rtx expand_builtin_apply_args_1 (void);
static rtx expand_builtin_apply (rtx, rtx, rtx);
static void expand_builtin_return (rtx);
static rtx expand_builtin_next_arg (void);
static rtx expand_builtin_va_start (tree);
static rtx expand_builtin_va_end (tree);
static rtx expand_builtin_va_copy (tree);
static rtx expand_builtin_memcmp (tree, rtx, enum machine_mode);
static rtx expand_builtin_strcmp (tree, rtx);
static rtx expand_builtin_strncmp (tree, rtx, enum machine_mode);
static rtx builtin_memcpy_read_str (void *, HOST_WIDE_INT, enum machine_mode);
static rtx expand_builtin_memcpy (tree, rtx);
static rtx builtin_memset_gen_str (void *, HOST_WIDE_INT, enum machine_mode);
static rtx expand_builtin_memset (tree, rtx, enum machine_mode);
static rtx expand_builtin_memset_args (tree, tree, tree, rtx, enum machine_mode, tree);
static rtx expand_builtin_bzero (tree);
static rtx expand_builtin_alloca (tree, bool);
static rtx expand_builtin_unop (enum machine_mode, tree, rtx, rtx, optab);
static rtx expand_builtin_frame_address (tree, tree);
static tree stabilize_va_list_loc (location_t, tree, int);
static rtx expand_builtin_expect (tree, rtx);
static bool validate_arg (const_tree, enum tree_code code);
static bool integer_valued_real_p (tree);
static bool readonly_data_expr (tree);
static tree fold_builtin_memory_op (location_t, tree, tree, tree, tree, bool, int);
static tree fold_builtin_memchr (location_t, tree, tree, tree, tree);
static tree fold_builtin_memcmp (location_t, tree, tree, tree);
static tree fold_builtin_unordered_cmp (location_t, tree, tree, tree, enum tree_code,
					enum tree_code);
static tree fold_builtin_n (location_t, tree, tree *, int, bool);
static tree fold_builtin_2 (location_t, tree, tree, tree, bool);
static tree fold_builtin_3 (location_t, tree, tree, tree, tree, bool);
static tree fold_builtin_4 (location_t, tree, tree, tree, tree, tree, bool);
#define fold_builtin_varargs(location_t, tree0, tree1, bool) (NULL_TREE)

static rtx expand_builtin_object_size (tree);
static rtx expand_builtin_memory_chk (tree, rtx, enum machine_mode,
				      enum built_in_function);
static void maybe_emit_chk_warning (tree, enum built_in_function);
static void maybe_emit_sprintf_chk_warning (tree, enum built_in_function);
static void maybe_emit_free_warning (tree);
static tree fold_builtin_object_size (tree, tree);
static bool init_target_chars (void);

static unsigned HOST_WIDE_INT target_newline;
static unsigned HOST_WIDE_INT target_percent;
static unsigned HOST_WIDE_INT target_c;
static unsigned HOST_WIDE_INT target_s;
static char target_percent_c[3];
static char target_percent_s[3];
static char target_percent_s_newline[4];
static void expand_builtin_sync_synchronize (void);

/* Return true if NAME starts with __builtin_ or __sync_.  */

static bool
is_builtin_name (const char *name)
{
  if (strncmp (name, "__builtin_", 10) == 0)
    return true;
  if (strncmp (name, "__sync_", 7) == 0)
    return true;
  if (strncmp (name, "__atomic_", 9) == 0)
    return true;
  return false;
}


/* Return true if DECL is a function symbol representing a built-in.  */

bool
is_builtin_fn (tree decl)
{
  return TREE_CODE (decl) == FUNCTION_DECL && DECL_BUILT_IN (decl);
}


/* Return true if NODE should be considered for inline expansion regardless
   of the optimization level.  This means whenever a function is invoked with
   its "internal" name, which normally contains the prefix "__builtin".  */

static bool
called_as_built_in (tree node)
{
  /* Note that we must use DECL_NAME, not DECL_ASSEMBLER_NAME_SET_P since
     we want the name used to call the function, not the name it
     will have. */
  const char *name = IDENTIFIER_POINTER (DECL_NAME (node));
  return is_builtin_name (name);
}

/* Compute values M and N such that M divides (address of EXP - N) and
   such that N < M.  Store N in *BITPOSP and return M.

   Note that the address (and thus the alignment) computed here is based
   on the address to which a symbol resolves, whereas DECL_ALIGN is based
   on the address at which an object is actually located.  These two
   addresses are not always the same.  For example, on ARM targets,
   the address &foo of a Thumb function foo() has the lowest bit set,
   whereas foo() itself starts on an even address.  */

unsigned int
get_object_alignment_1 (tree exp, unsigned HOST_WIDE_INT *bitposp)
{
  HOST_WIDE_INT bitsize, bitpos;
  tree offset;
  enum machine_mode mode;
  int unsignedp, volatilep;
  unsigned int align, inner;

  /* Get the innermost object and the constant (bitpos) and possibly
     variable (offset) offset of the access.  */
  exp = get_inner_reference (exp, &bitsize, &bitpos, &offset,
			     &mode, &unsignedp, &volatilep, true);

  /* Extract alignment information from the innermost object and
     possibly adjust bitpos and offset.  */
  if (TREE_CODE (exp) == CONST_DECL)
    exp = DECL_INITIAL (exp);
  if (DECL_P (exp)
      && TREE_CODE (exp) != LABEL_DECL)
    {
      if (TREE_CODE (exp) == FUNCTION_DECL)
	{
	  /* Function addresses can encode extra information besides their
	     alignment.  However, if TARGET_PTRMEMFUNC_VBIT_LOCATION
	     allows the low bit to be used as a virtual bit, we know
	     that the address itself must be 2-byte aligned.  */
	  if (TARGET_PTRMEMFUNC_VBIT_LOCATION == ptrmemfunc_vbit_in_pfn)
	    align = 2 * BITS_PER_UNIT;
	  else
	    align = BITS_PER_UNIT;
	}
      else
	align = DECL_ALIGN (exp);
    }
  else if (CONSTANT_CLASS_P (exp))
    {
      align = TYPE_ALIGN (TREE_TYPE (exp));
#ifdef CONSTANT_ALIGNMENT
      align = (unsigned)CONSTANT_ALIGNMENT (exp, align);
#endif
    }
  else if (TREE_CODE (exp) == VIEW_CONVERT_EXPR)
    align = TYPE_ALIGN (TREE_TYPE (exp));
  else if (TREE_CODE (exp) == INDIRECT_REF)
    align = TYPE_ALIGN (TREE_TYPE (exp));
  else if (TREE_CODE (exp) == MEM_REF)
    {
      tree addr = TREE_OPERAND (exp, 0);
      struct ptr_info_def *pi;
      if (TREE_CODE (addr) == BIT_AND_EXPR
	  && TREE_CODE (TREE_OPERAND (addr, 1)) == INTEGER_CST)
	{
	  align = (TREE_INT_CST_LOW (TREE_OPERAND (addr, 1))
		    & -TREE_INT_CST_LOW (TREE_OPERAND (addr, 1)));
	  align *= BITS_PER_UNIT;
	  addr = TREE_OPERAND (addr, 0);
	}
      else
	align = BITS_PER_UNIT;
      if (TREE_CODE (addr) == SSA_NAME
	  && (pi = SSA_NAME_PTR_INFO (addr)))
	{
	  bitpos += (pi->misalign * BITS_PER_UNIT) & ~(align - 1);
	  align = MAX (pi->align * BITS_PER_UNIT, align);
	}
      else if (TREE_CODE (addr) == ADDR_EXPR)
	align = MAX (align, get_object_alignment (TREE_OPERAND (addr, 0)));
      bitpos += mem_ref_offset (exp).low * BITS_PER_UNIT;
    }
  else if (TREE_CODE (exp) == TARGET_MEM_REF)
    {
      struct ptr_info_def *pi;
      tree addr = TMR_BASE (exp);
      if (TREE_CODE (addr) == BIT_AND_EXPR
	  && TREE_CODE (TREE_OPERAND (addr, 1)) == INTEGER_CST)
	{
	  align = (TREE_INT_CST_LOW (TREE_OPERAND (addr, 1))
		   & -TREE_INT_CST_LOW (TREE_OPERAND (addr, 1)));
	  align *= BITS_PER_UNIT;
	  addr = TREE_OPERAND (addr, 0);
	}
      else
	align = BITS_PER_UNIT;
      if (TREE_CODE (addr) == SSA_NAME
	  && (pi = SSA_NAME_PTR_INFO (addr)))
	{
	  bitpos += (pi->misalign * BITS_PER_UNIT) & ~(align - 1);
	  align = MAX (pi->align * BITS_PER_UNIT, align);
	}
      else if (TREE_CODE (addr) == ADDR_EXPR)
	align = MAX (align, get_object_alignment (TREE_OPERAND (addr, 0)));
      if (TMR_OFFSET (exp))
	bitpos += TREE_INT_CST_LOW (TMR_OFFSET (exp)) * BITS_PER_UNIT;
      if (TMR_INDEX (exp) && TMR_STEP (exp))
	{
	  unsigned HOST_WIDE_INT step = TREE_INT_CST_LOW (TMR_STEP (exp));
	  align = MIN (align, (step & -step) * BITS_PER_UNIT);
	}
      else if (TMR_INDEX (exp))
	align = BITS_PER_UNIT;
      if (TMR_INDEX2 (exp))
	align = BITS_PER_UNIT;
    }
  else
    align = BITS_PER_UNIT;

  /* If there is a non-constant offset part extract the maximum
     alignment that can prevail.  */
  inner = ~0U;
  while (offset)
    {
      tree next_offset;

      if (TREE_CODE (offset) == PLUS_EXPR)
	{
	  next_offset = TREE_OPERAND (offset, 0);
	  offset = TREE_OPERAND (offset, 1);
	}
      else
	next_offset = NULL;
      if (host_integerp (offset, 1))
	{
	  /* Any overflow in calculating offset_bits won't change
	     the alignment.  */
	  unsigned offset_bits
	    = ((unsigned) tree_low_cst (offset, 1) * BITS_PER_UNIT);

	  if (offset_bits)
	    inner = MIN (inner, (offset_bits & -offset_bits));
	}
      else if (TREE_CODE (offset) == MULT_EXPR
	       && host_integerp (TREE_OPERAND (offset, 1), 1))
	{
	  /* Any overflow in calculating offset_factor won't change
	     the alignment.  */
	  unsigned offset_factor
	    = ((unsigned) tree_low_cst (TREE_OPERAND (offset, 1), 1)
	       * BITS_PER_UNIT);

	  if (offset_factor)
	    inner = MIN (inner, (offset_factor & -offset_factor));
	}
      else
	{
	  inner = MIN (inner, BITS_PER_UNIT);
	  break;
	}
      offset = next_offset;
    }

  /* Alignment is innermost object alignment adjusted by the constant
     and non-constant offset parts.  */
  align = MIN (align, inner);
  bitpos = bitpos & (align - 1);

  *bitposp = bitpos;
  return align;
}

/* Return the alignment in bits of EXP, an object.  */

unsigned int
get_object_alignment (tree exp)
{
  unsigned HOST_WIDE_INT bitpos = 0;
  unsigned int align;

  align = get_object_alignment_1 (exp, &bitpos);

  /* align and bitpos now specify known low bits of the pointer.
     ptr & (align - 1) == bitpos.  */

  if (bitpos != 0)
    align = (bitpos & -bitpos);

  return align;
}

/* Return the alignment of object EXP, also considering its type when we do
   not know of explicit misalignment.  Only handle MEM_REF and TARGET_MEM_REF.

   ??? Note that, in the general case, the type of an expression is not kept
   consistent with misalignment information by the front-end, for example when
   taking the address of a member of a packed structure.  However, in most of
   the cases, expressions have the alignment of their type so we optimistically
   fall back to this alignment when we cannot compute a misalignment.  */

unsigned int
get_object_or_type_alignment (tree exp)
{
  unsigned HOST_WIDE_INT misalign;
  unsigned int align = get_object_alignment_1 (exp, &misalign);

  gcc_assert (TREE_CODE (exp) == MEM_REF || TREE_CODE (exp) == TARGET_MEM_REF);

  if (misalign != 0)
    align = (misalign & -misalign);
  else
    align = MAX (TYPE_ALIGN (TREE_TYPE (exp)), align);

  return align;
}

/* For a pointer valued expression EXP compute values M and N such that
   M divides (EXP - N) and such that N < M.  Store N in *BITPOSP and return M.

   If EXP is not a pointer, 0 is returned.  */

unsigned int
get_pointer_alignment_1 (tree exp, unsigned HOST_WIDE_INT *bitposp)
{
  STRIP_NOPS (exp);

  if (TREE_CODE (exp) == ADDR_EXPR)
    return get_object_alignment_1 (TREE_OPERAND (exp, 0), bitposp);
  else if (TREE_CODE (exp) == SSA_NAME
	   && POINTER_TYPE_P (TREE_TYPE (exp)))
    {
      struct ptr_info_def *pi = SSA_NAME_PTR_INFO (exp);
      if (!pi)
	{
	  *bitposp = 0;
	  return BITS_PER_UNIT;
	}
      *bitposp = pi->misalign * BITS_PER_UNIT;
      return pi->align * BITS_PER_UNIT;
    }

  *bitposp = 0;
  return POINTER_TYPE_P (TREE_TYPE (exp)) ? BITS_PER_UNIT : 0;
}

/* Return the alignment in bits of EXP, a pointer valued expression.
   The alignment returned is, by default, the alignment of the thing that
   EXP points to.  If it is not a POINTER_TYPE, 0 is returned.

   Otherwise, look at the expression to see if we can do better, i.e., if the
   expression is actually pointing at an object whose alignment is tighter.  */

unsigned int
get_pointer_alignment (tree exp)
{
  unsigned HOST_WIDE_INT bitpos = 0;
  unsigned int align;
  
  align = get_pointer_alignment_1 (exp, &bitpos);

  /* align and bitpos now specify known low bits of the pointer.
     ptr & (align - 1) == bitpos.  */

  if (bitpos != 0)
    align = (bitpos & -bitpos);

  return align;
}

/* Compute the length of a C string.  TREE_STRING_LENGTH is not the right
   way, because it could contain a zero byte in the middle.
   TREE_STRING_LENGTH is the size of the character array, not the string.

   ONLY_VALUE should be nonzero if the result is not going to be emitted
   into the instruction stream and zero if it is going to be expanded.
   E.g. with i++ ? "foo" : "bar", if ONLY_VALUE is nonzero, constant 3
   is returned, otherwise NULL, since
   len = c_strlen (src, 1); if (len) expand_expr (len, ...); would not
   evaluate the side-effects.

   The value returned is of type `ssizetype'.

   Unfortunately, string_constant can't access the values of const char
   arrays with initializers, so neither can we do so here.  */

tree
c_strlen (tree src, int only_value)
{
  tree offset_node;
  HOST_WIDE_INT offset;
  int max;
  const char *ptr;
  location_t loc;

  STRIP_NOPS (src);
  if (TREE_CODE (src) == COND_EXPR
      && (only_value || !TREE_SIDE_EFFECTS (TREE_OPERAND (src, 0))))
    {
      tree len1, len2;

      len1 = c_strlen (TREE_OPERAND (src, 1), only_value);
      len2 = c_strlen (TREE_OPERAND (src, 2), only_value);
      if (tree_int_cst_equal (len1, len2))
	return len1;
    }

  if (TREE_CODE (src) == COMPOUND_EXPR
      && (only_value || !TREE_SIDE_EFFECTS (TREE_OPERAND (src, 0))))
    return c_strlen (TREE_OPERAND (src, 1), only_value);

  loc = EXPR_LOC_OR_HERE (src);

  src = string_constant (src, &offset_node);
  if (src == 0)
    return NULL_TREE;

  max = TREE_STRING_LENGTH (src) - 1;
  ptr = TREE_STRING_POINTER (src);

  if (offset_node && TREE_CODE (offset_node) != INTEGER_CST)
    {
      /* If the string has an internal zero byte (e.g., "foo\0bar"), we can't
	 compute the offset to the following null if we don't know where to
	 start searching for it.  */
      int i;

      for (i = 0; i < max; i++)
	if (ptr[i] == 0)
	  return NULL_TREE;

      /* We don't know the starting offset, but we do know that the string
	 has no internal zero bytes.  We can assume that the offset falls
	 within the bounds of the string; otherwise, the programmer deserves
	 what he gets.  Subtract the offset from the length of the string,
	 and return that.  This would perhaps not be valid if we were dealing
	 with named arrays in addition to literal string constants.  */

      return size_diffop_loc (loc, size_int (max), offset_node);
    }

  /* We have a known offset into the string.  Start searching there for
     a null character if we can represent it as a single HOST_WIDE_INT.  */
  if (offset_node == 0)
    offset = 0;
  else if (! host_integerp (offset_node, 0))
    offset = -1;
  else
    offset = tree_low_cst (offset_node, 0);

  /* If the offset is known to be out of bounds, warn, and call strlen at
     runtime.  */
  if (offset < 0 || offset > max)
    {
     /* Suppress multiple warnings for propagated constant strings.  */
      if (! TREE_NO_WARNING (src))
        {
          warning_at (loc, 0, "offset outside bounds of constant string");
          TREE_NO_WARNING (src) = 1;
        }
      return NULL_TREE;
    }

  /* Use strlen to search for the first zero byte.  Since any strings
     constructed with build_string will have nulls appended, we win even
     if we get handed something like (char[4])"abcd".

     Since OFFSET is our starting index into the string, no further
     calculation is needed.  */
  return ssize_int (strlen (ptr + offset));
}

/* Return a char pointer for a C string if it is a string constant
   or sum of string constant and integer constant.  */

static const char *
c_getstr (tree src)
{
  tree offset_node;

  src = string_constant (src, &offset_node);
  if (src == 0)
    return 0;

  if (offset_node == 0)
    return TREE_STRING_POINTER (src);
  else if (!host_integerp (offset_node, 1)
	   || compare_tree_int (offset_node, TREE_STRING_LENGTH (src) - 1) > 0)
    return 0;

  return TREE_STRING_POINTER (src) + tree_low_cst (offset_node, 1);
}

/* Return a CONST_INT or CONST_DOUBLE corresponding to target reading
   GET_MODE_BITSIZE (MODE) bits from string constant STR.  */

static rtx
c_readstr (const char *str, enum machine_mode mode)
{
  HOST_WIDE_INT c[2];
  HOST_WIDE_INT ch;
  unsigned int i, j;

  gcc_assert (GET_MODE_CLASS (mode) == MODE_INT);

  c[0] = 0;
  c[1] = 0;
  ch = 1;
  for (i = 0; i < GET_MODE_SIZE (mode); i++)
    {
      j = i;
      if (WORDS_BIG_ENDIAN)
	j = GET_MODE_SIZE (mode) - i - 1;
      if (BYTES_BIG_ENDIAN != WORDS_BIG_ENDIAN
	  && GET_MODE_SIZE (mode) >= UNITS_PER_WORD)
	j = j + UNITS_PER_WORD - 2 * (j % UNITS_PER_WORD) - 1;
      j *= BITS_PER_UNIT;
      gcc_assert (j < 2 * HOST_BITS_PER_WIDE_INT);

      if (ch)
	ch = (unsigned char) str[i];
      c[j / HOST_BITS_PER_WIDE_INT] |= ch << (j % HOST_BITS_PER_WIDE_INT);
    }
  return immed_double_const (c[0], c[1], mode);
}

/* Cast a target constant CST to target CHAR and if that value fits into
   host char type, return zero and put that value into variable pointed to by
   P.  */

static int
target_char_cast (tree cst, char *p)
{
  unsigned HOST_WIDE_INT val, hostval;

  if (TREE_CODE (cst) != INTEGER_CST
      || CHAR_TYPE_SIZE > HOST_BITS_PER_WIDE_INT)
    return 1;

  val = TREE_INT_CST_LOW (cst);
  if (CHAR_TYPE_SIZE < HOST_BITS_PER_WIDE_INT)
    val &= (((unsigned HOST_WIDE_INT) 1) << CHAR_TYPE_SIZE) - 1;

  hostval = val;
  if (HOST_BITS_PER_CHAR < HOST_BITS_PER_WIDE_INT)
    hostval &= (((unsigned HOST_WIDE_INT) 1) << HOST_BITS_PER_CHAR) - 1;

  if (val != hostval)
    return 1;

  *p = hostval;
  return 0;
}

/* Similar to save_expr, but assumes that arbitrary code is not executed
   in between the multiple evaluations.  In particular, we assume that a
   non-addressable local variable will not be modified.  */

static tree
builtin_save_expr (tree exp)
{
  if (TREE_CODE (exp) == SSA_NAME
      || (TREE_ADDRESSABLE (exp) == 0
	  && (TREE_CODE (exp) == PARM_DECL
	      || (TREE_CODE (exp) == VAR_DECL && !TREE_STATIC (exp)))))
    return exp;

  return save_expr (exp);
}

/* Given TEM, a pointer to a stack frame, follow the dynamic chain COUNT
   times to get the address of either a higher stack frame, or a return
   address located within it (depending on FNDECL_CODE).  */

static rtx
expand_builtin_return_addr (enum built_in_function fndecl_code, int count)
{
  int i;

#ifdef INITIAL_FRAME_ADDRESS_RTX
  rtx tem = INITIAL_FRAME_ADDRESS_RTX;
#else
  rtx tem;

  /* For a zero count with __builtin_return_address, we don't care what
     frame address we return, because target-specific definitions will
     override us.  Therefore frame pointer elimination is OK, and using
     the soft frame pointer is OK.

     For a nonzero count, or a zero count with __builtin_frame_address,
     we require a stable offset from the current frame pointer to the
     previous one, so we must use the hard frame pointer, and
     we must disable frame pointer elimination.  */
  if (count == 0 && fndecl_code == BUILT_IN_RETURN_ADDRESS)
    tem = frame_pointer_rtx;
  else
    {
      tem = hard_frame_pointer_rtx;

      /* Tell reload not to eliminate the frame pointer.  */
      crtl->accesses_prior_frames = 1;
    }
#endif

  /* Some machines need special handling before we can access
     arbitrary frames.  For example, on the SPARC, we must first flush
     all register windows to the stack.  */
#ifdef SETUP_FRAME_ADDRESSES
  if (count > 0)
    SETUP_FRAME_ADDRESSES ();
#endif

  /* On the SPARC, the return address is not in the frame, it is in a
     register.  There is no way to access it off of the current frame
     pointer, but it can be accessed off the previous frame pointer by
     reading the value from the register window save area.  */
#ifdef RETURN_ADDR_IN_PREVIOUS_FRAME
  if (fndecl_code == BUILT_IN_RETURN_ADDRESS)
    count--;
#endif

  /* Scan back COUNT frames to the specified frame.  */
  for (i = 0; i < count; i++)
    {
      /* Assume the dynamic chain pointer is in the word that the
	 frame address points to, unless otherwise specified.  */
#ifdef DYNAMIC_CHAIN_ADDRESS
      tem = DYNAMIC_CHAIN_ADDRESS (tem);
#endif
      tem = memory_address (Pmode, tem);
      tem = gen_frame_mem (Pmode, tem);
      tem = copy_to_reg (tem);
    }

  /* For __builtin_frame_address, return what we've got.  But, on
     the SPARC for example, we may have to add a bias.  */
  if (fndecl_code == BUILT_IN_FRAME_ADDRESS)
#ifdef FRAME_ADDR_RTX
    return FRAME_ADDR_RTX (tem);
#else
    return tem;
#endif

  /* For __builtin_return_address, get the return address from that frame.  */
#ifdef RETURN_ADDR_RTX
  tem = RETURN_ADDR_RTX (count, tem);
#else
  tem = memory_address (Pmode,
			plus_constant (tem, GET_MODE_SIZE (Pmode)));
  tem = gen_frame_mem (Pmode, tem);
#endif
  return tem;
}

/* Alias set used for setjmp buffer.  */
static alias_set_type setjmp_alias_set = -1;

/* Construct the leading half of a __builtin_setjmp call.  Control will
   return to RECEIVER_LABEL.  This is also called directly by the SJLJ
   exception handling code.  */

void
expand_builtin_setjmp_setup (rtx buf_addr, rtx receiver_label)
{
  enum machine_mode sa_mode = STACK_SAVEAREA_MODE (SAVE_NONLOCAL);
  rtx stack_save;
  rtx mem;

  if (setjmp_alias_set == -1)
    setjmp_alias_set = new_alias_set ();

  buf_addr = convert_memory_address (Pmode, buf_addr);

  buf_addr = force_reg (Pmode, force_operand (buf_addr, NULL_RTX));

  /* We store the frame pointer and the address of receiver_label in
     the buffer and use the rest of it for the stack save area, which
     is machine-dependent.  */

  mem = gen_rtx_MEM (Pmode, buf_addr);
  set_mem_alias_set (mem, setjmp_alias_set);
  emit_move_insn (mem, targetm.builtin_setjmp_frame_value ());

  mem = gen_rtx_MEM (Pmode, plus_constant (buf_addr, GET_MODE_SIZE (Pmode))),
  set_mem_alias_set (mem, setjmp_alias_set);

  emit_move_insn (validize_mem (mem),
		  force_reg (Pmode, gen_rtx_LABEL_REF (Pmode, receiver_label)));

  stack_save = gen_rtx_MEM (sa_mode,
			    plus_constant (buf_addr,
					   2 * GET_MODE_SIZE (Pmode)));
  set_mem_alias_set (stack_save, setjmp_alias_set);
  emit_stack_save (SAVE_NONLOCAL, &stack_save);

  /* If there is further processing to do, do it.  */
#ifdef HAVE_builtin_setjmp_setup
  if (HAVE_builtin_setjmp_setup)
    emit_insn (gen_builtin_setjmp_setup (buf_addr));
#endif

  /* We have a nonlocal label.   */
  cfun->has_nonlocal_label = 1;
}

/* Construct the trailing part of a __builtin_setjmp call.  This is
   also called directly by the SJLJ exception handling code.  */

void
expand_builtin_setjmp_receiver (rtx receiver_label ATTRIBUTE_UNUSED)
{
  rtx chain;

  /* Clobber the FP when we get here, so we have to make sure it's
     marked as used by this function.  */
  emit_use (hard_frame_pointer_rtx);

  /* Mark the static chain as clobbered here so life information
     doesn't get messed up for it.  */
  chain = targetm.calls.static_chain (current_function_decl, true);
  if (chain && REG_P (chain))
    emit_clobber (chain);

  /* Now put in the code to restore the frame pointer, and argument
     pointer, if needed.  */
#ifdef HAVE_nonlocal_goto
  if (! HAVE_nonlocal_goto)
#endif
    {
      emit_move_insn (virtual_stack_vars_rtx, hard_frame_pointer_rtx);
      /* This might change the hard frame pointer in ways that aren't
	 apparent to early optimization passes, so force a clobber.  */
      emit_clobber (hard_frame_pointer_rtx);
    }

#if !HARD_FRAME_POINTER_IS_ARG_POINTER
  if (fixed_regs[ARG_POINTER_REGNUM])
    {
#ifdef ELIMINABLE_REGS
      size_t i;
      static const struct elims {const int from, to;} elim_regs[] = ELIMINABLE_REGS;

      for (i = 0; i < ARRAY_SIZE (elim_regs); i++)
	if (elim_regs[i].from == ARG_POINTER_REGNUM
	    && elim_regs[i].to == HARD_FRAME_POINTER_REGNUM)
	  break;

      if (i == ARRAY_SIZE (elim_regs))
#endif
	{
	  /* Now restore our arg pointer from the address at which it
	     was saved in our stack frame.  */
	  emit_move_insn (crtl->args.internal_arg_pointer,
			  copy_to_reg (get_arg_pointer_save_area ()));
	}
    }
#endif

#ifdef HAVE_builtin_setjmp_receiver
  if (HAVE_builtin_setjmp_receiver)
    emit_insn (gen_builtin_setjmp_receiver (receiver_label));
  else
#endif
#ifdef HAVE_nonlocal_goto_receiver
    if (HAVE_nonlocal_goto_receiver)
      emit_insn (gen_nonlocal_goto_receiver ());
    else
#endif
      { /* Nothing */ }

  /* We must not allow the code we just generated to be reordered by
     scheduling.  Specifically, the update of the frame pointer must
     happen immediately, not later.  */
  emit_insn (gen_blockage ());
}

/* __builtin_longjmp is passed a pointer to an array of five words (not
   all will be used on all machines).  It operates similarly to the C
   library function of the same name, but is more efficient.  Much of
   the code below is copied from the handling of non-local gotos.  */

static void
expand_builtin_longjmp (rtx buf_addr, rtx value)
{
  rtx fp, lab, stack, insn, last;
  enum machine_mode sa_mode = STACK_SAVEAREA_MODE (SAVE_NONLOCAL);

  /* DRAP is needed for stack realign if longjmp is expanded to current
     function  */
  if (SUPPORTS_STACK_ALIGNMENT)
    crtl->need_drap = true;

  if (setjmp_alias_set == -1)
    setjmp_alias_set = new_alias_set ();

  buf_addr = convert_memory_address (Pmode, buf_addr);

  buf_addr = force_reg (Pmode, buf_addr);

  /* We require that the user must pass a second argument of 1, because
     that is what builtin_setjmp will return.  */
  gcc_assert (value == const1_rtx);

  last = get_last_insn ();
#ifdef HAVE_builtin_longjmp
  if (HAVE_builtin_longjmp)
    emit_insn (gen_builtin_longjmp (buf_addr));
  else
#endif
    {
      fp = gen_rtx_MEM (Pmode, buf_addr);
      lab = gen_rtx_MEM (Pmode, plus_constant (buf_addr,
					       GET_MODE_SIZE (Pmode)));

      stack = gen_rtx_MEM (sa_mode, plus_constant (buf_addr,
						   2 * GET_MODE_SIZE (Pmode)));
      set_mem_alias_set (fp, setjmp_alias_set);
      set_mem_alias_set (lab, setjmp_alias_set);
      set_mem_alias_set (stack, setjmp_alias_set);

      /* Pick up FP, label, and SP from the block and jump.  This code is
	 from expand_goto in stmt.c; see there for detailed comments.  */
#ifdef HAVE_nonlocal_goto
      if (HAVE_nonlocal_goto)
	/* We have to pass a value to the nonlocal_goto pattern that will
	   get copied into the static_chain pointer, but it does not matter
	   what that value is, because builtin_setjmp does not use it.  */
	emit_insn (gen_nonlocal_goto (value, lab, stack, fp));
      else
#endif
	{
	  lab = copy_to_reg (lab);

	  emit_clobber (gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (VOIDmode)));
	  emit_clobber (gen_rtx_MEM (BLKmode, hard_frame_pointer_rtx));

	  emit_move_insn (hard_frame_pointer_rtx, fp);
	  emit_stack_restore (SAVE_NONLOCAL, stack);

	  emit_use (hard_frame_pointer_rtx);
	  emit_use (stack_pointer_rtx);
	  emit_indirect_jump (lab);
	}
    }

  /* Search backwards and mark the jump insn as a non-local goto.
     Note that this precludes the use of __builtin_longjmp to a
     __builtin_setjmp target in the same function.  However, we've
     already cautioned the user that these functions are for
     internal exception handling use only.  */
  for (insn = get_last_insn (); insn; insn = PREV_INSN (insn))
    {
      gcc_assert (insn != last);

      if (JUMP_P (insn))
	{
	  add_reg_note (insn, REG_NON_LOCAL_GOTO, const0_rtx);
	  break;
	}
      else if (CALL_P (insn))
	break;
    }
}

/* Expand a call to __builtin_nonlocal_goto.  We're passed the target label
   and the address of the save area.  */

static rtx
expand_builtin_nonlocal_goto (tree exp)
{
  tree t_label, t_save_area;
  rtx r_label, r_save_area, r_fp, r_sp, insn;

  if (!validate_arglist (exp, POINTER_TYPE, POINTER_TYPE, VOID_TYPE))
    return NULL_RTX;

  t_label = CALL_EXPR_ARG (exp, 0);
  t_save_area = CALL_EXPR_ARG (exp, 1);

  r_label = expand_normal (t_label);
  r_label = convert_memory_address (Pmode, r_label);
  r_save_area = expand_normal (t_save_area);
  r_save_area = convert_memory_address (Pmode, r_save_area);
  /* Copy the address of the save location to a register just in case it was
     based on the frame pointer.   */
  r_save_area = copy_to_reg (r_save_area);
  r_fp = gen_rtx_MEM (Pmode, r_save_area);
  r_sp = gen_rtx_MEM (STACK_SAVEAREA_MODE (SAVE_NONLOCAL),
		      plus_constant (r_save_area, GET_MODE_SIZE (Pmode)));

  crtl->has_nonlocal_goto = 1;

#ifdef HAVE_nonlocal_goto
  /* ??? We no longer need to pass the static chain value, afaik.  */
  if (HAVE_nonlocal_goto)
    emit_insn (gen_nonlocal_goto (const0_rtx, r_label, r_sp, r_fp));
  else
#endif
    {
      r_label = copy_to_reg (r_label);

      emit_clobber (gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (VOIDmode)));
      emit_clobber (gen_rtx_MEM (BLKmode, hard_frame_pointer_rtx));

      /* Restore frame pointer for containing function.  */
      emit_move_insn (hard_frame_pointer_rtx, r_fp);
      emit_stack_restore (SAVE_NONLOCAL, r_sp);

      /* USE of hard_frame_pointer_rtx added for consistency;
	 not clear if really needed.  */
      emit_use (hard_frame_pointer_rtx);
      emit_use (stack_pointer_rtx);

      /* If the architecture is using a GP register, we must
	 conservatively assume that the target function makes use of it.
	 The prologue of functions with nonlocal gotos must therefore
	 initialize the GP register to the appropriate value, and we
	 must then make sure that this value is live at the point
	 of the jump.  (Note that this doesn't necessarily apply
	 to targets with a nonlocal_goto pattern; they are free
	 to implement it in their own way.  Note also that this is
	 a no-op if the GP register is a global invariant.)  */
      if ((unsigned) PIC_OFFSET_TABLE_REGNUM != INVALID_REGNUM
	  && fixed_regs[PIC_OFFSET_TABLE_REGNUM])
	emit_use (pic_offset_table_rtx);

      emit_indirect_jump (r_label);
    }

  /* Search backwards to the jump insn and mark it as a
     non-local goto.  */
  for (insn = get_last_insn (); insn; insn = PREV_INSN (insn))
    {
      if (JUMP_P (insn))
	{
	  add_reg_note (insn, REG_NON_LOCAL_GOTO, const0_rtx);
	  break;
	}
      else if (CALL_P (insn))
	break;
    }

  return const0_rtx;
}

/* __builtin_update_setjmp_buf is passed a pointer to an array of five words
   (not all will be used on all machines) that was passed to __builtin_setjmp.
   It updates the stack pointer in that block to correspond to the current
   stack pointer.  */

static void
expand_builtin_update_setjmp_buf (rtx buf_addr)
{
  enum machine_mode sa_mode = STACK_SAVEAREA_MODE (SAVE_NONLOCAL);
  rtx stack_save
    = gen_rtx_MEM (sa_mode,
		   memory_address
		   (sa_mode,
		    plus_constant (buf_addr, 2 * GET_MODE_SIZE (Pmode))));

  emit_stack_save (SAVE_NONLOCAL, &stack_save);
}

/* Expand a call to __builtin_prefetch.  For a target that does not support
   data prefetch, evaluate the memory address argument in case it has side
   effects.  */

static void
expand_builtin_prefetch (tree exp)
{
  tree arg0, arg1, arg2;
  int nargs;
  rtx op0, op1, op2;

  if (!validate_arglist (exp, POINTER_TYPE, 0))
    return;

  arg0 = CALL_EXPR_ARG (exp, 0);

  /* Arguments 1 and 2 are optional; argument 1 (read/write) defaults to
     zero (read) and argument 2 (locality) defaults to 3 (high degree of
     locality).  */
  nargs = call_expr_nargs (exp);
  if (nargs > 1)
    arg1 = CALL_EXPR_ARG (exp, 1);
  else
    arg1 = integer_zero_node;
  if (nargs > 2)
    arg2 = CALL_EXPR_ARG (exp, 2);
  else
    arg2 = integer_three_node;

  /* Argument 0 is an address.  */
  op0 = expand_expr (arg0, NULL_RTX, Pmode, EXPAND_NORMAL);

  /* Argument 1 (read/write flag) must be a compile-time constant int.  */
  if (TREE_CODE (arg1) != INTEGER_CST)
    {
      error ("second argument to %<__builtin_prefetch%> must be a constant");
      arg1 = integer_zero_node;
    }
  op1 = expand_normal (arg1);
  /* Argument 1 must be either zero or one.  */
  if (INTVAL (op1) != 0 && INTVAL (op1) != 1)
    {
      warning (0, "invalid second argument to %<__builtin_prefetch%>;"
	       " using zero");
      op1 = const0_rtx;
    }

  /* Argument 2 (locality) must be a compile-time constant int.  */
  if (TREE_CODE (arg2) != INTEGER_CST)
    {
      error ("third argument to %<__builtin_prefetch%> must be a constant");
      arg2 = integer_zero_node;
    }
  op2 = expand_normal (arg2);
  /* Argument 2 must be 0, 1, 2, or 3.  */
  if (INTVAL (op2) < 0 || INTVAL (op2) > 3)
    {
      warning (0, "invalid third argument to %<__builtin_prefetch%>; using zero");
      op2 = const0_rtx;
    }

#ifdef HAVE_prefetch
  if (HAVE_prefetch)
    {
      struct expand_operand ops[3];

      create_address_operand (&ops[0], op0);
      create_integer_operand (&ops[1], INTVAL (op1));
      create_integer_operand (&ops[2], INTVAL (op2));
      if (maybe_expand_insn (CODE_FOR_prefetch, 3, ops))
	return;
    }
#endif

  /* Don't do anything with direct references to volatile memory, but
     generate code to handle other side effects.  */
  if (!MEM_P (op0) && side_effects_p (op0))
    emit_insn (op0);
}

/* Get a MEM rtx for expression EXP which is the address of an operand
   to be used in a string instruction (cmpstrsi, movmemsi, ..).  LEN is
   the maximum length of the block of memory that might be accessed or
   NULL if unknown.  */

static rtx
get_memory_rtx (tree exp, tree len)
{
  tree orig_exp = exp;
  rtx addr, mem;
  HOST_WIDE_INT off;

  /* When EXP is not resolved SAVE_EXPR, MEM_ATTRS can be still derived
     from its expression, for expr->a.b only <variable>.a.b is recorded.  */
  if (TREE_CODE (exp) == SAVE_EXPR && !SAVE_EXPR_RESOLVED_P (exp))
    exp = TREE_OPERAND (exp, 0);

  addr = expand_expr (orig_exp, NULL_RTX, ptr_mode, EXPAND_NORMAL);
  mem = gen_rtx_MEM (BLKmode, memory_address (BLKmode, addr));

  /* Get an expression we can use to find the attributes to assign to MEM.
     If it is an ADDR_EXPR, use the operand.  Otherwise, dereference it if
     we can.  First remove any nops.  */
  while (CONVERT_EXPR_P (exp)
	 && POINTER_TYPE_P (TREE_TYPE (TREE_OPERAND (exp, 0))))
    exp = TREE_OPERAND (exp, 0);

  off = 0;
  if (TREE_CODE (exp) == POINTER_PLUS_EXPR
      && TREE_CODE (TREE_OPERAND (exp, 0)) == ADDR_EXPR
      && host_integerp (TREE_OPERAND (exp, 1), 0)
      && (off = tree_low_cst (TREE_OPERAND (exp, 1), 0)) > 0)
    exp = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
  else if (TREE_CODE (exp) == ADDR_EXPR)
    exp = TREE_OPERAND (exp, 0);
  else if (POINTER_TYPE_P (TREE_TYPE (exp)))
    exp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (exp)), exp);
  else
    exp = NULL;

  /* Honor attributes derived from exp, except for the alias set
     (as builtin stringops may alias with anything) and the size
     (as stringops may access multiple array elements).  */
  if (exp)
    {
      set_mem_attributes (mem, exp, 0);

      if (off)
	mem = adjust_automodify_address_nv (mem, BLKmode, NULL, off);

      /* Allow the string and memory builtins to overflow from one
	 field into another, see http://gcc.gnu.org/PR23561.
	 Thus avoid COMPONENT_REFs in MEM_EXPR unless we know the whole
	 memory accessed by the string or memory builtin will fit
	 within the field.  */
      if (MEM_EXPR (mem) && TREE_CODE (MEM_EXPR (mem)) == COMPONENT_REF)
	{
	  tree mem_expr = MEM_EXPR (mem);
	  HOST_WIDE_INT offset = -1, length = -1;
	  tree inner = exp;

	  while (TREE_CODE (inner) == ARRAY_REF
		 || CONVERT_EXPR_P (inner)
		 || TREE_CODE (inner) == VIEW_CONVERT_EXPR
		 || TREE_CODE (inner) == SAVE_EXPR)
	    inner = TREE_OPERAND (inner, 0);

	  gcc_assert (TREE_CODE (inner) == COMPONENT_REF);

	  if (MEM_OFFSET_KNOWN_P (mem))
	    offset = MEM_OFFSET (mem);

	  if (offset >= 0 && len && host_integerp (len, 0))
	    length = tree_low_cst (len, 0);

	  while (TREE_CODE (inner) == COMPONENT_REF)
	    {
	      tree field = TREE_OPERAND (inner, 1);
	      gcc_assert (TREE_CODE (mem_expr) == COMPONENT_REF);
	      gcc_assert (field == TREE_OPERAND (mem_expr, 1));

	      /* Bitfields are generally not byte-addressable.  */
	      gcc_assert (!DECL_BIT_FIELD (field)
			  || ((tree_low_cst (DECL_FIELD_BIT_OFFSET (field), 1)
			       % BITS_PER_UNIT) == 0
			      && host_integerp (DECL_SIZE (field), 0)
			      && (TREE_INT_CST_LOW (DECL_SIZE (field))
				  % BITS_PER_UNIT) == 0));

	      /* If we can prove that the memory starting at XEXP (mem, 0) and
		 ending at XEXP (mem, 0) + LENGTH will fit into this field, we
		 can keep the COMPONENT_REF in MEM_EXPR.  But be careful with
		 fields without DECL_SIZE_UNIT like flexible array members.  */
	      if (length >= 0
		  && DECL_SIZE_UNIT (field)
		  && host_integerp (DECL_SIZE_UNIT (field), 0))
		{
		  HOST_WIDE_INT size
		    = TREE_INT_CST_LOW (DECL_SIZE_UNIT (field));
		  if (offset <= size
		      && length <= size
		      && offset + length <= size)
		    break;
		}

	      if (offset >= 0
		  && host_integerp (DECL_FIELD_OFFSET (field), 0))
		offset += TREE_INT_CST_LOW (DECL_FIELD_OFFSET (field))
			  + tree_low_cst (DECL_FIELD_BIT_OFFSET (field), 1)
			    / BITS_PER_UNIT;
	      else
		{
		  offset = -1;
		  length = -1;
		}

	      mem_expr = TREE_OPERAND (mem_expr, 0);
	      inner = TREE_OPERAND (inner, 0);
	    }

	  if (mem_expr == NULL)
	    offset = -1;
	  if (mem_expr != MEM_EXPR (mem))
	    {
	      set_mem_expr (mem, mem_expr);
	      if (offset >= 0)
		set_mem_offset (mem, offset);
	      else
		clear_mem_offset (mem);
	    }
	}
      set_mem_alias_set (mem, 0);
      clear_mem_size (mem);
    }

  return mem;
}

/* Built-in functions to perform an untyped call and return.  */

#define apply_args_mode \
  (this_target_builtins->x_apply_args_mode)
#define apply_result_mode \
  (this_target_builtins->x_apply_result_mode)

/* Return the size required for the block returned by __builtin_apply_args,
   and initialize apply_args_mode.  */

static int
apply_args_size (void)
{
  static int size = -1;
  int align;
  unsigned int regno;
  enum machine_mode mode;

  /* The values computed by this function never change.  */
  if (size < 0)
    {
      /* The first value is the incoming arg-pointer.  */
      size = GET_MODE_SIZE (Pmode);

      /* The second value is the structure value address unless this is
	 passed as an "invisible" first argument.  */
      if (targetm.calls.struct_value_rtx (cfun ? TREE_TYPE (cfun->decl) : 0, 0))
	size += GET_MODE_SIZE (Pmode);

      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	if (FUNCTION_ARG_REGNO_P (regno))
	  {
	    mode = targetm.calls.get_raw_arg_mode (regno);

	    gcc_assert (mode != VOIDmode);

	    align = GET_MODE_ALIGNMENT (mode) / BITS_PER_UNIT;
	    if (size % align != 0)
	      size = CEIL (size, align) * align;
	    size += GET_MODE_SIZE (mode);
	    apply_args_mode[regno] = mode;
	  }
	else
	  {
	    apply_args_mode[regno] = VOIDmode;
	  }
    }
  return size;
}

/* Return the size required for the block returned by __builtin_apply,
   and initialize apply_result_mode.  */

static int
apply_result_size (void)
{
  static int size = -1;
  int align, regno;
  enum machine_mode mode;

  /* The values computed by this function never change.  */
  if (size < 0)
    {
      size = 0;

      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	if (targetm.calls.function_value_regno_p (regno))
	  {
	    mode = targetm.calls.get_raw_result_mode (regno);

	    gcc_assert (mode != VOIDmode);

	    align = GET_MODE_ALIGNMENT (mode) / BITS_PER_UNIT;
	    if (size % align != 0)
	      size = CEIL (size, align) * align;
	    size += GET_MODE_SIZE (mode);
	    apply_result_mode[regno] = mode;
	  }
	else
	  apply_result_mode[regno] = VOIDmode;

      /* Allow targets that use untyped_call and untyped_return to override
	 the size so that machine-specific information can be stored here.  */
#ifdef APPLY_RESULT_SIZE
      size = APPLY_RESULT_SIZE;
#endif
    }
  return size;
}

#if defined (HAVE_untyped_call) || defined (HAVE_untyped_return)
/* Create a vector describing the result block RESULT.  If SAVEP is true,
   the result block is used to save the values; otherwise it is used to
   restore the values.  */

static rtx
result_vector (int savep, rtx result)
{
  int regno, size, align, nelts;
  enum machine_mode mode;
  rtx reg, mem;
  rtx *savevec = XALLOCAVEC (rtx, FIRST_PSEUDO_REGISTER);

  size = nelts = 0;
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if ((mode = apply_result_mode[regno]) != VOIDmode)
      {
	align = GET_MODE_ALIGNMENT (mode) / BITS_PER_UNIT;
	if (size % align != 0)
	  size = CEIL (size, align) * align;
	reg = gen_rtx_REG (mode, savep ? regno : INCOMING_REGNO (regno));
	mem = adjust_address (result, mode, size);
	savevec[nelts++] = (savep
			    ? gen_rtx_SET (VOIDmode, mem, reg)
			    : gen_rtx_SET (VOIDmode, reg, mem));
	size += GET_MODE_SIZE (mode);
      }
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (nelts, savevec));
}
#endif /* HAVE_untyped_call or HAVE_untyped_return */

/* Save the state required to perform an untyped call with the same
   arguments as were passed to the current function.  */

static rtx
expand_builtin_apply_args_1 (void)
{
  rtx registers, tem;
  int size, align, regno;
  enum machine_mode mode;
  rtx struct_incoming_value = targetm.calls.struct_value_rtx (cfun ? TREE_TYPE (cfun->decl) : 0, 1);

  /* Create a block where the arg-pointer, structure value address,
     and argument registers can be saved.  */
  registers = assign_stack_local (BLKmode, apply_args_size (), -1);

  /* Walk past the arg-pointer and structure value address.  */
  size = GET_MODE_SIZE (Pmode);
  if (targetm.calls.struct_value_rtx (cfun ? TREE_TYPE (cfun->decl) : 0, 0))
    size += GET_MODE_SIZE (Pmode);

  /* Save each register used in calling a function to the block.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if ((mode = apply_args_mode[regno]) != VOIDmode)
      {
	align = GET_MODE_ALIGNMENT (mode) / BITS_PER_UNIT;
	if (size % align != 0)
	  size = CEIL (size, align) * align;

	tem = gen_rtx_REG (mode, INCOMING_REGNO (regno));

	emit_move_insn (adjust_address (registers, mode, size), tem);
	size += GET_MODE_SIZE (mode);
      }

  /* Save the arg pointer to the block.  */
  tem = copy_to_reg (crtl->args.internal_arg_pointer);
#ifdef STACK_GROWS_DOWNWARD
  /* We need the pointer as the caller actually passed them to us, not
     as we might have pretended they were passed.  Make sure it's a valid
     operand, as emit_move_insn isn't expected to handle a PLUS.  */
  tem
    = force_operand (plus_constant (tem, crtl->args.pretend_args_size),
		     NULL_RTX);
#endif
  emit_move_insn (adjust_address (registers, Pmode, 0), tem);

  size = GET_MODE_SIZE (Pmode);

  /* Save the structure value address unless this is passed as an
     "invisible" first argument.  */
  if (struct_incoming_value)
    {
      emit_move_insn (adjust_address (registers, Pmode, size),
		      copy_to_reg (struct_incoming_value));
      size += GET_MODE_SIZE (Pmode);
    }

  /* Return the address of the block.  */
  return copy_addr_to_reg (XEXP (registers, 0));
}

/* __builtin_apply_args returns block of memory allocated on
   the stack into which is stored the arg pointer, structure
   value address, static chain, and all the registers that might
   possibly be used in performing a function call.  The code is
   moved to the start of the function so the incoming values are
   saved.  */

static rtx
expand_builtin_apply_args (void)
{
  /* Don't do __builtin_apply_args more than once in a function.
     Save the result of the first call and reuse it.  */
  if (apply_args_value != 0)
    return apply_args_value;
  {
    /* When this function is called, it means that registers must be
       saved on entry to this function.  So we migrate the
       call to the first insn of this function.  */
    rtx temp;
    rtx seq;

    start_sequence ();
    temp = expand_builtin_apply_args_1 ();
    seq = get_insns ();
    end_sequence ();

    apply_args_value = temp;

    /* Put the insns after the NOTE that starts the function.
       If this is inside a start_sequence, make the outer-level insn
       chain current, so the code is placed at the start of the
       function.  If internal_arg_pointer is a non-virtual pseudo,
       it needs to be placed after the function that initializes
       that pseudo.  */
    push_topmost_sequence ();
    if (REG_P (crtl->args.internal_arg_pointer)
	&& REGNO (crtl->args.internal_arg_pointer) > LAST_VIRTUAL_REGISTER)
      emit_insn_before (seq, parm_birth_insn);
    else
      emit_insn_before (seq, NEXT_INSN (entry_of_function ()));
    pop_topmost_sequence ();
    return temp;
  }
}

/* Perform an untyped call and save the state required to perform an
   untyped return of whatever value was returned by the given function.  */

static rtx
expand_builtin_apply (rtx function, rtx arguments, rtx argsize)
{
  int size, align, regno;
  enum machine_mode mode;
  rtx incoming_args, result, reg, dest, src, call_insn;
  rtx old_stack_level = 0;
  rtx call_fusage = 0;
  rtx struct_value = targetm.calls.struct_value_rtx (cfun ? TREE_TYPE (cfun->decl) : 0, 0);

  arguments = convert_memory_address (Pmode, arguments);

  /* Create a block where the return registers can be saved.  */
  result = assign_stack_local (BLKmode, apply_result_size (), -1);

  /* Fetch the arg pointer from the ARGUMENTS block.  */
  incoming_args = gen_reg_rtx (Pmode);
  emit_move_insn (incoming_args, gen_rtx_MEM (Pmode, arguments));
#ifndef STACK_GROWS_DOWNWARD
  incoming_args = expand_simple_binop (Pmode, MINUS, incoming_args, argsize,
				       incoming_args, 0, OPTAB_LIB_WIDEN);
#endif

  /* Push a new argument block and copy the arguments.  Do not allow
     the (potential) memcpy call below to interfere with our stack
     manipulations.  */
  do_pending_stack_adjust ();
  NO_DEFER_POP;

  /* Save the stack with nonlocal if available.  */
#ifdef HAVE_save_stack_nonlocal
  if (HAVE_save_stack_nonlocal)
    emit_stack_save (SAVE_NONLOCAL, &old_stack_level);
  else
#endif
    emit_stack_save (SAVE_BLOCK, &old_stack_level);

  /* Allocate a block of memory onto the stack and copy the memory
     arguments to the outgoing arguments address.  We can pass TRUE
     as the 4th argument because we just saved the stack pointer
     and will restore it right after the call.  */
  allocate_dynamic_stack_space (argsize, 0, BIGGEST_ALIGNMENT, true);

  /* Set DRAP flag to true, even though allocate_dynamic_stack_space
     may have already set current_function_calls_alloca to true.
     current_function_calls_alloca won't be set if argsize is zero,
     so we have to guarantee need_drap is true here.  */
  if (SUPPORTS_STACK_ALIGNMENT)
    crtl->need_drap = true;

  dest = virtual_outgoing_args_rtx;
#ifndef STACK_GROWS_DOWNWARD
  if (CONST_INT_P (argsize))
    dest = plus_constant (dest, -INTVAL (argsize));
  else
    dest = gen_rtx_PLUS (Pmode, dest, negate_rtx (Pmode, argsize));
#endif
  dest = gen_rtx_MEM (BLKmode, dest);
  set_mem_align (dest, PARM_BOUNDARY);
  src = gen_rtx_MEM (BLKmode, incoming_args);
  set_mem_align (src, PARM_BOUNDARY);
  emit_block_move (dest, src, argsize, BLOCK_OP_NORMAL);

  /* Refer to the argument block.  */
  apply_args_size ();
  arguments = gen_rtx_MEM (BLKmode, arguments);
  set_mem_align (arguments, PARM_BOUNDARY);

  /* Walk past the arg-pointer and structure value address.  */
  size = GET_MODE_SIZE (Pmode);
  if (struct_value)
    size += GET_MODE_SIZE (Pmode);

  /* Restore each of the registers previously saved.  Make USE insns
     for each of these registers for use in making the call.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if ((mode = apply_args_mode[regno]) != VOIDmode)
      {
	align = GET_MODE_ALIGNMENT (mode) / BITS_PER_UNIT;
	if (size % align != 0)
	  size = CEIL (size, align) * align;
	reg = gen_rtx_REG (mode, regno);
	emit_move_insn (reg, adjust_address (arguments, mode, size));
	use_reg (&call_fusage, reg);
	size += GET_MODE_SIZE (mode);
      }

  /* Restore the structure value address unless this is passed as an
     "invisible" first argument.  */
  size = GET_MODE_SIZE (Pmode);
  if (struct_value)
    {
      rtx value = gen_reg_rtx (Pmode);
      emit_move_insn (value, adjust_address (arguments, Pmode, size));
      emit_move_insn (struct_value, value);
      if (REG_P (struct_value))
	use_reg (&call_fusage, struct_value);
      size += GET_MODE_SIZE (Pmode);
    }

  /* All arguments and registers used for the call are set up by now!  */
  function = prepare_call_address (NULL, function, NULL, &call_fusage, 0, 0);

  /* Ensure address is valid.  SYMBOL_REF is already valid, so no need,
     and we don't want to load it into a register as an optimization,
     because prepare_call_address already did it if it should be done.  */
  if (GET_CODE (function) != SYMBOL_REF)
    function = memory_address (FUNCTION_MODE, function);

  /* Generate the actual call instruction and save the return value.  */
#ifdef HAVE_untyped_call
  if (HAVE_untyped_call)
    emit_call_insn (gen_untyped_call (gen_rtx_MEM (FUNCTION_MODE, function),
				      result, result_vector (1, result)));
  else
#endif
#ifdef HAVE_call_value
  if (HAVE_call_value)
    {
      rtx valreg = 0;

      /* Locate the unique return register.  It is not possible to
	 express a call that sets more than one return register using
	 call_value; use untyped_call for that.  In fact, untyped_call
	 only needs to save the return registers in the given block.  */
      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	if ((mode = apply_result_mode[regno]) != VOIDmode)
	  {
	    gcc_assert (!valreg); /* HAVE_untyped_call required.  */

	    valreg = gen_rtx_REG (mode, regno);
	  }

      emit_call_insn (GEN_CALL_VALUE (valreg,
				      gen_rtx_MEM (FUNCTION_MODE, function),
				      const0_rtx, NULL_RTX, const0_rtx));

      emit_move_insn (adjust_address (result, GET_MODE (valreg), 0), valreg);
    }
  else
#endif
    gcc_unreachable ();

  /* Find the CALL insn we just emitted, and attach the register usage
     information.  */
  call_insn = last_call_insn ();
  add_function_usage_to (call_insn, call_fusage);

  /* Restore the stack.  */
#ifdef HAVE_save_stack_nonlocal
  if (HAVE_save_stack_nonlocal)
    emit_stack_restore (SAVE_NONLOCAL, old_stack_level);
  else
#endif
    emit_stack_restore (SAVE_BLOCK, old_stack_level);
  fixup_args_size_notes (call_insn, get_last_insn(), 0);

  OK_DEFER_POP;

  /* Return the address of the result block.  */
  result = copy_addr_to_reg (XEXP (result, 0));
  return convert_memory_address (ptr_mode, result);
}

/* Perform an untyped return.  */

static void
expand_builtin_return (rtx result)
{
  int size, align, regno;
  enum machine_mode mode;
  rtx reg;
  rtx call_fusage = 0;

  result = convert_memory_address (Pmode, result);

  apply_result_size ();
  result = gen_rtx_MEM (BLKmode, result);

#ifdef HAVE_untyped_return
  if (HAVE_untyped_return)
    {
      emit_jump_insn (gen_untyped_return (result, result_vector (0, result)));
      emit_barrier ();
      return;
    }
#endif

  /* Restore the return value and note that each value is used.  */
  size = 0;
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if ((mode = apply_result_mode[regno]) != VOIDmode)
      {
	align = GET_MODE_ALIGNMENT (mode) / BITS_PER_UNIT;
	if (size % align != 0)
	  size = CEIL (size, align) * align;
	reg = gen_rtx_REG (mode, INCOMING_REGNO (regno));
	emit_move_insn (reg, adjust_address (result, mode, size));

	push_to_sequence (call_fusage);
	emit_use (reg);
	call_fusage = get_insns ();
	end_sequence ();
	size += GET_MODE_SIZE (mode);
      }

  /* Put the USE insns before the return.  */
  emit_insn (call_fusage);

  /* Return whatever values was restored by jumping directly to the end
     of the function.  */
  expand_naked_return ();
}

/* This helper macro, meant to be used in mathfn_built_in below,
   determines which among a set of three builtin math functions is
   appropriate for a given type mode.  The `F' and `L' cases are
   automatically generated from the `double' case.  */
#define CASE_MATHFN(BUILT_IN_MATHFN) \
  case BUILT_IN_MATHFN: case BUILT_IN_MATHFN##F: case BUILT_IN_MATHFN##L: \
  fcode = BUILT_IN_MATHFN; fcodef = BUILT_IN_MATHFN##F ; \
  fcodel = BUILT_IN_MATHFN##L ; break;
/* Similar to above, but appends _R after any F/L suffix.  */
#define CASE_MATHFN_REENT(BUILT_IN_MATHFN) \
  case BUILT_IN_MATHFN##_R: case BUILT_IN_MATHFN##F_R: case BUILT_IN_MATHFN##L_R: \
  fcode = BUILT_IN_MATHFN##_R; fcodef = BUILT_IN_MATHFN##F_R ; \
  fcodel = BUILT_IN_MATHFN##L_R ; break;

/* Return mathematic function equivalent to FN but operating directly on TYPE,
   if available.  If IMPLICIT is true use the implicit builtin declaration,
   otherwise use the explicit declaration.  If we can't do the conversion,
   return zero.  */

static tree
mathfn_built_in_1 (tree type, enum built_in_function fn, bool implicit_p)
{
  enum built_in_function fcode, fcodef, fcodel, fcode2;

  switch (fn)
    {
      CASE_MATHFN (BUILT_IN_ACOS)
      CASE_MATHFN (BUILT_IN_ACOSH)
      CASE_MATHFN (BUILT_IN_ASIN)
      CASE_MATHFN (BUILT_IN_ASINH)
      CASE_MATHFN (BUILT_IN_ATAN)
      CASE_MATHFN (BUILT_IN_ATAN2)
      CASE_MATHFN (BUILT_IN_ATANH)
      CASE_MATHFN (BUILT_IN_CBRT)
      CASE_MATHFN (BUILT_IN_CEIL)
      CASE_MATHFN (BUILT_IN_CEXPI)
      CASE_MATHFN (BUILT_IN_COPYSIGN)
      CASE_MATHFN (BUILT_IN_COS)
      CASE_MATHFN (BUILT_IN_COSH)
      CASE_MATHFN (BUILT_IN_DREM)
      CASE_MATHFN (BUILT_IN_ERF)
      CASE_MATHFN (BUILT_IN_ERFC)
      CASE_MATHFN (BUILT_IN_EXP)
      CASE_MATHFN (BUILT_IN_EXP10)
      CASE_MATHFN (BUILT_IN_EXP2)
      CASE_MATHFN (BUILT_IN_EXPM1)
      CASE_MATHFN (BUILT_IN_FABS)
      CASE_MATHFN (BUILT_IN_FDIM)
      CASE_MATHFN (BUILT_IN_FLOOR)
      CASE_MATHFN (BUILT_IN_FMA)
      CASE_MATHFN (BUILT_IN_FMAX)
      CASE_MATHFN (BUILT_IN_FMIN)
      CASE_MATHFN (BUILT_IN_FMOD)
      CASE_MATHFN (BUILT_IN_FREXP)
      CASE_MATHFN (BUILT_IN_GAMMA)
      CASE_MATHFN_REENT (BUILT_IN_GAMMA) /* GAMMA_R */
      CASE_MATHFN (BUILT_IN_HUGE_VAL)
      CASE_MATHFN (BUILT_IN_HYPOT)
      CASE_MATHFN (BUILT_IN_ILOGB)
      CASE_MATHFN (BUILT_IN_ICEIL)
      CASE_MATHFN (BUILT_IN_IFLOOR)
      CASE_MATHFN (BUILT_IN_INF)
      CASE_MATHFN (BUILT_IN_IRINT)
      CASE_MATHFN (BUILT_IN_IROUND)
      CASE_MATHFN (BUILT_IN_ISINF)
      CASE_MATHFN (BUILT_IN_J0)
      CASE_MATHFN (BUILT_IN_J1)
      CASE_MATHFN (BUILT_IN_JN)
      CASE_MATHFN (BUILT_IN_LCEIL)
      CASE_MATHFN (BUILT_IN_LDEXP)
      CASE_MATHFN (BUILT_IN_LFLOOR)
      CASE_MATHFN (BUILT_IN_LGAMMA)
      CASE_MATHFN_REENT (BUILT_IN_LGAMMA) /* LGAMMA_R */
      CASE_MATHFN (BUILT_IN_LLCEIL)
      CASE_MATHFN (BUILT_IN_LLFLOOR)
      CASE_MATHFN (BUILT_IN_LLRINT)
      CASE_MATHFN (BUILT_IN_LLROUND)
      CASE_MATHFN (BUILT_IN_LOG)
      CASE_MATHFN (BUILT_IN_LOG10)
      CASE_MATHFN (BUILT_IN_LOG1P)
      CASE_MATHFN (BUILT_IN_LOG2)
      CASE_MATHFN (BUILT_IN_LOGB)
      CASE_MATHFN (BUILT_IN_LRINT)
      CASE_MATHFN (BUILT_IN_LROUND)
      CASE_MATHFN (BUILT_IN_MODF)
      CASE_MATHFN (BUILT_IN_NAN)
      CASE_MATHFN (BUILT_IN_NANS)
      CASE_MATHFN (BUILT_IN_NEARBYINT)
      CASE_MATHFN (BUILT_IN_NEXTAFTER)
      CASE_MATHFN (BUILT_IN_NEXTTOWARD)
      CASE_MATHFN (BUILT_IN_POW)
      CASE_MATHFN (BUILT_IN_POWI)
      CASE_MATHFN (BUILT_IN_POW10)
      CASE_MATHFN (BUILT_IN_REMAINDER)
      CASE_MATHFN (BUILT_IN_REMQUO)
      CASE_MATHFN (BUILT_IN_RINT)
      CASE_MATHFN (BUILT_IN_ROUND)
      CASE_MATHFN (BUILT_IN_SCALB)
      CASE_MATHFN (BUILT_IN_SCALBLN)
      CASE_MATHFN (BUILT_IN_SCALBN)
      CASE_MATHFN (BUILT_IN_SIGNBIT)
      CASE_MATHFN (BUILT_IN_SIGNIFICAND)
      CASE_MATHFN (BUILT_IN_SIN)
      CASE_MATHFN (BUILT_IN_SINCOS)
      CASE_MATHFN (BUILT_IN_SINH)
      CASE_MATHFN (BUILT_IN_SQRT)
      CASE_MATHFN (BUILT_IN_TAN)
      CASE_MATHFN (BUILT_IN_TANH)
      CASE_MATHFN (BUILT_IN_TGAMMA)
      CASE_MATHFN (BUILT_IN_TRUNC)
      CASE_MATHFN (BUILT_IN_Y0)
      CASE_MATHFN (BUILT_IN_Y1)
      CASE_MATHFN (BUILT_IN_YN)

      default:
	return NULL_TREE;
      }

  if (TYPE_MAIN_VARIANT (type) == double_type_node)
    fcode2 = fcode;
  else if (TYPE_MAIN_VARIANT (type) == float_type_node)
    fcode2 = fcodef;
  else if (TYPE_MAIN_VARIANT (type) == long_double_type_node)
    fcode2 = fcodel;
  else
    return NULL_TREE;

  if (implicit_p && !builtin_decl_implicit_p (fcode2))
    return NULL_TREE;

  return builtin_decl_explicit (fcode2);
}

/* Like mathfn_built_in_1(), but always use the implicit array.  */

tree
mathfn_built_in (tree type, enum built_in_function fn)
{
  return mathfn_built_in_1 (type, fn, /*implicit=*/ 1);
}

/* Conveniently construct a function call expression.  FNDECL names the
   function to be called, N is the number of arguments, and the "..."
   parameters are the argument expressions.  Unlike build_call_exr
   this doesn't fold the call, hence it will always return a CALL_EXPR.  */

static tree
build_call_nofold_loc (location_t loc, tree fndecl, int n, ...)
{
  va_list ap;
  tree fntype = TREE_TYPE (fndecl);
  tree fn = build1 (ADDR_EXPR, build_pointer_type (fntype), fndecl);

  va_start (ap, n);
  fn = build_call_valist (TREE_TYPE (fntype), fn, n, ap);
  va_end (ap);
  SET_EXPR_LOCATION (fn, loc);
  return fn;
}

/* Callback routine for store_by_pieces.  Read GET_MODE_BITSIZE (MODE)
   bytes from constant string DATA + OFFSET and return it as target
   constant.  */

static rtx
builtin_memcpy_read_str (void *data, HOST_WIDE_INT offset,
			 enum machine_mode mode)
{
  const char *str = (const char *) data;

  gcc_assert (offset >= 0
	      && ((unsigned HOST_WIDE_INT) offset + GET_MODE_SIZE (mode)
		  <= strlen (str) + 1));

  return c_readstr (str + offset, mode);
}

/* Expand a call EXP to the memcpy builtin.
   Return NULL_RTX if we failed, the caller should emit a normal call,
   otherwise try to get the result in TARGET, if convenient (and in
   mode MODE if that's convenient).  */

static rtx
expand_builtin_memcpy (tree exp, rtx target)
{
  if (!validate_arglist (exp,
 			 POINTER_TYPE, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;
  else
    {
      tree dest = CALL_EXPR_ARG (exp, 0);
      tree src = CALL_EXPR_ARG (exp, 1);
      tree len = CALL_EXPR_ARG (exp, 2);
      const char *src_str;
      unsigned int src_align = get_pointer_alignment (src);
      unsigned int dest_align = get_pointer_alignment (dest);
      rtx dest_mem, src_mem, dest_addr, len_rtx;
      HOST_WIDE_INT expected_size = -1;
      unsigned int expected_align = 0;

      /* If DEST is not a pointer type, call the normal function.  */
      if (dest_align == 0)
	return NULL_RTX;

      /* If either SRC is not a pointer type, don't do this
	 operation in-line.  */
      if (src_align == 0)
	return NULL_RTX;

      if (currently_expanding_gimple_stmt)
        stringop_block_profile (currently_expanding_gimple_stmt,
				&expected_align, &expected_size);

      if (expected_align < dest_align)
	expected_align = dest_align;
      dest_mem = get_memory_rtx (dest, len);
      set_mem_align (dest_mem, dest_align);
      len_rtx = expand_normal (len);
      src_str = c_getstr (src);

      /* If SRC is a string constant and block move would be done
	 by pieces, we can avoid loading the string from memory
	 and only stored the computed constants.  */
      if (src_str
	  && CONST_INT_P (len_rtx)
	  && (unsigned HOST_WIDE_INT) INTVAL (len_rtx) <= strlen (src_str) + 1
	  && can_store_by_pieces (INTVAL (len_rtx), builtin_memcpy_read_str,
				  CONST_CAST (char *, src_str),
				  dest_align, false))
	{
	  dest_mem = store_by_pieces (dest_mem, INTVAL (len_rtx),
				      builtin_memcpy_read_str,
				      CONST_CAST (char *, src_str),
				      dest_align, false, 0);
	  dest_mem = force_operand (XEXP (dest_mem, 0), target);
	  dest_mem = convert_memory_address (ptr_mode, dest_mem);
	  return dest_mem;
	}

      src_mem = get_memory_rtx (src, len);
      set_mem_align (src_mem, src_align);

      /* Copy word part most expediently.  */
      dest_addr = emit_block_move_hints (dest_mem, src_mem, len_rtx,
				         CALL_EXPR_TAILCALL (exp)
				         ? BLOCK_OP_TAILCALL : BLOCK_OP_NORMAL,
					 expected_align, expected_size);

      if (dest_addr == 0)
	{
	  dest_addr = force_operand (XEXP (dest_mem, 0), target);
	  dest_addr = convert_memory_address (ptr_mode, dest_addr);
	}
      return dest_addr;
    }
}

#ifndef HAVE_movstr
# define HAVE_movstr 0
# define CODE_FOR_movstr CODE_FOR_nothing
#endif

/* Callback routine for store_by_pieces.  Read GET_MODE_BITSIZE (MODE)
   bytes from constant string DATA + OFFSET and return it as target
   constant.  */

rtx
builtin_strncpy_read_str (void *data, HOST_WIDE_INT offset,
			  enum machine_mode mode)
{
  const char *str = (const char *) data;

  if ((unsigned HOST_WIDE_INT) offset > strlen (str))
    return const0_rtx;

  return c_readstr (str + offset, mode);
}

/* Callback routine for store_by_pieces.  Read GET_MODE_BITSIZE (MODE)
   bytes from constant string DATA + OFFSET and return it as target
   constant.  */

rtx
builtin_memset_read_str (void *data, HOST_WIDE_INT offset ATTRIBUTE_UNUSED,
			 enum machine_mode mode)
{
  const char *c = (const char *) data;
  char *p = XALLOCAVEC (char, GET_MODE_SIZE (mode));

  memset (p, *c, GET_MODE_SIZE (mode));

  return c_readstr (p, mode);
}

/* Callback routine for store_by_pieces.  Return the RTL of a register
   containing GET_MODE_SIZE (MODE) consecutive copies of the unsigned
   char value given in the RTL register data.  For example, if mode is
   4 bytes wide, return the RTL for 0x01010101*data.  */

static rtx
builtin_memset_gen_str (void *data, HOST_WIDE_INT offset ATTRIBUTE_UNUSED,
			enum machine_mode mode)
{
  rtx target, coeff;
  size_t size;
  char *p;

  size = GET_MODE_SIZE (mode);
  if (size == 1)
    return (rtx) data;

  p = XALLOCAVEC (char, size);
  memset (p, 1, size);
  coeff = c_readstr (p, mode);

  target = convert_to_mode (mode, (rtx) data, 1);
  target = expand_mult (mode, target, coeff, NULL_RTX, 1);
  return force_reg (mode, target);
}

/* Expand expression EXP, which is a call to the memset builtin.  Return
   NULL_RTX if we failed the caller should emit a normal call, otherwise
   try to get the result in TARGET, if convenient (and in mode MODE if that's
   convenient).  */

static rtx
expand_builtin_memset (tree exp, rtx target, enum machine_mode mode)
{
  if (!validate_arglist (exp,
 			 POINTER_TYPE, INTEGER_TYPE, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;
  else
    {
      tree dest = CALL_EXPR_ARG (exp, 0);
      tree val = CALL_EXPR_ARG (exp, 1);
      tree len = CALL_EXPR_ARG (exp, 2);
      return expand_builtin_memset_args (dest, val, len, target, mode, exp);
    }
}

/* Helper function to do the actual work for expand_builtin_memset.  The
   arguments to the builtin_memset call DEST, VAL, and LEN are broken out
   so that this can also be called without constructing an actual CALL_EXPR.
   The other arguments and return value are the same as for
   expand_builtin_memset.  */

static rtx
expand_builtin_memset_args (tree dest, tree val, tree len,
			    rtx target, enum machine_mode mode, tree orig_exp)
{
  tree fndecl, fn;
  enum built_in_function fcode;
  enum machine_mode val_mode;
  char c;
  unsigned int dest_align;
  rtx dest_mem, dest_addr, len_rtx;
  HOST_WIDE_INT expected_size = -1;
  unsigned int expected_align = 0;

  dest_align = get_pointer_alignment (dest);

  /* If DEST is not a pointer type, don't do this operation in-line.  */
  if (dest_align == 0)
    return NULL_RTX;

  if (currently_expanding_gimple_stmt)
    stringop_block_profile (currently_expanding_gimple_stmt,
			    &expected_align, &expected_size);

  if (expected_align < dest_align)
    expected_align = dest_align;

  /* If the LEN parameter is zero, return DEST.  */
  if (integer_zerop (len))
    {
      /* Evaluate and ignore VAL in case it has side-effects.  */
      expand_expr (val, const0_rtx, VOIDmode, EXPAND_NORMAL);
      return expand_expr (dest, target, mode, EXPAND_NORMAL);
    }

  /* Stabilize the arguments in case we fail.  */
  dest = builtin_save_expr (dest);
  val = builtin_save_expr (val);
  len = builtin_save_expr (len);

  len_rtx = expand_normal (len);
  dest_mem = get_memory_rtx (dest, len);
  val_mode = TYPE_MODE (unsigned_char_type_node);

  if (TREE_CODE (val) != INTEGER_CST)
    {
      rtx val_rtx;

      val_rtx = expand_normal (val);
      val_rtx = convert_to_mode (val_mode, val_rtx, 0);

      /* Assume that we can memset by pieces if we can store
       * the coefficients by pieces (in the required modes).
       * We can't pass builtin_memset_gen_str as that emits RTL.  */
      c = 1;
      if (host_integerp (len, 1)
	  && can_store_by_pieces (tree_low_cst (len, 1),
				  builtin_memset_read_str, &c, dest_align,
				  true))
	{
	  val_rtx = force_reg (val_mode, val_rtx);
	  store_by_pieces (dest_mem, tree_low_cst (len, 1),
			   builtin_memset_gen_str, val_rtx, dest_align,
			   true, 0);
	}
      else if (!set_storage_via_setmem (dest_mem, len_rtx, val_rtx,
					dest_align, expected_align,
					expected_size))
	goto do_libcall;

      dest_mem = force_operand (XEXP (dest_mem, 0), NULL_RTX);
      dest_mem = convert_memory_address (ptr_mode, dest_mem);
      return dest_mem;
    }

  if (target_char_cast (val, &c))
    goto do_libcall;

  if (c)
    {
      if (host_integerp (len, 1)
	  && can_store_by_pieces (tree_low_cst (len, 1),
				  builtin_memset_read_str, &c, dest_align,
				  true))
	store_by_pieces (dest_mem, tree_low_cst (len, 1),
			 builtin_memset_read_str, &c, dest_align, true, 0);
      else if (!set_storage_via_setmem (dest_mem, len_rtx,
					gen_int_mode (c, val_mode),
					dest_align, expected_align,
					expected_size))
	goto do_libcall;

      dest_mem = force_operand (XEXP (dest_mem, 0), NULL_RTX);
      dest_mem = convert_memory_address (ptr_mode, dest_mem);
      return dest_mem;
    }

  set_mem_align (dest_mem, dest_align);
  dest_addr = clear_storage_hints (dest_mem, len_rtx,
				   CALL_EXPR_TAILCALL (orig_exp)
				   ? BLOCK_OP_TAILCALL : BLOCK_OP_NORMAL,
				   expected_align, expected_size);

  if (dest_addr == 0)
    {
      dest_addr = force_operand (XEXP (dest_mem, 0), NULL_RTX);
      dest_addr = convert_memory_address (ptr_mode, dest_addr);
    }

  return dest_addr;

 do_libcall:
  fndecl = get_callee_fndecl (orig_exp);
  fcode = DECL_FUNCTION_CODE (fndecl);
  if (fcode == BUILT_IN_MEMSET)
    fn = build_call_nofold_loc (EXPR_LOCATION (orig_exp), fndecl, 3,
				dest, val, len);
  else if (fcode == BUILT_IN_BZERO)
    fn = build_call_nofold_loc (EXPR_LOCATION (orig_exp), fndecl, 2,
				dest, len);
  else
    gcc_unreachable ();
  gcc_assert (TREE_CODE (fn) == CALL_EXPR);
  CALL_EXPR_TAILCALL (fn) = CALL_EXPR_TAILCALL (orig_exp);
  return expand_call (fn, target, target == const0_rtx);
}

/* Expand expression EXP, which is a call to the bzero builtin.  Return
   NULL_RTX if we failed the caller should emit a normal call.  */

static rtx
expand_builtin_bzero (tree exp)
{
  tree dest, size;
  location_t loc = EXPR_LOCATION (exp);

  if (!validate_arglist (exp, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;

  dest = CALL_EXPR_ARG (exp, 0);
  size = CALL_EXPR_ARG (exp, 1);

  /* New argument list transforming bzero(ptr x, int y) to
     memset(ptr x, int 0, size_t y).   This is done this way
     so that if it isn't expanded inline, we fallback to
     calling bzero instead of memset.  */

  return expand_builtin_memset_args (dest, integer_zero_node,
				     fold_convert_loc (loc,
						       size_type_node, size),
				     const0_rtx, VOIDmode, exp);
}

/* Expand expression EXP, which is a call to the memcmp built-in function.
   Return NULL_RTX if we failed and the caller should emit a normal call,
   otherwise try to get the result in TARGET, if convenient (and in mode
   MODE, if that's convenient).  */

static rtx
expand_builtin_memcmp (tree exp, ATTRIBUTE_UNUSED rtx target,
		       ATTRIBUTE_UNUSED enum machine_mode mode)
{
  location_t loc ATTRIBUTE_UNUSED = EXPR_LOCATION (exp);

  if (!validate_arglist (exp,
 			 POINTER_TYPE, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;

  /* Note: The cmpstrnsi pattern, if it exists, is not suitable for
     implementing memcmp because it will stop if it encounters two
     zero bytes.  */
#if defined HAVE_cmpmemsi
  {
    rtx arg1_rtx, arg2_rtx, arg3_rtx;
    rtx result;
    rtx insn;
    tree arg1 = CALL_EXPR_ARG (exp, 0);
    tree arg2 = CALL_EXPR_ARG (exp, 1);
    tree len = CALL_EXPR_ARG (exp, 2);

    unsigned int arg1_align = get_pointer_alignment (arg1) / BITS_PER_UNIT;
    unsigned int arg2_align = get_pointer_alignment (arg2) / BITS_PER_UNIT;
    enum machine_mode insn_mode;

    if (HAVE_cmpmemsi)
      insn_mode = insn_data[(int) CODE_FOR_cmpmemsi].operand[0].mode;
    else
      return NULL_RTX;

    /* If we don't have POINTER_TYPE, call the function.  */
    if (arg1_align == 0 || arg2_align == 0)
      return NULL_RTX;

    /* Make a place to write the result of the instruction.  */
    result = target;
    if (! (result != 0
	   && REG_P (result) && GET_MODE (result) == insn_mode
	   && REGNO (result) >= FIRST_PSEUDO_REGISTER))
      result = gen_reg_rtx (insn_mode);

    arg1_rtx = get_memory_rtx (arg1, len);
    arg2_rtx = get_memory_rtx (arg2, len);
    arg3_rtx = expand_normal (fold_convert_loc (loc, sizetype, len));

    /* Set MEM_SIZE as appropriate.  */
    if (CONST_INT_P (arg3_rtx))
      {
	set_mem_size (arg1_rtx, INTVAL (arg3_rtx));
	set_mem_size (arg2_rtx, INTVAL (arg3_rtx));
      }

    if (HAVE_cmpmemsi)
      insn = gen_cmpmemsi (result, arg1_rtx, arg2_rtx, arg3_rtx,
			   GEN_INT (MIN (arg1_align, arg2_align)));
    else
      gcc_unreachable ();

    if (insn)
      emit_insn (insn);
    else
      emit_library_call_value (memcmp_libfunc, result, LCT_PURE,
			       TYPE_MODE (integer_type_node), 3,
			       XEXP (arg1_rtx, 0), Pmode,
			       XEXP (arg2_rtx, 0), Pmode,
			       convert_to_mode (TYPE_MODE (sizetype), arg3_rtx,
						TYPE_UNSIGNED (sizetype)),
			       TYPE_MODE (sizetype));

    /* Return the value in the proper mode for this function.  */
    mode = TYPE_MODE (TREE_TYPE (exp));
    if (GET_MODE (result) == mode)
      return result;
    else if (target != 0)
      {
	convert_move (target, result, 0);
	return target;
      }
    else
      return convert_to_mode (mode, result, 0);
  }
#endif /* HAVE_cmpmemsi.  */

  return NULL_RTX;
}

/* Expand expression EXP, which is a call to the strcmp builtin.  Return NULL_RTX
   if we failed the caller should emit a normal call, otherwise try to get
   the result in TARGET, if convenient.  */

static rtx
expand_builtin_strcmp (tree exp, ATTRIBUTE_UNUSED rtx target)
{
  if (!validate_arglist (exp, POINTER_TYPE, POINTER_TYPE, VOID_TYPE))
    return NULL_RTX;

#if defined HAVE_cmpstrsi || defined HAVE_cmpstrnsi
  if (direct_optab_handler (cmpstr_optab, SImode) != CODE_FOR_nothing
      || direct_optab_handler (cmpstrn_optab, SImode) != CODE_FOR_nothing)
    {
      rtx arg1_rtx, arg2_rtx;
      rtx result, insn = NULL_RTX;
      tree fndecl, fn;
      tree arg1 = CALL_EXPR_ARG (exp, 0);
      tree arg2 = CALL_EXPR_ARG (exp, 1);

      unsigned int arg1_align = get_pointer_alignment (arg1) / BITS_PER_UNIT;
      unsigned int arg2_align = get_pointer_alignment (arg2) / BITS_PER_UNIT;

      /* If we don't have POINTER_TYPE, call the function.  */
      if (arg1_align == 0 || arg2_align == 0)
	return NULL_RTX;

      /* Stabilize the arguments in case gen_cmpstr(n)si fail.  */
      arg1 = builtin_save_expr (arg1);
      arg2 = builtin_save_expr (arg2);

      arg1_rtx = get_memory_rtx (arg1, NULL);
      arg2_rtx = get_memory_rtx (arg2, NULL);

#ifdef HAVE_cmpstrsi
      /* Try to call cmpstrsi.  */
      if (HAVE_cmpstrsi)
	{
	  enum machine_mode insn_mode
	    = insn_data[(int) CODE_FOR_cmpstrsi].operand[0].mode;

	  /* Make a place to write the result of the instruction.  */
	  result = target;
	  if (! (result != 0
		 && REG_P (result) && GET_MODE (result) == insn_mode
		 && REGNO (result) >= FIRST_PSEUDO_REGISTER))
	    result = gen_reg_rtx (insn_mode);

	  insn = gen_cmpstrsi (result, arg1_rtx, arg2_rtx,
			       GEN_INT (MIN (arg1_align, arg2_align)));
	}
#endif
#ifdef HAVE_cmpstrnsi
      /* Try to determine at least one length and call cmpstrnsi.  */
      if (!insn && HAVE_cmpstrnsi)
	{
	  tree len;
	  rtx arg3_rtx;

	  enum machine_mode insn_mode
	    = insn_data[(int) CODE_FOR_cmpstrnsi].operand[0].mode;
	  tree len1 = c_strlen (arg1, 1);
	  tree len2 = c_strlen (arg2, 1);

	  if (len1)
	    len1 = size_binop (PLUS_EXPR, ssize_int (1), len1);
	  if (len2)
	    len2 = size_binop (PLUS_EXPR, ssize_int (1), len2);

	  /* If we don't have a constant length for the first, use the length
	     of the second, if we know it.  We don't require a constant for
	     this case; some cost analysis could be done if both are available
	     but neither is constant.  For now, assume they're equally cheap,
	     unless one has side effects.  If both strings have constant lengths,
	     use the smaller.  */

	  if (!len1)
	    len = len2;
	  else if (!len2)
	    len = len1;
	  else if (TREE_SIDE_EFFECTS (len1))
	    len = len2;
	  else if (TREE_SIDE_EFFECTS (len2))
	    len = len1;
	  else if (TREE_CODE (len1) != INTEGER_CST)
	    len = len2;
	  else if (TREE_CODE (len2) != INTEGER_CST)
	    len = len1;
	  else if (tree_int_cst_lt (len1, len2))
	    len = len1;
	  else
	    len = len2;

	  /* If both arguments have side effects, we cannot optimize.  */
	  if (!len || TREE_SIDE_EFFECTS (len))
	    goto do_libcall;

	  arg3_rtx = expand_normal (len);

	  /* Make a place to write the result of the instruction.  */
	  result = target;
	  if (! (result != 0
		 && REG_P (result) && GET_MODE (result) == insn_mode
		 && REGNO (result) >= FIRST_PSEUDO_REGISTER))
	    result = gen_reg_rtx (insn_mode);

	  insn = gen_cmpstrnsi (result, arg1_rtx, arg2_rtx, arg3_rtx,
				GEN_INT (MIN (arg1_align, arg2_align)));
	}
#endif

      if (insn)
	{
	  enum machine_mode mode;
	  emit_insn (insn);

	  /* Return the value in the proper mode for this function.  */
	  mode = TYPE_MODE (TREE_TYPE (exp));
	  if (GET_MODE (result) == mode)
	    return result;
	  if (target == 0)
	    return convert_to_mode (mode, result, 0);
	  convert_move (target, result, 0);
	  return target;
	}

      /* Expand the library call ourselves using a stabilized argument
	 list to avoid re-evaluating the function's arguments twice.  */
#ifdef HAVE_cmpstrnsi
    do_libcall:
#endif
      fndecl = get_callee_fndecl (exp);
      fn = build_call_nofold_loc (EXPR_LOCATION (exp), fndecl, 2, arg1, arg2);
      gcc_assert (TREE_CODE (fn) == CALL_EXPR);
      CALL_EXPR_TAILCALL (fn) = CALL_EXPR_TAILCALL (exp);
      return expand_call (fn, target, target == const0_rtx);
    }
#endif
  return NULL_RTX;
}

/* Expand expression EXP, which is a call to the strncmp builtin. Return
   NULL_RTX if we failed the caller should emit a normal call, otherwise try to get
   the result in TARGET, if convenient.  */

static rtx
expand_builtin_strncmp (tree exp, ATTRIBUTE_UNUSED rtx target,
			ATTRIBUTE_UNUSED enum machine_mode mode)
{
  location_t loc ATTRIBUTE_UNUSED = EXPR_LOCATION (exp);

  if (!validate_arglist (exp,
 			 POINTER_TYPE, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;

  /* If c_strlen can determine an expression for one of the string
     lengths, and it doesn't have side effects, then emit cmpstrnsi
     using length MIN(strlen(string)+1, arg3).  */
#ifdef HAVE_cmpstrnsi
  if (HAVE_cmpstrnsi)
  {
    tree len, len1, len2;
    rtx arg1_rtx, arg2_rtx, arg3_rtx;
    rtx result, insn;
    tree fndecl, fn;
    tree arg1 = CALL_EXPR_ARG (exp, 0);
    tree arg2 = CALL_EXPR_ARG (exp, 1);
    tree arg3 = CALL_EXPR_ARG (exp, 2);

    unsigned int arg1_align = get_pointer_alignment (arg1) / BITS_PER_UNIT;
    unsigned int arg2_align = get_pointer_alignment (arg2) / BITS_PER_UNIT;
    enum machine_mode insn_mode
      = insn_data[(int) CODE_FOR_cmpstrnsi].operand[0].mode;

    len1 = c_strlen (arg1, 1);
    len2 = c_strlen (arg2, 1);

    if (len1)
      len1 = size_binop_loc (loc, PLUS_EXPR, ssize_int (1), len1);
    if (len2)
      len2 = size_binop_loc (loc, PLUS_EXPR, ssize_int (1), len2);

    /* If we don't have a constant length for the first, use the length
       of the second, if we know it.  We don't require a constant for
       this case; some cost analysis could be done if both are available
       but neither is constant.  For now, assume they're equally cheap,
       unless one has side effects.  If both strings have constant lengths,
       use the smaller.  */

    if (!len1)
      len = len2;
    else if (!len2)
      len = len1;
    else if (TREE_SIDE_EFFECTS (len1))
      len = len2;
    else if (TREE_SIDE_EFFECTS (len2))
      len = len1;
    else if (TREE_CODE (len1) != INTEGER_CST)
      len = len2;
    else if (TREE_CODE (len2) != INTEGER_CST)
      len = len1;
    else if (tree_int_cst_lt (len1, len2))
      len = len1;
    else
      len = len2;

    /* If both arguments have side effects, we cannot optimize.  */
    if (!len || TREE_SIDE_EFFECTS (len))
      return NULL_RTX;

    /* The actual new length parameter is MIN(len,arg3).  */
    len = fold_build2_loc (loc, MIN_EXPR, TREE_TYPE (len), len,
		       fold_convert_loc (loc, TREE_TYPE (len), arg3));

    /* If we don't have POINTER_TYPE, call the function.  */
    if (arg1_align == 0 || arg2_align == 0)
      return NULL_RTX;

    /* Make a place to write the result of the instruction.  */
    result = target;
    if (! (result != 0
	   && REG_P (result) && GET_MODE (result) == insn_mode
	   && REGNO (result) >= FIRST_PSEUDO_REGISTER))
      result = gen_reg_rtx (insn_mode);

    /* Stabilize the arguments in case gen_cmpstrnsi fails.  */
    arg1 = builtin_save_expr (arg1);
    arg2 = builtin_save_expr (arg2);
    len = builtin_save_expr (len);

    arg1_rtx = get_memory_rtx (arg1, len);
    arg2_rtx = get_memory_rtx (arg2, len);
    arg3_rtx = expand_normal (len);
    insn = gen_cmpstrnsi (result, arg1_rtx, arg2_rtx, arg3_rtx,
			  GEN_INT (MIN (arg1_align, arg2_align)));
    if (insn)
      {
	emit_insn (insn);

	/* Return the value in the proper mode for this function.  */
	mode = TYPE_MODE (TREE_TYPE (exp));
	if (GET_MODE (result) == mode)
	  return result;
	if (target == 0)
	  return convert_to_mode (mode, result, 0);
	convert_move (target, result, 0);
	return target;
      }

    /* Expand the library call ourselves using a stabilized argument
       list to avoid re-evaluating the function's arguments twice.  */
    fndecl = get_callee_fndecl (exp);
    fn = build_call_nofold_loc (EXPR_LOCATION (exp), fndecl, 3,
				arg1, arg2, len);
    gcc_assert (TREE_CODE (fn) == CALL_EXPR);
    CALL_EXPR_TAILCALL (fn) = CALL_EXPR_TAILCALL (exp);
    return expand_call (fn, target, target == const0_rtx);
  }
#endif
  return NULL_RTX;
}

/* Expand a call to __builtin_saveregs, generating the result in TARGET,
   if that's convenient.  */

rtx
expand_builtin_saveregs (void)
{
  rtx val, seq;

  /* Don't do __builtin_saveregs more than once in a function.
     Save the result of the first call and reuse it.  */
  if (saveregs_value != 0)
    return saveregs_value;

  /* When this function is called, it means that registers must be
     saved on entry to this function.  So we migrate the call to the
     first insn of this function.  */

  start_sequence ();

  /* Do whatever the machine needs done in this case.  */
  val = targetm.calls.expand_builtin_saveregs ();

  seq = get_insns ();
  end_sequence ();

  saveregs_value = val;

  /* Put the insns after the NOTE that starts the function.  If this
     is inside a start_sequence, make the outer-level insn chain current, so
     the code is placed at the start of the function.  */
  push_topmost_sequence ();
  emit_insn_after (seq, entry_of_function ());
  pop_topmost_sequence ();

  return val;
}

/* Expand a call to __builtin_next_arg.  */

static rtx
expand_builtin_next_arg (void)
{
  /* Checking arguments is already done in fold_builtin_next_arg
     that must be called before this function.  */
  return expand_binop (ptr_mode, add_optab,
		       crtl->args.internal_arg_pointer,
		       crtl->args.arg_offset_rtx,
		       NULL_RTX, 0, OPTAB_LIB_WIDEN);
}

/* Make it easier for the backends by protecting the valist argument
   from multiple evaluations.  */

static tree
stabilize_va_list_loc (location_t loc, tree valist, int needs_lvalue)
{
  tree vatype = targetm.canonical_va_list_type (TREE_TYPE (valist));

  /* The current way of determining the type of valist is completely
     bogus.  We should have the information on the va builtin instead.  */
  if (!vatype)
    vatype = targetm.fn_abi_va_list (cfun->decl);

  if (TREE_CODE (vatype) == ARRAY_TYPE)
    {
      if (TREE_SIDE_EFFECTS (valist))
	valist = save_expr (valist);

      /* For this case, the backends will be expecting a pointer to
	 vatype, but it's possible we've actually been given an array
	 (an actual TARGET_CANONICAL_VA_LIST_TYPE (valist)).
	 So fix it.  */
      if (TREE_CODE (TREE_TYPE (valist)) == ARRAY_TYPE)
	{
	  tree p1 = build_pointer_type (TREE_TYPE (vatype));
	  valist = build_fold_addr_expr_with_type_loc (loc, valist, p1);
	}
    }
  else
    {
      tree pt = build_pointer_type (vatype);

      if (! needs_lvalue)
	{
	  if (! TREE_SIDE_EFFECTS (valist))
	    return valist;

	  valist = fold_build1_loc (loc, ADDR_EXPR, pt, valist);
	  TREE_SIDE_EFFECTS (valist) = 1;
	}

      if (TREE_SIDE_EFFECTS (valist))
	valist = save_expr (valist);
      valist = fold_build2_loc (loc, MEM_REF,
				vatype, valist, build_int_cst (pt, 0));
    }

  return valist;
}

/* The "standard" definition of va_list is void*.  */

tree
std_build_builtin_va_list (void)
{
  return ptr_type_node;
}

/* The "standard" abi va_list is va_list_type_node.  */

tree
std_fn_abi_va_list (tree fndecl ATTRIBUTE_UNUSED)
{
  return va_list_type_node;
}

/* The "standard" type of va_list is va_list_type_node.  */

tree
std_canonical_va_list_type (tree type)
{
  tree wtype, htype;

  if (INDIRECT_REF_P (type))
    type = TREE_TYPE (type);
  else if (POINTER_TYPE_P (type) && POINTER_TYPE_P (TREE_TYPE(type)))
    type = TREE_TYPE (type);
  wtype = va_list_type_node;
  htype = type;
  /* Treat structure va_list types.  */
  if (TREE_CODE (wtype) == RECORD_TYPE && POINTER_TYPE_P (htype))
    htype = TREE_TYPE (htype);
  else if (TREE_CODE (wtype) == ARRAY_TYPE)
    {
      /* If va_list is an array type, the argument may have decayed
	 to a pointer type, e.g. by being passed to another function.
	 In that case, unwrap both types so that we can compare the
	 underlying records.  */
      if (TREE_CODE (htype) == ARRAY_TYPE
	  || POINTER_TYPE_P (htype))
	{
	  wtype = TREE_TYPE (wtype);
	  htype = TREE_TYPE (htype);
	}
    }
  if (TYPE_MAIN_VARIANT (wtype) == TYPE_MAIN_VARIANT (htype))
    return va_list_type_node;

  return NULL_TREE;
}

/* The "standard" implementation of va_start: just assign `nextarg' to
   the variable.  */

void
std_expand_builtin_va_start (tree valist, rtx nextarg)
{
  rtx va_r = expand_expr (valist, NULL_RTX, VOIDmode, EXPAND_WRITE);
  convert_move (va_r, nextarg, 0);
}

/* Expand EXP, a call to __builtin_va_start.  */

static rtx
expand_builtin_va_start (tree exp)
{
  rtx nextarg;
  tree valist;
  location_t loc = EXPR_LOCATION (exp);

  if (call_expr_nargs (exp) < 2)
    {
      error_at (loc, "too few arguments to function %<va_start%>");
      return const0_rtx;
    }

  if (fold_builtin_next_arg (exp, true))
    return const0_rtx;

  nextarg = expand_builtin_next_arg ();
  valist = stabilize_va_list_loc (loc, CALL_EXPR_ARG (exp, 0), 1);

  if (targetm.expand_builtin_va_start)
    targetm.expand_builtin_va_start (valist, nextarg);
  else
    std_expand_builtin_va_start (valist, nextarg);

  return const0_rtx;
}

/* The "standard" implementation of va_arg: read the value from the
   current (padded) address and increment by the (padded) size.  */

tree
std_gimplify_va_arg_expr (tree valist, tree type, gimple_seq *pre_p,
			  gimple_seq *post_p)
{
  tree addr, t, type_size, rounded_size, valist_tmp;
  unsigned HOST_WIDE_INT align, boundary;
  bool indirect;

#ifdef ARGS_GROW_DOWNWARD
  /* All of the alignment and movement below is for args-grow-up machines.
     As of 2004, there are only 3 ARGS_GROW_DOWNWARD targets, and they all
     implement their own specialized gimplify_va_arg_expr routines.  */
  gcc_unreachable ();
#endif

  indirect = pass_by_reference (NULL, TYPE_MODE (type), type, false);
  if (indirect)
    type = build_pointer_type (type);

  align = PARM_BOUNDARY / BITS_PER_UNIT;
  boundary = targetm.calls.function_arg_boundary (TYPE_MODE (type), type);

  /* When we align parameter on stack for caller, if the parameter
     alignment is beyond MAX_SUPPORTED_STACK_ALIGNMENT, it will be
     aligned at MAX_SUPPORTED_STACK_ALIGNMENT.  We will match callee
     here with caller.  */
  if (boundary > MAX_SUPPORTED_STACK_ALIGNMENT)
    boundary = MAX_SUPPORTED_STACK_ALIGNMENT;

  boundary /= BITS_PER_UNIT;

  /* Hoist the valist value into a temporary for the moment.  */
  valist_tmp = get_initialized_tmp_var (valist, pre_p, NULL);

  /* va_list pointer is aligned to PARM_BOUNDARY.  If argument actually
     requires greater alignment, we must perform dynamic alignment.  */
  if (boundary > align
      && !integer_zerop (TYPE_SIZE (type)))
    {
      t = build2 (MODIFY_EXPR, TREE_TYPE (valist), valist_tmp,
		  fold_build_pointer_plus_hwi (valist_tmp, boundary - 1));
      gimplify_and_add (t, pre_p);

      t = build2 (MODIFY_EXPR, TREE_TYPE (valist), valist_tmp,
		  fold_build2 (BIT_AND_EXPR, TREE_TYPE (valist),
			       valist_tmp,
			       build_int_cst (TREE_TYPE (valist), -boundary)));
      gimplify_and_add (t, pre_p);
    }
  else
    boundary = align;

  /* If the actual alignment is less than the alignment of the type,
     adjust the type accordingly so that we don't assume strict alignment
     when dereferencing the pointer.  */
  boundary *= BITS_PER_UNIT;
  if (boundary < TYPE_ALIGN (type))
    {
      type = build_variant_type_copy (type);
      TYPE_ALIGN (type) = boundary;
    }

  /* Compute the rounded size of the type.  */
  type_size = size_in_bytes (type);
  rounded_size = round_up (type_size, align);

  /* Reduce rounded_size so it's sharable with the postqueue.  */
  gimplify_expr (&rounded_size, pre_p, post_p, is_gimple_val, fb_rvalue);

  /* Get AP.  */
  addr = valist_tmp;
  if (PAD_VARARGS_DOWN && !integer_zerop (rounded_size))
    {
      /* Small args are padded downward.  */
      t = fold_build2_loc (input_location, GT_EXPR, sizetype,
		       rounded_size, size_int (align));
      t = fold_build3 (COND_EXPR, sizetype, t, size_zero_node,
		       size_binop (MINUS_EXPR, rounded_size, type_size));
      addr = fold_build_pointer_plus (addr, t);
    }

  /* Compute new value for AP.  */
  t = fold_build_pointer_plus (valist_tmp, rounded_size);
  t = build2 (MODIFY_EXPR, TREE_TYPE (valist), valist, t);
  gimplify_and_add (t, pre_p);

  addr = fold_convert (build_pointer_type (type), addr);

  if (indirect)
    addr = build_va_arg_indirect_ref (addr);

  return build_va_arg_indirect_ref (addr);
}

/* Build an indirect-ref expression over the given TREE, which represents a
   piece of a va_arg() expansion.  */
tree
build_va_arg_indirect_ref (tree addr)
{
  addr = build_simple_mem_ref_loc (EXPR_LOCATION (addr), addr);

  if (flag_mudflap) /* Don't instrument va_arg INDIRECT_REF.  */
    mf_mark (addr);

  return addr;
}

/* Return a dummy expression of type TYPE in order to keep going after an
   error.  */

static tree
dummy_object (tree type)
{
  tree t = build_int_cst (build_pointer_type (type), 0);
  return build2 (MEM_REF, type, t, t);
}

/* Gimplify __builtin_va_arg, aka VA_ARG_EXPR, which is not really a
   builtin function, but a very special sort of operator.  */

enum gimplify_status
gimplify_va_arg_expr (tree *expr_p, gimple_seq *pre_p, gimple_seq *post_p)
{
  tree promoted_type, have_va_type;
  tree valist = TREE_OPERAND (*expr_p, 0);
  tree type = TREE_TYPE (*expr_p);
  tree t;
  location_t loc = EXPR_LOCATION (*expr_p);

  /* Verify that valist is of the proper type.  */
  have_va_type = TREE_TYPE (valist);
  if (have_va_type == error_mark_node)
    return GS_ERROR;
  have_va_type = targetm.canonical_va_list_type (have_va_type);

  if (have_va_type == NULL_TREE)
    {
      error_at (loc, "first argument to %<va_arg%> not of type %<va_list%>");
      return GS_ERROR;
    }

  /* Generate a diagnostic for requesting data of a type that cannot
     be passed through `...' due to type promotion at the call site.  */
  if ((promoted_type = lang_hooks.types.type_promotes_to (type))
	   != type)
    {
      static bool gave_help;
      bool warned;

      /* Unfortunately, this is merely undefined, rather than a constraint
	 violation, so we cannot make this an error.  If this call is never
	 executed, the program is still strictly conforming.  */
      warned = warning_at (loc, 0,
	  		   "%qT is promoted to %qT when passed through %<...%>",
			   type, promoted_type);
      if (!gave_help && warned)
	{
	  gave_help = true;
	  inform (loc, "(so you should pass %qT not %qT to %<va_arg%>)",
		  promoted_type, type);
	}

      /* We can, however, treat "undefined" any way we please.
	 Call abort to encourage the user to fix the program.  */
      if (warned)
	inform (loc, "if this code is reached, the program will abort");
      /* Before the abort, allow the evaluation of the va_list
	 expression to exit or longjmp.  */
      gimplify_and_add (valist, pre_p);
      t = build_call_expr_loc (loc,
			       builtin_decl_implicit (BUILT_IN_TRAP), 0);
      gimplify_and_add (t, pre_p);

      /* This is dead code, but go ahead and finish so that the
	 mode of the result comes out right.  */
      *expr_p = dummy_object (type);
      return GS_ALL_DONE;
    }
  else
    {
      /* Make it easier for the backends by protecting the valist argument
	 from multiple evaluations.  */
      if (TREE_CODE (have_va_type) == ARRAY_TYPE)
	{
	  /* For this case, the backends will be expecting a pointer to
	     TREE_TYPE (abi), but it's possible we've
	     actually been given an array (an actual TARGET_FN_ABI_VA_LIST).
	     So fix it.  */
	  if (TREE_CODE (TREE_TYPE (valist)) == ARRAY_TYPE)
	    {
	      tree p1 = build_pointer_type (TREE_TYPE (have_va_type));
	      valist = fold_convert_loc (loc, p1,
					 build_fold_addr_expr_loc (loc, valist));
	    }

	  gimplify_expr (&valist, pre_p, post_p, is_gimple_val, fb_rvalue);
	}
      else
	gimplify_expr (&valist, pre_p, post_p, is_gimple_min_lval, fb_lvalue);

      if (!targetm.gimplify_va_arg_expr)
	/* FIXME: Once most targets are converted we should merely
	   assert this is non-null.  */
	return GS_ALL_DONE;

      *expr_p = targetm.gimplify_va_arg_expr (valist, type, pre_p, post_p);
      return GS_OK;
    }
}

/* Expand EXP, a call to __builtin_va_end.  */

static rtx
expand_builtin_va_end (tree exp)
{
  tree valist = CALL_EXPR_ARG (exp, 0);

  /* Evaluate for side effects, if needed.  I hate macros that don't
     do that.  */
  if (TREE_SIDE_EFFECTS (valist))
    expand_expr (valist, const0_rtx, VOIDmode, EXPAND_NORMAL);

  return const0_rtx;
}

/* Expand EXP, a call to __builtin_va_copy.  We do this as a
   builtin rather than just as an assignment in stdarg.h because of the
   nastiness of array-type va_list types.  */

static rtx
expand_builtin_va_copy (tree exp)
{
  tree dst, src, t;
  location_t loc = EXPR_LOCATION (exp);

  dst = CALL_EXPR_ARG (exp, 0);
  src = CALL_EXPR_ARG (exp, 1);

  dst = stabilize_va_list_loc (loc, dst, 1);
  src = stabilize_va_list_loc (loc, src, 0);

  gcc_assert (cfun != NULL && cfun->decl != NULL_TREE);

  if (TREE_CODE (targetm.fn_abi_va_list (cfun->decl)) != ARRAY_TYPE)
    {
      t = build2 (MODIFY_EXPR, targetm.fn_abi_va_list (cfun->decl), dst, src);
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
    }
  else
    {
      rtx dstb, srcb, size;

      /* Evaluate to pointers.  */
      dstb = expand_expr (dst, NULL_RTX, Pmode, EXPAND_NORMAL);
      srcb = expand_expr (src, NULL_RTX, Pmode, EXPAND_NORMAL);
      size = expand_expr (TYPE_SIZE_UNIT (targetm.fn_abi_va_list (cfun->decl)),
      		  NULL_RTX, VOIDmode, EXPAND_NORMAL);

      dstb = convert_memory_address (Pmode, dstb);
      srcb = convert_memory_address (Pmode, srcb);

      /* "Dereference" to BLKmode memories.  */
      dstb = gen_rtx_MEM (BLKmode, dstb);
      set_mem_alias_set (dstb, get_alias_set (TREE_TYPE (TREE_TYPE (dst))));
      set_mem_align (dstb, TYPE_ALIGN (targetm.fn_abi_va_list (cfun->decl)));
      srcb = gen_rtx_MEM (BLKmode, srcb);
      set_mem_alias_set (srcb, get_alias_set (TREE_TYPE (TREE_TYPE (src))));
      set_mem_align (srcb, TYPE_ALIGN (targetm.fn_abi_va_list (cfun->decl)));

      /* Copy.  */
      emit_block_move (dstb, srcb, size, BLOCK_OP_NORMAL);
    }

  return const0_rtx;
}

/* Expand a call to one of the builtin functions __builtin_frame_address or
   __builtin_return_address.  */

static rtx
expand_builtin_frame_address (tree fndecl, tree exp)
{
  /* The argument must be a nonnegative integer constant.
     It counts the number of frames to scan up the stack.
     The value is the return address saved in that frame.  */
  if (call_expr_nargs (exp) == 0)
    /* Warning about missing arg was already issued.  */
    return const0_rtx;
  else if (! host_integerp (CALL_EXPR_ARG (exp, 0), 1))
    {
      if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_FRAME_ADDRESS)
	error ("invalid argument to %<__builtin_frame_address%>");
      else
	error ("invalid argument to %<__builtin_return_address%>");
      return const0_rtx;
    }
  else
    {
      rtx tem
	= expand_builtin_return_addr (DECL_FUNCTION_CODE (fndecl),
				      tree_low_cst (CALL_EXPR_ARG (exp, 0), 1));

      /* Some ports cannot access arbitrary stack frames.  */
      if (tem == NULL)
	{
	  if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_FRAME_ADDRESS)
	    warning (0, "unsupported argument to %<__builtin_frame_address%>");
	  else
	    warning (0, "unsupported argument to %<__builtin_return_address%>");
	  return const0_rtx;
	}

      /* For __builtin_frame_address, return what we've got.  */
      if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_FRAME_ADDRESS)
	return tem;

      if (!REG_P (tem)
	  && ! CONSTANT_P (tem))
	tem = copy_to_mode_reg (Pmode, tem);
      return tem;
    }
}

/* Expand EXP, a call to the alloca builtin.  Return NULL_RTX if we
   failed and the caller should emit a normal call.  CANNOT_ACCUMULATE
   is the same as for allocate_dynamic_stack_space.  */

static rtx
expand_builtin_alloca (tree exp, bool cannot_accumulate)
{
  rtx op0;
  rtx result;
  bool valid_arglist;
  unsigned int align;
  bool alloca_with_align = (DECL_FUNCTION_CODE (get_callee_fndecl (exp))
			    == BUILT_IN_ALLOCA_WITH_ALIGN);

  /* Emit normal call if we use mudflap.  */
  if (flag_mudflap)
    return NULL_RTX;

  valid_arglist
    = (alloca_with_align
       ? validate_arglist (exp, INTEGER_TYPE, INTEGER_TYPE, VOID_TYPE)
       : validate_arglist (exp, INTEGER_TYPE, VOID_TYPE));

  if (!valid_arglist)
    return NULL_RTX;

  /* Compute the argument.  */
  op0 = expand_normal (CALL_EXPR_ARG (exp, 0));

  /* Compute the alignment.  */
  align = (alloca_with_align
	   ? TREE_INT_CST_LOW (CALL_EXPR_ARG (exp, 1))
	   : BIGGEST_ALIGNMENT);

  /* Allocate the desired space.  */
  result = allocate_dynamic_stack_space (op0, 0, align, cannot_accumulate);
  result = convert_memory_address (ptr_mode, result);

  return result;
}

/* Expand a call to a bswap builtin with argument ARG0.  MODE
   is the mode to expand with.  */

static rtx
expand_builtin_bswap (tree exp, rtx target, rtx subtarget)
{
  enum machine_mode mode;
  tree arg;
  rtx op0;

  if (!validate_arglist (exp, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;

  arg = CALL_EXPR_ARG (exp, 0);
  mode = TYPE_MODE (TREE_TYPE (arg));
  op0 = expand_expr (arg, subtarget, VOIDmode, EXPAND_NORMAL);

  target = expand_unop (mode, bswap_optab, op0, target, 1);

  gcc_assert (target);

  return convert_to_mode (mode, target, 0);
}

/* Expand a call to a unary builtin in EXP.
   Return NULL_RTX if a normal call should be emitted rather than expanding the
   function in-line.  If convenient, the result should be placed in TARGET.
   SUBTARGET may be used as the target for computing one of EXP's operands.  */

static rtx
expand_builtin_unop (enum machine_mode target_mode, tree exp, rtx target,
		     rtx subtarget, optab op_optab)
{
  rtx op0;

  if (!validate_arglist (exp, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;

  /* Compute the argument.  */
  op0 = expand_expr (CALL_EXPR_ARG (exp, 0),
		     (subtarget
		      && (TYPE_MODE (TREE_TYPE (CALL_EXPR_ARG (exp, 0)))
			  == GET_MODE (subtarget))) ? subtarget : NULL_RTX,
		     VOIDmode, EXPAND_NORMAL);
  /* Compute op, into TARGET if possible.
     Set TARGET to wherever the result comes back.  */
  target = expand_unop (TYPE_MODE (TREE_TYPE (CALL_EXPR_ARG (exp, 0))),
			op_optab, op0, target, op_optab != clrsb_optab);
  gcc_assert (target);

  return convert_to_mode (target_mode, target, 0);
}

/* Expand a call to __builtin_expect.  We just return our argument
   as the builtin_expect semantic should've been already executed by
   tree branch prediction pass. */

static rtx
expand_builtin_expect (tree exp, rtx target)
{
  tree arg;

  if (call_expr_nargs (exp) < 2)
    return const0_rtx;
  arg = CALL_EXPR_ARG (exp, 0);

  target = expand_expr (arg, target, VOIDmode, EXPAND_NORMAL);
  /* When guessing was done, the hints should be already stripped away.  */
  gcc_assert (!flag_guess_branch_prob
	      || optimize == 0 || seen_error ());
  return target;
}

/* Expand a call to __builtin_assume_aligned.  We just return our first
   argument as the builtin_assume_aligned semantic should've been already
   executed by CCP.  */

static rtx
expand_builtin_assume_aligned (tree exp, rtx target)
{
  if (call_expr_nargs (exp) < 2)
    return const0_rtx;
  target = expand_expr (CALL_EXPR_ARG (exp, 0), target, VOIDmode,
			EXPAND_NORMAL);
  gcc_assert (!TREE_SIDE_EFFECTS (CALL_EXPR_ARG (exp, 1))
	      && (call_expr_nargs (exp) < 3
		  || !TREE_SIDE_EFFECTS (CALL_EXPR_ARG (exp, 2))));
  return target;
}

void
expand_builtin_trap (void)
{
#ifdef HAVE_trap
  if (HAVE_trap)
    emit_insn (gen_trap ());
  else
#endif
    emit_library_call (abort_libfunc, LCT_NORETURN, VOIDmode, 0);
  emit_barrier ();
}

/* Expand a call to __builtin_unreachable.  We do nothing except emit
   a barrier saying that control flow will not pass here.

   It is the responsibility of the program being compiled to ensure
   that control flow does never reach __builtin_unreachable.  */
static void
expand_builtin_unreachable (void)
{
  emit_barrier ();
}

/* Create a new constant string literal and return a char* pointer to it.
   The STRING_CST value is the LEN characters at STR.  */
tree
build_string_literal (int len, const char *str)
{
  tree t, elem, index, type;

  t = build_string (len, str);
  elem = build_type_variant (char_type_node, 1, 0);
  index = build_index_type (size_int (len - 1));
  type = build_array_type (elem, index);
  TREE_TYPE (t) = type;
  TREE_CONSTANT (t) = 1;
  TREE_READONLY (t) = 1;
  TREE_STATIC (t) = 1;

  type = build_pointer_type (elem);
  t = build1 (ADDR_EXPR, type,
	      build4 (ARRAY_REF, elem,
		      t, integer_zero_node, NULL_TREE, NULL_TREE));
  return t;
}

/* Expand a call to __builtin___clear_cache.  */

static rtx
expand_builtin___clear_cache (tree exp ATTRIBUTE_UNUSED)
{
#ifndef HAVE_clear_cache
#ifdef CLEAR_INSN_CACHE
  /* There is no "clear_cache" insn, and __clear_cache() in libgcc
     does something.  Just do the default expansion to a call to
     __clear_cache().  */
  return NULL_RTX;
#else
  /* There is no "clear_cache" insn, and __clear_cache() in libgcc
     does nothing.  There is no need to call it.  Do nothing.  */
  return const0_rtx;
#endif /* CLEAR_INSN_CACHE */
#else
  /* We have a "clear_cache" insn, and it will handle everything.  */
  tree begin, end;
  rtx begin_rtx, end_rtx;

  /* We must not expand to a library call.  If we did, any
     fallback library function in libgcc that might contain a call to
     __builtin___clear_cache() would recurse infinitely.  */
  if (!validate_arglist (exp, POINTER_TYPE, POINTER_TYPE, VOID_TYPE))
    {
      error ("both arguments to %<__builtin___clear_cache%> must be pointers");
      return const0_rtx;
    }

  if (HAVE_clear_cache)
    {
      struct expand_operand ops[2] = { 0 };

      begin = CALL_EXPR_ARG (exp, 0);
      begin_rtx = expand_expr (begin, NULL_RTX, Pmode, EXPAND_NORMAL);

      end = CALL_EXPR_ARG (exp, 1);
      end_rtx = expand_expr (end, NULL_RTX, Pmode, EXPAND_NORMAL);

      create_address_operand (&ops[0], begin_rtx);
      create_address_operand (&ops[1], end_rtx);
      if (maybe_expand_insn (CODE_FOR_clear_cache, 2, ops))
	return const0_rtx;
    }
  return const0_rtx;
#endif /* HAVE_clear_cache */
}

/* Given a trampoline address, make sure it satisfies TRAMPOLINE_ALIGNMENT.  */

static rtx
round_trampoline_addr (rtx tramp)
{
  rtx temp = { 0 };
  rtx addend = { 0 };
  rtx mask = { 0 };

  /* If we don't need too much alignment, we'll have been guaranteed
     proper alignment by get_trampoline_type.  */
  if (TRAMPOLINE_ALIGNMENT <= STACK_BOUNDARY)
    return tramp;

  /* Round address up to desired boundary.  */
  temp = gen_reg_rtx (Pmode);
  addend = GEN_INT (TRAMPOLINE_ALIGNMENT / BITS_PER_UNIT - 1);
  mask = GEN_INT (-TRAMPOLINE_ALIGNMENT / BITS_PER_UNIT);

  temp  = expand_simple_binop (Pmode, PLUS, tramp, addend,
			       temp, 0, OPTAB_LIB_WIDEN);
  tramp = expand_simple_binop (Pmode, AND, temp, mask,
			       temp, 0, OPTAB_LIB_WIDEN);

  return tramp;
}

static rtx
expand_builtin_init_trampoline (tree exp, bool onstack)
{
  tree t_tramp = { 0 };
  tree t_func = { 0 };
  tree t_chain = { 0 };
  rtx m_tramp = { 0 };
  rtx r_tramp = { 0 };
  rtx r_chain = { 0 };
  rtx tmp = { 0 };

  if (!validate_arglist (exp, POINTER_TYPE, POINTER_TYPE,
			 POINTER_TYPE, VOID_TYPE))
    return NULL_RTX;

  t_tramp = CALL_EXPR_ARG (exp, 0);
  t_func = CALL_EXPR_ARG (exp, 1);
  t_chain = CALL_EXPR_ARG (exp, 2);

  r_tramp = expand_normal (t_tramp);
  m_tramp = gen_rtx_MEM (BLKmode, r_tramp);
  MEM_NOTRAP_P (m_tramp) = 1;

  /* If ONSTACK, the TRAMP argument should be the address of a field
     within the local function's FRAME decl.  Either way, let's see if
     we can fill in the MEM_ATTRs for this memory.  */
  if (TREE_CODE (t_tramp) == ADDR_EXPR)
    set_mem_attributes_minus_bitpos (m_tramp, TREE_OPERAND (t_tramp, 0),
				     true, 0);

  /* Creator of a heap trampoline is responsible for making sure the
     address is aligned to at least STACK_BOUNDARY.  Normally malloc
     will ensure this anyhow.  */
  tmp = round_trampoline_addr (r_tramp);
  if (tmp != r_tramp)
    {
      m_tramp = change_address (m_tramp, BLKmode, tmp);
      set_mem_align (m_tramp, TRAMPOLINE_ALIGNMENT);
      set_mem_size (m_tramp, TRAMPOLINE_SIZE);
    }

  /* The FUNC argument should be the address of the nested function.
     Extract the actual function decl to pass to the hook.  */
  gcc_assert (TREE_CODE (t_func) == ADDR_EXPR);
  t_func = TREE_OPERAND (t_func, 0);
  gcc_assert (TREE_CODE (t_func) == FUNCTION_DECL);

  r_chain = expand_normal (t_chain);

  /* Generate insns to initialize the trampoline.  */
  targetm.calls.trampoline_init (m_tramp, t_func, r_chain);

  if (onstack)
    {
      trampolines_created = 1;

      warning_at (DECL_SOURCE_LOCATION (t_func), OPT_Wtrampolines,
		  "trampoline generated for nested function %qD", t_func);
    }

  return const0_rtx;
}

static rtx
expand_builtin_adjust_trampoline (tree exp)
{
  rtx tramp = { 0 };

  if (!validate_arglist (exp, POINTER_TYPE, VOID_TYPE))
    return NULL_RTX;

  tramp = expand_normal (CALL_EXPR_ARG (exp, 0));
  tramp = round_trampoline_addr (tramp);
  if (targetm.calls.trampoline_adjust_address)
    tramp = targetm.calls.trampoline_adjust_address (tramp);

  return tramp;
}

/* Reconstitute a mode for a __sync intrinsic operation.  Since the type of
   the pointer in these functions is void*, the tree optimizers may remove
   casts.  The mode computed in expand_builtin isn't reliable either, due
   to __sync_bool_compare_and_swap.

   FCODE_DIFF should be fcode - base, where base is the FOO_1 code for the
   group of builtins.  This gives us log2 of the mode size.  */

static inline enum machine_mode
get_builtin_sync_mode (int fcode_diff)
{
  /* The size is not negotiable, so ask not to get BLKmode in return
     if the target indicates that a smaller size would be better.  */
  return mode_for_size (BITS_PER_UNIT << fcode_diff, MODE_INT, 0);
}

/* Expand the memory expression LOC and return the appropriate memory operand
   for the builtin_sync operations.  */

static rtx
get_builtin_sync_mem (tree loc, enum machine_mode mode)
{
  rtx addr, mem;

  addr = expand_expr (loc, NULL_RTX, ptr_mode, EXPAND_SUM);
  addr = convert_memory_address (Pmode, addr);

  /* Note that we explicitly do not want any alias information for this
     memory, so that we kill all other live memories.  Otherwise we don't
     satisfy the full barrier semantics of the intrinsic.  */
  mem = validize_mem (gen_rtx_MEM (mode, addr));

  /* The alignment needs to be at least according to that of the mode.  */
  set_mem_align (mem, MAX (GET_MODE_ALIGNMENT (mode),
			   get_pointer_alignment (loc)));
  set_mem_alias_set (mem, ALIAS_SET_MEMORY_BARRIER);
  MEM_VOLATILE_P (mem) = 1;

  return mem;
}

/* Make sure an argument is in the right mode.
   EXP is the tree argument. 
   MODE is the mode it should be in.  */

static rtx
expand_expr_force_mode (tree exp, enum machine_mode mode)
{
  rtx val;
  enum machine_mode old_mode;

  val = expand_expr (exp, NULL_RTX, mode, EXPAND_NORMAL);
  /* If VAL is promoted to a wider mode, convert it back to MODE.  Take care
     of CONST_INTs, where we know the old_mode only from the call argument.  */

  old_mode = GET_MODE (val);
  if (old_mode == VOIDmode)
    old_mode = TYPE_MODE (TREE_TYPE (exp));
  val = convert_modes (mode, old_mode, val, 1);
  return val;
}


/* Expand the __sync_xxx_and_fetch and __sync_fetch_and_xxx intrinsics.
   EXP is the CALL_EXPR.  CODE is the rtx code
   that corresponds to the arithmetic or logical operation from the name;
   an exception here is that NOT actually means NAND.  TARGET is an optional
   place for us to store the results; AFTER is true if this is the
   fetch_and_xxx form.  */

static rtx
expand_builtin_sync_operation (enum machine_mode mode, tree exp,
			       enum rtx_code code, bool after,
			       rtx target)
{
  rtx val, mem;
  location_t loc = EXPR_LOCATION (exp);

  if (code == NOT && warn_sync_nand)
    {
      tree fndecl = get_callee_fndecl (exp);
      enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);

      static bool warned_f_a_n, warned_n_a_f;

      switch (fcode)
	{
	case BUILT_IN_SYNC_FETCH_AND_NAND_1:
	case BUILT_IN_SYNC_FETCH_AND_NAND_2:
	case BUILT_IN_SYNC_FETCH_AND_NAND_4:
	case BUILT_IN_SYNC_FETCH_AND_NAND_8:
	case BUILT_IN_SYNC_FETCH_AND_NAND_16:
	  if (warned_f_a_n)
	    break;

	  fndecl = builtin_decl_implicit (BUILT_IN_SYNC_FETCH_AND_NAND_N);
	  inform (loc, "%qD changed semantics in GCC 4.4", fndecl);
	  warned_f_a_n = true;
	  break;

	case BUILT_IN_SYNC_NAND_AND_FETCH_1:
	case BUILT_IN_SYNC_NAND_AND_FETCH_2:
	case BUILT_IN_SYNC_NAND_AND_FETCH_4:
	case BUILT_IN_SYNC_NAND_AND_FETCH_8:
	case BUILT_IN_SYNC_NAND_AND_FETCH_16:
	  if (warned_n_a_f)
	    break;

	 fndecl = builtin_decl_implicit (BUILT_IN_SYNC_NAND_AND_FETCH_N);
	  inform (loc, "%qD changed semantics in GCC 4.4", fndecl);
	  warned_n_a_f = true;
	  break;

	default:
	  gcc_unreachable ();
	}
    }

  /* Expand the operands.  */
  mem = get_builtin_sync_mem (CALL_EXPR_ARG (exp, 0), mode);
  val = expand_expr_force_mode (CALL_EXPR_ARG (exp, 1), mode);

  return expand_atomic_fetch_op (target, mem, val, code, MEMMODEL_SEQ_CST,
				 after);
}

/* Expand the __sync_val_compare_and_swap and __sync_bool_compare_and_swap
   intrinsics. EXP is the CALL_EXPR.  IS_BOOL is
   true if this is the boolean form.  TARGET is a place for us to store the
   results; this is NOT optional if IS_BOOL is true.  */

static rtx
expand_builtin_compare_and_swap (enum machine_mode mode, tree exp,
				 bool is_bool, rtx target)
{
  rtx old_val, new_val, mem;
  rtx *pbool, *poval;

  /* Expand the operands.  */
  mem = get_builtin_sync_mem (CALL_EXPR_ARG (exp, 0), mode);
  old_val = expand_expr_force_mode (CALL_EXPR_ARG (exp, 1), mode);
  new_val = expand_expr_force_mode (CALL_EXPR_ARG (exp, 2), mode);

  pbool = poval = NULL;
  if (target != const0_rtx)
    {
      if (is_bool)
	pbool = &target;
      else
	poval = &target;
    }
  if (!expand_atomic_compare_and_swap (pbool, poval, mem, old_val, new_val,
				       false, MEMMODEL_SEQ_CST,
				       MEMMODEL_SEQ_CST))
    return NULL_RTX;

  return target;
}

/* Expand the __sync_lock_test_and_set intrinsic.  Note that the most
   general form is actually an atomic exchange, and some targets only
   support a reduced form with the second argument being a constant 1.
   EXP is the CALL_EXPR; TARGET is an optional place for us to store
   the results.  */

static rtx
expand_builtin_sync_lock_test_and_set (enum machine_mode mode, tree exp,
				       rtx target)
{
  rtx val, mem;

  /* Expand the operands.  */
  mem = get_builtin_sync_mem (CALL_EXPR_ARG (exp, 0), mode);
  val = expand_expr_force_mode (CALL_EXPR_ARG (exp, 1), mode);

  return expand_sync_lock_test_and_set (target, mem, val);
}

/* Expand the __sync_lock_release intrinsic.  EXP is the CALL_EXPR.  */

static void
expand_builtin_sync_lock_release (enum machine_mode mode, tree exp)
{
  rtx mem;

  /* Expand the operands.  */
  mem = get_builtin_sync_mem (CALL_EXPR_ARG (exp, 0), mode);

  expand_atomic_store (mem, const0_rtx, MEMMODEL_RELEASE, true);
}

/* Given an integer representing an ``enum memmodel'', verify its
   correctness and return the memory model enum.  */

static enum memmodel
get_memmodel (tree exp)
{
  rtx op;

  /* If the parameter is not a constant, it's a run time value so we'll just
     convert it to MEMMODEL_SEQ_CST to avoid annoying runtime checking.  */
  if (TREE_CODE (exp) != INTEGER_CST)
    return MEMMODEL_SEQ_CST;

  op = expand_normal (exp);
  if (INTVAL (op) < 0 || INTVAL (op) >= MEMMODEL_LAST)
    {
      warning (OPT_Winvalid_memory_model,
	       "invalid memory model argument to builtin");
      return MEMMODEL_SEQ_CST;
    }
  return (enum memmodel) INTVAL (op);
}

/* Expand the __atomic_exchange intrinsic:
   	TYPE __atomic_exchange (TYPE *object, TYPE desired, enum memmodel)
   EXP is the CALL_EXPR.
   TARGET is an optional place for us to store the results.  */

static rtx
expand_builtin_atomic_exchange (enum machine_mode mode, tree exp, rtx target)
{
  rtx val, mem;
  enum memmodel model;

  model = get_memmodel (CALL_EXPR_ARG (exp, 2));
  if (model == MEMMODEL_CONSUME)
    {
      error ("invalid memory model for %<__atomic_exchange%>");
      return NULL_RTX;
    }

  if (!flag_inline_atomics)
    return NULL_RTX;

  /* Expand the operands.  */
  mem = get_builtin_sync_mem (CALL_EXPR_ARG (exp, 0), mode);
  val = expand_expr_force_mode (CALL_EXPR_ARG (exp, 1), mode);

  return expand_atomic_exchange (target, mem, val, model);
}

/* Expand the __atomic_compare_exchange intrinsic:
   	bool __atomic_compare_exchange (TYPE *object, TYPE *expect, 
					TYPE desired, BOOL weak, 
					enum memmodel success,
					enum memmodel failure)
   EXP is the CALL_EXPR.
   TARGET is an optional place for us to store the results.  */

static rtx
expand_builtin_atomic_compare_exchange (enum machine_mode mode, tree exp, 
					rtx target)
{
  rtx expect, desired, mem, oldval;
  enum memmodel success, failure;
  tree weak;
  bool is_weak;

  success = get_memmodel (CALL_EXPR_ARG (exp, 4));
  failure = get_memmodel (CALL_EXPR_ARG (exp, 5));

  if (failure == MEMMODEL_RELEASE || failure == MEMMODEL_ACQ_REL)
    {
      error ("invalid failure memory model for %<__atomic_compare_exchange%>");
      return NULL_RTX;
    }

  if (failure > success)
    {
      error ("failure memory model cannot be stronger than success "
	     "memory model for %<__atomic_compare_exchange%>");
      return NULL_RTX;
    }
  
  if (!flag_inline_atomics)
    return NULL_RTX;

  /* Expand the operands.  */
  mem = get_builtin_sync_mem (CALL_EXPR_ARG (exp, 0), mode);

  expect = expand_normal (CALL_EXPR_ARG (exp, 1));
  expect = convert_memory_address (Pmode, expect);
  desired = expand_expr_force_mode (CALL_EXPR_ARG (exp, 2), mode);

  weak = CALL_EXPR_ARG (exp, 3);
  is_weak = false;
  if (host_integerp (weak, 0) && tree_low_cst (weak, 0) != 0)
    is_weak = true;

  oldval = copy_to_reg (gen_rtx_MEM (mode, expect));

  if (!expand_atomic_compare_and_swap ((target == const0_rtx ? NULL : &target),
				       &oldval, mem, oldval, desired,
				       is_weak, success, failure))
    return NULL_RTX;

  emit_move_insn (gen_rtx_MEM (mode, expect), oldval);
  return target;
}

/* Expand the __atomic_load intrinsic:
   	TYPE __atomic_load (TYPE *object, enum memmodel)
   EXP is the CALL_EXPR.
   TARGET is an optional place for us to store the results.  */

static rtx
expand_builtin_atomic_load (enum machine_mode mode, tree exp, rtx target)
{
  rtx mem;
  enum memmodel model;

  model = get_memmodel (CALL_EXPR_ARG (exp, 1));
  if (model == MEMMODEL_RELEASE
      || model == MEMMODEL_ACQ_REL)
    {
      error ("invalid memory model for %<__atomic_load%>");
      return NULL_RTX;
    }

  if (!flag_inline_atomics)
    return NULL_RTX;

  /* Expand the operand.  */
  mem = get_builtin_sync_mem (CALL_EXPR_ARG (exp, 0), mode);

  return expand_atomic_load (target, mem, model);
}


/* Expand the __atomic_store intrinsic:
   	void __atomic_store (TYPE *object, TYPE desired, enum memmodel)
   EXP is the CALL_EXPR.
   TARGET is an optional place for us to store the results.  */

static rtx
expand_builtin_atomic_store (enum machine_mode mode, tree exp)
{
  rtx mem, val;
  enum memmodel model;

  model = get_memmodel (CALL_EXPR_ARG (exp, 2));
  if (model != MEMMODEL_RELAXED
      && model != MEMMODEL_SEQ_CST
      && model != MEMMODEL_RELEASE)
    {
      error ("invalid memory model for %<__atomic_store%>");
      return NULL_RTX;
    }

  if (!flag_inline_atomics)
    return NULL_RTX;

  /* Expand the operands.  */
  mem = get_builtin_sync_mem (CALL_EXPR_ARG (exp, 0), mode);
  val = expand_expr_force_mode (CALL_EXPR_ARG (exp, 1), mode);

  return expand_atomic_store (mem, val, model, false);
}

/* Expand the __atomic_fetch_XXX intrinsic:
   	TYPE __atomic_fetch_XXX (TYPE *object, TYPE val, enum memmodel)
   EXP is the CALL_EXPR.
   TARGET is an optional place for us to store the results.
   CODE is the operation, PLUS, MINUS, ADD, XOR, or IOR.
   FETCH_AFTER is true if returning the result of the operation.
   FETCH_AFTER is false if returning the value before the operation.
   IGNORE is true if the result is not used.
   EXT_CALL is the correct builtin for an external call if this cannot be
   resolved to an instruction sequence.  */

static rtx
expand_builtin_atomic_fetch_op (enum machine_mode mode, tree exp, rtx target,
				enum rtx_code code, bool fetch_after,
				bool ignore, enum built_in_function ext_call)
{
  rtx val, mem, ret;
  enum memmodel model;
  tree fndecl;
  tree addr;

  model = get_memmodel (CALL_EXPR_ARG (exp, 2));

  /* Expand the operands.  */
  mem = get_builtin_sync_mem (CALL_EXPR_ARG (exp, 0), mode);
  val = expand_expr_force_mode (CALL_EXPR_ARG (exp, 1), mode);

  /* Only try generating instructions if inlining is turned on.  */
  if (flag_inline_atomics)
    {
      ret = expand_atomic_fetch_op (target, mem, val, code, model, fetch_after);
      if (ret)
	return ret;
    }

  /* Return if a different routine isn't needed for the library call.  */
  if (ext_call == BUILT_IN_NONE)
    return NULL_RTX;

  /* Change the call to the specified function.  */
  fndecl = get_callee_fndecl (exp);
  addr = CALL_EXPR_FN (exp);
  STRIP_NOPS (addr);

  gcc_assert (TREE_OPERAND (addr, 0) == fndecl);
  TREE_OPERAND (addr, 0) = builtin_decl_explicit(ext_call);

  /* Expand the call here so we can emit trailing code.  */
  ret = expand_call (exp, target, ignore);

  /* Replace the original function just in case it matters.  */
  TREE_OPERAND (addr, 0) = fndecl;

  /* Then issue the arithmetic correction to return the right result.  */
  if (!ignore)
    {
      if (code == NOT)
	{
	  ret = expand_simple_binop (mode, AND, ret, val, NULL_RTX, true,
				     OPTAB_LIB_WIDEN);
	  ret = expand_simple_unop (mode, NOT, ret, target, true);
	}
      else
	ret = expand_simple_binop (mode, code, ret, val, target, true,
				   OPTAB_LIB_WIDEN);
    }
  return ret;
}


#ifndef HAVE_atomic_clear
# define HAVE_atomic_clear 0
# define gen_atomic_clear(x,y) (gcc_unreachable (), NULL_RTX)
#endif

/* Expand an atomic clear operation.
	void _atomic_clear (BOOL *obj, enum memmodel)
   EXP is the call expression.  */

static rtx
expand_builtin_atomic_clear (tree exp) 
{
  enum machine_mode mode;
  rtx mem, ret;
  enum memmodel model;

  mode = mode_for_size (BOOL_TYPE_SIZE, MODE_INT, 0);
  mem = get_builtin_sync_mem (CALL_EXPR_ARG (exp, 0), mode);
  model = get_memmodel (CALL_EXPR_ARG (exp, 1));

  if (model == MEMMODEL_ACQUIRE || model == MEMMODEL_ACQ_REL)
    {
      error ("invalid memory model for %<__atomic_store%>");
      return const0_rtx;
    }

  if (HAVE_atomic_clear)
    {
      emit_insn (gen_atomic_clear (mem, model));
      return const0_rtx;
    }

  /* Try issuing an __atomic_store, and allow fallback to __sync_lock_release.
     Failing that, a store is issued by __atomic_store.  The only way this can
     fail is if the bool type is larger than a word size.  Unlikely, but
     handle it anyway for completeness.  Assume a single threaded model since
     there is no atomic support in this case, and no barriers are required.  */
  ret = expand_atomic_store (mem, const0_rtx, model, true);
  if (!ret)
    emit_move_insn (mem, const0_rtx);
  return const0_rtx;
}

/* Expand an atomic test_and_set operation.
	bool _atomic_test_and_set (BOOL *obj, enum memmodel)
   EXP is the call expression.  */

static rtx
expand_builtin_atomic_test_and_set (tree exp, rtx target)
{
  rtx mem;
  enum memmodel model;
  enum machine_mode mode;

  mode = mode_for_size (BOOL_TYPE_SIZE, MODE_INT, 0);
  mem = get_builtin_sync_mem (CALL_EXPR_ARG (exp, 0), mode);
  model = get_memmodel (CALL_EXPR_ARG (exp, 1));

  return expand_atomic_test_and_set (target, mem, model);
}


/* Return true if (optional) argument ARG1 of size ARG0 is always lock free on
   this architecture.  If ARG1 is NULL, use typical alignment for size ARG0.  */

static tree
fold_builtin_atomic_always_lock_free (tree arg0, tree arg1)
{
  int size;
  enum machine_mode mode;
  unsigned int mode_align, type_align;

  if (TREE_CODE (arg0) != INTEGER_CST)
    return NULL_TREE;

  size = INTVAL (expand_normal (arg0)) * BITS_PER_UNIT;
  mode = mode_for_size (size, MODE_INT, 0);
  mode_align = GET_MODE_ALIGNMENT (mode);

  if (TREE_CODE (arg1) == INTEGER_CST && INTVAL (expand_normal (arg1)) == 0)
    type_align = mode_align;
  else
    {
      tree ttype = TREE_TYPE (arg1);

      /* This function is usually invoked and folded immediately by the front
	 end before anything else has a chance to look at it.  The pointer
	 parameter at this point is usually cast to a void *, so check for that
	 and look past the cast.  */
      if (TREE_CODE (arg1) == NOP_EXPR && POINTER_TYPE_P (ttype)
	  && VOID_TYPE_P (TREE_TYPE (ttype)))
	arg1 = TREE_OPERAND (arg1, 0);

      ttype = TREE_TYPE (arg1);
      gcc_assert (POINTER_TYPE_P (ttype));

      /* Get the underlying type of the object.  */
      ttype = TREE_TYPE (ttype);
      type_align = TYPE_ALIGN (ttype);
    }

  /* If the object has smaller alignment, the the lock free routines cannot
     be used.  */
  if (type_align < mode_align)
    return boolean_false_node;

  /* Check if a compare_and_swap pattern exists for the mode which represents
     the required size.  The pattern is not allowed to fail, so the existence
     of the pattern indicates support is present.  */
  if (can_compare_and_swap_p (mode, true))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Return true if the parameters to call EXP represent an object which will
   always generate lock free instructions.  The first argument represents the
   size of the object, and the second parameter is a pointer to the object 
   itself.  If NULL is passed for the object, then the result is based on 
   typical alignment for an object of the specified size.  Otherwise return 
   false.  */

static rtx
expand_builtin_atomic_always_lock_free (tree exp)
{
  tree size;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);

  if (TREE_CODE (arg0) != INTEGER_CST)
    {
      error ("non-constant argument 1 to __atomic_always_lock_free");
      return const0_rtx;
    }

  size = fold_builtin_atomic_always_lock_free (arg0, arg1);
  if (size == boolean_true_node)
    return const1_rtx;
  return const0_rtx;
}

/* Return a one or zero if it can be determined that object ARG1 of size ARG 
   is lock free on this architecture.  */

static tree
fold_builtin_atomic_is_lock_free (tree arg0, tree arg1)
{
  if (!flag_inline_atomics)
    return NULL_TREE;
  
  /* If it isn't always lock free, don't generate a result.  */
  if (fold_builtin_atomic_always_lock_free (arg0, arg1) == boolean_true_node)
    return boolean_true_node;

  return NULL_TREE;
}

/* Return true if the parameters to call EXP represent an object which will
   always generate lock free instructions.  The first argument represents the
   size of the object, and the second parameter is a pointer to the object 
   itself.  If NULL is passed for the object, then the result is based on 
   typical alignment for an object of the specified size.  Otherwise return 
   NULL*/

static rtx
expand_builtin_atomic_is_lock_free (tree exp)
{
  tree size;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);

  if (!INTEGRAL_TYPE_P (TREE_TYPE (arg0)))
    {
      error ("non-integer argument 1 to __atomic_is_lock_free");
      return NULL_RTX;
    }

  if (!flag_inline_atomics)
    return NULL_RTX; 

  /* If the value is known at compile time, return the RTX for it.  */
  size = fold_builtin_atomic_is_lock_free (arg0, arg1);
  if (size == boolean_true_node)
    return const1_rtx;

  return NULL_RTX;
}

/* Expand the __atomic_thread_fence intrinsic:
   	void __atomic_thread_fence (enum memmodel)
   EXP is the CALL_EXPR.  */

static void
expand_builtin_atomic_thread_fence (tree exp)
{
  enum memmodel model = get_memmodel (CALL_EXPR_ARG (exp, 0));
  expand_mem_thread_fence (model);
}

/* Expand the __atomic_signal_fence intrinsic:
   	void __atomic_signal_fence (enum memmodel)
   EXP is the CALL_EXPR.  */

static void
expand_builtin_atomic_signal_fence (tree exp)
{
  enum memmodel model = get_memmodel (CALL_EXPR_ARG (exp, 0));
  expand_mem_signal_fence (model);
}

/* Expand the __sync_synchronize intrinsic.  */

static void
expand_builtin_sync_synchronize (void)
{
  expand_mem_thread_fence (MEMMODEL_SEQ_CST);
}


/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */

rtx
expand_builtin (tree exp, rtx target, rtx subtarget, enum machine_mode mode,
		int ignore)
{
  tree fndecl = get_callee_fndecl (exp);
  enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);
  enum machine_mode target_mode = TYPE_MODE (TREE_TYPE (exp));
  int flags;

  if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_MD)
    return targetm.expand_builtin (exp, target, subtarget, mode, ignore);

  /* When not optimizing, generate calls to library functions for a certain
     set of builtins.  */
  if (!optimize
      && !called_as_built_in (fndecl)
      && fcode != BUILT_IN_ALLOCA
      && fcode != BUILT_IN_ALLOCA_WITH_ALIGN
      && fcode != BUILT_IN_FREE)
    return expand_call (exp, target, ignore);

  /* The built-in function expanders test for target == const0_rtx
     to determine whether the function's result will be ignored.  */
  if (ignore)
    target = const0_rtx;

  /* If the result of a pure or const built-in function is ignored, and
     none of its arguments are volatile, we can avoid expanding the
     built-in call and just evaluate the arguments for side-effects.  */
  if (target == const0_rtx
      && ((flags = flags_from_decl_or_type (fndecl)) & (ECF_CONST | ECF_PURE))
      && !(flags & ECF_LOOPING_CONST_OR_PURE))
    {
      bool volatilep = false;
      tree arg;
      call_expr_arg_iterator iter;

      FOR_EACH_CALL_EXPR_ARG (arg, iter, exp)
	if (TREE_THIS_VOLATILE (arg))
	  {
	    volatilep = true;
	    break;
	  }

      if (! volatilep)
	{
	  FOR_EACH_CALL_EXPR_ARG (arg, iter, exp)
	    expand_expr (arg, const0_rtx, VOIDmode, EXPAND_NORMAL);
	  return const0_rtx;
	}
    }

  switch (fcode)
    {
    case BUILT_IN_APPLY_ARGS:
      return expand_builtin_apply_args ();

      /* __builtin_apply (FUNCTION, ARGUMENTS, ARGSIZE) invokes
	 FUNCTION with a copy of the parameters described by
	 ARGUMENTS, and ARGSIZE.  It returns a block of memory
	 allocated on the stack into which is stored all the registers
	 that might possibly be used for returning the result of a
	 function.  ARGUMENTS is the value returned by
	 __builtin_apply_args.  ARGSIZE is the number of bytes of
	 arguments that must be copied.  ??? How should this value be
	 computed?  We'll also need a safe worst case value for varargs
	 functions.  */
    case BUILT_IN_APPLY:
      if (!validate_arglist (exp, POINTER_TYPE,
			     POINTER_TYPE, INTEGER_TYPE, VOID_TYPE)
	  && !validate_arglist (exp, REFERENCE_TYPE,
				POINTER_TYPE, INTEGER_TYPE, VOID_TYPE))
	return const0_rtx;
      else
	{
	  rtx ops[3];

	  ops[0] = expand_normal (CALL_EXPR_ARG (exp, 0));
	  ops[1] = expand_normal (CALL_EXPR_ARG (exp, 1));
	  ops[2] = expand_normal (CALL_EXPR_ARG (exp, 2));

	  return expand_builtin_apply (ops[0], ops[1], ops[2]);
	}

      /* __builtin_return (RESULT) causes the function to return the
	 value described by RESULT.  RESULT is address of the block of
	 memory returned by __builtin_apply.  */
    case BUILT_IN_RETURN:
      if (validate_arglist (exp, POINTER_TYPE, VOID_TYPE))
	expand_builtin_return (expand_normal (CALL_EXPR_ARG (exp, 0)));
      return const0_rtx;

    case BUILT_IN_SAVEREGS:
      return expand_builtin_saveregs ();

    case BUILT_IN_VA_ARG_PACK:
      /* All valid uses of __builtin_va_arg_pack () are removed during
	 inlining.  */
      error ("%Kinvalid use of %<__builtin_va_arg_pack ()%>", exp);
      return const0_rtx;

    case BUILT_IN_VA_ARG_PACK_LEN:
      /* All valid uses of __builtin_va_arg_pack_len () are removed during
	 inlining.  */
      error ("%Kinvalid use of %<__builtin_va_arg_pack_len ()%>", exp);
      return const0_rtx;

      /* Return the address of the first anonymous stack arg.  */
    case BUILT_IN_NEXT_ARG:
      if (fold_builtin_next_arg (exp, false))
	return const0_rtx;
      return expand_builtin_next_arg ();

    case BUILT_IN_CLEAR_CACHE:
      target = expand_builtin___clear_cache (exp);
      if (target)
        return target;
      break;

    case BUILT_IN_CONSTANT_P:
      return const0_rtx;

    case BUILT_IN_FRAME_ADDRESS:
    case BUILT_IN_RETURN_ADDRESS:
      return expand_builtin_frame_address (fndecl, exp);

    /* Returns the address of the area where the structure is returned.
       0 otherwise.  */
    case BUILT_IN_AGGREGATE_INCOMING_ADDRESS:
      if (call_expr_nargs (exp) != 0
	  || ! AGGREGATE_TYPE_P (TREE_TYPE (TREE_TYPE (current_function_decl)))
	  || !MEM_P (DECL_RTL (DECL_RESULT (current_function_decl))))
	return const0_rtx;
      else
	return XEXP (DECL_RTL (DECL_RESULT (current_function_decl)), 0);

    case BUILT_IN_ALLOCA:
    case BUILT_IN_ALLOCA_WITH_ALIGN:
      /* If the allocation stems from the declaration of a variable-sized
	 object, it cannot accumulate.  */
      target = expand_builtin_alloca (exp, CALL_ALLOCA_FOR_VAR_P (exp));
      if (target)
	return target;
      break;

    case BUILT_IN_STACK_SAVE:
      return expand_stack_save ();

    case BUILT_IN_STACK_RESTORE:
      expand_stack_restore (CALL_EXPR_ARG (exp, 0));
      return const0_rtx;

    case BUILT_IN_BSWAP32:
    case BUILT_IN_BSWAP64:
      target = expand_builtin_bswap (exp, target, subtarget);

      if (target)
	return target;
      break;

    CASE_INT_FN (BUILT_IN_FFS):
    case BUILT_IN_FFSIMAX:
      target = expand_builtin_unop (target_mode, exp, target,
				    subtarget, ffs_optab);
      if (target)
	return target;
      break;

    CASE_INT_FN (BUILT_IN_CLZ):
    case BUILT_IN_CLZIMAX:
      target = expand_builtin_unop (target_mode, exp, target,
				    subtarget, clz_optab);
      if (target)
	return target;
      break;

    CASE_INT_FN (BUILT_IN_CTZ):
    case BUILT_IN_CTZIMAX:
      target = expand_builtin_unop (target_mode, exp, target,
				    subtarget, ctz_optab);
      if (target)
	return target;
      break;

    CASE_INT_FN (BUILT_IN_CLRSB):
    case BUILT_IN_CLRSBIMAX:
      target = expand_builtin_unop (target_mode, exp, target,
				    subtarget, clrsb_optab);
      if (target)
	return target;
      break;

    CASE_INT_FN (BUILT_IN_POPCOUNT):
    case BUILT_IN_POPCOUNTIMAX:
      target = expand_builtin_unop (target_mode, exp, target,
				    subtarget, popcount_optab);
      if (target)
	return target;
      break;

    CASE_INT_FN (BUILT_IN_PARITY):
    case BUILT_IN_PARITYIMAX:
      target = expand_builtin_unop (target_mode, exp, target,
				    subtarget, parity_optab);
      if (target)
	return target;
      break;

    case BUILT_IN_MEMCPY:
      target = expand_builtin_memcpy (exp, target);
      if (target)
	return target;
      break;

    case BUILT_IN_MEMSET:
      target = expand_builtin_memset (exp, target, mode);
      if (target)
	return target;
      break;

    case BUILT_IN_BZERO:
      target = expand_builtin_bzero (exp);
      if (target)
	return target;
      break;

    case BUILT_IN_STRCMP:
      target = expand_builtin_strcmp (exp, target);
      if (target)
	return target;
      break;

    case BUILT_IN_STRNCMP:
      target = expand_builtin_strncmp (exp, target, mode);
      if (target)
	return target;
      break;

    case BUILT_IN_BCMP:
    case BUILT_IN_MEMCMP:
      target = expand_builtin_memcmp (exp, target, mode);
      if (target)
	return target;
      break;

    case BUILT_IN_SETJMP:
      /* This should have been lowered to the builtins below.  */
      gcc_unreachable ();

    case BUILT_IN_SETJMP_SETUP:
      /* __builtin_setjmp_setup is passed a pointer to an array of five words
          and the receiver label.  */
      if (validate_arglist (exp, POINTER_TYPE, POINTER_TYPE, VOID_TYPE))
	{
	  rtx buf_addr = expand_expr (CALL_EXPR_ARG (exp, 0), subtarget,
				      VOIDmode, EXPAND_NORMAL);
	  tree label = TREE_OPERAND (CALL_EXPR_ARG (exp, 1), 0);
	  rtx label_r = label_rtx (label);

	  /* This is copied from the handling of non-local gotos.  */
	  expand_builtin_setjmp_setup (buf_addr, label_r);
	  nonlocal_goto_handler_labels
	    = gen_rtx_EXPR_LIST (VOIDmode, label_r,
				 nonlocal_goto_handler_labels);
	  /* ??? Do not let expand_label treat us as such since we would
	     not want to be both on the list of non-local labels and on
	     the list of forced labels.  */
	  FORCED_LABEL (label) = 0;
	  return const0_rtx;
	}
      break;

    case BUILT_IN_SETJMP_DISPATCHER:
       /* __builtin_setjmp_dispatcher is passed the dispatcher label.  */
      if (validate_arglist (exp, POINTER_TYPE, VOID_TYPE))
	{
	  tree label = TREE_OPERAND (CALL_EXPR_ARG (exp, 0), 0);
	  rtx label_r = label_rtx (label);

	  /* Remove the dispatcher label from the list of non-local labels
	     since the receiver labels have been added to it above.  */
	  remove_node_from_expr_list (label_r, &nonlocal_goto_handler_labels);
	  return const0_rtx;
	}
      break;

    case BUILT_IN_SETJMP_RECEIVER:
       /* __builtin_setjmp_receiver is passed the receiver label.  */
      if (validate_arglist (exp, POINTER_TYPE, VOID_TYPE))
	{
	  tree label = TREE_OPERAND (CALL_EXPR_ARG (exp, 0), 0);
	  rtx label_r = label_rtx (label);

	  expand_builtin_setjmp_receiver (label_r);
	  return const0_rtx;
	}
      break;

      /* __builtin_longjmp is passed a pointer to an array of five words.
	 It's similar to the C library longjmp function but works with
	 __builtin_setjmp above.  */
    case BUILT_IN_LONGJMP:
      if (validate_arglist (exp, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE))
	{
	  rtx buf_addr = expand_expr (CALL_EXPR_ARG (exp, 0), subtarget,
				      VOIDmode, EXPAND_NORMAL);
	  rtx value = expand_normal (CALL_EXPR_ARG (exp, 1));

	  if (value != const1_rtx)
	    {
	      error ("%<__builtin_longjmp%> second argument must be 1");
	      return const0_rtx;
	    }

	  expand_builtin_longjmp (buf_addr, value);
	  return const0_rtx;
	}
      break;

    case BUILT_IN_NONLOCAL_GOTO:
      target = expand_builtin_nonlocal_goto (exp);
      if (target)
	return target;
      break;

      /* This updates the setjmp buffer that is its argument with the value
	 of the current stack pointer.  */
    case BUILT_IN_UPDATE_SETJMP_BUF:
      if (validate_arglist (exp, POINTER_TYPE, VOID_TYPE))
	{
	  rtx buf_addr
	    = expand_normal (CALL_EXPR_ARG (exp, 0));

	  expand_builtin_update_setjmp_buf (buf_addr);
	  return const0_rtx;
	}
      break;

    case BUILT_IN_TRAP:
      expand_builtin_trap ();
      return const0_rtx;

    case BUILT_IN_UNREACHABLE:
      expand_builtin_unreachable ();
      return const0_rtx;

      /* Various hooks for the DWARF 2 __throw routine.  */
    case BUILT_IN_UNWIND_INIT:
      expand_builtin_unwind_init ();
      return const0_rtx;
    case BUILT_IN_DWARF_CFA:
      return virtual_cfa_rtx;
#ifdef DWARF2_UNWIND_INFO
    case BUILT_IN_DWARF_SP_COLUMN:
      return expand_builtin_dwarf_sp_column ();
    case BUILT_IN_INIT_DWARF_REG_SIZES:
      expand_builtin_init_dwarf_reg_sizes (CALL_EXPR_ARG (exp, 0));
      return const0_rtx;
#endif
    case BUILT_IN_FROB_RETURN_ADDR:
      return expand_builtin_frob_return_addr (CALL_EXPR_ARG (exp, 0));
    case BUILT_IN_EXTRACT_RETURN_ADDR:
      return expand_builtin_extract_return_addr (CALL_EXPR_ARG (exp, 0));
    case BUILT_IN_EH_RETURN:
      expand_builtin_eh_return (CALL_EXPR_ARG (exp, 0),
				CALL_EXPR_ARG (exp, 1));
      return const0_rtx;
#ifdef EH_RETURN_DATA_REGNO
    case BUILT_IN_EH_RETURN_DATA_REGNO:
      return expand_builtin_eh_return_data_regno (exp);
#endif
    case BUILT_IN_EXTEND_POINTER:
      return expand_builtin_extend_pointer (CALL_EXPR_ARG (exp, 0));
    case BUILT_IN_EH_POINTER:
      return expand_builtin_eh_pointer (exp);
    case BUILT_IN_EH_FILTER:
      return expand_builtin_eh_filter (exp);
    case BUILT_IN_EH_COPY_VALUES:
      return expand_builtin_eh_copy_values (exp);

    case BUILT_IN_VA_START:
      return expand_builtin_va_start (exp);
    case BUILT_IN_VA_END:
      return expand_builtin_va_end (exp);
    case BUILT_IN_VA_COPY:
      return expand_builtin_va_copy (exp);
    case BUILT_IN_EXPECT:
      return expand_builtin_expect (exp, target);
    case BUILT_IN_ASSUME_ALIGNED:
      return expand_builtin_assume_aligned (exp, target);
    case BUILT_IN_PREFETCH:
      expand_builtin_prefetch (exp);
      return const0_rtx;

    case BUILT_IN_INIT_TRAMPOLINE:
      return expand_builtin_init_trampoline (exp, true);
    case BUILT_IN_INIT_HEAP_TRAMPOLINE:
      return expand_builtin_init_trampoline (exp, false);
    case BUILT_IN_ADJUST_TRAMPOLINE:
      return expand_builtin_adjust_trampoline (exp);

    case BUILT_IN_SYNC_FETCH_AND_ADD_1:
    case BUILT_IN_SYNC_FETCH_AND_ADD_2:
    case BUILT_IN_SYNC_FETCH_AND_ADD_4:
    case BUILT_IN_SYNC_FETCH_AND_ADD_8:
    case BUILT_IN_SYNC_FETCH_AND_ADD_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_SYNC_FETCH_AND_ADD_1);
      target = expand_builtin_sync_operation (mode, exp, PLUS, false, target);
      if (target)
	return target;
      break;

    case BUILT_IN_SYNC_FETCH_AND_SUB_1:
    case BUILT_IN_SYNC_FETCH_AND_SUB_2:
    case BUILT_IN_SYNC_FETCH_AND_SUB_4:
    case BUILT_IN_SYNC_FETCH_AND_SUB_8:
    case BUILT_IN_SYNC_FETCH_AND_SUB_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_SYNC_FETCH_AND_SUB_1);
      target = expand_builtin_sync_operation (mode, exp, MINUS, false, target);
      if (target)
	return target;
      break;

    case BUILT_IN_SYNC_FETCH_AND_OR_1:
    case BUILT_IN_SYNC_FETCH_AND_OR_2:
    case BUILT_IN_SYNC_FETCH_AND_OR_4:
    case BUILT_IN_SYNC_FETCH_AND_OR_8:
    case BUILT_IN_SYNC_FETCH_AND_OR_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_SYNC_FETCH_AND_OR_1);
      target = expand_builtin_sync_operation (mode, exp, IOR, false, target);
      if (target)
	return target;
      break;

    case BUILT_IN_SYNC_FETCH_AND_AND_1:
    case BUILT_IN_SYNC_FETCH_AND_AND_2:
    case BUILT_IN_SYNC_FETCH_AND_AND_4:
    case BUILT_IN_SYNC_FETCH_AND_AND_8:
    case BUILT_IN_SYNC_FETCH_AND_AND_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_SYNC_FETCH_AND_AND_1);
      target = expand_builtin_sync_operation (mode, exp, AND, false, target);
      if (target)
	return target;
      break;

    case BUILT_IN_SYNC_FETCH_AND_XOR_1:
    case BUILT_IN_SYNC_FETCH_AND_XOR_2:
    case BUILT_IN_SYNC_FETCH_AND_XOR_4:
    case BUILT_IN_SYNC_FETCH_AND_XOR_8:
    case BUILT_IN_SYNC_FETCH_AND_XOR_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_SYNC_FETCH_AND_XOR_1);
      target = expand_builtin_sync_operation (mode, exp, XOR, false, target);
      if (target)
	return target;
      break;

    case BUILT_IN_SYNC_FETCH_AND_NAND_1:
    case BUILT_IN_SYNC_FETCH_AND_NAND_2:
    case BUILT_IN_SYNC_FETCH_AND_NAND_4:
    case BUILT_IN_SYNC_FETCH_AND_NAND_8:
    case BUILT_IN_SYNC_FETCH_AND_NAND_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_SYNC_FETCH_AND_NAND_1);
      target = expand_builtin_sync_operation (mode, exp, NOT, false, target);
      if (target)
	return target;
      break;

    case BUILT_IN_SYNC_ADD_AND_FETCH_1:
    case BUILT_IN_SYNC_ADD_AND_FETCH_2:
    case BUILT_IN_SYNC_ADD_AND_FETCH_4:
    case BUILT_IN_SYNC_ADD_AND_FETCH_8:
    case BUILT_IN_SYNC_ADD_AND_FETCH_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_SYNC_ADD_AND_FETCH_1);
      target = expand_builtin_sync_operation (mode, exp, PLUS, true, target);
      if (target)
	return target;
      break;

    case BUILT_IN_SYNC_SUB_AND_FETCH_1:
    case BUILT_IN_SYNC_SUB_AND_FETCH_2:
    case BUILT_IN_SYNC_SUB_AND_FETCH_4:
    case BUILT_IN_SYNC_SUB_AND_FETCH_8:
    case BUILT_IN_SYNC_SUB_AND_FETCH_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_SYNC_SUB_AND_FETCH_1);
      target = expand_builtin_sync_operation (mode, exp, MINUS, true, target);
      if (target)
	return target;
      break;

    case BUILT_IN_SYNC_OR_AND_FETCH_1:
    case BUILT_IN_SYNC_OR_AND_FETCH_2:
    case BUILT_IN_SYNC_OR_AND_FETCH_4:
    case BUILT_IN_SYNC_OR_AND_FETCH_8:
    case BUILT_IN_SYNC_OR_AND_FETCH_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_SYNC_OR_AND_FETCH_1);
      target = expand_builtin_sync_operation (mode, exp, IOR, true, target);
      if (target)
	return target;
      break;

    case BUILT_IN_SYNC_AND_AND_FETCH_1:
    case BUILT_IN_SYNC_AND_AND_FETCH_2:
    case BUILT_IN_SYNC_AND_AND_FETCH_4:
    case BUILT_IN_SYNC_AND_AND_FETCH_8:
    case BUILT_IN_SYNC_AND_AND_FETCH_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_SYNC_AND_AND_FETCH_1);
      target = expand_builtin_sync_operation (mode, exp, AND, true, target);
      if (target)
	return target;
      break;

    case BUILT_IN_SYNC_XOR_AND_FETCH_1:
    case BUILT_IN_SYNC_XOR_AND_FETCH_2:
    case BUILT_IN_SYNC_XOR_AND_FETCH_4:
    case BUILT_IN_SYNC_XOR_AND_FETCH_8:
    case BUILT_IN_SYNC_XOR_AND_FETCH_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_SYNC_XOR_AND_FETCH_1);
      target = expand_builtin_sync_operation (mode, exp, XOR, true, target);
      if (target)
	return target;
      break;

    case BUILT_IN_SYNC_NAND_AND_FETCH_1:
    case BUILT_IN_SYNC_NAND_AND_FETCH_2:
    case BUILT_IN_SYNC_NAND_AND_FETCH_4:
    case BUILT_IN_SYNC_NAND_AND_FETCH_8:
    case BUILT_IN_SYNC_NAND_AND_FETCH_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_SYNC_NAND_AND_FETCH_1);
      target = expand_builtin_sync_operation (mode, exp, NOT, true, target);
      if (target)
	return target;
      break;

    case BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_1:
    case BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_2:
    case BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_4:
    case BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_8:
    case BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_16:
      if (mode == VOIDmode)
	mode = TYPE_MODE (boolean_type_node);
      if (!target || !register_operand (target, mode))
	target = gen_reg_rtx (mode);

      mode = get_builtin_sync_mode 
				(fcode - BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_1);
      target = expand_builtin_compare_and_swap (mode, exp, true, target);
      if (target)
	return target;
      break;

    case BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_1:
    case BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_2:
    case BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_4:
    case BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_8:
    case BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_16:
      mode = get_builtin_sync_mode 
				(fcode - BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_1);
      target = expand_builtin_compare_and_swap (mode, exp, false, target);
      if (target)
	return target;
      break;

    case BUILT_IN_SYNC_LOCK_TEST_AND_SET_1:
    case BUILT_IN_SYNC_LOCK_TEST_AND_SET_2:
    case BUILT_IN_SYNC_LOCK_TEST_AND_SET_4:
    case BUILT_IN_SYNC_LOCK_TEST_AND_SET_8:
    case BUILT_IN_SYNC_LOCK_TEST_AND_SET_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_SYNC_LOCK_TEST_AND_SET_1);
      target = expand_builtin_sync_lock_test_and_set (mode, exp, target);
      if (target)
	return target;
      break;

    case BUILT_IN_SYNC_LOCK_RELEASE_1:
    case BUILT_IN_SYNC_LOCK_RELEASE_2:
    case BUILT_IN_SYNC_LOCK_RELEASE_4:
    case BUILT_IN_SYNC_LOCK_RELEASE_8:
    case BUILT_IN_SYNC_LOCK_RELEASE_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_SYNC_LOCK_RELEASE_1);
      expand_builtin_sync_lock_release (mode, exp);
      return const0_rtx;

    case BUILT_IN_SYNC_SYNCHRONIZE:
      expand_builtin_sync_synchronize ();
      return const0_rtx;

    case BUILT_IN_ATOMIC_EXCHANGE_1:
    case BUILT_IN_ATOMIC_EXCHANGE_2:
    case BUILT_IN_ATOMIC_EXCHANGE_4:
    case BUILT_IN_ATOMIC_EXCHANGE_8:
    case BUILT_IN_ATOMIC_EXCHANGE_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_ATOMIC_EXCHANGE_1);
      target = expand_builtin_atomic_exchange (mode, exp, target);
      if (target)
	return target;
      break;

    case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_1:
    case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_2:
    case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_4:
    case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_8:
    case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_16:
      {
	unsigned int nargs, z;
	VEC(tree,gc) *vec;

	mode = 
	    get_builtin_sync_mode (fcode - BUILT_IN_ATOMIC_COMPARE_EXCHANGE_1);
	target = expand_builtin_atomic_compare_exchange (mode, exp, target);
	if (target)
	  return target;

	/* If this is turned into an external library call, the weak parameter
	   must be dropped to match the expected parameter list.  */
	nargs = call_expr_nargs (exp);
	vec = VEC_alloc (tree, gc, nargs - 1);
	for (z = 0; z < 3; z++)
	  VEC_quick_push (tree, vec, CALL_EXPR_ARG (exp, z));
	/* Skip the boolean weak parameter.  */
	for (z = 4; z < 6; z++)
	  VEC_quick_push (tree, vec, CALL_EXPR_ARG (exp, z));
	exp = build_call_vec (TREE_TYPE (exp), CALL_EXPR_FN (exp), vec);
	break;
      }

    case BUILT_IN_ATOMIC_LOAD_1:
    case BUILT_IN_ATOMIC_LOAD_2:
    case BUILT_IN_ATOMIC_LOAD_4:
    case BUILT_IN_ATOMIC_LOAD_8:
    case BUILT_IN_ATOMIC_LOAD_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_ATOMIC_LOAD_1);
      target = expand_builtin_atomic_load (mode, exp, target);
      if (target)
	return target;
      break;

    case BUILT_IN_ATOMIC_STORE_1:
    case BUILT_IN_ATOMIC_STORE_2:
    case BUILT_IN_ATOMIC_STORE_4:
    case BUILT_IN_ATOMIC_STORE_8:
    case BUILT_IN_ATOMIC_STORE_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_ATOMIC_STORE_1);
      target = expand_builtin_atomic_store (mode, exp);
      if (target)
	return const0_rtx;
      break;

    case BUILT_IN_ATOMIC_ADD_FETCH_1:
    case BUILT_IN_ATOMIC_ADD_FETCH_2:
    case BUILT_IN_ATOMIC_ADD_FETCH_4:
    case BUILT_IN_ATOMIC_ADD_FETCH_8:
    case BUILT_IN_ATOMIC_ADD_FETCH_16:
      {
	enum built_in_function lib;
	mode = get_builtin_sync_mode (fcode - BUILT_IN_ATOMIC_ADD_FETCH_1);
	lib = (enum built_in_function)((int)BUILT_IN_ATOMIC_FETCH_ADD_1 + 
				       (fcode - BUILT_IN_ATOMIC_ADD_FETCH_1));
	target = expand_builtin_atomic_fetch_op (mode, exp, target, PLUS, true,
						 ignore, lib);
	if (target)
	  return target;
	break;
      }
    case BUILT_IN_ATOMIC_SUB_FETCH_1:
    case BUILT_IN_ATOMIC_SUB_FETCH_2:
    case BUILT_IN_ATOMIC_SUB_FETCH_4:
    case BUILT_IN_ATOMIC_SUB_FETCH_8:
    case BUILT_IN_ATOMIC_SUB_FETCH_16:
      {
	enum built_in_function lib;
	mode = get_builtin_sync_mode (fcode - BUILT_IN_ATOMIC_SUB_FETCH_1);
	lib = (enum built_in_function)((int)BUILT_IN_ATOMIC_FETCH_SUB_1 + 
				       (fcode - BUILT_IN_ATOMIC_SUB_FETCH_1));
	target = expand_builtin_atomic_fetch_op (mode, exp, target, MINUS, true,
						 ignore, lib);
	if (target)
	  return target;
	break;
      }
    case BUILT_IN_ATOMIC_AND_FETCH_1:
    case BUILT_IN_ATOMIC_AND_FETCH_2:
    case BUILT_IN_ATOMIC_AND_FETCH_4:
    case BUILT_IN_ATOMIC_AND_FETCH_8:
    case BUILT_IN_ATOMIC_AND_FETCH_16:
      {
	enum built_in_function lib;
	mode = get_builtin_sync_mode (fcode - BUILT_IN_ATOMIC_AND_FETCH_1);
	lib = (enum built_in_function)((int)BUILT_IN_ATOMIC_FETCH_AND_1 + 
				       (fcode - BUILT_IN_ATOMIC_AND_FETCH_1));
	target = expand_builtin_atomic_fetch_op (mode, exp, target, AND, true,
						 ignore, lib);
	if (target)
	  return target;
	break;
      }
    case BUILT_IN_ATOMIC_NAND_FETCH_1:
    case BUILT_IN_ATOMIC_NAND_FETCH_2:
    case BUILT_IN_ATOMIC_NAND_FETCH_4:
    case BUILT_IN_ATOMIC_NAND_FETCH_8:
    case BUILT_IN_ATOMIC_NAND_FETCH_16:
      {
	enum built_in_function lib;
	mode = get_builtin_sync_mode (fcode - BUILT_IN_ATOMIC_NAND_FETCH_1);
	lib = (enum built_in_function)((int)BUILT_IN_ATOMIC_FETCH_NAND_1 + 
				       (fcode - BUILT_IN_ATOMIC_NAND_FETCH_1));
	target = expand_builtin_atomic_fetch_op (mode, exp, target, NOT, true,
						 ignore, lib);
	if (target)
	  return target;
	break;
      }
    case BUILT_IN_ATOMIC_XOR_FETCH_1:
    case BUILT_IN_ATOMIC_XOR_FETCH_2:
    case BUILT_IN_ATOMIC_XOR_FETCH_4:
    case BUILT_IN_ATOMIC_XOR_FETCH_8:
    case BUILT_IN_ATOMIC_XOR_FETCH_16:
      {
	enum built_in_function lib;
	mode = get_builtin_sync_mode (fcode - BUILT_IN_ATOMIC_XOR_FETCH_1);
	lib = (enum built_in_function)((int)BUILT_IN_ATOMIC_FETCH_XOR_1 + 
				       (fcode - BUILT_IN_ATOMIC_XOR_FETCH_1));
	target = expand_builtin_atomic_fetch_op (mode, exp, target, XOR, true,
						 ignore, lib);
	if (target)
	  return target;
	break;
      }
    case BUILT_IN_ATOMIC_OR_FETCH_1:
    case BUILT_IN_ATOMIC_OR_FETCH_2:
    case BUILT_IN_ATOMIC_OR_FETCH_4:
    case BUILT_IN_ATOMIC_OR_FETCH_8:
    case BUILT_IN_ATOMIC_OR_FETCH_16:
      {
	enum built_in_function lib;
	mode = get_builtin_sync_mode (fcode - BUILT_IN_ATOMIC_OR_FETCH_1);
	lib = (enum built_in_function)((int)BUILT_IN_ATOMIC_FETCH_OR_1 + 
				       (fcode - BUILT_IN_ATOMIC_OR_FETCH_1));
	target = expand_builtin_atomic_fetch_op (mode, exp, target, IOR, true,
						 ignore, lib);
	if (target)
	  return target;
	break;
      }
    case BUILT_IN_ATOMIC_FETCH_ADD_1:
    case BUILT_IN_ATOMIC_FETCH_ADD_2:
    case BUILT_IN_ATOMIC_FETCH_ADD_4:
    case BUILT_IN_ATOMIC_FETCH_ADD_8:
    case BUILT_IN_ATOMIC_FETCH_ADD_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_ATOMIC_FETCH_ADD_1);
      target = expand_builtin_atomic_fetch_op (mode, exp, target, PLUS, false,
					       ignore, BUILT_IN_NONE);
      if (target)
	return target;
      break;
 
    case BUILT_IN_ATOMIC_FETCH_SUB_1:
    case BUILT_IN_ATOMIC_FETCH_SUB_2:
    case BUILT_IN_ATOMIC_FETCH_SUB_4:
    case BUILT_IN_ATOMIC_FETCH_SUB_8:
    case BUILT_IN_ATOMIC_FETCH_SUB_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_ATOMIC_FETCH_SUB_1);
      target = expand_builtin_atomic_fetch_op (mode, exp, target, MINUS, false,
					       ignore, BUILT_IN_NONE);
      if (target)
	return target;
      break;

    case BUILT_IN_ATOMIC_FETCH_AND_1:
    case BUILT_IN_ATOMIC_FETCH_AND_2:
    case BUILT_IN_ATOMIC_FETCH_AND_4:
    case BUILT_IN_ATOMIC_FETCH_AND_8:
    case BUILT_IN_ATOMIC_FETCH_AND_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_ATOMIC_FETCH_AND_1);
      target = expand_builtin_atomic_fetch_op (mode, exp, target, AND, false,
					       ignore, BUILT_IN_NONE);
      if (target)
	return target;
      break;
  
    case BUILT_IN_ATOMIC_FETCH_NAND_1:
    case BUILT_IN_ATOMIC_FETCH_NAND_2:
    case BUILT_IN_ATOMIC_FETCH_NAND_4:
    case BUILT_IN_ATOMIC_FETCH_NAND_8:
    case BUILT_IN_ATOMIC_FETCH_NAND_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_ATOMIC_FETCH_NAND_1);
      target = expand_builtin_atomic_fetch_op (mode, exp, target, NOT, false,
					       ignore, BUILT_IN_NONE);
      if (target)
	return target;
      break;
 
    case BUILT_IN_ATOMIC_FETCH_XOR_1:
    case BUILT_IN_ATOMIC_FETCH_XOR_2:
    case BUILT_IN_ATOMIC_FETCH_XOR_4:
    case BUILT_IN_ATOMIC_FETCH_XOR_8:
    case BUILT_IN_ATOMIC_FETCH_XOR_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_ATOMIC_FETCH_XOR_1);
      target = expand_builtin_atomic_fetch_op (mode, exp, target, XOR, false,
					       ignore, BUILT_IN_NONE);
      if (target)
	return target;
      break;
 
    case BUILT_IN_ATOMIC_FETCH_OR_1:
    case BUILT_IN_ATOMIC_FETCH_OR_2:
    case BUILT_IN_ATOMIC_FETCH_OR_4:
    case BUILT_IN_ATOMIC_FETCH_OR_8:
    case BUILT_IN_ATOMIC_FETCH_OR_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_ATOMIC_FETCH_OR_1);
      target = expand_builtin_atomic_fetch_op (mode, exp, target, IOR, false,
					       ignore, BUILT_IN_NONE);
      if (target)
	return target;
      break;

    case BUILT_IN_ATOMIC_TEST_AND_SET:
      return expand_builtin_atomic_test_and_set (exp, target);

    case BUILT_IN_ATOMIC_CLEAR:
      return expand_builtin_atomic_clear (exp);
 
    case BUILT_IN_ATOMIC_ALWAYS_LOCK_FREE:
      return expand_builtin_atomic_always_lock_free (exp);

    case BUILT_IN_ATOMIC_IS_LOCK_FREE:
      target = expand_builtin_atomic_is_lock_free (exp);
      if (target)
        return target;
      break;

    case BUILT_IN_ATOMIC_THREAD_FENCE:
      expand_builtin_atomic_thread_fence (exp);
      return const0_rtx;

    case BUILT_IN_ATOMIC_SIGNAL_FENCE:
      expand_builtin_atomic_signal_fence (exp);
      return const0_rtx;

    case BUILT_IN_OBJECT_SIZE:
      return expand_builtin_object_size (exp);

    case BUILT_IN_MEMCPY_CHK:
    case BUILT_IN_MEMPCPY_CHK:
    case BUILT_IN_MEMMOVE_CHK:
    case BUILT_IN_MEMSET_CHK:
      target = expand_builtin_memory_chk (exp, target, mode, fcode);
      if (target)
	return target;
      break;

    case BUILT_IN_STRCPY_CHK:
    case BUILT_IN_STPCPY_CHK:
    case BUILT_IN_STRNCPY_CHK:
    case BUILT_IN_STPNCPY_CHK:
    case BUILT_IN_STRCAT_CHK:
    case BUILT_IN_STRNCAT_CHK:
    case BUILT_IN_SNPRINTF_CHK:
    case BUILT_IN_VSNPRINTF_CHK:
      maybe_emit_chk_warning (exp, fcode);
      break;

    case BUILT_IN_SPRINTF_CHK:
    case BUILT_IN_VSPRINTF_CHK:
      maybe_emit_sprintf_chk_warning (exp, fcode);
      break;

    case BUILT_IN_FREE:
      if (warn_free_nonheap_object)
	maybe_emit_free_warning (exp);
      break;

    default:	/* just do library call, if unknown builtin */
      break;
    }

  /* The switch statement above can drop through to cause the function
     to be called normally.  */
  return expand_call (exp, target, ignore);
}

/* Determine whether a tree node represents a call to a built-in
   function.  If the tree T is a call to a built-in function with
   the right number of arguments of the appropriate types, return
   the DECL_FUNCTION_CODE of the call, e.g. BUILT_IN_SQRT.
   Otherwise the return value is END_BUILTINS.  */

enum built_in_function
builtin_mathfn_code (const_tree t)
{
  const_tree fndecl, arg, parmlist;
  const_tree argtype, parmtype;
  const_call_expr_arg_iterator iter;

  if (TREE_CODE (t) != CALL_EXPR
      || TREE_CODE (CALL_EXPR_FN (t)) != ADDR_EXPR)
    return END_BUILTINS;

  fndecl = get_callee_fndecl (t);
  if (fndecl == NULL_TREE
      || TREE_CODE (fndecl) != FUNCTION_DECL
      || ! DECL_BUILT_IN (fndecl)
      || DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_MD)
    return END_BUILTINS;

  parmlist = TYPE_ARG_TYPES (TREE_TYPE (fndecl));
  init_const_call_expr_arg_iterator (t, &iter);
  for (; parmlist; parmlist = TREE_CHAIN (parmlist))
    {
      /* If a function doesn't take a variable number of arguments,
	 the last element in the list will have type `void'.  */
      parmtype = TREE_VALUE (parmlist);
      if (VOID_TYPE_P (parmtype))
	{
	  if (more_const_call_expr_args_p (&iter))
	    return END_BUILTINS;
	  return DECL_FUNCTION_CODE (fndecl);
	}

      if (! more_const_call_expr_args_p (&iter))
	return END_BUILTINS;

      arg = next_const_call_expr_arg (&iter);
      argtype = TREE_TYPE (arg);

      if (SCALAR_FLOAT_TYPE_P (parmtype))
	{
	  if (! SCALAR_FLOAT_TYPE_P (argtype))
	    return END_BUILTINS;
	}
      else if (COMPLEX_FLOAT_TYPE_P (parmtype))
	{
	  if (! COMPLEX_FLOAT_TYPE_P (argtype))
	    return END_BUILTINS;
	}
      else if (POINTER_TYPE_P (parmtype))
	{
	  if (! POINTER_TYPE_P (argtype))
	    return END_BUILTINS;
	}
      else if (INTEGRAL_TYPE_P (parmtype))
	{
	  if (! INTEGRAL_TYPE_P (argtype))
	    return END_BUILTINS;
	}
      else
	return END_BUILTINS;
    }

  /* Variable-length argument list.  */
  return DECL_FUNCTION_CODE (fndecl);
}

/* Return true if the floating point expression T has an integer value.
   We also allow +Inf, -Inf and NaN to be considered integer values.  */

static bool
integer_valued_real_p (tree t)
{
  switch (TREE_CODE (t))
    {
    case FLOAT_EXPR:
      return true;

    case ABS_EXPR:
    case SAVE_EXPR:
      return integer_valued_real_p (TREE_OPERAND (t, 0));

    case COMPOUND_EXPR:
    case MODIFY_EXPR:
    case BIND_EXPR:
      return integer_valued_real_p (TREE_OPERAND (t, 1));

    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case MIN_EXPR:
    case MAX_EXPR:
      return integer_valued_real_p (TREE_OPERAND (t, 0))
	     && integer_valued_real_p (TREE_OPERAND (t, 1));

    case COND_EXPR:
      return integer_valued_real_p (TREE_OPERAND (t, 1))
	     && integer_valued_real_p (TREE_OPERAND (t, 2));

    case REAL_CST:
      return real_isinteger (TREE_REAL_CST_PTR (t), TYPE_MODE (TREE_TYPE (t)));

    case NOP_EXPR:
      {
	tree type = TREE_TYPE (TREE_OPERAND (t, 0));
	if (TREE_CODE (type) == INTEGER_TYPE)
	  return true;
	if (TREE_CODE (type) == REAL_TYPE)
	  return integer_valued_real_p (TREE_OPERAND (t, 0));
	break;
      }

    case CALL_EXPR:
      switch (builtin_mathfn_code (t))
	{
	CASE_FLT_FN (BUILT_IN_CEIL):
	CASE_FLT_FN (BUILT_IN_FLOOR):
	CASE_FLT_FN (BUILT_IN_NEARBYINT):
	CASE_FLT_FN (BUILT_IN_RINT):
	CASE_FLT_FN (BUILT_IN_ROUND):
	CASE_FLT_FN (BUILT_IN_TRUNC):
	  return true;

	CASE_FLT_FN (BUILT_IN_FMIN):
	CASE_FLT_FN (BUILT_IN_FMAX):
	  return integer_valued_real_p (CALL_EXPR_ARG (t, 0))
 	    && integer_valued_real_p (CALL_EXPR_ARG (t, 1));

	default:
	  break;
	}
      break;

    default:
      break;
    }
  return false;
}

/* Return true if VAR is a VAR_DECL or a component thereof.  */

static bool
var_decl_component_p (tree var)
{
  tree inner = var;
  while (handled_component_p (inner))
    inner = TREE_OPERAND (inner, 0);
  return SSA_VAR_P (inner);
}

/* Fold function call to builtin memset.  Return
   NULL_TREE if no simplification can be made.  */

static tree
fold_builtin_memset (location_t loc, tree dest, tree c, tree len,
		     tree type, bool ignore)
{
  tree var, ret, etype;
  unsigned HOST_WIDE_INT length, cval;

  if (! validate_arg (dest, POINTER_TYPE)
      || ! validate_arg (c, INTEGER_TYPE)
      || ! validate_arg (len, INTEGER_TYPE))
    return NULL_TREE;

  if (! host_integerp (len, 1))
    return NULL_TREE;

  /* If the LEN parameter is zero, return DEST.  */
  if (integer_zerop (len))
    return omit_one_operand_loc (loc, type, dest, c);

  if (TREE_CODE (c) != INTEGER_CST || TREE_SIDE_EFFECTS (dest))
    return NULL_TREE;

  var = dest;
  STRIP_NOPS (var);
  if (TREE_CODE (var) != ADDR_EXPR)
    return NULL_TREE;

  var = TREE_OPERAND (var, 0);
  if (TREE_THIS_VOLATILE (var))
    return NULL_TREE;

  etype = TREE_TYPE (var);
  if (TREE_CODE (etype) == ARRAY_TYPE)
    etype = TREE_TYPE (etype);

  if (!INTEGRAL_TYPE_P (etype)
      && !POINTER_TYPE_P (etype))
    return NULL_TREE;

  if (! var_decl_component_p (var))
    return NULL_TREE;

  length = tree_low_cst (len, 1);
  if (GET_MODE_SIZE (TYPE_MODE (etype)) != length
      || get_pointer_alignment (dest) / BITS_PER_UNIT < length)
    return NULL_TREE;

  if (length > HOST_BITS_PER_WIDE_INT / BITS_PER_UNIT)
    return NULL_TREE;

  if (integer_zerop (c))
    cval = 0;
  else
    {
      if (CHAR_BIT != 8 || BITS_PER_UNIT != 8 || HOST_BITS_PER_WIDE_INT > 64)
	return NULL_TREE;

      cval = TREE_INT_CST_LOW (c);
      cval &= 0xff;
      cval |= cval << 8;
      cval |= cval << 16;
      cval |= (cval << 31) << 1;
    }

  ret = build_int_cst_type (etype, cval);
  var = build_fold_indirect_ref_loc (loc,
				 fold_convert_loc (loc,
						   build_pointer_type (etype),
						   dest));
  ret = build2 (MODIFY_EXPR, etype, var, ret);
  if (ignore)
    return ret;

  return omit_one_operand_loc (loc, type, dest, ret);
}

/* Fold function call to builtin memset.  Return
   NULL_TREE if no simplification can be made.  */

static tree
fold_builtin_bzero (location_t loc, tree dest, tree size, bool ignore)
{
  if (! validate_arg (dest, POINTER_TYPE)
      || ! validate_arg (size, INTEGER_TYPE))
    return NULL_TREE;

  if (!ignore)
    return NULL_TREE;

  /* New argument list transforming bzero(ptr x, int y) to
     memset(ptr x, int 0, size_t y).   This is done this way
     so that if it isn't expanded inline, we fallback to
     calling bzero instead of memset.  */

  return fold_builtin_memset (loc, dest, integer_zero_node,
			      fold_convert_loc (loc, size_type_node, size),
			      void_type_node, ignore);
}

/* Fold function call to builtin mem{{,p}cpy,move}.  Return
   NULL_TREE if no simplification can be made.
   If ENDP is 0, return DEST (like memcpy).
   If ENDP is 1, return DEST+LEN (like mempcpy).
   If ENDP is 2, return DEST+LEN-1 (like stpcpy).
   If ENDP is 3, return DEST, additionally *SRC and *DEST may overlap
   (memmove).   */

static tree
fold_builtin_memory_op (location_t loc, tree dest, tree src,
			tree len, tree type, bool ignore, int endp)
{
  tree destvar, srcvar, expr;

  if (! validate_arg (dest, POINTER_TYPE)
      || ! validate_arg (src, POINTER_TYPE)
      || ! validate_arg (len, INTEGER_TYPE))
    return NULL_TREE;

  /* If the LEN parameter is zero, return DEST.  */
  if (integer_zerop (len))
    return omit_one_operand_loc (loc, type, dest, src);

  /* If SRC and DEST are the same (and not volatile), return
     DEST{,+LEN,+LEN-1}.  */
  if (operand_equal_p (src, dest, 0))
    expr = len;
  else
    {
      tree srctype, desttype;
      unsigned int src_align, dest_align;
      tree off0;

      if (endp == 3)
	{
	  src_align = get_pointer_alignment (src);
	  dest_align = get_pointer_alignment (dest);

	  /* Both DEST and SRC must be pointer types.
	     ??? This is what old code did.  Is the testing for pointer types
	     really mandatory?

	     If either SRC is readonly or length is 1, we can use memcpy.  */
	  if (!dest_align || !src_align)
	    return NULL_TREE;
	  if (readonly_data_expr (src)
	      || (host_integerp (len, 1)
		  && (MIN (src_align, dest_align) / BITS_PER_UNIT
		      >= (unsigned HOST_WIDE_INT) tree_low_cst (len, 1))))
	    {
	      tree fn = builtin_decl_implicit (BUILT_IN_MEMCPY);
	      if (!fn)
		return NULL_TREE;
              return build_call_expr_loc (loc, fn, 3, dest, src, len);
	    }

	  /* If *src and *dest can't overlap, optimize into memcpy as well.  */
	  if (TREE_CODE (src) == ADDR_EXPR
	      && TREE_CODE (dest) == ADDR_EXPR)
	    {
	      tree src_base, dest_base, fn;
	      HOST_WIDE_INT src_offset = 0, dest_offset = 0;
	      HOST_WIDE_INT size = -1;
	      HOST_WIDE_INT maxsize = -1;

	      srcvar = TREE_OPERAND (src, 0);
	      src_base = get_ref_base_and_extent (srcvar, &src_offset,
						  &size, &maxsize);
	      destvar = TREE_OPERAND (dest, 0);
	      dest_base = get_ref_base_and_extent (destvar, &dest_offset,
						   &size, &maxsize);
	      if (host_integerp (len, 1))
		maxsize = tree_low_cst (len, 1);
	      else
		maxsize = -1;
	      src_offset /= BITS_PER_UNIT;
	      dest_offset /= BITS_PER_UNIT;
	      if (SSA_VAR_P (src_base)
		  && SSA_VAR_P (dest_base))
		{
		  if (operand_equal_p (src_base, dest_base, 0)
		      && ranges_overlap_p (src_offset, maxsize,
					   dest_offset, maxsize))
		    return NULL_TREE;
		}
	      else if (TREE_CODE (src_base) == MEM_REF
		       && TREE_CODE (dest_base) == MEM_REF)
		{
		  double_int off;
		  if (! operand_equal_p (TREE_OPERAND (src_base, 0),
					 TREE_OPERAND (dest_base, 0), 0))
		    return NULL_TREE;
		  off = double_int_add (mem_ref_offset (src_base),
					shwi_to_double_int (src_offset));
		  if (!double_int_fits_in_shwi_p (off))
		    return NULL_TREE;
		  src_offset = off.low;
		  off = double_int_add (mem_ref_offset (dest_base),
					shwi_to_double_int (dest_offset));
		  if (!double_int_fits_in_shwi_p (off))
		    return NULL_TREE;
		  dest_offset = off.low;
		  if (ranges_overlap_p (src_offset, maxsize,
					dest_offset, maxsize))
		    return NULL_TREE;
		}
	      else
		return NULL_TREE;

	      fn = builtin_decl_implicit (BUILT_IN_MEMCPY);
	      if (!fn)
		return NULL_TREE;
	      return build_call_expr_loc (loc, fn, 3, dest, src, len);
	    }

	  /* If the destination and source do not alias optimize into
	     memcpy as well.  */
	  if ((is_gimple_min_invariant (dest)
	       || TREE_CODE (dest) == SSA_NAME)
	      && (is_gimple_min_invariant (src)
		  || TREE_CODE (src) == SSA_NAME))
	    {
	      ao_ref destr, srcr;
	      ao_ref_init_from_ptr_and_size (&destr, dest, len);
	      ao_ref_init_from_ptr_and_size (&srcr, src, len);
	      if (!refs_may_alias_p_1 (&destr, &srcr, false))
		{
		  tree fn;
		  fn = builtin_decl_implicit (BUILT_IN_MEMCPY);
		  if (!fn)
		    return NULL_TREE;
		  return build_call_expr_loc (loc, fn, 3, dest, src, len);
		}
	    }

	  return NULL_TREE;
	}

      if (!host_integerp (len, 0))
	return NULL_TREE;
      /* FIXME:
         This logic lose for arguments like (type *)malloc (sizeof (type)),
         since we strip the casts of up to VOID return value from malloc.
	 Perhaps we ought to inherit type from non-VOID argument here?  */
      STRIP_NOPS (src);
      STRIP_NOPS (dest);
      if (!POINTER_TYPE_P (TREE_TYPE (src))
	  || !POINTER_TYPE_P (TREE_TYPE (dest)))
	return NULL_TREE;
      /* As we fold (void *)(p + CST) to (void *)p + CST undo this here.  */
      if (TREE_CODE (src) == POINTER_PLUS_EXPR)
	{
	  tree tem = TREE_OPERAND (src, 0);
	  STRIP_NOPS (tem);
	  if (tem != TREE_OPERAND (src, 0))
	    src = build1 (NOP_EXPR, TREE_TYPE (tem), src);
	}
      if (TREE_CODE (dest) == POINTER_PLUS_EXPR)
	{
	  tree tem = TREE_OPERAND (dest, 0);
	  STRIP_NOPS (tem);
	  if (tem != TREE_OPERAND (dest, 0))
	    dest = build1 (NOP_EXPR, TREE_TYPE (tem), dest);
	}
      srctype = TREE_TYPE (TREE_TYPE (src));
      if (TREE_CODE (srctype) == ARRAY_TYPE
	  && !tree_int_cst_equal (TYPE_SIZE_UNIT (srctype), len))
	{
	  srctype = TREE_TYPE (srctype);
	  STRIP_NOPS (src);
	  src = build1 (NOP_EXPR, build_pointer_type (srctype), src);
	}
      desttype = TREE_TYPE (TREE_TYPE (dest));
      if (TREE_CODE (desttype) == ARRAY_TYPE
	  && !tree_int_cst_equal (TYPE_SIZE_UNIT (desttype), len))
	{
	  desttype = TREE_TYPE (desttype);
	  STRIP_NOPS (dest);
	  dest = build1 (NOP_EXPR, build_pointer_type (desttype), dest);
	}
      if (TREE_ADDRESSABLE (srctype)
	  || TREE_ADDRESSABLE (desttype))
	return NULL_TREE;

      src_align = get_pointer_alignment (src);
      dest_align = get_pointer_alignment (dest);
      if (dest_align < TYPE_ALIGN (desttype)
	  || src_align < TYPE_ALIGN (srctype))
	return NULL_TREE;

      if (!ignore)
        dest = builtin_save_expr (dest);

      /* Build accesses at offset zero with a ref-all character type.  */
      off0 = build_int_cst (build_pointer_type_for_mode (char_type_node,
							 ptr_mode, true), 0);

      destvar = dest;
      STRIP_NOPS (destvar);
      if (TREE_CODE (destvar) == ADDR_EXPR
	  && var_decl_component_p (TREE_OPERAND (destvar, 0))
	  && tree_int_cst_equal (TYPE_SIZE_UNIT (desttype), len))
	destvar = fold_build2 (MEM_REF, desttype, destvar, off0);
      else
	destvar = NULL_TREE;

      srcvar = src;
      STRIP_NOPS (srcvar);
      if (TREE_CODE (srcvar) == ADDR_EXPR
	  && var_decl_component_p (TREE_OPERAND (srcvar, 0))
	  && tree_int_cst_equal (TYPE_SIZE_UNIT (srctype), len))
	{
	  if (!destvar
	      || src_align >= TYPE_ALIGN (desttype))
	    srcvar = fold_build2 (MEM_REF, destvar ? desttype : srctype,
				  srcvar, off0);
	  else if (!STRICT_ALIGNMENT)
	    {
	      srctype = build_aligned_type (TYPE_MAIN_VARIANT (desttype),
					    src_align);
	      srcvar = fold_build2 (MEM_REF, srctype, srcvar, off0);
	    }
	  else
	    srcvar = NULL_TREE;
	}
      else
	srcvar = NULL_TREE;

      if (srcvar == NULL_TREE && destvar == NULL_TREE)
	return NULL_TREE;

      if (srcvar == NULL_TREE)
	{
	  STRIP_NOPS (src);
	  if (src_align >= TYPE_ALIGN (desttype))
	    srcvar = fold_build2 (MEM_REF, desttype, src, off0);
	  else
	    {
	      if (STRICT_ALIGNMENT)
		return NULL_TREE;
	      srctype = build_aligned_type (TYPE_MAIN_VARIANT (desttype),
					    src_align);
	      srcvar = fold_build2 (MEM_REF, srctype, src, off0);
	    }
	}
      else if (destvar == NULL_TREE)
	{
	  STRIP_NOPS (dest);
	  if (dest_align >= TYPE_ALIGN (srctype))
	    destvar = fold_build2 (MEM_REF, srctype, dest, off0);
	  else
	    {
	      if (STRICT_ALIGNMENT)
		return NULL_TREE;
	      desttype = build_aligned_type (TYPE_MAIN_VARIANT (srctype),
					     dest_align);
	      destvar = fold_build2 (MEM_REF, desttype, dest, off0);
	    }
	}

      expr = build2 (MODIFY_EXPR, TREE_TYPE (destvar), destvar, srcvar);
    }

  if (ignore)
    return expr;

  if (endp == 0 || endp == 3)
    return omit_one_operand_loc (loc, type, dest, expr);

  if (expr == len)
    expr = NULL_TREE;

  if (endp == 2)
    len = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (len), len,
		       ssize_int (1));

  dest = fold_build_pointer_plus_loc (loc, dest, len);
  dest = fold_convert_loc (loc, type, dest);
  if (expr)
    dest = omit_one_operand_loc (loc, type, dest, expr);
  return dest;
}

/* Fold function call to builtin memchr.  ARG1, ARG2 and LEN are the
   arguments to the call, and TYPE is its return type.
   Return NULL_TREE if no simplification can be made.  */

static tree
fold_builtin_memchr (location_t loc, tree arg1, tree arg2, tree len, tree type)
{
  if (!validate_arg (arg1, POINTER_TYPE)
      || !validate_arg (arg2, INTEGER_TYPE)
      || !validate_arg (len, INTEGER_TYPE))
    return NULL_TREE;
  else
    {
      const char *p1;

      if (TREE_CODE (arg2) != INTEGER_CST
	  || !host_integerp (len, 1))
	return NULL_TREE;

      p1 = c_getstr (arg1);
      if (p1 && compare_tree_int (len, strlen (p1) + 1) <= 0)
	{
	  char c;
	  const char *r;
	  tree tem;

	  if (target_char_cast (arg2, &c))
	    return NULL_TREE;

	  r = (const char *) memchr (p1, c, tree_low_cst (len, 1));

	  if (r == NULL)
	    return build_int_cst (TREE_TYPE (arg1), 0);

	  tem = fold_build_pointer_plus_hwi_loc (loc, arg1, r - p1);
	  return fold_convert_loc (loc, type, tem);
	}
      return NULL_TREE;
    }
}

/* Fold function call to builtin memcmp with arguments ARG1 and ARG2.
   Return NULL_TREE if no simplification can be made.  */

static tree
fold_builtin_memcmp (location_t loc, tree arg1, tree arg2, tree len)
{
  const char *p1, *p2;

  if (!validate_arg (arg1, POINTER_TYPE)
      || !validate_arg (arg2, POINTER_TYPE)
      || !validate_arg (len, INTEGER_TYPE))
    return NULL_TREE;

  /* If the LEN parameter is zero, return zero.  */
  if (integer_zerop (len))
    return omit_two_operands_loc (loc, integer_type_node, integer_zero_node,
			      arg1, arg2);

  /* If ARG1 and ARG2 are the same (and not volatile), return zero.  */
  if (operand_equal_p (arg1, arg2, 0))
    return omit_one_operand_loc (loc, integer_type_node, integer_zero_node, len);

  p1 = c_getstr (arg1);
  p2 = c_getstr (arg2);

  /* If all arguments are constant, and the value of len is not greater
     than the lengths of arg1 and arg2, evaluate at compile-time.  */
  if (host_integerp (len, 1) && p1 && p2
      && compare_tree_int (len, strlen (p1) + 1) <= 0
      && compare_tree_int (len, strlen (p2) + 1) <= 0)
    {
      const int r = memcmp (p1, p2, tree_low_cst (len, 1));

      if (r > 0)
	return integer_one_node;
      else if (r < 0)
	return integer_minus_one_node;
      else
	return integer_zero_node;
    }

  /* If len parameter is one, return an expression corresponding to
     (*(const unsigned char*)arg1 - (const unsigned char*)arg2).  */
  if (host_integerp (len, 1) && tree_low_cst (len, 1) == 1)
    {
      tree cst_uchar_node = build_type_variant (unsigned_char_type_node, 1, 0);
      tree cst_uchar_ptr_node
	= build_pointer_type_for_mode (cst_uchar_node, ptr_mode, true);

      tree ind1
	= fold_convert_loc (loc, integer_type_node,
			    build1 (INDIRECT_REF, cst_uchar_node,
				    fold_convert_loc (loc,
						      cst_uchar_ptr_node,
						      arg1)));
      tree ind2
	= fold_convert_loc (loc, integer_type_node,
			    build1 (INDIRECT_REF, cst_uchar_node,
				    fold_convert_loc (loc,
						      cst_uchar_ptr_node,
						      arg2)));
      return fold_build2_loc (loc, MINUS_EXPR, integer_type_node, ind1, ind2);
    }

  return NULL_TREE;
}

/* Fold a call to an unordered comparison function such as
   __builtin_isgreater().  FNDECL is the FUNCTION_DECL for the function
   being called and ARG0 and ARG1 are the arguments for the call.
   UNORDERED_CODE and ORDERED_CODE are comparison codes that give
   the opposite of the desired result.  UNORDERED_CODE is used
   for modes that can hold NaNs and ORDERED_CODE is used for
   the rest.  */

static tree
fold_builtin_unordered_cmp (location_t loc, tree fndecl, tree arg0, tree arg1,
			    enum tree_code unordered_code,
			    enum tree_code ordered_code)
{
  tree type = TREE_TYPE (TREE_TYPE (fndecl));
  enum tree_code code;
  tree type0, type1;
  enum tree_code code0, code1;
  tree cmp_type = NULL_TREE;

  type0 = TREE_TYPE (arg0);
  type1 = TREE_TYPE (arg1);

  code0 = TREE_CODE (type0);
  code1 = TREE_CODE (type1);

  if (code0 == REAL_TYPE && code1 == REAL_TYPE)
    /* Choose the wider of two real types.  */
    cmp_type = TYPE_PRECISION (type0) >= TYPE_PRECISION (type1)
      ? type0 : type1;
  else if (code0 == REAL_TYPE && code1 == INTEGER_TYPE)
    cmp_type = type0;
  else if (code0 == INTEGER_TYPE && code1 == REAL_TYPE)
    cmp_type = type1;

  arg0 = fold_convert_loc (loc, cmp_type, arg0);
  arg1 = fold_convert_loc (loc, cmp_type, arg1);

  if (unordered_code == UNORDERED_EXPR)
    {
      if (!HONOR_NANS (TYPE_MODE (TREE_TYPE (arg0))))
	return omit_two_operands_loc (loc, type, integer_zero_node, arg0, arg1);
      return fold_build2_loc (loc, UNORDERED_EXPR, type, arg0, arg1);
    }

  code = HONOR_NANS (TYPE_MODE (TREE_TYPE (arg0))) ? unordered_code
						   : ordered_code;
  return fold_build1_loc (loc, TRUTH_NOT_EXPR, type,
		      fold_build2_loc (loc, code, type, arg0, arg1));
}

/* Fold a call to built-in function FNDECL with 2 arguments, ARG0 and ARG1.
   IGNORE is true if the result of the function call is ignored.  This
   function returns NULL_TREE if no simplification was possible.  */

static tree
fold_builtin_2 (location_t loc, tree fndecl, tree arg0, tree arg1, bool ignore)
{
  enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);

  switch (fcode)
    {
    case BUILT_IN_BZERO:
      return fold_builtin_bzero (loc, arg0, arg1, ignore);

    case BUILT_IN_ISGREATER:
      return fold_builtin_unordered_cmp (loc, fndecl,
					 arg0, arg1, UNLE_EXPR, LE_EXPR);
    case BUILT_IN_ISGREATEREQUAL:
      return fold_builtin_unordered_cmp (loc, fndecl,
					 arg0, arg1, UNLT_EXPR, LT_EXPR);
    case BUILT_IN_ISLESS:
      return fold_builtin_unordered_cmp (loc, fndecl,
					 arg0, arg1, UNGE_EXPR, GE_EXPR);
    case BUILT_IN_ISLESSEQUAL:
      return fold_builtin_unordered_cmp (loc, fndecl,
					 arg0, arg1, UNGT_EXPR, GT_EXPR);
    case BUILT_IN_ISLESSGREATER:
      return fold_builtin_unordered_cmp (loc, fndecl,
					 arg0, arg1, UNEQ_EXPR, EQ_EXPR);
    case BUILT_IN_ISUNORDERED:
      return fold_builtin_unordered_cmp (loc, fndecl,
					 arg0, arg1, UNORDERED_EXPR,
					 NOP_EXPR);

      /* We do the folding for va_start in the expander.  */
    case BUILT_IN_VA_START:
      break;

    case BUILT_IN_OBJECT_SIZE:
      return fold_builtin_object_size (arg0, arg1);

    case BUILT_IN_ATOMIC_ALWAYS_LOCK_FREE:
      return fold_builtin_atomic_always_lock_free (arg0, arg1);

    case BUILT_IN_ATOMIC_IS_LOCK_FREE:
      return fold_builtin_atomic_is_lock_free (arg0, arg1);

    default:
      break;
    }
  return NULL_TREE;
}

/* Fold a call to built-in function FNDECL with 3 arguments, ARG0, ARG1,
   and ARG2.  IGNORE is true if the result of the function call is ignored.
   This function returns NULL_TREE if no simplification was possible.  */

static tree
fold_builtin_3 (location_t loc, tree fndecl,
		tree arg0, tree arg1, tree arg2, bool ignore)
{
  tree type = TREE_TYPE (TREE_TYPE (fndecl));
  enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);
  switch (fcode)
    {
    case BUILT_IN_MEMSET:
      return fold_builtin_memset (loc, arg0, arg1, arg2, type, ignore);

    case BUILT_IN_BCOPY:
      return fold_builtin_memory_op (loc, arg1, arg0, arg2,
				     void_type_node, true, /*endp=*/3);

    case BUILT_IN_MEMCPY:
      return fold_builtin_memory_op (loc, arg0, arg1, arg2,
				     type, ignore, /*endp=*/0);

    case BUILT_IN_MEMPCPY:
      return fold_builtin_memory_op (loc, arg0, arg1, arg2,
				     type, ignore, /*endp=*/1);

    case BUILT_IN_MEMMOVE:
      return fold_builtin_memory_op (loc, arg0, arg1, arg2,
				     type, ignore, /*endp=*/3);

    case BUILT_IN_MEMCHR:
      return fold_builtin_memchr (loc, arg0, arg1, arg2, type);

    case BUILT_IN_BCMP:
    case BUILT_IN_MEMCMP:
      return fold_builtin_memcmp (loc, arg0, arg1, arg2);;

    default:
      break;
    }
  return NULL_TREE;
}

/* Fold a call to built-in function FNDECL with 4 arguments, ARG0, ARG1,
   ARG2, and ARG3.  IGNORE is true if the result of the function call is
   ignored.  This function returns NULL_TREE if no simplification was
   possible.  */

static tree
fold_builtin_4 (location_t loc, tree fndecl,
		tree arg0, tree arg1, tree arg2, tree arg3, bool ignore)
{
  enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);

  switch (fcode)
    {
    case BUILT_IN_MEMCPY_CHK:
    case BUILT_IN_MEMPCPY_CHK:
    case BUILT_IN_MEMMOVE_CHK:
    case BUILT_IN_MEMSET_CHK:
      return fold_builtin_memory_chk (loc, fndecl, arg0, arg1, arg2, arg3,
				      NULL_TREE, ignore,
				      DECL_FUNCTION_CODE (fndecl));

    default:
      break;
    }
  return NULL_TREE;
}

/* Fold a call to built-in function FNDECL.  ARGS is an array of NARGS
    arguments, where NARGS <= 4.  IGNORE is true if the result of the
    function call is ignored.  This function returns NULL_TREE if no
    simplification was possible.  Note that this only folds builtins with
    fixed argument patterns.  Foldings that do varargs-to-varargs
    transformations, or that match calls with more than 4 arguments,
    need to be handled with fold_builtin_varargs instead.  */

#define MAX_ARGS_TO_FOLD_BUILTIN 4

static tree
fold_builtin_n (location_t loc, tree fndecl, tree *args, int nargs, bool ignore)
{
  tree ret = NULL_TREE;

  switch (nargs)
    {
    case 2:
      ret = fold_builtin_2 (loc, fndecl, args[0], args[1], ignore);
      break;
    case 3:
      ret = fold_builtin_3 (loc, fndecl, args[0], args[1], args[2], ignore);
      break;
    case 4:
      ret = fold_builtin_4 (loc, fndecl, args[0], args[1], args[2], args[3],
 			    ignore);
      break;
    default:
      break;
    }
  if (ret)
    {
      ret = build1 (NOP_EXPR, TREE_TYPE (ret), ret);
      SET_EXPR_LOCATION (ret, loc);
      TREE_NO_WARNING (ret) = 1;
      return ret;
    }
  return NULL_TREE;
}

/* Return true if FNDECL shouldn't be folded right now.
   If a built-in function has an inline attribute always_inline
   wrapper, defer folding it after always_inline functions have
   been inlined, otherwise e.g. -D_FORTIFY_SOURCE checking
   might not be performed.  */

bool
avoid_folding_inline_builtin (tree fndecl)
{
  return (DECL_DECLARED_INLINE_P (fndecl)
	  && DECL_DISREGARD_INLINE_LIMITS (fndecl)
	  && cfun
	  && !cfun->always_inline_functions_inlined
	  && lookup_attribute ("always_inline", DECL_ATTRIBUTES (fndecl)));
}

/* A wrapper function for builtin folding that prevents warnings for
   "statement without effect" and the like, caused by removing the
   call node earlier than the warning is generated.  */

tree
fold_call_expr (location_t loc, tree exp, bool ignore)
{
  tree ret = NULL_TREE;
  tree fndecl = get_callee_fndecl (exp);
  if (fndecl
      && TREE_CODE (fndecl) == FUNCTION_DECL
      && DECL_BUILT_IN (fndecl)
      /* If CALL_EXPR_VA_ARG_PACK is set, the arguments aren't finalized
	 yet.  Defer folding until we see all the arguments
	 (after inlining).  */
      && !CALL_EXPR_VA_ARG_PACK (exp))
    {
      int nargs = call_expr_nargs (exp);

      /* Before gimplification CALL_EXPR_VA_ARG_PACK is not set, but
	 instead last argument is __builtin_va_arg_pack ().  Defer folding
	 even in that case, until arguments are finalized.  */
      if (nargs && TREE_CODE (CALL_EXPR_ARG (exp, nargs - 1)) == CALL_EXPR)
	{
	  tree fndecl2 = get_callee_fndecl (CALL_EXPR_ARG (exp, nargs - 1));
	  if (fndecl2
	      && TREE_CODE (fndecl2) == FUNCTION_DECL
	      && DECL_BUILT_IN_CLASS (fndecl2) == BUILT_IN_NORMAL
	      && DECL_FUNCTION_CODE (fndecl2) == BUILT_IN_VA_ARG_PACK)
	    return NULL_TREE;
	}

      if (avoid_folding_inline_builtin (fndecl))
	return NULL_TREE;

      if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_MD)
        return targetm.fold_builtin (fndecl, call_expr_nargs (exp),
				     CALL_EXPR_ARGP (exp), ignore);
      else
	{
	  if (nargs <= MAX_ARGS_TO_FOLD_BUILTIN)
	    {
	      tree *args = CALL_EXPR_ARGP (exp);
	      ret = fold_builtin_n (loc, fndecl, args, nargs, ignore);
	    }
	  if (!ret)
	    ret = fold_builtin_varargs (loc, fndecl, exp, ignore);
	  if (ret)
	    return ret;
	}
    }
  return NULL_TREE;
}

/* Conveniently construct a function call expression.  FNDECL names the
   function to be called and N arguments are passed in the array
   ARGARRAY.  */

tree
build_call_expr_loc_array (location_t loc, tree fndecl, int n, tree *argarray)
{
  tree fntype = TREE_TYPE (fndecl);
  tree fn = build1 (ADDR_EXPR, build_pointer_type (fntype), fndecl);
 
  return fold_builtin_call_array (loc, TREE_TYPE (fntype), fn, n, argarray);
}

/* Conveniently construct a function call expression.  FNDECL names the
   function to be called and the arguments are passed in the vector
   VEC.  */

tree
build_call_expr_loc_vec (location_t loc, tree fndecl, VEC(tree,gc) *vec)
{
  return build_call_expr_loc_array (loc, fndecl, VEC_length (tree, vec),
				    VEC_address (tree, vec));
}


/* Conveniently construct a function call expression.  FNDECL names the
   function to be called, N is the number of arguments, and the "..."
   parameters are the argument expressions.  */

tree
build_call_expr_loc (location_t loc, tree fndecl, int n, ...)
{
  va_list ap;
  tree *argarray = XALLOCAVEC (tree, n);
  int i;

  va_start (ap, n);
  for (i = 0; i < n; i++)
    argarray[i] = va_arg (ap, tree);
  va_end (ap);
  return build_call_expr_loc_array (loc, fndecl, n, argarray);
}

/* Like build_call_expr_loc (UNKNOWN_LOCATION, ...).  Duplicated because
   varargs macros aren't supported by all bootstrap compilers.  */

tree
build_call_expr (tree fndecl, int n, ...)
{
  va_list ap;
  tree *argarray = XALLOCAVEC (tree, n);
  int i;

  va_start (ap, n);
  for (i = 0; i < n; i++)
    argarray[i] = va_arg (ap, tree);
  va_end (ap);
  return build_call_expr_loc_array (UNKNOWN_LOCATION, fndecl, n, argarray);
}

/* Construct a CALL_EXPR with type TYPE with FN as the function expression.
   N arguments are passed in the array ARGARRAY.  */

tree
fold_builtin_call_array (location_t loc, tree type,
			 tree fn,
			 int n,
			 tree *argarray)
{
  tree ret = NULL_TREE;
   tree exp;

  if (TREE_CODE (fn) == ADDR_EXPR)
  {
    tree fndecl = TREE_OPERAND (fn, 0);
    if (TREE_CODE (fndecl) == FUNCTION_DECL
        && DECL_BUILT_IN (fndecl))
      {
	/* If last argument is __builtin_va_arg_pack (), arguments to this
	   function are not finalized yet.  Defer folding until they are.  */
	if (n && TREE_CODE (argarray[n - 1]) == CALL_EXPR)
	  {
	    tree fndecl2 = get_callee_fndecl (argarray[n - 1]);
	    if (fndecl2
		&& TREE_CODE (fndecl2) == FUNCTION_DECL
		&& DECL_BUILT_IN_CLASS (fndecl2) == BUILT_IN_NORMAL
		&& DECL_FUNCTION_CODE (fndecl2) == BUILT_IN_VA_ARG_PACK)
	      return build_call_array_loc (loc, type, fn, n, argarray);
	  }
	if (avoid_folding_inline_builtin (fndecl))
	  return build_call_array_loc (loc, type, fn, n, argarray);
        if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_MD)
          {
	    ret = targetm.fold_builtin (fndecl, n, argarray, false);
	    if (ret)
	      return ret;

	    return build_call_array_loc (loc, type, fn, n, argarray);
          }
        else if (n <= MAX_ARGS_TO_FOLD_BUILTIN)
          {
            /* First try the transformations that don't require consing up
               an exp.  */
            ret = fold_builtin_n (loc, fndecl, argarray, n, false);
            if (ret)
              return ret;
          }

        /* If we got this far, we need to build an exp.  */
        exp = build_call_array_loc (loc, type, fn, n, argarray);
        ret = fold_builtin_varargs (loc, fndecl, exp, false);
        return ret ? ret : exp;
      }
  }

  return build_call_array_loc (loc, type, fn, n, argarray);
}

/* Validate a single argument ARG against a tree code CODE representing
   a type.  */

static bool
validate_arg (const_tree arg, enum tree_code code)
{
  if (!arg)
    return false;
  else if (code == POINTER_TYPE)
    return POINTER_TYPE_P (TREE_TYPE (arg));
  else if (code == INTEGER_TYPE)
    return INTEGRAL_TYPE_P (TREE_TYPE (arg));
  return code == TREE_CODE (TREE_TYPE (arg));
}

/* This function validates the types of a function call argument list
   against a specified list of tree_codes.  If the last specifier is a 0,
   that represents an ellipses, otherwise the last specifier must be a
   VOID_TYPE.

   This is the GIMPLE version of validate_arglist.  Eventually we want to
   completely convert builtins.c to work from GIMPLEs and the tree based
   validate_arglist will then be removed.  */

bool
validate_gimple_arglist (const_gimple call, ...)
{
  enum tree_code code;
  bool res = 0;
  va_list ap;
  const_tree arg;
  size_t i;

  va_start (ap, call);
  i = 0;

  do
    {
      code = (enum tree_code) va_arg (ap, int);
      switch (code)
	{
	case 0:
	  /* This signifies an ellipses, any further arguments are all ok.  */
	  res = true;
	  goto end;
	case VOID_TYPE:
	  /* This signifies an endlink, if no arguments remain, return
	     true, otherwise return false.  */
	  res = (i == gimple_call_num_args (call));
	  goto end;
	default:
	  /* If no parameters remain or the parameter's code does not
	     match the specified code, return false.  Otherwise continue
	     checking any remaining arguments.  */
	  arg = gimple_call_arg (call, i++);
	  if (!validate_arg (arg, code))
	    goto end;
	  break;
	}
    }
  while (1);

  /* We need gotos here since we can only have one VA_CLOSE in a
     function.  */
 end: ;
  va_end (ap);

  return res;
}

/* This function validates the types of a function call argument list
   against a specified list of tree_codes.  If the last specifier is a 0,
   that represents an ellipses, otherwise the last specifier must be a
   VOID_TYPE.  */

bool
validate_arglist (const_tree callexpr, ...)
{
  enum tree_code code;
  bool res = 0;
  va_list ap;
  const_call_expr_arg_iterator iter;
  const_tree arg;

  va_start (ap, callexpr);
  init_const_call_expr_arg_iterator (callexpr, &iter);

  do
    {
      code = (enum tree_code) va_arg (ap, int);
      switch (code)
	{
	case 0:
	  /* This signifies an ellipses, any further arguments are all ok.  */
	  res = true;
	  goto end;
	case VOID_TYPE:
	  /* This signifies an endlink, if no arguments remain, return
	     true, otherwise return false.  */
	  res = !more_const_call_expr_args_p (&iter);
	  goto end;
	default:
	  /* If no parameters remain or the parameter's code does not
	     match the specified code, return false.  Otherwise continue
	     checking any remaining arguments.  */
	  arg = next_const_call_expr_arg (&iter);
	  if (!validate_arg (arg, code))
	    goto end;
	  break;
	}
    }
  while (1);

  /* We need gotos here since we can only have one VA_CLOSE in a
     function.  */
 end: ;
  va_end (ap);

  return res;
}

/* Default target-specific builtin expander that does nothing.  */

rtx
default_expand_builtin (tree exp ATTRIBUTE_UNUSED,
			rtx target ATTRIBUTE_UNUSED,
			rtx subtarget ATTRIBUTE_UNUSED,
			enum machine_mode mode ATTRIBUTE_UNUSED,
			int ignore ATTRIBUTE_UNUSED)
{
  return NULL_RTX;
}

/* Returns true is EXP represents data that would potentially reside
   in a readonly section.  */

static bool
readonly_data_expr (tree exp)
{
  STRIP_NOPS (exp);

  if (TREE_CODE (exp) != ADDR_EXPR)
    return false;

  exp = get_base_address (TREE_OPERAND (exp, 0));
  if (!exp)
    return false;

  /* Make sure we call decl_readonly_section only for trees it
     can handle (since it returns true for everything it doesn't
     understand).  */
  if (TREE_CODE (exp) == STRING_CST
      || TREE_CODE (exp) == CONSTRUCTOR
      || (TREE_CODE (exp) == VAR_DECL && TREE_STATIC (exp)))
    return decl_readonly_section (exp, 0);
  else
    return false;
}

/* Fold a call to the fputs builtin.  ARG0 and ARG1 are the arguments
   to the call.  IGNORE is true if the value returned
   by the builtin will be ignored.  UNLOCKED is true is true if this
   actually a call to fputs_unlocked.  If LEN in non-NULL, it represents
   the known length of the string.  Return NULL_TREE if no simplification
   was possible.  */

tree
fold_builtin_fputs (location_t loc, tree arg0, tree arg1,
		    bool ignore, bool unlocked, tree len)
{
  /* If we're using an unlocked function, assume the other unlocked
     functions exist explicitly.  */
  tree const fn_fputc = (unlocked
			 ? builtin_decl_explicit (BUILT_IN_FPUTC_UNLOCKED)
			 : builtin_decl_implicit (BUILT_IN_FPUTC));
  tree const fn_fwrite = (unlocked
			  ? builtin_decl_explicit (BUILT_IN_FWRITE_UNLOCKED)
			  : builtin_decl_implicit (BUILT_IN_FWRITE));

  /* If the return value is used, don't do the transformation.  */
  if (!ignore)
    return NULL_TREE;

  /* Verify the arguments in the original call.  */
  if (!validate_arg (arg0, POINTER_TYPE)
      || !validate_arg (arg1, POINTER_TYPE))
    return NULL_TREE;

  if (! len)
    len = c_strlen (arg0, 0);

  /* Get the length of the string passed to fputs.  If the length
     can't be determined, punt.  */
  if (!len
      || TREE_CODE (len) != INTEGER_CST)
    return NULL_TREE;

  switch (compare_tree_int (len, 1))
    {
    case -1: /* length is 0, delete the call entirely .  */
      return omit_one_operand_loc (loc, integer_type_node,
			       integer_zero_node, arg1);;

    case 0: /* length is 1, call fputc.  */
      {
	const char *p = c_getstr (arg0);

	if (p != NULL)
	  {
 	    if (fn_fputc)
	      return build_call_expr_loc (loc, fn_fputc, 2,
					  build_int_cst
					    (integer_type_node, p[0]), arg1);
	    else
	      return NULL_TREE;
	  }
      }
      /* FALLTHROUGH */
    case 1: /* length is greater than 1, call fwrite.  */
      {
	/* If optimizing for size keep fputs.  */
	if (optimize_function_for_size_p (cfun))
	  return NULL_TREE;
	/* New argument list transforming fputs(string, stream) to
	   fwrite(string, 1, len, stream).  */
	if (fn_fwrite)
	  return build_call_expr_loc (loc, fn_fwrite, 4, arg0,
				  size_one_node, len, arg1);
	else
	  return NULL_TREE;
      }
    default:
      gcc_unreachable ();
    }
  return NULL_TREE;
}

/* Fold the next_arg or va_start call EXP. Returns true if there was an error
   produced.  False otherwise.  This is done so that we don't output the error
   or warning twice or three times.  */

bool
fold_builtin_next_arg (tree exp, bool va_start_p)
{
  tree fntype = TREE_TYPE (current_function_decl);
  int nargs = call_expr_nargs (exp);
  tree arg;

  if (!stdarg_p (fntype))
    {
      error ("%<va_start%> used in function with fixed args");
      return true;
    }

  if (va_start_p)
    {
      if (va_start_p && (nargs != 2))
	{
	  error ("wrong number of arguments to function %<va_start%>");
	  return true;
	}
      arg = CALL_EXPR_ARG (exp, 1);
    }
  /* We use __builtin_va_start (ap, 0, 0) or __builtin_next_arg (0, 0)
     when we checked the arguments and if needed issued a warning.  */
  else
    {
      if (nargs == 0)
	{
	  /* Evidently an out of date version of <stdarg.h>; can't validate
	     va_start's second argument, but can still work as intended.  */
	  warning (0, "%<__builtin_next_arg%> called without an argument");
	  return true;
	}
      else if (nargs > 1)
	{
	  error ("wrong number of arguments to function %<__builtin_next_arg%>");
	  return true;
	}
      arg = CALL_EXPR_ARG (exp, 0);
    }

  if (TREE_CODE (arg) == SSA_NAME)
    arg = SSA_NAME_VAR (arg);

  /* We destructively modify the call to be __builtin_va_start (ap, 0)
     or __builtin_next_arg (0) the first time we see it, after checking
     the arguments and if needed issuing a warning.  */
  if (!integer_zerop (arg))
    {
      tree last_parm = tree_last (DECL_ARGUMENTS (current_function_decl));

      /* Strip off all nops for the sake of the comparison.  This
	 is not quite the same as STRIP_NOPS.  It does more.
	 We must also strip off INDIRECT_EXPR for C++ reference
	 parameters.  */
      while (CONVERT_EXPR_P (arg)
	     || TREE_CODE (arg) == INDIRECT_REF)
	arg = TREE_OPERAND (arg, 0);
      if (arg != last_parm)
	{
	  /* FIXME: Sometimes with the tree optimizers we can get the
	     not the last argument even though the user used the last
	     argument.  We just warn and set the arg to be the last
	     argument so that we will get wrong-code because of
	     it.  */
	  warning (0, "second parameter of %<va_start%> not last named argument");
	}

      /* Undefined by C99 7.15.1.4p4 (va_start):
         "If the parameter parmN is declared with the register storage
         class, with a function or array type, or with a type that is
         not compatible with the type that results after application of
         the default argument promotions, the behavior is undefined."
      */
      else if (DECL_REGISTER (arg))
        warning (0, "undefined behaviour when second parameter of "
                 "%<va_start%> is declared with %<register%> storage");

      /* We want to verify the second parameter just once before the tree
	 optimizers are run and then avoid keeping it in the tree,
	 as otherwise we could warn even for correct code like:
	 void foo (int i, ...)
	 { va_list ap; i++; va_start (ap, i); va_end (ap); }  */
      if (va_start_p)
	CALL_EXPR_ARG (exp, 1) = integer_zero_node;
      else
	CALL_EXPR_ARG (exp, 0) = integer_zero_node;
    }
  return false;
}

/* Expand a call EXP to __builtin_object_size.  */

rtx
expand_builtin_object_size (tree exp)
{
  tree ost;
  int object_size_type;
  tree fndecl = get_callee_fndecl (exp);

  if (!validate_arglist (exp, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE))
    {
      error ("%Kfirst argument of %D must be a pointer, second integer constant",
	     exp, fndecl);
      expand_builtin_trap ();
      return const0_rtx;
    }

  ost = CALL_EXPR_ARG (exp, 1);
  STRIP_NOPS (ost);

  if (TREE_CODE (ost) != INTEGER_CST
      || tree_int_cst_sgn (ost) < 0
      || compare_tree_int (ost, 3) > 0)
    {
      error ("%Klast argument of %D is not integer constant between 0 and 3",
	     exp, fndecl);
      expand_builtin_trap ();
      return const0_rtx;
    }

  object_size_type = tree_low_cst (ost, 0);

  return object_size_type < 2 ? constm1_rtx : const0_rtx;
}

/* Expand EXP, a call to the __mem{cpy,pcpy,move,set}_chk builtin.
   FCODE is the BUILT_IN_* to use.
   Return NULL_RTX if we failed; the caller should emit a normal call,
   otherwise try to get the result in TARGET, if convenient (and in
   mode MODE if that's convenient).  */

static rtx
expand_builtin_memory_chk (tree exp, rtx target, enum machine_mode mode,
			   enum built_in_function fcode)
{
  tree dest, src, len, size;

  if (!validate_arglist (exp,
			 POINTER_TYPE,
			 fcode == BUILT_IN_MEMSET_CHK
			 ? INTEGER_TYPE : POINTER_TYPE,
			 INTEGER_TYPE, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;

  dest = CALL_EXPR_ARG (exp, 0);
  src = CALL_EXPR_ARG (exp, 1);
  len = CALL_EXPR_ARG (exp, 2);
  size = CALL_EXPR_ARG (exp, 3);

  if (! host_integerp (size, 1))
    return NULL_RTX;

  if (host_integerp (len, 1) || integer_all_onesp (size))
    {
      tree fn;

      if (! integer_all_onesp (size) && tree_int_cst_lt (size, len))
	{
	  warning_at (tree_nonartificial_location (exp),
		      0, "%Kcall to %D will always overflow destination buffer",
		      exp, get_callee_fndecl (exp));
	  return NULL_RTX;
	}

      fn = NULL_TREE;
      /* If __builtin_mem{cpy,pcpy,move,set}_chk is used, assume
	 mem{cpy,pcpy,move,set} is available.  */
      switch (fcode)
	{
	case BUILT_IN_MEMCPY_CHK:
	  fn = builtin_decl_explicit (BUILT_IN_MEMCPY);
	  break;
	case BUILT_IN_MEMPCPY_CHK:
	  fn = builtin_decl_explicit (BUILT_IN_MEMPCPY);
	  break;
	case BUILT_IN_MEMMOVE_CHK:
	  fn = builtin_decl_explicit (BUILT_IN_MEMMOVE);
	  break;
	case BUILT_IN_MEMSET_CHK:
	  fn = builtin_decl_explicit (BUILT_IN_MEMSET);
	  break;
	default:
	  break;
	}

      if (! fn)
	return NULL_RTX;

      fn = build_call_nofold_loc (EXPR_LOCATION (exp), fn, 3, dest, src, len);
      gcc_assert (TREE_CODE (fn) == CALL_EXPR);
      CALL_EXPR_TAILCALL (fn) = CALL_EXPR_TAILCALL (exp);
      return expand_expr (fn, target, mode, EXPAND_NORMAL);
    }
  else if (fcode == BUILT_IN_MEMSET_CHK)
    return NULL_RTX;
  else
    {
      unsigned int dest_align = get_pointer_alignment (dest);

      /* If DEST is not a pointer type, call the normal function.  */
      if (dest_align == 0)
	return NULL_RTX;

      /* If SRC and DEST are the same (and not volatile), do nothing.  */
      if (operand_equal_p (src, dest, 0))
	{
	  tree expr;

	  if (fcode != BUILT_IN_MEMPCPY_CHK)
	    {
	      /* Evaluate and ignore LEN in case it has side-effects.  */
	      expand_expr (len, const0_rtx, VOIDmode, EXPAND_NORMAL);
	      return expand_expr (dest, target, mode, EXPAND_NORMAL);
	    }

	  expr = fold_build_pointer_plus (dest, len);
	  return expand_expr (expr, target, mode, EXPAND_NORMAL);
	}

      /* __memmove_chk special case.  */
      if (fcode == BUILT_IN_MEMMOVE_CHK)
	{
	  unsigned int src_align = get_pointer_alignment (src);

	  if (src_align == 0)
	    return NULL_RTX;

	  /* If src is categorized for a readonly section we can use
	     normal __memcpy_chk.  */
	  if (readonly_data_expr (src))
	    {
	      tree fn = builtin_decl_explicit (BUILT_IN_MEMCPY_CHK);
	      if (!fn)
		return NULL_RTX;
	      fn = build_call_nofold_loc (EXPR_LOCATION (exp), fn, 4,
					  dest, src, len, size);
	      gcc_assert (TREE_CODE (fn) == CALL_EXPR);
	      CALL_EXPR_TAILCALL (fn) = CALL_EXPR_TAILCALL (exp);
	      return expand_expr (fn, target, mode, EXPAND_NORMAL);
	    }
	}
      return NULL_RTX;
    }
}

/* Emit warning if a buffer overflow is detected at compile time.  */

static void
maybe_emit_chk_warning (tree exp, enum built_in_function fcode)
{
  int is_strlen = 0;
  tree len, size;
  location_t loc = tree_nonartificial_location (exp);

  switch (fcode)
    {
    case BUILT_IN_STRCPY_CHK:
    case BUILT_IN_STPCPY_CHK:
    /* For __strcat_chk the warning will be emitted only if overflowing
       by at least strlen (dest) + 1 bytes.  */
    case BUILT_IN_STRCAT_CHK:
      len = CALL_EXPR_ARG (exp, 1);
      size = CALL_EXPR_ARG (exp, 2);
      is_strlen = 1;
      break;
    case BUILT_IN_STRNCAT_CHK:
    case BUILT_IN_STRNCPY_CHK:
    case BUILT_IN_STPNCPY_CHK:
      len = CALL_EXPR_ARG (exp, 2);
      size = CALL_EXPR_ARG (exp, 3);
      break;
    case BUILT_IN_SNPRINTF_CHK:
    case BUILT_IN_VSNPRINTF_CHK:
      len = CALL_EXPR_ARG (exp, 1);
      size = CALL_EXPR_ARG (exp, 3);
      break;
    default:
      gcc_unreachable ();
    }

  if (!len || !size)
    return;

  if (! host_integerp (size, 1) || integer_all_onesp (size))
    return;

  if (is_strlen)
    {
      len = c_strlen (len, 1);
      if (! len || ! host_integerp (len, 1) || tree_int_cst_lt (len, size))
	return;
    }
  else if (fcode == BUILT_IN_STRNCAT_CHK)
    {
      tree src = CALL_EXPR_ARG (exp, 1);
      if (! src || ! host_integerp (len, 1) || tree_int_cst_lt (len, size))
	return;
      src = c_strlen (src, 1);
      if (! src || ! host_integerp (src, 1))
	{
	  warning_at (loc, 0, "%Kcall to %D might overflow destination buffer",
		      exp, get_callee_fndecl (exp));
	  return;
	}
      else if (tree_int_cst_lt (src, size))
	return;
    }
  else if (! host_integerp (len, 1) || ! tree_int_cst_lt (size, len))
    return;

  warning_at (loc, 0, "%Kcall to %D will always overflow destination buffer",
	      exp, get_callee_fndecl (exp));
}

/* Emit warning if a buffer overflow is detected at compile time
   in __sprintf_chk/__vsprintf_chk calls.  */

static void
maybe_emit_sprintf_chk_warning (tree exp, enum built_in_function fcode)
{
  tree size, len, fmt;
  const char *fmt_str;
  int nargs = call_expr_nargs (exp);

  /* Verify the required arguments in the original call.  */

  if (nargs < 4)
    return;
  size = CALL_EXPR_ARG (exp, 2);
  fmt = CALL_EXPR_ARG (exp, 3);

  if (! host_integerp (size, 1) || integer_all_onesp (size))
    return;

  /* Check whether the format is a literal string constant.  */
  fmt_str = c_getstr (fmt);
  if (fmt_str == NULL)
    return;

  if (!init_target_chars ())
    return;

  /* If the format doesn't contain % args or %%, we know its size.  */
  if (strchr (fmt_str, target_percent) == 0)
    len = build_int_cstu (size_type_node, strlen (fmt_str));
  /* If the format is "%s" and first ... argument is a string literal,
     we know it too.  */
  else if (fcode == BUILT_IN_SPRINTF_CHK
	   && strcmp (fmt_str, target_percent_s) == 0)
    {
      tree arg;

      if (nargs < 5)
	return;
      arg = CALL_EXPR_ARG (exp, 4);
      if (! POINTER_TYPE_P (TREE_TYPE (arg)))
	return;

      len = c_strlen (arg, 1);
      if (!len || ! host_integerp (len, 1))
	return;
    }
  else
    return;

  if (! tree_int_cst_lt (len, size))
    warning_at (tree_nonartificial_location (exp),
		0, "%Kcall to %D will always overflow destination buffer",
		exp, get_callee_fndecl (exp));
}

/* Emit warning if a free is called with address of a variable.  */

static void
maybe_emit_free_warning (tree exp)
{
  tree arg = CALL_EXPR_ARG (exp, 0);

  STRIP_NOPS (arg);
  if (TREE_CODE (arg) != ADDR_EXPR)
    return;

  arg = get_base_address (TREE_OPERAND (arg, 0));
  if (arg == NULL || INDIRECT_REF_P (arg) || TREE_CODE (arg) == MEM_REF)
    return;

  if (SSA_VAR_P (arg))
    warning_at (tree_nonartificial_location (exp), OPT_Wfree_nonheap_object,
		"%Kattempt to free a non-heap object %qD", exp, arg);
  else
    warning_at (tree_nonartificial_location (exp), OPT_Wfree_nonheap_object,
		"%Kattempt to free a non-heap object", exp);
}

/* Fold a call to __builtin_object_size with arguments PTR and OST,
   if possible.  */

tree
fold_builtin_object_size (tree ptr, tree ost)
{
  unsigned HOST_WIDE_INT bytes;
  int object_size_type;

  if (!validate_arg (ptr, POINTER_TYPE)
      || !validate_arg (ost, INTEGER_TYPE))
    return NULL_TREE;

  STRIP_NOPS (ost);

  if (TREE_CODE (ost) != INTEGER_CST
      || tree_int_cst_sgn (ost) < 0
      || compare_tree_int (ost, 3) > 0)
    return NULL_TREE;

  object_size_type = tree_low_cst (ost, 0);

  /* __builtin_object_size doesn't evaluate side-effects in its arguments;
     if there are any side-effects, it returns (size_t) -1 for types 0 and 1
     and (size_t) 0 for types 2 and 3.  */
  if (TREE_SIDE_EFFECTS (ptr))
    return build_int_cst_type (size_type_node, object_size_type < 2 ? -1 : 0);

  if (TREE_CODE (ptr) == ADDR_EXPR)
    {
      bytes = compute_builtin_object_size (ptr, object_size_type);
      if (double_int_fits_to_tree_p (size_type_node,
				     uhwi_to_double_int (bytes)))
	return build_int_cstu (size_type_node, bytes);
    }
  else if (TREE_CODE (ptr) == SSA_NAME)
    {
      /* If object size is not known yet, delay folding until
       later.  Maybe subsequent passes will help determining
       it.  */
      bytes = compute_builtin_object_size (ptr, object_size_type);
      if (bytes != (unsigned HOST_WIDE_INT) (object_size_type < 2 ? -1 : 0)
          && double_int_fits_to_tree_p (size_type_node,
					uhwi_to_double_int (bytes)))
	return build_int_cstu (size_type_node, bytes);
    }

  return NULL_TREE;
}

/* Fold a call to the __mem{cpy,pcpy,move,set}_chk builtin.
   DEST, SRC, LEN, and SIZE are the arguments to the call.
   IGNORE is true, if return value can be ignored.  FCODE is the BUILT_IN_*
   code of the builtin.  If MAXLEN is not NULL, it is maximum length
   passed as third argument.  */

tree
fold_builtin_memory_chk (location_t loc, tree fndecl,
			 tree dest, tree src, tree len, tree size,
			 tree maxlen, bool ignore,
			 enum built_in_function fcode)
{
  tree fn;

  if (!validate_arg (dest, POINTER_TYPE)
      || !validate_arg (src,
			(fcode == BUILT_IN_MEMSET_CHK
			 ? INTEGER_TYPE : POINTER_TYPE))
      || !validate_arg (len, INTEGER_TYPE)
      || !validate_arg (size, INTEGER_TYPE))
    return NULL_TREE;

  /* If SRC and DEST are the same (and not volatile), return DEST
     (resp. DEST+LEN for __mempcpy_chk).  */
  if (fcode != BUILT_IN_MEMSET_CHK && operand_equal_p (src, dest, 0))
    {
      if (fcode != BUILT_IN_MEMPCPY_CHK)
	return omit_one_operand_loc (loc, TREE_TYPE (TREE_TYPE (fndecl)),
				 dest, len);
      else
	{
	  tree temp = fold_build_pointer_plus_loc (loc, dest, len);
	  return fold_convert_loc (loc, TREE_TYPE (TREE_TYPE (fndecl)), temp);
	}
    }

  if (! host_integerp (size, 1))
    return NULL_TREE;

  if (! integer_all_onesp (size))
    {
      if (! host_integerp (len, 1))
	{
	  /* If LEN is not constant, try MAXLEN too.
	     For MAXLEN only allow optimizing into non-_ocs function
	     if SIZE is >= MAXLEN, never convert to __ocs_fail ().  */
	  if (maxlen == NULL_TREE || ! host_integerp (maxlen, 1))
	    {
	      if (fcode == BUILT_IN_MEMPCPY_CHK && ignore)
		{
		  /* (void) __mempcpy_chk () can be optimized into
		     (void) __memcpy_chk ().  */
		  fn = builtin_decl_explicit (BUILT_IN_MEMCPY_CHK);
		  if (!fn)
		    return NULL_TREE;

		  return build_call_expr_loc (loc, fn, 4, dest, src, len, size);
		}
	      return NULL_TREE;
	    }
	}
      else
	maxlen = len;

      if (tree_int_cst_lt (size, maxlen))
	return NULL_TREE;
    }

  fn = NULL_TREE;
  /* If __builtin_mem{cpy,pcpy,move,set}_chk is used, assume
     mem{cpy,pcpy,move,set} is available.  */
  switch (fcode)
    {
    case BUILT_IN_MEMCPY_CHK:
      fn = builtin_decl_explicit (BUILT_IN_MEMCPY);
      break;
    case BUILT_IN_MEMPCPY_CHK:
      fn = builtin_decl_explicit (BUILT_IN_MEMPCPY);
      break;
    case BUILT_IN_MEMMOVE_CHK:
      fn = builtin_decl_explicit (BUILT_IN_MEMMOVE);
      break;
    case BUILT_IN_MEMSET_CHK:
      fn = builtin_decl_explicit (BUILT_IN_MEMSET);
      break;
    default:
      break;
    }

  if (!fn)
    return NULL_TREE;

  return build_call_expr_loc (loc, fn, 3, dest, src, len);
}

/* Initialize format string characters in the target charset.  */

static bool
init_target_chars (void)
{
  static bool init;
  if (!init)
    {
      target_newline = lang_hooks.to_target_charset ('\n');
      target_percent = lang_hooks.to_target_charset ('%');
      target_c = lang_hooks.to_target_charset ('c');
      target_s = lang_hooks.to_target_charset ('s');
      if (target_newline == 0 || target_percent == 0 || target_c == 0
	  || target_s == 0)
	return false;

      target_percent_c[0] = target_percent;
      target_percent_c[1] = target_c;
      target_percent_c[2] = '\0';

      target_percent_s[0] = target_percent;
      target_percent_s[1] = target_s;
      target_percent_s[2] = '\0';

      target_percent_s_newline[0] = target_percent;
      target_percent_s_newline[1] = target_s;
      target_percent_s_newline[2] = target_newline;
      target_percent_s_newline[3] = '\0';

      init = true;
    }
  return true;
}

/* A wrapper function for builtin folding that prevents warnings for
   "statement without effect" and the like, caused by removing the
   call node earlier than the warning is generated.  */

tree
fold_call_stmt (gimple stmt, bool ignore)
{
  tree ret = NULL_TREE;
  tree fndecl = gimple_call_fndecl (stmt);
  location_t loc = gimple_location (stmt);
  if (fndecl
      && TREE_CODE (fndecl) == FUNCTION_DECL
      && DECL_BUILT_IN (fndecl)
      && !gimple_call_va_arg_pack_p (stmt))
    {
      int nargs = gimple_call_num_args (stmt);
      tree *args = (nargs > 0
		    ? gimple_call_arg_ptr (stmt, 0)
		    : &error_mark_node);

      if (avoid_folding_inline_builtin (fndecl))
	return NULL_TREE;
      if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_MD)
        {
	  return targetm.fold_builtin (fndecl, nargs, args, ignore);
        }
      else
	{
	  if (nargs <= MAX_ARGS_TO_FOLD_BUILTIN)
	    ret = fold_builtin_n (loc, fndecl, args, nargs, ignore);
	  if (ret)
	    {
	      /* Propagate location information from original call to
		 expansion of builtin.  Otherwise things like
		 maybe_emit_chk_warning, that operate on the expansion
		 of a builtin, will use the wrong location information.  */
	      if (gimple_has_location (stmt))
                {
		  tree realret = ret;
		  if (TREE_CODE (ret) == NOP_EXPR)
		    realret = TREE_OPERAND (ret, 0);
		  if (CAN_HAVE_LOCATION_P (realret)
		      && !EXPR_HAS_LOCATION (realret))
		    SET_EXPR_LOCATION (realret, loc);
                  return realret;
                }
	      return ret;
	    }
	}
    }
  return NULL_TREE;
}

/* Look up the function in builtin_decl that corresponds to DECL
   and set ASMSPEC as its user assembler name.  DECL must be a
   function decl that declares a builtin.  */

void
set_builtin_user_assembler_name (tree decl, const char *asmspec)
{
  tree builtin;
  gcc_assert (TREE_CODE (decl) == FUNCTION_DECL
	      && DECL_BUILT_IN_CLASS (decl) == BUILT_IN_NORMAL
	      && asmspec != 0);

  builtin = builtin_decl_explicit (DECL_FUNCTION_CODE (decl));
  set_user_assembler_name (builtin, asmspec);
  switch (DECL_FUNCTION_CODE (decl))
    {
    case BUILT_IN_MEMCPY:
      init_block_move_fn (asmspec);
      memcpy_libfunc = set_user_assembler_libfunc ("memcpy", asmspec);
      break;
    case BUILT_IN_MEMSET:
      init_block_clear_fn (asmspec);
      memset_libfunc = set_user_assembler_libfunc ("memset", asmspec);
      break;
    case BUILT_IN_MEMMOVE:
      memmove_libfunc = set_user_assembler_libfunc ("memmove", asmspec);
      break;
    case BUILT_IN_MEMCMP:
      memcmp_libfunc = set_user_assembler_libfunc ("memcmp", asmspec);
      break;
    case BUILT_IN_ABORT:
      abort_libfunc = set_user_assembler_libfunc ("abort", asmspec);
      break;
    case BUILT_IN_FFS:
      if (INT_TYPE_SIZE < BITS_PER_WORD)
	{
	  set_user_assembler_libfunc ("ffs", asmspec);
	  set_optab_libfunc (ffs_optab, mode_for_size (INT_TYPE_SIZE,
						       MODE_INT, 0), "ffs");
	}
      break;
    default:
      break;
    }
}

/* Return true if DECL is a builtin that expands to a constant or similarly
   simple code.  */
bool
is_simple_builtin (tree decl)
{
  if (decl && DECL_BUILT_IN_CLASS (decl) == BUILT_IN_NORMAL)
    switch (DECL_FUNCTION_CODE (decl))
      {
	/* Builtins that expand to constants.  */
      case BUILT_IN_CONSTANT_P:
      case BUILT_IN_EXPECT:
      case BUILT_IN_OBJECT_SIZE:
      case BUILT_IN_UNREACHABLE:
	/* Simple register moves or loads from stack.  */
      case BUILT_IN_ASSUME_ALIGNED:
      case BUILT_IN_RETURN_ADDRESS:
      case BUILT_IN_EXTRACT_RETURN_ADDR:
      case BUILT_IN_FROB_RETURN_ADDR:
      case BUILT_IN_RETURN:
      case BUILT_IN_AGGREGATE_INCOMING_ADDRESS:
      case BUILT_IN_FRAME_ADDRESS:
      case BUILT_IN_VA_END:
      case BUILT_IN_STACK_SAVE:
      case BUILT_IN_STACK_RESTORE:
	/* Exception state returns or moves registers around.  */
      case BUILT_IN_EH_FILTER:
      case BUILT_IN_EH_POINTER:
      case BUILT_IN_EH_COPY_VALUES:
	return true;

      default:
	return false;
      }

  return false;
}

/* Return true if DECL is a builtin that is not expensive, i.e., they are
   most probably expanded inline into reasonably simple code.  This is a
   superset of is_simple_builtin.  */
bool
is_inexpensive_builtin (tree decl)
{
  if (!decl)
    return false;
  else if (DECL_BUILT_IN_CLASS (decl) == BUILT_IN_MD)
    return true;
  else if (DECL_BUILT_IN_CLASS (decl) == BUILT_IN_NORMAL)
    switch (DECL_FUNCTION_CODE (decl))
      {
      case BUILT_IN_ABS:
      case BUILT_IN_ALLOCA:
      case BUILT_IN_ALLOCA_WITH_ALIGN:
      case BUILT_IN_BSWAP32:
      case BUILT_IN_BSWAP64:
      case BUILT_IN_CLZ:
      case BUILT_IN_CLZIMAX:
      case BUILT_IN_CLZL:
      case BUILT_IN_CLZLL:
      case BUILT_IN_CTZ:
      case BUILT_IN_CTZIMAX:
      case BUILT_IN_CTZL:
      case BUILT_IN_CTZLL:
      case BUILT_IN_FFS:
      case BUILT_IN_FFSIMAX:
      case BUILT_IN_FFSL:
      case BUILT_IN_FFSLL:
      case BUILT_IN_IMAXABS:
      case BUILT_IN_FINITE:
      case BUILT_IN_FINITEF:
      case BUILT_IN_FINITEL:
      case BUILT_IN_FINITED32:
      case BUILT_IN_FINITED64:
      case BUILT_IN_FINITED128:
      case BUILT_IN_FPCLASSIFY:
      case BUILT_IN_ISFINITE:
      case BUILT_IN_ISINF_SIGN:
      case BUILT_IN_ISINF:
      case BUILT_IN_ISINFF:
      case BUILT_IN_ISINFL:
      case BUILT_IN_ISINFD32:
      case BUILT_IN_ISINFD64:
      case BUILT_IN_ISINFD128:
      case BUILT_IN_ISNAN:
      case BUILT_IN_ISNANF:
      case BUILT_IN_ISNANL:
      case BUILT_IN_ISNAND32:
      case BUILT_IN_ISNAND64:
      case BUILT_IN_ISNAND128:
      case BUILT_IN_ISNORMAL:
      case BUILT_IN_ISGREATER:
      case BUILT_IN_ISGREATEREQUAL:
      case BUILT_IN_ISLESS:
      case BUILT_IN_ISLESSEQUAL:
      case BUILT_IN_ISLESSGREATER:
      case BUILT_IN_ISUNORDERED:
      case BUILT_IN_VA_ARG_PACK:
      case BUILT_IN_VA_ARG_PACK_LEN:
      case BUILT_IN_VA_COPY:
      case BUILT_IN_TRAP:
      case BUILT_IN_SAVEREGS:
      case BUILT_IN_POPCOUNTL:
      case BUILT_IN_POPCOUNTLL:
      case BUILT_IN_POPCOUNTIMAX:
      case BUILT_IN_POPCOUNT:
      case BUILT_IN_PARITYL:
      case BUILT_IN_PARITYLL:
      case BUILT_IN_PARITYIMAX:
      case BUILT_IN_PARITY:
      case BUILT_IN_LABS:
      case BUILT_IN_LLABS:
      case BUILT_IN_PREFETCH:
	return true;

      default:
	return is_simple_builtin (decl);
      }

  return false;
}
