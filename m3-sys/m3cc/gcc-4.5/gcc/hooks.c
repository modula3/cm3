/* General-purpose hooks.
   Copyright (C) 2002, 2003, 2004, 2005, 2007, 2008, 2009
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

/* This file contains generic hooks that can be used as defaults for
   target or language-dependent hook initializers.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "hooks.h"

/* Generic hook that does absolutely zappo.  */
void
hook_void_void (void)
{
}

/* Generic hook that takes no arguments and returns false.  */
bool
hook_bool_void_false (void)
{
  return false;
}

/* Generic hook that takes no arguments and returns true.  */
bool
hook_bool_void_true (void)
{
  return true;
}

/* Generic hook that takes (bool) and returns false.  */
bool
hook_bool_bool_false (bool ARG_UNUSED(a))
{
  return false;
}

/* Generic hook that takes const int, const int) and returns true.  */
bool hook_bool_const_int_const_int_true (const int ARG_UNUSED(a),
                                         const int ARG_UNUSED(b))
{
  return true;
}

/* Generic hook that takes (enum machine_mode) and returns false.  */
bool
hook_bool_mode_false (enum machine_mode ARG_UNUSED(mode))
{
  return false;
}

/* Generic hook that takes (enum machine_mode, rtx) and returns false.  */
bool
hook_bool_mode_const_rtx_false (enum machine_mode ARG_UNUSED(mode),
				const_rtx ARG_UNUSED(value))
{
  return false;
}

/* Generic hook that takes (enum machine_mode, rtx) and returns true.  */
bool
hook_bool_mode_const_rtx_true (enum machine_mode ARG_UNUSED(mode),
			       const_rtx ARG_UNUSED(value))
{
  return true;
}

/* Generic hook that takes (FILE *, const char *) and does nothing.  */
void
hook_void_FILEptr_constcharptr (FILE *ARG_UNUSED(a), const char *b ATTRIBUTE_UNUSED)
{
}

/* Used for the TARGET_ASM_CAN_OUTPUT_MI_THUNK hook.  */
bool
hook_bool_const_tree_hwi_hwi_const_tree_false (const_tree ARG_UNUSED(a),
					       HOST_WIDE_INT ARG_UNUSED(b),
					       HOST_WIDE_INT ARG_UNUSED(c),
					       const_tree ARG_UNUSED(d))
{
  return false;
}

bool
hook_bool_const_tree_hwi_hwi_const_tree_true (const_tree ARG_UNUSED(a),
					      HOST_WIDE_INT ARG_UNUSED(b),
					      HOST_WIDE_INT ARG_UNUSED(c),
					      const_tree ARG_UNUSED(d))
{
  return true;
}

bool
hook_bool_constcharptr_size_t_false (const char *ARG_UNUSED(a),
				     size_t ARG_UNUSED(b))
{
  return false;
}

bool
hook_bool_size_t_constcharptr_int_true (size_t ARG_UNUSED(a),
					const char *ARG_UNUSED(b),
					int ARG_UNUSED(c))
{
  return true;
}

bool
default_can_output_mi_thunk_no_vcall (const_tree ARG_UNUSED(a),
				      HOST_WIDE_INT ARG_UNUSED(b),
				      HOST_WIDE_INT c,
				      const_tree ARG_UNUSED(d))
{
  return c == 0;
}

int
hook_int_const_tree_0 (const_tree ARG_UNUSED(a))
{
  return 0;
}

/* ??? Used for comp_type_attributes, which ought to return bool.  */
int
hook_int_const_tree_const_tree_1 (const_tree ARG_UNUSED(a), const_tree b ATTRIBUTE_UNUSED)
{
  return 1;
}

int
hook_int_rtx_0 (rtx ARG_UNUSED(a))
{
  return 0;
}

int
hook_int_rtx_bool_0 (rtx ARG_UNUSED(a), bool b ATTRIBUTE_UNUSED)
{
  return 0;
}

int
hook_int_size_t_constcharptr_int_0 (size_t ARG_UNUSED(a),
				    const char *ARG_UNUSED(b),
				    int ARG_UNUSED(c))
{
  return 0;
}

unsigned int
hook_uint_uint_constcharptrptr_0 (unsigned int ARG_UNUSED(a),
				  const char **ARG_UNUSED(b))
{
  return 0;
}

void
hook_void_tree (tree ARG_UNUSED(a))
{
}

void
hook_void_constcharptr (const char *ARG_UNUSED(a))
{
}

void
hook_void_tree_treeptr (tree ARG_UNUSED(a), tree *b ATTRIBUTE_UNUSED)
{
}

bool
hook_bool_tree_false (tree ARG_UNUSED(a))
{
  return false;
}

bool
hook_bool_const_tree_false (const_tree ARG_UNUSED(a))
{
  return false;
}

bool
hook_bool_tree_true (tree ARG_UNUSED(a))
{
  return true;
}

bool
hook_bool_const_tree_true (const_tree ARG_UNUSED(a))
{
  return true;
}

bool
hook_bool_tree_tree_false (tree ARG_UNUSED(a), tree b ATTRIBUTE_UNUSED)
{
  return false;
}

bool
hook_bool_tree_tree_true (tree ARG_UNUSED(a), tree b ATTRIBUTE_UNUSED)
{
  return true;
}

bool
hook_bool_tree_bool_false (tree ARG_UNUSED(a), bool b ATTRIBUTE_UNUSED)
{
  return false;
}

bool
hook_bool_rtx_false (rtx ARG_UNUSED(a))
{
  return false;
}

bool
hook_bool_uintp_uintp_false (unsigned int *ARG_UNUSED(a),
			     unsigned int *ARG_UNUSED(b))
{
  return false;
}

bool
hook_bool_rtx_int_int_intp_bool_false (rtx ARG_UNUSED(a),
				       int ARG_UNUSED(b),
				       int ARG_UNUSED(c),
				       int *ARG_UNUSED(d),
				       bool ARG_UNUSED(speed_p))
{
  return false;
}

/* Generic hook that takes an rtx and returns it.  */
rtx
hook_rtx_rtx_identity (rtx x)
{
  return x;
}

/* Generic hook that takes an rtx and returns NULL_RTX.  */
rtx
hook_rtx_rtx_null (rtx ARG_UNUSED(x))
{
  return NULL;
}

/* Generic hook that takes a tree and an int and returns NULL_RTX.  */
rtx
hook_rtx_tree_int_null (tree ARG_UNUSED(a), int b ATTRIBUTE_UNUSED)
{
  return NULL;
}

/* Generic hook that takes three trees and returns the last one as is.  */
tree
hook_tree_tree_tree_tree_3rd_identity (tree ARG_UNUSED(a),
				       tree ARG_UNUSED(b), tree c)
{
  return c;
}

/* Generic hook that takes a tree and returns a NULL string.  */
const char *
hook_constcharptr_const_tree_null (const_tree ARG_UNUSED(t))
{
  return NULL;
}

tree
hook_tree_tree_tree_bool_null (tree ARG_UNUSED(t0),
			       tree ARG_UNUSED(t1),
			       bool ARG_UNUSED(ignore))
{
  return NULL;
}

tree
hook_tree_tree_tree_null (tree ARG_UNUSED(t0), tree t1 ATTRIBUTE_UNUSED)
{
  return NULL;
}

tree
hook_tree_tree_tree_tree_null (tree ARG_UNUSED(t0),
			       tree ARG_UNUSED(t1),
			       tree ARG_UNUSED(t2))
{
  return NULL;
}

/* Generic hook that takes a rtx and returns a NULL string.  */
const char *
hook_constcharptr_const_rtx_null (const_rtx ARG_UNUSED(r))
{
  return NULL;
}

const char *
hook_constcharptr_const_tree_const_tree_null (const_tree ARG_UNUSED(t0),
					      const_tree ARG_UNUSED(t1))
{
  return NULL;
}

const char *
hook_constcharptr_int_const_tree_null (int ARG_UNUSED(i),
				       const_tree ARG_UNUSED(t0))
{
  return NULL;
}

const char *
hook_constcharptr_int_const_tree_const_tree_null (int ARG_UNUSED(i),
						  const_tree ARG_UNUSED(t0),
						  const_tree ARG_UNUSED(t1))
{
  return NULL;
}

/* Generic hook that takes a const_tree and returns NULL_TREE.  */
tree
hook_tree_const_tree_null (const_tree ARG_UNUSED(t))
{
  return NULL;
}
