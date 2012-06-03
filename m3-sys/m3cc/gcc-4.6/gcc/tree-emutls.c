/* Modula-3: modified */

/* Lower TLS operations to emulation functions.
   Copyright (C) 2006, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "tree-flow.h"
#include "cgraph.h"
#include "langhooks.h"
#include "target.h"
#include "targhooks.h"
#include "tree-iterator.h"

EXTERN_C_START

/* Whenever a target does not support thread-local storage (TLS) natively,
   we can emulate it with some run-time support in libgcc.  This will in
   turn rely on "keyed storage" a-la pthread_key_create; essentially all
   thread libraries provide such functionality.

   In order to coordinate with the libgcc runtime, each TLS variable is
   described by a "control variable".  This control variable records the
   required size, alignment, and initial value of the TLS variable for
   instantiation at runtime.  It also stores an integer token to be used
   by the runtime to find the address of the variable within each thread.

   On the compiler side, this means that we need to replace all instances
   of "tls_var" in the code with "*__emutls_get_addr(&control_var)".  We
   also need to eliminate "tls_var" from the symbol table and introduce
   "control_var".

   We used to perform all of the transformations during conversion to rtl,
   and the variable substitutions magically within assemble_variable.
   However, this late fiddling of the symbol table conflicts with LTO and
   whole-program compilation.  Therefore we must now make all the changes
   to the symbol table early in the GIMPLE optimization path, before we
   write things out to LTO intermediate files.  */

/* These two vectors, once fully populated, are kept in lock-step so that
   the index of a TLS variable equals the index of its control variable in
   the other vector.  */
static varpool_node_set tls_vars;
static VEC(varpool_node_ptr, heap) *control_vars;

/* For the current basic block, an SSA_NAME that has computed the address 
   of the TLS variable at the corresponding index.  */
static VEC(tree, heap) *access_vars;

/* The type of the control structure, shared with the emutls.c runtime.  */
static tree emutls_object_type;

#if !defined (NO_DOT_IN_LABEL)
# define EMUTLS_SEPARATOR	"."
#elif !defined (NO_DOLLAR_IN_LABEL)
# define EMUTLS_SEPARATOR	"$"
#else
# define EMUTLS_SEPARATOR	"_"
#endif

/* Create an IDENTIFIER_NODE by prefixing PREFIX to the
   IDENTIFIER_NODE NAME's name.  */

static tree
prefix_name (const char *prefix, tree name)
{
  unsigned plen = strlen (prefix);
  unsigned nlen = strlen (IDENTIFIER_POINTER (name));
  char *toname = (char *) alloca (plen + nlen + 1);

  memcpy (toname, prefix, plen);
  memcpy (toname + plen, IDENTIFIER_POINTER (name), nlen + 1);

  return get_identifier (toname);
}

/* Create an identifier for the struct __emutls_object, given an identifier
   of the DECL_ASSEMBLY_NAME of the original object.  */

static tree
get_emutls_object_name (tree name)
{
  const char *prefix = (targetm.emutls.var_prefix
			? targetm.emutls.var_prefix
			: "__emutls_v" EMUTLS_SEPARATOR);
  return prefix_name (prefix, name);
}

/* Create the fields of the type for the control variables.  Ordinarily
   this must match struct __emutls_object defined in emutls.c.  However
   this is a target hook so that VxWorks can define its own layout.  */

tree
default_emutls_var_fields (tree type, tree *name ATTRIBUTE_UNUSED)
{
  tree word_type_node, field, next_field;

  field = build_decl (UNKNOWN_LOCATION,
		      FIELD_DECL, get_identifier ("__templ"), ptr_type_node);
  DECL_CONTEXT (field) = type;
  next_field = field;

  field = build_decl (UNKNOWN_LOCATION,
		      FIELD_DECL, get_identifier ("__offset"),
		      ptr_type_node);
  DECL_CONTEXT (field) = type;
  DECL_CHAIN (field) = next_field;
  next_field = field;

  word_type_node = lang_hooks.types.type_for_mode (word_mode, 1);
  field = build_decl (UNKNOWN_LOCATION,
		      FIELD_DECL, get_identifier ("__align"),
		      word_type_node);
  DECL_CONTEXT (field) = type;
  DECL_CHAIN (field) = next_field;
  next_field = field;

  field = build_decl (UNKNOWN_LOCATION,
		      FIELD_DECL, get_identifier ("__size"), word_type_node);
  DECL_CONTEXT (field) = type;
  DECL_CHAIN (field) = next_field;

  return field;
}

/* Initialize emulated tls object TO, which refers to TLS variable DECL and
   is initialized by PROXY.  As above, this is the default implementation of
   a target hook overridden by VxWorks.  */

tree
default_emutls_var_init (tree to, tree decl, tree proxy)
{
  VEC(constructor_elt,gc) *v = VEC_alloc (constructor_elt, gc, 4);
  constructor_elt *elt;
  tree type = TREE_TYPE (to);
  tree field = TYPE_FIELDS (type);

  elt = VEC_quick_push (constructor_elt, v, NULL);
  elt->index = field;
  elt->value = fold_convert (TREE_TYPE (field), DECL_SIZE_UNIT (decl));

  elt = VEC_quick_push (constructor_elt, v, NULL);
  field = DECL_CHAIN (field);
  elt->index = field;
  elt->value = build_int_cst (TREE_TYPE (field),
			      DECL_ALIGN_UNIT (decl));

  elt = VEC_quick_push (constructor_elt, v, NULL);
  field = DECL_CHAIN (field);
  elt->index = field;
  elt->value = null_pointer_node;

  elt = VEC_quick_push (constructor_elt, v, NULL);
  field = DECL_CHAIN (field);
  elt->index = field;
  elt->value = proxy;

  return build_constructor (type, v);
}

EXTERN_C_END
