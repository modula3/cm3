/* Modula-3: modified */

/* LTO symbol table.
   Copyright 2009, 2010 Free Software Foundation, Inc.
   Contributed by CodeSourcery, Inc.

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
#include "diagnostic-core.h"
#include "tree.h"
#include "gimple.h"
#include "ggc.h"
#include "hashtab.h"
#include "plugin-api.h"
#include "lto-streamer.h"

EXTERN_C_START

tree
lto_symtab_prevailing_decl (tree decl);

/* Vector to keep track of external variables we've seen so far.  */
VEC(tree,gc) *lto_global_var_decls;

/* Symbol table entry.  */

struct GTY(()) lto_symtab_entry_def
{
  /* The symbol table entry key, an IDENTIFIER.  */
  tree id;
  /* The symbol table entry, a DECL.  */
  tree decl;
  /* The cgraph node if decl is a function decl.  Filled in during the
     merging process.  */
  struct cgraph_node *node;
  /* The varpool node if decl is a variable decl.  Filled in during the
     merging process.  */
  struct varpool_node *vnode;
  enum ld_plugin_symbol_resolution resolution;
  /* True when resolution was guessed and not read from the file.  */
  bool guessed;
  /* Pointer to the next entry with the same key.  Before decl merging
     this links all symbols from the different TUs.  After decl merging
     this links merged but incompatible decls, thus all prevailing ones
     remaining.  */
  struct lto_symtab_entry_def *next;
};
typedef struct lto_symtab_entry_def *lto_symtab_entry_t;

/* A poor man's symbol table. This hashes identifier to prevailing DECL
   if there is one. */

static GTY ((if_marked ("lto_symtab_entry_marked_p"),
	     param_is (struct lto_symtab_entry_def)))
  htab_t lto_symtab_identifiers;

/* Free symtab hashtable.  */

void
lto_symtab_free (void)
{
  htab_delete (lto_symtab_identifiers);
  lto_symtab_identifiers = NULL;
}

/* Return the hash value of an lto_symtab_entry_t object pointed to by P.  */

static hashval_t
lto_symtab_entry_hash (const void *p)
{
  const struct lto_symtab_entry_def *base =
    (const struct lto_symtab_entry_def *) p;
  return IDENTIFIER_HASH_VALUE (base->id);
}

/* Return non-zero if P1 and P2 points to lto_symtab_entry_def structs
   corresponding to the same symbol.  */

static int
lto_symtab_entry_eq (const void *p1, const void *p2)
{
  const struct lto_symtab_entry_def *base1 =
     (const struct lto_symtab_entry_def *) p1;
  const struct lto_symtab_entry_def *base2 =
     (const struct lto_symtab_entry_def *) p2;
  return (base1->id == base2->id);
}

/* Returns non-zero if P points to an lto_symtab_entry_def struct that needs
   to be marked for GC.  */

static int
lto_symtab_entry_marked_p (const void *p)
{
  const struct lto_symtab_entry_def *base =
     (const struct lto_symtab_entry_def *) p;

  /* Keep this only if the common IDENTIFIER_NODE of the symtab chain
     is marked which it will be if at least one of the DECLs in the
     chain is marked.  */
  return ggc_marked_p (base->id);
}

/* Lazily initialize resolution hash tables.  */

static void
lto_symtab_maybe_init_hash_table (void)
{
  if (lto_symtab_identifiers)
    return;

  lto_symtab_identifiers =
    htab_create_ggc (1021, lto_symtab_entry_hash,
		     lto_symtab_entry_eq, NULL);
}

/* Get the lto_symtab_entry_def struct associated with ID
   if there is one.  */

static lto_symtab_entry_t
lto_symtab_get (tree id)
{
  gcc_unreachable ();
  return 0;
}

/* Get the linker resolution for DECL.  */

enum ld_plugin_symbol_resolution
lto_symtab_get_resolution (tree decl)
{
  gcc_unreachable ();
  return LDPR_UNKNOWN;
}


/* Replace the cgraph node NODE with PREVAILING_NODE in the cgraph, merging
   all edges and removing the old node.  */

static void
lto_cgraph_replace_node (struct cgraph_node *node,
			 struct cgraph_node *prevailing_node)
{
  gcc_unreachable ();
}

/* Replace the cgraph node NODE with PREVAILING_NODE in the cgraph, merging
   all edges and removing the old node.  */

static void
lto_varpool_replace_node (struct varpool_node *vnode,
			  struct varpool_node *prevailing_node)
{
  gcc_unreachable ();
}

/* Merge two variable or function symbol table entries PREVAILING and ENTRY.
   Return false if the symbols are not fully compatible and a diagnostic
   should be emitted.  */

static bool
lto_symtab_merge (lto_symtab_entry_t prevailing, lto_symtab_entry_t entry)
{
  gcc_unreachable ();
  return false;
}

/* Return true if the symtab entry E can be replaced by another symtab
   entry.  */

static bool
lto_symtab_resolve_replaceable_p (lto_symtab_entry_t e)
{
  gcc_unreachable ();
  return false;
}

/* Return true if the symtab entry E can be the prevailing one.  */

static bool
lto_symtab_resolve_can_prevail_p (lto_symtab_entry_t e)
{
  /* The C++ frontend ends up neither setting TREE_STATIC nor
     DECL_EXTERNAL on virtual methods but only TREE_PUBLIC.
     So do not reject !TREE_STATIC here but only DECL_EXTERNAL.  */
  if (DECL_EXTERNAL (e->decl))
    return false;

  /* For functions we need a non-discarded body.  */
  if (TREE_CODE (e->decl) == FUNCTION_DECL)
    return (e->node
	    && (e->node->analyzed
	        || (e->node->same_body_alias && e->node->same_body->analyzed)));

  /* A variable should have a size.  */
  else if (TREE_CODE (e->decl) == VAR_DECL)
    {
      if (!e->vnode)
	return false;
      if (e->vnode->finalized)
	return true;
      return e->vnode->alias && e->vnode->extra_name->finalized;
    }

  gcc_unreachable ();
}


/* Resolve and merge all symbol table chains to a prevailing decl.  */

void
lto_symtab_merge_decls (void)
{
  gcc_unreachable ();
}

void
lto_symtab_merge_cgraph_nodes (void)
{
  gcc_unreachable ();
}

/* Given the decl DECL, return the prevailing decl with the same name. */

tree
lto_symtab_prevailing_decl (tree decl)
{
  gcc_unreachable ();
  return 0;
}

EXTERN_C_END

#include "gt-lto-symtab.h"
