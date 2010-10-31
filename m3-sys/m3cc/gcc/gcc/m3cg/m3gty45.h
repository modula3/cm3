/* Modula-3 Compiler back end parser.

   Copyright (C) 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.

   In other words, you are welcome to use, share and improve this program.
   You are forbidden to forbid anyone else to use, share and improve
   what you give them.   Help stamp out software-hoarding! */

/* Types expected by gcc's garbage collector.
   These types exist to allow language front-ends to
   add extra information in gcc's parse tree data structure.
   But the treelang front end doesn't use them -- it has
   its own parse tree data structure.
   We define them here only to satisfy gcc's garbage collector.  */

/* Language-specific identifier information.  */

struct GTY(()) lang_identifier
{
  struct tree_identifier common;
};

/* Language-specific tree node information.  */

union GTY((desc("TREE_CODE(&%h.generic) == IDENTIFIER_NODE"))) lang_tree_node
{
  union GTY((tag("0"), desc("tree_node_structure (&%h)"))) tree_node generic;
  struct GTY((tag("1"))) lang_identifier identifier;
};

/* Language-specific type information.  */

typedef char* PSTR;
typedef const char* PCSTR;
typedef unsigned char UCHAR;
typedef unsigned int UINT;
typedef unsigned long ULONG;
typedef unsigned HOST_WIDE_INT UWIDE;
typedef HOST_WIDE_INT WIDE;

typedef enum {
  lang_type_array,
  lang_type_enum,
  lang_type_object,
  lang_type_open_array,
  lang_type_record,
  lang_type_set,
  lang_type_subrange,
  lang_type_proctype
} enum_lang_type;

struct GTY(()) lang_field
{
  PCSTR name;
  UWIDE offset;
  UWIDE size;
  ULONG my_id;
};

struct GTY(()) lang_formal
{
  PCSTR name;
  ULONG my_id;
};

struct GTY(()) lang_proctype
{
  ULONG my_id;
  ULONG result_id;
  UWIDE n_formals;
  UWIDE n_raises;
  UCHAR cc;
  struct lang_formal* formals;
  PCSTR* raises;
};

struct GTY(()) lang_method
{
  PCSTR name;
  ULONG my_id; /* of a proctype */
};

struct GTY(()) lang_type
{
  ULONG my_id;
  enum_lang_type type;
  union GTY ((desc ("%1.type"))) {
    struct {
      UWIDE size;
      ULONG index_id;
      ULONG elts_id;
    } GTY ((tag ("lang_type_array"))) array;
    struct {
      UWIDE n_elts;
      UWIDE size;
    } GTY ((tag ("lang_type_enum"))) xenum;
    struct {
      UWIDE size;
      ULONG elts_id;
    } GTY ((tag ("lang_type_open_array"))) open_array;
    struct {
      UWIDE n_fields;
      struct lang_field* GTY(()) fields;
    } GTY ((tag ("lang_type_record"))) record;
    struct {
      PCSTR brand; /* can be NULL */
      UWIDE n_fields;
      UWIDE n_methods;
      UWIDE field_size;
      struct lang_field* GTY(()) fields;
      struct lang_method* GTY(()) methods;
      ULONG my_id;
      ULONG super_id;
      bool traced;
    } GTY ((tag ("lang_type_object"))) object;
    struct {
      UWIDE size;
      ULONG domain_id;
    } GTY ((tag ("lang_type_set"))) set;
    struct {
      WIDE min;
      WIDE max;
      UWIDE size;
      ULONG domain_id;
    } GTY ((tag ("lang_type_subrange"))) subrange;
  } u;
};

/* Language-specific declaration information.  */

typedef struct GTY(()) lang_decl
{
  char junk; /* dummy field to ensure struct is not empty */
} lang_decl_t;

struct GTY(()) language_function
{
    bool volatil; /* does function call setjmp/fork/vfork */
};

typedef struct GTY(()) m3type
{
  ULONG typeid;
  tree GTY(()) t;
} m3type_t;
