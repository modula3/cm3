/* C language support definitions for GDB, the GNU debugger.
   Copyright 1992 Free Software Foundation, Inc.

This file is part of GDB.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

extern int
m3_parse PARAMS ((void));	/* Defined in c-exp.y */

extern void			/* Defined in c-typeprint.c */
m3_print_type PARAMS ((struct type *, char *, FILE *, int, int));

extern int
m3_val_print PARAMS ((struct type *, char *, CORE_ADDR, FILE *, int, int,
		     int, enum val_prettyprint));

extern struct type *
m3_find_export_type PARAMS ((struct type *));

extern struct type *builtin_type_m3_address;
extern struct type *builtin_type_m3_boolean;
extern struct type *builtin_type_m3_cardinal;
extern struct type *builtin_type_m3_char;
extern struct type *builtin_type_m3_extended;
extern struct type *builtin_type_m3_integer;
extern struct type *builtin_type_m3_longreal;
extern struct type *builtin_type_m3_mutex;
extern struct type *builtin_type_m3_null;
extern struct type *builtin_type_m3_real;
extern struct type *builtin_type_m3_refany;
extern struct type *builtin_type_m3_root;
extern struct type *builtin_type_m3_text;
extern struct type *builtin_type_m3_untraced_root;
extern struct type *builtin_type_m3_void;

extern LONGEST
m3_unpack_ord PARAMS ((char *valaddr, int bitpos, int bitsize, int sign_extend));

extern CORE_ADDR
m3_unpack_pointer PARAMS ((char *valaddr, int bitpos));

extern LONGEST
m3_unpack_int2 PARAMS ((value_ptr val));

extern double
m3_unpack_float2 PARAMS ((value_ptr val));

extern CORE_ADDR
m3_unpack_pointer2 PARAMS ((value_ptr val));

extern struct type *
find_m3_type_with_uid PARAMS ((int uid));

extern struct type *
find_m3_type_named PARAMS ((char *name));

extern struct type *
find_m3_exported_interfaces PARAMS ((char *name));

extern struct symbol *
find_m3_ir PARAMS ((int kind, char* name));

extern char *
find_m3_type_name PARAMS ((struct type *type));


/* given a heap reference,
   find the address of the typecell for the actual type */
extern CORE_ADDR
find_m3_heap_tc_addr PARAMS ((CORE_ADDR addr));

/* given the address of a typecell, find the gdb type for it */
extern struct type *
find_m3_type_from_tc PARAMS ((CORE_ADDR tc_addr));

/* given a heap reference, find it's actual type */
extern struct type *
find_m3_heap_type PARAMS ((CORE_ADDR addr));

extern int
tc_address_to_dataOffset PARAMS ((CORE_ADDR tc_addr));

extern int
tc_address_to_methodOffset PARAMS ((CORE_ADDR tc_addr));

extern int
tc_address_to_dataSize PARAMS ((CORE_ADDR tc_addr));

extern CORE_ADDR
tc_address_to_parent_tc_address PARAMS ((CORE_ADDR tc_addr));

extern CORE_ADDR
tc_address_to_defaultMethods PARAMS ((CORE_ADDR tc_addr));

extern value_ptr
m3_value_from_longest PARAMS ((struct type *type, LONGEST num));

extern int
is_m3_ordinal_type PARAMS ((struct type *type));

extern void
m3_ordinal_bounds PARAMS ((struct type *type, LONGEST *lower, LONGEST *upper));

extern int
m3_value_print PARAMS ((struct value *, GDB_FILE *, int, enum val_prettyprint));
