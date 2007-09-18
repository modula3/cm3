/* Support for printing C and C++ types for GDB, the GNU debugger.
   Copyright 1986, 1988, 1989, 1991 Free Software Foundation, Inc.

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


#include "defs.h"
#include "obstack.h"
#include "c-lang.h"


#include "m3-typeprint.h" 
#include "m3-util.h"


/* Print the name of the type (or the ultimate pointer target,
   function value or array element), or the description of a
   structure or union.

   SHOW nonzero means don't print this type as just its name;
   show its real definition even if it has a name.
   SHOW zero means print just typename or struct tag if there is one
   SHOW negative means abbreviate structure elements.
   SHOW is decremented for printing of structure elements.

   LEVEL is the depth to indent by.
   We increase it for some recursive calls.  */

void
m3_type_print_base (type, stream, show, level)
     struct type *type;
     struct ui_file *stream;
     int show;
     int level;
{
  char *name;
  register int i;
  register int len;
  char *mangled_name;
  char *demangled_name;
  enum {s_none, s_public, s_private, s_protected} section_type;
  struct type *t;
  QUIT;

  wrap_here ("    ");
  if (type == NULL)
    {
      fputs_filtered ("<type unknown>", stream);
      return;
    }

  /* When SHOW is zero or less, and there is a valid type name, then always
     just print the type name directly from the type. */

  if (show <= 0) {
    char *n = find_m3_type_name (type);
    if ((n) && (strncmp (n, "<typeid=", 8) != 0)) {
      fputs_filtered (n, stream);
      return; }}

  if (show < 0) {
    fprintf_filtered (stream, "..."); 
    return; }

  switch (TYPE_CODE (type))
    {
    case TYPE_CODE_M3_ARRAY:
      fprintf_filtered (stream, "ARRAY ");
      m3_type_print_base (TYPE_M3_ARRAY_INDEX (type), stream, show-1, level);
      fprintf_filtered (stream, " OF ");
      m3_type_print_base (TYPE_M3_ARRAY_ELEM (type), stream, show-1, level);
      break;

    case TYPE_CODE_M3_OPEN_ARRAY:
      fprintf_filtered (stream, "ARRAY OF ");
      m3_type_print_base (TYPE_M3_OPEN_ARRAY_ELEM (type), stream, show-1, level);
      break;

    case TYPE_CODE_M3_PACKED:
      fprintf_filtered (stream, "BITS %d FOR ", (int)TYPE_M3_SIZE (type));
      m3_type_print_base (TYPE_M3_PACKED_TARGET (type), stream, show-1, level);
      break;

    case TYPE_CODE_M3_ENUM:
      fprintf_filtered (stream, "{");
      for (i = 0; i < TYPE_M3_ENUM_NVALS (type); i++) {
	if (i != 0) { fprintf_filtered (stream, ", "); }
	wrap_here ("    ");
	fputs_filtered (TYPE_M3_ENUM_VALNAME (type, i), stream); }
      fprintf_filtered (stream, "}");
      break;

    case TYPE_CODE_M3_INDIRECT:
      m3_type_print_base (TYPE_M3_TARGET (type), stream, show, level);
      break;

    case TYPE_CODE_M3_OBJECT: {
      int sc = TYPE_CODE (TYPE_M3_OBJ_SUPER (type));

       if (sc == TYPE_CODE_M3_ROOT) {
	 /* nothing */ }
       else if (sc == TYPE_CODE_M3_TRANSIENT_ROOT) {
	 fprintf_filtered (stream, "TRANSIENT "); }
       else if (sc == TYPE_CODE_M3_UN_ROOT) {
	 fprintf_filtered (stream, "UNTRACED "); }
       else {
	 m3_type_print_base (TYPE_M3_OBJ_SUPER (type) , stream, show-1, level);
	 fprintf_filtered (stream, " ");
         wrap_here ("  "); }

      if (TYPE_M3_OBJ_BRANDED (type)) {
	fprintf_filtered (stream, "BRANDED \"%s\" ", 
			  TYPE_M3_OBJ_BRAND (type)); }

      fprintf_filtered (stream, "OBJECT ");
      for (i = 0; i < TYPE_M3_OBJ_NFIELDS (type); i++) {
	fprintf_filtered (stream, "%s: ", TYPE_M3_OBJ_FIELD_NAME (type, i));
	m3_type_print_base (TYPE_M3_OBJ_FIELD_TYPE (type, i), 
			    stream, show-1, level);
	fprintf_filtered (stream, "; ");
	wrap_here ("    "); }

      if (TYPE_M3_OBJ_NMETHODS (type) > 0) fprintf_filtered (stream, "METHODS ");
      for (i = 0; i < TYPE_M3_OBJ_NMETHODS (type); i++) {
	fprintf_filtered (stream, "%s: ", TYPE_M3_OBJ_METHOD_NAME (type, i));
	m3_type_print_base (TYPE_M3_OBJ_METHOD_TYPE (type, i), stream, show-1, level);
	fprintf_filtered (stream, "; ");
	wrap_here ("    "); }
      fprintf_filtered (stream, "END");
      break; }

    case TYPE_CODE_M3_PROC:
    case TYPE_CODE_M3_METHOD: /* REVIEWME: What do we want to do here? */ 
      if (show < 0) {
	fprintf_filtered (stream, "PROCEDURE ..."); 
	break; }

      fprintf_filtered (stream, "PROCEDURE (");
      for (i = 0; i < TYPE_M3_PROC_NARGS (type); i++) {
	if (i != 0) {
	  fprintf_filtered (stream, "; ");
	  wrap_here ("    "); }
	fprintf_filtered (stream, "%s: ", TYPE_M3_PROC_ARG_NAME (type, i) + 1);
	m3_type_print_base (TYPE_M3_PROC_ARG_TYPE (type, i), 
			    stream, 0, level); }
      fprintf_filtered (stream, ")");
      if (M3_TYPEP (TYPE_CODE (TYPE_TARGET_TYPE (type)))
	  && TYPE_CODE (TYPE_TARGET_TYPE (type)) != TYPE_CODE_M3_VOID) {
	fprintf_filtered (stream, ": ");
	m3_type_print_base (TYPE_TARGET_TYPE (type),
			    stream, 0, level); }
      switch (TYPE_M3_PROC_NRAISES (type))
	{
	case -1: fprintf_filtered (stream, " RAISES ANY");  break;
        case  0:                                            break;
	default: fprintf_filtered (stream, " RAISES {");
	  for (i = 0; i < TYPE_M3_PROC_NRAISES (type); i++) {
	    if (i != 0) {
	      fprintf_filtered (stream, ", ");
	      wrap_here ("    "); }
	    fprintf_filtered (stream, "%s", 
			      TYPE_M3_PROC_RAISE_NAME (type, i)); }
	  fprintf_filtered (stream, "}"); }
      break;
	  
    case TYPE_CODE_M3_RECORD:
      fprintf_filtered (stream, "RECORD ");
      for (i = 0; i < TYPE_M3_REC_NFIELDS (type); i++) {
	fprintf_filtered (stream, "%s: ", TYPE_M3_REC_FIELD_NAME (type, i));
	m3_type_print_base (TYPE_M3_REC_FIELD_TYPE (type, i),
			    stream, show-1, level);
	fprintf_filtered (stream, "; ");
	wrap_here ("    "); }
      fprintf_filtered (stream, "END");
      break;
      
    case TYPE_CODE_M3_SET:
      fprintf_filtered (stream, "SET OF ");
      m3_type_print_base (TYPE_M3_SET_TARGET (type), stream, show-1, level);
      break; 

    case TYPE_CODE_M3_POINTER: {
      /* FIXME: This isn't right.  There can be real REF ARRAY OF CHAR that
         are not TEXT.  Arrange so the statment below is no longer true,
         then delete this case. rodney.bates@wichita.edu */ 
      /* Texts are passed as TYPE_CODE_M3_POINTER, not as TYPE_CODE_M3_TEXT ... */
      struct type *target = TYPE_M3_POINTER_TARGET (type);
      if (TYPE_CODE (target) == TYPE_CODE_M3_OPEN_ARRAY
	  && TYPE_CODE (TYPE_M3_OPEN_ARRAY_ELEM (target)) == TYPE_CODE_M3_CHAR) {
	fprintf_filtered (stream, "TEXT"); }
      else {
	if (! TYPE_M3_POINTER_TRACED (type)) {
	  fprintf_filtered (stream, "UNTRACED "); }
	
	if (TYPE_M3_POINTER_BRANDED (type)) {
	  fprintf_filtered (stream, "BRANDED \"%s\" ",
			    TYPE_M3_POINTER_BRAND (type)); }
	fprintf_filtered (stream, "REF ");
	if (show >= 0) {
	  m3_type_print_base (TYPE_M3_POINTER_TARGET (type), stream,
			      show-1, level); }
	else {
	  fprintf_filtered (stream, "..."); }
      }
	break; }
	
    case TYPE_CODE_M3_SUBRANGE: {
      LONGEST lower, upper;
      struct type *target = TYPE_M3_SUBRANGE_TARGET (type);
      int en = (TYPE_CODE (target) == TYPE_CODE_M3_ENUM);
      
      m3_ordinal_bounds (type, &lower, &upper);
      fprintf_filtered (stream, "[");
      if (en) {
	fputs_filtered (TYPE_M3_ENUM_VALNAME (target, lower), stream); }
      else {
	print_longest (stream, 'd', 1, lower); }
      fprintf_filtered (stream, " .. ");
      if (en) {
	fputs_filtered (TYPE_M3_ENUM_VALNAME (target, upper), stream); }
      else {
	print_longest (stream, 'd', 1, upper); }
      fprintf_filtered (stream, "]");
      break; }

    case TYPE_CODE_M3_ADDRESS:
      fprintf_filtered (stream, "ADDRESS");
      break; 

    case TYPE_CODE_M3_BOOLEAN:
      fprintf_filtered (stream, "BOOLEAN");
      break; 

    case TYPE_CODE_M3_CHAR:
      fprintf_filtered (stream, "CHAR");
      break; 

    case TYPE_CODE_M3_INTEGER:
      fprintf_filtered (stream, "INTEGER");
      break; 

    case TYPE_CODE_M3_CARDINAL:
      fprintf_filtered (stream, "CARDINAL");
      break; 

    case TYPE_CODE_M3_REFANY:
      fprintf_filtered (stream, "REFANY");
      break; 

    case TYPE_CODE_M3_TRANSIENT_REFANY:
      fprintf_filtered (stream, "TRANSIENT REFANY");
      break; 

    case TYPE_CODE_M3_MUTEX:
      fprintf_filtered (stream, "MUTEX");
      break; 

    case TYPE_CODE_M3_NULL:
      fprintf_filtered (stream, "NULL");
      break; 

    case TYPE_CODE_M3_ROOT:
      fprintf_filtered (stream, "ROOT");
      break; 

    case TYPE_CODE_M3_TRANSIENT_ROOT:
      fprintf_filtered (stream, "TRANSIENT ROOT");
      break; 

    case TYPE_CODE_M3_TEXT:
      fprintf_filtered (stream, "TEXT");
      break; 

    case TYPE_CODE_M3_UN_ROOT:
      fprintf_filtered (stream, "UNTRACED ROOT");
      break; 

    case TYPE_CODE_M3_VOID:
      fprintf_filtered (stream, "VOID");
      break; 

    default:
      /* Handle types not explicitly handled by the other cases,
	 such as fundamental types.  For these, just print whatever
	 the type name is, as recorded in the type itself.  If there
	 is no type name, then complain. */
      if (TYPE_NAME (type) != NULL)
	{
	  fputs_filtered (TYPE_NAME (type), stream);
	}
      else
	{
	  c_type_print_base (type, stream, show, level);
	}
      break;
    }
} /* m3_type_print_base */ 

/* End of file m3-typeprint.c */ 
