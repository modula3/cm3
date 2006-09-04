/* Modula-3 language support definitions for GDB, the GNU debugger.
   Copyright 2006, Free Software Foundation, Inc.

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

/* This file contains code to evaluate Modula-3 expressions. */ 

#include "defs.h"
#include "block.h"
#include "gdbtypes.h"
#include "gdb_assert.h"
#include "gdbcore.h"
#include "gdb_string.h"
#include "expression.h"
#include "infcall.h" 
#include "value.h" 
#include "m3-eval.h" 
#include "m3-lang.h" 
#include "m3-util.h" 
#include "m3-valprint.h" 

static LONGEST
m3_div (a, b)
  LONGEST a, b;
{
  if (a == 0) { return 0; }
  if (a < 0) {
    return (b < 0)
             ? ((-a) / (-b))
	     : (- ((-a-1) / b) - 1);
  } else {
    return (b < 0)
             ? (- ((a - 1) / (-b)) - 1)
             : (a / b);
  }
} /* m3_div */

static LONGEST
m3_modi (a, b)
  LONGEST a, b;
{
  if (a == 0) { return 0; }
  if (a < 0) {
    return (b < 0)
             ? (- ((-a) % (-b)))
             : (b - 1 - ((-a-1) % b));
  } else {
    return (b < 0)
             ? (b + 1 + ((a - 1) % (-b)))
             : (a % b);
  }
} /* m3_modi */

static double
m3_modf (a, b)
  double a, b;
{
  double  z = a / b;
  LONGEST zi = (LONGEST) z;
  if ((z < 0.0) && ((double)zi != z)) { zi--; }
  return a - b * (double)zi;
} /* m3_modf */

/* Simulate the Modula-3  operator = by returning a 1
   iff ARG1 and ARG2 have equal contents.  */

static int
m3_value_equal (struct value *arg1, struct value *arg2) 
{
  int len;
  const char * p1, * p2;
  enum type_code code1;
  enum type_code code2;

  coerce_array (arg1);
  coerce_array (arg2);

  code1 = TYPE_CODE (value_type (arg1));
  code2 = TYPE_CODE (value_type (arg2));

  if (code1 == TYPE_CODE_M3_INTEGER) { code1 = TYPE_CODE_INT; }
  if (code2 == TYPE_CODE_M3_INTEGER) { code2 = TYPE_CODE_INT; }
  if (code1 == TYPE_CODE_M3_POINTER) { code1 = TYPE_CODE_PTR; }
  if (code2 == TYPE_CODE_M3_POINTER) { code2 = TYPE_CODE_PTR; }

  if (code1 == TYPE_CODE_M3_INTEGER && code2 == TYPE_CODE_M3_INTEGER)
    return m3_value_as_integer (arg1) == m3_value_as_integer (arg2);
  else if (code1 == TYPE_CODE_FLT && code2 == TYPE_CODE_M3_INTEGER)
    return m3_value_as_float (arg1) == (double) m3_value_as_integer (arg2);
  else if (code2 == TYPE_CODE_FLT && code1 == TYPE_CODE_M3_INTEGER)
    return m3_value_as_float (arg2) == (double) m3_value_as_integer (arg1);
  else if (code1 == TYPE_CODE_FLT && code2 == TYPE_CODE_FLT)
    return m3_value_as_float (arg1) == m3_value_as_float (arg2);

  /* FIXME: Need to promote to either CORE_ADDR or LONGEST, whichever
     is bigger.  */
  else if (code1 == TYPE_CODE_M3_POINTER && code2 == TYPE_CODE_M3_INTEGER)
    return m3_value_as_address (arg1) == (CORE_ADDR) m3_value_as_integer (arg2);
  else if (code2 == TYPE_CODE_M3_POINTER && code1 == TYPE_CODE_M3_INTEGER)
    return (CORE_ADDR) m3_value_as_integer (arg1) == m3_value_as_address (arg2);

  else if (code1 == code2
           && ((len = TYPE_LENGTH (value_type (arg1)))
               == TYPE_LENGTH (value_type (arg2))))
    {
      p1 = value_contents (arg1);
      p2 = value_contents (arg2);
      while (--len >= 0)
        {
          if (*p1++ != *p2++)
            break;
        }
      return len < 0;
    }
  else
    {
      error ("Invalid type combination in equality test.");
      return 0;  /* For lint -- never reached */
    }
}

static bool
m3_types_equal ( struct type * left, struct type * right ); 

static bool 
m3_type_fields_equal ( struct type * left, struct type * right ) 

{ int i;

  if ( TYPE_NFIELDS ( left ) != TYPE_NFIELDS ( right ) )
    { return false; } 
  for ( i = 0; i < TYPE_NFIELDS ( left ); i ++ ) 
    { if ( ! m3_types_equal 
               ( TYPE_M3_FIELD_TYPE ( left, i ), 
                 TYPE_M3_FIELD_TYPE ( right, i ) 
               ) 
         ) { return false; } 
    } 
  return true; 
} 

/* Strip away any indirect types from a type. */ 
static struct type * 
m3_direct_type ( struct type * param_type ) 

  { struct type * l_type; 

    if ( param_type == NULL ) { return NULL; } 
    l_type = param_type; 
    while ( TYPE_CODE ( l_type ) == TYPE_CODE_M3_INDIRECT ) 
      { l_type = TYPE_M3_INDIRECT_TARGET ( l_type ); } 
    return l_type; 
  } /* m3_direct_type */ 

/* Strip away any indirect and packed types from a type. */ 
static struct type * 
m3_unpacked_type ( struct type * param_type ) 

  { struct type * l_type; 

    if ( param_type == NULL ) { return NULL; } 
    l_type = param_type; 
    while ( true ) 
      { switch  ( TYPE_CODE ( l_type ) ) 
          { case TYPE_CODE_M3_INDIRECT: 
              l_type = TYPE_M3_INDIRECT_TARGET ( l_type ); 
              break; /* And loop. */  
            case TYPE_CODE_M3_PACKED: 
              l_type = TYPE_M3_PACKED_TARGET ( l_type ); 
              break; /* And loop. */  
            default:
              return l_type; 
          }
      } 
  } /* m3_unpacked_type */ 

static struct  type *  
m3_revealed_type ( struct type * opaque_type ) 

  { if ( opaque_type != NULL 
         && TYPE_CODE ( opaque_type ) == TYPE_CODE_M3_OPAQUE 
       ) 
      { return TYPE_M3_OPAQUE_REVEALED ( opaque_type ); } 
    return opaque_type; 
  } /* m3_revealed_type */ 

bool
m3_types_equal ( struct type * left, struct type * right ) 

{ struct type * left_direct; 
  struct type * right_direct; 
  int i; 

  if ( left == NULL || right == NULL ) { return false; } 
  left_direct = m3_direct_type ( left ); 
  right_direct = m3_direct_type ( right ); 
  left_direct = m3_revealed_type ( left ); 
  right_direct = m3_revealed_type ( right ); 
  if ( left_direct == right_direct ) { return true; } 
  if ( TYPE_CODE ( left_direct ) != TYPE_CODE ( right_direct ) ) 
    { return false; } 
  switch ( TYPE_CODE ( left_direct ) ) 
    { case TYPE_CODE_M3_ARRAY : 
        return m3_type_fields_equal ( left_direct, right_direct ); 
      case TYPE_CODE_M3_OPEN_ARRAY : 
        return m3_type_fields_equal ( left_direct, right_direct ); 
      case TYPE_CODE_M3_ENUM : 
        return m3_type_fields_equal ( left_direct, right_direct ); 
      case TYPE_CODE_M3_PACKED :
        return m3_type_fields_equal ( left_direct, right_direct ); 
      case TYPE_CODE_M3_SET : 
        return m3_type_fields_equal ( left_direct, right_direct ); 
      case TYPE_CODE_M3_SUBRANGE : 
        if ( ! m3_type_fields_equal ( left_direct, right_direct ) ) 
          { return false; } 
        return 
          TYPE_M3_SUBRANGE_MIN ( left_direct ) 
          == TYPE_M3_SUBRANGE_MIN ( right_direct )  
          && TYPE_M3_SUBRANGE_MAX ( left_direct ) 
             == TYPE_M3_SUBRANGE_MAX ( right_direct );
      case TYPE_CODE_M3_POINTER : 
        if ( ! m3_type_fields_equal ( left_direct, right_direct ) ) 
          { return false; } 
        if ( TYPE_M3_POINTER_TRACED ( left_direct ) 
             != TYPE_M3_POINTER_TRACED ( right_direct ) 
           )
          { return false; } 
        if ( TYPE_M3_POINTER_BRANDED ( left_direct ) 
             != TYPE_M3_POINTER_BRANDED ( right_direct ) 
           )
          { return false; } 
        if ( TYPE_M3_POINTER_BRANDED ( left_direct ) ) 
          { return TYPE_M3_POINTER_BRAND ( left_direct ) 
                   == TYPE_M3_POINTER_BRAND ( right_direct );
          } 
        return true;  
      case TYPE_CODE_M3_OPAQUE : 
        return false; /* Shouldn't happen. */ 

      case TYPE_CODE_M3_RECORD : 
        if ( ! m3_type_fields_equal ( left_direct, right_direct ) ) 
          { return false; } 
        for ( i = 0; i < TYPE_NFIELDS ( left_direct ); i ++ ) 
          { if ( TYPE_FIELD_NAME ( left_direct, i ) 
                 != TYPE_FIELD_NAME ( right_direct, i ) 
               ) 
              { return false; }
            if ( TYPE_M3_REC_FIELD_BITPOS ( left_direct, i ) 
                 != TYPE_M3_REC_FIELD_BITPOS ( right_direct, i ) 
               ) 
              { return false; }
            if ( TYPE_M3_REC_FIELD_BITSIZE ( left_direct, i ) 
                 != TYPE_M3_REC_FIELD_BITSIZE ( right_direct, i ) 
               ) 
              { return false; }
          } 
        return true; 
      case TYPE_CODE_M3_OBJECT : 
        if ( TYPE_M3_OBJ_NFIELDS ( left_direct ) 
             != TYPE_M3_OBJ_NFIELDS ( right_direct) 
           )
          { return false; } 
        if ( TYPE_M3_OBJ_NMETHODS ( left_direct ) 
             != TYPE_M3_OBJ_NMETHODS ( right_direct) 
           )
          { return false; } 
        if ( TYPE_M3_OBJ_TRACED ( left_direct ) 
             != TYPE_M3_OBJ_TRACED ( right_direct ) 
           )
          { return false; } 
        if ( TYPE_M3_OBJ_BRANDED ( left_direct ) 
             != TYPE_M3_OBJ_BRANDED ( right_direct ) 
           )
          { return false; } 
        if ( TYPE_M3_OBJ_BRANDED ( left_direct ) ) 
          { if ( strcmp ( TYPE_M3_OBJ_BRAND ( left_direct ) 
                        , TYPE_M3_OBJ_BRAND ( right_direct )
                        ) 
                 != 0 
               ) 
            { return false; } 
          } 
        for ( i = 0; i < TYPE_M3_OBJ_NMETHODS ( left_direct ); i ++ )
          { if ( strcmp ( TYPE_M3_OBJ_METHOD_NAME ( left_direct, i ), 
                          TYPE_M3_OBJ_METHOD_NAME ( right_direct, i )
                        ) 
                 != 0  
               ) 
              { return false; }
            if ( ! m3_types_equal 
                     ( TYPE_M3_OBJ_METHOD_TYPE ( left_direct, i ), 
                       TYPE_M3_OBJ_METHOD_TYPE ( right_direct, i ) 
                     ) 
               ) 
              { return false; }
          } 
        for ( i = 0; i < TYPE_M3_OBJ_NFIELDS ( left_direct ); i ++ ) 
          { if ( strcmp ( TYPE_M3_OBJ_FIELD_NAME ( left_direct, i ), 
                          TYPE_M3_OBJ_FIELD_NAME ( right_direct, i ) 
                        )
                 != 0 
               ) 
              { return false; }
            if ( ! m3_types_equal 
                     ( TYPE_M3_OBJ_FIELD_TYPE ( left_direct, i ), 
                       TYPE_M3_OBJ_FIELD_TYPE ( right_direct, i ) 
                     ) 
               ) 
              { return false; }
            /* Perhaps it would be safe to assume that if the types
               are really the same, then the compiler will have ensured
               the bit packing is the same too, but then there is no 
               whole-object assignment or comparison, so be paranoid. */ 
            if ( TYPE_M3_OBJ_FIELD_BITPOS ( left_direct, i ) 
                 != TYPE_M3_OBJ_FIELD_BITPOS ( right_direct, i ) 
               ) 
              { return false; }
            if ( TYPE_M3_OBJ_FIELD_BITSIZE ( left_direct, i ) 
                 != TYPE_M3_OBJ_FIELD_BITSIZE ( right_direct, i ) 
               ) 
              { return false; }
          } 
        return m3_types_equal  
                 ( TYPE_M3_OBJ_SUPER ( left_direct ), 
                   TYPE_M3_OBJ_SUPER ( right_direct ) 
                 ); 

      case TYPE_CODE_M3_PROC : 
      case TYPE_CODE_M3_METHOD : 
        if ( TYPE_M3_PROC_NRAISES ( left_direct ) 
             != TYPE_M3_PROC_NRAISES ( right_direct ) 
           )
          { return false; } 
        if ( ! m3_type_fields_equal ( left_direct, right_direct ) ) 
          { return false; } 
        for ( i = 0; i < TYPE_NFIELDS ( left_direct ); i ++ ) 
          { if ( TYPE_FIELD_NAME ( left_direct, i ) 
                 != TYPE_FIELD_NAME ( right_direct, i ) 
               ) 
              { return false; }
          } 
        return true; 

      case TYPE_CODE_M3_ADDRESS : 
      case TYPE_CODE_M3_BOOLEAN : 
      case TYPE_CODE_M3_CHAR : 
      case TYPE_CODE_M3_WIDECHAR : 
      case TYPE_CODE_M3_INTEGER : 
      case TYPE_CODE_M3_CARDINAL : 
      case TYPE_CODE_M3_REFANY : 
      case TYPE_CODE_M3_TRANSIENT_REFANY : 
      case TYPE_CODE_M3_ROOT : 
      case TYPE_CODE_M3_TRANSIENT_ROOT : 
      case TYPE_CODE_M3_UN_ROOT : 
      case TYPE_CODE_M3_MUTEX : 
      case TYPE_CODE_M3_TEXT : 
      case TYPE_CODE_M3_NULL : 
      case TYPE_CODE_M3_VOID : 
        /* There is only one type with each of these code. */ 
        return TYPE_CODE ( left_direct ) == TYPE_CODE ( right_direct ); 
      default : { return false; } 
    } /* switch */ 
} /* m3_types_equal */

/* This system is crafted so that (hopefully), 
   if m3_type_code_tier ( tc1 ) < m3_type_code_tier ( tc2 ), 
   then, loosely, m3_subtype_relation ( tc1, tc2 ) != subtype_super.
   Also, the reference types are all at the high end and ordered 
   such that if tc1 is a reference type code and 
   m3_type_code_tier ( tc1 ) < m3_type_code_tier ( tc2 )
   then loosely, m3_subtype_relation ( tc1, tc2 ) == subtype_sub,
   except for different types with TYPE_CODE_M3_OBJECT. 

   It allows ordering of types by their code for subtype checking, 
   so as to cut down on the number of elements of the cartesion square 
   of type codes that have to be explicitly coded.  Don't try this on 
   TYPE_CODE_M3_INDIRECT, which can't be made to fit such a system. */ 

static int 
m3_type_code_tier ( enum type_code code ) 

  { switch ( code ) 
      { case TYPE_CODE_M3_PACKED :
        case TYPE_CODE_M3_OPAQUE : /* Probably shouldn't happen. */  
          return 0; 
        case TYPE_CODE_M3_SET : 
        case TYPE_CODE_M3_RECORD : 
        case TYPE_CODE_M3_METHOD : 
        case TYPE_CODE_M3_VOID : 
          return 1; 
        case TYPE_CODE_M3_OPEN_ARRAY : 
          return 2; 
        case TYPE_CODE_M3_ARRAY : 
          return 3; 
        case TYPE_CODE_M3_SUBRANGE : 
          return 4; 
        case TYPE_CODE_M3_BOOLEAN : 
        case TYPE_CODE_M3_CHAR : 
        case TYPE_CODE_M3_WIDECHAR : 
        case TYPE_CODE_M3_INTEGER : 
        case TYPE_CODE_M3_CARDINAL : 
        case TYPE_CODE_M3_ENUM : 
          return 5; 
        case TYPE_CODE_M3_PROC : 
        case TYPE_CODE_M3_PROC_CLOSURE :
          return 6; 
        case TYPE_CODE_M3_REFANY : 
        case TYPE_CODE_M3_TRANSIENT_REFANY : 
        case TYPE_CODE_M3_ADDRESS : 
          return 7;  
        case TYPE_CODE_M3_TEXT : 
          return 8; 
        case TYPE_CODE_M3_ROOT : 
        case TYPE_CODE_M3_TRANSIENT_ROOT : 
        case TYPE_CODE_M3_UN_ROOT : 
        case TYPE_CODE_M3_POINTER : 
          return 9; 
        case TYPE_CODE_M3_MUTEX : 
          return 10; 
        case TYPE_CODE_M3_OBJECT : 
          return 11; 
        case TYPE_CODE_M3_NULL : 
          return 12; 
        default : 
          return 1;  
      } /* switch */ 
  } /* m3_type_code_tier */ 

enum subtype_rel 
  { subtype_sub, subtype_equal, subtype_super, subtype_both, subtype_norel };

static enum subtype_rel 
commute_subtype_relation ( enum subtype_rel param_rel ) 

  { switch ( param_rel ) 
      { case subtype_sub : return subtype_super; 
        case subtype_super : return subtype_sub; 
        default: return param_rel; 
      } 
  } /* commute_subtype_relation */ 

/* Does not strip off indirects, unless it's a real value of reference type. */
static struct type * 
m3_allocated_type ( struct value * val ) 

  { struct type * val_type; 
    struct type * direct_type; 
    struct type * result_type; 
    CORE_ADDR val_contents; 

    val_type = value_type ( val ) ; 
    if ( * ( LONGEST * ) value_contents ( val ) == m3_type_magic_value ) 
      { return val_type; } 
    else 
      { direct_type = m3_direct_type ( val_type ); 
        direct_type = m3_revealed_type ( val_type ); 
        switch ( TYPE_CODE ( direct_type ) ) 
          { case TYPE_CODE_M3_REFANY:
            case TYPE_CODE_M3_ROOT:
            case TYPE_CODE_M3_OBJECT:
            case TYPE_CODE_M3_TRANSIENT_ROOT:
            case TYPE_CODE_M3_TRANSIENT_REFANY:
              val_contents = value_as_address ( val ); 
              if ( val_contents == 0 ) 
                { return builtin_type_m3_null; } 
              result_type 
                = m3_allocated_type_from_object_addr ( val_contents ); 
              return result_type; 
            default: 
              return val_type; 
          } 
      } 
  } /* m3_allocated_type*/ 

/* Call this only if left_type is known to be neither indirect nor packed and
   an ordinal type. */ 
static enum subtype_rel
m3_ordinal_subtype_relation ( 
    struct type * left_type, 
    struct type * left_base_type, 
    struct type * right_type
  ) 

  { enum type_code left_base_code; 
    enum type_code right_base_code; 
    struct type * right_base_type;
    LONGEST left_lower; 
    LONGEST left_upper; 
    LONGEST right_lower; 
    LONGEST right_upper; 

    right_type = m3_unpacked_type ( right_type ); 
    switch ( TYPE_CODE ( right_type ) ) 
      { case TYPE_CODE_M3_SUBRANGE : 
          right_base_type = TYPE_M3_SUBRANGE_TARGET ( right_type ); 
        case TYPE_CODE_M3_BOOLEAN : 
        case TYPE_CODE_M3_CHAR : 
        case TYPE_CODE_M3_WIDECHAR : 
        case TYPE_CODE_M3_INTEGER : 
        case TYPE_CODE_M3_CARDINAL : 
        case TYPE_CODE_M3_ENUM : 
          right_base_type = right_type;
        default : 
          return subtype_norel; 
      } 
    left_base_code = TYPE_CODE ( left_base_type ); 
    right_base_code = TYPE_CODE ( right_base_type ); 
    if ( left_base_code != right_base_code ) 
      { return subtype_norel; } 
    if ( left_base_code == TYPE_CODE_M3_ENUM ) 
      { if ( ! m3_type_fields_equal ( left_base_type, right_base_type ) ) 
          { return subtype_norel; } 
      } 
    /* Here, we know the two base types are equal. */   
    if ( left_type == left_base_type && right_type == right_base_type ) 
      { return subtype_equal; } 
    /* Here, at least one is a subrange. */ 
    m3_ordinal_bounds ( left_type , & left_lower, & left_upper );  
    m3_ordinal_bounds ( right_type , & right_lower, & right_upper );  
    if ( left_lower == right_lower && left_upper == right_upper ) 
      /* Bounds are equal. */ 
      { if ( left_type != left_base_type && right_type != right_base_type ) 
          /* Both are subranges. */ 
          { return subtype_equal; } 
        else 
          { return subtype_both; }
      }  
    if ( left_lower <= right_lower && left_upper >= right_upper ) 
      { return subtype_super; } /* Left is supertype. */ 
    if ( left_lower >= right_lower && left_upper <= right_upper ) 
      { return subtype_sub; } /* Left is subtype. */ 
    return subtype_norel; 
  } /* m3_ordinal_subtype_relation */ 

/* FIXME: This has been replaced by m3_types_equal.  Take it out. */ 
static bool
m3_equal_object_types ( struct type * left, struct type * right ) 

  { enum type_code left_code;  
  int i; 

    left_code = TYPE_CODE ( left );  
    switch ( left_code ) 
      { case TYPE_CODE_M3_REFANY : 
        case TYPE_CODE_M3_TRANSIENT_REFANY : 
        case TYPE_CODE_M3_ADDRESS : 
        case TYPE_CODE_M3_ROOT : 
        case TYPE_CODE_M3_TRANSIENT_ROOT : 
        case TYPE_CODE_M3_UN_ROOT : 
        case TYPE_CODE_M3_NULL : 
        case TYPE_CODE_M3_MUTEX : 
        case TYPE_CODE_M3_TEXT : 
          return left_code == TYPE_CODE ( right );  

        case TYPE_CODE_M3_OBJECT : 
          if ( left_code != TYPE_CODE ( right ) ) 
            { return false; } 
          if ( TYPE_M3_OBJ_TRACED ( left ) != TYPE_M3_OBJ_TRACED ( right ) )
            { return false; } 
          if ( TYPE_M3_OBJ_BRANDED ( left ) != TYPE_M3_OBJ_BRANDED ( right ) )
            { return false; } 
          if ( TYPE_M3_OBJ_BRANDED ( left ) 
               && TYPE_M3_OBJ_BRAND ( left ) != TYPE_M3_OBJ_BRAND ( right )
             ) 
            { return false; } 
          if ( TYPE_M3_OBJ_NFIELDS ( left ) != TYPE_M3_OBJ_NFIELDS ( right ) )
            { return false; } 
          if ( TYPE_M3_OBJ_NMETHODS ( left ) != TYPE_M3_OBJ_NMETHODS ( right ) )
            { return false; } 
          for ( i = 0; i < TYPE_M3_OBJ_NMETHODS ( left ); i ++ )
            { if ( strcmp ( TYPE_M3_OBJ_METHOD_NAME ( left, i ), 
                            TYPE_M3_OBJ_METHOD_NAME ( right, i )
                          ) 
                   != 0  
                 ) 
                { return false; }
              if ( ! m3_types_equal 
                       ( TYPE_M3_OBJ_METHOD_TYPE ( left, i ), 
                         TYPE_M3_OBJ_METHOD_TYPE ( right, i ) 
                       ) 
                 ) 
                { return false; }
            } 
          for ( i = 0; i < TYPE_M3_OBJ_NFIELDS ( left ); i ++ ) 
            { if ( strcmp ( TYPE_M3_OBJ_FIELD_NAME ( left, i ), 
                            TYPE_M3_OBJ_FIELD_NAME ( right, i ) 
                          )
                   != 0 
                 ) 
                { return false; }
              if ( ! m3_types_equal 
                       ( TYPE_M3_OBJ_FIELD_TYPE ( left, i ), 
                         TYPE_M3_OBJ_FIELD_TYPE ( right, i ) 
                       ) 
                 ) 
                { return false; }
            } 
          return m3_equal_object_types  
                   ( TYPE_M3_OBJ_SUPER ( left ), TYPE_M3_OBJ_SUPER ( right ) ); 
        default:
          return false; 
      } 

  }  /* m3_equal_object_types */ 

/* Return the number of reference type ancestors start_type has. */ 
static int 
reference_type_depth ( struct type * start_type ) 

{ struct type * l_type; 
  int result; 

  result = 0; 
  l_type = start_type; 
  while ( true ) 
    { if ( l_type == NULL ) 
        { return result; } 
      switch ( TYPE_CODE ( l_type ) )  
        { case TYPE_CODE_M3_REFANY : 
          case TYPE_CODE_M3_TRANSIENT_REFANY : 
          case TYPE_CODE_M3_ADDRESS : 
            return result;
          case TYPE_CODE_M3_ROOT : 
          case TYPE_CODE_M3_TRANSIENT_ROOT : 
          case TYPE_CODE_M3_UN_ROOT : 
            return result + 1;
          case TYPE_CODE_M3_TEXT : 
            return result + ( int ) m3_is_cm3 ( ); 

          case TYPE_CODE_M3_MUTEX : 
            return result + 2; 

          case TYPE_CODE_M3_OBJECT :
            l_type = TYPE_M3_OBJ_SUPER ( l_type ); 
            result ++; 
            break; 
            /* and loop */  

          default: /* Shouldn't happen. */ 
            return result; 
        }
    } /* while */ 
} /* reference_type_depth */ 

/* Is this type TEXT or the PM3 revelation of TEXT, namely 
   BRANDED "Text-1.0" REF ARRAY OF CHAR? 
*/ 
static bool
is_pm3_text_revelation ( struct type * text_type ) 

{ struct type * array_type; 
  struct type * elem_type; 

  if ( text_type == NULL ) { return false; } 
  switch ( TYPE_CODE ( text_type ) ) 
    { case TYPE_CODE_M3_TEXT: 
        return true; 
      case TYPE_CODE_M3_POINTER:
        if ( ! TYPE_M3_POINTER_BRANDED ( text_type ) )  
          { return false; } 
        if ( strcmp ( TYPE_M3_POINTER_BRAND ( text_type ), "Text-1.0" ) != 0 )
          { return false; } 
        array_type = TYPE_M3_POINTER_TARGET ( text_type ); 
        if ( array_type == NULL 
             || TYPE_CODE ( array_type ) != TYPE_CODE_M3_OPEN_ARRAY 
           ) 
          { return false; } 
        elem_type = TYPE_M3_OPEN_ARRAY_ELEM ( array_type ); 
        if ( elem_type == NULL 
             || TYPE_CODE ( elem_type ) != TYPE_CODE_M3_CHAR  
           ) 
          { return false; } 
        return true; 
      default: 
        return false; 
    } 
} /* is_pm3_text_revelation */ 

/* Computes the type-to-type subtype relation. */ 
static enum subtype_rel 
m3_subtype_relation ( struct type * left, struct type * right ) 

  { struct type * left_direct; 
    struct type * right_direct; 
    enum type_code left_code; 
    enum type_code right_code; 
    int left_tier; 
    int right_tier; 
    enum subtype_rel child_rel; 
    LONGEST lower; 
    LONGEST upper; 

    if ( left == NULL || right == NULL ) { return subtype_norel; } 
    left_direct = m3_direct_type ( left ); 
    right_direct = m3_direct_type ( right ); 
    left_direct = m3_revealed_type ( left ); 
    right_direct = m3_revealed_type ( right ); 

    if ( left_direct == right_direct ) { return subtype_equal; } 
    if ( m3_types_equal ( left_direct, right_direct ) ) { return subtype_equal; }
    left_code = TYPE_CODE ( left_direct ); 
    right_code = TYPE_CODE ( right_direct ); 
    /* Swap operands so that if the type codes differ, left will not be the 
       supertype only. */ 
    left_tier = m3_type_code_tier ( left_code );
    right_tier = m3_type_code_tier ( right_code ); 
    if ( left_tier > right_tier ) 
      { return commute_subtype_relation 
                 ( m3_subtype_relation ( right_direct, left_direct ) ); 
      } 

    switch ( left_code ) 
      { case TYPE_CODE_M3_PACKED :
          child_rel 
            = m3_subtype_relation 
                ( TYPE_M3_PACKED_TARGET ( left_direct ) , right_direct ); 
          if ( child_rel == subtype_equal 
               && right_code != TYPE_CODE_M3_PACKED 
             ) 
            { return subtype_both; } 
          else { return child_rel; } 

        case TYPE_CODE_M3_OPAQUE : 
          /* Shouldn't get here. */
          return subtype_norel;  

        case TYPE_CODE_M3_SUBRANGE : 
          return m3_ordinal_subtype_relation 
                   ( left_direct, 
                     TYPE_M3_SUBRANGE_TARGET ( left_direct ), 
                     right_direct 
                   ); 

        case TYPE_CODE_M3_BOOLEAN : 
        case TYPE_CODE_M3_CHAR : 
        case TYPE_CODE_M3_WIDECHAR : 
        case TYPE_CODE_M3_INTEGER : 
        case TYPE_CODE_M3_CARDINAL : 
        case TYPE_CODE_M3_ENUM : 
          return m3_ordinal_subtype_relation 
                   ( left_direct, left_direct, right_direct ); 

        case TYPE_CODE_M3_REFANY : 
        case TYPE_CODE_M3_TRANSIENT_REFANY : 
        case TYPE_CODE_M3_ADDRESS : 
        case TYPE_CODE_M3_ROOT : 
        case TYPE_CODE_M3_TRANSIENT_ROOT : 
        case TYPE_CODE_M3_UN_ROOT : 
        case TYPE_CODE_M3_NULL : 
          if ( left_code == right_code )
            { return subtype_equal; } 
          else if ( left_tier < right_tier ) 
            { return subtype_sub; } 
          else { return subtype_norel; }  

        case TYPE_CODE_M3_POINTER : 
          if ( right_code != left_code ) 
            { return subtype_norel; } 
          else if ( ! m3_type_fields_equal ( left_direct, right_direct ) ) 
            { return subtype_norel; } 
          else if ( TYPE_M3_POINTER_TRACED ( left_direct ) 
                    != TYPE_M3_POINTER_TRACED ( right_direct ) 
                  )
            { return subtype_norel; } 
          else if ( TYPE_M3_POINTER_BRANDED ( left_direct ) 
                    != TYPE_M3_POINTER_BRANDED ( right_direct ) 
                  )
            { return subtype_norel; } 
          else if ( TYPE_M3_POINTER_BRANDED ( left_direct ) 
                    && strcmp ( TYPE_M3_POINTER_BRAND ( left_direct ), 
                                TYPE_M3_POINTER_BRAND ( right_direct )
                              ) 
                       != 0 
                  ) 
            { return subtype_norel; } 
          else { return subtype_equal; } 

        case TYPE_CODE_M3_TEXT : 
          if ( ! m3_is_cm3 ( ) )  
            { if ( left_code == right_code )
                { return subtype_equal; } 
              if ( is_pm3_text_revelation ( left_direct ) ) 
                { return subtype_sub; } 
              else { return subtype_norel; }  
            } 
          /* else for CM3, fall through to object case. */ 

        case TYPE_CODE_M3_MUTEX : 
        case TYPE_CODE_M3_OBJECT : 
          switch ( right_code ) 
            { case TYPE_CODE_M3_NULL: 
                return subtype_sub; 
              case TYPE_CODE_M3_MUTEX : 
              case TYPE_CODE_M3_OBJECT: 
                { int left_depth; 
                  int right_depth; 
                  struct type * left_super; 
                  struct type * right_super; 

                  left_depth = reference_type_depth ( left_direct ); 
                  right_depth = reference_type_depth ( right_direct ); 
                  left_super = left_direct; 
                  right_super = right_direct; 
                  if ( left_depth < right_depth ) 
                    { while ( left_depth < right_depth ) 
                        { right_super = TYPE_M3_OBJ_SUPER ( right_super ); 
                          right_depth --; 
                        } 
                      if ( m3_types_equal ( left_super, right_super ) ) 
                        { return subtype_super; }
                    } 
                  else if ( left_depth > right_depth ) 
                    { while ( left_depth > right_depth ) 
                        { left_super = TYPE_M3_OBJ_SUPER ( left_super ); 
                          left_depth --; 
                        } 
                      if ( m3_types_equal ( left_super, right_super ) ) 
                        { return subtype_sub; }
                    } 
                  else if ( m3_types_equal ( left_super, right_super ) ) 
                    { return subtype_equal; }
                  return subtype_norel;  
                } 
              default: 
                return subtype_norel; 
            } 

        case TYPE_CODE_M3_ARRAY : 
        case TYPE_CODE_M3_OPEN_ARRAY : 
        case TYPE_CODE_M3_PROC : 
        case TYPE_CODE_M3_PROC_CLOSURE :
          error ( "Subtype relation not implemented for array or procedure, "
                  "types." 
                ); /* NORETURN */  

        case TYPE_CODE_M3_SET : 
        case TYPE_CODE_M3_RECORD : 
        case TYPE_CODE_M3_METHOD :
          if ( m3_types_equal ( left_direct, right_direct ) )  
            { return subtype_equal; } 
          else { return subtype_norel; } 

        case TYPE_CODE_M3_VOID : 
          return right_code == left_code; 

        default : { return subtype_norel; } 
      } /* switch ( left_code ) */ 
  } /* m3_subtype_relation */ 

/* Given a block, find the first superblock ancestor that is a function block. */
static struct block * 
m3_proc_block ( struct block * blk ) 

  { struct block * l_block; 

    l_block = blk ; 
    while ( l_block != NULL && BLOCK_FUNCTION ( l_block ) == NULL ) 
      { l_block = BLOCK_SUPERBLOCK ( l_block ) ; } 
    return l_block; 
  } /* m3_proc_block */ 

bool use_static_link = true; 

/* Follow static links, as needed, to get the right frame for target_block. */ 
static struct frame_info *
m3_frame_for_block (struct block *target_block )
{
  struct frame_info *l_frame; 
  struct block * l_frame_proc_block; 
  struct block * l_target_proc_block ; 

  CORE_ADDR start;
  CORE_ADDR end;
  CORE_ADDR calling_pc;

  if ( target_block == NULL) { return NULL; } 
  l_frame = deprecated_safe_get_selected_frame ( ); 
  if ( use_static_link ) 
    { /* Find the first frame in the static chain that has the same 
         procedure-level block as target_block.  In a static chain, 
         there can be only one such frame. 
         It might be nice to count the number of static links followed,
         but the number would have to be computed during expression 
         parsing and stored in the OP_VAR_VALUE node, which would have 
         to be changed to M3_OP_VAR_VALUE. */ 
      l_target_proc_block = m3_proc_block ( target_block ); 
      while ( true ) 
        { l_frame_proc_block 
            = m3_proc_block 
                ( get_frame_block ( l_frame , NULL ) 
                  /* ^ Which may not be the right static ancestor block at all,
                       but it doesn't matter, because we skip to the enclosing 
                       procedure block for this block and target_block. */ 
                ); 
          if ( l_frame_proc_block == NULL ) { return NULL; } 
          if ( l_frame_proc_block == l_target_proc_block ) { return l_frame; } 
          l_frame = m3_static_parent_frame ( l_frame );
        } /* while */
    } /* if */ 
  else /* Do it the old way. */  
    { /* Starting with the selected frame and working outward, find the first
         frame that belongs to block.  This is a crude way to locate non-local
         variables/parameters of statically-enclosing procedures of the selected
         frame's procedure.  If procedures are called as procedure constants,
         this should find the right frame.  If something was called as the
         value of a procedure parameter, it may be wrong.  
      */
      start = BLOCK_START ( target_block );
      end = BLOCK_END ( target_block );
      while ( true )
        { if ( l_frame == NULL ) { return NULL; } 
          calling_pc = get_frame_address_in_block ( l_frame );
          if ( start <= calling_pc && calling_pc < end ) { return l_frame; }
          l_frame = get_prev_frame ( l_frame );
        } /* while */ 
    } /* else */ 
} /* m3_frame_for_block */ 

static struct type * 
m3_ordinal_base_type ( struct type * param_type, bool * is_int_or_card )

  { struct type * result_type; 

    result_type = param_type; 
    while ( true ) 
      { switch ( TYPE_CODE ( result_type ) ) 
          { case TYPE_CODE_M3_INDIRECT :
              result_type = TYPE_M3_INDIRECT_TARGET ( result_type ); 
              break; /* And loop. */  
            case TYPE_CODE_M3_OPAQUE :
              result_type = TYPE_M3_OPAQUE_REVEALED ( result_type ); 
              break; /* And loop. */  
            case TYPE_CODE_M3_PACKED : 
              /* The value should still be in packed form, with bitpos and 
                 bitsize fields set. */ 
              result_type = TYPE_M3_PACKED_TARGET ( result_type );
              break; /* And loop. */  
            case TYPE_CODE_M3_SUBRANGE :
              result_type = TYPE_M3_SUBRANGE_TARGET ( result_type ); 
              break; /* And loop. */  
            case TYPE_CODE_M3_INTEGER : 
            case TYPE_CODE_M3_CARDINAL : 
              if ( is_int_or_card != NULL ) { * is_int_or_card = true; } 
              return result_type;  
            case TYPE_CODE_M3_BOOLEAN : 
            case TYPE_CODE_M3_CHAR : 
            case TYPE_CODE_M3_WIDECHAR : 
            case TYPE_CODE_M3_ENUM : 
              if ( is_int_or_card != NULL ) { * is_int_or_card = false; } 
              return result_type;  
            default: 
              return NULL; 
          } 
      } 
  } /* m3_ordinal_base_type */ 

/* Handle value conversion of an ordinal value for either assignment
   or parameter passing.  Returns an appropriate struct value * if 
   the types are ordinal and everything is OK.  Displays an error (and
   thus doesn't return) if ordinal types are involved but something is
   wrong.  Retuns NULL if ordinal types are irrelevant. */  
static struct value * 
m3_check_and_coerce_ordinal ( 
    struct type * lhs_type,
    struct value * rhs_value,
    struct type * rhs_type,
    char * proc_name,
    char * formal_name 
  ) 

  { struct type * lhs_base_type; 
    struct type * rhs_base_type; 
    struct value * result_value; 
    LONGEST lhs_lower; 
    LONGEST lhs_upper; 
    LONGEST rhs_lower; 
    LONGEST rhs_upper; 
    LONGEST contents; 
    bool lhs_is_int_or_card = false; 
    bool rhs_is_int_or_card = false; 

    lhs_base_type = m3_ordinal_base_type ( lhs_type, & lhs_is_int_or_card ); 
    if ( lhs_base_type == NULL ) { return NULL; } 
    m3_ordinal_bounds ( lhs_type, & lhs_lower, & lhs_upper ); 
    rhs_base_type = m3_ordinal_base_type ( rhs_type, & rhs_is_int_or_card  ); 
    if ( rhs_base_type == NULL ) 
      { if ( proc_name == NULL ) 
          { error 
              ( "Type not assignable to ordinal type." ); /* NORETURN */ 
          } 
        else 
          { error 
              ( "Type not assignable to ordinal type, " 
                " formal \"%s\" of procedure \"%s\", ",
                formal_name, proc_name 
              ); /* NORETURN */ 
          } 
      } 
    m3_ordinal_bounds ( rhs_type, & rhs_lower, & rhs_upper ); 
    if ( lhs_is_int_or_card && rhs_is_int_or_card ) 
      { /* Base types OK */ } 
    else if ( m3_types_equal ( lhs_base_type, rhs_base_type ) )
      { /* Base types OK */ } 
    else 
      { if ( proc_name == NULL ) 
          { error 
              ( "Ordinal types with different base types." ); /* NORETURN */ 
          } 
        else 
          { error 
              ( "Ordinal types with different base types, " 
                " formal \"%s\" of procedure \"%s\", ",
                formal_name, proc_name 
              ); /* NORETURN */ 
          } 
      } 
    contents = m3_value_as_integer ( rhs_value );  
    if ( lhs_lower <= rhs_lower && rhs_upper <= lhs_upper )
      { /* RHS <: LHS. All is well. */ }
    else if ( rhs_upper < lhs_lower || rhs_lower > lhs_upper )  
      { /* Disjoint value sets.  A static error. */ 
        if ( proc_name == NULL ) 
          { error 
              ( "Ordinal types with disjoint ranges." ); /* NORETURN */ 
          } 
        else 
          { error 
              ( "Ordinal types with disjoint ranges, " 
                " formal \"%s\" of procedure \"%s\", ",
                formal_name, proc_name 
              ); /* NORETURN */ 
          } 
      }
    else if ( contents < lhs_lower || lhs_upper < contents ) 
      { /* value not in LHS range. */ 
        if ( proc_name == NULL ) 
          { error 
              ( "Value out of range." ); /* NORETURN */ 
          } 
        else 
          { error 
              ( "Value out of range, " 
                " formal \"%s\" of procedure \"%s\", ",
                formal_name, proc_name 
              ); /* NORETURN */ 
          } 
      }  
    result_value = m3_value_from_longest ( lhs_base_type, contents ); 
    return result_value; 

  } /* m3_check_and_coerce_ordinal */ 

/* Handle value conversion of a reference value for either assignment
   or parameter passing.  Returns an appropriate struct value * if the
   types are reference types and everything is OK.  Displays an error
   (and thus doesn't return) if reference types are involved but
   something is wrong.  Retuns NULL if reference types are
   irrelevant. */  
static struct value * 
m3_check_and_coerce_reference ( 
    struct type * lhs_type,
    struct value * rhs_value,
    struct type * rhs_type,
    char * proc_name,
    char * formal_name 
 ) 

  { enum subtype_rel static_rel; 
    enum subtype_rel allocated_rel; 
    struct type * allocated_type; 
    struct type * lhs_revealed_type = m3_revealed_type ( lhs_type ); 
    struct type * rhs_revealed_type = m3_revealed_type ( rhs_type ); 

    switch ( TYPE_CODE ( lhs_revealed_type ) ) 
      { case TYPE_CODE_M3_OBJECT:
        case TYPE_CODE_M3_POINTER:
        case TYPE_CODE_M3_ADDRESS:
        case TYPE_CODE_M3_REFANY:
        case TYPE_CODE_M3_TRANSIENT_REFANY:
        case TYPE_CODE_M3_ROOT:
        case TYPE_CODE_M3_TRANSIENT_ROOT:
        case TYPE_CODE_M3_UN_ROOT:
        case TYPE_CODE_M3_MUTEX:
        case TYPE_CODE_M3_TEXT:
        case TYPE_CODE_M3_NULL:
          /* LHS is a reference type */ 
          switch ( TYPE_CODE ( rhs_revealed_type ) ) 
            { case TYPE_CODE_M3_OBJECT:
              case TYPE_CODE_M3_POINTER:
              case TYPE_CODE_M3_ADDRESS:
              case TYPE_CODE_M3_REFANY:
              case TYPE_CODE_M3_TRANSIENT_REFANY:
              case TYPE_CODE_M3_ROOT:
              case TYPE_CODE_M3_TRANSIENT_ROOT:
              case TYPE_CODE_M3_UN_ROOT:
              case TYPE_CODE_M3_MUTEX:
              case TYPE_CODE_M3_TEXT:
              case TYPE_CODE_M3_NULL:
                /* RHS is a reference type too. */ 
                static_rel 
                   = m3_subtype_relation 
                       ( lhs_revealed_type, rhs_revealed_type );
                switch ( static_rel ) 
                  { case subtype_norel: 
                      break; 
                    case subtype_equal: 
                    case subtype_super: 
                    case subtype_both: /* Can this happen? */
                      /* Statically legal, no runtime check needed. */ 
                      return rhs_value; 
                    case subtype_sub: 
                      allocated_type = m3_allocated_type ( rhs_value ); 
                      allocated_rel 
                        = m3_subtype_relation 
                            ( lhs_revealed_type, allocated_type );
                      switch ( allocated_rel ) 
                        { case subtype_norel: 
                            break; 
                          case subtype_equal: 
                          case subtype_super: 
                          case subtype_both: /* Can this happen? */
                            /* Passes dynamic check, no coercion needed. */ 
                            return rhs_value; 
                          case subtype_sub: 
                            if ( proc_name == NULL ) 
                              { error ( "NARROW failure." ); /* NORETURN */ }  
                            else 
                              { error 
                                 ( "NARROW failure, " 
                                   "formal \"%s\" of procedure \"%s\"",
                                   formal_name, proc_name 
                                 ); /* NORETURN */
                              }  
                        }  
                  }
              default: 
                break; 
            }  
        default:
          return NULL; 
      } 
    if ( proc_name == NULL ) 
      { error ( "Reference type not assignable." ); /* NORETURN */ }  
    else 
      { error 
         ( "Reference type not assignable, " 
           "formal \"%s\" of procedure \"%s\"",
           formal_name, proc_name 
         ); /* NORETURN */
      }  
  } /* m3_check_and_coerce_reference */ 

/* For a fixed or open array type, traverse all the way to the final 
   (non-array) element type and return the stated info. */
static void              
m3_get_array_info ( struct type * array_type, 
                 int * open_dims, 
                 int * fixed_dims, 
                 struct type * * elem_type
               )

{ struct type * l_type = array_type; 
  int l_open_dims = 0; 
  int l_fixed_dims = 0; 

  if ( l_type != NULL ) 
    { while ( true ) 
        { switch ( TYPE_CODE ( l_type ) ) 
            { case TYPE_CODE_M3_ARRAY: 
                l_fixed_dims ++; 
                l_type = TYPE_M3_ARRAY_ELEM ( l_type );
                break;  
              case TYPE_CODE_M3_OPEN_ARRAY: 
                l_open_dims ++; 
                l_type = TYPE_M3_OPEN_ARRAY_ELEM ( l_type ); 
                break; 
              default: 
                if ( open_dims != NULL ) { * open_dims = l_open_dims; } 
                if ( fixed_dims != NULL ) { * fixed_dims = l_fixed_dims; } 
                if ( elem_type != NULL ) { * elem_type = l_type; } 
                return; 
            } 
        }
    } 
} /* m3_get_array_info */ 

/* Handle value conversion of an array value for either assignment or parameter 
   passing.  If lhs_value is non NULL, its element counts for open dimensions
   are shape-checked against the rhs shape.  Otherwise, open dimensions have
   their element counts set from the rhs shape. Returns NULL if either lhs
   or rhs is not an array type.  Emits error and doesn't return if something
   is wrong.  Otherwise returns a suitable value, which may be equal to
   rhs_vlue, if nothing has to be done, or is constructed dope otherwise. */ 
static struct value * 
m3_check_and_coerce_array ( 
    struct value * lhs_value,
    struct type * lhs_type,
    struct value * rhs_value,
    struct type * rhs_type,
    char * proc_name,
    char * formal_name 
  ) 

  { enum type_code lhs_type_code; 
    enum type_code rhs_type_code; 
    int lhs_open_dims;
    int lhs_fixed_dims;
    int rhs_open_dims;
    int rhs_fixed_dims;
    struct type * lhs_elem_type; 
    struct type * rhs_elem_type; 
    struct value * result;
    CORE_ADDR array_addr; 
    struct type * l_lhs_type = lhs_type; 
    struct type * l_rhs_type = rhs_type; 
    int dimension = 0;
    int lhs_shape_comp;
    int rhs_shape_comp;
    LONGEST lower; 
    LONGEST upper; 

    lhs_type_code = TYPE_CODE ( lhs_type );  
    rhs_type_code = TYPE_CODE ( rhs_type );  
    m3_get_array_info 
      ( rhs_type, 
        & rhs_open_dims, 
        & rhs_fixed_dims, 
        & rhs_elem_type
      );
    m3_get_array_info 
      ( lhs_type, 
        & lhs_open_dims, 
        & lhs_fixed_dims, 
        & lhs_elem_type
      );
    switch ( lhs_type_code ) 
      { case TYPE_CODE_M3_OPEN_ARRAY: 
          switch ( rhs_type_code ) 
            { case TYPE_CODE_M3_OPEN_ARRAY: 
                if ( lhs_value == NULL 
                     && lhs_fixed_dims == 0 
                     && rhs_fixed_dims == 0 
                     && lhs_open_dims == rhs_open_dims 
                   ) 
                  { result = rhs_value; } /* Nothing in rhs_value will change. */
                else 
                  { result = allocate_value ( lhs_type ); 
                    array_addr = m3_value_open_array_elems_addr ( rhs_value );
                    m3_set_value_open_array_elems_addr ( result, array_addr );
                  } 
                break; 
              case TYPE_CODE_M3_ARRAY: 
                if ( VALUE_LVAL ( rhs_value ) != lval_memory ) 
                  { if ( proc_name == NULL ) 
                      { error 
                          ( "Assigning an array constructor to an open array "
                            "is not supported, " 
                          ); /* NORETURN */ 
                      } 
                    else 
                      { error 
                          ( "Passing an array constructor to an open array "
                            "formal is not supported, " 
                            " formal \"%s\" of procedure \"%s\"",
                            formal_name, proc_name 
                          ); /* NORETURN */ 
                      } 
                  } 
                result = allocate_value ( lhs_type ); 
                array_addr 
                  = VALUE_ADDRESS ( rhs_value ) + value_offset ( rhs_value ); 
                m3_set_value_open_array_elems_addr ( result, array_addr );
                break; 
              default: 
                if ( proc_name == NULL ) 
                  { error ( "Non-assignable types." ); /* NORETURN */ } 
                else 
                  { error 
                      ( "Non-assignable types, " 
                        " formal \"%s\" of procedure \"%s\"",
                        formal_name, proc_name 
                      ); /* NORETURN */ 
                  } 
            } 
          break; 
        case TYPE_CODE_M3_ARRAY: 
          switch ( rhs_type_code ) 
            { case TYPE_CODE_M3_OPEN_ARRAY: 
                result = allocate_value ( lhs_type ); 
                array_addr = m3_value_open_array_elems_addr ( rhs_value );
                VALUE_ADDRESS ( result ) = array_addr; 
                set_value_lazy ( result, 1 );
                break; 
              case TYPE_CODE_M3_ARRAY: 
                if ( lhs_value == NULL ) 
                  { result = rhs_value; } /* Nothing in rhs_value will change. */
                else 
                  { result = allocate_value ( lhs_type ); 
                    array_addr = VALUE_ADDRESS ( rhs_value ); 
                    VALUE_ADDRESS ( result ) = array_addr; 
                    set_value_offset ( result, value_offset ( rhs_value ) ); 
                    set_value_lazy ( result, 1 );
                  } 
                break; 
              default: 
                if ( proc_name == NULL ) 
                  { error ( "Non-assignable types." ); /* NORETURN */ } 
                else 
                  { error 
                      ( "Non-assignable types, " 
                        " formal \"%s\" of procedure \"%s\"",
                        formal_name, proc_name 
                      ); /* NORETURN */ 
                  } 
            } 
          break; 
        default: 
          return NULL; 
      } 
    if ( lhs_open_dims + lhs_fixed_dims != rhs_open_dims + rhs_fixed_dims ) 
      { if ( proc_name == NULL ) 
          { error 
              ( "Unequal array dimensions, %d, %d",
                lhs_open_dims + lhs_fixed_dims,
                rhs_open_dims + rhs_fixed_dims
              ); /* NORETURN */ 
          } 
        else 
          { error 
              ( "Unequal array dimensions, %d, %d, "
                "formal \"%s\" of procedure \"%s\"",
                lhs_open_dims + lhs_fixed_dims,
                rhs_open_dims + rhs_fixed_dims,
                formal_name, proc_name 
              ); /* NORETURN */ 
          }
      }  
    if ( ! m3_types_equal ( lhs_elem_type, rhs_elem_type ) ) 
      { if ( proc_name == NULL ) 
          { error ( "Unequal array element types." ); /* NORETURN */ }  
        else 
          { error 
              ( "Unequal array element types,"
                " formal \"%s\" of procedure \"%s\"",
                formal_name, proc_name 
              ); /* NORETURN */
          }  
      } 
    if ( lhs_type_code == TYPE_CODE_M3_OPEN_ARRAY 
         && rhs_type_code == TYPE_CODE_M3_OPEN_ARRAY 
         && result == rhs_value
       ) 
      /* Reusing dope. No check or change to shape required. */ 
      { return result; } 
    while ( true ) 
      { switch ( TYPE_CODE ( l_lhs_type ) ) 
          { case TYPE_CODE_M3_OPEN_ARRAY: 
              switch ( TYPE_CODE ( l_rhs_type ) ) 
                { case TYPE_CODE_M3_OPEN_ARRAY: 
                    rhs_shape_comp 
                      = m3_value_open_array_shape_component 
                          ( rhs_value, dimension );
                    l_rhs_type = TYPE_M3_OPEN_ARRAY_ELEM ( l_rhs_type ); 
                    break; 
                  case TYPE_CODE_M3_ARRAY: 
                    m3_ordinal_bounds 
                      ( TYPE_M3_ARRAY_INDEX ( l_rhs_type ), 
                        & lower, & upper 
                      ); 
                    rhs_shape_comp = upper - lower + 1; 
                    l_rhs_type = TYPE_M3_ARRAY_ELEM ( l_rhs_type ); 
                    break; 
                } 
              if ( lhs_value != NULL ) 
                { lhs_shape_comp 
                    = m3_value_open_array_shape_component 
                        ( lhs_value, dimension );
                  if ( lhs_shape_comp != rhs_shape_comp ) 
                    { if ( proc_name == NULL ) 
                        { error 
                            ( "Shape check failure." 
                              "dimension %d, lhs %d, rhs %d.",
                               dimension, lhs_shape_comp, rhs_shape_comp
                            ); /* NORETURN */ 
                        } 
                      else 
                        { error 
                            ( "Shape check failure." 
                              " formal \"%s\" of procedure \"%s\", "
                              "dimension %d, lhs %d, rhs %d.",
                              formal_name, proc_name, 
                              dimension, lhs_shape_comp, rhs_shape_comp
                            ); /* NORETURN */ 
                        } 
                    } 
                } 
              m3_set_value_open_array_shape_component 
                ( result, dimension, rhs_shape_comp );  
              l_lhs_type = TYPE_M3_OPEN_ARRAY_ELEM ( l_lhs_type ); 
              break; 
            case TYPE_CODE_M3_ARRAY: 
              m3_ordinal_bounds 
                ( TYPE_M3_ARRAY_INDEX ( l_lhs_type ), 
                  & lower, & upper 
                ); 
              lhs_shape_comp = upper - lower + 1; 
              switch ( TYPE_CODE ( l_rhs_type ) ) 
                { case TYPE_CODE_M3_OPEN_ARRAY: 
                    rhs_shape_comp 
                      = m3_value_open_array_shape_component 
                          ( rhs_value, dimension );
                    l_rhs_type = TYPE_M3_OPEN_ARRAY_ELEM ( l_rhs_type ); 
                    break; 
                  case TYPE_CODE_M3_ARRAY: 
                    m3_ordinal_bounds 
                      ( TYPE_M3_ARRAY_INDEX ( l_rhs_type ), 
                        & lower, & upper 
                      ); 
                    rhs_shape_comp = upper - lower + 1; 
                    l_rhs_type = TYPE_M3_ARRAY_ELEM ( l_rhs_type ); 
                    break; 
                } 
              if ( lhs_shape_comp != rhs_shape_comp ) 
                { if ( proc_name == NULL ) 
                    { error 
                        ( "Shape check failure." 
                          "dimension %d, lhs %d, rhs %d.",
                           dimension, lhs_shape_comp, rhs_shape_comp
                        ); /* NORETURN */ 
                    } 
                  else 
                    { error 
                        ( "Shape check failure." 
                          " formal \"%s\" of procedure \"%s\", "
                          "dimension %d, lhs %d, rhs %d.",
                          formal_name, proc_name, 
                          dimension, lhs_shape_comp, rhs_shape_comp
                        ); /* NORETURN */ 
                    } 
                } 
              l_lhs_type = TYPE_M3_ARRAY_ELEM ( l_lhs_type ); 
              break; 
            default: /* A non-array type, this is the ultimate element type. */  
             /* We just built the dope for this open array parameter
                and it is located only in gdb process space.  A copy
                of the dope has to be pushed on the inferior stack,
                then the copy's address passed as the parameter, all
                of which has to be done inside call_function_by_hand,
                after it has done various stack-aligning, etc.
                call_function_by_hand can distinguish this case by
                TYPE_CODE_M3_OPEN_ARRAY, which will not occur
                otherwise.  (If the dope is already in inferior space,
                the parameter value will be for a pointer to the dope
                and have TYPE_CODE_M3_INDIRECT.) */ 
              return result; 
          } 
        dimension ++;
      } 
  } /* m3_check_and_coerce_array */ 

/* The Modula-3 compilers do a poor job of giving enough info to figure out if 
   we have a function procedure and if so, what its result type is.  Here is a 
   hare-brained scheme, worked out experimentally, for inferring it.  
   If it's a function and the result type is "small", there will be a local
   variable named "_result" whose type is what we want.  If the result type is
   "big" (i.e., compiled as if it were a VAR parameter), there will be a formal
   parameter, of the same name, whose type is that of a VAR parameter of the
   result type.  
   In order to do this, We have to have the symbol for the actual procedure 
   constant.  This will have a type whose code is TYPE_CODE_FUNC, and whose 
   "fields" have been built from the separate N_PSYM stab entries that follow
   the N_FUN stab entry for the procedure.  It will have a constructed result
   type that is almost no help, except when it has a builtin type.  However,
   its block will have entries for the formals, also constructed from N_PSYM
   entries, and locals, constructed from N_LSYM entries.  
   The type of "_result" is what is returned.  .    
   Procedure type stabs entries for procedure variables and methods are produced
   a little differently by the compilers, and don't contain enough info at all.  
   Fortunately, for evaluating a user-typed call, we can get the runtime address 
   of the procedure constant first, then get the symbol from that. 
   It is not clear that the type of "_result" will be available when the 
   procedure constant symbol is constructed, so we delay this lookup until 
   a user-typed call is being evaluated.  
   Note that Modula-3 identifiers must begin with a letter, so the compiler-
   generated name "_result" can't be spoofed by the Modula-3 programmer. 
   This function patches the result type of the procedure constant symbol
   and also returns its result type.  result_is_ref_param  (which may be NULL) 
   is set to true iff it is a result that is, at machine-code level, 
   implemented as a  VAR parameter. 
*/
static struct type * 
m3_patched_proc_result_type ( 
  CORE_ADDR code_addr, char * name, bool * result_is_ref_param )

{ struct symbol * proc_sym; 
  struct block * proc_block;
  struct symbol * result_sym; 
  struct type * result_type;  
  bool l_result_is_ref_param; 

  if ( result_is_ref_param != NULL ) { * result_is_ref_param = false; } 
  proc_sym = find_pc_function ( code_addr ); 
  if ( proc_sym == NULL ) 
    { error ( "Can't get symbol for procedure \"%s\".", name ); /* NORETURN */ }
  proc_block = SYMBOL_BLOCK_VALUE ( proc_sym );
  result_sym = lookup_block_symbol ( proc_block, "_result", NULL, VAR_DOMAIN ); 
  if ( result_sym == NULL ) /* No result type => proper procedure. */ 
    { l_result_is_ref_param = false; 
      result_type = builtin_type_m3_void; 
    } 
  else 
    { l_result_is_ref_param = ( SYMBOL_CLASS ( result_sym ) == LOC_ARG ); 
      result_type = SYMBOL_TYPE ( result_sym ); 
      gdb_assert  
        ( l_result_is_ref_param 
          == ( TYPE_CODE ( result_type ) == TYPE_CODE_M3_INDIRECT )
          /* We will need this inside call_fuction_by_hand to tell if result
                    is passed by reference. */ 
        ); 
    } 
  if ( result_is_ref_param != NULL ) 
    { * result_is_ref_param = l_result_is_ref_param; } 
  /* Patch the procedure type's result type, which will be used elsewhere to
     detect that this is a function whose result is actually a ref parameter. */ 
  TYPE_TARGET_TYPE ( SYMBOL_TYPE ( proc_sym ) ) = result_type; 
  return result_type; 
} /* m3_patched_proc_result_type */

/* NOTE: on Modula-3 dynamic values of procedure types. 

   All the compilers represent dynamic procedure values as a pointer.
   This points to the beginning of the procedure code, iff it is a top-level
   procedure.  If it is a local (i.e. nested) procedure, it points to a
   "closure".  A closure is a record with three words.  The first is a "mark",
   a word that has all bits set.  You can tell whether you have a pointer to
   the code of a top-level procedure or a closure by checking for this value.
   The obvious assumption is that this bit pattern will never occur as machine
   code in any of the targets.  The remaining two words are the address of the
   procedure's target machine code and its environment pointer. 

   In gdb, we handle closures in two ways.  Procedure values that are stored
   in the inferior program are represented in gdb space as struct value 
   objects  whose value is the pointer (to either kind), and whose type has 
   TYPE_CODE_M3_PROC.  In evaluating user-typed expressions and statements,
   gdb checks which kind it is when:
     1) Trying to assign it to a variable.  Only top-level is legal.
     2) Calling the denoted procedure, which requires different call
        mechanisms for the two cases.  
     3) Printing its value, which also has to be done differently.
   When passing it as a parameter, which kind it is does not matter.  The
   pointer itself is just passed. 

   gdb also constructs closures in gdb space that don't exist in inferior
   space.  These are represented by a struct value node whose value is the
   entire 3-word closure, and whose type has TYPE_CODE_M3_PROC_CLOSURE and
   whose TYPE_TARGET_TYPE is the type of the procedure constant (which has
   TYPE_CODE_FUNC).  These have a relatively short lifetime.  This is just 
   a trick to get extra information into call_function_by_hand and several 
   other functions it can call, including many target-dependent functions,
   without changing their parameter lists.  These are built: 
     1) When a user-typed call on a nested procedure constant is evaluated.
        The closure is built and passed in to call_function_by_hand as the
        function to be called.  It is then recognized and used by m3-dependent  
        code to pass the static link.  
     2) When a user_typed call that passes a nested procedure constant as
        an actual parameter is evaluated.  It is later used by m3-dependent 
        code to push a copy of the closure onto the inferior stack and then 
        pass a pointer to this closure.
*/ 

/* If proc_const_value is a value for a procedure constant that is nested,
   return a value for a closure for that procedure, using the current, 
   user-selected frame to construct its environment pointer. 
   Nested or not, if inf_code_addr_result is non-NULL, set it to the 
   inferior code address of the procedure. */
static struct value *
m3_nested_proc_const_closure ( 
    struct value * proc_const_value, 
    CORE_ADDR * inf_code_addr_result
  ) 

{ struct type * proc_type; 
  struct block * callee_block; 
  struct block * callee_pred_block; 
  struct frame_info * static_link_frame; 
  struct value * result; 
  CORE_ADDR inf_code_addr; 
  CORE_ADDR static_link; 
  
  if ( proc_const_value == NULL ) { return NULL; } 
  proc_type = value_type ( proc_const_value ); 
  if ( proc_type == NULL || TYPE_CODE ( proc_type ) != TYPE_CODE_FUNC ) 
    { return proc_const_value; }
  /* Types with TYPE_CODE_FUNC are constructed with length of 1 byte, but
     Modula-3 calls will use these as the type of a (procedure) parameter to be
     passed, whose size must be the size of a pointer. */ 
  TYPE_LENGTH ( proc_type ) = TARGET_PTR_BIT / TARGET_CHAR_BIT; 
  inf_code_addr 
    = VALUE_ADDRESS ( proc_const_value ) + value_offset ( proc_const_value ); 
  if ( inf_code_addr == 0 ) { return proc_const_value; } 
  callee_block = block_for_pc ( inf_code_addr );
  callee_pred_block = m3_proc_block ( BLOCK_SUPERBLOCK ( callee_block ) );
  if ( callee_pred_block == NULL ) /* Not nested. */
    { result = proc_const_value; } 
  else /* Nested procedure. */  
    { static_link_frame = m3_frame_for_block ( callee_pred_block );  
      static_link = get_frame_base_address ( static_link_frame );  
      result  
        = m3_build_gdb_proc_closure 
            ( proc_type, inf_code_addr, static_link ); 
    } 
  if ( inf_code_addr_result != NULL ) { * inf_code_addr_result = inf_code_addr; }
  return result; 
} /* m3_nested_proc_const_closure */  

/* This handles both assignment and parameter passing of procedure types. 
   Returns NULL if at least one of the types is not a procedure type.  
   For it to do something significant, lhs_type needs to have TYPE_CODE_M3_PROC 
   and rhs_type needs to have TYPE_CODE_FUNC. */ 
static struct value * 
m3_check_and_coerce_proc (
    struct type * lhs_type,
    struct value * rhs_value,
    struct type * rhs_type,
    char * proc_name,
    char * formal_name 
  ) 

  { struct value * result_val; 
    struct value * result_val_2;  
    CORE_ADDR inf_code_addr; 

    if ( TYPE_CODE ( lhs_type ) != TYPE_CODE_M3_PROC ) { return NULL; } 
    if ( TYPE_CODE ( rhs_type ) != TYPE_CODE_FUNC ) { return NULL; }  
    /* FIXME: Do type check of two similar procedure types. */ 
    if ( false )
      { if ( proc_name == NULL )  
          { error ( "Procedure type not assignable." ); /* NORETURN */ } 
        else 
          { error 
              ( "Actual parameter type not assignable to procedure "
                " formal \"%s\" of procedure \"%s\"" 
                , formal_name, proc_name
              ); /* NORETURN */ 
          } 
      } 
    result_val 
      = m3_nested_proc_const_closure 
          ( rhs_value, & inf_code_addr /* unused. */ ); 
    if ( TYPE_CODE ( value_type ( result_val ) ) == TYPE_CODE_FUNC ) 
      /* Convert (top-level) procedure constant to a variable. */
      { result_val_2 = allocate_value ( lhs_type ); 
        store_unsigned_integer 
          ( value_contents_raw ( result_val_2 ), 
            TYPE_LENGTH ( lhs_type ), 
            VALUE_ADDRESS ( result_val ) + value_offset ( result_val )
          );
        return result_val_2;  
      } 
    else { return result_val; } 
  } /* m3_check_and_coerce_proc */ 

static struct value * 
m3_check_and_coerce_actual ( 
    struct type * formal_type, 
    struct value * actual_value,   
    char * proc_name,
    char * formal_name 
  ) 

{ struct type * actual_type; 
  struct type * actual_direct_type;
  struct type * formal_direct_type;
  struct value * actual_direct_value;
  struct value * result_value;
  struct value * result_direct_value;

  if ( actual_value == NULL ) { return NULL; } 
  actual_type = value_type ( actual_value );  
  if ( actual_type == NULL || formal_type == NULL ) { return actual_value; } 
  
  if ( TYPE_CODE ( actual_type ) == TYPE_CODE_M3_INDIRECT )
    { actual_direct_type = TYPE_M3_INDIRECT_TARGET ( actual_type ); 
      actual_direct_value 
         = value_at_lazy 
             ( actual_direct_type , value_as_address ( actual_value ) );
    } 
  else 
    { actual_direct_type = actual_type; 
      actual_direct_value = actual_value; 
    } 
  if ( TYPE_CODE ( formal_type ) == TYPE_CODE_M3_INDIRECT ) 
    /* Passed by reference.  This includes all VAR and READONLY parameters and
       open array formals of any mode.  

       For READONLY of an ordinal type where the actual has a
       non-equal but assignable type, compiler-generated code makes a
       copy at the call site.  We can't distinguish READONLY from VAR
       in the stabs info, so we just assume it's VAR and refuse
       non-equal type actuals.

       For a VALUE mode open array parameter, a copy is made inside the called
       procedure, so we don't have to worry about doing this or even about
       what mode the parameter has. 

       For parameters of procedure type, VAR and READONLY are the same, since
       values of these types are effectively pointers to immutable objects
       and no copies ever have to be made. 

       For other type categories, there are no assignable but non-identical
       types, and the semantics of READONLY and VAR are always the same.

    */ 
    { formal_direct_type = TYPE_M3_INDIRECT_TARGET ( formal_type ); 
      result_direct_value = m3_check_and_coerce_array 
        ( /* formal_value */ NULL, 
          formal_direct_type, 
          actual_direct_value, 
          actual_direct_type, 
          proc_name, 
          formal_name 
        );  
      if ( result_direct_value == NULL ) 
        { /* Something other than an array type. */ 
          if ( ! m3_types_equal ( formal_direct_type, actual_direct_type ) ) 
            { error 
                ( "Actual parameter type not equal for VAR formal \"%s\" "
                  "of procedure \"%s\"", 
                  formal_name, proc_name
                ); /* NORETURN */ 
            } 
          if ( actual_direct_value == actual_value 
               && ! VALUE_LVAL ( actual_direct_value ) 
             ) 
            { error 
                ( "Actual parameter is not a designator for VAR formal \"%s\" "
                  "of procedure \"%s\".", 
                  formal_name, proc_name
                ); /* NORETURN */ 
            } 
          result_direct_value = actual_direct_value; 
        } 
      if ( result_direct_value == actual_direct_value 
           && TYPE_CODE ( actual_type ) == TYPE_CODE_M3_INDIRECT 
         )
        { return actual_value; } 
      if ( result_direct_value != actual_direct_value 
           && TYPE_CODE ( value_type ( result_direct_value ) ) 
              == TYPE_CODE_M3_OPEN_ARRAY 
         )
        { /* We have an open array formal, whose dope is always passed by 
             reference, but result_direct_value will be array dope that 
             exists only in gdb process space, so for now, it has no 
             inferior address yet.  Taking its address will be done in 
             m3_push_aux_param_data, after it pushes a copy of the dope 
             on the inferior stack.  For now, we leave the actual value 
             having TYPE_CODE_M3_OPEN_ARRAY, which is the clue to 
             m3_push_aux_param_data that it should do that. */
          return result_direct_value; 
        }  

      /* Remaining cases ust take address of coerced actual. */ 
      result_value = value_from_pointer 
        ( m3_indirect_type_from_type ( value_type ( result_direct_value ) ),
          VALUE_ADDRESS ( result_direct_value )
          + value_offset ( result_direct_value )
          + value_embedded_offset ( result_direct_value )
        );
      return result_value; 
    }
  else /* VALUE mode (passed by value.) */ 
    { formal_direct_type = formal_type; 
      if ( m3_types_equal ( formal_type, actual_direct_type ) ) 
        { return actual_direct_value; } 

      result_direct_value = m3_check_and_coerce_array 
        ( /* formal_value */ NULL, formal_direct_type, actual_direct_value, 
          actual_direct_type, proc_name, formal_name 
        );  
      if ( result_direct_value != NULL ) { return result_direct_value; }  

      result_direct_value = m3_check_and_coerce_proc 
        ( formal_direct_type, actual_direct_value, actual_direct_type, 
          proc_name, formal_name 
        );  
      if ( result_direct_value != NULL ) { return result_direct_value; }  

      result_direct_value = m3_check_and_coerce_ordinal 
        ( formal_direct_type, actual_direct_value, 
          actual_direct_type, proc_name, formal_name 
        );  
      if ( result_direct_value != NULL ) { return result_direct_value; }  

      result_direct_value = m3_check_and_coerce_reference
        ( formal_direct_type, actual_direct_value, 
          actual_direct_type, proc_name, formal_name 
        );  
      if ( result_direct_value != NULL ) { return result_direct_value; }  

      error 
        ( "Actual parameter type not assignable to formal \"%s\" "
          "of procedure \"%s\"" 
          , formal_name, proc_name
        ); /* NORETURN */
    } 
  return NULL; /* Suppress warnings.  Shouldn't get here. */ 
} /* m3_check_and_coerce_actual */ 

static struct value *
m3_evaluate_call ( 
    struct value * proc_const_value,  /* Procedure constant to be called.  Should
                                         have a type with TYPE_CODE_FUNC. */
    struct type * check_actuals_type, /* Use for checking/coercing actuals. Can
                                         be either TYPE_CODE_M3_PROC or 
                                         TYPE_CODE_FUNC. */ 
    CORE_ADDR code_addr,              /* Inferior address of procedure code. */ 
    struct value * self,          /* If a method call, the self expression. */
    int nargs,                    /* Number actually typed by user. */ 
    char * name, 
    struct expression *exp, 
    int *pos,
    enum noside noside
  )

  { struct type * result_type; 
    bool result_is_ref_param; 
    struct value * * argvec; 
    int downward_nargs; /* number in argvec, passed at machine level. */ 
    int expected_args; 
    int next_argvec; 
    int next_formal; 
    int next_actual; /* */  

    next_formal = 0; 
    expected_args = TYPE_NFIELDS ( check_actuals_type ); 
    if ( TYPE_CODE ( check_actuals_type ) == TYPE_CODE_M3_PROC ) 
      /* First "formal" in fields is actually result type, possibly void. */ 
      { next_formal ++; 
        expected_args --;
      } 
    result_type 
      = m3_patched_proc_result_type ( code_addr, name, & result_is_ref_param ); 
    if ( result_type != NULL 
         && TYPE_CODE ( result_type ) == TYPE_CODE_M3_INDIRECT 
         && TYPE_CODE ( check_actuals_type ) == TYPE_CODE_FUNC 
       ) /* Last "formal" of check_actuals_type is really a large function 
            result, passed by reference. */ 
      { expected_args --; } 
    if ( nargs != expected_args ) 
      { error 
         ( "Procedure %s requires %d parameters, but %d were supplied.", 
           name,
           expected_args, 
           nargs  
         ); /* NORETURN */ 
      }
    downward_nargs = nargs + ( self != NULL ) + ( result_is_ref_param );
    argvec 
      = ( struct value * * ) 
        alloca ( sizeof ( struct value * ) * ( downward_nargs + 1 ) );
    next_argvec = 0; 
    if ( self != NULL ) /* This is a method call. */ 
      { argvec [ 0 ] = self; 
        next_argvec ++; 
      } 
    for ( next_actual = 0; next_actual < nargs; next_actual ++ )  
      { argvec [ next_argvec ] = m3_evaluate_subexp ( NULL, exp, pos, noside );
        argvec [ next_argvec ] 
          = m3_check_and_coerce_actual 
              ( TYPE_M3_FIELD_TYPE ( check_actuals_type , next_formal ),
                argvec [ next_argvec ], 
                name,
                TYPE_FIELD_NAME ( check_actuals_type , next_formal )
              ); 
        next_formal ++; 
        next_argvec ++; 
      } 
    if ( result_is_ref_param ) 
      { argvec [ next_argvec ] 
          = allocate_value ( result_type /* has TYPE_CODE_M3_INDIRECT */ ); 
      /* Setting value_contents_raw ( argvec [ i ])  will have to wait until the
         space for the result is pushed on the inferior stack, which happens 
         inside call_function_by_hand. */ 
        set_value_lazy ( argvec [ next_argvec ], 0 ); 
        next_argvec ++;
      } 
    argvec [ next_argvec ] = NULL; 
    return call_function_by_hand ( proc_const_value, downward_nargs, argvec ); 
  } /* m3_evaluate_call */ 

/* Construct and return a new a struct value node for a field (or method),
   with parent record/object value parent_val, located at bitsize and bit
   pos, and having type field_type. */ 
static struct value * 
m3_field_value ( 
    struct value * parent_val,
    int bitsize,
    int bitpos,
    struct type * field_type
  ) 

  { struct value * result_val; 

    result_val = allocate_value ( field_type );
    VALUE_LVAL ( result_val ) = 1;
    VALUE_ADDRESS ( result_val ) = VALUE_ADDRESS ( parent_val );
    set_value_offset 
      ( result_val, 
        value_offset ( parent_val ) + bitpos / TARGET_CHAR_BIT 
      ); 
    set_value_bitpos ( result_val, bitpos % TARGET_CHAR_BIT ) ; 
#if 0 /* But we never let these get anywhere but to m3_specific code. */ 
    if ( bitpos % TARGET_CHAR_BIT == 0 
         && TYPE_LENGTH ( field_type ) == bitpos / TARGET_CHAR_BIT ) 
      /* Code elsewhere in gdb takes nonzero value_bitsize to mean 
         non-byte-aligned.  When value_bitsize == 0, it uses the size
         (in bytes) from the type. */ 
      { set_value_bitsize ( result_val, 0 ); } 
      else
#endif     
      { set_value_bitsize ( result_val, bitsize ); } 
    set_value_lazy ( result_val, 1 );
    return result_val; 
  } /* m3_field_value */ 

static struct value * 
m3_evaluate_enum_const ( 
    struct expression *exp, 
    int *pos,
    enum noside noside   
  )

{ int pc; 
  struct symbol * sym; 
  int field_name_len; 
  char * field_name; 
  struct value * result_value; 
  struct type * lhs_type; 
  ULONGEST i; 
  int nconsts; 
  
  pc = * pos; 
  sym = exp -> elts [ pc + 1 ] . symbol; 
  lhs_type = exp -> elts [ pc + 2 ] . type; 
  field_name_len = longest_to_int ( exp -> elts [ pc + 3 ] . longconst );  
  field_name = & exp -> elts [ pc + 4 ] . string;  
  * pos += 6 + BYTES_TO_EXP_ELEM ( field_name_len + 1 ); 
  nconsts = TYPE_M3_ENUM_NVALS ( lhs_type ); 
  i = 0; 
  while ( true ) 
    { if ( i >= nconsts ) 
        { error 
            ( "Enumeration type \"%s\" has no value named \"%s\".", 
              SYMBOL_NATURAL_NAME ( sym ) + 2 /* Strip off the "B$" */ , 
              field_name
            ); /* NORETURN */ 
        } 
      if ( strcmp ( TYPE_FIELD_NAME ( lhs_type, i ) , field_name ) == 0 ) 
        { result_value = m3_value_from_longest ( lhs_type, i ); 
          return result_value; 
        } 
      else { i ++; } 
    } 
} /* m3_evaluate_enum_const */ 

/* Evaluate a dot construct.  If the caller is evaluating a call construct
   (and uses this for its left operand), and the dot construct turns out
   to denote a method of an object value, the caller will need values for
   both the dot construct itself (what to call) and the left operand of the
   dot (to pass as the first, ('self') parameter.  The former value is returned
   as the function result.  If this is a method, 'self' will be set to the latter
   value.  Otherwise, 'self' will be set to null. */ 
/* NOTE: As of 2006-6-5, m3_parse_e8, in m3-exp.c, during expression parsing,
         handles dot-constructs that are merely qualified references to 
         declared entities.  So This only needs to handle access to fields
         and methods of record and object values. 
*/ 
static struct value *
m3_evaluate_dot ( 
    struct expression *exp, 
    int *pos,
    enum noside noside,  
    struct value ** self, 
    char * * out_field_name
  )

{ int pc; 
  int field_name_len; 
  char * field_name; 
  struct value * lhs_value; 
  struct value * dot_value; 
  struct type * lhs_type; 
  struct type * dot_type; 
  struct type * supertype; 
  struct type * allocated_type; 
  CORE_ADDR allocated_tc_addr; 
  CORE_ADDR supertype_tc_addr; 
  CORE_ADDR lhs_inf_addr; 
  int bitsize;  
  int bitpos; 

  if ( self != NULL ) { *self = NULL; } 
  if ( out_field_name != NULL ) { *out_field_name = NULL; } 
  pc = * pos; 
  field_name_len = longest_to_int ( exp -> elts [ pc + 1 ] . longconst );  
  field_name = &exp -> elts [ pc + 2 ] . string;  
  *pos += 4 + BYTES_TO_EXP_ELEM ( field_name_len + 1 ); 
  lhs_value = m3_evaluate_subexp ( NULL_TYPE, exp, pos, noside ); 
  lhs_type = value_type ( lhs_value ); 
  while ( TYPE_CODE ( lhs_type ) == TYPE_CODE_M3_INDIRECT  
          || ( TYPE_CODE ( lhs_type ) == TYPE_CODE_M3_POINTER 
               && TYPE_CODE ( TYPE_M3_TARGET ( lhs_type ) ) 
                  == TYPE_CODE_M3_RECORD  
             ) 
        ) 
    { lhs_inf_addr = value_as_address ( lhs_value ); 
      if ( lhs_inf_addr == 0 ) 
        { error 
            ( "Attempt to implicitly dereference NIL, for record field \"%s\".",
              field_name
            ); /* NORETURN */ 
        } 
      lhs_value = value_at_lazy ( TYPE_M3_TARGET ( lhs_type ), lhs_inf_addr ); 
      VALUE_LVAL ( lhs_value ) = 1;
      lhs_type = value_type ( lhs_value ); 
    } /* while */
  switch ( TYPE_CODE ( lhs_type ) )  
    { case TYPE_CODE_M3_RECORD: 
        if ( m3_find_rec_field 
               ( lhs_type, field_name, & bitsize, & bitpos, & dot_type ) 
           ) 
          { dot_value = m3_field_value ( lhs_value, bitsize, bitpos, dot_type ); 
            if ( out_field_name != NULL ) 
              { *out_field_name = field_name; } 
             return dot_value; 
          } 
        else 
          { error 
              ( "Record has no field named \"%s\".", field_name ); /* NORETURN */
          } 
        break; 

      case TYPE_CODE_M3_REFANY:
      case TYPE_CODE_M3_ROOT:
      case TYPE_CODE_M3_TRANSIENT_REFANY:
      case TYPE_CODE_M3_TRANSIENT_ROOT: 
      case TYPE_CODE_M3_OBJECT:
        lhs_inf_addr = value_as_address ( lhs_value ); 
        if ( lhs_inf_addr == 0 ) 
          { error 
              ( "NIL object cannot have a selection (\".%s\") applied.",
                field_name
              ); /* NORETURN */ 
          } 
	allocated_tc_addr = m3_tc_addr_from_object_addr ( lhs_inf_addr );
        if ( m3_check_TextLiteral_buf 
              ( lhs_inf_addr, allocated_tc_addr, field_name, & bitsize, & bitpos,
                & dot_type 
              ) 
           ) 
          { dot_value = value_at_lazy ( dot_type, lhs_inf_addr ); 
            set_value_offset 
              ( dot_value, value_offset ( dot_value ) + bitpos / TARGET_CHAR_BIT );
            if ( out_field_name != NULL ) 
              { *out_field_name = field_name; } 
            return dot_value; 
          } 
        else 
          { /* This is a faster way to find the typecell corresponding to 
               lhs_type than m3_tc_addr_from_type, which must search all 
               typecodes: */ 
            supertype_tc_addr = allocated_tc_addr; 
            supertype = m3_type_from_tc ( supertype_tc_addr ); 
            while ( false && supertype != lhs_type ) 
              { supertype_tc_addr 
                  = m3_super_tc_addr_from_tc_addr ( supertype_tc_addr ); 
                supertype = m3_type_from_tc ( supertype_tc_addr ); 
              } 
            supertype = lhs_type; 
            /* Here, supertype_tc_addr and supertype are for the static type. */
            while ( true ) /* Search supertypes for field/method. */ 
              { if ( TYPE_CODE ( supertype ) != TYPE_CODE_M3_OBJECT ) 
                  { error 
                      ( "Object has no field or method named \"%s\"."
                      , field_name 
                      ); /* NORETURN */ 
                    break; 
                  }
                else if ( m3_find_obj_field 
                            ( supertype, field_name, & bitsize, & bitpos, 
                              & dot_type 
                            ) 
                        ) 
                  { dot_value = value_at_lazy ( dot_type, lhs_inf_addr );
                    bitpos 
                      += TARGET_CHAR_BIT 
                         * m3_dataOffset_from_tc_addr ( supertype_tc_addr ); 
                    dot_value 
                      = m3_field_value ( dot_value, bitsize, bitpos, dot_type ); 
                    if ( out_field_name != NULL ) 
                      { *out_field_name = field_name; } 
                    return dot_value; 
                  } /* if */ 
                else if ( m3_find_obj_method 
                            ( supertype, field_name, & bitsize, & bitpos, 
                              & dot_type 
                            ) 
                        ) 
                  { dot_value 
                      = value_at_lazy 
                          ( dot_type, 
                            m3_defaultMethods_from_tc_addr ( allocated_tc_addr )
                          );
                    set_value_offset 
                      ( dot_value, 
                        m3_methodOffset_from_tc_addr ( supertype_tc_addr )
                        + bitpos / TARGET_CHAR_BIT 
                      ); 
                    if ( self != NULL ) { *self = lhs_value; } 
                    if ( out_field_name != NULL ) 
                      { *out_field_name = field_name; } 
                    return dot_value; 
                  } /* else if */ 
                else /* Go up a level in supertype hierarchy. */ 
                  { supertype_tc_addr 
                      = m3_super_tc_addr_from_tc_addr ( supertype_tc_addr ); 
                    supertype = m3_type_from_tc ( supertype_tc_addr ); 
                    /* And loop. */ 
                  } /* else */ 
              } /* while */ 
          } /* else */ 
        break; 

      default:  
        error ( "A selection (\".%s\") can apply only to a RECORD, "
                "REF RECORD, or OBJECT.",
                field_name
              ); /* NORETURN */ 
    } /* switch */ 

} /* m3_evaluate_dot */ 

/* This evaluates a call that is known not a dispatching method call, but could
   be a call on a procedure constant, a procedure variable, or something not 
   callable.
*/ 
static struct value *
m3_call_proc_const_or_var ( 
    struct value * orig_proc_value, 
    int nargs, 
    char * name, 
    struct expression *exp, 
    int *pos,
    enum noside noside
  ) 

{ struct value * proc_value; 
  struct type * closure_type; 
  struct type * proc_type; 
  struct symbol * proc_const_sym; 
  struct type * proc_const_type;
  struct value * deref_value; 
  CORE_ADDR inf_addr; 
  CORE_ADDR inf_code_addr; 

  proc_value = orig_proc_value; 
  proc_type = value_type ( proc_value ); 
  while ( TYPE_CODE ( proc_type ) == TYPE_CODE_M3_INDIRECT ) 
    { inf_addr = value_as_address ( proc_value ); 
      if ( inf_addr == 0 ) 
        { error 
            ( "NIL indirect.  Modula-3 compiler shouldn't allow this to happen." ); 
          /* NORETURN */ 
        } 
      proc_value = value_at_lazy ( TYPE_M3_TARGET ( proc_type ), inf_addr ); 
      proc_type = value_type ( proc_value ); 
    } /* while */
  switch ( TYPE_CODE ( proc_type ) )  
    { case TYPE_CODE_FUNC: /* A procedure constant. */
        proc_value 
          = m3_nested_proc_const_closure ( proc_value, & inf_code_addr ); 
        break; 
      case TYPE_CODE_M3_PROC: /* A procedure variable. */  
        inf_addr = value_as_address ( proc_value ); 
        if ( inf_addr == 0 ) 
          { error ( "Attempt to call NIL procedure variable \"%s\".", name ); 
            /* NORETURN */ 
          } 
        if ( m3_inf_address_is_proc_closure ( inf_addr ) ) 
          /* inf_addr points to a closure in inferior. 
             Build a gdb closure value. */ 
          { closure_type = m3_alloc_closure_type ( NULL );
            deref_value = value_at_lazy ( closure_type, inf_addr );
            inf_code_addr = m3_proc_value_code_addr ( deref_value );
            proc_const_sym = find_pc_function ( inf_code_addr ); 
            proc_const_type = SYMBOL_TYPE ( proc_const_sym ); 
            TYPE_TARGET_TYPE ( closure_type ) = proc_const_type; 
            proc_value = deref_value; 
          } 
        else /* It's not a closure. */ 
          /* inf_addr is the direct code address.  
             Give proc_value the procedure constant value and type. */ 
          { inf_code_addr = inf_addr; 
            proc_const_sym = find_pc_function ( inf_code_addr ); 
            proc_const_type = SYMBOL_TYPE ( proc_const_sym ); 
            proc_value = value_at_lazy ( proc_const_type, inf_code_addr ); 
            /* proc_type = value_type ( proc_value ); */ 
          }
        break; 
      default: 
        error ( "Attempt to call non-procedure \"%s\".", name ); /* NORETURN */  
    } 
  
  return 
    m3_evaluate_call 
      ( proc_value, /* For the procedure constant. */ 
        proc_type,  /* Original proc type, for checking actuals. */ 
        inf_code_addr,
        /* self */ NULL,
        nargs, name, exp, pos, noside 
      );  
} /* m3_call_proc_const_or_var */ 

/* Never returns a value whose type has TYPE_CODE M3_INDIRECT. */ 
static struct value * 
m3_check_and_coerce_assignment ( 
    struct value * lhs_value,   
    struct value * rhs_value   
  ) 

{ struct type * lhs_type; 
  struct type * rhs_type; 
  struct type * rhs_direct_type;
  struct type * lhs_direct_type;
  struct value * lhs_direct_value;
  struct value * rhs_direct_value;
  struct value * result_direct_value;

  if ( lhs_value == NULL  || rhs_value == NULL ) { return NULL; } 
  lhs_type = value_type ( lhs_value );  
  rhs_type = value_type ( rhs_value );  
  
  if ( TYPE_CODE ( rhs_type ) == TYPE_CODE_M3_INDIRECT )
    { rhs_direct_type = TYPE_M3_INDIRECT_TARGET ( rhs_type ); 
      rhs_direct_value 
         = value_at_lazy 
             ( rhs_direct_type , value_as_address ( rhs_value ) );
    } 
  else 
    { rhs_direct_type = rhs_type; 
      rhs_direct_value = rhs_value; 
    } 

  if ( TYPE_CODE ( lhs_type ) == TYPE_CODE_M3_INDIRECT ) 
    { lhs_direct_type = TYPE_M3_INDIRECT_TARGET ( lhs_type ); 
      lhs_direct_value 
         = value_at_lazy 
             ( lhs_direct_type , value_as_address ( lhs_value ) );
    }
  else /* VALUE mode (passed by value.) */ 
    { lhs_direct_type = lhs_type; 
      lhs_direct_value = lhs_value; 
      if ( ! VALUE_LVAL ( lhs_direct_value ) ) 
        { error ( "LHS of assignment is not a designator." ); /* NORETURN */ } 
    } 
  if ( m3_types_equal ( lhs_direct_type, rhs_direct_type ) ) 
    { return rhs_direct_value; } 

  result_direct_value = m3_check_and_coerce_array 
    ( lhs_direct_value, lhs_direct_type, rhs_direct_value, 
      rhs_direct_type, NULL, NULL 
    );  
  if ( result_direct_value != NULL ) { return result_direct_value; }  

  result_direct_value = m3_check_and_coerce_proc 
    ( lhs_direct_type, rhs_direct_value, rhs_direct_type, 
      NULL, NULL 
    );  
  if ( result_direct_value != NULL ) { return result_direct_value; }  

  result_direct_value = m3_check_and_coerce_ordinal 
    ( lhs_direct_type, rhs_direct_value, 
      rhs_direct_type, NULL, NULL 
    );  
  if ( result_direct_value != NULL ) { return result_direct_value; }  

  result_direct_value = m3_check_and_coerce_reference
    ( lhs_direct_type, rhs_direct_value, 
      rhs_direct_type, NULL, NULL 
    );  
  if ( result_direct_value != NULL ) { return result_direct_value; }  

  error ( "RHS expression type not assignable to LHS type in "
          "assignment statement/" 
        ); /* NORETURN */

  return NULL; /* Suppress warnings.  Shouldn't get here. */ 
} /* m3_check_and_coerce_assignment */ 

static struct value *
m3_evaluate_subexp_maybe_packed ( 
    struct type *expect_type,
    struct expression *exp, 
    int *pos,
    enum noside noside
  )
{
  enum exp_opcode op;
  int length;
  int tempval;
  int pc;
  struct value *arg1 = NULL;
  struct value *arg2 = NULL;
  /*struct type *type;*/
  /*int upper, lower;*/
  int float_ok, int_ok;

  pc = *pos;
  op = exp->elts[pc].opcode;

  switch (op)
    {
    case OP_VAR_VALUE: 
      { struct symbol * sym;
        struct block *b;
        struct value *val;
        struct frame_info *frame;

        (*pos) += 4; 
        if (noside == EVAL_SKIP) 
           { return m3_value_from_longest (builtin_type_long, (LONGEST) 1);} 
        b = exp->elts[pc+1].block; 
        sym = exp->elts[pc+2].symbol; 
        if (symbol_read_needs_frame (sym))
          {
            frame = m3_frame_for_block (b);
            if (!frame)
              {
                if (BLOCK_FUNCTION (b)
                    && SYMBOL_PRINT_NAME (BLOCK_FUNCTION (b)))
                  error (_("No frame is currently executing in block %s."),
                         SYMBOL_PRINT_NAME (BLOCK_FUNCTION (b)));
                else
                  error (_("No frame is currently executing in specified block"));
              }
          }
        else { frame = NULL; } 

        val = read_var_value (sym, frame);
        if (!val)
          error (_("Address of symbol \"%s\" is unknown."), 
                 SYMBOL_PRINT_NAME (sym));

        return val;
      } 

    case OP_REGISTER:
      {
	int regno = longest_to_int (exp->elts[pc + 1].longconst);
	struct value *val 
           = value_of_register (regno, get_selected_frame ("No frame"));
	(*pos) += 3;
	if (val == NULL)
	  error ("Value of register %s not available.",
		 frame_map_regnum_to_name 
                   (get_selected_frame ("No frame"), regno));

 	else {
 	  arg1 
            = value_of_register 
                (longest_to_int (exp->elts[pc +1].longconst)
                , get_selected_frame("No frame")
                );
 	  /* hack to convert registers to Modula-3 types... */
 	  if (value_type(arg1) == builtin_type_long) {
 	    deprecated_set_value_type(arg1, builtin_type_m3_integer);
   	    return arg1;
  	  } 
 	  else if (value_type(arg1) == builtin_type_double) {
 	    deprecated_set_value_type(arg1, builtin_type_m3_longreal);
   	    return arg1;
 	  }
 	  else return val;
        }
      }

    case OP_M3_WIDETEXT:
    case OP_M3_TEXT:
      length = longest_to_int (exp->elts[pc + 1].longconst);
      (*pos) += 4 + BYTES_TO_EXP_ELEM (length + 1);
      if (noside == EVAL_SKIP)
        return m3_value_from_longest (builtin_type_long, (LONGEST) 1);
      else 
      return value_string (&exp->elts[pc + 2].string, length);

    case OP_M3_LONG:
    case OP_M3_CHAR:
    case OP_M3_WIDECHAR:
      (*pos) += 4;
      return m3_value_from_longest (exp->elts[pc+1].type,
                                 exp->elts[pc + 2].longconst);

    case OP_M3_REEL:
    case OP_M3_LREEL:
    case OP_M3_XREEL: {
      (*pos) += 4;
      return value_from_double (exp->elts[pc + 1].type,
				exp->elts[pc + 2].doubleconst); }

    case STRUCTOP_M3_ENUM: 
      arg1 = m3_evaluate_enum_const ( exp, pos, noside ); 
      return arg1;
      
    case STRUCTOP_M3_INTERFACE:
    case STRUCTOP_M3_MODULE:
    case STRUCTOP_M3_STRUCT: 
      { arg1 = m3_evaluate_dot ( exp, pos, noside, NULL, NULL );  
        return arg1; 
      } 

    case OP_FUNCALL: 
      { enum exp_opcode subop;
        struct value * proc_value; 
        struct value * proc_const_value; 
        struct symbol * proc_const_sym; 
        struct type * proc_type; 
        struct type * proc_const_type; 
        int nargs; 
        struct value * self;
        char * field_name; 
        CORE_ADDR inf_proc_addr; 

        (*pos) += 3; 
        subop = exp -> elts [ *pos ] . opcode; 
        nargs = longest_to_int ( exp -> elts [ pc + 1 ] . longconst ); 
        switch ( subop ) 
          { case STRUCTOP_M3_INTERFACE:
            case STRUCTOP_M3_MODULE:
            case STRUCTOP_M3_STRUCT:  
              proc_value 
                = m3_evaluate_dot ( exp, pos, noside, &self, &field_name );  
              if ( self == NULL ) 
                { return 
                    m3_call_proc_const_or_var 
                      ( proc_value, nargs, field_name, exp, pos, noside );  
                } 
              else /* Method call. proc_value denotes the self object. */
                { proc_type = value_type ( proc_value ); 
                  inf_proc_addr = value_as_address ( proc_value ); 
                  proc_const_sym = find_pc_function ( inf_proc_addr ); 
                  proc_const_type = SYMBOL_TYPE ( proc_const_sym ); 
                  proc_const_value = value_at_lazy 
                    ( proc_const_type, inf_proc_addr ); 
                  return 
                    m3_evaluate_call 
                      ( proc_const_value, 
                        proc_type, 
                        inf_proc_addr, 
                        self, 
                        nargs, field_name, exp, pos, noside 
                      );
                } 
            default:
              proc_value = m3_evaluate_subexp (0, exp, pos, noside);
              proc_type = value_type ( proc_value ); 
              inf_proc_addr = value_as_address ( proc_value ); 
              proc_const_sym = find_pc_function ( inf_proc_addr ); 
              return 
                m3_call_proc_const_or_var 
                  ( proc_value, nargs, SYMBOL_PRINT_NAME ( proc_const_sym ), 
                    exp, pos, noside 
                  );
          } /* switch */ 
      } 

    /* M3_FINAL_TYPE converts an expression of reference type to its allocated 
       type. */
    case M3_FINAL_TYPE: 
      { struct type * arg1_type;
        struct type * allocated_type;

        ( * pos)  += 1; 
        arg1 = m3_evaluate_subexp ( 0, exp, pos, noside );
        arg1_type = value_type ( arg1 );

        allocated_type = m3_allocated_type ( arg1 ); 
        if ( allocated_type != arg1_type ) 
          { deprecated_set_value_type ( arg1, arg1_type ); } 
        return ( arg1 ); 
      }

    case OP_M3_TYPE:
      (*pos) += 3;
      arg1 = allocate_value (exp->elts[pc+1].type);
      *(LONGEST *) value_contents_raw (arg1) = m3_type_magic_value;
      return arg1;

    case UNOP_M3_DEREF: {
      struct type *res_type, *arg1_type;

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (0, exp, pos, noside);
      arg1_type = value_type (arg1);

      while (TYPE_CODE (arg1_type) == TYPE_CODE_M3_INDIRECT) 
        { arg1_type = TYPE_M3_TARGET (arg1_type);
	  arg1 = value_at_lazy (arg1_type, m3_value_as_address (arg1)); 
        }

      if (TYPE_CODE (arg1_type) == TYPE_CODE_M3_REFANY) 
        { if (value_as_address (arg1) == 0) 
            { error ("^ applied to NIL"); 
              return arg1; 
            }
        /* REVIEWME:  Do we really want to allow dereferencing of REFANY to
           take the allocated type?  If so, don't we want to do the same
           for a traced pointer type as well? */ 
	  res_type 
            = m3_allocated_type_from_object_addr (value_as_address (arg1)); 
          return value_at_lazy (res_type, m3_value_as_address (arg1)); 
        }

      if (TYPE_CODE (arg1_type) == TYPE_CODE_M3_POINTER) 
        { if (value_as_address (arg1) == 0) 
            { error ("^ applied to NIL"); 
              return arg1; 
            }
          res_type = TYPE_M3_TARGET (arg1_type); 
          return value_at_lazy (res_type, m3_value_as_address (arg1)); 
        }

      else if (TYPE_CODE (arg1_type) == TYPE_CODE_M3_OBJECT ) 
        /* FIXME:  Do this for builtin object types as well, e.g. ROOT. */ 
        { warning ("Ignoring redundant ^ applied to object"); 
          if (value_as_address (arg1) == 0) 
            /* REVIEWME: Is this check really necessary here? */ 
            { error ("^ applied to NIL object"); }
          return arg1; 
        }

      else 
        { error ("^ applied to a non-REF"); 
          return arg1; 
        }
      } 

    case UNOP_M3_NEG: {
      struct type *neg_type; 

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (0, exp, pos, noside);
      neg_type = value_type (arg1);
      if (TYPE_CODE (neg_type) == TYPE_CODE_FLT)
	return value_from_double (neg_type, - m3_value_as_float (arg1));
      else if (TYPE_CODE (neg_type) == TYPE_CODE_M3_INTEGER)
	return m3_value_from_longest (neg_type, - m3_value_as_integer (arg1));
      else {
	error ("'-' must be applied to an integer or floating-point value");
	return arg1;
      }}
      
    case UNOP_M3_NOT: {
      LONGEST val;

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      val  = ! m3_value_as_integer (arg1);
      return m3_value_from_longest (builtin_type_m3_boolean, val);  }
	
    case UNOP_M3_FIRST:
    case UNOP_M3_LAST: 
    case UNOP_M3_NUMBER: {
      struct value * res, * array;
      struct type * array_type, * index_type = NULL;
      LONGEST lowerbound, upperbound, val = 0;

      (*pos) += 1; 
      array = m3_evaluate_subexp (0, exp, pos, noside);
      array_type = value_type (array);

      while (TYPE_CODE (array_type) == TYPE_CODE_M3_POINTER
	     || TYPE_CODE (array_type) == TYPE_CODE_M3_INDIRECT) {
	array_type = TYPE_M3_TARGET (array_type);
	array = value_at_lazy (array_type, m3_value_as_address (array));
        if (array == 0) {
          error ("FIRST, LAST or NUMBER applied to NIL");  }}

      if (TYPE_CODE (array_type) == TYPE_CODE_M3_ARRAY) {
	index_type = TYPE_M3_ARRAY_INDEX (array_type);
	m3_ordinal_bounds (index_type, &lowerbound, &upperbound);
      } else if (TYPE_CODE (array_type) == TYPE_CODE_M3_OPEN_ARRAY) {
	lowerbound = 0;
	upperbound = *(long*) (value_contents (array) + sizeof(long)) - 1;
        index_type = builtin_type_m3_integer; 
      } else if (m3_is_ordinal_type (array_type)) {
	index_type = array_type;
	m3_ordinal_bounds (index_type, &lowerbound, &upperbound);
      } else {
	error ("FIRST, LAST, NUMBER can only be applied to arrays.");
      }

      res = allocate_value (builtin_type_m3_integer);
      set_value_lazy (res, 0);
      switch (op) {
	case UNOP_M3_FIRST:   val = lowerbound;  break;
	case UNOP_M3_LAST:    val = upperbound;  break;
	case UNOP_M3_NUMBER:  val = upperbound - lowerbound + 1; 
                              index_type = builtin_type_m3_cardinal;  break;
      }
      res = allocate_value (index_type);
      *(LONGEST *)value_contents_raw (res) = val;
      set_value_lazy (res, 0);
      return res;
    }

    case UNOP_M3_ABS: {
      struct type *arg1_type;

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = value_type (arg1);
      if (TYPE_CODE (arg1_type) == TYPE_CODE_M3_INTEGER) {
	LONGEST val = m3_value_as_integer (arg1);
	if (val < 0) { val = -val; };
	return m3_value_from_longest (arg1_type, val);
      } else if (TYPE_CODE (arg1_type) == TYPE_CODE_FLT) {
	double val = m3_value_as_float (arg1);
	if (val < 0.0) { val = -val; };
	return value_from_double (arg1_type, val);
      } else {
	error ("ABS requires an INTEGER, REAL, LONGREAL, or EXTENDED parameter");
	return arg1;
      }}

    case UNOP_M3_ADR: 
      { struct value * v; 

        ( * pos) += 1; 
      v = evaluate_subexp_for_address (exp, pos, noside); 
      TYPE_CODE (value_type (v)) = TYPE_CODE_M3_ADDRESS;
      TYPE_M3_SIZE (value_type (v)) = TARGET_PTR_BIT;
      return v; }

    case UNOP_M3_ADRSIZE: {
      LONGEST sz;
      struct type *arg1_type;

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = value_type (arg1);
      sz = TYPE_M3_SIZE (arg1_type) / HOST_CHAR_BIT;
      if (TYPE_CODE (arg1_type) == TYPE_CODE_M3_OPEN_ARRAY) {
	error ("ADRSIZE(open array) not implemented");
	sz = 1;
      }
      return m3_value_from_longest (builtin_type_m3_integer, sz); }

    case UNOP_M3_BITSIZE: {
      LONGEST sz;
      struct type *arg1_type;

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = value_type (arg1);
      sz = TYPE_M3_SIZE (arg1_type);
      if (TYPE_CODE (arg1_type) == TYPE_CODE_M3_OPEN_ARRAY) {
	error ("BITSIZE(open array) not implemented");
	sz = 8;
      }
      return m3_value_from_longest (builtin_type_m3_integer, sz); }

    case UNOP_M3_BYTESIZE: {
      LONGEST sz;
      struct type *arg1_type;

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = value_type (arg1);
      sz = TYPE_M3_SIZE (arg1_type) / 8;
      if (TYPE_CODE (arg1_type) == TYPE_CODE_M3_OPEN_ARRAY) {
	error ("BYTESIZE(open array) not implemented");
	sz = 1;
      }
      return m3_value_from_longest (builtin_type_m3_integer, sz);
    }

    case UNOP_M3_CEILING: {
      struct type *arg1_type;

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = value_type (arg1);

      if (TYPE_CODE (arg1_type) == TYPE_CODE_FLT) {
        double val;
        LONGEST intval;
        val  = m3_value_as_float (arg1);
        intval = (LONGEST) (val);
        if ((val > 0.0e0) && ((double)intval != val)) { intval++; }
        return m3_value_from_longest (builtin_type_m3_integer, intval);
      } else {
	error ("CEILING must be applied to a floating-point value");
	return arg1;
      }
    }

    case UNOP_M3_FLOOR: {
      struct type *arg1_type;

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = value_type (arg1);

      if (TYPE_CODE (arg1_type) == TYPE_CODE_FLT) {
        double val;
        LONGEST intval;
        val  = m3_value_as_float (arg1);
        intval = (LONGEST) (val);
        if ((val < 0.0e0) && ((double)intval != val)) { intval--; }
        return m3_value_from_longest (builtin_type_m3_integer, intval);
      } else {
	error ("FLOOR must be applied to a floating-point value");
	return arg1;
      }
    }

    case UNOP_M3_ROUND: {
      struct type *arg1_type;

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = value_type (arg1);

      if (TYPE_CODE (arg1_type) == TYPE_CODE_FLT) {
        double val;
        LONGEST intval;
        val  = m3_value_as_float (arg1);
        intval = (LONGEST) (val + 0.5);
        return m3_value_from_longest (builtin_type_m3_integer, intval);
      } else {
	error ("ROUND must be applied to a floating-point value");
	return arg1;
      }
    }

    case UNOP_M3_TRUNC: {
      struct type *arg1_type;

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = value_type (arg1);

      if (TYPE_CODE (arg1_type) == TYPE_CODE_FLT) {
        double val;
        LONGEST intval;
        val  = m3_value_as_float (arg1);
        intval = (LONGEST) (val);
        return m3_value_from_longest (builtin_type_m3_integer, intval);
      } else {
	error ("TRUNC must be applied to a floating-point value");
	return arg1;
      }
    }

    case UNOP_M3_ORD: {
      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      if (m3_is_ordinal_type (value_type (arg1))) {
        LONGEST val;
        val  = m3_value_as_integer (arg1);
        return m3_value_from_longest (builtin_type_m3_integer, val);
      } else {
	error ("value passed to ORD is not of an ordinal type");
        return arg1;
      }
    }

    case BINOP_M3_VAL: {
      struct type *arg1_type;

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = value_type (arg1);

      if (TYPE_CODE (arg1_type) != TYPE_CODE_M3_INTEGER) {
	error ("first argument of VAL must be an integer");
	return arg1;
      } else if ((*(LONGEST *) value_contents (arg2) != m3_type_magic_value) ||
		 (! m3_is_ordinal_type (value_type(arg2)))) {
	error ("second argument of VAL must be an ordinal type");
	return arg1;
      } else {
        LONGEST val, lower, upper;
        val  = m3_value_as_integer (arg1);
	m3_ordinal_bounds (value_type(arg2), &lower, &upper);
	if ((val < lower) || (upper < val)) {
	  error ("value passed to VAL is out of range");
	  return arg1;
        } else {
          return m3_value_from_longest (value_type(arg2), val);
        }
      }
    }

    case BINOP_M3_FLOAT: {
      struct type *arg1_type;
      double val;

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = value_type (arg1);

      if (TYPE_CODE (arg1_type) == TYPE_CODE_FLT) {
        val = m3_value_as_float (arg1);
      } else if (TYPE_CODE (arg1_type) == TYPE_CODE_M3_INTEGER) {
        val = (double) m3_value_as_integer (arg1);
      } else {
	error 
          ("first parameter of FLOAT must be an INTEGER, REAL, LONGREAL, "
            "or EXTENDED value" 
          );
	return arg1;
      }

      if ((*(LONGEST *) value_contents (arg2) != m3_type_magic_value)
	 || (TYPE_CODE (value_type(arg2)) != TYPE_CODE_FLT)) {
	error ("second parameter of FLOAT must be REAL, LONGREAL, or EXTENDED");
	return arg1;
      }

      return value_from_double (value_type(arg2), val);
    }

    case BINOP_M3_LOOPHOLE: {
      struct type *arg1_type, *arg2_type;

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = value_type (arg1);
      arg2_type = value_type (arg2);

      if (TYPE_CODE(arg1_type) == TYPE_CODE_M3_OPEN_ARRAY) {
	error ("LOOPHOLE of open array values is illegal");
        return arg1;
      } else if (*(LONGEST *) value_contents (arg2) != m3_type_magic_value) {
	error ("second parameter of LOOPHOLE must be a type");
	return arg1;
      } else if (TYPE_CODE (arg2_type) == TYPE_CODE_M3_OPEN_ARRAY) {
	error ("LOOPHOLE to an open array type is not (yet) supported");
        return arg1;
      } else if (TYPE_M3_SIZE (arg1_type) != TYPE_M3_SIZE (arg2_type)) {
	error ("size of value and type passed to LOOPHOLE don't agree");
	return arg1;
      }

      deprecated_set_value_type (arg1, arg2_type);
      return arg1;
    }

    case BINOP_M3_SUBSCRIPT: {
      LONGEST lowerbound, upperbound;
      long index_val; 
      long offset;
      struct type *index_type, *elem_type, *array_type;
      struct value * v, * array, * index;
      long elt_size;

      (*pos) += 1; 
      array = m3_evaluate_subexp (0, exp, pos, noside);
      index = m3_evaluate_subexp (0, exp, pos, noside);
      array_type = value_type (array);

      while (TYPE_CODE (array_type) == TYPE_CODE_M3_POINTER
	     || TYPE_CODE (array_type) == TYPE_CODE_M3_INDIRECT) {
	array_type = TYPE_M3_TARGET (array_type);
	array = value_at_lazy (array_type, m3_value_as_address (array)); }

      if (TYPE_CODE (array_type) == TYPE_CODE_M3_ARRAY) {
	index_type = TYPE_M3_ARRAY_INDEX (array_type);
	elem_type  = TYPE_M3_ARRAY_ELEM (array_type);
	elt_size   = TYPE_M3_SIZE (elem_type);
	m3_ordinal_bounds (index_type, &lowerbound, &upperbound); }
      else if (TYPE_CODE (array_type) == TYPE_CODE_M3_OPEN_ARRAY) {
	elem_type = TYPE_M3_OPEN_ARRAY_ELEM (array_type);
	lowerbound = 0;
        /* FIXME: this needs to be a target long.  Use the fetch functions
           for open array dope. */ 
	upperbound 
          = *(long*) (value_contents (array) + TARGET_PTR_BIT/HOST_CHAR_BIT) - 1;
        { struct type *e = elem_type;
	  long n = (TARGET_PTR_BIT + TARGET_LONG_BIT) / HOST_CHAR_BIT;
	  elt_size = 1;
	  while (TYPE_CODE (e) == TYPE_CODE_M3_OPEN_ARRAY) {
            /* FIXME: This should be a target long, not a host long. */
	    elt_size *= *(long*) (value_contents (array) + n);
            n += TARGET_LONG_BIT / HOST_CHAR_BIT;
	    e = TYPE_M3_OPEN_ARRAY_ELEM (e); }
	  elt_size *= TYPE_M3_SIZE (e); }}
      else {
	error ("indexed expression is not an array"); }

      array = coerce_ref (array);

      index_val = m3_value_as_integer (index);
      if (lowerbound > index_val || index_val > upperbound) {
	error ("range fault on array access");
	return 0; }

      offset = elt_size * (index_val - lowerbound);
      if (offset % 8 != 0) {
	error ("Non-byte-aligned, bit-packed array elements not supported."); 
	return 0; }
      
      v = allocate_value (elem_type);

      if (TYPE_CODE (array_type) == TYPE_CODE_M3_OPEN_ARRAY) {

	if (TYPE_CODE (elem_type) == TYPE_CODE_M3_OPEN_ARRAY) {
	  /* recreate a dope vector for the next guy */
	  memcpy (value_contents_raw (v) + (TARGET_PTR_BIT / HOST_CHAR_BIT),
		  value_contents (array)
		    + (TARGET_PTR_BIT + TARGET_LONG_BIT)/ HOST_CHAR_BIT, 
		  TYPE_LENGTH (elem_type) - TARGET_LONG_BIT / HOST_CHAR_BIT);
	  *(char **)value_contents_raw (v) = 
	    *(char **)value_contents (array) + offset / 8; }

	else {
	  /* mark the thing as not read yet */
	  set_value_lazy (v, 1);
	  VALUE_LVAL (v) = VALUE_LVAL (array);
	  VALUE_ADDRESS (v) = 
	    (*(long*)(value_contents (array))) + offset / 8;
	  set_value_offset (v, 0); }}

      else {

	if (value_lazy (array)) { 
	  set_value_lazy (v, 1); }
	else {
	  memcpy (value_contents_raw (v), 
		  value_contents (array) + offset / 8,
		  TYPE_LENGTH (elem_type)); }
	VALUE_LVAL (v) = VALUE_LVAL (array);
	if (VALUE_LVAL (array) == lval_internalvar) {
	  VALUE_LVAL (v) = lval_internalvar_component; }
	VALUE_ADDRESS (v) = VALUE_ADDRESS (array);
	set_value_offset (v, value_offset (array) + offset / 8);  }
      return v; 
      break; }
      
    case BINOP_M3_DIVIDE: {
      float_ok = 1;
      int_ok = 0;
      goto arith_binop; }

    case BINOP_M3_DIV: {
      float_ok = 0;
      int_ok = 1;
      goto arith_binop; }

    case BINOP_M3_MOD:
    case BINOP_M3_MULT: 
    case BINOP_M3_ADD:
    case BINOP_M3_MINUS: {
      float_ok = 1;
      int_ok = 1;
      goto arith_binop; }

    /* TODO: Factor this out: */ 
    arith_binop: {
      struct value * res;
      LONGEST ival1 = 0, ival2 = 0;
      double fval1 = 0.0, fval2 = 0.0;
      struct type *arg1_type, *arg2_type;

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (0, exp, pos, noside);
      arg2 = m3_evaluate_subexp (0, exp, pos, noside);

      arg1_type = value_type (arg1);
    restart:
      switch (TYPE_CODE (arg1_type)) 
	{
	case TYPE_CODE_M3_INDIRECT:
	  arg1_type = TYPE_M3_TARGET (arg1_type);
	  arg1 = value_at_lazy (arg1_type, m3_value_as_address (arg1));
	  goto restart; 
	case TYPE_CODE_M3_PACKED:
	  arg1_type = TYPE_M3_PACKED_TARGET (arg1_type);
	  goto restart;
	case TYPE_CODE_M3_CARDINAL:
	case TYPE_CODE_M3_SUBRANGE:
	  arg1_type = builtin_type_m3_integer;
	  /* fall through */
	case TYPE_CODE_M3_INTEGER:
	  ival1 = m3_value_as_integer (arg1);
	  break;
	case TYPE_CODE_FLT:
	  fval1 = m3_value_as_float (arg1);
	  arg1_type = builtin_type_double;
	  break;
	case TYPE_CODE_INT:
	  ival1 = value_as_long (arg1);
	  arg1_type = builtin_type_m3_integer;
	  break;
        case TYPE_CODE_M3_ADDRESS:
        case TYPE_CODE_M3_POINTER:
        case TYPE_CODE_M3_NULL:
        case TYPE_CODE_M3_UN_ROOT:
          /* FIXME:  And the rest of the reference types? */ 
          ival1 = (LONGEST) m3_value_as_address (arg1);
	  arg1_type = builtin_type_m3_integer;
	  break;
	default:
	  arg1_type = builtin_type_m3_void;
	  break; }

      arg2_type = value_type (arg2);
    restart2:
      switch (TYPE_CODE (arg2_type)) 
	{
	case TYPE_CODE_M3_INDIRECT:
	  arg2_type = TYPE_M3_TARGET (arg2_type);
	  arg2 = value_at_lazy (arg2_type, m3_value_as_address (arg2));
	  goto restart2; 
	case TYPE_CODE_M3_PACKED:
	  arg2_type = TYPE_M3_TARGET (arg2_type);
	  goto restart2;
	case TYPE_CODE_M3_CARDINAL:
	case TYPE_CODE_M3_SUBRANGE:
	  arg2_type = builtin_type_m3_integer;
	  /* fall through */
	case TYPE_CODE_M3_INTEGER:
	  ival2 = m3_value_as_integer (arg2);
	  break;
	case TYPE_CODE_FLT:
	  fval2 = m3_value_as_float (arg2);
	  arg2_type = builtin_type_double;
	  break;
	case TYPE_CODE_INT:
	  ival2 = value_as_long (arg2);
	  arg2_type = builtin_type_m3_integer;
	  break;
        case TYPE_CODE_M3_ADDRESS:
        case TYPE_CODE_M3_POINTER:
        case TYPE_CODE_M3_NULL:
        case TYPE_CODE_M3_UN_ROOT:
          /* FIXME:  And the rest of the reference types? */ 
          ival2 = (LONGEST) m3_value_as_address (arg2);
	  arg2_type = builtin_type_m3_integer;
	  break;
	default:
	  arg2_type = builtin_type_m3_void;
	  break; }


      if (TYPE_CODE (arg1_type) != TYPE_CODE (arg2_type)
	  || TYPE_CODE (arg1_type) == TYPE_CODE_M3_VOID
	  || (TYPE_CODE (arg1_type) == TYPE_CODE_M3_INTEGER && !int_ok)
	  || (TYPE_CODE (arg1_type) == TYPE_CODE_FLT && !float_ok)) {
	error ("wrong arguments for binary operation");
      }

      if (TYPE_CODE (arg1_type) == TYPE_CODE_M3_INTEGER) {
	LONGEST res = 0;
	switch (op) {
	  case BINOP_M3_MULT: 	res = ival1 * ival2;          break;
	  case BINOP_M3_ADD:	res = ival1 + ival2;          break;
	  case BINOP_M3_MINUS:  res = ival1 - ival2;          break;
	  case BINOP_M3_DIV:    res = m3_div (ival1, ival2);  break;
	  case BINOP_M3_MOD:    res = m3_modi (ival1, ival2); break;
	} /* switch */
	return m3_value_from_longest (builtin_type_m3_integer, res);
      }

      if (TYPE_CODE (arg1_type) == TYPE_CODE_FLT) {
	double res = 0.0;
	switch (op) {
	  case BINOP_M3_DIVIDE: res = fval1 / fval2;          break;
	  case BINOP_M3_MULT:   res = fval1 * fval2;          break;
	  case BINOP_M3_ADD:    res = fval1 + fval2;          break;
	  case BINOP_M3_MINUS:  res = fval1 - fval2;          break;
	  case BINOP_M3_MOD:    res = m3_modf (fval1, fval2); break;
	}
	return value_from_double (arg1_type, res);
      }
    }

    case BINOP_M3_AND: {

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      if (m3_value_as_integer (arg1) == 0) {	return arg1; }
      return m3_evaluate_subexp (NULL_TYPE, exp, pos, noside); }

    case BINOP_M3_OR: {

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      if (m3_value_as_integer (arg1) == 1) {	return arg1; }
      return m3_evaluate_subexp (NULL_TYPE, exp, pos, noside); }

    case BINOP_M3_EQUAL: {

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = m3_evaluate_subexp (value_type (arg1), exp, pos, noside);
      tempval = m3_value_equal (arg1, arg2);
      return 
        m3_value_from_longest (builtin_type_m3_boolean, (LONGEST) tempval); }

    case BINOP_M3_NE: {

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = m3_evaluate_subexp (value_type (arg1), exp, pos, noside);
      tempval = ! m3_value_equal (arg1, arg2);
      return m3_value_from_longest 
               (builtin_type_m3_boolean, (LONGEST) tempval); }

    case BINOP_M3_LT: {

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = m3_evaluate_subexp (value_type (arg1), exp, pos, noside);
      tempval = value_less (arg1, arg2);
      return m3_value_from_longest 
               (builtin_type_m3_boolean, (LONGEST) tempval); }

    case BINOP_M3_LE: {
      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = m3_evaluate_subexp (value_type (arg1), exp, pos, noside);
      tempval = ! (value_less (arg2, arg1));
      return m3_value_from_longest 
               (builtin_type_m3_boolean, (LONGEST) tempval); }

    case BINOP_M3_GT: {
      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = m3_evaluate_subexp (value_type (arg1), exp, pos, noside);
      tempval = value_less (arg2, arg1);
      return m3_value_from_longest 
              (builtin_type_m3_boolean, (LONGEST) tempval); }

    case BINOP_M3_GE: {
      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = m3_evaluate_subexp (value_type (arg1), exp, pos, noside);
      tempval = ! (value_less (arg1, arg2));
      return m3_value_from_longest 
               (builtin_type_m3_boolean, (LONGEST) tempval); }

    case BINOP_M3_MIN: {
      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = m3_evaluate_subexp (value_type (arg1), exp, pos, noside);
      return value_less (arg1, arg2) ? arg1 : arg2; }

    case BINOP_M3_MAX: {
      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = m3_evaluate_subexp (value_type (arg1), exp, pos, noside);
      return value_less (arg1, arg2) ? arg2 : arg1; }

    case BINOP_M3_CAT:
      error ("Not yet implemented: '&' text concatenation");
      return 0; 

    case BINOP_M3_IN:
      error ("Not yet implemented: 'IN' set membership test");
      return 0; 

    case BINOP_ASSIGN:
      (*pos) += 1; 
      arg1 = m3_evaluate_subexp_maybe_packed ( NULL_TYPE, exp, pos, noside );
      arg2 = m3_evaluate_subexp ( value_type ( arg1 ), exp, pos, noside );
      if ( noside == EVAL_SKIP || noside == EVAL_AVOID_SIDE_EFFECTS )
	{ return arg1; } 
      arg2 = m3_check_and_coerce_assignment ( arg1, arg2 ); 
      return value_assign ( arg1, arg2 );

    default:
      return evaluate_subexp_standard ( expect_type, exp, pos, noside);

    }
} /* m3_evaluate_subexp_maybe_packed */ 

struct value *
m3_evaluate_subexp ( 
    struct type *expect_type,
    struct expression *exp, 
    int *pos,
    enum noside noside
  )

{ struct value * packed_val; 
  struct value * unpacked_val; 

  packed_val 
    = m3_evaluate_subexp_maybe_packed ( expect_type, exp, pos, noside ); 
  unpacked_val = m3_ensure_value_is_unpacked ( packed_val ); 
  return unpacked_val; 
} /* m3_evaluate_subexp */ 

/* End of file m3-eval.c */ 
