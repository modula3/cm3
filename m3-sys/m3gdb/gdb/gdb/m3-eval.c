/* *INDENT-OFF* */

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

static BOOL
m3_types_equal ( struct type * left, struct type * right );

static BOOL
m3_type_fields_equal ( struct type * left, struct type * right )

{ int i;

  if ( TYPE_NFIELDS ( left ) != TYPE_NFIELDS ( right ) )
    { return FALSE; }
  for ( i = 0; i < TYPE_NFIELDS ( left ); i ++ )
    { if ( ! m3_types_equal
               ( TYPE_M3_FIELD_TYPE ( left, i ),
                 TYPE_M3_FIELD_TYPE ( right, i )
               )
         ) { return FALSE; }
    }
  return TRUE;
}

BOOL
m3_types_equal ( struct type * left, struct type * right )

{ struct type * left_direct;
  struct type * right_direct;
  int i;

  if ( left == NULL || right == NULL ) { return FALSE; }
  left_direct = m3_direct_type ( left );
  right_direct = m3_direct_type ( right );
  /* Avoid stripping off packed, because it affects type equality. */
  left_direct = m3_revealed_type ( left_direct );
  right_direct = m3_revealed_type ( right_direct );
  if ( left_direct == right_direct ) { return TRUE; }
  if ( TYPE_CODE ( left_direct ) != TYPE_CODE ( right_direct ) )
    { return FALSE; }
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
          { return FALSE; }
        return
          TYPE_M3_SUBRANGE_MIN ( left_direct )
          == TYPE_M3_SUBRANGE_MIN ( right_direct )
          && TYPE_M3_SUBRANGE_MAX ( left_direct )
             == TYPE_M3_SUBRANGE_MAX ( right_direct );
      case TYPE_CODE_M3_POINTER :
        if ( ! m3_type_fields_equal ( left_direct, right_direct ) )
          { return FALSE; }
        if ( TYPE_M3_POINTER_TRACED ( left_direct )
             != TYPE_M3_POINTER_TRACED ( right_direct )
           )
          { return FALSE; }
        if ( TYPE_M3_POINTER_BRANDED ( left_direct )
             != TYPE_M3_POINTER_BRANDED ( right_direct )
           )
          { return FALSE; }
        if ( TYPE_M3_POINTER_BRANDED ( left_direct ) )
          { return TYPE_M3_POINTER_BRAND ( left_direct )
                   == TYPE_M3_POINTER_BRAND ( right_direct );
          }
        return TRUE;
      case TYPE_CODE_M3_OPAQUE :
        return FALSE; /* Shouldn't happen. */

      case TYPE_CODE_M3_RECORD :
        if ( ! m3_type_fields_equal ( left_direct, right_direct ) )
          { return FALSE; }
        for ( i = 0; i < TYPE_NFIELDS ( left_direct ); i ++ )
          { if ( TYPE_FIELD_NAME ( left_direct, i )
                 != TYPE_FIELD_NAME ( right_direct, i )
               )
              { return FALSE; }
            if ( TYPE_M3_REC_FIELD_BITPOS ( left_direct, i )
                 != TYPE_M3_REC_FIELD_BITPOS ( right_direct, i )
               )
              { return FALSE; }
            if ( TYPE_M3_REC_FIELD_BITSIZE ( left_direct, i )
                 != TYPE_M3_REC_FIELD_BITSIZE ( right_direct, i )
               )
              { return FALSE; }
          }
        return TRUE;
      case TYPE_CODE_M3_OBJECT :
        if ( TYPE_M3_OBJ_NFIELDS ( left_direct )
             != TYPE_M3_OBJ_NFIELDS ( right_direct)
           )
          { return FALSE; }
        if ( TYPE_M3_OBJ_NMETHODS ( left_direct )
             != TYPE_M3_OBJ_NMETHODS ( right_direct)
           )
          { return FALSE; }
        if ( TYPE_M3_OBJ_TRACED ( left_direct )
             != TYPE_M3_OBJ_TRACED ( right_direct )
           )
          { return FALSE; }
        if ( TYPE_M3_OBJ_BRANDED ( left_direct )
             != TYPE_M3_OBJ_BRANDED ( right_direct )
           )
          { return FALSE; }
        if ( TYPE_M3_OBJ_BRANDED ( left_direct ) )
          { if ( strcmp ( TYPE_M3_OBJ_BRAND ( left_direct )
                        , TYPE_M3_OBJ_BRAND ( right_direct )
                        )
                 != 0
               )
            { return FALSE; }
          }
        for ( i = 0; i < TYPE_M3_OBJ_NMETHODS ( left_direct ); i ++ )
          { if ( strcmp ( TYPE_M3_OBJ_METHOD_NAME ( left_direct, i ),
                          TYPE_M3_OBJ_METHOD_NAME ( right_direct, i )
                        )
                 != 0
               )
              { return FALSE; }
            if ( ! m3_types_equal
                     ( TYPE_M3_OBJ_METHOD_TYPE ( left_direct, i ),
                       TYPE_M3_OBJ_METHOD_TYPE ( right_direct, i )
                     )
               )
              { return FALSE; }
          }
        for ( i = 0; i < TYPE_M3_OBJ_NFIELDS ( left_direct ); i ++ )
          { if ( strcmp ( TYPE_M3_OBJ_FIELD_NAME ( left_direct, i ),
                          TYPE_M3_OBJ_FIELD_NAME ( right_direct, i )
                        )
                 != 0
               )
              { return FALSE; }
            if ( ! m3_types_equal
                     ( TYPE_M3_OBJ_FIELD_TYPE ( left_direct, i ),
                       TYPE_M3_OBJ_FIELD_TYPE ( right_direct, i )
                     )
               )
              { return FALSE; }
            /* Perhaps it would be safe to assume that if the types
               are really the same, then the compiler will have ensured
               the bit packing is the same too, but then there is no
               whole-object assignment or comparison, so be paranoid. */
            if ( TYPE_M3_OBJ_FIELD_BITPOS ( left_direct, i )
                 != TYPE_M3_OBJ_FIELD_BITPOS ( right_direct, i )
               )
              { return FALSE; }
            if ( TYPE_M3_OBJ_FIELD_BITSIZE ( left_direct, i )
                 != TYPE_M3_OBJ_FIELD_BITSIZE ( right_direct, i )
               )
              { return FALSE; }
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
          { return FALSE; }
        if ( ! m3_type_fields_equal ( left_direct, right_direct ) )
          { return FALSE; }
        for ( i = 0; i < TYPE_NFIELDS ( left_direct ); i ++ )
          { if ( TYPE_FIELD_NAME ( left_direct, i )
                 != TYPE_FIELD_NAME ( right_direct, i )
               )
              { return FALSE; }
          }
        return TRUE;

      case TYPE_CODE_M3_ADDRESS :
      case TYPE_CODE_M3_BOOLEAN :
      case TYPE_CODE_M3_CHAR :
      case TYPE_CODE_M3_WIDECHAR :
      case TYPE_CODE_M3_INTEGER :
      case TYPE_CODE_M3_CARDINAL :
      case TYPE_CODE_M3_LONGINT :
      case TYPE_CODE_M3_LONGCARD :
      case TYPE_CODE_M3_REFANY :
      case TYPE_CODE_M3_TRANSIENT_REFANY :
      case TYPE_CODE_M3_ROOT :
      case TYPE_CODE_M3_TRANSIENT_ROOT :
      case TYPE_CODE_M3_UN_ROOT :
      case TYPE_CODE_M3_MUTEX :
      case TYPE_CODE_M3_TEXT :
      case TYPE_CODE_M3_NULL :
      case TYPE_CODE_M3_VOID :
        /* There is only one type with each of these codes. */
        return TYPE_CODE ( left_direct ) == TYPE_CODE ( right_direct );
      default : { return FALSE; }
    } /* switch */
} /* m3_types_equal */

/* Call this only if left_type is known to be neither indirect nor packed and
   an ordinal type and left tier <= right tier. */
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

    right_type = m3_unpacked_direct_type ( right_type );
    switch ( TYPE_CODE ( right_type ) )
      { case TYPE_CODE_M3_SUBRANGE :
          right_base_type = TYPE_M3_SUBRANGE_TARGET ( right_type );
        case TYPE_CODE_M3_BOOLEAN :
        case TYPE_CODE_M3_CHAR :
        case TYPE_CODE_M3_WIDECHAR :
        case TYPE_CODE_M3_INTEGER :
        case TYPE_CODE_M3_CARDINAL :
        case TYPE_CODE_M3_LONGINT :
        case TYPE_CODE_M3_LONGCARD :
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
    /* Here, left is a subrange and right could be too. */
    m3_ordinal_bounds ( left_type , & left_lower, & left_upper );
    m3_ordinal_bounds ( right_type , & right_lower, & right_upper );
    if ( left_lower == right_lower && left_upper == right_upper )
      /* Bounds are equal. */
      { if ( right_type != right_base_type )
          /* Both are subranges. */
          { return subtype_equal; }
      else /* left is a subrange of right, with equal bounds. */
        { return subtype_sub; }
      }
    if ( left_lower <= right_lower && left_upper >= right_upper )
      /* Right must be a subrange too.  Left is supertype. */
      { return subtype_super; }
    if ( left_lower >= right_lower && left_upper <= right_upper )
      { return subtype_sub; } /* Left is subtype. */
    return subtype_norel;
  } /* m3_ordinal_subtype_relation */

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

  code1 = TYPE_CODE (value_type (arg1));
  code2 = TYPE_CODE (value_type (arg2));

  /* FIXME: This need a lot of rework.  */

  if ( (code1 == TYPE_CODE_M3_INTEGER && code2 == TYPE_CODE_M3_INTEGER)
       || (code1 == TYPE_CODE_M3_CARDINAL && code2 == TYPE_CODE_M3_CARDINAL)
       || (code1 == TYPE_CODE_M3_LONGINT && code2 == TYPE_CODE_M3_LONGINT)
       || (code1 == TYPE_CODE_M3_LONGCARD && code2 == TYPE_CODE_M3_LONGCARD)
     )
    { return m3_value_as_integer (arg1) == m3_value_as_integer (arg2); }
  else if (code1 == TYPE_CODE_FLT && code2 == TYPE_CODE_M3_INTEGER)
    { return m3_value_as_float (arg1) == (double) m3_value_as_integer (arg2); }
  else if (code2 == TYPE_CODE_FLT && code1 == TYPE_CODE_M3_INTEGER)
    { return m3_value_as_float (arg2) == (double) m3_value_as_integer (arg1); }
  else if (code1 == TYPE_CODE_FLT && code2 == TYPE_CODE_FLT)
    { return m3_value_as_float (arg1) == m3_value_as_float (arg2); }

  /* FIXME: Need to promote to LONGEST. is bigger.  */
  else if (code1 == TYPE_CODE_M3_POINTER && code2 == TYPE_CODE_M3_INTEGER)
    { return
        m3_value_as_address (arg1) == (CORE_ADDR) m3_value_as_integer (arg2);
    }
  else if (code2 == TYPE_CODE_M3_POINTER && code1 == TYPE_CODE_M3_INTEGER)
    { return
        (CORE_ADDR) m3_value_as_integer (arg1) == m3_value_as_address (arg2);
    }

  else if ( code1 == code2
             && ( ( len = TYPE_LENGTH (value_type (arg1)))
                  == TYPE_LENGTH (value_type (arg2))
                )
          )
    { p1 = value_contents (arg1);
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
      error (_("Invalid type combination in equality test."));
      return 0;  /* For lint -- never reached */
    }
}

static BOOL
m3_value_less (struct value *arg1, struct value *arg2)
{
  struct type * type1;
  struct type * type2;
  enum type_code code1;
  enum type_code code2;
  struct type * base_type1;
  struct type * base_type2;
  LONGEST l1;
  LONGEST l2;
  DOUBLEST d1;
  DOUBLEST d2;
  CORE_ADDR a1;
  CORE_ADDR a2;

  type1 = value_type ( arg1 );
  type2 = value_type ( arg2 );
  code1 = TYPE_CODE ( type1 );
  code2 = TYPE_CODE ( type2 );

  if ( m3_is_ordinal_type ( type1 ) && m3_is_ordinal_type ( type2 ) )
    { base_type1 = m3_ordinal_base_type ( type1, NULL );
      base_type2 = m3_ordinal_base_type ( type2,NULL  );
      if ( m3_types_equal ( base_type1, base_type2 ) )
        { l1 = value_as_long ( arg1 );
          l2 = value_as_long ( arg2 );
          return l1 < l2;
        }
    }

  else if ( code1 == TYPE_CODE_FLT && code2 == TYPE_CODE_FLT )
    { if ( m3_types_equal ( type1, type2 ) )
        { /* NOTE: kettenis/20050816: Avoid compiler bug on systems where
         `   long double' values are returned in static storage (m68k).  */
          d1 = value_as_double ( arg1 );
          d2 = value_as_double ( arg2 );
          /* FIXME: ^This seems very unlikely to be right for the 3 Modula-3
                     floating types. */
          /* FIXME: This also probably doesn't agree with the M3 definition
                    for < and > on floating types. */
          return d1 < d2;
        }
    }
  else if ( code1 == TYPE_CODE_M3_ADDRESS && code2 == TYPE_CODE_M3_ADDRESS )
    { a1 = value_as_address ( arg1 );
      a2 = value_as_address ( arg2 );
      return a1 < a2;
    }
  else if ( code1 == TYPE_CODE_SET && code2 == TYPE_CODE_M3_SET )
    { /* Is arg1 a proper subset of arg2? */
      LONGEST l1 = value_as_long ( arg1 );
      LONGEST l2 = value_as_long ( arg2 );
      return ( l1 != l2 ) && ( l1 & ! l2 ) == 0;
    }

  error (_("Invalid type combination in M3 ordering comparison.") );
  /* NORETURN*/ return 0;
} /* m3_value_less */

/* The purpose of tiers is just to cut down the portion of the cartesian
   square of type codes that has to be checked to a triangular matrix.
   Indirects and opaques need to be removed first, on both left and
   right types.

   After that, the tiers order the types by their code for subtype
   checking.  This system is crafted so that if
   m3_type_code_tier ( tc1 ) < m3_type_code_tier ( tc2 ),
   then, loosely, m3_subtype_relation ( tc1, tc2 ) != subtype_super.
   Also, the reference types are all at the high end and ordered
   such that if tc1 is a reference type code and
   m3_type_code_tier ( tc1 ) < m3_type_code_tier ( tc2 )
   then loosely, m3_subtype_relation ( tc1, tc2 ) == subtype_sub,
   except for different types with TYPE_CODE_M3_OBJECT and reference
   class.
*/

static int
m3_type_code_tier ( enum type_code code )

  { switch ( code )
      { case TYPE_CODE_M3_INDIRECT :
          return 0;
        case TYPE_CODE_M3_OPAQUE :
          return 1;
        case TYPE_CODE_M3_PACKED :
          return 2;
        case TYPE_CODE_M3_SET :
        case TYPE_CODE_M3_RECORD :
        case TYPE_CODE_M3_METHOD :
        case TYPE_CODE_M3_VOID :
          return 3;
        case TYPE_CODE_M3_ARRAY :
          return 4;
        case TYPE_CODE_M3_OPEN_ARRAY :
          return 5;
        case TYPE_CODE_M3_SUBRANGE :
          return 6;
        case TYPE_CODE_M3_BOOLEAN :
        case TYPE_CODE_M3_CHAR :
        case TYPE_CODE_M3_WIDECHAR :
        case TYPE_CODE_M3_INTEGER :
        case TYPE_CODE_M3_CARDINAL :
        case TYPE_CODE_M3_LONGINT :
        case TYPE_CODE_M3_LONGCARD :
        case TYPE_CODE_M3_ENUM :
          return 7;
        case TYPE_CODE_M3_PROC :
        case TYPE_CODE_M3_PROC_CLOSURE :
        case TYPE_CODE_FUNC :
          return 8;
        case TYPE_CODE_M3_NULL :
          return 9;
        case TYPE_CODE_M3_TEXT :
        case TYPE_CODE_M3GDB_STRING :
        case TYPE_CODE_M3GDB_WIDESTRING :
          return 10;
        case TYPE_CODE_M3_POINTER :
          return 11;
        case TYPE_CODE_M3_MUTEX :
        case TYPE_CODE_M3_OBJECT :
          return 12;
        case TYPE_CODE_M3_ROOT :
        case TYPE_CODE_M3_TRANSIENT_ROOT :
        case TYPE_CODE_M3_UN_ROOT :
          return 13;
        case TYPE_CODE_M3_REFANY :
        case TYPE_CODE_M3_TRANSIENT_REFANY :
        case TYPE_CODE_M3_ADDRESS :
          return 14;
        default :
          return - 1;
      } /* switch */
  } /* m3_type_code_tier */

/* Strips off indirects, packeds, and opaques only if this leads to a
   reference type.  Otherwise, identity.
*/
static struct type *
m3_allocated_type ( struct value * val )

  { struct type * val_type;
    struct type * direct_type;
    CORE_ADDR val_contents;

    val_type = value_type ( val );
    if ( m3_value_is_type ( val ) )
      { return val_type; }
    else
      { direct_type = m3_revealed_unpacked_direct_type ( val_type );
        switch ( TYPE_CODE ( direct_type ) )
          {
            case TYPE_CODE_M3_ADDRESS:
              val_contents = value_as_address ( val );
              if ( val_contents == 0 )
                { return builtin_type_m3_null; }
              return direct_type;

            case TYPE_CODE_M3_POINTER:
              val_contents = value_as_address ( val );
              if ( val_contents == 0 )
                { return builtin_type_m3_null; }
              if ( ( val_contents % 2 ) != 0 )
                { return builtin_type_m3_integer; }
              /* ^Treat misaligned pseudo-pointer as integer. */  
              if ( TYPE_M3_POINTER_TRACED ( direct_type ) )
                { return m3_allocated_type_from_object_addr ( val_contents ); }
              return direct_type;

            case TYPE_CODE_M3_REFANY:
            case TYPE_CODE_M3_TRANSIENT_REFANY:
            case TYPE_CODE_M3_ROOT:
            case TYPE_CODE_M3_TRANSIENT_ROOT:
            case TYPE_CODE_M3_UN_ROOT:
            case TYPE_CODE_M3_OBJECT:
            case TYPE_CODE_M3_MUTEX:
            case TYPE_CODE_M3_TEXT:
              val_contents = value_as_address ( val );
              if ( val_contents == 0 )
                { return builtin_type_m3_null; }
              if ( ( val_contents % 2 ) != 0 )
                { return builtin_type_m3_integer; }
              /* ^Treat misaligned pseudo-pointer as integer. */  
              return m3_allocated_type_from_object_addr ( val_contents );

            case TYPE_CODE_M3_NULL:
              return builtin_type_m3_null;
            default:
              return val_type;
          }
      }
  } /* m3_allocated_type*/

/* FIXME: This has been replaced by m3_types_equal.  Take it out. */
static BOOL
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
            { return FALSE; }
          if ( TYPE_M3_OBJ_TRACED ( left ) != TYPE_M3_OBJ_TRACED ( right ) )
            { return FALSE; }
          if ( TYPE_M3_OBJ_BRANDED ( left ) != TYPE_M3_OBJ_BRANDED ( right ) )
            { return FALSE; }
          if ( TYPE_M3_OBJ_BRANDED ( left )
               && TYPE_M3_OBJ_BRAND ( left ) != TYPE_M3_OBJ_BRAND ( right )
             )
            { return FALSE; }
          if ( TYPE_M3_OBJ_NFIELDS ( left ) != TYPE_M3_OBJ_NFIELDS ( right ) )
            { return FALSE; }
          if ( TYPE_M3_OBJ_NMETHODS ( left ) != TYPE_M3_OBJ_NMETHODS ( right ) )
            { return FALSE; }
          for ( i = 0; i < TYPE_M3_OBJ_NMETHODS ( left ); i ++ )
            { if ( strcmp ( TYPE_M3_OBJ_METHOD_NAME ( left, i ),
                            TYPE_M3_OBJ_METHOD_NAME ( right, i )
                          )
                   != 0
                 )
                { return FALSE; }
              if ( ! m3_types_equal
                       ( TYPE_M3_OBJ_METHOD_TYPE ( left, i ),
                         TYPE_M3_OBJ_METHOD_TYPE ( right, i )
                       )
                 )
                { return FALSE; }
            }
          for ( i = 0; i < TYPE_M3_OBJ_NFIELDS ( left ); i ++ )
            { if ( strcmp ( TYPE_M3_OBJ_FIELD_NAME ( left, i ),
                            TYPE_M3_OBJ_FIELD_NAME ( right, i )
                          )
                   != 0
                 )
                { return FALSE; }
              if ( ! m3_types_equal
                       ( TYPE_M3_OBJ_FIELD_TYPE ( left, i ),
                         TYPE_M3_OBJ_FIELD_TYPE ( right, i )
                       )
                 )
                { return FALSE; }
            }
          return m3_equal_object_types
                   ( TYPE_M3_OBJ_SUPER ( left ), TYPE_M3_OBJ_SUPER ( right ) );
        default:
          return FALSE;
      }

  }  /* m3_equal_object_types */

/* Return the number of reference type ancestors start_type has. */
static int
reference_type_depth ( struct type * start_type )

{ struct type * l_type;
  int result;

  result = 0;
  l_type = start_type;
  while ( TRUE )
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
            return result + ( int ) m3_compiler_kind ( ) == m3_ck_cm3;

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
   PRE: Indirects, packed, and opaque have been removed from 'text_type'.
*/
static BOOL
is_pm3_text_revelation ( struct type * text_type )

{ struct type * array_type;
  struct type * elem_type;

  if ( text_type == NULL )
    { return FALSE; }
  if ( m3_compiler_kind ( ) != m3_ck_pm3  )
    { return FALSE; }
  switch ( TYPE_CODE ( text_type ) )
    { case TYPE_CODE_M3_TEXT:
      case TYPE_CODE_M3GDB_STRING:
      case TYPE_CODE_M3GDB_WIDESTRING:
        return TRUE;
      case TYPE_CODE_M3_POINTER:
        if ( ! TYPE_M3_POINTER_BRANDED ( text_type ) )
          { return FALSE; }
        if ( strcmp ( TYPE_M3_POINTER_BRAND ( text_type ), "Text-1.0" ) != 0 )
          { return FALSE; }
        array_type = TYPE_M3_POINTER_TARGET ( text_type );
        if ( array_type == NULL
             || TYPE_CODE ( array_type ) != TYPE_CODE_M3_OPEN_ARRAY
           )
          { return FALSE; }
        elem_type = TYPE_M3_OPEN_ARRAY_ELEM ( array_type );
        if ( elem_type == NULL
             || TYPE_CODE ( elem_type ) != TYPE_CODE_M3_CHAR
           )
          { return FALSE; }
        return TRUE;
      default:
        return FALSE;
    }
} /* is_pm3_text_revelation */

/* Is this type CM3 TEXT, its revelation, or any subtype thereof?
   PRE: Indirects, packed, and opaque have been removed from 'text_type'.
*/
static BOOL
is_cm3_text_subtype ( struct type * text_type )

{ struct type * supertype;

  if ( m3_compiler_kind ( ) != m3_ck_cm3 )
    { return FALSE; }
  supertype = text_type;
  while ( TRUE )
    { if ( supertype == NULL )
        { return FALSE; }
      switch ( TYPE_CODE ( supertype ) )
        { case TYPE_CODE_M3_TEXT:
          case TYPE_CODE_M3GDB_STRING:
          case TYPE_CODE_M3GDB_WIDESTRING:
            return TRUE;
          case TYPE_CODE_M3_OBJECT:
            if ( TYPE_M3_POINTER_BRANDED ( supertype )
                 && strcmp ( TYPE_M3_POINTER_BRAND ( supertype ), "Text-2.0" )
                    == 0 )
              { return TRUE; }
            supertype = TYPE_M3_OBJ_SUPER ( supertype );
          default:
            return FALSE;
        }
    }
} /* is_cm3_text_subtype */

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
  /* Avoid stripping off packed, because it affects the subtype relation. */
    left_direct = m3_revealed_type ( left );
    right_direct = m3_revealed_type ( right );

    if ( left_direct == right_direct )
      { return subtype_equal; }
    if ( m3_types_equal ( left_direct, right_direct ) )
      { return subtype_equal; }
    left_code = TYPE_CODE ( left_direct );
    right_code = TYPE_CODE ( right_direct );
    left_tier = m3_type_code_tier ( left_code );
    right_tier = m3_type_code_tier ( right_code );
    /* Swap operands so that left_tier <= right_tier. */
    if ( left_tier > right_tier )
      { return commute_subtype_relation
                 ( m3_subtype_relation ( right_direct, left_direct ) );
      }

    /* Here: left, left_direct, right, and right-direct are non-NULL.
             left_code and right-code are neither TYPE_CODE_M3_INDIRECT
               nor TYPE_CODE_M3_OPAQUE.
             left_direct and right_direct are not equal types.
             left_tier <= right_tier
    */
    switch ( left_code )
      { case TYPE_CODE_M3_PACKED :
          if ( right_code == TYPE_CODE_M3_PACKED ) /* And they're unequal. */
            { return subtype_norel; }
          child_rel
            = m3_subtype_relation
                ( TYPE_M3_PACKED_TARGET ( left_direct ) , right_direct );
          if ( child_rel == subtype_equal )
            { return subtype_both; }
          else /* This case covers transitive subtypes. */
            { return child_rel; }

        case TYPE_CODE_M3_VOID :
          return right_code == left_code;

        case TYPE_CODE_M3_ARRAY :
        case TYPE_CODE_M3_OPEN_ARRAY :
        case TYPE_CODE_M3_PROC :
        case TYPE_CODE_M3_PROC_CLOSURE :
          error (_( "Subtype relation not implemented for array or "
                    "procedure types.")
                ); /* NORETURN */

        case TYPE_CODE_M3_SET :
        case TYPE_CODE_M3_RECORD :
        case TYPE_CODE_M3_METHOD :
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
        case TYPE_CODE_M3_LONGINT :
        case TYPE_CODE_M3_LONGCARD :
        case TYPE_CODE_M3_ENUM :
          return m3_ordinal_subtype_relation
                   ( left_direct, left_direct, right_direct );

        case TYPE_CODE_M3_NULL :
          if ( left_code == right_code ) /* Probably can't happen. */
            { return subtype_equal; }
          else if ( left_tier < right_tier )
            /* right's reference class is irrelevant. */
            { return subtype_sub; }
          else { return subtype_norel; }

        case TYPE_CODE_M3_POINTER :
          switch ( right_code )
            { case TYPE_CODE_M3_REFANY :
              case TYPE_CODE_M3_TRANSIENT_REFANY :
                if ( TYPE_M3_POINTER_TRACED ( left_direct ) )
                  { return subtype_sub; }
                else { return subtype_norel; }
              case TYPE_CODE_M3_ADDRESS :
                if ( ! TYPE_M3_POINTER_TRACED ( left_direct ) )
                  { return subtype_sub; }
                else { return subtype_norel; }
              default :
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
            } /* switch ( right_code ) */

        case TYPE_CODE_M3_TEXT :
        case TYPE_CODE_M3GDB_STRING :
        case TYPE_CODE_M3GDB_WIDESTRING :
          switch ( right_code )
            { case TYPE_CODE_M3_TEXT:
              case TYPE_CODE_M3GDB_STRING:
              case TYPE_CODE_M3GDB_WIDESTRING:
                { return subtype_equal; }
                  /* ^Even though the m3gdb representation might differ. */
              case TYPE_CODE_M3_REFANY:
              case TYPE_CODE_M3_TRANSIENT_REFANY:
                { return subtype_sub; }
              case TYPE_CODE_M3_POINTER:
                if ( is_pm3_text_revelation ( right_direct ) )
                  { return subtype_equal; }
                else { return subtype_norel; }
              case TYPE_CODE_M3_OBJECT:
                if ( is_cm3_text_subtype ( right_direct ) )
                  { return subtype_sub; }
                else { return subtype_norel; }
              default : return subtype_norel;             }

        case TYPE_CODE_M3_MUTEX :
        case TYPE_CODE_M3_OBJECT :
          switch ( right_code )
            { case TYPE_CODE_M3_ROOT:
              case TYPE_CODE_M3_TRANSIENT_ROOT:
              case TYPE_CODE_M3_REFANY:
              case TYPE_CODE_M3_TRANSIENT_REFANY:
                { return subtype_sub; }
              case TYPE_CODE_M3_UN_ROOT:
              case TYPE_CODE_M3_ADDRESS:
                { return subtype_norel; }

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
                      return subtype_norel;
                    }
                  else if ( left_depth > right_depth )
                    { while ( left_depth > right_depth )
                        { left_super = TYPE_M3_OBJ_SUPER ( left_super );
                          left_depth --;
                        }
                      if ( m3_types_equal ( left_super, right_super ) )
                        { return subtype_sub; }
                      return subtype_norel;
                    }
                  else if ( m3_types_equal ( left_super, right_super ) )
                    { return subtype_equal; }
                  return subtype_norel;
                }
              default:
                return subtype_norel;
            } /* switch ( right_code ) */

        case TYPE_CODE_M3_REFANY :
        case TYPE_CODE_M3_TRANSIENT_REFANY :
        case TYPE_CODE_M3_ADDRESS :
        case TYPE_CODE_M3_ROOT :
        case TYPE_CODE_M3_TRANSIENT_ROOT :
        case TYPE_CODE_M3_UN_ROOT :
          if ( left_code == right_code ) /* Probably can't happen. */
            { return subtype_equal; }
          else if ( left_tier < right_tier
                    && TYPE_M3_POINTER_TRACED ( left_direct )
                       == TYPE_M3_POINTER_TRACED ( right_direct )
                  )
            { return subtype_sub; }
          else { return subtype_norel; }

        default : { return subtype_norel; }
      } /* switch ( left_code ) */
  } /* m3_subtype_relation */

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
    BOOL lhs_is_int_or_card = FALSE;
    BOOL rhs_is_int_or_card = FALSE;

    lhs_base_type = m3_ordinal_base_type ( lhs_type, & lhs_is_int_or_card );
    if ( lhs_base_type == NULL ) { return NULL; }
    m3_ordinal_bounds ( lhs_type, & lhs_lower, & lhs_upper );
    rhs_base_type = m3_ordinal_base_type ( rhs_type, & rhs_is_int_or_card  );
    if ( rhs_base_type == NULL )
      { if ( proc_name == NULL )
          { error
              (_("Type not assignable to ordinal type.")); /* NORETURN */
          }
        else
          { error
              (_("Type not assignable to ordinal type, "
                 " formal \"%s\" of procedure \"%s\", "),
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
              (_("Ordinal types with different base types.")); /* NORETURN */
          }
        else
          { error
              (_("Ordinal types with different base types, "
                  " formal \"%s\" of procedure \"%s\", "),
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
              (_("Ordinal types with disjoint ranges.")); /* NORETURN */
          }
        else
          { error
              (_("Ordinal types with disjoint ranges, "
                 " formal \"%s\" of procedure \"%s\", "),
                formal_name, proc_name
              ); /* NORETURN */
          }
      }
    else if ( contents < lhs_lower || lhs_upper < contents )
      { /* value not in LHS range. */
        if ( proc_name == NULL )
          { error
              (_("Value out of range.")); /* NORETURN */
          }
        else
          { error
              (_("Value out of range, "
                 " formal \"%s\" of procedure \"%s\", "),
                formal_name, proc_name
              ); /* NORETURN */
          }
      }
    result_value = m3_value_from_longest ( lhs_base_type, contents );
    return result_value;

  } /* m3_check_and_coerce_ordinal */

static struct value *
m3_proc_value_from_qualified_name (
    const char * unit_name, const char * proc_name )

  { struct symbol * proc_sym;
    struct value * proc_val;

    proc_sym = m3_lookup_interface_id ( unit_name, proc_name, NULL );
    if ( proc_sym == NULL )
      { proc_sym = m3_lookup_module_id ( unit_name, proc_name, NULL ); }
    if ( proc_sym == NULL )
      { error
         (_("Can't find builtin procedure %s.%s"), unit_name, proc_name);
        /* NORETURN */
      }
    proc_val = read_var_value ( proc_sym, NULL );
    if ( proc_val == NULL )
      { error
         (_("Can't evaluate builtin procedure %s.%s"), unit_name, proc_name);
        /* NORETURN */
      }
    return proc_val;
  } /* m3_proc_value_from_qualified_name */

static const char * FromChars_proc_name = "FromChars";
static const char * FromWideChars_proc_name = "FromWideChars";

/* m3gdb_string might be a value of a string of chars or wide chars that is
   located in m3gdb space.  If so, convert it to a TEXT in inferior space,
   by making a call on Text.FromChars or Text.FromWideChars.  Otherwise,
   identity. */
static struct value *
m3_coerce_m3gdb_string ( struct value * m3gdb_string )

  { struct type * m3gdb_type;
    enum type_code code;
    LONGEST length; /* Always in bytes. */
    const char * proc_name;
    struct symbol * proc_sym;
    struct value * argvec [ 2 ];
    struct value * result;
    struct value * proc_val;

    m3gdb_type = value_type ( m3gdb_string );
    code = TYPE_CODE ( m3gdb_type );
    length = TYPE_LENGTH ( m3gdb_type );
    switch ( code )
      { case TYPE_CODE_M3GDB_STRING:
          proc_name = FromChars_proc_name;
          break;
        case TYPE_CODE_M3GDB_WIDESTRING:
          proc_name = FromWideChars_proc_name;
          /* ^We disallow wide text literals from being scanned unless this
             is cm3-compiled code, so FromWideChars will exist in m3core. */
          break;
        default:
          return m3gdb_string;
      }
    proc_val = m3_proc_value_from_qualified_name ( "Text", proc_name );
    if ( proc_val == NULL ) /* Shouldn't be possible. */
      { return NULL; }
    argvec [ 0 ] = m3gdb_string;
    argvec [ 1 ] = NULL;
    result = call_function_by_hand ( proc_val, 1, argvec);
    return result;
  } /* m3_coerce_m3gdb_string */

/* Handle value conversion of a reference value for either assignment
   or parameter passing.  Returns an appropriate struct value * if the
   types are reference types and everything is OK.  Displays an error
   (and thus doesn't return) if reference types are involved but
   something is wrong.  Returns NULL if reference types are
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
    struct type * lhs_revealed_type;
    struct type * rhs_revealed_type;
    enum type_code rhs_revealed_type_code;

    lhs_revealed_type = m3_revealed_unpacked_direct_type ( lhs_type );
    rhs_revealed_type = m3_revealed_unpacked_direct_type ( rhs_type );

    switch ( TYPE_CODE ( lhs_revealed_type ) )
      { case TYPE_CODE_M3_REFANY:
        case TYPE_CODE_M3_TRANSIENT_REFANY:
        case TYPE_CODE_M3_ADDRESS:
        case TYPE_CODE_M3_POINTER:
        case TYPE_CODE_M3_ROOT:
        case TYPE_CODE_M3_TRANSIENT_ROOT:
        case TYPE_CODE_M3_UN_ROOT:
        case TYPE_CODE_M3_OBJECT:
        case TYPE_CODE_M3_MUTEX:
        case TYPE_CODE_M3_TEXT:
        case TYPE_CODE_M3_NULL:
          /* LHS is a reference type */
          rhs_revealed_type_code = TYPE_CODE ( rhs_revealed_type );
          switch ( rhs_revealed_type_code )
            { case TYPE_CODE_M3_REFANY:
              case TYPE_CODE_M3_TRANSIENT_REFANY:
              case TYPE_CODE_M3_ADDRESS:
              case TYPE_CODE_M3_POINTER:
              case TYPE_CODE_M3_ROOT:
              case TYPE_CODE_M3_TRANSIENT_ROOT:
              case TYPE_CODE_M3_UN_ROOT:
              case TYPE_CODE_M3_OBJECT:
              case TYPE_CODE_M3_MUTEX:
              case TYPE_CODE_M3_TEXT:
              case TYPE_CODE_M3_NULL:
              case TYPE_CODE_M3GDB_STRING:     /* These can occur only on */
              case TYPE_CODE_M3GDB_WIDESTRING: /* the RHS */
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
                      { return m3_coerce_m3gdb_string ( rhs_value ); }
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
                              { error (_("NARROW failure.")); /* NORETURN */ }
                            else
                              { error
                                 (_("NARROW failure, "
                                    "formal \"%s\" of procedure \"%s\""),
                                   formal_name, proc_name
                                 ); /* NORETURN */
                              }
                        }
                      break;
                  }
              default:
                break;
            }
        default:
          return NULL;
      }
    if ( proc_name == NULL )
      { error (_("Reference type not assignable.")); /* NORETURN */ }
    else
      { error
         (_("Reference type not assignable, "
            "formal \"%s\" of procedure \"%s\""),
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
    { while ( TRUE )
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
   rhs_value, if nothing has to be done, or is constructed dope otherwise. */
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
                          (_("Assigning an array constructor to an open array "
                             "is not supported.")
                          ); /* NORETURN */
                      }
                    else
                      { error
                          (_("Passing an array constructor to an open array "
                             "formal is not supported, "
                             " formal \"%s\" of procedure \"%s\""),
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
                  { error (_("Non-assignable types.")); /* NORETURN */ }
                else
                  { error
                      (_("Non-assignable types, "
                         " formal \"%s\" of procedure \"%s\""),
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
                  { error (_("Non-assignable types.")); /* NORETURN */ }
                else
                  { error
                      (_("Non-assignable types, "
                         " formal \"%s\" of procedure \"%s\""),
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
              (_("Unequal array dimensions, %d, and %d."),
                lhs_open_dims + lhs_fixed_dims,
                rhs_open_dims + rhs_fixed_dims
              ); /* NORETURN */
          }
        else
          { error
              (_("Unequal array dimensions, %d, and %d, "
                 "formal \"%s\" of procedure \"%s\""),
                lhs_open_dims + lhs_fixed_dims,
                rhs_open_dims + rhs_fixed_dims,
                formal_name, proc_name
              ); /* NORETURN */
          }
      }
    if ( ! m3_types_equal ( lhs_elem_type, rhs_elem_type ) )
      { if ( proc_name == NULL )
          { error (_("Unequal array element types.")); /* NORETURN */ }
        else
          { error
              (_("Unequal array element types,"
                 " formal \"%s\" of procedure \"%s\""),
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
    while ( TRUE )
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
                            (_("Shape check failure."
                               "dimension %d, lhs %d, rhs %d."),
                               dimension, lhs_shape_comp, rhs_shape_comp
                            ); /* NORETURN */
                        }
                      else
                        { error
                            (_("Shape check failure."
                               " formal \"%s\" of procedure \"%s\", "
                               "dimension %d, lhs %d, rhs %d."),
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
                        (_("Shape check failure."
                           "dimension %d, lhs %d, rhs %d."),
                           dimension, lhs_shape_comp, rhs_shape_comp
                        ); /* NORETURN */
                    }
                  else
                    { error
                        (_("Shape check failure."
                           " formal \"%s\" of procedure \"%s\", "
                           "dimension %d, lhs %d, rhs %d."),
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
                after it has done various inferior stack-aligning, etc.
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

   If it's a function and the result type is "small", it will have a local
   variable named "_result" whose type is what we want.  If the result type is
   "big" (i.e., compiled as if it were a VAR parameter), there will be a formal
   parameter, also named "_result", whose type is that of a VAR parameter of
   the result type.

   The type of "_result" is what is returned.

   In order to do this, We have to have the symbol for the actual procedure
   constant.  This will have a type whose code is TYPE_CODE_FUNC, and whose
   "fields" have been built from the separate N_PSYM stab entries that follow
   the N_FUN stab entry for the procedure.  It will have a constructed result
   type that is almost no help, except when it has a builtin type.  However,
   its block will have entries for the formals, also constructed from N_PSYM
   entries, and locals, constructed from N_LSYM entries.

   Procedure type stabs entries for procedure variables and methods are
   produced a little differently by the compilers, and don't contain enough
   information at all.  Fortunately, for evaluating a user-entered call, we
   can get the runtime address of the procedure constant first, then get its
   symbol from that.

   It is not clear that the type of "_result" will be available when the
   procedure constant symbol is constructed, so we delay this lookup until
   a user-typed call is being evaluated.

   Note that Modula-3 identifiers must begin with a letter, so the compiler-
   generated name "_result" can't be spoofed by the Modula-3 programmer.

   This function patches the result type of the procedure constant symbol
   and also returns its result type.  result_is_ref_param  (which may be NULL)
   is set to TRUE iff it is a result that is, at machine-code level,
   implemented as a VAR parameter.
*/
static struct type *
m3_patched_proc_result_type (
  CORE_ADDR code_addr, char * name, BOOL * result_is_ref_param )

{ struct symbol * proc_sym;
  struct type * proc_type;
  struct block * proc_block;
  struct block * body_block;
  struct symbol * result_sym;
  struct type * result_type;
  BOOL l_result_is_ref_param;

  if ( result_is_ref_param != NULL ) { * result_is_ref_param = FALSE; }
  proc_sym = find_pc_function ( code_addr );
  if ( proc_sym == NULL )
    { error
       (_("Can't get symbol for procedure \"%s\"."), name ); /* NORETURN */ }
  proc_type = SYMBOL_TYPE ( proc_sym );
  proc_block = SYMBOL_BLOCK_VALUE ( proc_sym );
  result_sym
    = lookup_block_symbol ( proc_block, "_result", NULL, VAR_DOMAIN );
  if ( result_sym == NULL )
    { body_block = m3_proc_body_block ( proc_block );
      if ( body_block != NULL )
        { result_sym
            = lookup_block_symbol
                ( body_block, "_result", NULL, VAR_DOMAIN );
        }
    }
  if ( result_sym == NULL ) /* No result type => proper procedure. */
    /* CHECK:  This could happen just because of inadequate debug info,
               even for a function procedure.  Is there anything we
               can do? */
    { l_result_is_ref_param = FALSE;
      result_type = builtin_type_m3_void;
      TYPE_M3_FUNC_RESULT_CODE ( proc_type ) = m3_res_none;
    }
  else
    { l_result_is_ref_param = ( SYMBOL_CLASS ( result_sym ) == LOC_ARG );
      result_type = SYMBOL_TYPE ( result_sym );
      gdb_assert
        ( l_result_is_ref_param
          == ( TYPE_CODE ( result_type ) == TYPE_CODE_M3_INDIRECT )
          /* ^call_function_by_hand will need this to ascertain whether the
              result is passed by reference. */
        );
      if ( l_result_is_ref_param )
        { if ( strcmp ( TYPE_FIELD_NAME ( proc_type, 0 ), "_result" ) == 0 )
            { TYPE_M3_FUNC_RESULT_CODE ( proc_type ) = m3_res_leftmost; }
          else if ( strcmp
                     ( TYPE_FIELD_NAME
                         ( proc_type, TYPE_NFIELDS ( proc_type ) - 1 ),
                       "_result" ) == 0
                     )
            { TYPE_M3_FUNC_RESULT_CODE ( proc_type ) = m3_res_rightmost; }
          else { gdb_assert ( FALSE ); }
        }
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
   objects whose value is the pointer (to either kind), and whose type has
   TYPE_CODE_M3_PROC.  In evaluating user-typed expressions and statements,
   gdb checks which kind it is when:
     1) Trying to assign it to a variable.  Only top-level is legal.
     2) Calling the denoted procedure, which requires different call
        mechanisms for the two cases.
     3) Printing its value, which also has to be done differently.
   When passing it as a parameter, which kind it is does not matter.  The
   pointer itself is just passed.

   m3gdb also constructs closures in gdb space that don't exist in inferior
   space.  These are represented by a struct value node whose value is the
   entire 3-word closure, and whose type has TYPE_CODE_M3_PROC_CLOSURE and
   whose TYPE_TARGET_TYPE is the type of the procedure constant (which has
   TYPE_CODE_FUNC).  These have a relatively short lifetime.  This is just
   a trick to get extra information into call_function_by_hand and several
   other functions it can call, including many target-dependent functions,
   without changing their parameter lists.  These gdb-space closures are built:
     1) When a user-typed call on a nested procedure constant is evaluated.
        The closure is built and passed in to call_function_by_hand as the
        function to be called.  There it will be recognized and used by
        m3-dependent code to pass the static link.
     2) When a user_typed call that passes a nested procedure constant as
        an actual parameter is evaluated.  It is later used by m3-dependent
        code to push a copy of the closure onto the inferior stack and then
        pass a pointer to this closure.
*/

/* If proc_const_value is a value for a procedure constant that is nested,
   return a value for a (gdb-space) closure for that procedure, using the
   current, user-selected frame to construct its environment pointer.
   Nested or not, if inf_code_addr_result is non-NULL, set it to the
   inferior code address of the procedure. */
static struct value *
m3_nested_proc_const_closure (
    struct value * proc_const_value,
    CORE_ADDR * inf_code_addr_result
  )

{ struct type * proc_type;
  struct block * callee_block;
  struct block * callee_parent_proc_block;
  struct frame_info * callee_parent_frame;
  struct frame_info * referring_frame;
  struct value * result;
  CORE_ADDR inf_code_addr;
  CORE_ADDR inf_static_link;

  if ( inf_code_addr_result != NULL ) { * inf_code_addr_result = 0; }
  if ( proc_const_value == NULL ) { return NULL; }
  proc_type = value_type ( proc_const_value );
  if ( proc_type == NULL || TYPE_CODE ( proc_type ) != TYPE_CODE_FUNC )
    { return proc_const_value; }
  /* Types with TYPE_CODE_FUNC are constructed with length of 1 byte, but
     Modula-3 calls will use these as the type of a (procedure) parameter
     to be passed, whose size must be the size of a pointer. */
  TYPE_LENGTH ( proc_type ) = TARGET_PTR_BIT / TARGET_CHAR_BIT;
  inf_code_addr
    = VALUE_ADDRESS ( proc_const_value ) + value_offset ( proc_const_value );
  if ( inf_code_addr == 0 ) { return proc_const_value; }
  callee_block = block_for_pc ( inf_code_addr );
  callee_parent_proc_block
    = m3_block_proc_block ( BLOCK_SUPERBLOCK ( callee_block ) );
  if ( callee_parent_proc_block == NULL ) /* Not nested. */
    { result = proc_const_value; }
  else /* Nested procedure. */
    { referring_frame = deprecated_safe_get_selected_frame ( );
      /* TODO: ^Push this frame value up to call sites. */
      callee_parent_frame
        = m3_static_ancestor_frame
            ( referring_frame, callee_parent_proc_block, & inf_static_link);
      result
        = m3_build_gdb_proc_closure
            ( proc_type, inf_code_addr, inf_static_link );
    }
  if ( inf_code_addr_result != NULL )
    { * inf_code_addr_result = inf_code_addr; }
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

    if ( TYPE_CODE ( lhs_type ) != TYPE_CODE_M3_PROC )
      { return NULL; }
    if ( TYPE_CODE ( rhs_type ) == TYPE_CODE_M3_NULL )
      { return rhs_value; }
    if ( TYPE_CODE ( rhs_type ) != TYPE_CODE_M3_PROC
         && TYPE_CODE ( rhs_type ) != TYPE_CODE_FUNC
       )
      { return NULL; }
    /* FIXME: Do type check of two similar procedure types. */
    if ( FALSE )
      { if ( proc_name == NULL )
          { error (_("Procedure type not assignable.")); /* NORETURN */ }
        else
          { error
              (_("Actual parameter type not assignable to procedure "
                 " formal \"%s\" of procedure \"%s\""),
                formal_name, proc_name
              ); /* NORETURN */
          }
      }
    warning (_("m3gdb does not check procedure types.") );
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
                (_("Actual parameter type not equal for VAR/READONLY formal "
                  "\"%s\" of procedure \"%s\""),
                  formal_name, proc_name
                ); /* NORETURN */
            }
          if ( actual_direct_value == actual_value
               && ! VALUE_LVAL ( actual_direct_value )
             )
            { error
                (_("Actual parameter is not a designator for VAR/READONLY "
                  "formal \"%s\" of procedure \"%s\"."),
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

      /* Remaining cases just take address of coerced actual. */
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
        (_("Actual parameter type not assignable to formal \"%s\" "
           "of procedure \"%s\""),
          formal_name, proc_name
        ); /* NORETURN */
    }
  return NULL; /* Suppress warnings.  Shouldn't get here. */
} /* m3_check_and_coerce_actual */

static struct value *
m3_evaluate_call (
    struct value * proc_const_value,  /* Procedure constant to be called.  Must
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
    struct type * proc_type;
    BOOL result_is_ref_param;
    struct value * * argvec;
    int downward_nargs; /* number in argvec, passed at machine level. */
    int expected_args;
    int next_argvec;
    int next_formal;
    int next_actual; /* */

    next_formal = 0;
    expected_args = TYPE_NFIELDS ( check_actuals_type );
    if ( TYPE_CODE ( check_actuals_type ) == TYPE_CODE_M3_PROC )
      /* First "formal" in fields is actually result type, possibly void.
         We don't want a user-typed actual to correspond to it. */
      { next_formal ++;
        expected_args --;
      }
    result_type
      = m3_patched_proc_result_type ( code_addr, name, & result_is_ref_param );
    if ( result_type != NULL
         && TYPE_CODE ( result_type ) == TYPE_CODE_M3_INDIRECT
         && TYPE_CODE ( check_actuals_type ) == TYPE_CODE_FUNC
       ) /* First or last "formal" of check_actuals_type is really a large
            function result, passed by reference. */
      { expected_args --; }
    if ( nargs != expected_args )
      { error
         (_("Procedure \"%s\" requires %d parameters, but %d were supplied."),
           name,
           expected_args,
           nargs
         ); /* NORETURN */
      }
    downward_nargs = nargs + ( self != NULL ) + ( result_is_ref_param );
    /* ^Number of actuals in the machine code call. */
    argvec
      = ( struct value * * )
        alloca ( sizeof ( struct value * ) * ( downward_nargs + 1 ) );
    next_argvec = 0;
    proc_type = value_type ( proc_const_value );
    if ( result_is_ref_param
         && TYPE_M3_FUNC_RESULT_CODE ( proc_type ) == m3_res_leftmost
       )
      { argvec [ next_argvec ]
          = allocate_value ( result_type /* has TYPE_CODE_M3_INDIRECT */ );
      /* Setting value_contents_raw ( argvec [ i ])  will have to wait until
         the space for the result is pushed on the inferior stack, which
         happens inside call_function_by_hand. */
        set_value_lazy ( argvec [ next_argvec ], 0 );
        next_argvec ++;
      }
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
    if ( result_is_ref_param
         && TYPE_M3_FUNC_RESULT_CODE ( proc_type ) == m3_res_rightmost
       )
      { argvec [ next_argvec ]
          = allocate_value
              ( result_type /* See comments above for leftmost case. */ );
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
    set_value_bitpos ( result_val, bitpos % TARGET_CHAR_BIT );
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
  while ( TRUE )
    { if ( i >= nconsts )
        { error
            (("Enumeration type \"%s\" has no value named \"%s\"."),
              SYMBOL_NATURAL_NAME ( sym ) + 2 /* Strip off leading "B$" */ ,
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
   as the function result.  If this is a method, 'self' will be set to the
   latter value.  Otherwise, 'self' will be set to null. */
/* NOTE: m3_parse_e8, in m3-exp.c, during expression parsing, handles
         dot-constructs that are merely qualified references to declared
         entities.  So this code only needs to handle access to fields and
         methods of record and object values.
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
            (_("Attempt to implicitly dereference NIL, for record field "
              "\"%s\"."),
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
          { dot_value
              = m3_field_value ( lhs_value, bitsize, bitpos, dot_type );
            if ( out_field_name != NULL )
              { *out_field_name = field_name; }
             return dot_value;
          }
        else
          { error (_("Record has no field named \"%s\"."), field_name );
              /* NORETURN */
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
              (_("NIL object cannot have a selection (\".%s\") applied."),
                field_name
              ); /* NORETURN */
          }
        allocated_tc_addr = m3_tc_addr_from_inf_object_addr ( lhs_inf_addr );
        if ( m3_check_TextLiteral_buf
              ( lhs_inf_addr, allocated_tc_addr, field_name, & bitsize,
                & bitpos, & dot_type
              )
           )
          { dot_value = value_at_lazy ( dot_type, lhs_inf_addr );
            set_value_offset
              ( dot_value,
                value_offset ( dot_value ) + bitpos / TARGET_CHAR_BIT
              );
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
            if ( FALSE )
          { while ( supertype != lhs_type )
              { supertype_tc_addr
                  = m3_super_tc_addr_from_tc_addr ( supertype_tc_addr );
                supertype = m3_type_from_tc ( supertype_tc_addr );
              }
            supertype = lhs_type;
            /* Here, supertype_tc_addr and supertype are for the static type. */
          }
            /* Here, supertype_tc_addr and supertype are for the allocated type. */
            while ( TRUE ) /* Search supertypes for field/method. */
              { if ( TYPE_CODE ( supertype ) != TYPE_CODE_M3_OBJECT )
                  { error
                      (_("Object has no field or method named \"%s\".")
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
        error (_("A selection (\".%s\") can apply only to a RECORD, "
                "REF RECORD, or OBJECT."),
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
            (_("NIL indirect.  Modula-3 compiler shouldn't allow this to "
                "happen.")
            );
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
          { error (_("Attempt to call NIL procedure variable \"%s\"."), name );
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
        error (_("Attempt to call non-procedure \"%s\"."), name );
          /* NORETURN */
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
  else
    { lhs_direct_type = lhs_type;
      lhs_direct_value = lhs_value;
      if ( ! VALUE_LVAL ( lhs_direct_value ) )
        { error (_("LHS of assignment is not a designator.") );
            /* NORETURN */
        }
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

  error (_("RHS expression type not assignable to LHS type in "
           "assignment statement.")
        ); /* NORETURN */

  return NULL; /* Suppress warnings.  Shouldn't get here. */
} /* m3_check_and_coerce_assignment */

/* Build a struct type and struct value for a (possibly wide) text string,
   that, for now, will only exist in m3gdb space.  It takes quite a bit of
   work to get it into inferior space, so will postpone that unless/until
   it is known to be needed.  */
static struct value *
m3_evaluate_text_literal (
    char * value, LONGEST length, enum type_code code )

  { struct value *val;
    struct type * text_type;

    text_type = alloc_type (NULL);
    /* FIXME: ^Figure out how to create a cleanup that will deallocate
              this when the current command is done. */
    TYPE_CODE (text_type) = code;
    TYPE_LENGTH (text_type) = length;

    val = allocate_value (text_type);
    memcpy (value_contents_raw (val), value, length);
    return val;
  } /* m3_evaluate_text_literal */

/* If 'arg' is any ordinal value (including a type) that FIRST, LAST, or
   NUMBER can be applied to, return 'TRUE' and set the ordinal base type
   and bounds of 'arg'.  Write an error (and don't return) if something
   is known illegal.  Return FALSE if other legal possibilities exist. */
static BOOL
m3_ordinal_FIRST_info (
    struct value * arg,
    struct type * * base_type,
    LONGEST * lower,
    LONGEST * upper,
    char * op_name
  )
  { struct type * arg_type = NULL;
    struct type * ordinal_type = NULL;

    arg_type = value_type ( arg );

    while ( TYPE_CODE ( arg_type ) == TYPE_CODE_M3_INDIRECT )
      { arg_type = TYPE_M3_TARGET ( arg_type );
        arg = value_at_lazy ( arg_type, m3_value_as_address ( arg ) );
      }
    /* FIXME: ^first, use a function to dereference indirects.  Second, pull
              this out to call sites, so FIRST and LAST can be made to work on
              floating types via other code. */

    if ( m3_is_ordinal_type ( arg_type ) )
      { ordinal_type = arg_type;
        m3_ordinal_bounds ( ordinal_type, lower, upper );
      }
    else if ( TYPE_CODE ( arg_type ) == TYPE_CODE_M3_ARRAY )
      { ordinal_type = TYPE_M3_ARRAY_INDEX ( arg_type );
        m3_ordinal_bounds ( ordinal_type, lower, upper );
      }
    else if ( TYPE_CODE ( arg_type ) == TYPE_CODE_M3_OPEN_ARRAY )
      { if ( m3_value_is_type ( arg ) )
          { error
              (_("Cannot apply %s to an open array type.\n" ), op_name );
            /* NORETURN */
          }
        if ( lower != NULL )
          { * lower = 0; }
        if ( upper != NULL )
          { * upper = m3_value_open_array_shape_component ( arg, 0 ) - 1; }
        ordinal_type = builtin_type_m3_integer;
      }
    else { return FALSE; } /* Let caller try something else. */
    if ( base_type != NULL )
      { * base_type = m3_ordinal_base_type ( ordinal_type, NULL ); }
    return TRUE;
  } /* m3_ordinal_FIRST_info */

static struct value *
m3_eval_VAL ( struct expression *exp, int *pos, enum noside noside )

  { struct value * arg1;
    struct value * arg2;
    struct type * arg1_type;
    struct type * arg2_type;
    struct type * arg1_base_type;
    LONGEST sval;
    BOOL arg1_is_int_or_card;

    (*pos) += 1;
    arg1 = m3_evaluate_subexp ( NULL_TYPE, exp, pos, noside );
    arg2 = m3_evaluate_subexp ( NULL_TYPE, exp, pos, noside );
    arg1_type = value_type ( arg1 );
    arg2_type = value_type ( arg2 );

    if ( ! m3_value_is_type ( arg2 )
         || ! m3_is_ordinal_type ( arg2_type )
       )
      { error
          (_( "Second argument of VAL must be an ordinal type.\n " ) );
        /* NORETURN */
      }
    arg1_base_type
      = m3_ordinal_base_type ( arg1_type, & arg1_is_int_or_card );
    if ( ! arg1_is_int_or_card )
      { error
          (_( "First argument of VAL must be a value assignable to an "
              "integer or cardinal type.\n"
          ) );
        /* NORETURN */
      }
    sval  = m3_value_as_integer ( arg1 );
    m3_ordinal_range_check ( sval, arg2_type, "first argument to VAL" );
    return m3_value_from_longest ( arg2_type, sval );
  } /* m3_eval_VAL */

static struct value *
m3_evaluate_ADR ( struct expression *exp, int *pos, enum noside noside )
{ enum exp_opcode op;
  int pc;
  struct symbol *var;
  enum address_class sym_class;
  struct value * arg_val;
  struct type * arg_type;
  struct value * result;

  pc = (*pos);
  op = exp->elts[pc].opcode;

  switch (op)
    { case UNOP_M3_DEREF:
      /* ADR function cancels earlier-applied dereference.
         This is overly liberal in not changing the type to ADDRESS.
      */
      (*pos)++;
      return m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);

      case OP_VAR_VALUE:
        var = exp->elts[pc + 2].symbol;
        sym_class = SYMBOL_CLASS (var);
        if (sym_class == LOC_CONST
            || sym_class == LOC_CONST_BYTES
            || sym_class == LOC_REGISTER
            || sym_class == LOC_REGPARM)
          { error (_("ADR applied to register or constant.")); }
        break;
    }
    arg_val = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
    arg_type = value_type ( arg_val );
    while ( TYPE_CODE ( arg_type ) == TYPE_CODE_M3_INDIRECT )
      { arg_type = TYPE_M3_TARGET (arg_type);
        arg_val
          = value_at_lazy ( arg_type, m3_value_as_address ( arg_val ) );
      }
    if (VALUE_LVAL (arg_val) != lval_memory)
      { error (_("ADR applied to non-designator.")); /* NORETURN */ }
    return
      value_from_pointer
        ( builtin_type_m3_address
          , VALUE_ADDRESS (arg_val)
            + value_offset (arg_val)
            + value_embedded_offset (arg_val)
        );
    return result;
} /* m3_evaluate_ADR */

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
        struct symbol * proc_sym;
        struct block *b;
        struct value *val;
        struct frame_info *frame;

        (*pos) += 4;
        if (noside == EVAL_SKIP)
           { return m3_value_from_longest (builtin_type_long, (LONGEST) 1);}
        b = exp->elts[pc+1].block;
        sym = exp->elts[pc+2].symbol;
        if (symbol_read_needs_frame (sym))
          { frame
              = m3_static_ancestor_frame
                  ( deprecated_safe_get_selected_frame ( ), b, NULL );
            if (frame == NULL)
              { proc_sym = BLOCK_FUNCTION ( m3_block_proc_block ( b ) );
                if ( proc_sym != NULL
                     && SYMBOL_PRINT_NAME ( proc_sym ) != NULL
                   )
                  { error (_("No frame is currently executing in procedure "
                             "\"%s\" containing variable \"%s\"."),
                            SYMBOL_PRINT_NAME ( proc_sym ),
                            SYMBOL_PRINT_NAME ( sym )
                          );
                  }
                else
                  { error (_("No frame is currently executing in block "
                             "containing \"%s\"."),
                            SYMBOL_PRINT_NAME ( sym )
                          );
                  }
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
          error (_("Value of register %s not available."),
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

    case OP_M3_TEXT:
      length = longest_to_int (exp->elts[pc + 1].longconst);
      (*pos) += 4 + BYTES_TO_EXP_ELEM (length + 1);
      if (noside == EVAL_SKIP)
        return m3_value_from_longest (builtin_type_long, (LONGEST) 1);
      else
        return
          m3_evaluate_text_literal
            ( & exp->elts[pc + 2].string, length, TYPE_CODE_M3GDB_STRING );

    case OP_M3_WIDETEXT:
      length = longest_to_int (exp->elts[pc + 1].longconst);
      (*pos) += 4 + BYTES_TO_EXP_ELEM (length + 1);
      if (noside == EVAL_SKIP)
        return m3_value_from_longest (builtin_type_long, (LONGEST) 1);
      else
        return
          m3_evaluate_text_literal
            ( & exp->elts[pc + 2].string, length, TYPE_CODE_M3GDB_WIDESTRING );

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
      arg1 = m3_allocate_value_that_is_type ( exp -> elts [ pc + 1 ] . type );
      return arg1;

    case UNOP_M3_DEREF: {
      struct type * arg1_type;
      struct type * revealed_type;
      struct type * allocated_type;
      struct type * res_type;
      CORE_ADDR ref_value;

      (*pos) += 1;
      arg1 = m3_evaluate_subexp (0, exp, pos, noside);
      arg1_type = value_type ( arg1 );

      while ( TYPE_CODE ( arg1_type ) == TYPE_CODE_M3_INDIRECT )
        { arg1_type = TYPE_M3_TARGET (arg1_type);
          arg1 = value_at_lazy ( arg1_type, m3_value_as_address ( arg1 ) );
        }
      revealed_type = m3_revealed_type ( arg1_type );

      switch ( TYPE_CODE ( revealed_type ) )
        { case TYPE_CODE_M3_ADDRESS :
            { error (_("^ applied to ADDRESS.")); /* NORETURN */
              return arg1;
            }
          case TYPE_CODE_M3_REFANY :
          case TYPE_CODE_M3_TRANSIENT_REFANY:
          case TYPE_CODE_M3_POINTER:
            ref_value = m3_value_as_address ( arg1 );
            { if ( ref_value == 0 )
                { error (_("^ applied to NIL")); /* NORETURN */
                  return arg1;
                }
              allocated_type = m3_allocated_type ( arg1 );
              if ( TYPE_CODE ( allocated_type ) == TYPE_CODE_M3_POINTER )
                { res_type = TYPE_M3_TARGET ( allocated_type );
                  return value_at_lazy ( res_type, ref_value );
                }
              /* else fall through to object cases.
                 This is bizarre, but reflects the m3gdb dynamic philosophy
                 of using allocated types.  If an expression of type REFANY
                 has an allocated REF type, we allow the dereference.
                 If an object type, we treat it as redundant.
              */
            }

          case TYPE_CODE_M3_OBJECT:
          case TYPE_CODE_M3_TRANSIENT_ROOT:
          case TYPE_CODE_M3_ROOT:
          case TYPE_CODE_M3_UN_ROOT:
          case TYPE_CODE_M3_TEXT:
               /* TODO: ^Maybe make this do the same as the /k format. */
          case TYPE_CODE_M3_MUTEX:
               /* CHECK: ^Do we want to display this? */
            { warning (_("Ignoring redundant ^ applied to object"));
              if ( value_as_address ( arg1 ) == 0 )
                /* REVIEWME: Is this check really necessary here? */
                { error (_("^ applied to NIL object")); }
              return arg1;
            }

          default:
            { error (_("^ applied to a non-reference.")); /* NORETURN */
              return arg1;
            }
        }
      }

    case UNOP_M3_NEG: {
      struct type *neg_type;

      (*pos) += 1;
      arg1 = m3_evaluate_subexp (0, exp, pos, noside);
      neg_type = value_type (arg1);
      switch ( TYPE_CODE ( neg_type ) )
        { case TYPE_CODE_FLT:
            return
              value_from_double ( neg_type, - m3_value_as_float ( arg1 ) );
          case TYPE_CODE_M3_INTEGER:
          case TYPE_CODE_M3_LONGINT:
            return
              m3_value_from_longest
                ( neg_type, - m3_value_as_integer ( arg1 ) );
          default:
            error
              (_("Unary '-' must be applied to an integer or floating-point value.\n"));
        }
      }

    case UNOP_M3_NOT: {
      LONGEST val;

      (*pos) += 1;
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      val  = ! m3_value_as_integer (arg1);
      return m3_value_from_longest (builtin_type_m3_boolean, val);  }
        
    case UNOP_M3_FIRST:
      { struct value * arg;
        struct value * res;
        struct type * base_type;
        LONGEST lower;
        LONGEST upper;

        (*pos) += 1;
        arg = m3_evaluate_subexp (0, exp, pos, noside);
        /* FIXME: We want noside to be conditional.  Evaluate the argument
                  iff it is a value of open array type. */
        if ( m3_ordinal_FIRST_info
              ( arg, & base_type, & lower, & upper, "FIRST" )
           )
          { res = m3_value_from_longest ( base_type, lower );
            return res;
          }
        /* IMPLEMENTME: Implement this on floating types. */
        else
          { error
              (_("FIRST is implemented only on ordinal and array types.\n") );
          }
      }

    case UNOP_M3_LAST:
      { struct value * arg;
        struct value * res;
        struct type * base_type;
        LONGEST lower;
        LONGEST upper;

        (*pos) += 1;
        arg = m3_evaluate_subexp (0, exp, pos, noside);
        /* FIXME: We want noside to be conditional.  Evaluate the argument
                  iff it is a value of open array type. */
        if ( m3_ordinal_FIRST_info
               ( arg, & base_type, & lower, & upper, "LAST" )
           )
          { res = m3_value_from_longest ( base_type, upper );
            return res;
          }
        /* IMPLEMENTME: Implement this on floating types. */
        else
          { error
              (_("LAST is implemented only on ordinal and array types.\n") );
          }
      }

    case UNOP_M3_NUMBER:
      { struct value * arg;
        struct value * res;
        struct type * base_type;
        LONGEST lower;
        LONGEST upper;

        (*pos) += 1;
        arg = m3_evaluate_subexp (0, exp, pos, noside);
        if ( m3_ordinal_FIRST_info
               ( arg, & base_type, & lower, & upper, "NUMBER" )
           )
          { res = m3_value_from_longest
                    ( builtin_type_m3_cardinal, upper - lower + 1 );
            return res;
          }
        else
          { error
              (_("NUMBER can apply only to ordinal and array types.\n") );
          }
      }

    case UNOP_M3_ABS: {
      struct type *arg1_type;

      (*pos) += 1;
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = value_type (arg1);
      switch ( TYPE_CODE (arg1_type) )
        { case TYPE_CODE_M3_INTEGER:
          case TYPE_CODE_M3_LONGINT:
            { LONGEST val = m3_value_as_integer ( arg1 );
              if ( val < 0 )
                { val = - val; }
              return m3_value_from_longest ( arg1_type, val );
            }

          case TYPE_CODE_FLT:
            { double val = m3_value_as_float ( arg1 );
              if ( val < 0.0 )
                { val = - val; }
              return value_from_double ( arg1_type, val );
            }
          default:
            error (_("ABS requires an INTEGER, LONGINT, REAL, LONGREAL, "
                     "or EXTENDED parameter.\n"
                  ) );
        }
      }

    case UNOP_M3_ADR:
      { struct value * v;

        ( * pos) += 1;
        v = m3_evaluate_ADR (exp, pos, noside);
        return v;
      }

    case UNOP_M3_ADRSIZE: {
      LONGEST sz;
      struct type *arg1_type;

      (*pos) += 1;
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = value_type (arg1);
      sz = TYPE_M3_SIZE (arg1_type) / HOST_CHAR_BIT;
      if (TYPE_CODE (arg1_type) == TYPE_CODE_M3_OPEN_ARRAY) {
        error (_("ADRSIZE(<open array>) not implemented"));
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
        error (_("BITSIZE(<open array>) not implemented"));
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
        error (_("BYTESIZE(<open array>) not implemented"));
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
        error (_("CEILING must be applied to a floating-point value"));
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
        error (_("FLOOR must be applied to a floating-point value"));
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
        error (_("ROUND must be applied to a floating-point value"));
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
        error (_("TRUNC must be applied to a floating-point value"));
        return arg1;
      }
    }

    case UNOP_M3_TYPECODE: {
      struct type * arg1_type;
      CORE_ADDR val_contents;
      LONGEST typecode;

      (*pos) += 1;
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      if ( m3_value_is_type ( arg1 ) )
        { error
            (_("TYPECODE is not implemented for types.")); /* NORETURN */
        }
      arg1_type = value_type ( arg1 );
      switch ( TYPE_CODE ( arg1_type ) )
        { case TYPE_CODE_M3_NULL :
            /* This probably can't happen. */
            { error
                (_("TYPECODE ( NIL ) not implemented.")); /* NORETURN */
            }
          case TYPE_CODE_M3_POINTER :
            if ( ! TYPE_M3_POINTER_TRACED ( arg1_type ) )
              { error
                  (_("TYPECODE can't apply to an untraced REF type."));
                  /* NORETURN */
              }
            /* Otherwise, fall through. */
          case TYPE_CODE_M3_TEXT :
          case TYPE_CODE_M3_MUTEX :
          case TYPE_CODE_M3_OBJECT :
          case TYPE_CODE_M3_ROOT :
          case TYPE_CODE_M3_TRANSIENT_ROOT :
          case TYPE_CODE_M3_UN_ROOT :
          case TYPE_CODE_M3_REFANY :
          case TYPE_CODE_M3_TRANSIENT_REFANY :
            val_contents = value_as_address ( arg1 );
            if ( val_contents == 0 )
              { error
                  (_("TYPECODE ( NIL ) not implemented.")); /* NORETURN */
              }
            typecode = m3_typecode_from_inf_object_addr ( val_contents );
            return m3_value_from_longest
                     ( builtin_type_m3_integer, typecode );
          default :
            error
              (_("TYPECODE must be applied to a traced reference or object "
                 "value.")
              );
        }
    }

    case UNOP_M3_ORD:
      { struct value * arg1;
        LONGEST sval;

        (*pos) += 1;
        arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
        if ( m3_is_ordinal_type ( value_type ( arg1 ) )
             && ! m3_value_is_type ( arg1 )
           )
          { sval = m3_value_as_integer ( arg1 );
            m3_ordinal_range_check
              ( sval, builtin_type_m3_integer, "argument of ORD" );
            return m3_value_from_longest ( builtin_type_m3_integer, sval );
          }
        else
          { error (_("Argument to ORD is not a value of ordinal type.\n" ) );
            /* NORETURN */
          }
      }

    case BINOP_M3_VAL:
      { return m3_eval_VAL ( exp, pos, noside ); }

    case BINOP_M3_FLOAT: {
      struct type *arg1_type;
      double val;

      (*pos) += 1;
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = value_type (arg1);

      switch ( TYPE_CODE ( arg1_type ) )
        { case TYPE_CODE_FLT:
            val = m3_value_as_float ( arg1 );
            break;
          case TYPE_CODE_M3_INTEGER:
          case TYPE_CODE_M3_CARDINAL:
          case TYPE_CODE_M3_LONGINT:
          case TYPE_CODE_M3_LONGCARD:
           val = (double) m3_value_as_integer ( arg1 );
            break;
          default:
            error
              (_("First parameter to FLOAT must be a value of integer or "
                 "real type.\n"
              ) ); /* NORETURN */
        }

      if ( ! m3_value_is_type ( arg2 )
           || (TYPE_CODE ( value_type( arg2 ) ) != TYPE_CODE_FLT )
         ) {
        error
          (_("Second parameter to FLOAT must be REAL, LONGREAL, "
             "or EXTENDED.\n"
          ) ); /* NORETURN */
      }

      return value_from_double ( value_type( arg2 ), val );
    }

    case BINOP_M3_LOOPHOLE: {
      struct type *arg1_type, *arg2_type;

      (*pos) += 1;
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = value_type (arg1);
      arg2_type = value_type (arg2);

      if (TYPE_CODE(arg1_type) == TYPE_CODE_M3_OPEN_ARRAY) {
        error (_("LOOPHOLE of open array values is illegal"));
        return arg1;
      }
      else if ( ! m3_value_is_type ( arg2 ) ) {
        error (_("Second parameter of LOOPHOLE must be a type"));
        return arg1;
      } else if (TYPE_CODE (arg2_type) == TYPE_CODE_M3_OPEN_ARRAY) {
        error (_("LOOPHOLE to an open array type is not (yet) supported"));
        return arg1;
      } else if (TYPE_M3_SIZE (arg1_type) != TYPE_M3_SIZE (arg2_type)) {
        error (_("Size of value and type passed to LOOPHOLE don't agree"));
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
          long n = (TARGET_PTR_BIT + m3_target_integer_bit) / HOST_CHAR_BIT;
          elt_size = 1;
          while (TYPE_CODE (e) == TYPE_CODE_M3_OPEN_ARRAY) {
            /* FIXME: This should be a target long, not a host long. */
            elt_size *= *(long*) (value_contents (array) + n);
            n += m3_target_integer_bit / HOST_CHAR_BIT;
            e = TYPE_M3_OPEN_ARRAY_ELEM (e); }
          elt_size *= TYPE_M3_SIZE (e); }}
      else {
        error (_("Indexed expression is not an array")); }

      array = coerce_ref (array);

      index_val = m3_value_as_integer (index);
      if (lowerbound > index_val || index_val > upperbound) {
        error (_("Range fault on array access"));
        return 0; }

      offset = elt_size * (index_val - lowerbound);
      if (offset % 8 != 0) {
        error
          (_("Non-byte-aligned, bit-packed array elements not supported."));
        return 0; }

      v = allocate_value (elem_type);

      if (TYPE_CODE (array_type) == TYPE_CODE_M3_OPEN_ARRAY) {

        if (TYPE_CODE (elem_type) == TYPE_CODE_M3_OPEN_ARRAY) {
          /* recreate a dope vector for the next guy */
          memcpy (value_contents_raw (v) + (TARGET_PTR_BIT / HOST_CHAR_BIT),
                  value_contents (array)
                    + (TARGET_PTR_BIT + m3_target_integer_bit)/ HOST_CHAR_BIT,
                  TYPE_LENGTH (elem_type) - m3_target_integer_bit
                  / HOST_CHAR_BIT);
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
      struct type * result_type;

      (*pos) += 1;
      arg1 = m3_evaluate_subexp (0, exp, pos, noside);
      arg2 = m3_evaluate_subexp (0, exp, pos, noside);

      arg1_type = value_type (arg1);
      result_type = builtin_type_m3_integer;
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
          ival1 = m3_value_as_integer (arg1);
          break;
        case TYPE_CODE_M3_INTEGER:
          ival1 = m3_value_as_integer (arg1);
          break;
        case TYPE_CODE_M3_LONGCARD:
          arg1_type = builtin_type_m3_longint;
          result_type = builtin_type_m3_longint;
          ival1 = m3_value_as_integer (arg1);
          break;
        case TYPE_CODE_M3_LONGINT:
          result_type = builtin_type_m3_longint;
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
        case TYPE_CODE_M3_REFANY:
        case TYPE_CODE_M3_TRANSIENT_REFANY:
        case TYPE_CODE_M3_ADDRESS:
        case TYPE_CODE_M3_POINTER:
        case TYPE_CODE_M3_TEXT:
        case TYPE_CODE_M3_ROOT:
        case TYPE_CODE_M3_TRANSIENT_ROOT:
        case TYPE_CODE_M3_UN_ROOT:
        case TYPE_CODE_M3_OBJECT:
        case TYPE_CODE_M3_MUTEX:
        case TYPE_CODE_M3_NULL:
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
          ival2 = m3_value_as_integer (arg2);
          break;
        case TYPE_CODE_M3_INTEGER:
          ival2 = m3_value_as_integer (arg2);
          break;
        case TYPE_CODE_M3_LONGCARD:
          arg2_type = builtin_type_m3_longint;
          result_type = builtin_type_m3_longint;
          ival2 = m3_value_as_integer (arg2);
          break;
        case TYPE_CODE_M3_LONGINT:
          result_type = builtin_type_m3_longint;
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
        case TYPE_CODE_M3_REFANY:
        case TYPE_CODE_M3_TRANSIENT_REFANY:
        case TYPE_CODE_M3_ADDRESS:
        case TYPE_CODE_M3_POINTER:
        case TYPE_CODE_M3_TEXT:
        case TYPE_CODE_M3_ROOT:
        case TYPE_CODE_M3_TRANSIENT_ROOT:
        case TYPE_CODE_M3_UN_ROOT:
        case TYPE_CODE_M3_OBJECT:
        case TYPE_CODE_M3_MUTEX:
        case TYPE_CODE_M3_NULL:
          ival2 = (LONGEST) m3_value_as_address (arg2);
          arg2_type = builtin_type_m3_integer;
          break;
        default:
          arg2_type = builtin_type_m3_void;
          break; }

      if ( TYPE_CODE (arg1_type) == TYPE_CODE (arg2_type)
           || ( TYPE_CODE (arg1_type) == TYPE_CODE_M3_INTEGER
                && TYPE_CODE (arg2_type) == TYPE_CODE_M3_LONGINT
              )
           || ( TYPE_CODE (arg1_type) == TYPE_CODE_M3_LONGINT
                && TYPE_CODE (arg2_type) == TYPE_CODE_M3_INTEGER
              )
         ) { /* types are OK. */ }
      else
        { error (_("Incompatible argument types for binary operation."));
          /* NORETURN */
        }

      if ( TYPE_CODE (arg1_type) == TYPE_CODE_M3_INTEGER
           || TYPE_CODE (arg1_type) == TYPE_CODE_M3_LONGINT
         )
        { LONGEST res = 0;
          if ( ! int_ok )
           { error (_("Binary operation requires integer typed operands."));
             /* NORETURN */
           }
          switch (op)
            { case BINOP_M3_MULT:   res = ival1 * ival2;          break;
              case BINOP_M3_ADD:    res = ival1 + ival2;          break;
              case BINOP_M3_MINUS:  res = ival1 - ival2;          break;
              case BINOP_M3_DIV:    res = m3_div (ival1, ival2);  break;
              case BINOP_M3_MOD:    res = m3_modi (ival1, ival2); break;
            } /* switch */
          return m3_value_from_longest (result_type, res);
        }

      if (TYPE_CODE (arg1_type) == TYPE_CODE_FLT)
        { double res = 0.0;
          if ( ! float_ok )
            { error (_("Binary operation requires floating typed operands."));
              /* NORETURN */
            }

          switch (op)
            { case BINOP_M3_DIVIDE: res = fval1 / fval2;          break;
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
      if (m3_value_as_integer (arg1) == 0) {        return arg1; }
      return m3_evaluate_subexp (NULL_TYPE, exp, pos, noside); }

    case BINOP_M3_OR: {

      (*pos) += 1;
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      if (m3_value_as_integer (arg1) == 1) {        return arg1; }
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
      tempval = m3_value_less (arg1, arg2);
      return m3_value_from_longest
               (builtin_type_m3_boolean, (LONGEST) tempval); }

    case BINOP_M3_LE: {
      (*pos) += 1;
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = m3_evaluate_subexp (value_type (arg1), exp, pos, noside);
      tempval = ! (m3_value_less (arg2, arg1));
      return m3_value_from_longest
               (builtin_type_m3_boolean, (LONGEST) tempval); }

    case BINOP_M3_GT: {
      (*pos) += 1;
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = m3_evaluate_subexp (value_type (arg1), exp, pos, noside);
      tempval = m3_value_less (arg2, arg1);
      return m3_value_from_longest
              (builtin_type_m3_boolean, (LONGEST) tempval); }

    case BINOP_M3_GE: {
      (*pos) += 1;
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = m3_evaluate_subexp (value_type (arg1), exp, pos, noside);
      tempval = ! (m3_value_less (arg1, arg2));
      return m3_value_from_longest
               (builtin_type_m3_boolean, (LONGEST) tempval); }

    case BINOP_M3_MIN: {
      (*pos) += 1;
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = m3_evaluate_subexp (value_type (arg1), exp, pos, noside);
      return m3_value_less (arg1, arg2) ? arg1 : arg2; }

    case BINOP_M3_MAX: {
      (*pos) += 1;
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = m3_evaluate_subexp (value_type (arg1), exp, pos, noside);
      return m3_value_less (arg1, arg2) ? arg2 : arg1; }

    case BINOP_M3_CAT:
      { struct value * proc_val;

        (*pos) += 1;
        proc_val = m3_proc_value_from_qualified_name ( "Text", "Cat" );
        if ( proc_val == NULL )
          { return 0; }
/* FIXME: If both operands are m3gdb strings (located in m3gdb
          process space, keep the concatenation in m3gdb space too. */
        return m3_call_proc_const_or_var
                 ( proc_val, 2, "Text.Cat", exp, pos, noside );
      }

    case BINOP_M3_IN:
      error (_("Not yet implemented: 'IN' set membership test"));
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
