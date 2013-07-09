/* M3 language support routines for GDB, the GNU debugger.
   Copyright 2006 Free Software Foundation, Inc.

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

/* This file contains code to lexically scan Modula-3 code. */

#include "m3-bool.h"

#include "defs.h"
#include "expression.h"
#include "value.h"
#include "parser-defs.h"
#include "gdb_string.h"

#include "m3-token.h"
#include "m3-lang.h"
#include "m3-util.h"

/*-------------------------------------------------------- reserved words ---*/

struct reserved {char *name; int kind; };

static struct reserved reserved [] = {
  { "ABS",       TK_ABS       },
  { "ADDRESS",   TK_ADDRESS   },
  { "ADR",       TK_ADR       },
  { "ADRSIZE",   TK_ADRSIZE   },
  { "AND",       TK_AND       },
  { "ANY",       TK_ANY       },
  { "ARRAY",     TK_ARRAY     },
  { "AS",        TK_AS        },
  { "BEGIN",     TK_BEGIN     },
  { "BITS",      TK_BITS      },
  { "BITSIZE",   TK_BITSIZE   },
  { "BOOLEAN",   TK_BOOLEAN   },
  { "BRANDED",   TK_BRANDED   },
  { "BY",        TK_BY        },
  { "BYTESIZE",  TK_BYTESIZE  },
  { "CARDINAL",  TK_CARDINAL  },
  { "CASE",      TK_CASE      },
  { "CEILING",   TK_CEILING   },
  { "CHAR",      TK_CHAR      },
  { "CONST",     TK_CONST     },
  { "DEC",       TK_DEC       },
  { "DISPOSE",   TK_DISPOSE   },
  { "DIV",       TK_DIV       },
  { "DO",        TK_DO        },
  { "ELSE",      TK_ELSE      },
  { "ELSIF",     TK_ELSIF     },
  { "END",       TK_END       },
  { "EVAL",      TK_EVAL      },
  { "EXCEPT",    TK_EXCEPT    },
  { "EXCEPTION", TK_EXCEPTION },
  { "EXIT",      TK_EXIT      },
  { "EXPORTS",   TK_EXPORTS   },
  { "EXTENDED",  TK_EXTENDED  },
  { "FALSE",     TK_FALSE     },
  { "FINALLY",   TK_FINALLY   },
  { "FIRST",     TK_FIRST     },
  { "FLOAT",     TK_FLOAT     },
  { "FLOOR",     TK_FLOOR     },
  { "FOR",       TK_FOR       },
  { "FROM",      TK_FROM      },
  { "GENERIC",   TK_GENERIC   },
  { "IF",        TK_IF        },
  { "IMPORT",    TK_IMPORT    },
  { "IN",        TK_IN        },
  { "INC",       TK_INC       },
  { "INTEGER",   TK_INTEGER   },
  { "INTERFACE", TK_INTERFACE },
  { "ISTYPE",    TK_ISTYPE    },
  { "LAST",      TK_LAST      },
  { "LOCK",      TK_LOCK      },
  { "LONGCARD",  TK_LONGCARD  },
  { "LONGINT",   TK_LONGINT  },
  { "LONGREAL",  TK_LONGREAL  },
  { "LOOP",      TK_LOOP      },
  { "LOOPHOLE",  TK_LOOPHOLE  },
  { "MAX",       TK_MAX       },
  { "METHODS",   TK_METHODS   },
  { "MIN",       TK_MIN       },
  { "MOD",       TK_MOD       },
  { "MODULE",    TK_MODULE    },
  { "MUTEX",     TK_MUTEX     },
  { "NARROW",    TK_NARROW    },
  { "NEW",       TK_NEW       },
  { "NIL",       TK_NIL       },
  { "NOT",       TK_NOT       },
  { "NULL",      TK_NULL      },
  { "NUMBER",    TK_NUMBER    },
  { "OBJECT",    TK_OBJECT    },
  { "OF",        TK_OF        },
  { "OR",        TK_OR        },
  { "ORD",       TK_ORD       },
  { "OVERRIDES", TK_OVERRIDES },
  { "PROCEDURE", TK_PROCEDURE },
  { "RAISE",     TK_RAISE     },
  { "RAISES",    TK_RAISES    },
  { "READONLY",  TK_READONLY  },
  { "REAL",      TK_REAL      },
  { "RECORD",    TK_RECORD    },
  { "REF",       TK_REF       },
  { "REFANY",    TK_REFANY    },
  { "REPEAT",    TK_REPEAT    },
  { "RETURN",    TK_RETURN    },
  { "REVEAL",    TK_REVEAL    },
  { "ROOT",      TK_ROOT      },
  { "ROUND",     TK_ROUND     },
  { "SET",       TK_SET       },
  { "SUBARRAY",  TK_SUBARRAY  },
  { "TEXT",      TK_TEXT      },
  { "THEN",      TK_THEN      },
  { "TO",        TK_TO        },
  { "TRUE",      TK_TRUE      },
  { "TRUNC",     TK_TRUNC     },
  { "TRY",       TK_TRY       },
  { "TYPE",      TK_TYPE      },
  { "TYPECASE",  TK_TYPECASE  },
  { "TYPECODE",  TK_TYPECODE  },
  { "UNSAFE",    TK_UNSAFE    },
  { "UNTIL",     TK_UNTIL     },
  { "UNTRACED",  TK_UNTRACED  },
  { "VAL",       TK_VAL       },
  { "VALUE",     TK_VALUE     },
  { "VAR",       TK_VAR       },
  { "WHILE",     TK_WHILE     },
  { "WIDECHAR",  TK_WIDECHAR  },
  { "WITH",      TK_WITH      }
};

static void
recognize_reserved_word (tok)
   struct m3_token *tok;
{
  int low, high, mid, cmp;

  low = 0;
  high = sizeof (reserved) / sizeof (struct reserved);

  /* tok->string may be in [low .. high) */

  while (low < high) {
    mid = (low + high) / 2;
    cmp = strcmp (reserved [mid].name, tok->string);
    if (cmp == 0) {
      tok->kind = reserved [mid].kind;
      return;
    } else if (cmp < 0) {
      low  = mid + 1;
    } else {
      high = mid;
    }
  }

  tok->kind = TK_IDENT;
  return;
} /* recognize_reserved_word */

/*--------------------------------------------------------------- numbers ---*/

static char *
m3_scan_number (char *input, struct m3_token *tok)

  { char *c;
    char *based_start;
    int i;
    int digit;
    ULONGEST val;
    ULONGEST base;
    LONGEST signed_val;
    LONGEST lower;
    LONGEST upper;
    BOOL is_based;

    c = input;
    val = 0;
    if (*c == '0' && ( *(c+1) == 'x' || *(c+1) == 'X' ) )
      { /* Go ahead and accept the C lexical syntax for hex numbers. */
        is_based = TRUE;
        base = 16;
        c = c + 2;
      }
    else
      { /* Scan and convert the leading decimal digits */
        while ('0' <= *c && *c <= '9')
          { digit = *c - '0';
            if ( ( ULONGEST_MAX - digit ) / 10 < val )
              { error
                  (_("Numeric literal value too large.\n")); /* NORETURN */
              }
            val = val * 10 + digit;
            c++;
          }
        if (*c == '_')
          { /* It's a based value in Modula-3 syntax. */
            is_based = TRUE;
            base = val;
            c++;
            if ((base < 2) || (16 < base))
              { error
                  (_("%d is an illegal base for a Modula-3 literal.")
                  , (int) base
                  ); /* NORETURN */
              }
          }
        else is_based = FALSE;
      }

    /* See if it's a real literal. If so, throw away the converted integer
       value, lexically check it, then call atof to convert. */
    if ( (*c == '.') && (c[1] != '.') )
      { if ( is_based )
          { error (_("Based number cannot be real.\n") ); /* NORETURN */ }
        /* scan a floating point number */
        c++; /* skip the decimal point */

        /* scan the fractional digits */
        if ( (*c < '0') || ('9' < *c) )
          { error (_("Missing digits in real fraction") ); /* NORETURN */ }

        while ( '0' <= *c && *c <= '9' )
          { c++; }

        /* check for the exponent */
        if ( ( *c == 'e' ) || ( *c == 'E' ) )
          { *c++ = 'e';  /* since atof only knows about 'e' */
            tok->kind = TK_REAL_LIT;
          }
        else if ( ( *c == 'd' ) || ( *c == 'D' ) )
          { *c++ = 'e';  /* since atof only knows about 'e' */
            tok->kind = TK_LREAL_LIT;
          }
        else if ( ( *c == 'x' ) || ( *c == 'X' ) )
          { *c++ = 'e';  /* since atof only knows about 'e' */
            tok->kind = TK_XREAL_LIT;
          }
        else /* real constant with no exponent */
          { tok->kind = TK_REAL_LIT;
            tok->floatval = atof (input);
            return c;
          }

        /* check for an exponent sign */
        if ( ( *c == '+' ) || ( *c == '-' ) )
          { c++; }

        /* scan the exponent digits */
        if ( ( *c < '0' ) || ( '9' < *c ) )
          { error (_("Missing digits in real exponent") ); /* NORETURN */ }
        while ( '0' <= *c && *c <= '9' )
          { c++; }

        /* and do the conversion... */
        tok->floatval = atof (input);
        return c;
      }

    /* It wasn't real.  Must be INTEGER or LONGINT. */
    if ( is_based )
      { /* scan a based integer */
        val = 0;
        based_start = c;
        while ( TRUE )
          { if ('0' <= *c && *c <= '9')
              { digit = *c - '0'; }
            else if ('A' <= *c && *c <= 'F')
              { digit = *c - 'A' + 10; }
            else if ('a' <= *c && *c <= 'f')
              { digit = *c - 'a' + 10; }
            else { break; }
            if (digit >= base)
              { error
                  (_("Numeric literal digit too large.\n") ); /* NORETURN */
              }
            if ( ( ULONGEST_MAX - digit ) / base < val )
              { error
                  (_("Numeric literal value too large.\n") ); /* NORETURN */
              }
            val = val * base + digit;
            c++;
          }

        if ( c == based_start )
          { error
              (_("Based integer literal has no digits.") ); /* NORETURN */
          }
      }

    /* See if it's LONGINT. */
    if ( * c == 'l' || * c == 'L' )
      { if ( m3_compiler_kind ( ) != m3_ck_cm3 )
        /* FIXME: ^This error condition is too weak.  Earlier CM3 compilers
                   do not support LONGINT either, but how do we detect this? */
          { error
              (_("LONGINT literals not supported by this "
                 "Modula-3 compiler ") );
          } /* NORETURN */
        tok->kind   = TK_LONGINT_LIT;
        m3_ordinal_bounds ( builtin_type_m3_longint, & lower, & upper );
        c ++;
      }
    else /* It's INTEGER. */
      { tok->kind   = TK_INTEGER_LIT; /* Could change. */
        m3_ordinal_bounds ( builtin_type_m3_integer, & lower, & upper );
      }

    if ( is_based )
      { if ( val / 2 > upper )
          { error
              (_("Based integer literal value too large.\n"));
              /* NORETURN */
          }
      }
    else
      { if ( val > upper )
          { error
              (_("Integer literal value too large.\n"));
              /* NORETURN */
          }
      }
    signed_val = (LONGEST) val; /* Hopefully, no overflow checks happen. */
    tok->intval = val;

    return c;
  } /* m3_scan_number */

/*------------------------------------------------------------- GDB tokens --*/

static char *
scan_gdb_token (input, tok)
  char *input;
  struct m3_token *tok;
{
  char *tokstart;
  int sign = 1;
  int len, c;

  input++;
  tokstart = input;

  switch (*input) {
    case '$':
      tokstart++;
      input++;
      sign = -1;
      /* fall through */

    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':

      while ('0' <= *input && *input <= '9') { input++; }
      tok->kind = TK_GDB_HISTORY;
      tok->intval = sign * ((input == tokstart)? 1 : atoi(tokstart));
      return input;

    default: break;
  }

  /* scan an identifier */
  input = tokstart;
  while (   ('a' <= *input && *input <= 'z')
         || ('A' <= *input && *input <= 'Z')
         || ('0' <= *input && *input <= '9')
         || *input == '_') {
    input++;
  }
  len = input - tokstart;

  if (len == 0) {
    tok->kind = TK_GDB_HISTORY;
    tok->intval = 0;
    return input;
  }

  /* check for a register name */
  for (c = 0; c < NUM_REGS; c++) {
   if (len == strlen (REGISTER_NAME(c))
        && strncmp (tokstart, REGISTER_NAME(c), len) == 0) {
      tok->kind = TK_REGISTER;
      tok->intval = c;
      return input;
    }
  }

  /* check for a pseudo-register name */
#if 0
  for (c = 0; c < num_std_regs; c++) {
    if (len == strlen (std_regs [c].name)
        && strncmp (tokstart, std_regs[c].name, len) == 0) {
      tok->kind = TK_REGISTER;
      tok->intval = std_regs[c].regnum;
      return input;
    }
  }
#endif

  /* ? must be a GDB variable */
  tok->kind = TK_GDB_VAR;
  tok->string = (char *) malloc (len + 1);
  strncpy (tok->string, tokstart, len);
  tok->string [len] = '\0';
  tok->length = len;
  return input;
} /* scan_gdb_token */

/*-------------------------------------------------- char & text literals ---*/

static void
bad_octal ( BOOL wide )

  { error (_("Octal %sCHAR escape sequence must have exactly %d digits"),
            ( wide ? "WIDE" : "" ),
            ( wide ? 6 : 3)
          ); /* NORETURN */
  } /* bad_octal */

static BOOL
octal_digit ( char ch, int *digit )

{
  if ( '0' <= ch && ch <= '7' )
    { *digit = ch - '0';  return TRUE; }
  return FALSE;
} /* octal_digit */

static char *
scan_octal ( char *input, LONGEST *val,  BOOL wide )

  { int digit;
    *val = 0;

    if (!octal_digit (*input, &digit)) { bad_octal(wide);  return input; }
    *val = digit;  input++;
    if (!octal_digit (*input, &digit)) { bad_octal(wide);  return input; }
    *val = 8 * (*val) + digit;  input++;
    if (!octal_digit (*input, &digit)) { bad_octal(wide);  return input; }
    *val = 8 * (*val) + digit;  input++;

    if (!wide) { return input; }

    if (!octal_digit (*input, &digit)) { bad_octal(wide);  return input; }
    *val = 8 * (*val) + digit;  input++;
    if (!octal_digit (*input, &digit)) { bad_octal(wide);  return input; }
    *val = 8 * (*val) + digit;  input++;
    if (!octal_digit (*input, &digit)) { bad_octal(wide);  return input; }
    *val = 8 * (*val) + digit;  input++;

    return input;
  } /* scan_octal */
static void
bad_hex ( int bytes /*1..3*/ )

  { error (_("%scharacter escape sequence must exactly have %d digits"),
            ( bytes == 3 ? "Unicode " : "Hex " ),
            ( bytes * 2 )
          ); /* NORETURN */
  } /* bad_hex */

static BOOL
hex_digit ( char ch, int *digit )

  { if ( '0' <= ch && ch <= '9' )
      { *digit = ch - '0';       return TRUE; }
    if ( 'A' <= ch && ch <= 'F' )
      { *digit = ch - 'A' + 10;  return TRUE; }
    if ( 'a' <= ch && ch <= 'f' )
      { *digit = ch - 'a' + 10;  return TRUE; }
    return FALSE;
  } /* hex_digit */

static char *
scan_hex ( char *input, LONGEST *val, int bytes /*1..3*/ )

  { int digit;
    *val = 0;

    if (!hex_digit (*input, &digit)) { bad_hex(bytes);  return input; }
    *val = digit;  input++;
    if (!hex_digit (*input, &digit)) { bad_hex(bytes);  return input; }
    *val = 16 * (*val) + digit;  input++;

    if (bytes==1) { return input; }

    if (!hex_digit (*input, &digit)) { bad_hex(bytes);  return input; }
    *val = 16 * (*val) + digit;  input++;
    if (!hex_digit (*input, &digit)) { bad_hex(bytes);  return input; }
    *val = 16 * (*val) + digit;  input++;

    if (bytes==2) { return input; }

    if (!hex_digit (*input, &digit)) { bad_hex(bytes);  return input; }
    *val = 16 * (*val) + digit;  input++;
    if (!hex_digit (*input, &digit)) { bad_hex(bytes);  return input; }
    *val = 16 * (*val) + digit;  input++;

    return input;
  } /* scan_hex */

/* PRE: input points to the opening single quote. */
static char *
scan_char ( char *input, struct m3_token *tok, BOOL wide )

{
  int val = 0;

  tok->kind   = (wide ? TK_WIDECHAR_LIT : TK_CHAR_LIT);
  tok->intval = 0;

  input++;  /* skip opening quote */

  if (*input == '\'') {
    error (_("No character in character literal") ); /* NORETURN */
    return input+1;

  } else if ((*input == '\n') || (*input == '\r') || (*input == '\f')) {
    error (_("End-of-line encountered in character literal") ); /* NORETURN */
    return input+1;

  } else if (*input == '\\') {
    input ++;
    if      (*input == 'n')  { tok->intval = '\n';  input++; }
    else if (*input == 't')  { tok->intval = '\t';  input++; }
    else if (*input == 'r')  { tok->intval = '\r';  input++; }
    else if (*input == 'f')  { tok->intval = '\f';  input++; }
    else if (*input == '\\') { tok->intval = '\\';  input++; }
    else if (*input == '\'') { tok->intval = '\'';  input++; }
    else if (*input == '"')  { tok->intval = '"';   input++; }
    else if (*input == 'x' || *input == 'X')  {
      input = scan_hex (++input, &tok->intval, (wide ? 2 : 1) );
    } else if (*input == 'U')  {
      input = scan_hex (++input, &tok->intval, 3);
      if (tok->intval > (wide ? m3_widechar_LAST : 0xFF) ) 
        error
          (_("Out-of-range Unicode escape in character literal") ); /* NORETURN */
    } else if (('0' <= *input) && (*input <= '7')) {
      input = scan_octal (input, &tok->intval, wide);
      if (tok->intval > (wide ? m3_widechar_LAST : 0xFF) )
        error
          (_("Out-of-range octal escape in character literal") ); /* NORETURN */
    } else {
      error (_("Unknown escape sequence in character literal") ); /* NORETURN */
      return input;
    }

  } else if (*input == 0) {
    error (_("EOF encountered in character literal") ); /* NORETURN */
    return input;

  } else {
    /* vanilla character literal */
    tok->intval = *input++;
  }

  if (*input == '\'') {
    input++;
  } else {
    error (_("Missing closing quote on character literal") ); /* NORETURN */
  }

  return input;
} /* scan_char */

/* Do not insert a trailing zero character.  A Modula-3 text literal can have
   embedded zero characters, and all uses of this string will use a separate
   length. */
/* BEWARE: This just changes a portion of the user-typed command, in place,
           into the collected string, with escapes and trailing quote
           removed, and trailing \0 inserted; and puts the pointer to
           that into the token.  Don't ever try to rescan the input line! */
static char *
scan_text (input, tok)
  char *input;
  struct m3_token *tok;
{
  char *start, *next;

  input++;  /* skip the leading quote */
  start = input;
  next  = start;

  tok->kind   = TK_TEXT_LIT;
  tok->string = start;

  while (1) {
    if (*input == '"') {
      input++;
      break;

    } else if ((*input == '\n') || (*input == '\r') || (*input == '\f')) {
      error (_("End-of-line encountered in text literal") ); /* NORETURN */
      input++;
      break;

    } else if (*input == '\\') {
      input ++;
      if      (*input == 'n')  { *next++ = '\n';  input++; }
      else if (*input == 't')  { *next++ = '\t';  input++; }
      else if (*input == 'r')  { *next++ = '\r';  input++; }
      else if (*input == 'f')  { *next++ = '\f';  input++; }
      else if (*input == '\\') { *next++ = '\\';  input++; }
      else if (*input == '\'') { *next++ = '\'';  input++; }
      else if (*input == '"')  { *next++ = '"';   input++; }
      else if (*input == 'x' || *input == 'X') {
        LONGEST hexval;
        input = scan_hex (++input, &hexval, 1);
        *next++ = hexval;
      } else if (*input == 'U') {
        LONGEST hexval;
        input = scan_hex (++input, &hexval, 3);
        if (hexval > 0xFF) 
          error
            (_("Out-of-range Unicode escape in text literal") ); /* NORETURN */
        *next++ = hexval;
      } else if (('0' <= *input) && (*input <= '7')) {
        LONGEST octval;
        input = scan_octal (input, &octval, FALSE);
        if (octval > 0xFF) 
          error
            (_("Out-of-range octal escape in text literal") ); /* NORETURN */
        *next++ = octval;
      } else {
        error (_("Unknown escape sequence in text literal") ); /* NORETURN */
      }

    } else if (*input == 0) {
      error (_("EOF encountered in text literal") ); /* NORETURN */
      break;

    } else {
      /* vanilla character */
      *next++ = *input++;
    }
  }

  /* finish the string. */
  tok->length = next - start;
  return input;
} /* scan_text */

/* Do not insert a trailing zero character.  A Modula-3 text literal can have
   embedded zero characters, and all uses of this string will use a separate
   length. */
/* PRE: input points to the opening double quote.
   POST: tok->string is a newly-allocated (by malloc) array of bytes,
         possibly a bit overlength, containing the wide characters
         and terminated by a wide null.
   POST: tok->length is length in bytes of the actual contents of
         tok->string.
*/
static char *
scan_widetext ( char *input, struct m3_token *tok )

  { char *start;
    char *out;
    int wchar_value;
    int len;

    input++;  /* skip the leading quote */
    start = input;

    /* prescan the string to estimate its final length */
    len = 0;
    while ( TRUE ) {
      if (*input == '"') {
        input++;
        break;

      } else if ((*input == '\n') || (*input == '\r') || (*input == '\f')) {
        input++;
        break;

      } else if (*input == '\\') {
        input ++;
        if      (*input == 'n')  { len++;  input++; }
        else if (*input == 't')  { len++;  input++; }
        else if (*input == 'r')  { len++;  input++; }
        else if (*input == 'f')  { len++;  input++; }
        else if (*input == '\\') { len++;  input++; }
        else if (*input == '\'') { len++;  input++; }
        else if (*input == '"')  { len++;  input++; }
        else if (*input == 'x' || *input == 'X'|| *input == 'U')  {
          len++;  input++; /* The hex digits all count as one widechar. */
        } else if (('0' <= *input) && (*input <= '7')) {
          len++;  input++; /* The octal digits all count as one widechar. */
        } else {
          len++;  input++;  /* ??? */
        }

      } else if (*input == 0) {
        break;

      } else {
        len++;  input++; /* vanilla character */
      }
    }

    /* finally, scan and build the string */
    out   = (char *) malloc (2 * (len + 1));
    /* ^ This gets freed in m3-exp.c, m3_parse_e8. */
    input = start;

    tok->kind   = TK_WIDETEXT_LIT;
    tok->string = (char *) out;

    while ( TRUE ) {
      if (*input == '"') {
        input++;
        break; /* Exit loop. */

      } else if ((*input == '\n') || (*input == '\r') || (*input == '\f')) {
        warning (_("End-of-line encountered in text literal") );
        input++;
        break; /* Exit loop. */

      } else if (*input == '\\') {
        input ++;
        if      (*input == 'n')  { wchar_value = '\n';  input++; }
        else if (*input == 't')  { wchar_value = '\t';  input++; }
        else if (*input == 'r')  { wchar_value = '\r';  input++; }
        else if (*input == 'f')  { wchar_value = '\f';  input++; }
        else if (*input == '\\') { wchar_value = '\\';  input++; }
        else if (*input == '\'') { wchar_value = '\'';  input++; }
        else if (*input == '"')  { wchar_value = '"';   input++; }
        else if (*input == 'x' || *input == 'X') {
          LONGEST hexval;
          input = scan_hex (++input, &hexval, 2 );
          wchar_value = hexval; 
        } else if (*input == 'U') {
          LONGEST hexval;
          input = scan_hex (++input, &hexval, 3);
          if (hexval > m3_widechar_LAST) 
            error
              (_("Out-of-range Unicode escape in wide text literal") ); /* NORETURN */
          wchar_value = hexval; 
        } else if (('0' <= *input) && (*input <= '7')) {
          LONGEST octval;
          input = scan_octal (input, &octval, TRUE );
          wchar_value = octval; 
        } else {
          error
            (_("Unknown escape sequence in wide text literal") ); /* NORETURN */
        }

      } else if (*input == 0) {
        error (_("EOF encountered in wide text literal") ); /* NORETURN */
        break;

      } else {
        /* vanilla character */
        wchar_value = *input++;
      }
      store_unsigned_integer ( out, m3_widechar_byte, wchar_value );
      out += m3_widechar_byte;
    }

    /* finish the string */
    tok->length = out - tok->string;

    return input;
  } /* scan_widetext */

/*-------------------------------------------------------------- comments ---*/

static char *
skip_comment (input)
   char *input;
{
  int nest = 1;

  while (nest > 0) {
    if (*input == '*') {
      input++;  if (*input == ')') { nest--; input++; }
    } else if (*input == '(') {
      input++;  if (*input == '*') { nest++; input++; }
    } else if (*input == 0) {
      nest = 0;
      error (_("EOF encountered in comment" ) ); /* NORETURN */
    } else {
      input++;
    }
  }

  return input;
} /* skip_comment */

/*------------------------------------------------------ Modula-3 scanner ---*/

char*
scan_m3_token (input, tok)
     char *input;
     struct m3_token *tok;
{
  char *tokstart;
  int   len;

  /* skip white space */
  while (*input == ' ' || *input == '\t' || *input == '\r' || *input == '\n') {
    input++;
  }

  /* remember where this token starts */
  tokstart = input;

  tok->kind     = TK_ERROR;
  tok->string   = input;
  tok->length   = strlen (input);
  tok->intval   = 0;
  tok->floatval = 0.0;

  switch (*tokstart) {
    case '_': case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
    case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': case 'M':
    case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T':
    case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z': case 'a':
    case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h':
    case 'i': case 'j': case 'k': case 'l': case 'm': case 'n': case 'o':
    case 'p': case 'q': case 'r': case 's': case 't': case 'u': case 'v':
    case 'w': case 'x': case 'y': case 'z':
      /* scan an identifier */
      input = tokstart + 1;

      if (*input == '\'' && ( * tokstart == 'w' || * tokstart == 'W' ) )
        { /* oops, it's actually a wide char literal. */
          if ( m3_compiler_kind ( ) != m3_ck_cm3 )
            { error
                (_("WIDECHAR literals not supported by this "
                   "Modula-3 compiler ") );
            } /* NORETURN */
          input = scan_char ( input, tok, TRUE );
           break;
        }

      if (*input == '"' && ( * tokstart == 'w' || * tokstart == 'W' ) )
        { /* oops, it's actually a wide text literal. */
          if ( m3_compiler_kind ( ) != m3_ck_cm3 )
            { error
                (_("Wide text literals not supported by this "
                   "Modula-3 compiler ") );
            } /* NORETURN */
          input = scan_widetext ( input, tok);
          break;
        }

      while (   ('a' <= *input && *input <= 'z')
             || ('A' <= *input && *input <= 'Z')
             || ('0' <= *input && *input <= '9')
             || (*input == '_')) {
        input++;
      }
      len = input - tokstart;
      tok->kind = TK_IDENT;
      tok->string = (char *) malloc (len + 1);
      strncpy (tok->string, tokstart, len);
      tok->string [len] = '\0';
      tok->length = len;
      recognize_reserved_word (tok);
      break;

    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      input = m3_scan_number (tokstart, tok);
      break;

    case '\'':
      input = scan_char (tokstart, tok, FALSE );
      break;

    case '"':
      input = scan_text (tokstart, tok);
      break;

    case '+':  tok->kind = TK_PLUS;       input++;  break;
    case '-':  tok->kind = TK_MINUS;      input++;  break;
    case '/':  tok->kind = TK_SLASH;      input++;  break;
    case '&':  tok->kind = TK_AMPERSAND;  input++;  break;
    case ',':  tok->kind = TK_COMMA;      input++;  break;
    case ';':  tok->kind = TK_SEMI;       input++;  break;
    case '[':  tok->kind = TK_LBRACKET;   input++;  break;
    case '{':  tok->kind = TK_LBRACE;     input++;  break;
    case '^':  tok->kind = TK_ARROW;      input++;  break;
    case '#':  tok->kind = TK_SHARP;      input++;  break;
    case ')':  tok->kind = TK_RPAREN;     input++;  break;
    case ']':  tok->kind = TK_RBRACKET;   input++;  break;
    case '}':  tok->kind = TK_RBRACE;     input++;  break;
    case '|':  tok->kind = TK_BAR;        input++;  break;
    case '*':  tok->kind = TK_ASTERISK;   input++;  break;
    case   0:  tok->kind = TK_EOF;                  break;

    case '=':
      tok->kind = TK_EQUAL;  input++;
      if (*input == '>') { tok->kind = TK_IMPLIES; input++; }
      break;

    case ':':
      tok->kind = TK_COLON;  input++;
      if (*input == '=') { tok->kind = TK_ASSIGN; input++; }
      break;

    case '.':
      tok->kind = TK_DOT;  input++;
      if (*input == '.') { tok->kind = TK_DOTDOT; input++; }
      break;

    case '(':
      tok->kind = TK_LPAREN;  input++;
      if (*input == '*') {
       return scan_m3_token (skip_comment (++input), tok);
      }
      break;

    case '>':
      tok->kind = TK_GREATER;  input++;
      if (tokstart[1] == '=') { tok->kind = TK_GREQUAL; input++; }
      break;

    case '<':
      if      (tokstart[1] == '=') { tok->kind = TK_LSEQUAL; input++; }
      else if (tokstart[1] == ':') { tok->kind = TK_SUBTYPE; input++; }
      else                         { tok->kind = TK_LESS; }
      input++;
      break;

    case '$':
      input = scan_gdb_token (input, tok);
      break;

    default:
      error (_("Can't recognize start of Modula-3 token: %s"), input );
        /* ^NORETURN */

    } /* switch */

    return input;
} /* scan_m3_token */

/*----------------------------------------------------------- token names ---*/

static char* toknames[] = {

  /* KEEP-ME-UP-TO-DATE: This must match enum m3_token_kind, in m3-token.h */

  "<EOF>",

  /* literals */

  "<IDENT>",            /* identifier       => string, length */
  "<CARD_LIT>",         /* CARDINAL literal => intval         */
  "<REAL_LIT>",         /* REAL literal     => floatval       */
  "<LREAL_LIT>",        /* LONGREAL literal => floatval       */
  "<XREAL_LIT>",        /* EXTENDED literal => floatval       */
  "<CHAR_LIT>",         /* CHAR literal     => intval         */
  "<WIDECHAR_LIT>",     /* WIDECHAR literal => intval         */
  "<TEXT_LIT>",         /* TEXT literal     => string, length */
  "<WIDETEXT_LIT>",     /* W"" TEXT literal => string, length */

  /* operators */

  "+", "-", "*", "/", ":=", "&",
  ".", ",", ";", "(", "[", "{",
  "^", "=", "#", "<", ">", "<=",
  ">=", "..", ":", ")", "]", "}",
  "|", "<:", "=>",

  /* reserved words */

  "AND", "ANY", "ARRAY", "AS", "BEGIN", "BITS", "BRANDED", "BY",
  "CASE", "CONST", "DIV", "DO", "ELSE", "ELSIF", "END", "EVAL",
  "EXCEPT", "EXCEPTION", "EXIT", "EXPORTS", "FINALLY", "FOR",
  "FROM", "GENERIC", "IF", "IMPORT", "IN", "INTERFACE", "LOCK",
  "LOOP", "METHODS", "MOD", "MODULE", "NOT", "OBJECT", "OF",
  "OR", "OVERRIDES", "PROCEDURE", "RAISE", "RAISES", "READONLY",
  "RECORD", "REF", "REPEAT", "RETURN", "REVEAL", "ROOT", "SET", "THEN",
  "TO", "TRY", "TYPE", "TYPECASE", "UNSAFE", "UNTIL", "UNTRACED",
  "VALUE", "VAR", "WHILE", "WITH",

  /* reserved identifiers */

  "ABS", "ADDRESS", "ADR", "ADRSIZE", "BITSIZE", "BOOLEAN",
  "BYTESIZE", "CARDINAL", "CEILING", "CHAR", "DEC", "DISPOSE",
  "EXTENDED", "FALSE", "FIRST", "FLOAT", "FLOOR", "INC",
  "INTEGER", "ISTYPE", "LAST", "LONGREAL", "LOOPHOLE", "MAX",
  "MIN", "MUTEX", "NARROW", "NEW", "NIL", "NULL", "NUMBER",
  "ORD", "REAL", "REFANY", "ROUND", "SUBARRAY", "TEXT", "TRUE",
  "TRUNC", "TYPECODE", "VAL", "WIDECHAR",

  /* misc. debugger tokens */

  "<GDB HISTORY>",       /*  $n   - history reference  => intval      */
  "<REGISTER>",          /*  $rn  - register reference => intval      */
  "<GDB VAR>",           /*  $id  - GDB variable       => string, len */
  "<ERROR>"

};

char *
m3_token_name (tok)
  struct m3_token *tok;
{
  return toknames [(int)tok->kind];
    /* good enough for now... */
}

/* End of file m3-token.c */
