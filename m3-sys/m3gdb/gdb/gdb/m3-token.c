#include "defs.h"
#include "expression.h"
#include "value.h"
#include "parser-defs.h"
#include "m3-token.h"

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
  { "WITH",      TK_WITH      }
};

static void
recognize_reserved_word (tok)
   m3_token *tok;
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
scan_number (input, tok)
     char *input;
     m3_token *tok;
{
  char *c;
  int i;

  /* scan the leading decimal digits */
  c = input;
  while ('0' <= *c && *c <= '9') { c++; }

  if (*c == '_') {
    /* scan a based integer */
    LONGEST base, val;
    int digit;

    /* get the base */
    i = sscanf (input, "%ld", &base);
    if ((i <= 0) || (base < 2) || (16 < base)) {
      error ("illegal base for based literal, 10 used");
      base = 10;
    }

    /* scan the value */
    val = 0;
    c++;  input = c;
    while (1) {
      if      ('0' <= *c && *c <= '9') { digit = *c - '0'; }
      else if ('A' <= *c && *c <= 'F') { digit = *c - 'A' + 10; }
      else if ('a' <= *c && *c <= 'f') { digit = *c - 'a' + 10; }
      else { break; }
      if (digit >= base) { break; }
      val = val * base + digit;
      c++;
    }

    if (c == input) {
      error ("illegal based integer literal, zero used");
      val = 0;
    }

    tok->kind   = TK_CARD_LIT;
    tok->intval = val;

  } else if ((*c == '.') && (c[1] != '.')) {
    /* scan a floating point number */
    c++; /* skip the decimal point */

    /* scan the fractional digits */
    if ((*c < '0') || ('9' < *c)) {
      error ("missing digits in real fraction");
    }
    while ('0' <= *c && *c <= '9') { c++; }

    /* check for the exponent */
    if ((*c == 'e') || (*c == 'E')) {
      *c++ = 'e';  /* since atof only knows about 'e' */
      tok->kind = TK_REAL_LIT;
    } else if ((*c == 'd') || (*c == 'D')) {
      *c++ = 'e';  /* since atof only knows about 'e' */
      tok->kind = TK_LREAL_LIT;
    } else if ((*c == 'x') || (*c == 'X')) {
      *c++ = 'e';  /* since atof only knows about 'e' */
      tok->kind = TK_XREAL_LIT;
    } else { /* real constant with no exponent */
      tok->kind = TK_REAL_LIT;
      tok->floatval = atof (input);
      return c;
    }

    /* check for an exponent sign */
    if ((*c == '+') || (*c == '-')) c++;

    /* scan the exponent digits */
    if ((*c < '0') || ('9' < *c)) {
      error ("missing digits in real exponent");
    }
    while ('0' <= *c && *c <= '9') { c++; }

    /* and do the conversion... */
    tok->floatval = atof (input);

  } else {
    /* already scanned a decimal integer */
    tok->kind   = TK_CARD_LIT;
    sscanf (input, "%ld", &tok->intval);

  }
  return c;
} /* scan_number */

/*------------------------------------------------------------- GDB tokens --*/

static char *
scan_gdb_token (input, tok)
  char *input;
  m3_token *tok;
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
    if (len == strlen (reg_names [c])
        && STREQN (tokstart, reg_names[c], len)) {
      tok->kind = TK_REGISTER;
      tok->intval = c;
      return input;
    }
  }

  /* check for a pseudo-register name */
  for (c = 0; c < num_std_regs; c++) {
    if (len == strlen (std_regs [c].name)
        && STREQN (tokstart, std_regs[c].name, len)) {
      tok->kind = TK_REGISTER;
      tok->intval = std_regs[c].regnum; 
      return input;
    }
  }

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
bad_octal ()
{
  error ("octal character constant must have 3 digits");
} /* bad_octal */


static char *
scan_octal (input, val)
  char *input;
  LONGEST *val;
{
  *val = 0;
  if ((*input < '0') || ('7' < *input)) { bad_octal();  return input; }
  *val = *input++ - '0'; 
  if ((*input < '0') || ('7' < *input)) { bad_octal();  return input; }
  *val = 8 * (*val) + (*input++ - '0'); 
  if ((*input < '0') || ('7' < *input)) { bad_octal();  return input; }
  *val = 8 * (*val) + (*input++ - '0'); 
  return input;
} /* scan_octal */


static char *
scan_char (input, tok)
  char *input;
  m3_token *tok;
{
  int val = 0;

  tok->kind   = TK_CHAR_LIT;
  tok->intval = 0;

  input++;  /* skip opening quote */

  if (*input == '\'') {
    error ("missing character in character literal");
    return input+1;

  } else if ((*input == '\n') || (*input == '\r') || (*input == '\f')) {
    error ("end-of-line encountered in character literal");
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
    else if (('0' <= *input) && (*input <= '7')) {
      input = scan_octal (input, &tok->intval);
    } else {
      error ("unknown escape sequence in character literal");
      return input;
    }

  } else if (*input == 0) {
    error ("EOF encountered in character literal");
    return input;

  } else {
    /* vanilla character literal */
    tok->intval = *input++;
  }

  if (*input == '\'') {
    input++;
  } else {
    error ("missing closing quote on character literal");
  }

  return input;
} /* scan_char */

static char *
scan_text (input, tok)
  char *input;
  m3_token *tok;
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
      error ("end-of-line encountered in text literal");
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
      else if (('0' <= *input) && (*input <= '7')) {
	LONGEST octval;
        input = scan_octal (input, octval);
	*next++ = octval;
      } else {
        error ("unknown escape sequence in text literal");
      }

    } else if (*input == 0) {
      error ("EOF encountered in text literal");
      break;

    } else {
      /* vanilla character */
      *next++ = *input++;
    }
  }

  /* finish the string */
  *next++ = 0;
  tok->length = next - start;
  return input;
} /* scan_text */

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
      error ("EOF encountered in comment");  nest = 0;
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
     m3_token *tok;
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
      input = scan_number (tokstart, tok);
      break;

    case '\'':
      input = scan_char (tokstart, tok);
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
      error ("can't recognize start of Modula-3 token: %s", input);

    } /* switch */

    return input;
} /* scan_m3_token */

/*----------------------------------------------------------- token names ---*/

static char* toknames[] = {

  "<EOF>", 

  /* literals */

  "<IDENT>",            /* identifier       => string, length */
  "<CARD_LIT>",         /* CARDINAL literal => intval         */
  "<REAL_LIT>",         /* REAL literal     => floatval       */
  "<LREAL_LIT>",        /* LONGREAL literal => floatval       */
  "<XREAL_LIT>",        /* EXTENDED literal => floatval       */
  "<CHAR_LIT>",         /* CHAR literal     => intval         */
  "<TEXT_LIT>",         /* TEXT literal     => string, length */

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
  "TRUNC", "TYPECODE", "VAL",

  /* misc. debugger tokens */

  "<GDB HISTORY>",       /*  $n   - history reference  => intval      */
  "<REGISTER>",          /*  $rn  - register reference => intval      */
  "<GDB VAR>",           /*  $id  - GDB variable       => string, len */
  "<ERROR>"

};

char *
m3_token_name (tok)
  m3_token *tok;
{
  return toknames [(int)tok->kind];
    /* good enough for now... */
}
