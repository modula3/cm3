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

/* This file contains code to parse, print (unevaluated) and dump
   Modula-3 expressions. */ 

#include <stdbool.h> 

#include "defs.h"
#include "block.h"
#include "dictionary.h" 
#include "expression.h"
#include "gdbtypes.h"
#include "gdb_string.h"
#include "language.h"
#include "parser-defs.h"
#include "value.h"

#include "m3-exp.h"
#include "m3-lang.h"
#include "m3-token.h"
#include "m3-util.h"

extern char* lexptr;
static struct m3_token cur_tok;

/** #define DEBUG_M3_SCANNER **/

static void get_token ()

{
#ifdef DEBUG_M3_SCANNER
  char *before = lexptr;
#endif
  lexptr = scan_m3_token (lexptr, &cur_tok);
#ifdef DEBUG_M3_SCANNER
#define xxstr(x) (x == 0 ? "<NIL>" : x)
  printf ("scan_m3_token: %s\n  before: %s\n  after:  %s\n",
	  m3_token_name (&cur_tok), xxstr(before), xxstr(lexptr));
#endif
} /* get_token */ 

static void 
write_exp_text (
    enum exp_opcode opcode,
    const char *str,
    int len
  )

{
  struct stoken t;
  
  write_exp_elt_opcode (opcode); 
  t.ptr = ( char * ) str;
  t.length = len;
  write_exp_string (t);
  write_exp_elt_opcode (opcode); 
} /* write_exp_text */ 

static void 
write_exp_var ( struct symbol *sym, struct block *blk )
     
{
  write_exp_elt_opcode ( OP_VAR_VALUE );
  write_exp_elt_block ( blk );
  write_exp_elt_sym ( sym );
  write_exp_elt_opcode ( OP_VAR_VALUE );
} /* write_exp_var */ 

static void
write_m3_const ( LONGEST val, struct type *tipe ) 
{
  write_exp_elt_opcode (OP_M3_LONG);
  write_exp_elt_type (tipe);
  write_exp_elt_longcst (val);
  write_exp_elt_opcode (OP_M3_LONG); 
  get_token ();
} /* write_m3_const */ 

static void 
write_m3_type ( struct type *tipe ) 
{
  write_exp_elt_opcode (OP_M3_TYPE);
  write_exp_elt_type (tipe);
  write_exp_elt_opcode (OP_M3_TYPE); 
  get_token ();
}

static int m3_parse_expr ( ); /* Forward decl. */ 

static void 
m3_builtin_1_param ( enum exp_opcode op ) 
{
  get_token (); /* builtin function name */
  if (cur_tok.kind != TK_LPAREN) { error ("missing opening ("); }
  get_token ();
  m3_parse_expr ();
  if (cur_tok.kind != TK_RPAREN) { error ("missing closing )"); }
  get_token ();
  write_exp_elt_opcode (op); 
} /* m3_builtin_1_param */

static void 
m3_builtin_2_params ( enum exp_opcode op ) 
{
  get_token (); /* builtin function name */
  if (cur_tok.kind != TK_LPAREN) { error ("missing opening ("); }
  get_token ();
  m3_parse_expr ();
  if (cur_tok.kind != TK_COMMA) { error ("missing second parameter"); }
  get_token ();
  m3_parse_expr ();
  if (cur_tok.kind != TK_RPAREN) { error ("missing closing )"); }
  get_token ();
  write_exp_elt_opcode (op); 
} /* m3_builtin_2_params */

static void 
m3_float_op ( enum exp_opcode op ) 
{
  get_token (); /* builtin function name */
  if (cur_tok.kind != TK_LPAREN) { error ("missing opening ("); }
  get_token ();
  m3_parse_expr ();
  if (cur_tok.kind == TK_COMMA) {
    get_token ();
    m3_parse_expr ();
  } else {
    write_exp_elt_opcode (OP_M3_TYPE);
    write_exp_elt_type (builtin_type_m3_real);
    write_exp_elt_opcode (OP_M3_TYPE); 
  }
  if (cur_tok.kind != TK_RPAREN) { error ("missing closing )"); }
  get_token ();
  write_exp_elt_opcode (op); 
} /* m3_float_op */


static void 
m3_not_yet ( char *nm ) 
{
  int depth = 1;

  error ("Modula-3 builtin function %s is not supported", nm);
  get_token ();
  if (cur_tok.kind != TK_LPAREN) { return; }
  get_token ();
  depth = 1;
  while (depth > 0) {
    if      (cur_tok.kind == TK_EOF)    { break; }
    else if (cur_tok.kind == TK_ERROR)  { break; }
    else if (cur_tok.kind == TK_LPAREN) { depth++; get_token (); }
    else if (cur_tok.kind == TK_RPAREN) { depth--; get_token (); }
    else                                { get_token (); }
  }
} /* m3_not_yet */ 

/* For investigation of what is in blocks and dictionaries: */ 

void 
all_blocks_and_syms ( struct block * b ) 

{ struct block * bl; 
  struct symbol * sym; 
  struct dict_iterator iter;
  char * name ; 
  char * linkage_name ; 
  int i; 

  bl = b; 
  i = 0; 
  while ( true ) 
    { if ( bl == NULL ) 
        { break; } 
      printf ( "Block no %d\n", i ); 
      ALL_BLOCK_SYMBOLS ( bl, iter, sym )
        { if ( sym != NULL ) 
            { name = SYMBOL_NATURAL_NAME ( sym ); 
              linkage_name = SYMBOL_LINKAGE_NAME ( sym ); 
              printf 
                ( "name=\"%s\", linkage_name=\"%s\"\n", name, linkage_name );  
            } 
        } 
      bl = BLOCK_SUPERBLOCK ( bl ); 
      i ++; 
    } 
}

/* Write a little expression that denotes something named, taking care
   of the globals_ref case and also qualified enumeration constants. 
   PRE: The (first) TK_IDENT is already consumed. */ 
static int  
m3_write_id_ref ( struct symbol * sym, const char * name ) 

  { struct type * type_name_type; 
    struct type * resolved_type; 
    struct stoken tok;

    if ( m3_is_globals_record_symbol ( sym ) ) 
      { /* This is the ugly case of a global variable, and it has 
           no symbol.  Instead, lookup_symbol returned the globals
           record, to signal the case, and we have to concoct a
           little field-selecting subexpression from it. */ 
        write_exp_var ( sym, block_found );
        write_exp_text ( STRUCTOP_M3_MODULE, name, strlen ( name ) );
        return 0; 
      } 
    else if ( m3_is_type_name_symbol ( sym ) ) 
      { type_name_type = SYMBOL_TYPE ( sym ); 
        resolved_type 
          = m3_resolve_type ( TYPE_FIELD_M3_UID ( type_name_type, 0 ) ); 
        if ( TYPE_CODE ( resolved_type ) == TYPE_CODE_M3_ENUM 
             && cur_tok . kind == TK_DOT 
           )          
          /* It's the start of a (qualified) enumeration constant name. */ 
          { get_token  ( ); /* Consume TK_DOT. */ 
            if ( cur_tok . kind != TK_IDENT ) 
              { error 
                  ( "Enumeration constant %s requires a selector after the dot",
                     name
                  );
                return 1;
              }
            write_exp_elt_opcode ( STRUCTOP_M3_ENUM ); 
            write_exp_elt_sym ( sym );
            write_exp_elt_type ( resolved_type );
            tok.ptr = ( char * ) cur_tok . string;
            tok.length = strlen ( cur_tok . string ); 
            write_exp_string ( tok );
            write_exp_elt_opcode ( STRUCTOP_M3_ENUM );
            get_token  ( ); /* Consume the second TK_IDENT. */  
            return 0; 
          } 
        else 
          { write_exp_elt_opcode ( OP_M3_TYPE );
            write_exp_elt_type ( resolved_type );
            write_exp_elt_opcode ( OP_M3_TYPE ); 
            return 0; 
          } 
      } 
    else 
      { write_exp_var ( sym, block_found ); 
        return 0; 
      } 
  } /* m3_write_id_ref */ 

static int 
m3_parse_e8 ( )
{ struct dict_iterator iter; 

  switch (cur_tok.kind) {

    case TK_EOF:
      error ("unexpected EOF in expression");
      return 1;

    case TK_IDENT: 
      { struct symbol * sym;
        char * name = NULL;
        char * unit_name = NULL;
        struct symtab * l_symtab; 

        /* Try to find the ident as an unqualified reference (to something
           other than an interface.) */ 
        sym = lookup_symbol 
                ( cur_tok . string, 
                  m3_proc_body_block ( expression_context_block ),
                  VAR_DOMAIN, 0, NULL
                ); 
        if ( sym != NULL ) 
          { name = cur_tok . string;
            get_token  ( ); /* Consume TK_IDENT. */ 
            return m3_write_id_ref ( sym, name );  
          } 

        /* Try to make this the start of a qualified reference, either 
           <interfaceName>.<decl>.  Other meanings of dot are handled 
           during evaluation. */ 
        if ( m3_unit_name_globals_symbol ( 'I', cur_tok . string, NULL ) ) 
          { /* Name of an interface.  We take the extravagant view that
               every interface name is accessible in m3gdb commands without
               needing anything like an IMPORT. There is no other possible
               interpretation, so it's error messages if this doesn't work. */
            unit_name = cur_tok . string;  
            get_token ( ); /* Consume TK_IDENT */ 
            if ( cur_tok . kind != TK_DOT ) 
              { error ( "Interface name %s requires a dot and identifier", 
                        unit_name
                      );
                return 1;
              }
            get_token ( ); /* Consume TK_DOT */ 
            if ( cur_tok . kind != TK_IDENT ) 
              { error ( "Interface name %s requires a selector after the dot", 
                         unit_name
                      );
                return 1;
              }
            name = cur_tok . string; 
            sym = m3_lookup_interface_id ( unit_name, name, & l_symtab ); 
            if ( sym != NULL ) 
              { get_token  ( ); /* Consume second TK_IDENT */ 
                return m3_write_id_ref ( sym, name );  
              } 
            error ( "Can't find Modula-3 qualified reference: %s.%s", 
                    unit_name, name
                  ); /* NORETURN */ 
          } 

        error ( "Can't find Modula-3 identifier: %s", cur_tok . string ); 
          /* NORETURN */ 
      } /* case TK_IDENT */
      
    case TK_CARD_LIT:
      write_m3_const (cur_tok.intval, builtin_type_m3_integer);
      break; 
      
    case TK_REAL_LIT:
      write_exp_elt_opcode (OP_M3_REEL);
      write_exp_elt_type (builtin_type_m3_real);
      write_exp_elt_dblcst (cur_tok.floatval);
      write_exp_elt_opcode (OP_M3_REEL); 
      get_token ();
      break;
      
    case TK_LREAL_LIT:
      write_exp_elt_opcode (OP_M3_LREEL);
      write_exp_elt_type (builtin_type_m3_longreal);
      write_exp_elt_dblcst (cur_tok.floatval);
      write_exp_elt_opcode (OP_M3_LREEL); 
      get_token ();
      break;
      
    case TK_XREAL_LIT:
      write_exp_elt_opcode (OP_M3_XREEL);
      write_exp_elt_type (builtin_type_m3_extended);
      write_exp_elt_dblcst (cur_tok.floatval);
      write_exp_elt_opcode (OP_M3_XREEL); 
      get_token ();
      break;

    case TK_CHAR_LIT:
      write_exp_elt_opcode (OP_M3_CHAR);
      write_exp_elt_type (builtin_type_m3_char);
      write_exp_elt_longcst ((LONGEST) cur_tok.intval);
      write_exp_elt_opcode (OP_M3_CHAR); 
      get_token ();
      break;

    case TK_WIDECHAR_LIT:
      write_exp_elt_opcode (OP_M3_WIDECHAR);
      write_exp_elt_type (builtin_type_m3_widechar);
      write_exp_elt_longcst ((LONGEST) cur_tok.intval);
      write_exp_elt_opcode (OP_M3_WIDECHAR); 
      get_token ();
      break;

    case TK_TEXT_LIT: {
      struct stoken str;
      str.ptr = cur_tok.string;
      str.length = cur_tok.length;
      write_exp_elt_opcode (OP_M3_TEXT);
      write_exp_string (str);
      write_exp_elt_opcode (OP_M3_TEXT); 
      get_token ();
      break;
    }

    case TK_WIDETEXT_LIT: {
      struct stoken str;
      str.ptr = cur_tok.string;
      str.length = cur_tok.length;
      write_exp_elt_opcode (OP_M3_WIDETEXT);
      write_exp_string (str);
      write_exp_elt_opcode (OP_M3_WIDETEXT); 
      get_token ();
      break;
    }

    case TK_LPAREN:
      get_token ();
      m3_parse_expr ();
      if (cur_tok.kind != TK_RPAREN) { error ("missing closing )"); }
      get_token ();
      break;
   
    case TK_GDB_HISTORY:
      write_exp_elt_opcode (OP_LAST);
      write_exp_elt_longcst ((LONGEST) cur_tok.intval);
      write_exp_elt_opcode (OP_LAST);
      get_token ();
      break; 

    case TK_GDB_VAR: 
      write_exp_elt_opcode (OP_INTERNALVAR);
      write_exp_elt_intern (lookup_internalvar (cur_tok.string));
      write_exp_elt_opcode (OP_INTERNALVAR);
      get_token ();
      break;

    case TK_REGISTER:
      write_exp_elt_opcode (OP_REGISTER);
      write_exp_elt_longcst ((LONGEST) cur_tok.intval);
      write_exp_elt_opcode (OP_REGISTER);
      get_token ();
      break;

    /*---- builtin functions  ----*/

    case TK_ABS:       m3_builtin_1_param (UNOP_M3_ABS);       break;
    case TK_ADR:       m3_builtin_1_param (UNOP_M3_ADR);       break;
    case TK_ADRSIZE:   m3_builtin_1_param (UNOP_M3_ADRSIZE);   break;
    case TK_BITSIZE:   m3_builtin_1_param (UNOP_M3_BITSIZE);   break;
    case TK_BYTESIZE:  m3_builtin_1_param (UNOP_M3_BYTESIZE);  break;
    case TK_CEILING:   m3_builtin_1_param (UNOP_M3_CEILING);   break;
    case TK_DEC:       m3_not_yet ("DEC");               break;
    case TK_DISPOSE:   m3_not_yet ("DISPOSE");           break;
    case TK_FIRST:     m3_builtin_1_param (UNOP_M3_FIRST);     break;
    case TK_FLOAT:     m3_float_op (BINOP_M3_FLOAT);     break;
    case TK_FLOOR:     m3_builtin_1_param (UNOP_M3_FLOOR);     break;
    case TK_INC:       m3_not_yet ("INC");               break;
    case TK_ISTYPE:    m3_not_yet ("ISTYPE");            break;
    case TK_LAST:      m3_builtin_1_param (UNOP_M3_LAST);      break;
    case TK_LOOPHOLE:  m3_builtin_2_params (BINOP_M3_LOOPHOLE); break;
    case TK_MAX:       m3_builtin_2_params (BINOP_M3_MAX);      break;
    case TK_MIN:       m3_builtin_2_params (BINOP_M3_MIN);      break;
    case TK_NARROW:    m3_not_yet ("NARROW");            break;
    case TK_NEW:       m3_not_yet ("NEW");               break;
    case TK_NUMBER:    m3_builtin_1_param (UNOP_M3_NUMBER);    break;
    case TK_ORD:       m3_builtin_1_param (UNOP_M3_ORD);       break;
    case TK_ROUND:     m3_builtin_1_param (UNOP_M3_ROUND);     break;
    case TK_SUBARRAY:  m3_not_yet ("SUBARRAY");          break;
    case TK_TRUNC:     m3_builtin_1_param (UNOP_M3_TRUNC);     break;
    case TK_TYPECODE:  m3_not_yet ("TYPECODE");          break;
    case TK_VAL:       m3_builtin_2_params (BINOP_M3_VAL);      break;

    /*---- builtin types ---- */

    case TK_ADDRESS:   write_m3_type (builtin_type_m3_address);   break;
    case TK_BOOLEAN:   write_m3_type (builtin_type_m3_boolean);   break;
    case TK_CARDINAL:  write_m3_type (builtin_type_m3_cardinal);  break;
    case TK_CHAR:      write_m3_type (builtin_type_m3_char);      break;
    case TK_EXTENDED:  write_m3_type (builtin_type_m3_extended);  break;
    case TK_INTEGER:   write_m3_type (builtin_type_m3_integer);   break;
    case TK_LONGREAL:  write_m3_type (builtin_type_m3_longreal);  break;
    case TK_MUTEX:     write_m3_type (builtin_type_m3_mutex);     break;
    case TK_NULL:      write_m3_type (builtin_type_m3_null);      break;
    case TK_REAL:      write_m3_type (builtin_type_m3_real);      break;
    case TK_REFANY:    write_m3_type (builtin_type_m3_refany);    break;
    case TK_ROOT:      write_m3_type (builtin_type_m3_root);      break;
    case TK_TEXT:      write_m3_type (builtin_type_m3_text);      break;
    case TK_WIDECHAR:  write_m3_type (builtin_type_m3_widechar);  break;

    case TK_UNTRACED:
      get_token ();
      if (cur_tok.kind != TK_ROOT) {
	error ("UNTRACED not followed by ROOT");
      };
      write_m3_type (builtin_type_m3_untraced_root);
      break;

    /*---- builtin constants ---*/

    case TK_TRUE:  write_m3_const ((LONGEST) 1, builtin_type_m3_boolean); break;
    case TK_FALSE: write_m3_const ((LONGEST) 0, builtin_type_m3_boolean); break;
    case TK_NIL:   write_m3_const ((LONGEST) 0, builtin_type_m3_null);    break;

    /*--- programmer-defined types --- */ 

    case TK_REF: 
    case TK_ARRAY: 
    case TK_RECORD: 
    case TK_SET: 
    case TK_BITS: 
    case TK_OBJECT: 
    case TK_BRANDED: 
      error ( "Programmer-defined types not implemented: \"%s\".", 
              m3_token_name (&cur_tok)
            ); 

    default: 
      error ("unexpected token in expression \"%s\" (kind = %d)",
	      m3_token_name (&cur_tok), (int)cur_tok.kind );
      return 1;

  } /* switch */

  return 0;
} /* m3_parse_e8 */  

static int 
m3_parse_e7 ( )
{

  if (m3_parse_e8 ()) { return 1; }

  while (1) {
    switch (cur_tok.kind) {
      case TK_ARROW: 
	write_exp_elt_opcode (UNOP_M3_DEREF);
	get_token ();
	break;

      case TK_DOT: {
	get_token ();
	/* The case <interfaceName>.<decl> won't reach here, because it is fully
           parsed my m3_parse_e8.  We can't distinguish other meanings of dot 
           constructs here, because we would need the the type of the left 
           subexpression.  So just use STRUCTOP_M3_STRUCT to build a 
           dot-construct expression and let evaluation figure it out later. */

	if (cur_tok.kind != TK_IDENT) {
	  error ("Field name must be an identifier"); 
	  return 1; }

	write_exp_text (STRUCTOP_M3_STRUCT, cur_tok.string, cur_tok.length);
	get_token ();
	break; }

      case TK_LPAREN: {
	extern int arglist_len;
        bool more_args; 
        get_token ();
	start_arglist ();
        if (cur_tok.kind == TK_RPAREN) {get_token ();}
        else {  
          more_args = true;
          while (more_args) { 
            if (m3_parse_expr ()) {return 1;}
            arglist_len++; 
            switch (cur_tok.kind) {
              case TK_COMMA: { get_token(); break; } 
              case TK_RPAREN: { get_token(); more_args = false; break; }
              default: { error ("missing ')'"); return 1;}  
            } /* switch */
          } /* while */ 
        } /* else */ 
        write_exp_elt_opcode (OP_FUNCALL);
	write_exp_elt_longcst 
          ((LONGEST) end_arglist () /* Before prefixify, number of actuals. */ );
	write_exp_elt_opcode (OP_FUNCALL);
	break;
      }
	
      case TK_LBRACKET: {
	struct type *array_type;
	cur_tok.kind = TK_COMMA;
	while (cur_tok.kind == TK_COMMA) {
	  get_token ();
	  if (m3_parse_expr ()) { return 1; }
	  write_exp_elt_opcode (BINOP_M3_SUBSCRIPT);
	}
	
	if (cur_tok.kind == TK_RBRACKET) { get_token (); } 
        else { error ("missing ']'"); return 1; }
	break;
      }

      case TK_EOF:
      default:
	return 0;
    } /* switch */
  } /* while(1) */
} /* m3_parse_e7 */


static int 
m3_parse_e6 ( )
{
  int m = 0;

  while (cur_tok.kind == TK_PLUS || cur_tok.kind == TK_MINUS) {
    if (cur_tok.kind == TK_MINUS) { m++; }
    get_token ();
  }

  if (m3_parse_e7 ()) { return 1; }
  if (m % 2 == 1) { write_exp_elt_opcode (UNOP_M3_NEG); }
  return 0;
} /* m3_parse_e6 */ 

static int 
m3_parse_e5 ( )
{
  if (m3_parse_e6 ()) { return 1; }
  while (1) {
    switch (cur_tok.kind) {
      case TK_ASTERISK: 
	get_token ();
	if (m3_parse_e6 ()) {return 1;}
	write_exp_elt_opcode (BINOP_M3_MULT);
	break;
      case TK_SLASH:
	get_token ();
	if (m3_parse_e6 ()) {return 1;}
	write_exp_elt_opcode (BINOP_M3_DIVIDE);
	break; 
      case TK_DIV:
	get_token ();
	if (m3_parse_e6 ()) {return 1;}
	write_exp_elt_opcode (BINOP_M3_DIV);
	break;
      case TK_MOD:
	get_token ();
	if (m3_parse_e6 ()) {return 1;}
	write_exp_elt_opcode (BINOP_M3_MOD);
	break;
      default:
	return 0;
    }
  }
} /* m3_parse_e5 */ 

static int 
m3_parse_e4 ()
{
  if (m3_parse_e5 ()) { return 1; }
  while (1) {
    switch (cur_tok.kind) {
      case TK_PLUS: 
	get_token ();
	if (m3_parse_e5 ()) {return 1;}
	write_exp_elt_opcode (BINOP_M3_ADD);
	break;
      case TK_MINUS:
	get_token ();
	if (m3_parse_e5 ()) {return 1;}
	write_exp_elt_opcode (BINOP_M3_MINUS);
	break; 
      case TK_AMPERSAND:
	get_token ();
	if (m3_parse_e5 ()) {return 1;}
	write_exp_elt_opcode (BINOP_M3_CAT);
	break; 
      default:
	return 0;
    }
  }
} /* m3_parse_e4 */

static int 
m3_parse_e3 ( )
{
  enum exp_opcode op;

  if (m3_parse_e4 ()) { return 1; }
  while (1) {
    switch (cur_tok.kind) {
      case TK_EQUAL:   op = BINOP_M3_EQUAL; goto other_arg;
      case TK_SHARP:   op = BINOP_M3_NE;    goto other_arg;
      case TK_LESS:    op = BINOP_M3_LT;    goto other_arg;
      case TK_LSEQUAL: op = BINOP_M3_LE;    goto other_arg;
      case TK_GREATER: op = BINOP_M3_GT;    goto other_arg;
      case TK_GREQUAL: op = BINOP_M3_GE;    goto other_arg;
      case TK_IN:      op = BINOP_M3_IN;    goto other_arg;

      other_arg:
	get_token ();
	if (m3_parse_e4 ()) { return (1); }
	write_exp_elt_opcode (op);
	break; 

      default:
	return 0;
    }
  }
} /* m3_parse_e3 */

static int 
m3_parse_e2 ( )
{
  int n = 0;

  while (cur_tok.kind == TK_NOT) { n++; get_token (); }

  if (m3_parse_e3 ()) { return 1; }
  if (n % 2 == 1) { write_exp_elt_opcode (UNOP_M3_NOT); }
  return 0;
} /* m3_parse_e2 */

static int 
m3_parse_e1 ( )
{
  if (m3_parse_e2 ()) { return 1; }
  while (cur_tok.kind == TK_AND) {
    get_token ();
    if (m3_parse_e2 ()) { return 1; }
    write_exp_elt_opcode (BINOP_M3_AND);
  }
  return 0; 
} /* m3_parse_e1 */

static int 
m3_parse_e0 ( )
{
  if (m3_parse_e1 ()) { return 1; }
  while (cur_tok.kind == TK_OR) {
    get_token ();
    if (m3_parse_e1 ()) { return 1; }
    write_exp_elt_opcode (BINOP_M3_OR);
  }
  return 0; 
} /* m3_parse_e0 */

static int 
m3_parse_expr ( )
{
  int lhs = 0, rhs = 0;
  lhs = m3_parse_e0 ();
  if (cur_tok.kind == TK_ASSIGN) {
    get_token ();
    rhs = m3_parse_e0 ();
    write_exp_elt_opcode (BINOP_ASSIGN);
  }
  write_exp_elt_opcode (M3_FINAL_TYPE);
  return ((lhs + rhs) != 0);
} /* m3_parse_expr */

int 
m3_parse ( )
{
  get_token ();
  if (m3_parse_expr ()) { return 1; }
  if (cur_tok.kind != TK_EOF) {
    error ("unexpected junk after Modula-3 expression: \"%s\"", lexptr);
    return 1;
  }
  return 0;
} /* m3_parse */

void
m3_print_subexp (
    struct expression *exp, 
    int *pos,
    struct ui_file *stream, 
    enum precedence prec
  )

{
  unsigned length;
  int pc;
  enum exp_opcode opcode;
  char * sym_name; 

  pc = (*pos)++;
  opcode = exp->elts[pc].opcode;
  switch (opcode)
    {
      /* Common ops, specialized Modula-3 versions: */

    case OP_VAR_VALUE:
      { struct block *b;
	(*pos) += 3;
#if 0
        /* This code was present when the OP_VAR_VALUE case in
           print_subexp_standard was specialized for Modula-3.
           It is unneeded for now, but I wonder if it might be
           wanted someday to print qualified names? */  
        b = exp->elts[pc + 1].block;
	if (b != NULL
	    && BLOCK_FUNCTION (b) != NULL
	    && SYMBOL_PRINT_NAME (BLOCK_FUNCTION (b)) != NULL) {

            if (exp->language_defn->la_language != language_m3) {
	      fputs_filtered (SYMBOL_PRINT_NAME (BLOCK_FUNCTION (b)), stream);
	      fputs_filtered ("::", stream);
            }
	}
#endif
        sym_name = SYMBOL_PRINT_NAME (exp->elts[pc + 2].symbol);
        if ((sym_name[0] == 'I' || sym_name[0] == 'M') &&
             sym_name[1] == '$')
	  fputs_filtered (sym_name+2, stream);
        else
	  fputs_filtered (sym_name, stream);
	}
      return;

    /* Modula-3-specific ops: */

    case OP_M3_LONG:
    case OP_M3_CHAR:
    case OP_M3_WIDECHAR:
      (*pos) += 3;
      value_print ( m3_value_from_longest ( exp->elts[pc + 1].type,
			      	            exp->elts[pc + 2].longconst),
		   stream, 0, Val_no_prettyprint);
      return;

    case OP_M3_REEL:
    case OP_M3_LREEL:
    case OP_M3_XREEL:
      (*pos) += 3;
      value_print (value_from_double (exp->elts[pc + 1].type,
				      exp->elts[pc + 2].doubleconst),
		   stream, 0, Val_no_prettyprint);
      return;

    case OP_M3_WIDETEXT: 
      /* like OP_STRING */
      length = longest_to_int (exp -> elts[pc + 1].longconst);
      (*pos) += 3 + 2 * BYTES_TO_EXP_ELEM (length + 1);
      LA_PRINT_STRING (stream, &exp->elts[pc + 2].string, length, 2, 0);
      return;

    case OP_M3_TEXT: 
      /* like OP_STRING */
      length = longest_to_int (exp -> elts[pc + 1].longconst);
      (*pos) += 3 + BYTES_TO_EXP_ELEM (length + 1);
      LA_PRINT_STRING (stream, &exp->elts[pc + 2].string, length, 1, 0);
      return;

    case M3_FINAL_TYPE:
      print_subexp (exp, pos, stream, PREC_PREFIX);
      return;

    case UNOP_M3_DEREF:
      print_subexp (exp, pos, stream, PREC_SUFFIX);
      fprintf_unfiltered(stream,"^");
      return;
      
    case STRUCTOP_M3_MODULE:
    case STRUCTOP_M3_INTERFACE:
    case STRUCTOP_M3_STRUCT: {
      char *field_name;

      field_name = &exp->elts[pc + 2].string;
      length = longest_to_int (exp->elts[pc + 1].longconst);
      (*pos) += 3 + BYTES_TO_EXP_ELEM (length + 1);
      print_subexp (exp, pos, stream, PREC_PREFIX);
      fprintf_unfiltered(stream,".");
      fputs_filtered (field_name, stream);
      return;
    } 

    case STRUCTOP_M3_ENUM: {
      char *field_name;
      struct symbol * sym; 

      sym = exp->elts[pc + 1].symbol; 
      field_name = &exp->elts[pc + 4].string;
      length = longest_to_int (exp->elts[pc + 3].longconst);
      (*pos) += 5 + BYTES_TO_EXP_ELEM (length + 1);
      fputs_filtered ( SYMBOL_NATURAL_NAME ( sym ), stream );
      fprintf_unfiltered(stream,".");
      fputs_filtered (field_name, stream);
      return;
    }

    case BINOP_M3_SUBSCRIPT:
      /* like BINOP_SUBSCRIPT */
      print_subexp (exp, pos, stream, PREC_SUFFIX);
      fprintf_unfiltered(stream,"[");
      print_subexp (exp, pos, stream, PREC_ABOVE_COMMA);
      fprintf_unfiltered(stream,"]");
      return;

    /* All of the UNOP_M3_*s, BINOP_M3_*s, and BINOP_ASSIGN are passed on to
       print_subexp_standard, which can handle them, because it uses the 
       Modula-3 version of la_op_print_tab. */
    default:
      (*pos)--; /* I hate this kludge, but it sure saves code. */
      print_subexp_standard (exp, pos, stream, prec ) ; 
  };
} /* m3_print_subexp */

int
m3_dump_subexp ( struct expression *exp, struct ui_file *stream, int elt )

{ int pc = elt;
  enum exp_opcode opcode = exp->elts[elt++].opcode;
  int length;
  char * field_name;
  long longval;   
  struct symbol * sym; 
  
  switch ( opcode ) 
  { case OP_M3_LONG:
    case OP_M3_CHAR:
    case OP_M3_WIDECHAR: 
      fprintf_filtered (stream, "Type @");
      gdb_print_host_address (exp->elts[elt].type, stream);
      fprintf_filtered (stream, " (");
      type_print (exp->elts[elt].type, NULL, stream, 0);
      longval = (long) exp->elts[elt + 1].longconst; 
      fprintf_filtered (stream, "), value %ld (0x%lx)",
			longval, longval);
      elt += 3;
      break;

    case OP_M3_REEL:
    case OP_M3_LREEL:
    case OP_M3_XREEL:
      fprintf_filtered (stream, "Type @");
      gdb_print_host_address (exp->elts[elt].type, stream);
      fprintf_filtered (stream, " (");
      type_print (exp->elts[elt].type, NULL, stream, 0);
      fprintf_filtered (stream, "), value %g",
			(double) exp->elts[elt + 1].doubleconst);
      elt += 3;
      break;

    case OP_M3_TEXT: 
      /* like OP_STRING */
      length = longest_to_int (exp -> elts[pc + 1].longconst);
      elt += 3 + BYTES_TO_EXP_ELEM (length + 1);
      LA_PRINT_STRING (stream, &(exp->elts[pc + 2].string), length, 1, 0);
      break;

    case OP_M3_WIDETEXT: 
      length = longest_to_int (exp -> elts[pc + 1].longconst);
      elt += 3 + 2 * BYTES_TO_EXP_ELEM (length + 1);
      LA_PRINT_STRING (stream, &(exp->elts[pc + 2].string), length, 2, 0);
      break;

    case OP_M3_TYPE:
      fprintf_filtered (stream, "Type @");
      gdb_print_host_address (exp->elts[elt].type, stream);
      fprintf_filtered (stream, " (");
      type_print (exp->elts[elt].type, NULL, stream, 0);
      fprintf_filtered (stream, ") ");
      elt += 2;
      break; 

    case STRUCTOP_M3_MODULE:
    case STRUCTOP_M3_INTERFACE:
    case STRUCTOP_M3_STRUCT: 
      length = longest_to_int (exp->elts[pc + 1].longconst);
      elt += 3 + BYTES_TO_EXP_ELEM (length + 1);
      field_name = &exp->elts[pc + 2].string;
      fprintf_filtered (stream, "Element name: `%.*s'", length, field_name);
      elt = dump_subexp (exp, stream, elt);
      break;

    case STRUCTOP_M3_ENUM: 
      sym = exp->elts[pc + 1].symbol;
      length = longest_to_int (exp->elts[pc + 3].longconst);
      elt += 5 + BYTES_TO_EXP_ELEM (length + 1);
      field_name = &exp->elts[pc + 4].string;
      fprintf_filtered 
        ( stream, "Enum const: '%s'.`%.*s'", 
          SYMBOL_NATURAL_NAME ( sym ) + 2 /* Strip off leading "B$" */ , 
          length, field_name
        );
      elt = dump_subexp (exp, stream, elt);
      break;

    case M3_FINAL_TYPE:
    case UNOP_M3_ABS:
    case UNOP_M3_ADR:
    case UNOP_M3_ADRSIZE:
    case UNOP_M3_BITSIZE:
    case UNOP_M3_BYTESIZE:
    case UNOP_M3_CEILING:
    case UNOP_M3_DEREF:
    case UNOP_M3_FIRST:
    case UNOP_M3_FLOOR:
    case UNOP_M3_LAST:
    case UNOP_M3_NEG:
    case UNOP_M3_NOT:
    case UNOP_M3_NUMBER:
    case UNOP_M3_ORD:
    case UNOP_M3_ROUND:
    case UNOP_M3_TRUNC:
      elt = dump_subexp ( exp, stream, elt); 
      break; 

    case BINOP_M3_SUBSCRIPT:
    case BINOP_M3_MULT:
    case BINOP_M3_DIVIDE:
    case BINOP_M3_DIV:
    case BINOP_M3_MOD:
    case BINOP_M3_ADD:
    case BINOP_M3_MINUS:
    case BINOP_M3_CAT:
    case BINOP_M3_EQUAL:
    case BINOP_M3_NE:
    case BINOP_M3_LT:
    case BINOP_M3_LE:
    case BINOP_M3_GT:
    case BINOP_M3_GE:
    case BINOP_M3_IN:
    case BINOP_M3_AND:
    case BINOP_M3_OR:
    case BINOP_M3_MAX:
    case BINOP_M3_MIN:
    case BINOP_M3_VAL:
    case BINOP_M3_FLOAT:
    case BINOP_M3_LOOPHOLE:
      elt = dump_subexp ( exp, stream, elt); 
      elt = dump_subexp ( exp, stream, elt); 
      break; 

    default:
      elt--; /* I hate this kludge, but it sure saves code. */
      return dump_subexp_body_standard (exp, stream, elt);
  } 
  return elt; 
} /* m3_dump_subexp */ 

/* End of file m3-exp.c */ 
