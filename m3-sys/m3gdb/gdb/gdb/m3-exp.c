#include "defs.h"
#include "expression.h"
#include "value.h"
#include "parser-defs.h"
#include "language.h"
#include "m3-lang.h"
#include "m3-token.h"
#include "gdbtypes.h"

extern char* lexptr;
static m3_token cur_tok;

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
}

static void write_exp_text (opcode, str, len)
     enum exp_opcode opcode;
     char *str;
     int len;
{
  struct stoken t;
  
  write_exp_elt_opcode (opcode); 
  t.ptr = str;
  t.length = len;
  write_exp_string (t);
  write_exp_elt_opcode (opcode); 
}

static void write_exp_var (sym)
     struct symbol *sym;
{
  write_exp_elt_opcode (OP_VAR_VALUE);
  write_exp_elt_block (block_found);
  write_exp_elt_sym (sym);
  write_exp_elt_opcode (OP_VAR_VALUE);
}

static int
m3_find_global (unit, entry)
    char *unit, *entry;
{
    struct symbol *ir;
    struct type *exports;
    int i;

    /* try the implementation module first */
    if (ir = find_m3_ir ('M', unit)) {
      if (find_m3_rec_field (SYMBOL_TYPE (ir), entry, 0, 0, 0)) {
	write_exp_var (ir);
	write_exp_text (STRUCTOP_M3_MODULE, entry, strlen (entry));
	return 1;
      }
  
      /* Could it be a global name in one of the interfaces
	 explicityly exported by the current unit ? */
      if (exports = find_m3_exported_interfaces (unit)) {
	for (i = 0; i < TYPE_NFIELDS (exports); i++) {
	  if (ir = find_m3_ir ('I', TYPE_FIELD_NAME (exports, i))) {
	    if (find_m3_rec_field (SYMBOL_TYPE (ir), entry, 0, 0, 0)) {
	      write_exp_var (ir);
	      write_exp_text (STRUCTOP_M3_INTERFACE, entry, strlen (entry));
	      return 1;
	    }
	  }
	}
      }
    }

    /* try the interface */
    if (ir = find_m3_ir ('I', unit)) {
      if (find_m3_rec_field (SYMBOL_TYPE (ir), entry, 0, 0, 0)) {
	write_exp_var (ir);
	write_exp_text (STRUCTOP_M3_INTERFACE, entry, strlen (entry));
	return 1;
      }
    }

    /* last chance: procedures that use direct calls ("-all_direct")
       don't have entries in the interface record, but they do
       have entries in the minimal symbol table of the form
       "unit__entry" */
    {
      struct symbol *sym;
      char tmp[500];

      strcpy (tmp, unit);
      strcat (tmp, "__");
      strcat (tmp, entry);
      if ((sym = lookup_symbol (tmp, expression_context_block,
			        VAR_NAMESPACE, 0, NULL)) != 0
 	  && sym->aclass != LOC_STATIC) {
        write_exp_var (sym);
        return 1;
      }
    }
    
    /* nope, give up... */
    return 0;
}

static int m3_parse_expr ();


static void write_m3_const (val, tipe)
  LONGEST val;
  struct type *tipe;
{
  write_exp_elt_opcode (OP_M3_LONG);
  write_exp_elt_type (tipe);
  write_exp_elt_longcst (val);
  write_exp_elt_opcode (OP_M3_LONG); 
  get_token ();
}

static void write_m3_type (tipe)
  struct type *tipe;
{
  write_exp_elt_opcode (OP_M3_TYPE);
  write_exp_elt_type (tipe);
  write_exp_elt_opcode (OP_M3_TYPE); 
  get_token ();
}

static void m3_builtin_1 (op)
  enum exp_opcode op;
{
  get_token (); /* builtin function name */
  if (cur_tok.kind != TK_LPAREN) { error ("missing opening ("); }
  get_token ();
  m3_parse_expr ();
  if (cur_tok.kind != TK_RPAREN) { error ("missing closing )"); }
  get_token ();
  write_exp_elt_opcode (op); 
} /* m3_builtin_1 */

static void m3_builtin_2 (op)
  enum exp_opcode op;
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
} /* m3_builtin_2 */

static void m3_float_op (op)
  enum exp_opcode op;
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


static void m3_not_yet (nm)
  char *nm;
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
}

static int m3_parse_e8 ()
{
  switch (cur_tok.kind) {

    case TK_EOF:
      error ("unexpected EOF in expression");
      return 1;

    case TK_IDENT: {
      struct symbol *sym;
      struct type *interfaces;
      struct block *b;
      char *unit_name = 0;
      int i;

      /* Is it a local symbol ? */
      if ((sym = lookup_symbol (cur_tok.string, expression_context_block,
				VAR_NAMESPACE, 0, NULL)) != 0
	  && sym->aclass != LOC_STATIC) {
	write_exp_var (sym);
	goto ident_ok;
      }

      /* Could it be a global name in the current unit ?
	 these are accessible only through the interface record,
	 which happens to be the only symbol in the topmost block. */
      b = expression_context_block;
      while (b && BLOCK_SUPERBLOCK (b)) { b = BLOCK_SUPERBLOCK (b); }
      if (b) {
	sym = BLOCK_SYM (b, 0);
	unit_name = SYMBOL_NAME (sym);
	if (   (unit_name[0] == 'M')
	    && ((unit_name[1] == 'I') || (unit_name[1] == 'M'))
	    && (unit_name[2] == '_')
	    && m3_find_global (unit_name + 3, cur_tok.string)) {
	  goto ident_ok;
	}
      }
	
      /* Is this the beginning of a qualified global name? */
      unit_name = cur_tok.string;
      if (find_m3_ir ('I', unit_name) || find_m3_ir ('M', unit_name)) {
	get_token ();
	if (cur_tok.kind != TK_DOT) {
	  error ("malformed expression");
	  return 1;
        }
	get_token ();
	if (cur_tok.kind != TK_IDENT) {
	  error ("malformed expression");
	  return 1;
        }
	if (m3_find_global (unit_name, cur_tok.string)) {
	  goto ident_ok;
	}
      } else {
	unit_name = 0;
      }

      /* out of ideas */
      if (unit_name) {
        error ("can't find identifier: %s.%s", unit_name, cur_tok.string);
      } else {
        error ("can't find identifier: %s", cur_tok.string);
      }

    ident_ok:
      get_token ();
      break;
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

    case TK_ABS:       m3_builtin_1 (UNOP_M3_ABS);       break;
    case TK_ADR:       m3_builtin_1 (UNOP_M3_ADR);       break;
    case TK_ADRSIZE:   m3_builtin_1 (UNOP_M3_ADRSIZE);   break;
    case TK_BITSIZE:   m3_builtin_1 (UNOP_M3_BITSIZE);   break;
    case TK_BYTESIZE:  m3_builtin_1 (UNOP_M3_BYTESIZE);  break;
    case TK_CEILING:   m3_builtin_1 (UNOP_M3_CEILING);   break;
    case TK_DEC:       m3_not_yet ("DEC");               break;
    case TK_DISPOSE:   m3_not_yet ("DISPOSE");           break;
    case TK_FIRST:     m3_builtin_1 (UNOP_M3_FIRST);     break;
    case TK_FLOAT:     m3_float_op (BINOP_M3_FLOAT);     break;
    case TK_FLOOR:     m3_builtin_1 (UNOP_M3_FLOOR);     break;
    case TK_INC:       m3_not_yet ("INC");               break;
    case TK_ISTYPE:    m3_not_yet ("ISTYPE");            break;
    case TK_LAST:      m3_builtin_1 (UNOP_M3_LAST);      break;
    case TK_LOOPHOLE:  m3_builtin_2 (BINOP_M3_LOOPHOLE); break;
    case TK_MAX:       m3_builtin_2 (BINOP_M3_MAX);      break;
    case TK_MIN:       m3_builtin_2 (BINOP_M3_MIN);      break;
    case TK_NARROW:    m3_not_yet ("NARROW");            break;
    case TK_NEW:       m3_not_yet ("NEW");               break;
    case TK_NUMBER:    m3_builtin_1 (UNOP_M3_NUMBER);    break;
    case TK_ORD:       m3_builtin_1 (UNOP_M3_ORD);       break;
    case TK_ROUND:     m3_builtin_1 (UNOP_M3_ROUND);     break;
    case TK_SUBARRAY:  m3_not_yet ("SUBARRAY");          break;
    case TK_TRUNC:     m3_builtin_1 (UNOP_M3_TRUNC);     break;
    case TK_TYPECODE:  m3_not_yet ("TYPECODE");          break;
    case TK_VAL:       m3_builtin_2 (BINOP_M3_VAL);      break;

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

    case TK_UNTRACED:
      get_token ();
      if (cur_tok.kind != TK_ROOT) {
	error ("UNTRACED not followed by ROOT");
      };
      write_m3_type (builtin_type_m3_untraced_root);
      break;

    /*---- builtin constants ---*/

    case TK_TRUE:      write_m3_const (1, builtin_type_m3_boolean); break;
    case TK_FALSE:     write_m3_const (0, builtin_type_m3_boolean); break;
    case TK_NIL:       write_m3_const (0, builtin_type_m3_null);    break;

    default: 
      error ("unexpected token in  expression \"%s\" (kind = %d)",
	      m3_token_name (&cur_tok), (int)cur_tok.kind );
      return 1;

  } /* switch */

  return 0;
} /* m3_parse_e8 */  

static int m3_parse_e7 ()
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
	/* we cannot ascertain what the type what we have parsed
	   so far is; it may be an object and we need the dynamic type.
	   So we are just going to accept anything that looks ok. */

	if (cur_tok.kind != TK_IDENT) {
	  error ("Field name must be an identifier"); 
	  return 1; }

	write_exp_text (STRUCTOP_M3_STRUCT, cur_tok.string, cur_tok.length);
	get_token ();
	break; }

      case TK_LPAREN: {
	extern int arglist_len;
	arglist_len = 0;
	cur_tok.kind = TK_COMMA;
	start_arglist ();
        while (cur_tok.kind == TK_COMMA) {
	  get_token ();
	  m3_parse_expr ();
	  arglist_len++; }
	if (cur_tok.kind != TK_RPAREN) { error ("missing ')'"); }
	get_token ();
        write_exp_elt_opcode (OP_FUNCALL);
	write_exp_elt_longcst ((LONGEST) end_arglist ());
	write_exp_elt_opcode (OP_FUNCALL);
	break;
      }
	
      case TK_LBRACKET: {
	struct type *array_type;
	cur_tok.kind = TK_COMMA;
	while (cur_tok.kind == TK_COMMA) {
	  get_token ();
	  m3_parse_expr ();
	  write_exp_elt_opcode (BINOP_M3_SUBSCRIPT);
	}
	
	if (cur_tok.kind != TK_RBRACKET) { error ("missing ']'");  }
	get_token ();
	break;
      }

      case TK_EOF:
      default:
	return 0;
    } /* switch */
  } /* while(1) */
} /* m3_parse_e7 */


static int m3_parse_e6 ()
{
  int m = 0;

  while (cur_tok.kind == TK_PLUS || cur_tok.kind == TK_MINUS) {
    if (cur_tok.kind == TK_MINUS) { m++; }
    get_token ();
  }

  if (m3_parse_e7 ()) { return 1; }
  if (m % 2 == 1) { write_exp_elt_opcode (UNOP_M3_NEG); }
  return 0;
}

static int m3_parse_e5 ()
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
}

static int m3_parse_e4 ()
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
}

static int m3_parse_e3 ()
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
}

static int m3_parse_e2 ()
{
  int n = 0;

  while (cur_tok.kind == TK_NOT) { n++; get_token (); }

  if (m3_parse_e3 ()) { return 1; }
  if (n % 2 == 1) { write_exp_elt_opcode (UNOP_M3_NOT); }
  return 0;
}

static int m3_parse_e1 ()
{
  if (m3_parse_e2 ()) { return 1; }
  while (cur_tok.kind == TK_AND) {
    get_token ();
    if (m3_parse_e2 ()) { return 1; }
    write_exp_elt_opcode (BINOP_M3_AND);
  }
  return 0; 
}

static int m3_parse_e0 ()
{
  if (m3_parse_e1 ()) { return 1; }
  while (cur_tok.kind == TK_OR) {
    get_token ();
    if (m3_parse_e1 ()) { return 1; }
    write_exp_elt_opcode (BINOP_M3_OR);
  }
  return 0; 
}

static int m3_parse_expr ()
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
}

int m3_parse ()
{
  get_token ();
  if (m3_parse_expr ()) { return 1; }
  if (cur_tok.kind != TK_EOF) {
    error ("unexpected junk after Modula-3 expression: \"%s\"", lexptr);
    return 1;
  }
  return 0;
}
