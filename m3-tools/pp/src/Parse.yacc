/* Copyright (C) 1989, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

/* Last modified on Wed Mar 13 12:28:14 PST 1996 by heydon                   */
/*      modified on Fri Jul  7 09:28:34 PDT 1995 by kalsow                   */
/*      modified on Mon Jun 22 09:00:23 PDT 1992 by schilit@xerox.com        */
/*      modified on Wed Apr 29 17:11:39 PDT 1992 by muller                   */
/*      modified on Mon Apr 20 16:02:50 1992 by nichols@xerox.com            */
/*      modified on Fri Feb 28 13:46:45 PST 1992 by meehan                   */
/*      modified on Mon Jun  1 11:37:54 1987 by firefly                      */
/*      modified on Wed Jan  8 16:38:12 1986 by hania                        */

/* A yacc source file for the Modula-3 pretty-printer. This grammar
   was constructed from the grammar given in the Modula-3 report;
   the main problem was to get it right for yacc (an expression can
   start by a type). */

/* Expect 2 shift/reduce conflicts */

/* The effect of yyparse is to parse a fragment of the Modula 3
   language and emit a stream of characters and formatting codes
   to a separate process that performs the formatting. This process
   is actually Formatter. 

   Several of the non-terminals in this grammar derive the empty
   string and are just used to cause semantic routines to be called.
   ("Q" non-terminals name routines that depend on the style-options.)
   These non-terminals are:

        G  begins a group.
	B  begins an indented object.
	B0 begins a non-indented object.
	E  ends a group or object.
	EF ends a group or object and forces comments to be emitted.
	A  inserts a space followed by an optimal, ununited breakpoint.
	AO inserts a space followed by a nobreak-optimal, ununited breakpoint.
	AX inserts a nobreak-optimal, ununited breakpoint.
	V  inserts a space followed by a united breakpoint.
	VZ inserts a space followed by an outdented, united breakpoint.
	VC inserts a space followed by an outdented, then indented by 2
		spaces, united breakpoint.
        Z  inserts a united breakpoint if it follows a blank line.
	SP inserts a space.
	XSP inserts a space without forcing comments to appear.
	BL inserts a forced breakpoint.
	AL2 begins a 2-column aligned object.
	AL3 begins a 3-column aligned object.
	ALZ5 begins a 5-column aligned object that may fit on a single line.
	ALNL marks where the column aligner will insert a newline.
	SPNL inserts a space or united breakpoint depending on the style.
	QSP may insert a space.
	NL inserts a forced breakpoint.
	Inc increases depth, which is used to distinguish outer level
	   comments, which are formatted differently from inner comments.
	Dec decreases depth.

   These semantic routines use the C procedures:

        BE(n) inserts a Formatter.Begin with offset n.
	EN()  inserts a Formatter.End ().
	GR()  inserts a Formatter.Group ().
	BL()  inserts a forced breakpoint with offset 0.
        P(c) inserts the character c, except that if c is
          newline, it is treated as a forced breakpoint with
          a large negative offset.
        Pr(s) applies P to each character of the string s.
        Flush() flushes the buffers of the formatted stream.
        Reset() does nothing.
	NL()  inserts a new line.
	DoSPNL() inserts a space or forced breakpoint depending on the style.
	DoQSP() may insert a space.
	DoAlign() starts an aligned object.
	DoBreak() inserts a breakpoint.

   */

/* The lexical analyzer communicates with the parser through
   a buffer capable of holding two lexemes; since YACC uses
   only one-token lookahead, and since lexemes are reduced
   immediately to non-terminals, and since the character string
   for a lexeme is used by the pretty-printer only when the
   lexeme is first reduced to a non-terminal, it follows that
   a two-lexeme buffer is sufficient.  (This buffer is only
   used for lexemes, like Identifiers and integers, that are
   not completely characterized by their token number.)
   The buffer is called  lexbuf ; it contains two null-terminated
   character strings, one beginning at position 0, the other at
   position 500.  The variable lexptr, used by the lexical
   analyzer, Identifer which of these two positions is current.   */

%{

#define lexbufsize 500
char lexbuf[2 * lexbufsize];
int lexptr = 0;
int lexposition = 0;
  /* See BufferLexeme and AddLexLength in Parse.lex */

int comdepth = 0;
  /* depth of comments, used only by lexer. */

int pragdepth = 0;
  /* depth of pragmas, used only by lexer. */

int depth = 0;
  /* depth of nesting in blocks, used by NPS for formatting comments */

int blanklinep;  
  /* Set by NPS if the non-program-sequence that it parses ends
     with a blank line. */

int calledFromEmacs;
  /* set to one by main if called from Emacs, to zero otherwise */

int capSwitch;
  /* 1 if -cap switch was set, 0 otherwise. */

int callspace;
  /* 1 if -callspace switch was set, 0 otherwise */

char *formatter;
  /* the opaque Formatter.T object */

double offset = 2.0;		
  /* indentation */

int alignDecls = 1;
  /* True if we should use alignment code for declarations. */

int breakType;
  /* Style of optimal breaks to use. */

double commentCol;
  /* Where comments go. */

int comBreakNLs;
  /* how many NLs before HandleComments bails out the first time */

typedef struct {
  char *body;
  char *keyword;
  char *builtinID;
  char *procName;
  char *comment;
  char *fixedComment;
  char *fixed;
} FontInfo;

FontInfo *fonts; /* various opaque fonts */

double fixedCommentSpaceWidth;
#define MAXWIDTH	(1.0E20)
/* Width of a space in various fonts. */
double bodySpaceWidth;
double commentLeaderWidth;

typedef long STYLE;
#define SRC_STYLE 0
#define EM_STYLE  1
STYLE style = SRC_STYLE;

typedef enum {NonOptimal, OptimalBreak, OptimalNoBreak} Formatter_BreakType;

%}

/* basic tokens */
%token ENDOFFILE 0

%token AMPERSAND ASSIGN ASTERISK BAR COLON COMMA DOT DOTDOT
%token EQUAL GREATER GREQUAL LESS LSEQUAL MINUS SHARP PERIOD PLUS
%token RARROW RBRACE RBRACKET RPAREN SEMICOLON SLASH
%token SUBTYPE UPARROW 
%token LPAREN LBRACKET LBRACE

%token IDENT CARD_CONST REAL_CONST CHAR_CONST STR_CONST

/* Various kinds of pragmas */
%token PR_EXTERNAL PR_INLINE PR_OBSOLETE PR_UNUSED

/* reserved words */ 
%token AND ANY ARRAY AS BGN BITS BRANDED BY CASE CONST 
%token DIV DO ELSE ELSIF END EVAL EXCEPT EXCEPTION EXIT EXPORTS
%token FINALLY FOR FROM GENERIC IF IMPORT IN INTERFACE LOCK LOOP
%token METHODS MOD MODULE NOT OBJECT OF OR OVERRIDES PROCEDURE RAISE RAISES
%token READONLY RECORD REF REPEAT RETURN REVEAL ROOT SET THEN TO
%token TRY TYPE TYPECASE UNSAFE UNTIL UNTRACED VALUE VAR WHILE WITH

/* token to force error from scanner */
%token BAD

/* Tokens needed only by the pretty-printer */
%token WHITESPACE

%token MODUNIT DEFUNIT

/* MODUNIT and DEFUNIT are needed only */
/* when the pretty-printer is called from Emacs. */

%start FormattingUnit

%%

/*--------------------- modules ------------------------*/


FormattingUnit:
      { depth=0; }         InitialBlankLines CompilationUnit NL
    | { depth=0; } MODUNIT InitialBlankLines ModUnit_list    { Flush(); }
    | { depth=0; } DEFUNIT InitialBlankLines DefUnit_list    { Flush(); }
    ;

InitialBlankLines:
      /* empty */
    | InitialNPS NL
    ;

ModUnit_list:
      ModUnit
    | ModUnit_list ModUnit
    ;

ModUnit:
      declaration_nl
    | import_nl
    | CompilationUnit NL
    | B Inc Begin stmts VZ Dec End SP Ident E NL
    ;

DefUnit_list:
      DefUnit
    | DefUnit_list DefUnit
    ;

DefUnit:
      declaration_nl
    | import_nl
    | CompilationUnit NL
    ;

CompilationUnit:
      interface
    | Unsafe SP interface
    | module
    | Unsafe SP module
    | generic_interface
    | generic_module
    ;

interface:
      Interface SP B IdentP Semi E NL
        import_nl_list
        declaration_nl_list
        End SP Ident Dot
    | Interface SP B IdentP SP Equal AO B Ident generic_params E E NL
        End SP Ident Dot
    ;

module:
      Module SP B IdentP exports Semi E NL
        import_nl_list
        named_block Z Dot
    | Module SP B IdentP exports SP Equal AO B Ident generic_params E E NL
        End SP Ident Dot
    ;

generic_interface:
      Generic SP Interface SP IdentP generic_params Semi NL
        import_nl_list
        declaration_nl_list
        End SP Ident Dot
    ;

generic_module:
      Generic SP Module SP IdentP generic_params Semi NL
        import_nl_list
        named_block Z Dot
    ;

/* Good enough for both formals and actuals. */
generic_params:
      /* empty */
    | Z QSP Lparen AX B0 opt_id_list E Z Rparen
    ;

exports:
      /* empty */
    | AO Exports SP B0 id_list E
    ;

import_nl_list:
      /* empty */
    | import_nl_list import_nl
    ;

import_nl:
      From SP Ident SP Import SP B0 id_list            E Semi NL
    |                  Import SP B0 import_module_list E Semi NL
    ;

import_module_list:
      B Ident                E
    | B Ident SP As SP Ident E
    | import_module_list Comma A B Ident          E
    | import_module_list Comma A B Ident As Ident E
    ;

block:
      declaration_nl_list B Inc Begin stmts NL Dec End Z E
    ;

named_block:
      declaration_nl_list B Inc Begin stmts NL Dec End SP Ident E
    ;

declaration_nl_list:
      /* empty */
    | declaration_nl_list declaration_nl 
    ;

declaration_nl:
      B decl_pragma procedure_head SP Equal BL B0 named_block Z Semi E E BL
    | B decl_pragma procedure_head Semi E BL
    | B decl_pragma Const                                  E BL
    | B decl_pragma Const     V AL3 const_decl_list     EA E BL
    | B decl_pragma Type                                   E BL
    | B decl_pragma Type      V AL2 type_decl_list      EA E BL
    | B decl_pragma Reveal                                 E BL
    | B decl_pragma Reveal    V AL2 type_decl_list      EA E BL
    | B decl_pragma Var                                    E BL
    | B decl_pragma Var       V AL3 var_decl_list       EA E BL
    | B decl_pragma Exception                              E BL
    | B decl_pragma Exception V     exception_decl_list    E BL
    ;

decl_pragma:
      /* empty */
    | decl_pragma Pr_External A
    | decl_pragma Pr_Inline   A
    | decl_pragma Pr_Obsolete A
    | decl_pragma Pr_Unused   A
    ;

const_decl_list:
                      const_decl ALNL
    | const_decl_list const_decl ALNL
    ;

const_decl:
      /* Should move SP outside E? */
      Inc G  G Ident EF G Colon SP type SP EF G Equal SP expr Z Dec Semi E E
    | Inc G  G Ident EF G               SP EF G Equal SP expr Z Dec Semi E E
    ;

type_decl_list:
                     type_decl ALNL
    | type_decl_list type_decl ALNL
    ;

type_decl:
      G  B type_name SP Equal   A type Semi E E
    | G  B type_name SP Subtype A type Semi E E
    ;

var_decl_list:
                    var_decl ALNL
    | var_decl_list var_decl ALNL
    ;

var_decl:
      Inc G  G id_list                    EF
             G Colon SP type Z Dec Semi1  E
             G NPS                        E  E
    | Inc G  G id_list                    EF
             G Colon SP type SP           EF
             G Assign SP expr Z Dec Semi  E  E
    | Inc G  G id_list                    EF
             G SP                         EF
             G Assign SP expr Z Dec Semi  E  E
    ;

exception_decl_list:
	/* did have V at end of both productions. */
                             exception_decl
    | exception_decl_list BL exception_decl
    ;

exception_decl:
      /* Moved break inside LParen. -DN */
      Inc id_list                         Z Dec Semi
    | Inc id_list Lparen AX type Z Rparen Z Dec Semi
    ;

procedure_head:
      Procedure SP IdentP SP signature
    ;

signature:
      Lparen2 formals Rparen return_type raises
    ;

return_type:
      /* empty */
    | Colon A type
    ;

raises:
      /* empty */
    | Raises SP Lbrace B0 opt_qid_list E Rbrace
    | Raises SP Any
    ;

formals:
      /* empty */
    | B ALZ5 formal                  EA E
    | B ALZ5 formal_semi_list        EA E
    | B ALZ5 formal_semi_list formal EA E
    ;

formal_semi_list:
      formal_semi ALNL
    | formal_semi_list formal_semi ALNL
    ;

formal_semi:
      G B decl_pragma EF B mode EF B id_list EF type_and_or_val_semi E
    ;

formal:
      G B decl_pragma EF B mode EF B id_list EF type_and_or_val      E
    ;

mode:
      /* empty */
    | Value     SP
    | Var       SP
    | Readonly  SP
    ;

type_and_or_val_semi:
      B    Colon  SP type   Semi1 XSP  E  G NPS                           E
    | B    Colon  SP type              EF B SP Assign SP expr Z Semi XSP  E
    | G                                E  B SP Assign SP expr Z Semi XSP  E
    ;

type_and_or_val:
      B    Colon  SP type E
    | B    Colon  SP type EF B SP Assign SP expr E
    | G                   E  B SP Assign SP expr E
    ;

/*--------------------- statements ------------------------*/

stmts:
      /* empty */
    | V stmt_list        E
    | V stmt_list Z Semi E
    ;

/* Statement list with G E around it only if non-empty. */
stmts_group:
      /* empty */
    | V G stmts1 E
    ;

/* Non-empty statement list. */
stmts1:
      stmt_list        E
    | stmt_list Z Semi E
    ;

stmt_list:
      B stmt
    | stmt_list Z Semi E V B stmt
    ;

stmt:
      assignment_stmt
    | B0 block E
    | call_stmt
    | case_stmt
    | exit_stmt
    | eval_stmt
    | for_stmt
    | if_stmt
    | lock_stmt
    | loop_stmt
    | raise_stmt
    | repeat_stmt
    | return_stmt
    | try_finally_stmt
    | try_stmt
    | typecase_stmt
    | while_stmt
    | with_stmt
    ;

assignment_stmt:
      /* Swapped B and A. -DN */
      expr SP Assign AO B expr E
    ;

call_stmt:
      expr
    ;

case_stmt:
      Case SP expr SP Of VC case case_list else SPNL End
    | Case SP expr SP Of         case_list else SPNL End
    ;

case_list:
      /*empty*/
    | case_list VZ Bar SP case
    ;

case:
      B labels_list SP Rarrow stmts_group E
    ;

labels_list:
      labels
    | labels_list Z Comma A labels
    ;

labels:
      B expr E
    | B expr SP Dotdot V expr E
    ;

exit_stmt:
      Exit
    ;

eval_stmt:
      /* Swapped A and B. -DN */
      Eval AO B expr E
    ;

for_stmt:      
      /* We need the B2/E here and not for similar statements because of
	 the top-level A's. */
      B2 For SP Ident A Assign SP expr A To SP expr by Do E
           stmts SPNL End
    ;

by:
      SP /* empty */
    | A By SP expr SP
    ;

if_stmt:
      If SP expr SP Then stmts elsif_list else SPNL End
    ;

else:
      /* empty */
    | VZ Else stmts
    ;

elsif_list:
      /* empty */
    | elsif_list elsif
    ;

elsif:
      VZ Elsif SP expr SP Then stmts
    ;

lock_stmt:
      Lock SP expr SP Do stmts SPNL End
    ;

loop_stmt: 
      Loop stmts SPNL End
    ;

raise_stmt:
      Raise AO expr 
    ;

repeat_stmt:
      Repeat stmts VZ B Until A expr E
    ;

return_stmt:
      Return
    | Return AO expr 
    ;

try_finally_stmt: 
      Try stmts VZ Finally stmts SPNL End 
    ;

try_stmt:
      Try stmts VZ Except VC handler handler_list else SPNL End
    | Try stmts VZ Except            handler_list else SPNL End
    ;

handler_list:
      /* empty */
    | handler_list VZ Bar SP handler
    ;

handler:
      B B2 qid_list A Lparen Ident Z Rparen SP Rarrow E stmts_group E
    | B B2 qid_list                         SP Rarrow E stmts_group E
    ;

typecase_stmt:
      B2 Typecase A expr A Of E VC tcase tcase_list else SPNL End
    | B2 Typecase A expr A Of E          tcase_list else SPNL End
    ;

tcase_list:
      /* empty */
    | tcase_list VZ Bar SP tcase
    ;

tcase:
      B type_list                        SP Rarrow stmts_group E
    | B type_list SP Lparen Ident Rparen SP Rarrow stmts_group E
    ;

while_stmt:
      While SP expr SP Do stmts SPNL End 
    ;

with_stmt:
      With SP AL2 binding_list E E EA SP Do stmts SPNL End 
    ;

binding_list:
      binding
    | binding_list Comma E E ALNL binding
    ;

binding:
      G G Ident SP E G Equal SP expr
    ;

opt_qid_list:
      /* empty */
    | qid_list
    ;

qid_list:
      qid
    | qid_list Comma A qid
    ;

qid:
      Ident
    | Ident Dot Ident
    ;

/*--------------------- types ------------------------*/

type_list:
      type
    | type_list Z Comma A type
    ;

type:
      B type_name                            E
    | B type_name SP simple_object_type_list E
    | B root_type SP simple_object_type_list E
    | B type_constructor                     E
    | B Lparen type Z Rparen                 E
    ;

type_name:
      qid
    ;

type_constructor:
      type_constructor1
    | type_constructor2
    ;

type_constructor1:
      Bits A expr A For V type
    | Procedure AO signature
    | Untraced SP simple_object_type_list
    |             simple_object_type_list
    | Untraced SP brand Ref A type
    |             brand Ref A type
    | B0 Lbrace B0 opt_id_list E Z Rbrace E
    | Lbracket expr SP Dotdot V expr Z Rbracket
    | root_type
    ;

root_type:
      Untraced SP Root
    | Root
    ;

/* these can appear as values in vanilla expressions */
type_constructor2:
      Array A type_list SP Of V type
    | Array             SP Of V type
    | Record fields SPNL End
    | Set SP Of V type
    ;

simple_object_type_list:
      simple_object_type
    | simple_object_type_list V simple_object_type
    ;

simple_object_type:
      brand Object fields methods_part overrides_part SPNL End 
    ;

methods_part:
      /* empty */
    | VZ Methods methods
    ;

overrides_part:
      /* empty */
    | VZ Overrides overrides
    ;

brand:
      /* empty */
    | Branded SP
    | Branded SP Str_expr SP
    ;

fields:
      /* empty */
    | V G AL3 field                 EA E
    | V G AL3 field_semi_list       EA E
    | V G AL3 field_semi_list field EA E
    ;

field_semi_list:
      field_semi ALNL
    | field_semi_list field_semi ALNL
    ;

field_semi:
      G B id_list E type_and_or_val_semi E
    ;
field:
      G B id_list E type_and_or_val      E
    ;

methods:
      /* empty */
    | V G AL3 method                  EA E
    | V G AL3 method_semi_list        EA E
    | V G AL3 method_semi_list method EA E
    ;

method_semi_list:
      method_semi ALNL
    | method_semi_list method_semi ALNL
    ;

method_semi:
      G  B Ident EF  B SP signature Semi E                              E
    | G  B Ident EF  B SP signature      EF  B SP Assign SP qid Semi E  E
    | G  B Ident EF  B SP                EF  B SP Assign SP qid Semi E  E
    ;

method:
      G  B Ident EF  B SP signature E                         E
    | G  B Ident EF  B SP signature EF  B SP Assign SP qid E  E
    | G  B Ident EF  B SP           EF  B SP Assign SP qid E  E
    ;

overrides:
      /* empty */
    | V G AL2 override                    EA E
    | V G AL2 override_semi_list          EA E
    | V G AL2 override_semi_list override EA E
    ;

override_semi_list:
      override_semi ALNL
    | override_semi_list override_semi ALNL
    ;

override_semi:
      G  B Ident EF B SP Assign SP qid Semi E  E
    ;

override:
      G  B Ident EF B SP Assign SP qid      E  E
    ;

/*--------------------- expressions ------------------------*/

expr:   B zexpr E ;
zexpr:  e1 | zexpr A Or SP e1  ;

e1:     B ze1 E ;
ze1:    e2 | ze1 A And SP e2 ;

e2:     Not SP e2 | e3 ;

e3:     B ze3 E ;
ze3:    e4 | ze3 A relop SP e4 ;
relop:  Equal | Notequal | Less | Lsequal | Greater | Grequal | In ;

e4:     B ze4 E ;
ze4:    e5 | ze4 A addop SP e5 ;
addop:  Plus | Minus | Ampersand ;

e5:     B ze5 E ;
ze5:    e6 | ze5 A mulop SP e6 ;
mulop:  Asterisk | Slash | Div | Mod ;

e6:     e7 | Plus Z e6 | Minus Z e6 ;

/* Removed a Z before selector_list. */
e7:     e8 selector_list ;

e8:     Ident | Card_const | Real_const | Char_const | Str_const
        | Lparen expr Z Rparen | type_constructor2 ;

selector_list:
      /* empty */
    | selector_list Z selector
    ;

selector:
      Dot Ident
    | Uparrow
	/* Removed SP from front of each of these. -DN */
	/* Added break before lists. -DN */
    | QSP Lbracket AX B0 expr_list           E Z Rbracket
    | QSP Lparen   AX B0 actual_list         E Z Rparen
    | QSP Lparen                               Z Rparen
    | QSP Lbrace   AX B0 elem_list elem_tail E Z Rbrace
    | QSP Lbrace                               Z Rbrace
    ;


/*-----------  _t ==> expr or type  ---------------*/

expr_t:   B zexpr_t E ;
zexpr_t:  e1_t | zexpr_t A Or SP e1  ;

e1_t:     B ze1_t E ;
ze1_t:    e2_t | ze1_t A And SP e2 ;

e2_t:     Not SP e2 | e3_t ;

e3_t:     B ze3_t E ;
ze3_t:    e4_t | ze3_t A relop SP e4 ;

e4_t:     B ze4_t E ;
ze4_t:    e5_t | ze4_t A addop SP e5 ;

e5_t:     B ze5_t E ;
ze5_t:    e6_t | ze5_t A mulop SP e6 ;

e6_t:     e7_t | Plus Z e6 | Minus Z e6 ;

e7_t:       type_name
          | type_name                            selector_list_t
          | type_name SP simple_object_type_list 
          | root_type SP simple_object_type_list
          | type_constructor1
          | type_constructor2
          | type_constructor2                    cons_value selector_list
          | Lparen expr_t Z Rparen               selector_list
          | e8_t
          ;

e8_t:     Card_const | Real_const | Char_const | Str_const ;

selector_list_t:
      selector_t
    | selector_list_t Z selector
    ;

selector_t:
      Dot Ident
    | Uparrow
	/* Removed SP from front of each of these. -DN */
	/* Added break before lists. -DN */
    | QSP Lbracket AX B0 expr_list           E Z Rbracket
    | QSP Lparen   AX B0 actual_list         E Z Rparen
    | QSP Lparen                               Z Rparen
    | QSP Lbrace   AX B0 elem_list elem_tail E Z Rbrace
    | QSP Lbrace                               Z Rbrace
    ;

cons_value:
      QSP Lbrace   AX B0 elem_list elem_tail E Z Rbrace
    | QSP Lbrace                               Z Rbrace
    ;

/*--------------------- string expressions ------------------------*/
/* This is slightly stripped down version of "expr".  It's used for
   the string expressions that may define brands...  Yuck!  */

Str_expr: B e4_s E ;

e4_s:     B ze4_s E ;
ze4_s:    e7_s | ze4_s A Ampersand SP e7_s ;

e7_s:     B e8_s selector_list E ;

e8_s:     Ident
        | Str_const
        | Lparen Str_expr Z Rparen
        | type_constructor2 cons_value
        ;

/*-----------------------------------------*/

expr_list:
      Z expr
    | expr_list Z Comma A expr
    ;

actual_list:
      Z actual
    | actual_list Z Comma A actual
    ;

actual:
      G expr_t             E
    | G B B B B B Ident SP Assign A expr E E E E E E
    ;    /* the extra B's & E's match the expression hierarchy. yech! */

elem_list:
      Z elem
    | elem_list Z Comma A elem
    ;

elem:
      expr
    | expr Z Dotdot A expr
    | expr SP Assign A expr
    ;

elem_tail:
      /* empty */
    | Z Comma A Dotdot
    ;

opt_id_list:
      /* empty */
    | id_list
    ;

id_list:
      Ident
    | id_list Comma A Ident
    ;


/*---------------------  terminals ----------------------------*/

Ampersand:     AMPERSAND { PR ("&");} NPS ;
Assign:        ASSIGN { PR (":=");} NPS ;
Asterisk:      ASTERISK { PR ("*");} NPS ;
Bar:           BAR { PR ("|");} NPS ;
Colon:         COLON { PR (":");} NPS ;
Comma:         COMMA { PR (",");} NPS ;
Dot:           DOT { PR (".");} NPS ;
Dotdot:        DOTDOT { PR ("..");} NPS ;
Equal:         EQUAL { PR ("=");} NPS ;
Greater:       GREATER { PR (">");} NPS ;
Grequal:       GREQUAL { PR (">=");} NPS ;
Less:          LESS { PR ("<");} NPS ;
Lsequal:       LSEQUAL { PR ("<=");} NPS ;
Minus:         MINUS { PR ("-");} NPS ;
Notequal:      SHARP { PR ("\043");} NPS ;
Plus:          PLUS { PR ("+");} NPS ;
Rarrow:        RARROW { PR ("=>");} NPS ;
Rbrace:        RBRACE { PR ("}");} NPS ;
Rbracket:      RBRACKET { PR ("]");} NPS ;
Rparen:        RPAREN { PR (")");} NPS ;
Semi:          SEMICOLON { PR (";");} NPS ;
Semi1:         SEMICOLON { PR (";");} ;
Slash:         SLASH { PR ("/");} NPS ;
Subtype:       SUBTYPE { PR ("<:");} NPS ;
Uparrow:       UPARROW { PR ("^");} NPS ;

/* These used to do CommentPragmaAfterOpen or CommentPragmaAfterOpen2. */
Lparen:        LPAREN { PR ("("); } NPS ;
Lparen2:       LPAREN { PR ("("); } NPS ;
Lbracket:      LBRACKET { PR ("["); } NPS ;
Lbrace:        LBRACE { PR ("{"); } NPS ;

/* CommentPragmaAfterOpen:  * empty * | SP InitialNPS A ; */
/* CommentPragmaAfterOpen2: * empty * |    InitialNPS ; */

Pr_External:   PR_EXTERNAL { PF (&lexbuf[$1], fonts->fixedComment);} NPS ;
Pr_Inline:     PR_INLINE { PF (&lexbuf[$1], fonts->fixedComment);} NPS ;
Pr_Obsolete:   PR_OBSOLETE { PF (&lexbuf[$1], fonts->fixedComment);} NPS ;
Pr_Unused:     PR_UNUSED { PF (&lexbuf[$1], fonts->fixedComment);} NPS ;

Ident:         IDENT { PRID (&lexbuf[$1]);} NPS ;
IdentP:	       IDENT { PF (&lexbuf[$1], fonts->procName);} NPS ;
Card_const:    CARD_CONST { PR (&lexbuf[$1]);} NPS ;
Real_const:    REAL_CONST { PR (&lexbuf[$1]);} NPS ;
Char_const:    CHAR_CONST { PF (&lexbuf[$1], fonts->fixed);} NPS ;
Str_const:     STR_CONST { PF (&lexbuf[$1], fonts->fixed);} NPS ;

And:           AND { PK ("AND");} NPS ;
Any:	       ANY { PK ("ANY");} NPS ;
Array:         ARRAY { PK ("ARRAY");} NPS ;
As:	       AS { PK ("AS");} NPS ;
Begin:         BGN { PK ("BEGIN");} NPS ;
Bits:          BITS { PK ("BITS");} NPS ;
Branded:       BRANDED { PK ("BRANDED");} NPS ;
By:            BY { PK ("BY");} NPS ;
Case:          CASE { PK ("CASE");} NPS ;
Const:         CONST { PK ("CONST");} NPS ;
Div:           DIV { PR ("DIV");} NPS ;
Do:            DO { PK ("DO");} NPS ;
Else:          ELSE { PK ("ELSE");} NPS ;
Elsif:         ELSIF { PK ("ELSIF");} NPS ;
End:           END { PK ("END");} NPS ;
Eval:          EVAL { PK ("EVAL");} NPS ;
Except:        EXCEPT { PK ("EXCEPT");} NPS ;
Exception:     EXCEPTION { PK ("EXCEPTION");} NPS ;
Exit:          EXIT { PK ("EXIT");} NPS ;
Exports:       EXPORTS { PK ("EXPORTS");} NPS ;
Finally:       FINALLY { PK ("FINALLY");} NPS ;
For:           FOR { PK ("FOR");} NPS ;
From:          FROM { PK ("FROM");} NPS ;
Generic:       GENERIC { PK ("GENERIC");} NPS ;
If:            IF { PK ("IF");} NPS ;
Import:        IMPORT { PK ("IMPORT");} NPS ;
In:            IN { PK ("IN");} NPS ;
Interface:     INTERFACE { PK ("INTERFACE");} NPS ;
Lock:          LOCK { PK ("LOCK");} NPS ;
Loop:          LOOP { PK ("LOOP");} NPS ;
Methods:       METHODS { PK ("METHODS");} NPS ;
Mod:           MOD { PK ("MOD");} NPS ;
Module:        MODULE { PK ("MODULE");} NPS ;
Not:           NOT { PK ("NOT");} NPS ;
Object:        OBJECT { PK ("OBJECT");} NPS ;
Of:            OF { PK ("OF");} NPS ;
Or:            OR { PK ("OR");} NPS ;
Overrides:     OVERRIDES { PK ("OVERRIDES");} NPS ;
Procedure:     PROCEDURE { PK ("PROCEDURE");} NPS ;
Raise:         RAISE { PK ("RAISE");} NPS ;
/* Because of grammar ambiguities, it's best to insert any leading spaces
   or breaks here.  The orginal grammar said 'PR(" RAISES")' here. -DN */
Raises:        RAISES { DoBreak(1, 2, 0.0); PK ("RAISES");} NPS ;
Readonly:      READONLY { PK ("READONLY");} NPS ;
Record:        RECORD { PK ("RECORD");} NPS ;
Ref:           REF { PK ("REF");} NPS ;
Repeat:        REPEAT { PK ("REPEAT");} NPS ;
Return:        RETURN { PK ("RETURN");} NPS ;
Reveal:        REVEAL { PK ("REVEAL");} NPS ;
Root:          ROOT { PK ("ROOT");} NPS;
Set:           SET { PK ("SET");} NPS ;
Then:          THEN { PK ("THEN");} NPS ;
To:            TO { PK ("TO");} NPS ;
Try:           TRY { PK ("TRY");} NPS ;
Type:          TYPE { PK ("TYPE");} NPS ;
Typecase:      TYPECASE { PK ("TYPECASE");} NPS ;
Unsafe:        UNSAFE { PK ("UNSAFE");} NPS ;
Until:         UNTIL { PK ("UNTIL");} NPS ;
Untraced:      UNTRACED { PK ("UNTRACED");} NPS ;
Value:         VALUE { PK ("VALUE");} NPS ;
Var:           VAR { PK ("VAR");} NPS ;
While:         WHILE { PK ("WHILE");} NPS ;
With:          WITH { PK ("WITH");} NPS ;

/*--------------------- comments ------------------------*/

InitialNPS:
      WHITESPACE { blanklinep = 0; PrintNPS(1); }
    ;

NPS:
      /* empty */ { blanklinep = 0; }
    | WHITESPACE { blanklinep = 0; PrintNPS(0); }
    ;

/*----------------- formatting semantic routines -----------------------*/

G:     { GR (); }       /* begin group */
B0:    { BE (0.0); }      /* begin object - no indentation */
B:     { BE (offset); } /* begin indented group */
B2:    { BE (offset*2); } /* begin doubly indented group */
E:     { EN (); }       /* end group/object */
EF:    { ENF (); }	/* end group/object forcing comment output */

A:     { DoBreak (1, 2, 0.0); };     /* optimal, ununited break point */
AO:    { DoBreak (1, 3, 0.0); };     /* nobreak-optimal, ununited break */
AX:    { DoBreak (0, 3, 0.0); };     /* no space, std indent */

V:     { DoBreak (1, 1, 0.0); };     /* united break point */
VZ:    { DoBreak (1, 1, -offset); };     /* space, outdent */
VC:    { DoBreak (1, 1, -offset + 2.0 * bodySpaceWidth); }

Z:     { DoBreak (0, 0, 0.0); };     /* no space, no break unless blank line */
SP:    { DoBreak (1, 0, 0.0); };     /* space */
XSP:   { P2 (' '); };              /* simple space */

BL:    { BL (); }                  /* forced break point */

AL2:   { DoAlign (2, 0); };        /* begin aligned object */
AL3:   { DoAlign (3, 0); };
ALZ5:  { DoAlign (5, 1); };
EA:    { EndAlign (); }
/* Tell comment code when Formatter.Align is going to generate a newline. */
ALNL:	{ ALNL(); }

SPNL:  { DoSPNL (); };             /* Space/Newline depending on the style */
QSP:   { DoQSP (); };              /* Space or not, depending on the style */
NL:    { NL (); };

Inc:   { depth++; };
Dec:   { depth--; };

%%

/*-------- additional C code to implement the semantic routines -----------*/

/* The moreComments variable and CheckComm() macro are a way to move some
   of the comment stuff around in the parse tree.  What we'd like to do is
   move all the comments and newlines starting at the first newline outside
   of any enclosing Formatter.End's.  We do this by having PrintNPS format
   the leading comment in a sequence, set moreComments and return.  All the
   routines that generate output use the CheckComm macro to make sure the
   leftover comments get printed first.  EN() doesn't check so that
   comments move outside of Formatter.End's.  P2() doesn't check, which
   allows comments to move past extra spacing characters.

   To move the comments past sequences like 'G stmts E' where stmts may be
   empty, the grammar has been modified so that the G and E are not
   generated for the empty case.  This is the reason for the stmts_group
   production, and for removing empty productions from type_decl_list, etc.

   Blanklinep is used to coordinate PrintNPS's generation of newlines with
   the grammar's.  If HandleComments is being called from something that
   can emit a newline and the whitespace sequence ends with a newline, then
   HandleComments will not emit that final newline, but will set blanklinep
   instead.  This allows the caller to output the newline, perhaps with
   different parameters than HandleComments would have used.

   The trickiest part is coping with Formatter.Align.  alignDepth is > 0
   whenever we are formatting an align.  Each row consists of a group with
   some nested subgroups for each column.  Comments in all but the last
   column are kept inside the subgroup by using EF instead of E to end
   them.  Comments in the last column are allowed to escape the row and are
   formatted as a special NoAlign row.  The alignRow variable tells
   PrintNPS which case we have. */

static int moreComments = 0;	/* PrintNPS has leftover work to do. */
#define CheckComm(br)	{ if (moreComments) HandleComments(0, 0, br); }
static int alignRow = 0;	/* we are at the end of an Align row. */
static int alignDepth = 0;	/* how many Formatter.Aligns are active? */

/*---- interface to the Modula-3 formatter package ---*/
typedef void (*PROC)();
typedef double (*FPROC)();
static PROC Formatter__Flush;
static PROC Formatter__SetFont;
static PROC Formatter__PutChar;
static PROC Formatter__Break;
static PROC Formatter__NewLine;
static PROC Formatter__UnitedBreak;
static PROC Formatter__Group;
static PROC Formatter__Begin;
static PROC Formatter__Align;
static PROC Formatter__NoAlign;
static PROC Formatter__Col;
static PROC Formatter__End;

PR (s)
char *s;
{
  while (*s != 0) {
    P (*s);
    s++; }
}

/* Print a keyword. */
PK (s)
    char *s;
{
    PF(s, fonts->keyword);
}

/* Print in arbitrary font. */
PF(s, f)
    char *s;
    char *f;
{
    Formatter__SetFont(formatter, f);
    PR(s);
    Formatter__SetFont(formatter, fonts->body);
}

static char *builtins[] = {
    "ABS",
    "ADDRESS",
    "ADR",
    "ADRSIZE",
    "BITSIZE",
    "BOOLEAN",
    "BYTESIZE",
    "CARDINAL",
    "CEILING",
    "CHAR",
    "DEC",
    "DISPOSE",
    "EXTENDED",
    "FALSE",
    "FIRST",
    "FLOAT",
    "FLOOR",
    "INC",
    "INTEGER",
    "ISTYPE",
    "LAST",
    "LONGREAL",
    "LOOPHOLE",
    "MAX",
    "MIN",
    "MUTEX",
    "NARROW",
    "NEW",
    "NIL",
    "NULL",
    "NUMBER",
    "ORD",
    "REAL",
    "REFANY",
    "ROUND",
    "SUBARRAY",
    "TEXT",
    "TRUE",
    "TRUNC",
    "TYPECODE",
    "VAL",
    NULL
};
    

PRID(s)
    register char *s;
{
    register int i;
    register char *b;

    for (i = 0; (b = builtins[i]) != NULL; ++i) {
	if (*b == *s && strcmp(b, s) == 0) {
	    PF(s, fonts->builtinID);
	    return;
	}
    }
    PR(s);
}

PRNONL (s)  /* strip newline */
char *s;
{
  while (*s != 0 && *s != '\n') {
    P (*s);
    s++; }
}

BE (n) double n; { CheckComm(0); Formatter__Begin (formatter, n, MAXWIDTH); }
EN ()          { Formatter__End (formatter); }
ENF ()         { CheckComm(0); Formatter__End (formatter); }
GR ()          { CheckComm(0); Formatter__Group (formatter); }
Flush ()       { CheckComm(0); Formatter__Flush (formatter); }
Reset ()       { }
P(n) int n;    { CheckComm(0); Formatter__PutChar (formatter, n); }
P2(n) int n;   { Formatter__PutChar (formatter, n); }


/* Emit a newline one level out. */
NL ()
{
    CheckComm(1);
    Formatter__NewLine (formatter, -offset, 0);
    blanklinep = 0;
}

/* Emit a newline at current level. */
BL ()
{
    CheckComm(1); 
    Formatter__NewLine (formatter, 0.0, 0);
    blanklinep = 0;
}

DoSPNL ()
{
   if (style == EM_STYLE) {
     DoBreak (1, 0, 0.0);
   } else {
     DoBreak (1, 1, -offset);
   };
}

DoQSP ()
{
    if (callspace) DoBreak (1, 0, 0);
}


DoAlign (cols, oneline)
int cols, oneline;
{
    CheckComm(0);
    ++alignDepth;
    /* Oneline is only true for formals to procedures.  alignDecls does not
       affect them. */
    Formatter__Align(formatter, cols, oneline, (oneline || alignDecls));
}

/* Tell comment code that align is going to insert a newline here. */
ALNL()
{
    /* Only do it if there is comment work left to do. */
    if (moreComments)
	alignRow = 1;
}

EndAlign()
{
    --alignDepth;
    alignRow = 0;
    Formatter__End(formatter);
}

DoBreak (blank, breakpt, offs)
    int blank, breakpt;
    double offs;
{
  CheckComm(1);
  /* Turn breaks into newlines if there is one left to do from comment
     handling. */
  if (blanklinep) {
      Formatter__NewLine(formatter, offs, 0);
      blanklinep = 0;
      return;
  }
  if (blank==1)   Formatter__PutChar (formatter, ' ');
  /* United Break */
  if (breakpt==1) Formatter__UnitedBreak (formatter, offs, 0);
  /* Optimal, OptimalBreak */
  if (breakpt==2) Formatter__Break (formatter, offs, OptimalBreak, 1);
  /* Optimal, OptimalNoBreak */
  if (breakpt==3) Formatter__Break (formatter, offs, breakType, 1);
  /* Not optimal (only used for comments). */
  if (breakpt==4) Formatter__Break (formatter, offs, NonOptimal, 1);
}

#define PRODUCE(x) {fprintf (stderr, "%d ", x); return (x); }

#include <stdio.h>
#include <string.h>
#include "hash.h"
#include "lex.yy.c"
#include "lex_help.h"

initParser (infile, outfile, emacs, caps, fontInfo,
	    offs, ccol, sty, ad, breaktype, follow, callsp,
            charWidth, flush, setFont, putChar, breakF, newLine,
            unitedBreak, group, begin, align, noAlign, col, end)
    char *infile;
    char *outfile;
    long emacs, caps;
    FontInfo *fontInfo;
    double offs, ccol;
    STYLE sty;
    long ad;
    long breaktype, follow, callsp;
    FPROC charWidth;
    PROC flush, setFont, putChar, breakF, newLine;
    PROC unitedBreak, group, begin, align, noAlign, col, end;
{
    yyin = stdin;
    if ((!emacs) && (infile != 0)) {
	yyin = fopen(infile, "r");
	if (yyin == NULL) {
	    fprintf(stderr, "m3pp: unable to open \"%s\".\n", infile);
	    exit(1);
	};
    };
    Formatter__Flush = flush;
    Formatter__SetFont = setFont;
    Formatter__PutChar = putChar;
    Formatter__Break = breakF;
    Formatter__NewLine = newLine;
    Formatter__UnitedBreak = unitedBreak;
    Formatter__Group = group;
    Formatter__Begin = begin;
    Formatter__Align = align;
    Formatter__NoAlign = noAlign;
    Formatter__Col = col;
    Formatter__End = end;
    formatter = outfile;
    calledFromEmacs = emacs;
    capSwitch = caps;
    fonts = fontInfo;
    bodySpaceWidth = charWidth(formatter, fonts->body, ' ');
    commentLeaderWidth = charWidth(formatter, fonts->comment, '(') +
      charWidth(formatter, fonts->comment, '*') +
      charWidth(formatter, fonts->comment, ' ');
    fixedCommentSpaceWidth = charWidth(formatter, fonts->fixedComment, ' ');
    commentCol = ccol;
    offset = offs;
    style = sty;
    alignDecls = ad;
    breakType = breaktype;
    callspace = callsp;
    comBreakNLs = follow;
    insertKeywords();
}

yyerror(s) char *s; {
  char temp; char temp2;
  Reset();
  Flush();
  if (calledFromEmacs == 0) {
        fprintf (stderr,
            "%s while pretty-printing; original position %d\n",
             s, lexposition);
        fprintf(stderr, "Error flagged in output\n");
  }
  PR ("(* SYNTAX ERROR *) ");
  if (!calledFromEmacs && (yychar == 0)) return;  /* end-of-file */
  if ((lexbuf[lexptr] == '\001') && calledFromEmacs) return;
    /* i.e., return if formatting unit from Emacs was incomplete. */
  PR (&lexbuf[lexptr]);
  /* Now print the rest of the input, but if
     the input is terminated by end-of-file (rather than the Emacs
     sentinel '001'), and if the last thing before the end-of-file
     is a newline, then don't print that final newline.  This is
     because Flush() will be called by main when yyerror returns. */
  temp2 = input();   /* input comes from the lex library. */
  if ((calledFromEmacs && (temp2 == '\001')) || (temp2 == 0)) return;
  temp = input();
  while ((temp > 0) && (!calledFromEmacs || (temp != '\001')))
    {P (temp2); temp2 = temp; temp = input();}
  if ((temp2 != '\n') || (temp > 0)) P(temp2);
}

/* Print out first comment.  Hidden down here so it can see the comment
   structure. */
PrintOnePragma()
{
    PR(comments[0].text);
}

PrintNPS(initNPS)
    int initNPS;
{
    HandleComments(1, initNPS, 0);
}

/* Determine if a comment should be refilled or not.  Returns TRUE if it is
   "fixed" (should not be refilled). */
int FixedComment(s)
    char *s;
{
    char c;

    /* True for pragmas, '(**', or '(*|' */
    if (*s == '<' || s[2] == '*' || s[2] == '|')
	return 1;
    /* True for '(*' on a blank line. */
    for (s += 2; (c = *s) != '\n' && c != 0; ++s)
	if (!IsWhite(c))
	    return 0;
    return 1;
}

static int iComment;

/* Comment and newline handling code. */
HandleComments(firstTime, initNPS, doBreak)
    int firstTime;		/* first time on this comment? */
    int initNPS;		/* is this an InitialNPS? */
    int doBreak;		/* is a Break about to happen? */
{
    int i;
    register char *s, c;
    int startCol, ws;
    int needEnd = 0;

    moreComments = 0;		/* avoid recursion in BL, etc. calls. */
    blanklinep = 0;
    if (firstTime) {
	/* Special case: a single newline is discarded.  The pretty-printer
	   will add newlines as necessary. */
	if (nComments == 0 && comments[0].NLs <= 1)
	    return;
	iComment = 0;
    }
    if (!firstTime && alignDepth != 0) {
	/* Put extra Group/End around continuation in case we're in an Align
	   group.  Also, be sure and substract a newline for the one Align
	   will insert automatically.  I hate align. */
	if (alignRow) {
	    /* If we're at the end of a formatter row, then the comment will
	       be a row of its own, so emit a NoAlign op.  NoAlign rows must
	       emit their own leading newline, but not the trailing one. */
	    alignRow = 0;
	    if (iComment < nComments || comments[nComments].NLs > 1) {
		Formatter__NoAlign(formatter);
		GR();
		needEnd = 1;
	    }
	    if (comments[nComments].NLs > 0)
		--comments[nComments].NLs;
	}
    }
    /* Either print a space or goto the comment column for a comment on the
       same line as preceding code. */
    if (firstTime && comments[0].NLs == 0 && !initNPS) {
	if (nComments == 1 && comments[1].NLs == 0)
	    P(' ');
	else
	    Formatter__Col(formatter, commentCol, 0, bodySpaceWidth);
    }
    /* If we're flushing comments in preperation to doing a break, remove one
       last newline, and set blanklinep appropriately.  I believe that this
       code and the align code above should never both execute in the same
       invocation of HandleComments. */
    if (doBreak && comments[nComments].NLs > 0) {
	--comments[nComments].NLs;
	blanklinep = 1;
    }
    for (;; ++iComment) {
	/* The first time, bail out at the first newline. */
	if (firstTime &&
	  (iComment >= nComments || comments[iComment].NLs > comBreakNLs)) {
	    moreComments = 1;
	    break;
	}
	for (i = 0; i < comments[iComment].NLs; ++i) {
	  Formatter__NewLine(formatter, 0.0, 0);
	}
	/* break in middle since last comment struct has no comment text. */
	if (iComment >= nComments)
	    break;
	/* Handle the comment */
	s = comments[iComment].text;
	if (FixedComment(s)) {
	    /* Comment that should not be reformated.  Each line should be
	       output unchanged except for indentation. */
	    startCol = comments[iComment].startCol;
	    BE(0.0);
	    Formatter__SetFont(formatter, fonts->fixedComment);
	    while (*s != 0) {
		/* Emit the text of the line (we're already at the right
		   place for the first line). */
		while (*s != 0 && *s != '\n')
		    P(*s++);
		if (*s == 0)
		    break;
		/* Skip the newline. */
		++s;
		/* Count white space at beginning of the next line. */
		ws = 0;
		while ((c = *s) == ' ' || c == '\t') {
		    if (c == ' ')
			++ws;
		    else
			ws = (ws + 8) & ~7;
		    ++s;
		}
		/* And emit the right amount to indent this line properly. */
		Formatter__NewLine(formatter,
			       fixedCommentSpaceWidth * (ws - startCol), 0);
	    }
	    Formatter__SetFont(formatter, fonts->body);
	    EN();
	}
	else {
	    /* Comment should be reformatted, so parse words and refill.
	       Lines that begin with a vertical bar ('|') are special and
	       should not be filled. */
	    int specialLine = FALSE;	/* this "word" is a special line */
	    int prevSpecial = FALSE;	/* last one was */
	    int nls;
	    int sentenceBreak = FALSE;	/* last word ended sentence. */

	    startCol = comments[iComment].startCol;
	    BE(commentLeaderWidth);
	    Formatter__SetFont(formatter, fonts->comment);
	    P(*s++);		/* '(' */
	    P(*s++);		/* '*' */
	    while (*s != 0) {
		/* Once around per word or special line. */
		nls = 0;
		prevSpecial = specialLine;
		specialLine = FALSE;
		while (IsWhite(*s)) {
		    if (*s == '\n') {
			++nls;
			/* Check for special line. */
			if (s[1] == '|') {
			    ++s;
			    specialLine = TRUE;
			    break;
			}
		    }
		    ++s;
		}
		/* Deal with special lines. */
		if (specialLine) {
		    while (nls-- > 0)
			Formatter__NewLine(formatter, -MAXWIDTH, 0);
		    Formatter__SetFont(formatter, fonts->fixedComment);
		    /* Count white space at beginning of this comment. */
		    ws = 1;	/* count the | for now */
		    P(*s++);	/* and print it */
		    while ((c = *s) == ' ' || c == '\t') {
			if (c == ' ')
			    ++ws;
			else
			    ws = (ws + 8) & ~7;
			++s;
		    }
		    /* Emit the right amount to move indent this line
		       properly. */
		    Formatter__Col(formatter,
			       fixedCommentSpaceWidth * (ws - startCol - 3),
				    1, 0.0);
		    while (*s != '\n' && *s != 0)
			P(*s++);
		    Formatter__SetFont(formatter, fonts->comment);
		}
		else {
		    /* If more than one newline we have a paragraph break.
		       Emit the newlines. */
		    if (nls > 1 || prevSpecial) {
			while (nls-- > 0)
			  Formatter__NewLine(formatter, 0.0, 0);
		    }
		    /* Word break. */
		    else {
			/* Avoid space if there was no leading white space. */
			if (s != comments[iComment].text + 2)
			    Formatter__PutChar(formatter, ' ');
			/* Don't break before the end of the comment.  This
			   should also check for first word, but doesn't yet. */
			if (strcmp(s, "*)") != 0) {
			    if (sentenceBreak)
				Formatter__PutChar(formatter, ' ');
			    Formatter__Break(formatter, 0.0, 0, 1);
			}
		    }
		    /* Emit the word. */
		    while (!IsWhite(*s) && *s != 0)
			P(*s++);
		    sentenceBreak = index(".!?", s[-1]) != 0;
		}
	    }
	    Formatter__SetFont(formatter, fonts->body);
	    EN();
	}
    }
    if (needEnd)
	EN();
}
