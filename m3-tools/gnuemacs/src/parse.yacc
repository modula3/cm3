/* Copyright (C) 1989, Digital Equipment Corporation                    */
/* All rights reserved.                                                 */
/* See the file COPYRIGHT for a full description.                       */

/* Last modified on Wed Aug 10 15:48:40 PDT 1994 by kalsow              */
/*      modified on Fri Mar 20 20:12:10 PST 1992 by muller              */
/*      modified on Fri Jan 31 16:09:58 PST 1992 by goldberg@xerox.com  */
/*      modified on Tue Dec  3 20:46:25 PST 1991 by meehan              */
/*      modified on Mon Aug 19 14:49:34 1991 by nichols@xerox.com       */
/*      modified on Mon Jun  1 11:37:54 1987 by firefly                 */
/*      modified on  hania, Wed Jan  8 16:38:12 1986                    */

/* A yacc source file for the Modula-3 tags creation.
   was constructed from the grammar given in the Modula-3 report;
   the main problem was to get it right for yacc (an expression can
   start by a type). */

/* basic tokens */
%token ENDOFFILE 0

%token AMPERSAND ASSIGN ASTERISK BAR COLON COMMA DOT DOTDOT
%token EQUAL GREATER GREQUAL LESS LSEQUAL MINUS SHARP PERIOD PLUS
%token RARROW RBRACE RBRACKET RPAREN SEMICOLON SLASH
%token SUBTYPE UPARROW 
%token LPAREN LBRACKET LBRACE

%token IDENT CARD_CONST REAL_CONST CHAR_CONST STR_CONST

/* reserved words */ 
%token AND ANY ARRAY AS BGN BITS BRANDED BY CASE CONST 
%token DIV DO ELSE ELSIF END EVAL EXCEPT EXCEPTION EXIT EXPORTS
%token FINALLY FOR FROM GENERIC IF IMPORT IN INTERFACE LOCK LOOP
%token METHODS MOD MODULE NOT OBJECT OF OR OVERRIDES PROCEDURE RAISE RAISES
%token READONLY RECORD REF REPEAT RETURN REVEAL ROOT SET THEN TO
%token TRY TYPE TYPECASE UNSAFE UNTIL UNTRACED VALUE VAR WHILE WITH

%start CompilationUnit

%{
#define YYMAXDEPTH 300
  /* make yacc's stack larger than the default of 150 */

#include <stdio.h>

extern notypes, novars, noconst, noexcepts, oldstyle, qnames;

int linecnt = 1;
int charcnt;
int charperlinecnt; /* charcnt for current line */
char linebuf[1000];  /* chars of current line */
char lastident[200];/* last identifier encountered */

int comdepth = 0;
  /* depth of comments, used only by lexer. */
int pragdepth = 0;
  /* depth of pragmas, used only by lexer. */

%}

%%

/*--------------------- modules ------------------------*/

CompilationUnit:
      interface
    | UNSAFE interface
    | module
    | UNSAFE module
    | generic_interface
    | generic_module
    ;

interface:
      INTERFACE IDENT  {recordname(lastident, 0); recordQual(lastident); }
        SEMICOLON 
        import_nl_list
        declaration_nl_list
        END IDENT DOT
    | INTERFACE IDENT  {recordname(lastident, 0); recordQual(lastident); }
         EQUAL IDENT generic_params 
        END IDENT DOT
    ;

module:
      MODULE IDENT  {recordname(lastident, 0); recordQual(lastident);}
         exports SEMICOLON  import_nl_list  named_block DOT
    | MODULE IDENT  {recordname(lastident, 0); recordQual(lastident);}
         exports EQUAL IDENT generic_params END IDENT DOT
    ;

generic_interface:
      GENERIC INTERFACE IDENT 
          {recordname(lastident, 0); recordQual(lastident); }
        generic_params SEMICOLON 
        import_nl_list
        declaration_nl_list
        END IDENT DOT
    ;

generic_module:
      GENERIC MODULE IDENT  {recordname(lastident, 0); recordQual(lastident);}
        generic_params SEMICOLON 
        import_nl_list
        named_block DOT
    ;

/* Good enough for both formals and actuals. */
generic_params:
      /* empty */
    | LPAREN opt_id_list RPAREN
    ;

exports:
      /* empty */
    | EXPORTS id_list     ;

import_nl_list:
      /* empty */
    | import_nl_list import_nl
    ;

import_nl:
      FROM IDENT IMPORT id_list SEMICOLON 
    |            IMPORT import_item_list SEMICOLON 
    ;

import_item_list:
      IDENT
    | IDENT AS IDENT
    | import_item_list COMMA IDENT
    | import_item_list COMMA IDENT AS IDENT
    ;

block:
      declaration_nl_list  BGN stmts   END     ;

named_block:
      declaration_nl_list  BGN stmts   END IDENT     ;

declaration_nl_list:
      /* empty */
    | declaration_nl_list declaration_nl 
    ;

declaration_nl:
      procedure_head EQUAL named_block SEMICOLON 
    | procedure_head SEMICOLON 
    | CONST                                  
    | CONST     const_decl_list      
    | TYPE                                   
    | TYPE      type_decl_list       
    | REVEAL                                 
    | REVEAL    type_decl_list       
    | VAR                                    
    | VAR       var_decl_list        
    | EXCEPTION                              
    | EXCEPTION     exception_decl_list    
    ;

const_decl_list:
                      const_decl
    | const_decl_list const_decl
    ;

const_decl:
      /* Should move outside  */
        IDENT {if (!noconst) recordname(lastident, 1);} COLON type EQUAL
                expr  SEMICOLON  
    |   IDENT  {if (!noconst) recordname(lastident, 1);}   EQUAL
                expr  SEMICOLON 
    ;

type_decl_list:
                     type_decl
    | type_decl_list type_decl
    ;

type_decl:
       type_name  {if (!notypes) recordname(lastident, 1);}
                  EQUAL  type SEMICOLON
    |  type_name  {if (!notypes) recordname(lastident, 1);}
	          SUBTYPE type SEMICOLON
    ;

var_decl_list:
                    var_decl
    | var_decl_list var_decl
    ;

id_list1:
      IDENT {if (!novars) recordname(lastident, 1);}
    | id_list1 COMMA IDENT {if (!novars) recordname(lastident, 1);}
    ;

var_decl:
      id_list1 COLON type  SEMICOLON
    | id_list1  COLON type  ASSIGN expr  SEMICOLON 
    | id_list1 ASSIGN expr  SEMICOLON  
    ;

exception_decl_list:
	/* did have at end of both productions. */
      exception_decl
    | exception_decl_list  exception_decl
    ;

id_list2:
      IDENT {if (!novars) recordname(lastident, 1);}
    | id_list2 COMMA IDENT {if (!novars) recordname(lastident, 1);}
    ;

exception_decl:
      /* Moved break inside LParen. -DN */
       id_list2                          SEMICOLON
    |  id_list2 LPAREN type RPAREN  SEMICOLON
    ;

procedure_head:
      PROCEDURE IDENT  {recordname(lastident, 1);} signature
    ;

signature:
      LPAREN formals RPAREN return_type raises
    ;

return_type:
      /* empty */
    | COLON type
    ;

raises:
      /* empty */
    | RAISES LBRACE opt_qid_list RBRACE
    | RAISES ANY
    ;

formals:
      /* empty */
    | formal  
    | formal_semi_list
    | formal_semi_list formal
    ;

formal_semi_list:
      formal_semi
    | formal_semi_list formal_semi
    ;

formal_semi:
    mode id_list type_and_or_val_semi
    ;

formal:
    mode id_list type_and_or_val
    ;

mode:
      /* empty */
    | VALUE         | VAR           | READONLY      ;

type_and_or_val_semi:
         COLON  type SEMICOLON 
    |    COLON  type ASSIGN expr SEMICOLON
    |    ASSIGN expr SEMICOLON
    ;

type_and_or_val:
         COLON  type
    |    COLON  type ASSIGN expr
    |    ASSIGN expr     ;

/*--------------------- statements ------------------------*/

stmts:
      /* empty */
    | stmt_list            | stmt_list SEMICOLON     ;

/* Statement list with around it only if non-empty. */
stmts_group:
      /* empty */
    | stmts1     ;

/* Non-empty statement list. */
stmts1:
      stmt_list            | stmt_list SEMICOLON     ;

stmt_list:
      stmt
    | stmt_list SEMICOLON stmt
    ;

stmt:
      assignment_stmt
    | block     | call_stmt
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
      /* Swapped and  -DN */
      expr ASSIGN expr     ;

call_stmt:
      expr
    ;

case_stmt:
      CASE expr OF case case_list else END
    | CASE expr OF         case_list else END
    ;

case_list:
      /*empty*/
    | case_list BAR case
    ;

case:
      labels_list RARROW stmts_group     ;

labels_list:
      labels
    | labels_list COMMA labels
    ;

labels:
      expr     | expr DOTDOT expr     ;

exit_stmt:
      EXIT
    ;

eval_stmt:
      /* Swapped and  -DN */
      EVAL expr     ;

for_stmt:      
      FOR IDENT ASSIGN expr TO expr by DO  stmts END
    ;

by:
      /* empty */
    | BY expr     ;

if_stmt:
      IF expr THEN stmts elsif_list else END
    ;

else:
      /* empty */
    | ELSE stmts
    ;

elsif_list:
      /* empty */
    | elsif_list elsif
    ;

elsif:
      ELSIF expr THEN stmts
    ;

lock_stmt:
      LOCK expr DO stmts END
    ;

loop_stmt: 
      LOOP stmts END
    ;

raise_stmt:
      RAISE expr 
    ;

repeat_stmt:
      REPEAT stmts UNTIL expr     ;

return_stmt:
      RETURN
    | RETURN expr 
    ;

try_finally_stmt: 
      TRY stmts FINALLY stmts END 
    ;

try_stmt:
      TRY stmts EXCEPT handler handler_list else END
    | TRY stmts EXCEPT         handler_list else END
    ;

handler_list:
      /* empty */
    | handler_list BAR handler
    ;

handler:
      qid_list LPAREN IDENT RPAREN RARROW stmts_group
    | qid_list      RARROW stmts_group
    ;

typecase_stmt:
      TYPECASE expr OF tcase tcase_list else END
    | TYPECASE expr OF       tcase_list else END
    ;

tcase_list:
      /* empty */
    | tcase_list BAR tcase
    ;

tcase:
      type_list   RARROW stmts_group
    | type_list LPAREN IDENT RPAREN RARROW stmts_group 
    ;

while_stmt:
      WHILE expr DO stmts END 
    ;

with_stmt:
      WITH binding_list DO stmts END 
    ;

binding_list:
      binding
    | binding_list COMMA binding
    ;

binding:
      IDENT EQUAL expr     ;

opt_qid_list:
      /* empty */
    | qid_list
    ;

qid_list:
      qid
    | qid_list COMMA qid
    ;

qid:
      IDENT
    | IDENT DOT IDENT
    ;

/*--------------------- types ------------------------*/

type_list:
      type
    | type_list COMMA type
    ;

type:
      type_name 
    | type_name simple_object_type_list 
    | root_type simple_object_type_list
    | type_constructor  
    | LPAREN type RPAREN          
    ;

type_name:
      qid
    ;

type_constructor:
      type_constructor1
    | type_constructor2
    ;

type_constructor1:
      BITS expr FOR type
    | PROCEDURE signature
    | UNTRACED simple_object_type_list
    |             simple_object_type_list
    | UNTRACED brand REF type
    |             brand REF type
    | LBRACE opt_id_list RBRACE
    | LBRACKET expr DOTDOT expr RBRACKET
    | root_type
    ;

root_type:
      UNTRACED ROOT
    | ROOT
    ;

type_constructor2:
      ARRAY type_list OF type
    | ARRAY             OF type
    | RECORD fields END
    | SET OF type
    ;

simple_object_type_list:
      simple_object_type
    | simple_object_type_list simple_object_type
    ;

simple_object_type:
      brand OBJECT fields methods_part overrides_part END 
    ;

methods_part:
      /* empty */
    | METHODS methods
    ;

overrides_part:
      /* empty */
    | OVERRIDES overrides
    ;

brand:
      /* empty */
    | BRANDED     | BRANDED STR_CONST     ;

fields:
      /* empty */
    | field   
    | field_semi_list 
    | field_semi_list field 
    ;

field_semi_list:
      field_semi
    | field_semi_list field_semi
    ;

field_semi:
      id_list type_and_or_val_semi     ;
field:
      id_list type_and_or_val          ;

methods:
      /* empty */
    | method  
    | method_semi_list  
    | method_semi_list method  
    ;

method_semi_list:
      method_semi
    | method_semi_list method_semi
    ;

method_semi:
       IDENT signature SEMICOLON             
    |  IDENT  signature    ASSIGN qid SEMICOLON
    |  IDENT ASSIGN qid SEMICOLON
    ;

method:
       IDENT signature       
    |  IDENT signature ASSIGN qid  
    |  IDENT ASSIGN qid  
    ;

overrides:
      /* empty */
    | override 
    | override_semi_list   
    | override_semi_list override
    ;

override_semi_list:
      override_semi
    | override_semi_list override_semi
    ;

override_semi:
       IDENT ASSIGN qid SEMICOLON      ;

override:
       IDENT ASSIGN qid           ;

/*--------------------- expressions ------------------------*/

expr:   zexpr ;
zexpr:  e1 | zexpr OR e1  ;

e1:     ze1 ;
ze1:    e2 | ze1 AND e2 ;

e2:     NOT e2 | e3 ;

e3:     ze3 ;
ze3:    e4 | ze3 relop e4 ;
relop:  EQUAL | SHARP | LESS | LSEQUAL | GREATER | GREQUAL | IN ;

e4:     ze4 ;
ze4:    e5 | ze4 addop e5 ;
addop:  PLUS | MINUS | AMPERSAND ;

e5:     ze5 ;
ze5:    e6 | ze5 mulop e6 ;
mulop:  ASTERISK | SLASH | DIV | MOD ;

e6:     e7 | PLUS e6 | MINUS e6 ;

/* Removed a before selector_list. */
e7:     e8 selector_list ;

e8:     IDENT | CARD_CONST | REAL_CONST | CHAR_CONST | STR_CONST
        | LPAREN expr RPAREN | type_constructor2 ;


selector_list:
      /* empty */
    | selector_list selector
    ;

selector:
      DOT IDENT
    | UPARROW
	/* Removed from front of each of these. -DN */
	/* Added break before lists. -DN */
    | LBRACKET expr_list           RBRACKET
    | LPAREN   actual_list         RPAREN
    | LPAREN                               RPAREN
    | LBRACE   elem_list elem_tail RBRACE
    | LBRACE                               RBRACE
    ;

expr_list:
      expr
    | expr_list COMMA expr
    ;

actual_list:
      actual
    | actual_list COMMA actual
    ;

actual:
      expr      
    | type_constructor1 
    | IDENT ASSIGN expr
    ;    /* the extra s & s match the expression hierarchy. yech! */

elem_list:
      elem
    | elem_list COMMA elem
    ;

elem:
      expr
    | expr DOTDOT expr
    | expr ASSIGN expr
    ;

elem_tail:
      /* empty */
    | COMMA DOTDOT
    ;

opt_id_list:
      /* empty */
    | id_list
    ;

id_list:
      IDENT
    | id_list COMMA IDENT
    ;


%%

yyerror(s)
     char *s;
{
    fprintf(stderr, "line %d: %s\n", linecnt, s);
}

#include "hash.h"
#include "lex.yy.c"

#define MAXITEMS 8000   /* X.i3 has a lot of constants! */
int itemcnt, itemcharcnt;
char *items[MAXITEMS];

setinput(fp)
	FILE *fp;
{
  yyin = fp;
}

char qualifier[200];
recordQual(name)
  char *name;
{
	strcpy(qualifier, name);
}

recordname(name, qual)
   char *name;
{
  char buf[200];
  
  recordname1(name);
   if (qual && qnames) {
     strcpy(buf, qualifier);
     strcat(buf, ".");
     strcat(buf, name);
     recordname1(buf);
   }
}

recordname1(name)
	char *name;
{
  int ln;
  char buf[200];
  
  if (oldstyle) {
    sprintf(buf, "%s\177%d,%d\n", linebuf, linecnt, charcnt); }
  else {
    sprintf(buf, "%s\177%s\177%d,%d\n", name, linebuf, linecnt, charcnt); }
  ln = strlen(buf);
  if (itemcnt >= MAXITEMS) {
    fprintf(stderr, "help: too many items\n");
    exit(1); }
  items[itemcnt] = (char *)malloc(ln+1);
  if (items[itemcnt] == 0) {
    fprintf (stderr, "help: memory exhausted\n");
    exit (1); } 
  strcpy(items[itemcnt], buf);
  itemcnt++;
  itemcharcnt += ln;
}

printit(name)
     char *name;
{
  int i;

  printf("\014\n%s,%d\n", name, itemcharcnt);
  for (i = 0; i < itemcnt; i++) {
    printf("%s", items[i]); }
}

initialize()
{
  itemcnt = itemcharcnt = 0;
  linecnt = 1;
  charcnt = charperlinecnt = 0;
}
