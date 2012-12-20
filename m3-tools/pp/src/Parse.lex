%{
/* Copyright (C) 1989, Digital Equipment Corporation               */
/* All rights reserved.                                            */
/* See the file COPYRIGHT for a full description.                  */

/* Last modified on Wed Aug 10 13:21:55 PDT 1994 by kalsow         */
/*      modified on Thu Apr 23 18:08:06 PDT 1992 by muller         */
/*      modified on Mon Apr 20 15:59:06 1992 by nichols@xerox.com  */
/*      modified on Mon Nov 25 17:41:09 PST 1991 by meehan         */

/*
  We distinguish between known and unknown pragmas.
  Known pragmas can appear at special positions only
  and must fulfill a certain syntax.
  They are part of the grammar and are processed in Parse.yacc.
  Unknown pragmas can appear anywhere and are treated like comments.
  They are processed by the lexer and are identified as WHITESPACE
  for the grammar.
  Some pragmas like NOWARN are known but are handled like unknown
  since they can appear anywhere.
*/

#if defined(__cplusplus) || __STDC__
#define USE_PROTOS
#endif

#ifdef __cplusplus
#define EXTERN_C extern "C"
#define EXTERN_C_BEGIN extern "C" {
#define EXTERN_C_END }
#else
#define EXTERN_C
#define EXTERN_C_BEGIN
#define EXTERN_C_END
#endif

#ifdef USE_PROTOS
EXTERN_C_BEGIN
int HandleSpaces (void);
int HandleCommentPragma (void);
void BufferLexeme (int addLength);
int input (void);
EXTERN_C_END
#endif

%}

%Start Prog Com Prag

%%

"<*"[ \t\f\n\r]*"EXTERNAL"      {BufferLexeme(1); return(PR_EXTERNAL);}
"<*"[ \t\f\n\r]*"INLINE"        {BufferLexeme(1); return(PR_INLINE);}
"<*"[ \t\f\n\r]*"ASSERT"        {BufferLexeme(1); return(PR_ASSERT);}
"<*"[ \t\f\n\r]*"TRACE"         {BufferLexeme(1); return(PR_TRACE);}
"<*"[ \t\f\n\r]*"FATAL"         {BufferLexeme(1); return(PR_FATAL);}
"<*"[ \t\f\n\r]*"UNUSED"        {BufferLexeme(1); return(PR_UNUSED);}
"<*"[ \t\f\n\r]*"OBSOLETE"      {BufferLexeme(1); return(PR_OBSOLETE);}
"<*"[ \t\f\n\r]*"CALLBACK"      {BufferLexeme(1); return(PR_CALLBACK);}
"<*"[ \t\f\n\r]*"EXPORTED"      {BufferLexeme(1); return(PR_EXPORTED);}

"<*"[ \t\f\n\r]*"PRAGMA"        {BufferLexeme(1); return(PR_PRAGMA);}
"<*"[ \t\f\n\r]*"NOWARN"        {BufferLexeme(1); return(PR_NOWARN);}
"<*"[ \t\f\n\r]*"LINE"          {BufferLexeme(1); return(PR_LINE);}
"<*"[ \t\f\n\r]*"LL"            {BufferLexeme(1); return(PR_LL);}
"<*"[ \t\f\n\r]*"LL.sup"        {BufferLexeme(1); return(PR_LLsup);}
"<*"[ \t\f\n\r]*"SPEC"          {BufferLexeme(1); return(PR_SPEC);}
"<*"[ \t\f\n\r]*"LOOPINV"       {BufferLexeme(1); return(PR_LOOPINV);}

"*>"		{BufferLexeme(1); return(RPRAGMA);}

[ \t\f\n\r]	{ return (HandleSpaces()); }
%{
/*
  We match "<"/"*" (look-ahead for "*") instead of "<*"
  since HandleCommentPragma() contains a loop
  that has to process nested comments
  and since it must process the inner comments char-by-char
  we do so at the top level as well.
*/
%}
"<"/"*"		{ return (HandleCommentPragma()); }
"("/"*"		{ return (HandleCommentPragma()); }


"+"		{BufferLexeme(1); return(PLUS);}
"-"		{BufferLexeme(1); return(MINUS);}
"*"		{BufferLexeme(1); return(ASTERISK);}
"/"		{BufferLexeme(1); return(SLASH);}
":="		{BufferLexeme(1); return(ASSIGN);}
"&"		{BufferLexeme(1); return(AMPERSAND);}
"."		{BufferLexeme(1); return(DOT);}
","		{BufferLexeme(1); return(COMMA);}
";"		{BufferLexeme(1); return(SEMICOLON);}
"("		{BufferLexeme(1); return(LPAREN);}
"["		{BufferLexeme(1); return(LBRACKET);}
"{"		{BufferLexeme(1); return(LBRACE);}
"^"		{BufferLexeme(1); return(UPARROW);}
"^'"		{BufferLexeme(1); return(UPARROWPRIME);}
"="		{BufferLexeme(1); return(EQUAL);}
"=>"		{BufferLexeme(1); return(RARROW);}
"#"		{BufferLexeme(1); return(SHARP);}
"<"		{BufferLexeme(1); return(LESS);}
">"		{BufferLexeme(1); return(GREATER);}
"<="		{BufferLexeme(1); return(LSEQUAL);}
"<:"		{BufferLexeme(1); return(SUBTYPE);}
">="		{BufferLexeme(1); return(GREQUAL);}
".."		{BufferLexeme(1); return(DOTDOT);}
":"		{BufferLexeme(1); return(COLON);}
")"		{BufferLexeme(1); return(RPAREN);}
"]"		{BufferLexeme(1); return(RBRACKET);}
"}"		{BufferLexeme(1); return(RBRACE);}
"|"		{BufferLexeme(1); return(BAR);}

[0-9]+(_[0-9A-Fa-f]+)?        {BufferLexeme(1); return(CARD_CONST);}

[0-9]+"."[0-9]*([EeDdXx][-+]?[0-9]+)?/[^.] {BufferLexeme(1); return(REAL_CONST);}

["]([^"\\\001\n]|\\[0-7]{3,3}|\\[^0-9\001])*["] {
				 BufferLexeme(1); return(STR_CONST);}

[']([^'\\\001\n]|\\[0-7]{3,3}|\\x[0-9a-zA-Z]{2,2}|\\[^0-9\001])['] {
				 BufferLexeme(1); return(STR_CONST);}

W[']([^'\\\001\n]|\\[0-7]{6,6}|\\x[0-9a-zA-Z]{4,4}|\\[^0-9\001])['] {
				 BufferLexeme(1); return(STR_CONST);}

%{
/* Primed identifiers as used in SPEC pragams are difficult to handle
   because primes introduce character literals in Modula 3 program text.
   A primed identifier like W' cannot be handled. */
%}
[a-zA-Z][a-zA-Z0-9_]*[']	{BufferLexeme(1); return(IDENTPRIME);}

[a-zA-Z][a-zA-Z0-9_]*	{PTRKEYWORDENTRY tempp;
				 if ((tempp=lookup(yytext))!=NULL){
				        CapBufferLexeme(1);
					return(tempp->lexval);}
				 else {BufferLexeme(1); return(IDENT);}}

[\001]	{
	BufferLexeme(0);
	/* Due to bison bug, we return 0 explicitly instead of ENDOFFILE. */
	return(0);
	}
[\n ]*[\002]	{BufferLexeme(0);return(MODUNIT);}
[\n ]*[\005]	{BufferLexeme(0);return(DEFUNIT);}
.		{BufferLexeme(1); return(BAD);}
