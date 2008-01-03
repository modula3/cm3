/* Copyright (C) 1989, Digital Equipment Corporation                    */
/* All rights reserved.                                                 */
/* See the file COPYRIGHT for a full description.                       */
/*                                                                      */
/* Last modified on Mon Jan 30 09:18:47 PST 1995 by kalsow              */
/*      modified on Fri Mar 20 20:11:40 PST 1992 by muller              */
/*      modified on Fri Jan 31 16:09:58 PST 1992 by goldberg@xerox.com  */
/*      modified on Mon Nov 25 17:41:09 PST 1991 by meehan              */
/*      modified on Mon Aug 19 14:51:03 1991 by nichols@xerox.com       */

%Start Prog Com Prag

 extern int yyleng;

 eol()
 {
	linecnt++;
	charcnt += charperlinecnt;
	charperlinecnt = 0;
	linebuf[0] = 0;
 }

 eot()
 {
	charperlinecnt += yyleng;
	strcat(linebuf, yytext);
 }

 initializeLex ()
 {
	BEGIN Prog; 
 }      

%%

<Prog>"+"		{eot(); return(PLUS);}
<Prog>"-"		{eot(); return(MINUS);}
<Prog>"*"		{eot(); return(ASTERISK);}
<Prog>"/"		{eot(); return(SLASH);}
<Prog>":="		{eot(); return(ASSIGN);}
<Prog>"&"		{eot(); return(AMPERSAND);}
<Prog>"."		{eot(); return(DOT);}
<Prog>","		{eot(); return(COMMA);}
<Prog>";"		{eot(); return(SEMICOLON);}
<Prog>"("		{eot(); return(LPAREN);}
<Prog>"["		{eot(); return(LBRACKET);}
<Prog>"{"		{eot(); return(LBRACE);}
<Prog>"^"		{eot(); return(UPARROW);}
<Prog>"="		{eot(); return(EQUAL);}
<Prog>"=>"              {eot(); return(RARROW);}
<Prog>"#"		{eot(); return(SHARP);}
<Prog>"<"		{eot(); return(LESS);}
<Prog>">"		{eot(); return(GREATER);}
<Prog>"<="		{eot(); return(LSEQUAL);}
<Prog>"<:"              {eot(); return(SUBTYPE);}
<Prog>">="		{eot(); return(GREQUAL);}
<Prog>".."		{eot(); return(DOTDOT);}
<Prog>":"		{eot(); return(COLON);}
<Prog>")"		{eot(); return(RPAREN);}
<Prog>"]"		{eot(); return(RBRACKET);}
<Prog>"}"		{eot(); return(RBRACE);}
<Prog>"|"		{eot(); return(BAR);}

<Prog>[a-zA-Z][a-zA-Z0-9_]*	{PTRKEYWORDENTRY tempp;
				 eot();
				 if ((tempp=lookup(yytext))!=NULL){
					return(tempp->lexval);}
				 else {
					strcpy(lastident, yytext);
					return(IDENT);
				     }
				}

<Prog>[0-9]+(_[0-9A-Fa-f]+)?        {eot(); return(CARD_CONST);}

<Prog>[0-9]+"."[0-9]*([EeDdXx][-+]?[0-9]+)?/[^.] {eot(); return(REAL_CONST);}

<Prog>["]([^"\\\n]|["]["]|\\.)*["] {
				 eot();
				 return(STR_CONST);}

<Prog>[']([^'\\\n]|['][']|\\.)*['] {
				 eot();
				 return(STR_CONST);}

<Prog>"\n"  		{eot(); eol();}
<Prog>^[ \t\f]+		{eot(); }/* leading spaces */ ;
<Prog>[ \t\f]+		/* other spaces */ /* putchar(' ')*/ eot();


<Prog>"(*"	{eot(); BEGIN Com;  comdepth=1; }
<Com>"(*"	{eot(); comdepth++; }
<Com>"*)"	{eot(); comdepth--; if (comdepth==0) BEGIN Prog; }

<Prog>"<*"	{eot(); BEGIN Prag;  pragdepth=1; }
<Prag>"<*"	{eot(); pragdepth++; }
<Prag>"*>"	{eot(); pragdepth--; if (pragdepth==0) BEGIN Prog; }

<Com>"\n"  		{eot(); eol();}
<Prag>"\n"  		{eot(); eol();}
<Com>.		{eot();}
<Prag>.		{eot();}

.		{yyless(0); BEGIN Prog;}
