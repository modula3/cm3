#include <stdio.h>
# define U(x) x
# define NLSTATE yyprevious=YYNEWLINE
# define BEGIN yybgin = yysvec + 1 +
# define INITIAL 0
# define YYLERR yysvec
# define YYSTATE (yyestate-yysvec-1)
# define YYOPTIM 1
# define YYLMAX BUFSIZ
#ifndef __cplusplus
# define output(c) (void)putc(c,yyout)
#else
# define lex_output(c) (void)putc(c,yyout)
#endif

#if defined(__cplusplus) || defined(__STDC__)

#if defined(__cplusplus) && defined(__EXTERN_C__)
extern "C" {
#endif
	int yyback(int *, int);
	int yyinput(void);
	int yylook(void);
	void yyoutput(int);
	int yyracc(int);
	int yyreject(void);
	void yyunput(int);
	int yylex(void);
#ifdef YYLEX_E
	void yywoutput(wchar_t);
	wchar_t yywinput(void);
#endif
#ifndef yyless
	int yyless(int);
#endif
#ifndef yywrap
	int yywrap(void);
#endif
#ifdef LEXDEBUG
	void allprint(char);
	void sprint(char *);
#endif
#if defined(__cplusplus) && defined(__EXTERN_C__)
}
#endif

#ifdef __cplusplus
extern "C" {
#endif
	void exit(int);
#ifdef __cplusplus
}
#endif

#endif
# define unput(c) {yytchar= (c);if(yytchar=='\n')yylineno--;*yysptr++=yytchar;}
# define yymore() (yymorfg=1)
#ifndef __cplusplus
# define input() (((yytchar=yysptr>yysbuf?U(*--yysptr):getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
#else
# define lex_input() (((yytchar=yysptr>yysbuf?U(*--yysptr):getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
#endif
#define ECHO fprintf(yyout, "%s",yytext)
# define REJECT { nstr = yyreject(); goto yyfussy;}
int yyleng;
char yytext[YYLMAX];
int yymorfg;
extern char *yysptr, yysbuf[];
int yytchar;
FILE *yyin = {stdin}, *yyout = {stdout};
extern int yylineno;
struct yysvf { 
	struct yywork *yystoff;
	struct yysvf *yyother;
	int *yystops;};
struct yysvf *yyestate;
extern struct yysvf yysvec[], *yybgin;

# line 2 "../parse.lex"
/* Copyright (C) 1989, Digital Equipment Corporation                    */

# line 3 "../parse.lex"
/* All rights reserved.                                                 */

# line 4 "../parse.lex"
/* See the file COPYRIGHT for a full description.                       */

# line 5 "../parse.lex"
/*                                                                      */

# line 6 "../parse.lex"
/* Last modified on Mon Jan 30 09:18:47 PST 1995 by kalsow              */

# line 7 "../parse.lex"
/*      modified on Fri Mar 20 20:11:40 PST 1992 by muller              */

# line 8 "../parse.lex"
/*      modified on Fri Jan 31 16:09:58 PST 1992 by goldberg@xerox.com  */

# line 9 "../parse.lex"
/*      modified on Mon Nov 25 17:41:09 PST 1991 by meehan              */

# line 10 "../parse.lex"
/*      modified on Mon Aug 19 14:51:03 1991 by nichols@xerox.com       */
# define Prog 2
# define Com 4
# define Prag 6
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
# define YYNEWLINE 10
yylex(){
int nstr; extern int yyprevious;
#ifdef __cplusplus
/* to avoid CC and lint complaining yyfussy not being used ...*/
static int __lex_hack = 0;
if (__lex_hack) goto yyfussy;
#endif
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:

# line 36 "../parse.lex"
	{eot(); return(PLUS);}
break;
case 2:

# line 37 "../parse.lex"
	{eot(); return(MINUS);}
break;
case 3:

# line 38 "../parse.lex"
	{eot(); return(ASTERISK);}
break;
case 4:

# line 39 "../parse.lex"
	{eot(); return(SLASH);}
break;
case 5:

# line 40 "../parse.lex"
	{eot(); return(ASSIGN);}
break;
case 6:

# line 41 "../parse.lex"
	{eot(); return(AMPERSAND);}
break;
case 7:

# line 42 "../parse.lex"
	{eot(); return(DOT);}
break;
case 8:

# line 43 "../parse.lex"
	{eot(); return(COMMA);}
break;
case 9:

# line 44 "../parse.lex"
	{eot(); return(SEMICOLON);}
break;
case 10:

# line 45 "../parse.lex"
	{eot(); return(LPAREN);}
break;
case 11:

# line 46 "../parse.lex"
	{eot(); return(LBRACKET);}
break;
case 12:

# line 47 "../parse.lex"
	{eot(); return(LBRACE);}
break;
case 13:

# line 48 "../parse.lex"
	{eot(); return(UPARROW);}
break;
case 14:

# line 49 "../parse.lex"
	{eot(); return(EQUAL);}
break;
case 15:

# line 50 "../parse.lex"
             {eot(); return(RARROW);}
break;
case 16:

# line 51 "../parse.lex"
	{eot(); return(SHARP);}
break;
case 17:

# line 52 "../parse.lex"
	{eot(); return(LESS);}
break;
case 18:

# line 53 "../parse.lex"
	{eot(); return(GREATER);}
break;
case 19:

# line 54 "../parse.lex"
	{eot(); return(LSEQUAL);}
break;
case 20:

# line 55 "../parse.lex"
             {eot(); return(SUBTYPE);}
break;
case 21:

# line 56 "../parse.lex"
	{eot(); return(GREQUAL);}
break;
case 22:

# line 57 "../parse.lex"
	{eot(); return(DOTDOT);}
break;
case 23:

# line 58 "../parse.lex"
	{eot(); return(COLON);}
break;
case 24:

# line 59 "../parse.lex"
	{eot(); return(RPAREN);}
break;
case 25:

# line 60 "../parse.lex"
	{eot(); return(RBRACKET);}
break;
case 26:

# line 61 "../parse.lex"
	{eot(); return(RBRACE);}
break;
case 27:

# line 62 "../parse.lex"
	{eot(); return(BAR);}
break;
case 28:

# line 64 "../parse.lex"
{PTRKEYWORDENTRY tempp;
				 eot();
				 if ((tempp=lookup(yytext))!=NULL){
					return(tempp->lexval);}
				 else {
					strcpy(lastident, yytext);
					return(IDENT);
				     }
				}
break;
case 29:

# line 74 "../parse.lex"
       {eot(); return(CARD_CONST);}
break;
case 30:

# line 76 "../parse.lex"
{eot(); return(REAL_CONST);}
break;
case 31:

# line 78 "../parse.lex"
{
				 eot();
				 return(STR_CONST);}
break;
case 32:

# line 82 "../parse.lex"
{
				 eot();
				 return(STR_CONST);}
break;
case 33:

# line 86 "../parse.lex"
 		{eot(); eol();}
break;
case 34:

# line 87 "../parse.lex"
	{eot(); }
break;
case 35:

# line 88 "../parse.lex"
	/* other spaces */ /* putchar(' ')*/ eot();
break;
case 36:

# line 91 "../parse.lex"
{eot(); BEGIN Com;  comdepth=1; }
break;
case 37:

# line 92 "../parse.lex"
{eot(); comdepth++; }
break;
case 38:

# line 93 "../parse.lex"
{eot(); comdepth--; if (comdepth==0) BEGIN Prog; }
break;
case 39:

# line 95 "../parse.lex"
{eot(); BEGIN Prag;  pragdepth=1; }
break;
case 40:

# line 96 "../parse.lex"
{eot(); pragdepth++; }
break;
case 41:

# line 97 "../parse.lex"
{eot(); pragdepth--; if (pragdepth==0) BEGIN Prog; }
break;
case 42:

# line 99 "../parse.lex"
 		{eot(); eol();}
break;
case 43:

# line 100 "../parse.lex"
 		{eot(); eol();}
break;
case 44:

# line 101 "../parse.lex"
	{eot();}
break;
case 45:

# line 102 "../parse.lex"
	{eot();}
break;
case 46:

# line 104 "../parse.lex"
	{yyless(0); BEGIN Prog;}
break;
case -1:
break;
default:
(void)fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */
int yyvstop[] = {
0,

46,
0,

35,
46,
0,

33,
0,

46,
0,

16,
46,
0,

6,
46,
0,

46,
0,

10,
46,
0,

24,
46,
0,

3,
46,
0,

1,
46,
0,

8,
46,
0,

2,
46,
0,

7,
46,
0,

4,
46,
0,

29,
46,
0,

23,
46,
0,

9,
46,
0,

17,
46,
0,

14,
46,
0,

18,
46,
0,

28,
46,
0,

11,
46,
0,

25,
46,
0,

13,
46,
0,

12,
46,
0,

27,
46,
0,

26,
46,
0,

34,
35,
46,
0,

44,
46,
0,

42,
0,

44,
46,
0,

44,
46,
0,

45,
46,
0,

43,
0,

45,
46,
0,

45,
46,
0,

35,
0,

31,
0,

32,
0,

36,
0,

22,
0,

-30,
0,

29,
0,

5,
0,

39,
0,

20,
0,

19,
0,

15,
0,

21,
0,

28,
0,

34,
35,
0,

37,
0,

38,
0,

41,
0,

40,
0,

30,
0,

30,
-30,
0,

30,
0,

29,
0,

-30,
0,

30,
-30,
0,
0};
# define YYTYPE unsigned char
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	1,9,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,9,	1,0,	
2,0,	10,46,	47,0,	37,65,	
10,46,	3,9,	37,65,	50,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	3,10,	3,11,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	1,9,	
10,46,	48,47,	37,65,	0,0,	
1,9,	6,40,	41,67,	6,41,	
1,9,	16,53,	40,66,	1,9,	
22,54,	1,9,	3,12,	3,13,	
8,44,	45,69,	3,14,	3,15,	
3,16,	3,17,	3,18,	3,19,	
3,20,	3,21,	3,22,	3,23,	
3,24,	25,58,	1,9,	27,59,	
28,62,	1,9,	8,45,	29,63,	
1,9,	44,68,	3,25,	3,26,	
3,27,	3,28,	3,29,	51,50,	
71,0,	3,30,	49,0,	27,60,	
3,30,	0,0,	27,61,	3,30,	
0,0,	1,9,	52,0,	0,0,	
0,0,	1,9,	4,37,	0,0,	
1,9,	0,0,	0,0,	0,0,	
75,0,	0,0,	75,76,	0,0,	
3,30,	0,0,	49,47,	3,31,	
3,9,	3,32,	3,33,	3,9,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	52,50,	
4,13,	0,0,	75,70,	4,14,	
0,0,	4,16,	4,17,	4,18,	
5,38,	4,20,	4,21,	0,0,	
4,23,	0,0,	0,0,	0,0,	
5,38,	5,39,	0,0,	3,34,	
3,35,	3,36,	75,70,	4,25,	
4,26,	4,27,	4,28,	4,29,	
24,55,	0,0,	24,56,	24,56,	
24,56,	24,56,	24,56,	24,56,	
24,56,	24,56,	24,56,	24,56,	
76,0,	5,38,	76,76,	0,0,	
49,47,	0,0,	5,38,	5,40,	
0,0,	5,41,	5,38,	0,0,	
52,50,	5,38,	0,0,	5,38,	
4,31,	0,0,	4,32,	4,33,	
0,0,	0,0,	76,70,	0,0,	
0,0,	7,42,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
5,38,	7,42,	7,43,	5,38,	
12,47,	24,57,	5,38,	0,0,	
0,0,	0,0,	76,70,	0,0,	
12,47,	12,0,	0,0,	0,0,	
4,34,	4,35,	4,36,	0,0,	
0,0,	0,0,	0,0,	5,38,	
0,0,	0,0,	7,42,	5,38,	
0,0,	0,0,	5,38,	7,42,	
0,0,	0,0,	7,44,	7,42,	
0,0,	12,48,	7,42,	0,0,	
7,42,	0,0,	12,47,	0,0,	
0,0,	0,0,	12,47,	0,0,	
0,0,	12,47,	0,0,	12,47,	
7,45,	0,0,	0,0,	0,0,	
0,0,	7,42,	0,0,	0,0,	
7,42,	15,50,	0,0,	7,42,	
0,0,	0,0,	0,0,	0,0,	
12,47,	15,50,	15,0,	12,47,	
0,0,	0,0,	12,47,	0,0,	
0,0,	0,0,	0,0,	0,0,	
7,42,	0,0,	0,0,	0,0,	
7,42,	0,0,	0,0,	7,42,	
0,0,	0,0,	0,0,	12,47,	
0,0,	0,0,	15,50,	12,49,	
0,0,	0,0,	12,47,	15,51,	
0,0,	0,0,	0,0,	15,50,	
0,0,	72,74,	15,50,	72,74,	
15,50,	0,0,	72,75,	72,75,	
72,75,	72,75,	72,75,	72,75,	
72,75,	72,75,	72,75,	72,75,	
0,0,	0,0,	0,0,	0,0,	
0,0,	15,50,	0,0,	0,0,	
15,50,	0,0,	0,0,	15,50,	
74,75,	74,75,	74,75,	74,75,	
74,75,	74,75,	74,75,	74,75,	
74,75,	74,75,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
15,50,	0,0,	0,0,	0,0,	
15,52,	0,0,	0,0,	15,50,	
30,64,	30,64,	30,64,	30,64,	
30,64,	30,64,	30,64,	30,64,	
30,64,	30,64,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	30,64,	30,64,	30,64,	
30,64,	30,64,	30,64,	30,64,	
30,64,	30,64,	30,64,	30,64,	
30,64,	30,64,	30,64,	30,64,	
30,64,	30,64,	30,64,	30,64,	
30,64,	30,64,	30,64,	30,64,	
30,64,	30,64,	30,64,	0,0,	
0,0,	0,0,	0,0,	30,64,	
0,0,	30,64,	30,64,	30,64,	
30,64,	30,64,	30,64,	30,64,	
30,64,	30,64,	30,64,	30,64,	
30,64,	30,64,	30,64,	30,64,	
30,64,	30,64,	30,64,	30,64,	
30,64,	30,64,	30,64,	30,64,	
30,64,	30,64,	30,64,	55,70,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	55,70,	
55,70,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
55,70,	0,0,	0,0,	0,0,	
0,0,	55,70,	0,0,	0,0,	
0,0,	55,70,	0,0,	0,0,	
55,0,	0,0,	55,71,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	57,73,	57,73,	57,73,	
57,73,	57,73,	57,73,	57,73,	
57,73,	57,73,	57,73,	55,70,	
0,0,	0,0,	55,72,	0,0,	
0,0,	55,70,	57,73,	57,73,	
57,73,	57,73,	57,73,	57,73,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	55,72,	0,0,	
0,0,	0,0,	55,70,	0,0,	
0,0,	55,70,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	57,73,	57,73,	
57,73,	57,73,	57,73,	57,73,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
yycrank+-1,	0,		0,	
yycrank+-2,	yysvec+1,	0,	
yycrank+-16,	0,		0,	
yycrank+-85,	yysvec+3,	0,	
yycrank+-127,	0,		0,	
yycrank+-1,	yysvec+5,	0,	
yycrank+-184,	0,		0,	
yycrank+-10,	yysvec+7,	0,	
yycrank+0,	0,		yyvstop+1,
yycrank+4,	0,		yyvstop+3,
yycrank+0,	0,		yyvstop+6,
yycrank+-195,	0,		yyvstop+8,
yycrank+0,	0,		yyvstop+10,
yycrank+0,	0,		yyvstop+13,
yycrank+-252,	0,		yyvstop+16,
yycrank+3,	0,		yyvstop+18,
yycrank+0,	0,		yyvstop+21,
yycrank+0,	0,		yyvstop+24,
yycrank+0,	0,		yyvstop+27,
yycrank+0,	0,		yyvstop+30,
yycrank+0,	0,		yyvstop+33,
yycrank+2,	0,		yyvstop+36,
yycrank+0,	0,		yyvstop+39,
yycrank+102,	0,		yyvstop+42,
yycrank+4,	0,		yyvstop+45,
yycrank+0,	0,		yyvstop+48,
yycrank+25,	0,		yyvstop+51,
yycrank+6,	0,		yyvstop+54,
yycrank+10,	0,		yyvstop+57,
yycrank+300,	0,		yyvstop+60,
yycrank+0,	0,		yyvstop+63,
yycrank+0,	0,		yyvstop+66,
yycrank+0,	0,		yyvstop+69,
yycrank+0,	0,		yyvstop+72,
yycrank+0,	0,		yyvstop+75,
yycrank+0,	0,		yyvstop+78,
yycrank+6,	0,		yyvstop+81,
yycrank+0,	0,		yyvstop+85,
yycrank+0,	0,		yyvstop+88,
yycrank+4,	0,		yyvstop+90,
yycrank+1,	0,		yyvstop+93,
yycrank+0,	0,		yyvstop+96,
yycrank+0,	0,		yyvstop+99,
yycrank+11,	0,		yyvstop+101,
yycrank+11,	0,		yyvstop+104,
yycrank+0,	yysvec+10,	yyvstop+107,
yycrank+-4,	yysvec+12,	0,	
yycrank+3,	0,		yyvstop+109,
yycrank+-72,	yysvec+12,	0,	
yycrank+-9,	yysvec+15,	0,	
yycrank+40,	0,		yyvstop+111,
yycrank+-80,	yysvec+15,	0,	
yycrank+0,	0,		yyvstop+113,
yycrank+0,	0,		yyvstop+115,
yycrank+-422,	0,		yyvstop+117,
yycrank+0,	yysvec+24,	yyvstop+119,
yycrank+429,	0,		0,	
yycrank+0,	0,		yyvstop+121,
yycrank+0,	0,		yyvstop+123,
yycrank+0,	0,		yyvstop+125,
yycrank+0,	0,		yyvstop+127,
yycrank+0,	0,		yyvstop+129,
yycrank+0,	0,		yyvstop+131,
yycrank+0,	yysvec+30,	yyvstop+133,
yycrank+0,	yysvec+37,	yyvstop+135,
yycrank+0,	0,		yyvstop+138,
yycrank+0,	0,		yyvstop+140,
yycrank+0,	0,		yyvstop+142,
yycrank+0,	0,		yyvstop+144,
yycrank+0,	0,		yyvstop+146,
yycrank+-34,	yysvec+55,	yyvstop+148,
yycrank+254,	0,		yyvstop+151,
yycrank+0,	yysvec+57,	yyvstop+153,
yycrank+276,	0,		0,	
yycrank+-54,	yysvec+55,	yyvstop+155,
yycrank+-114,	yysvec+55,	yyvstop+157,
0,	0,	0};
struct yywork *yytop = yycrank+531;
struct yysvf *yybgin = yysvec+1;
char yymatch[] = {
  0,   1,   1,   1,   1,   1,   1,   1, 
  1,   9,  10,   1,   9,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  9,   1,  34,   1,   1,   1,   1,  39, 
  1,   1,   1,  43,   1,  43,  46,   1, 
 48,  48,  48,  48,  48,  48,  48,  48, 
 48,  48,   1,   1,   1,   1,   1,   1, 
  1,  65,  65,  65,  68,  68,  65,  71, 
 71,  71,  71,  71,  71,  71,  71,  71, 
 71,  71,  71,  71,  71,  71,  71,  71, 
 88,  71,  71,   1,  92,   1,   1,  95, 
  1,  65,  65,  65,  68,  68,  65,  71, 
 71,  71,  71,  71,  71,  71,  71,  71, 
 71,  71,  71,  71,  71,  71,  71,  71, 
 88,  71,  71,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
0};
char yyextra[] = {
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,1,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0};
/*	Copyright (c) 1989 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#pragma ident	"@(#)ncform	6.8	95/02/11 SMI"

int yylineno =1;
# define YYU(x) x
# define NLSTATE yyprevious=YYNEWLINE
struct yysvf *yylstate [YYLMAX], **yylsp, **yyolsp;
char yysbuf[YYLMAX];
char *yysptr = yysbuf;
int *yyfnd;
extern struct yysvf *yyestate;
int yyprevious = YYNEWLINE;
#if defined(__cplusplus) || defined(__STDC__)
int yylook(void)
#else
yylook()
#endif
{
	register struct yysvf *yystate, **lsp;
	register struct yywork *yyt;
	struct yysvf *yyz;
	int yych, yyfirst;
	struct yywork *yyr;
# ifdef LEXDEBUG
	int debug;
# endif
	char *yylastch;
	/* start off machines */
# ifdef LEXDEBUG
	debug = 0;
# endif
	yyfirst=1;
	if (!yymorfg)
		yylastch = yytext;
	else {
		yymorfg=0;
		yylastch = yytext+yyleng;
		}
	for(;;){
		lsp = yylstate;
		yyestate = yystate = yybgin;
		if (yyprevious==YYNEWLINE) yystate++;
		for (;;){
# ifdef LEXDEBUG
			if(debug)fprintf(yyout,"state %d\n",yystate-yysvec-1);
# endif
			yyt = yystate->yystoff;
			if(yyt == yycrank && !yyfirst){  /* may not be any transitions */
				yyz = yystate->yyother;
				if(yyz == 0)break;
				if(yyz->yystoff == yycrank)break;
				}
#ifndef __cplusplus
			*yylastch++ = yych = input();
#else
			*yylastch++ = yych = lex_input();
#endif
			if(yylastch > &yytext[YYLMAX]) {
				fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
				exit(1);
			}
			yyfirst=0;
		tryagain:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"char ");
				allprint(yych);
				putchar('\n');
				}
# endif
			yyr = yyt;
			if ( (int)yyt > (int)yycrank){
				yyt = yyr + yych;
				if (yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					if(lsp > &yylstate[YYLMAX]) {
						fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
						exit(1);
					}
					goto contin;
					}
				}
# ifdef YYOPTIM
			else if((int)yyt < (int)yycrank) {		/* r < yycrank */
				yyt = yyr = yycrank+(yycrank-yyt);
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"compressed state\n");
# endif
				yyt = yyt + yych;
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					if(lsp > &yylstate[YYLMAX]) {
						fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
						exit(1);
					}
					goto contin;
					}
				yyt = yyr + YYU(yymatch[yych]);
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"try fall back character ");
					allprint(YYU(yymatch[yych]));
					putchar('\n');
					}
# endif
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transition */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					if(lsp > &yylstate[YYLMAX]) {
						fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
						exit(1);
					}
					goto contin;
					}
				}
			if ((yystate = yystate->yyother) && (yyt= yystate->yystoff) != yycrank){
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"fall back to state %d\n",yystate-yysvec-1);
# endif
				goto tryagain;
				}
# endif
			else
				{unput(*--yylastch);break;}
		contin:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"state %d char ",yystate-yysvec-1);
				allprint(yych);
				putchar('\n');
				}
# endif
			;
			}
# ifdef LEXDEBUG
		if(debug){
			fprintf(yyout,"stopped at %d with ",*(lsp-1)-yysvec-1);
			allprint(yych);
			putchar('\n');
			}
# endif
		while (lsp-- > yylstate){
			*yylastch-- = 0;
			if (*lsp != 0 && (yyfnd= (*lsp)->yystops) && *yyfnd > 0){
				yyolsp = lsp;
				if(yyextra[*yyfnd]){		/* must backup */
					while(yyback((*lsp)->yystops,-*yyfnd) != 1 && lsp > yylstate){
						lsp--;
						unput(*yylastch--);
						}
					}
				yyprevious = YYU(*yylastch);
				yylsp = lsp;
				yyleng = yylastch-yytext+1;
				yytext[yyleng] = 0;
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"\nmatch ");
					sprint(yytext);
					fprintf(yyout," action %d\n",*yyfnd);
					}
# endif
				return(*yyfnd++);
				}
			unput(*yylastch);
			}
		if (yytext[0] == 0  /* && feof(yyin) */)
			{
			yysptr=yysbuf;
			return(0);
			}
#ifndef __cplusplus
		yyprevious = yytext[0] = input();
		if (yyprevious>0)
			output(yyprevious);
#else
		yyprevious = yytext[0] = lex_input();
		if (yyprevious>0)
			lex_output(yyprevious);
#endif
		yylastch=yytext;
# ifdef LEXDEBUG
		if(debug)putchar('\n');
# endif
		}
	}
#if defined(__cplusplus) || defined(__STDC__)
int yyback(int *p, int m)
#else
yyback(p, m)
	int *p;
#endif
{
	if (p==0) return(0);
	while (*p) {
		if (*p++ == m)
			return(1);
	}
	return(0);
}
	/* the following are only used in the lex library */
#if defined(__cplusplus) || defined(__STDC__)
int yyinput(void)
#else
yyinput()
#endif
{
#ifndef __cplusplus
	return(input());
#else
	return(lex_input());
#endif
	}
#if defined(__cplusplus) || defined(__STDC__)
void yyoutput(int c)
#else
yyoutput(c)
  int c; 
#endif
{
#ifndef __cplusplus
	output(c);
#else
	lex_output(c);
#endif
	}
#if defined(__cplusplus) || defined(__STDC__)
void yyunput(int c)
#else
yyunput(c)
   int c; 
#endif
{
	unput(c);
	}
