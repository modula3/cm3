#include <stdio.h>

#ifdef __cplusplus
   extern "C" {
     extern int yyreject();
     extern int yywrap();
     extern int yylook();
     extern void main();
     extern int yyback(int *, int);
     extern int yyinput();
     extern void yyoutput(int);
     extern void yyunput(int);
     extern int yylex();
   }
#endif	/* __cplusplus */
# define U(x) x
# define NLSTATE yyprevious=YYNEWLINE
# define BEGIN yybgin = yysvec + 1 +
# define INITIAL 0
# define YYLERR yysvec
# define YYSTATE (yyestate-yysvec-1)
# define YYOPTIM 1
# define YYLMAX 200
# define output(c) putc(c,yyout)
# define input() (((yytchar=yysptr>yysbuf?U(*--yysptr):getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
# define unput(c) {yytchar= (c);if(yytchar=='\n')yylineno--;*yysptr++=yytchar;}
# define yymore() (yymorfg=1)
# define ECHO fprintf(yyout, "%s",yytext)
# define REJECT { nstr = yyreject(); goto yyfussy;}
int yyleng;
int yylenguc;
extern unsigned char yytextarr[];
extern unsigned char yytext[];
int yyposix_point=0;
int yynls16=0;
int yynls_wchar=0;
char *yylocale = "/\001:C;\002:C;\003:C;\004:C;:C;:C;:C;/";
int yymorfg;
extern unsigned char *yysptr, yysbuf[];
int yytchar;
FILE *yyin = {stdin}, *yyout = {stdout};
extern int yylineno;
struct yysvf { 
	int yystoff;
	struct yysvf *yyother;
	int *yystops;};
struct yysvf *yyestate;
extern struct yysvf yysvec[], *yybgin;
/* Copyright (C) 1989, Digital Equipment Corporation               */
/* All rights reserved.                                            */
/* See the file COPYRIGHT for a full description.                  */

/* Last modified on Wed Aug 10 13:21:55 PDT 1994 by kalsow         */
/*      modified on Thu Apr 23 18:08:06 PDT 1992 by muller         */
/*      modified on Mon Apr 20 15:59:06 1992 by nichols@xerox.com  */
/*      modified on Mon Nov 25 17:41:09 PST 1991 by meehan         */
# define Prog 2
# define Com 4
# define Prag 6
# define YYNEWLINE 10
yylex(){
   int nstr; extern int yyprevious;
   while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
   if(yywrap()) return(0); break;
case 1:
	{ return (HandleNPS()); }
break;
case 2:
	{ return (HandleNPS()); }
break;
case 3:
{ return (HandleNPS()); }
break;
case 4:
	{BufferLexeme(1); return(PLUS);}
break;
case 5:
	{BufferLexeme(1); return(MINUS);}
break;
case 6:
	{BufferLexeme(1); return(ASTERISK);}
break;
case 7:
	{BufferLexeme(1); return(SLASH);}
break;
case 8:
	{BufferLexeme(1); return(ASSIGN);}
break;
case 9:
	{BufferLexeme(1); return(AMPERSAND);}
break;
case 10:
	{BufferLexeme(1); return(DOT);}
break;
case 11:
	{BufferLexeme(1); return(COMMA);}
break;
case 12:
	{BufferLexeme(1); return(SEMICOLON);}
break;
case 13:
	{BufferLexeme(1); return(LPAREN);}
break;
case 14:
	{BufferLexeme(1); return(LBRACKET);}
break;
case 15:
	{BufferLexeme(1); return(LBRACE);}
break;
case 16:
	{BufferLexeme(1); return(UPARROW);}
break;
case 17:
	{BufferLexeme(1); return(EQUAL);}
break;
case 18:
	{BufferLexeme(1); return(RARROW);}
break;
case 19:
	{BufferLexeme(1); return(SHARP);}
break;
case 20:
	{BufferLexeme(1); return(LESS);}
break;
case 21:
	{BufferLexeme(1); return(GREATER);}
break;
case 22:
	{BufferLexeme(1); return(LSEQUAL);}
break;
case 23:
	{BufferLexeme(1); return(SUBTYPE);}
break;
case 24:
	{BufferLexeme(1); return(GREQUAL);}
break;
case 25:
	{BufferLexeme(1); return(DOTDOT);}
break;
case 26:
	{BufferLexeme(1); return(COLON);}
break;
case 27:
	{BufferLexeme(1); return(RPAREN);}
break;
case 28:
	{BufferLexeme(1); return(RBRACKET);}
break;
case 29:
	{BufferLexeme(1); return(RBRACE);}
break;
case 30:
	{BufferLexeme(1); return(BAR);}
break;
case 31:
{PTRKEYWORDENTRY tempp;
				 if ((tempp=lookup(yytext))!=NULL){
				        CapBufferLexeme(1);
					return(tempp->lexval);}
				 else {BufferLexeme(1); return(IDENT);}}
break;
case 32:
       {BufferLexeme(1); return(CARD_CONST);}
break;
case 33:
{BufferLexeme(1); return(REAL_CONST);}
break;
case 34:
{
				 BufferLexeme(1); return(STR_CONST);}
break;
case 35:
{
				 BufferLexeme(1); return(STR_CONST);}
break;
case 36:
{
	BufferLexeme(0);
	/* Due to bison bug, we return 0 explicitly instead of ENDOFFILE. */
	return(0);
	}
break;
case 37:
{BufferLexeme(0);return(MODUNIT);}
break;
case 38:
{BufferLexeme(0);return(DEFUNIT);}
break;
case 39:
	{BufferLexeme(1); return(BAD);}
break;
case -1:
break;
default:
   fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */

static void __yy__unused() { main(); }
int yyvstop[] = {
0,

36,
39,
0,

37,
39,
0,

39,
0,

38,
39,
0,

3,
39,
0,

3,
0,

3,
39,
0,

39,
0,

19,
39,
0,

9,
39,
0,

39,
0,

13,
39,
-1,
0,

27,
39,
0,

6,
39,
0,

4,
39,
0,

11,
39,
0,

5,
39,
0,

10,
39,
0,

7,
39,
0,

32,
39,
0,

26,
39,
0,

12,
39,
0,

20,
39,
-2,
0,

17,
39,
0,

21,
39,
0,

31,
39,
0,

14,
39,
0,

28,
39,
0,

16,
39,
0,

15,
39,
0,

30,
39,
0,

29,
39,
0,

37,
0,

38,
0,

34,
0,

35,
0,

1,
0,

25,
0,

-33,
0,

32,
0,

8,
0,

2,
0,

23,
0,

22,
0,

18,
0,

24,
0,

31,
0,

33,
0,

33,
-33,
0,

33,
0,

32,
0,

-33,
0,

33,
-33,
0,
0};
# define YYTYPE unsigned char
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	1,9,	1,10,	
1,11,	44,0,	1,12,	47,0,	
0,0,	0,0,	1,13,	1,14,	
0,0,	0,0,	44,0,	0,0,	
47,0,	0,0,	49,0,	0,0,	
14,41,	0,0,	46,0,	14,42,	
0,0,	0,0,	0,0,	49,47,	
14,43,	0,0,	0,0,	46,44,	
0,0,	1,15,	0,0,	1,16,	
1,17,	45,44,	0,0,	1,18,	
1,19,	1,20,	1,21,	1,22,	
1,23,	1,24,	1,25,	1,26,	
1,27,	1,28,	14,43,	20,50,	
26,51,	48,47,	31,56,	46,44,	
49,47,	63,0,	0,0,	1,29,	
1,30,	1,31,	1,32,	1,33,	
29,55,	32,59,	1,34,	33,60,	
0,0,	1,34,	31,57,	0,0,	
1,34,	31,58,	2,17,	0,0,	
0,0,	2,18,	0,0,	2,20,	
2,21,	2,22,	0,0,	2,24,	
2,25,	67,0,	2,27,	67,68,	
0,0,	1,34,	0,0,	0,0,	
1,35,	1,11,	1,36,	1,37,	
1,11,	2,29,	2,30,	2,31,	
2,32,	2,33,	0,0,	0,0,	
0,0,	0,0,	0,0,	67,62,	
0,0,	49,47,	0,0,	3,17,	
0,0,	46,44,	3,18,	0,0,	
3,20,	3,21,	3,22,	0,0,	
3,24,	3,25,	0,0,	3,27,	
1,38,	1,39,	1,40,	67,62,	
0,0,	0,0,	2,35,	0,0,	
2,36,	2,37,	3,29,	3,30,	
3,31,	3,32,	3,33,	68,0,	
0,0,	68,68,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
4,17,	0,0,	0,0,	4,18,	
0,0,	4,20,	4,21,	4,22,	
0,0,	4,24,	4,25,	0,0,	
4,27,	68,62,	2,38,	2,39,	
2,40,	0,0,	0,0,	3,35,	
0,0,	3,36,	3,37,	4,29,	
4,30,	4,31,	4,32,	4,33,	
0,0,	0,0,	0,0,	0,0,	
0,0,	68,62,	0,0,	0,0,	
0,0,	5,17,	0,0,	0,0,	
5,18,	0,0,	5,20,	5,21,	
5,22,	0,0,	5,24,	5,25,	
0,0,	5,27,	0,0,	3,38,	
3,39,	3,40,	0,0,	0,0,	
4,35,	0,0,	4,36,	4,37,	
5,29,	5,30,	5,31,	5,32,	
5,33,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	6,17,	0,0,	
0,0,	6,18,	0,0,	6,20,	
6,21,	6,22,	0,0,	6,24,	
6,25,	0,0,	6,27,	0,0,	
4,38,	4,39,	4,40,	0,0,	
0,0,	5,35,	0,0,	5,36,	
5,37,	6,29,	6,30,	6,31,	
6,32,	6,33,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	7,17,	
0,0,	0,0,	7,18,	0,0,	
7,20,	7,21,	7,22,	0,0,	
7,24,	7,25,	0,0,	7,27,	
0,0,	5,38,	5,39,	5,40,	
0,0,	0,0,	6,35,	0,0,	
6,36,	6,37,	7,29,	7,30,	
7,31,	7,32,	7,33,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
8,17,	0,0,	0,0,	8,18,	
0,0,	8,20,	8,21,	8,22,	
0,0,	8,24,	8,25,	0,0,	
8,27,	0,0,	6,38,	6,39,	
6,40,	0,0,	0,0,	7,35,	
0,0,	7,36,	7,37,	8,29,	
8,30,	8,31,	8,32,	8,33,	
0,0,	16,0,	16,44,	16,44,	
0,0,	16,44,	0,0,	0,0,	
0,0,	16,44,	16,0,	19,0,	
19,47,	19,47,	0,0,	19,47,	
0,0,	0,0,	0,0,	19,47,	
19,0,	0,0,	0,0,	7,38,	
7,39,	7,40,	0,0,	0,0,	
8,35,	0,0,	8,36,	8,37,	
16,44,	0,0,	16,45,	0,0,	
0,0,	0,0,	0,0,	16,44,	
0,0,	0,0,	19,47,	16,44,	
19,47,	0,0,	16,44,	0,0,	
16,44,	19,48,	0,0,	0,0,	
0,0,	19,47,	0,0,	0,0,	
19,47,	0,0,	19,47,	0,0,	
8,38,	8,39,	8,40,	0,0,	
0,0,	16,44,	0,0,	0,0,	
16,44,	0,0,	0,0,	16,44,	
0,0,	0,0,	0,0,	19,47,	
0,0,	0,0,	19,47,	0,0,	
0,0,	19,47,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
16,44,	0,0,	0,0,	0,0,	
16,46,	0,0,	0,0,	16,44,	
0,0,	0,0,	19,47,	0,0,	
0,0,	0,0,	19,49,	0,0,	
28,52,	19,47,	28,53,	28,53,	
28,53,	28,53,	28,53,	28,53,	
28,53,	28,53,	28,53,	28,53,	
64,66,	0,0,	64,66,	0,0,	
0,0,	64,67,	64,67,	64,67,	
64,67,	64,67,	64,67,	64,67,	
64,67,	64,67,	64,67,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	34,61,	34,61,	34,61,	
34,61,	34,61,	34,61,	34,61,	
34,61,	34,61,	34,61,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	28,54,	34,61,	34,61,	
34,61,	34,61,	34,61,	34,61,	
34,61,	34,61,	34,61,	34,61,	
34,61,	34,61,	34,61,	34,61,	
34,61,	34,61,	34,61,	34,61,	
34,61,	34,61,	34,61,	34,61,	
34,61,	34,61,	34,61,	34,61,	
0,0,	0,0,	0,0,	0,0,	
34,61,	0,0,	34,61,	34,61,	
34,61,	34,61,	34,61,	34,61,	
34,61,	34,61,	34,61,	34,61,	
34,61,	34,61,	34,61,	34,61,	
34,61,	34,61,	34,61,	34,61,	
34,61,	34,61,	34,61,	34,61,	
34,61,	34,61,	34,61,	34,61,	
52,62,	52,62,	52,62,	0,0,	
52,62,	0,0,	0,0,	0,0,	
52,62,	52,62,	66,67,	66,67,	
66,67,	66,67,	66,67,	66,67,	
66,67,	66,67,	66,67,	66,67,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	52,62,	
0,0,	52,62,	0,0,	0,0,	
0,0,	0,0,	52,62,	0,0,	
0,0,	0,0,	52,62,	0,0,	
0,0,	52,0,	0,0,	52,63,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	54,65,	54,65,	
54,65,	54,65,	54,65,	54,65,	
54,65,	54,65,	54,65,	54,65,	
52,62,	0,0,	0,0,	52,64,	
0,0,	0,0,	52,62,	54,65,	
54,65,	54,65,	54,65,	54,65,	
54,65,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	52,64,	
0,0,	0,0,	0,0,	52,62,	
0,0,	0,0,	52,62,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	54,65,	
54,65,	54,65,	54,65,	54,65,	
54,65,	0,0,	0,0,	0,0,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
-1,	0,		0,	
-39,	yysvec+1,	0,	
-76,	yysvec+1,	0,	
-113,	yysvec+1,	0,	
-150,	yysvec+1,	0,	
-187,	yysvec+1,	0,	
-224,	yysvec+1,	0,	
-261,	yysvec+1,	0,	
0,	0,		yyvstop+1,
0,	0,		yyvstop+4,
0,	0,		yyvstop+7,
0,	0,		yyvstop+9,
0,	0,		yyvstop+12,
18,	0,		yyvstop+15,
0,	yysvec+14,	yyvstop+17,
-324,	0,		yyvstop+20,
0,	0,		yyvstop+22,
0,	0,		yyvstop+25,
-334,	0,		yyvstop+28,
9,	0,		yyvstop+30,
0,	0,		yyvstop+34,
0,	0,		yyvstop+37,
0,	0,		yyvstop+40,
0,	0,		yyvstop+43,
0,	0,		yyvstop+46,
6,	0,		yyvstop+49,
0,	0,		yyvstop+52,
382,	0,		yyvstop+55,
3,	0,		yyvstop+58,
0,	0,		yyvstop+61,
12,	0,		yyvstop+64,
3,	0,		yyvstop+68,
6,	0,		yyvstop+71,
413,	0,		yyvstop+74,
0,	0,		yyvstop+77,
0,	0,		yyvstop+80,
0,	0,		yyvstop+83,
0,	0,		yyvstop+86,
0,	0,		yyvstop+89,
0,	0,		yyvstop+92,
0,	0,		yyvstop+95,
0,	0,		yyvstop+97,
0,	yysvec+14,	0,	
-4,	yysvec+16,	0,	
3,	0,		yyvstop+99,
-21,	yysvec+16,	0,	
-6,	yysvec+19,	0,	
14,	0,		yyvstop+101,
-17,	yysvec+19,	0,	
0,	0,		yyvstop+103,
0,	0,		yyvstop+105,
-535,	0,		yyvstop+107,
0,	yysvec+28,	yyvstop+109,
542,	0,		0,	
0,	0,		yyvstop+111,
0,	0,		yyvstop+113,
0,	0,		yyvstop+115,
0,	0,		yyvstop+117,
0,	0,		yyvstop+119,
0,	0,		yyvstop+121,
0,	yysvec+34,	yyvstop+123,
0,	0,		yyvstop+125,
-11,	yysvec+52,	yyvstop+127,
397,	0,		yyvstop+130,
0,	yysvec+54,	yyvstop+132,
498,	0,		0,	
-39,	yysvec+52,	yyvstop+134,
-93,	yysvec+52,	yyvstop+136,
0,	0,	0};
struct yywork *yytop = yycrank+644;
struct yysvf *yybgin = yysvec+1;
unsigned char yymatch[] = {
00  ,01  ,02  ,03  ,03  ,05  ,03  ,03  ,
03  ,011 ,012 ,03  ,011 ,011 ,03  ,03  ,
03  ,03  ,03  ,03  ,03  ,03  ,03  ,03  ,
03  ,03  ,03  ,03  ,03  ,03  ,03  ,03  ,
040 ,03  ,'"' ,03  ,03  ,03  ,03  ,047 ,
03  ,03  ,03  ,'+' ,03  ,'+' ,'.' ,03  ,
'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,
'0' ,'0' ,03  ,03  ,03  ,03  ,03  ,03  ,
03  ,'A' ,'A' ,'A' ,'D' ,'D' ,'A' ,'G' ,
'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,
'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,
'X' ,'G' ,'G' ,03  ,0134,03  ,03  ,'_' ,
03  ,'A' ,'A' ,'A' ,'D' ,'D' ,'A' ,'G' ,
'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,
'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,
'X' ,'G' ,'G' ,03  ,03  ,03  ,03  ,03  ,
03  ,03  ,03  ,03  ,03  ,03  ,03  ,03  ,
03  ,03  ,03  ,03  ,03  ,03  ,03  ,03  ,
03  ,03  ,03  ,03  ,03  ,03  ,03  ,03  ,
03  ,03  ,03  ,03  ,03  ,03  ,03  ,03  ,
03  ,03  ,03  ,03  ,03  ,03  ,03  ,03  ,
03  ,03  ,03  ,03  ,03  ,03  ,03  ,03  ,
03  ,03  ,03  ,03  ,03  ,03  ,03  ,03  ,
03  ,03  ,03  ,03  ,03  ,03  ,03  ,03  ,
03  ,03  ,03  ,03  ,03  ,03  ,03  ,03  ,
03  ,03  ,03  ,03  ,03  ,03  ,03  ,03  ,
03  ,03  ,03  ,03  ,03  ,03  ,03  ,03  ,
03  ,03  ,03  ,03  ,03  ,03  ,03  ,03  ,
03  ,03  ,03  ,03  ,03  ,03  ,03  ,03  ,
03  ,03  ,03  ,03  ,03  ,03  ,03  ,03  ,
03  ,03  ,03  ,03  ,03  ,03  ,03  ,03  ,
03  ,03  ,03  ,03  ,03  ,03  ,03  ,03  ,
0};
unsigned char yyextra[] = {
0,1,1,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,1,0,0,0,0,0,0,
0};
int yylineno =1;
# define YYU(x) x
# define NLSTATE yyprevious=YYNEWLINE
 
#ifdef YYNLS16_WCHAR
unsigned char yytextuc[YYLMAX * sizeof(wchar_t)];
# ifdef YY_PCT_POINT /* for %pointer */
wchar_t yytextarr[YYLMAX];
wchar_t *yytext;
# else               /* %array */
wchar_t yytextarr[1];
wchar_t yytext[YYLMAX];
# endif
#else
unsigned char yytextuc;
# ifdef YY_PCT_POINT /* for %pointer */
unsigned char yytextarr[YYLMAX];
unsigned char *yytext;
# else               /* %array */
unsigned char yytextarr[1];
unsigned char yytext[YYLMAX];
# endif
#endif

struct yysvf *yylstate [YYLMAX], **yylsp, **yyolsp;
unsigned char yysbuf[YYLMAX];
unsigned char *yysptr = yysbuf;
int *yyfnd;
extern struct yysvf *yyestate;
int yyprevious = YYNEWLINE;
yylook(){
	struct yysvf *yystate, **lsp;
	struct yywork *yyt;
	struct yysvf *yyz;
	int yych, yyfirst;
	struct yywork *yyr;
# ifdef LEXDEBUG
	int debug;
# endif
/*	char *yylastch;
 * ***** nls8 ***** */
	unsigned char *yylastch, sec;
	/* start off machines */
# ifdef LEXDEBUG
	debug = 0;
# endif
	yyfirst=1;
	if (!yymorfg)
#ifdef YYNLS16_WCHAR
		yylastch = yytextuc;
#else
		yylastch = yytext;
#endif
	else {
		yymorfg=0;
#ifdef YYNLS16_WCHAR
		yylastch = yytextuc+yylenguc;
#else
		yylastch = yytext+yyleng;
#endif
		}
	for(;;){
		lsp = yylstate;
		yyestate = yystate = yybgin;
		if (yyprevious==YYNEWLINE) yystate++;
		for (;;){
# ifdef LEXDEBUG
			if(debug)fprintf(yyout,"state %d\n",yystate-yysvec-1);
# endif
			yyt = &yycrank[yystate->yystoff];
			if(yyt == yycrank && !yyfirst){  /* may not be any transitions */
				yyz = yystate->yyother;
				if(yyz == 0)break;
				if(yyz->yystoff == 0)break;
				}
			*yylastch++ = yych = input();
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
					goto contin;
					}
				}
			if ((yystate = yystate->yyother) && (yyt = &yycrank[yystate->yystoff]) != yycrank){
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
#ifdef YYNLS16_WCHAR
				yylenguc = yylastch-yytextuc+1;
				yytextuc[yylenguc] = 0;
#else
				yyleng = yylastch-yytext+1;
				yytext[yyleng] = 0;
#endif
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"\nmatch ");
#ifdef YYNLS16_WCHAR
					sprint(yytextuc);
#else
					sprint(yytext);
#endif
					fprintf(yyout," action %d\n",*yyfnd);
					}
# endif
				return(*yyfnd++);
				}
			unput(*yylastch);
			}
#ifdef YYNLS16_WCHAR
		if (yytextuc[0] == 0  /* && feof(yyin) */)
#else
		if (yytext[0] == 0  /* && feof(yyin) */)
#endif
			{
			yysptr=yysbuf;
			return(0);
			}
#ifdef YYNLS16_WCHAR
		yyprevious = yytextuc[0] = input();
#else
		yyprevious = yytext[0] = input();
#endif
		if (yyprevious>0) {
			output(yyprevious);
#ifdef YYNLS16
                        if (yynls16)
#ifdef YYNLS16_WCHAR
                        	if (FIRSTof2(yytextuc[0]))
#else
                        	if (FIRSTof2(yytext[0]))
#endif
     					if (SECof2(sec = input()))
#ifdef YYNLS16_WCHAR
 						output(yyprevious=yytextuc[0]=sec);
#else
 						output(yyprevious=yytext[0]=sec);
#endif
					else 
						unput(sec);
#endif
                }
#ifdef YYNLS16_WCHAR
		yylastch=yytextuc;
#else
		yylastch=yytext;
#endif
# ifdef LEXDEBUG
		if(debug)putchar('\n');
# endif
		}
	}

# ifdef __cplusplus
yyback(int *p, int m)
# else
yyback(p, m)
	int *p;
# endif
{
if (p==0) return(0);
while (*p)
	{
	if (*p++ == m)
		return(1);
	}
return(0);
}
	/* the following are only used in the lex library */
yyinput(){
	return(input());
	
	}

#ifdef __cplusplus
void yyoutput(int c)
#else
yyoutput(c)
  int c;
# endif
{
	output(c);
}

#ifdef __cplusplus
void yyunput(int c)
#else
yyunput(c)
   int c;
#endif
{
	unput(c);
}
