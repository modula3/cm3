/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 92 "../Parse.yacc"


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

#include <stddef.h>

#define lexbufsize 500
char lexbuf[2 * lexbufsize];
int lexptr = 0;
int lexposition = 0;
  /* See BufferLexeme and AddLexLength in Parse.lex */

char *infileName = NULL;
  /* initialized by initParser, needed for error message */

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

/* the opaque Formatter.T object */
char *formatter;

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

#ifdef USE_PROTOS
EXTERN_C_BEGIN
#ifdef YYPARSE_PARAM
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse (void);
#endif
int yylex(void);
#ifdef __cplusplus
typedef void (*PROC)(...);
typedef double (*FPROC)(...);
#else
typedef void (*PROC)();
typedef double (*FPROC)();
#endif
#if 0 /* stronger types would be nice */
typedef double (*CharWidth_t)(Formatter_t*, char*, char);
typedef void (*Flush_t)(void);
typedef void (*SetFont_t)(Formatter_t*, char*);
typedef void (*PutChar_t)(Formatter_t*, char);
typedef void (*Break_t)(Formatter_t*, double, int, int);
typedef void (*Newline_t)(Formatter_t*);
#else
typedef char Formatter_t;
#endif
static int yyinput (void);
void BufferLexeme (int addLength);
void CapBufferLexeme (int addLength);
void PR (const char *s);
void PRID(const char *s);
void PK (const char *s);
void PF(const char *s, const char *f);
void PRID(const char *s);
void PRNONL (const char *s);
void BE (double n);
void EN (void);
void ENF (void);
void GR(void);
void Flush (void);
void Reset (void);
void P(int n);
void P2(int n);
void NL (void);
void BL (void);
void DoSPNL (void);
void DoQSP (void);
void DoAlign (int cols, int oneline);
void ALNL(void);
void EndAlign(void);
void DoBreak (int blank, int breakpt, double offs);
void
initParser (
    char *infile,
    Formatter_t* outfile,
    long emacs,
    long caps,
    FontInfo *fontInfo,
    double offs,
    double ccol,
    STYLE sty,
    long ad,
    long breaktype,
    long follow,
    long callsp,
    FPROC charWidth,
    PROC flush,
    PROC setFont,
    PROC putChar,
    PROC breakF,
    PROC newLine,
    PROC unitedBreak,
    PROC group,
    PROC begin,
    PROC align,
    PROC noAlign,
    PROC col,
    PROC end);
void yyerror(const char*);
void PrintOnePragma(void);
void PrintNPS(int);
int FixedComment(const char*);
void
HandleComments(
    int firstTime,		/* first time on this comment? */
    int initNPS,		/* is this an InitialNPS? */
    int doBreak);   	/* is a Break about to happen? */
EXTERN_C_END
#endif


#line 254 "y.tab.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif


/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    ENDOFFILE = 0,                 /* ENDOFFILE  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    AMPERSAND = 258,               /* AMPERSAND  */
    ASSIGN = 259,                  /* ASSIGN  */
    ASTERISK = 260,                /* ASTERISK  */
    BAR = 261,                     /* BAR  */
    COLON = 262,                   /* COLON  */
    COMMA = 263,                   /* COMMA  */
    DOT = 264,                     /* DOT  */
    DOTDOT = 265,                  /* DOTDOT  */
    EQUAL = 266,                   /* EQUAL  */
    GREATER = 267,                 /* GREATER  */
    GREQUAL = 268,                 /* GREQUAL  */
    LESS = 269,                    /* LESS  */
    LSEQUAL = 270,                 /* LSEQUAL  */
    MINUS = 271,                   /* MINUS  */
    SHARP = 272,                   /* SHARP  */
    PLUS = 273,                    /* PLUS  */
    RARROW = 274,                  /* RARROW  */
    RPRAGMA = 275,                 /* RPRAGMA  */
    RBRACE = 276,                  /* RBRACE  */
    RBRACKET = 277,                /* RBRACKET  */
    RPAREN = 278,                  /* RPAREN  */
    SEMICOLON = 279,               /* SEMICOLON  */
    SLASH = 280,                   /* SLASH  */
    SUBTYPE = 281,                 /* SUBTYPE  */
    UPARROW = 282,                 /* UPARROW  */
    LPAREN = 283,                  /* LPAREN  */
    LBRACKET = 284,                /* LBRACKET  */
    LBRACE = 285,                  /* LBRACE  */
    IDENT = 286,                   /* IDENT  */
    CARD_CONST = 287,              /* CARD_CONST  */
    REAL_CONST = 288,              /* REAL_CONST  */
    CHAR_CONST = 289,              /* CHAR_CONST  */
    STR_CONST = 290,               /* STR_CONST  */
    PR_EXTERNAL = 291,             /* PR_EXTERNAL  */
    PR_INLINE = 292,               /* PR_INLINE  */
    PR_OBSOLETE = 293,             /* PR_OBSOLETE  */
    PR_UNUSED = 294,               /* PR_UNUSED  */
    PR_FATAL = 295,                /* PR_FATAL  */
    PR_NOWARN = 296,               /* PR_NOWARN  */
    PR_ASSERT = 297,               /* PR_ASSERT  */
    PR_TRACE = 298,                /* PR_TRACE  */
    PR_LINE = 299,                 /* PR_LINE  */
    PR_PRAGMA = 300,               /* PR_PRAGMA  */
    PR_CALLBACK = 301,             /* PR_CALLBACK  */
    PR_LL = 302,                   /* PR_LL  */
    PR_LLsup = 303,                /* PR_LLsup  */
    PR_EXPORTED = 304,             /* PR_EXPORTED  */
    PR_SPEC = 305,                 /* PR_SPEC  */
    PR_LOOPINV = 306,              /* PR_LOOPINV  */
    IDENTPRIME = 307,              /* IDENTPRIME  */
    UPARROWPRIME = 308,            /* UPARROWPRIME  */
    ALL = 309,                     /* ALL  */
    AXIOM = 310,                   /* AXIOM  */
    DEPEND = 311,                  /* DEPEND  */
    ENSURES = 312,                 /* ENSURES  */
    EXISTS = 313,                  /* EXISTS  */
    FUNC = 314,                    /* FUNC  */
    IFF = 315,                     /* IFF  */
    IMPLIES = 316,                 /* IMPLIES  */
    INVARIANT = 317,               /* INVARIANT  */
    IS = 318,                      /* IS  */
    LET = 319,                     /* LET  */
    MAP = 320,                     /* MAP  */
    MODIFIES = 321,                /* MODIFIES  */
    ON = 322,                      /* ON  */
    PRED = 323,                    /* PRED  */
    PROTECT = 324,                 /* PROTECT  */
    ABSTRACT = 325,                /* ABSTRACT  */
    REQUIRES = 326,                /* REQUIRES  */
    AND = 327,                     /* AND  */
    ANY = 328,                     /* ANY  */
    ARRAY = 329,                   /* ARRAY  */
    AS = 330,                      /* AS  */
    BGN = 331,                     /* BGN  */
    BITS = 332,                    /* BITS  */
    BRANDED = 333,                 /* BRANDED  */
    BY = 334,                      /* BY  */
    CASE = 335,                    /* CASE  */
    CONST = 336,                   /* CONST  */
    DIV = 337,                     /* DIV  */
    DO = 338,                      /* DO  */
    ELSE = 339,                    /* ELSE  */
    ELSIF = 340,                   /* ELSIF  */
    END = 341,                     /* END  */
    EVAL = 342,                    /* EVAL  */
    EXCEPT = 343,                  /* EXCEPT  */
    EXCEPTION = 344,               /* EXCEPTION  */
    EXIT = 345,                    /* EXIT  */
    EXPORTS = 346,                 /* EXPORTS  */
    FINALLY = 347,                 /* FINALLY  */
    FOR = 348,                     /* FOR  */
    FROM = 349,                    /* FROM  */
    GENERIC = 350,                 /* GENERIC  */
    IF = 351,                      /* IF  */
    IMPORT = 352,                  /* IMPORT  */
    IN = 353,                      /* IN  */
    INTERFACE = 354,               /* INTERFACE  */
    LOCK = 355,                    /* LOCK  */
    LOOP = 356,                    /* LOOP  */
    METHODS = 357,                 /* METHODS  */
    MOD = 358,                     /* MOD  */
    MODULE = 359,                  /* MODULE  */
    NOT = 360,                     /* NOT  */
    OBJECT = 361,                  /* OBJECT  */
    OF = 362,                      /* OF  */
    OR = 363,                      /* OR  */
    OVERRIDES = 364,               /* OVERRIDES  */
    PROCEDURE = 365,               /* PROCEDURE  */
    RAISE = 366,                   /* RAISE  */
    RAISES = 367,                  /* RAISES  */
    READONLY = 368,                /* READONLY  */
    RECORD = 369,                  /* RECORD  */
    REF = 370,                     /* REF  */
    REPEAT = 371,                  /* REPEAT  */
    RETURN = 372,                  /* RETURN  */
    REVEAL = 373,                  /* REVEAL  */
    ROOT = 374,                    /* ROOT  */
    SET = 375,                     /* SET  */
    THEN = 376,                    /* THEN  */
    TO = 377,                      /* TO  */
    TRY = 378,                     /* TRY  */
    TYPE = 379,                    /* TYPE  */
    TYPECASE = 380,                /* TYPECASE  */
    UNSAFE = 381,                  /* UNSAFE  */
    UNTIL = 382,                   /* UNTIL  */
    UNTRACED = 383,                /* UNTRACED  */
    VALUE = 384,                   /* VALUE  */
    VAR = 385,                     /* VAR  */
    WHILE = 386,                   /* WHILE  */
    WITH = 387,                    /* WITH  */
    BAD = 388,                     /* BAD  */
    WHITESPACE = 389,              /* WHITESPACE  */
    MODUNIT = 390,                 /* MODUNIT  */
    DEFUNIT = 391                  /* DEFUNIT  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif
/* Token kinds.  */
#define YYEMPTY -2
#define ENDOFFILE 0
#define YYerror 256
#define YYUNDEF 257
#define AMPERSAND 258
#define ASSIGN 259
#define ASTERISK 260
#define BAR 261
#define COLON 262
#define COMMA 263
#define DOT 264
#define DOTDOT 265
#define EQUAL 266
#define GREATER 267
#define GREQUAL 268
#define LESS 269
#define LSEQUAL 270
#define MINUS 271
#define SHARP 272
#define PLUS 273
#define RARROW 274
#define RPRAGMA 275
#define RBRACE 276
#define RBRACKET 277
#define RPAREN 278
#define SEMICOLON 279
#define SLASH 280
#define SUBTYPE 281
#define UPARROW 282
#define LPAREN 283
#define LBRACKET 284
#define LBRACE 285
#define IDENT 286
#define CARD_CONST 287
#define REAL_CONST 288
#define CHAR_CONST 289
#define STR_CONST 290
#define PR_EXTERNAL 291
#define PR_INLINE 292
#define PR_OBSOLETE 293
#define PR_UNUSED 294
#define PR_FATAL 295
#define PR_NOWARN 296
#define PR_ASSERT 297
#define PR_TRACE 298
#define PR_LINE 299
#define PR_PRAGMA 300
#define PR_CALLBACK 301
#define PR_LL 302
#define PR_LLsup 303
#define PR_EXPORTED 304
#define PR_SPEC 305
#define PR_LOOPINV 306
#define IDENTPRIME 307
#define UPARROWPRIME 308
#define ALL 309
#define AXIOM 310
#define DEPEND 311
#define ENSURES 312
#define EXISTS 313
#define FUNC 314
#define IFF 315
#define IMPLIES 316
#define INVARIANT 317
#define IS 318
#define LET 319
#define MAP 320
#define MODIFIES 321
#define ON 322
#define PRED 323
#define PROTECT 324
#define ABSTRACT 325
#define REQUIRES 326
#define AND 327
#define ANY 328
#define ARRAY 329
#define AS 330
#define BGN 331
#define BITS 332
#define BRANDED 333
#define BY 334
#define CASE 335
#define CONST 336
#define DIV 337
#define DO 338
#define ELSE 339
#define ELSIF 340
#define END 341
#define EVAL 342
#define EXCEPT 343
#define EXCEPTION 344
#define EXIT 345
#define EXPORTS 346
#define FINALLY 347
#define FOR 348
#define FROM 349
#define GENERIC 350
#define IF 351
#define IMPORT 352
#define IN 353
#define INTERFACE 354
#define LOCK 355
#define LOOP 356
#define METHODS 357
#define MOD 358
#define MODULE 359
#define NOT 360
#define OBJECT 361
#define OF 362
#define OR 363
#define OVERRIDES 364
#define PROCEDURE 365
#define RAISE 366
#define RAISES 367
#define READONLY 368
#define RECORD 369
#define REF 370
#define REPEAT 371
#define RETURN 372
#define REVEAL 373
#define ROOT 374
#define SET 375
#define THEN 376
#define TO 377
#define TRY 378
#define TYPE 379
#define TYPECASE 380
#define UNSAFE 381
#define UNTIL 382
#define UNTRACED 383
#define VALUE 384
#define VAR 385
#define WHILE 386
#define WITH 387
#define BAD 388
#define WHITESPACE 389
#define MODUNIT 390
#define DEFUNIT 391

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);



/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* ENDOFFILE  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_AMPERSAND = 3,                  /* AMPERSAND  */
  YYSYMBOL_ASSIGN = 4,                     /* ASSIGN  */
  YYSYMBOL_ASTERISK = 5,                   /* ASTERISK  */
  YYSYMBOL_BAR = 6,                        /* BAR  */
  YYSYMBOL_COLON = 7,                      /* COLON  */
  YYSYMBOL_COMMA = 8,                      /* COMMA  */
  YYSYMBOL_DOT = 9,                        /* DOT  */
  YYSYMBOL_DOTDOT = 10,                    /* DOTDOT  */
  YYSYMBOL_EQUAL = 11,                     /* EQUAL  */
  YYSYMBOL_GREATER = 12,                   /* GREATER  */
  YYSYMBOL_GREQUAL = 13,                   /* GREQUAL  */
  YYSYMBOL_LESS = 14,                      /* LESS  */
  YYSYMBOL_LSEQUAL = 15,                   /* LSEQUAL  */
  YYSYMBOL_MINUS = 16,                     /* MINUS  */
  YYSYMBOL_SHARP = 17,                     /* SHARP  */
  YYSYMBOL_PLUS = 18,                      /* PLUS  */
  YYSYMBOL_RARROW = 19,                    /* RARROW  */
  YYSYMBOL_RPRAGMA = 20,                   /* RPRAGMA  */
  YYSYMBOL_RBRACE = 21,                    /* RBRACE  */
  YYSYMBOL_RBRACKET = 22,                  /* RBRACKET  */
  YYSYMBOL_RPAREN = 23,                    /* RPAREN  */
  YYSYMBOL_SEMICOLON = 24,                 /* SEMICOLON  */
  YYSYMBOL_SLASH = 25,                     /* SLASH  */
  YYSYMBOL_SUBTYPE = 26,                   /* SUBTYPE  */
  YYSYMBOL_UPARROW = 27,                   /* UPARROW  */
  YYSYMBOL_LPAREN = 28,                    /* LPAREN  */
  YYSYMBOL_LBRACKET = 29,                  /* LBRACKET  */
  YYSYMBOL_LBRACE = 30,                    /* LBRACE  */
  YYSYMBOL_IDENT = 31,                     /* IDENT  */
  YYSYMBOL_CARD_CONST = 32,                /* CARD_CONST  */
  YYSYMBOL_REAL_CONST = 33,                /* REAL_CONST  */
  YYSYMBOL_CHAR_CONST = 34,                /* CHAR_CONST  */
  YYSYMBOL_STR_CONST = 35,                 /* STR_CONST  */
  YYSYMBOL_PR_EXTERNAL = 36,               /* PR_EXTERNAL  */
  YYSYMBOL_PR_INLINE = 37,                 /* PR_INLINE  */
  YYSYMBOL_PR_OBSOLETE = 38,               /* PR_OBSOLETE  */
  YYSYMBOL_PR_UNUSED = 39,                 /* PR_UNUSED  */
  YYSYMBOL_PR_FATAL = 40,                  /* PR_FATAL  */
  YYSYMBOL_PR_NOWARN = 41,                 /* PR_NOWARN  */
  YYSYMBOL_PR_ASSERT = 42,                 /* PR_ASSERT  */
  YYSYMBOL_PR_TRACE = 43,                  /* PR_TRACE  */
  YYSYMBOL_PR_LINE = 44,                   /* PR_LINE  */
  YYSYMBOL_PR_PRAGMA = 45,                 /* PR_PRAGMA  */
  YYSYMBOL_PR_CALLBACK = 46,               /* PR_CALLBACK  */
  YYSYMBOL_PR_LL = 47,                     /* PR_LL  */
  YYSYMBOL_PR_LLsup = 48,                  /* PR_LLsup  */
  YYSYMBOL_PR_EXPORTED = 49,               /* PR_EXPORTED  */
  YYSYMBOL_PR_SPEC = 50,                   /* PR_SPEC  */
  YYSYMBOL_PR_LOOPINV = 51,                /* PR_LOOPINV  */
  YYSYMBOL_IDENTPRIME = 52,                /* IDENTPRIME  */
  YYSYMBOL_UPARROWPRIME = 53,              /* UPARROWPRIME  */
  YYSYMBOL_ALL = 54,                       /* ALL  */
  YYSYMBOL_AXIOM = 55,                     /* AXIOM  */
  YYSYMBOL_DEPEND = 56,                    /* DEPEND  */
  YYSYMBOL_ENSURES = 57,                   /* ENSURES  */
  YYSYMBOL_EXISTS = 58,                    /* EXISTS  */
  YYSYMBOL_FUNC = 59,                      /* FUNC  */
  YYSYMBOL_IFF = 60,                       /* IFF  */
  YYSYMBOL_IMPLIES = 61,                   /* IMPLIES  */
  YYSYMBOL_INVARIANT = 62,                 /* INVARIANT  */
  YYSYMBOL_IS = 63,                        /* IS  */
  YYSYMBOL_LET = 64,                       /* LET  */
  YYSYMBOL_MAP = 65,                       /* MAP  */
  YYSYMBOL_MODIFIES = 66,                  /* MODIFIES  */
  YYSYMBOL_ON = 67,                        /* ON  */
  YYSYMBOL_PRED = 68,                      /* PRED  */
  YYSYMBOL_PROTECT = 69,                   /* PROTECT  */
  YYSYMBOL_ABSTRACT = 70,                  /* ABSTRACT  */
  YYSYMBOL_REQUIRES = 71,                  /* REQUIRES  */
  YYSYMBOL_AND = 72,                       /* AND  */
  YYSYMBOL_ANY = 73,                       /* ANY  */
  YYSYMBOL_ARRAY = 74,                     /* ARRAY  */
  YYSYMBOL_AS = 75,                        /* AS  */
  YYSYMBOL_BGN = 76,                       /* BGN  */
  YYSYMBOL_BITS = 77,                      /* BITS  */
  YYSYMBOL_BRANDED = 78,                   /* BRANDED  */
  YYSYMBOL_BY = 79,                        /* BY  */
  YYSYMBOL_CASE = 80,                      /* CASE  */
  YYSYMBOL_CONST = 81,                     /* CONST  */
  YYSYMBOL_DIV = 82,                       /* DIV  */
  YYSYMBOL_DO = 83,                        /* DO  */
  YYSYMBOL_ELSE = 84,                      /* ELSE  */
  YYSYMBOL_ELSIF = 85,                     /* ELSIF  */
  YYSYMBOL_END = 86,                       /* END  */
  YYSYMBOL_EVAL = 87,                      /* EVAL  */
  YYSYMBOL_EXCEPT = 88,                    /* EXCEPT  */
  YYSYMBOL_EXCEPTION = 89,                 /* EXCEPTION  */
  YYSYMBOL_EXIT = 90,                      /* EXIT  */
  YYSYMBOL_EXPORTS = 91,                   /* EXPORTS  */
  YYSYMBOL_FINALLY = 92,                   /* FINALLY  */
  YYSYMBOL_FOR = 93,                       /* FOR  */
  YYSYMBOL_FROM = 94,                      /* FROM  */
  YYSYMBOL_GENERIC = 95,                   /* GENERIC  */
  YYSYMBOL_IF = 96,                        /* IF  */
  YYSYMBOL_IMPORT = 97,                    /* IMPORT  */
  YYSYMBOL_IN = 98,                        /* IN  */
  YYSYMBOL_INTERFACE = 99,                 /* INTERFACE  */
  YYSYMBOL_LOCK = 100,                     /* LOCK  */
  YYSYMBOL_LOOP = 101,                     /* LOOP  */
  YYSYMBOL_METHODS = 102,                  /* METHODS  */
  YYSYMBOL_MOD = 103,                      /* MOD  */
  YYSYMBOL_MODULE = 104,                   /* MODULE  */
  YYSYMBOL_NOT = 105,                      /* NOT  */
  YYSYMBOL_OBJECT = 106,                   /* OBJECT  */
  YYSYMBOL_OF = 107,                       /* OF  */
  YYSYMBOL_OR = 108,                       /* OR  */
  YYSYMBOL_OVERRIDES = 109,                /* OVERRIDES  */
  YYSYMBOL_PROCEDURE = 110,                /* PROCEDURE  */
  YYSYMBOL_RAISE = 111,                    /* RAISE  */
  YYSYMBOL_RAISES = 112,                   /* RAISES  */
  YYSYMBOL_READONLY = 113,                 /* READONLY  */
  YYSYMBOL_RECORD = 114,                   /* RECORD  */
  YYSYMBOL_REF = 115,                      /* REF  */
  YYSYMBOL_REPEAT = 116,                   /* REPEAT  */
  YYSYMBOL_RETURN = 117,                   /* RETURN  */
  YYSYMBOL_REVEAL = 118,                   /* REVEAL  */
  YYSYMBOL_ROOT = 119,                     /* ROOT  */
  YYSYMBOL_SET = 120,                      /* SET  */
  YYSYMBOL_THEN = 121,                     /* THEN  */
  YYSYMBOL_TO = 122,                       /* TO  */
  YYSYMBOL_TRY = 123,                      /* TRY  */
  YYSYMBOL_TYPE = 124,                     /* TYPE  */
  YYSYMBOL_TYPECASE = 125,                 /* TYPECASE  */
  YYSYMBOL_UNSAFE = 126,                   /* UNSAFE  */
  YYSYMBOL_UNTIL = 127,                    /* UNTIL  */
  YYSYMBOL_UNTRACED = 128,                 /* UNTRACED  */
  YYSYMBOL_VALUE = 129,                    /* VALUE  */
  YYSYMBOL_VAR = 130,                      /* VAR  */
  YYSYMBOL_WHILE = 131,                    /* WHILE  */
  YYSYMBOL_WITH = 132,                     /* WITH  */
  YYSYMBOL_BAD = 133,                      /* BAD  */
  YYSYMBOL_WHITESPACE = 134,               /* WHITESPACE  */
  YYSYMBOL_MODUNIT = 135,                  /* MODUNIT  */
  YYSYMBOL_DEFUNIT = 136,                  /* DEFUNIT  */
  YYSYMBOL_YYACCEPT = 137,                 /* $accept  */
  YYSYMBOL_FormattingUnit = 138,           /* FormattingUnit  */
  YYSYMBOL_139_1 = 139,                    /* $@1  */
  YYSYMBOL_140_2 = 140,                    /* $@2  */
  YYSYMBOL_141_3 = 141,                    /* $@3  */
  YYSYMBOL_InitialBlankLines = 142,        /* InitialBlankLines  */
  YYSYMBOL_ModUnit_list = 143,             /* ModUnit_list  */
  YYSYMBOL_ModUnit = 144,                  /* ModUnit  */
  YYSYMBOL_DefUnit_list = 145,             /* DefUnit_list  */
  YYSYMBOL_DefUnit = 146,                  /* DefUnit  */
  YYSYMBOL_CompilationUnit = 147,          /* CompilationUnit  */
  YYSYMBOL_interface = 148,                /* interface  */
  YYSYMBOL_module = 149,                   /* module  */
  YYSYMBOL_generic_interface = 150,        /* generic_interface  */
  YYSYMBOL_generic_module = 151,           /* generic_module  */
  YYSYMBOL_generic_params = 152,           /* generic_params  */
  YYSYMBOL_exports = 153,                  /* exports  */
  YYSYMBOL_import_nl_list = 154,           /* import_nl_list  */
  YYSYMBOL_import_nl = 155,                /* import_nl  */
  YYSYMBOL_import_module_list = 156,       /* import_module_list  */
  YYSYMBOL_import_module = 157,            /* import_module  */
  YYSYMBOL_block = 158,                    /* block  */
  YYSYMBOL_named_block = 159,              /* named_block  */
  YYSYMBOL_declaration_nl_list = 160,      /* declaration_nl_list  */
  YYSYMBOL_declaration_nl = 161,           /* declaration_nl  */
  YYSYMBOL_decl_pragma = 162,              /* decl_pragma  */
  YYSYMBOL_const_decl_list = 163,          /* const_decl_list  */
  YYSYMBOL_const_decl = 164,               /* const_decl  */
  YYSYMBOL_type_decl_list = 165,           /* type_decl_list  */
  YYSYMBOL_type_decl = 166,                /* type_decl  */
  YYSYMBOL_var_decl_list = 167,            /* var_decl_list  */
  YYSYMBOL_var_decl = 168,                 /* var_decl  */
  YYSYMBOL_exception_decl_list = 169,      /* exception_decl_list  */
  YYSYMBOL_exception_decl = 170,           /* exception_decl  */
  YYSYMBOL_procedure_head = 171,           /* procedure_head  */
  YYSYMBOL_signature = 172,                /* signature  */
  YYSYMBOL_return_type = 173,              /* return_type  */
  YYSYMBOL_raises = 174,                   /* raises  */
  YYSYMBOL_formals = 175,                  /* formals  */
  YYSYMBOL_formal_semi_list = 176,         /* formal_semi_list  */
  YYSYMBOL_formal_semi = 177,              /* formal_semi  */
  YYSYMBOL_formal = 178,                   /* formal  */
  YYSYMBOL_formal_pragma = 179,            /* formal_pragma  */
  YYSYMBOL_mode = 180,                     /* mode  */
  YYSYMBOL_type_and_or_val_semi = 181,     /* type_and_or_val_semi  */
  YYSYMBOL_type_and_or_val = 182,          /* type_and_or_val  */
  YYSYMBOL_stmts = 183,                    /* stmts  */
  YYSYMBOL_stmts_group = 184,              /* stmts_group  */
  YYSYMBOL_stmts1 = 185,                   /* stmts1  */
  YYSYMBOL_stmt_list = 186,                /* stmt_list  */
  YYSYMBOL_stmt_inner_list = 187,          /* stmt_inner_list  */
  YYSYMBOL_stmt_inner = 188,               /* stmt_inner  */
  YYSYMBOL_stmt_end = 189,                 /* stmt_end  */
  YYSYMBOL_stmt = 190,                     /* stmt  */
  YYSYMBOL_stmt_pragma = 191,              /* stmt_pragma  */
  YYSYMBOL_assignment_stmt = 192,          /* assignment_stmt  */
  YYSYMBOL_call_stmt = 193,                /* call_stmt  */
  YYSYMBOL_case_stmt = 194,                /* case_stmt  */
  YYSYMBOL_case_list = 195,                /* case_list  */
  YYSYMBOL_case = 196,                     /* case  */
  YYSYMBOL_labels_list = 197,              /* labels_list  */
  YYSYMBOL_labels = 198,                   /* labels  */
  YYSYMBOL_exit_stmt = 199,                /* exit_stmt  */
  YYSYMBOL_eval_stmt = 200,                /* eval_stmt  */
  YYSYMBOL_for_stmt = 201,                 /* for_stmt  */
  YYSYMBOL_by = 202,                       /* by  */
  YYSYMBOL_if_stmt = 203,                  /* if_stmt  */
  YYSYMBOL_else = 204,                     /* else  */
  YYSYMBOL_elsif_list = 205,               /* elsif_list  */
  YYSYMBOL_elsif = 206,                    /* elsif  */
  YYSYMBOL_lock_stmt = 207,                /* lock_stmt  */
  YYSYMBOL_loop_stmt = 208,                /* loop_stmt  */
  YYSYMBOL_raise_stmt = 209,               /* raise_stmt  */
  YYSYMBOL_repeat_stmt = 210,              /* repeat_stmt  */
  YYSYMBOL_return_stmt = 211,              /* return_stmt  */
  YYSYMBOL_try_finally_stmt = 212,         /* try_finally_stmt  */
  YYSYMBOL_try_stmt = 213,                 /* try_stmt  */
  YYSYMBOL_handler_list = 214,             /* handler_list  */
  YYSYMBOL_handler = 215,                  /* handler  */
  YYSYMBOL_typecase_stmt = 216,            /* typecase_stmt  */
  YYSYMBOL_tcase_list = 217,               /* tcase_list  */
  YYSYMBOL_tcase = 218,                    /* tcase  */
  YYSYMBOL_while_stmt = 219,               /* while_stmt  */
  YYSYMBOL_with_stmt = 220,                /* with_stmt  */
  YYSYMBOL_binding_list = 221,             /* binding_list  */
  YYSYMBOL_binding = 222,                  /* binding  */
  YYSYMBOL_opt_qid_list = 223,             /* opt_qid_list  */
  YYSYMBOL_qid_list = 224,                 /* qid_list  */
  YYSYMBOL_qid = 225,                      /* qid  */
  YYSYMBOL_type_list = 226,                /* type_list  */
  YYSYMBOL_type = 227,                     /* type  */
  YYSYMBOL_type_name = 228,                /* type_name  */
  YYSYMBOL_type_constructor = 229,         /* type_constructor  */
  YYSYMBOL_type_constructor1 = 230,        /* type_constructor1  */
  YYSYMBOL_root_type = 231,                /* root_type  */
  YYSYMBOL_type_constructor2 = 232,        /* type_constructor2  */
  YYSYMBOL_simple_object_type_list = 233,  /* simple_object_type_list  */
  YYSYMBOL_simple_object_type = 234,       /* simple_object_type  */
  YYSYMBOL_methods_part = 235,             /* methods_part  */
  YYSYMBOL_overrides_part = 236,           /* overrides_part  */
  YYSYMBOL_brand = 237,                    /* brand  */
  YYSYMBOL_fields = 238,                   /* fields  */
  YYSYMBOL_field_semi_list = 239,          /* field_semi_list  */
  YYSYMBOL_field_semi = 240,               /* field_semi  */
  YYSYMBOL_field = 241,                    /* field  */
  YYSYMBOL_methods = 242,                  /* methods  */
  YYSYMBOL_method_semi_list = 243,         /* method_semi_list  */
  YYSYMBOL_method_semi = 244,              /* method_semi  */
  YYSYMBOL_method = 245,                   /* method  */
  YYSYMBOL_overrides = 246,                /* overrides  */
  YYSYMBOL_override_semi_list = 247,       /* override_semi_list  */
  YYSYMBOL_override_semi = 248,            /* override_semi  */
  YYSYMBOL_override = 249,                 /* override  */
  YYSYMBOL_external_pragma = 250,          /* external_pragma  */
  YYSYMBOL_vtrace_pragma = 251,            /* vtrace_pragma  */
  YYSYMBOL_strace_pragma = 252,            /* strace_pragma  */
  YYSYMBOL_var_trace = 253,                /* var_trace  */
  YYSYMBOL_begin_trace = 254,              /* begin_trace  */
  YYSYMBOL_assert_pragma = 255,            /* assert_pragma  */
  YYSYMBOL_loopinv = 256,                  /* loopinv  */
  YYSYMBOL_loopinv_pragma = 257,           /* loopinv_pragma  */
  YYSYMBOL_fatal_pragma = 258,             /* fatal_pragma  */
  YYSYMBOL_fatal_exc_list = 259,           /* fatal_exc_list  */
  YYSYMBOL_inline_pragma = 260,            /* inline_pragma  */
  YYSYMBOL_unused_pragma = 261,            /* unused_pragma  */
  YYSYMBOL_obsolete_pragma = 262,          /* obsolete_pragma  */
  YYSYMBOL_callback_pragma = 263,          /* callback_pragma  */
  YYSYMBOL_exported_pragma = 264,          /* exported_pragma  */
  YYSYMBOL_anypragma = 265,                /* anypragma  */
  YYSYMBOL_pragma_pragma = 266,            /* pragma_pragma  */
  YYSYMBOL_nowarn_pragma = 267,            /* nowarn_pragma  */
  YYSYMBOL_ll_pragma = 268,                /* ll_pragma  */
  YYSYMBOL_ll_set = 269,                   /* ll_set  */
  YYSYMBOL_line_pragma = 270,              /* line_pragma  */
  YYSYMBOL_spec_pragma = 271,              /* spec_pragma  */
  YYSYMBOL_esc_spec = 272,                 /* esc_spec  */
  YYSYMBOL_spec_proc = 273,                /* spec_proc  */
  YYSYMBOL_spec_proc_opt_modifies = 274,   /* spec_proc_opt_modifies  */
  YYSYMBOL_spec_proc_opt_requires = 275,   /* spec_proc_opt_requires  */
  YYSYMBOL_spec_proc_opt_ensures = 276,    /* spec_proc_opt_ensures  */
  YYSYMBOL_spec_proc_signature = 277,      /* spec_proc_signature  */
  YYSYMBOL_spec_var = 278,                 /* spec_var  */
  YYSYMBOL_spec_depend = 279,              /* spec_depend  */
  YYSYMBOL_spec_abstract = 280,            /* spec_abstract  */
  YYSYMBOL_spec_abstract_lhs = 281,        /* spec_abstract_lhs  */
  YYSYMBOL_spec_opt_typed_id = 282,        /* spec_opt_typed_id  */
  YYSYMBOL_spec_pred_def = 283,            /* spec_pred_def  */
  YYSYMBOL_spec_func_def = 284,            /* spec_func_def  */
  YYSYMBOL_spec_axiom = 285,               /* spec_axiom  */
  YYSYMBOL_spec_protect = 286,             /* spec_protect  */
  YYSYMBOL_spec_inv = 287,                 /* spec_inv  */
  YYSYMBOL_spec_let = 288,                 /* spec_let  */
  YYSYMBOL_spec_term = 289,                /* spec_term  */
  YYSYMBOL_spec_pred = 290,                /* spec_pred  */
  YYSYMBOL_spec_quant = 291,               /* spec_quant  */
  YYSYMBOL_spec_zquant = 292,              /* spec_zquant  */
  YYSYMBOL_spec_concl = 293,               /* spec_concl  */
  YYSYMBOL_spec_zconcl = 294,              /* spec_zconcl  */
  YYSYMBOL_spec_weak_pred_op = 295,        /* spec_weak_pred_op  */
  YYSYMBOL_spec_disj = 296,                /* spec_disj  */
  YYSYMBOL_spec_zdisj = 297,               /* spec_zdisj  */
  YYSYMBOL_spec_conj = 298,                /* spec_conj  */
  YYSYMBOL_spec_zconj = 299,               /* spec_zconj  */
  YYSYMBOL_spec_literal = 300,             /* spec_literal  */
  YYSYMBOL_spec_zliteral = 301,            /* spec_zliteral  */
  YYSYMBOL_spec_atom = 302,                /* spec_atom  */
  YYSYMBOL_spec_term_sum = 303,            /* spec_term_sum  */
  YYSYMBOL_spec_zterm_sum = 304,           /* spec_zterm_sum  */
  YYSYMBOL_spec_addop = 305,               /* spec_addop  */
  YYSYMBOL_spec_term_prod = 306,           /* spec_term_prod  */
  YYSYMBOL_spec_zterm_prod = 307,          /* spec_zterm_prod  */
  YYSYMBOL_spec_mulop = 308,               /* spec_mulop  */
  YYSYMBOL_spec_term_selector = 309,       /* spec_term_selector  */
  YYSYMBOL_spec_term_paren = 310,          /* spec_term_paren  */
  YYSYMBOL_spec_term_list = 311,           /* spec_term_list  */
  YYSYMBOL_spec_prim_term = 312,           /* spec_prim_term  */
  YYSYMBOL_spec_typed_id_list = 313,       /* spec_typed_id_list  */
  YYSYMBOL_spec_typed_id = 314,            /* spec_typed_id  */
  YYSYMBOL_spec_type = 315,                /* spec_type  */
  YYSYMBOL_spec_bin_rel = 316,             /* spec_bin_rel  */
  YYSYMBOL_spec_proc_modifies = 317,       /* spec_proc_modifies  */
  YYSYMBOL_spec_sub_id_list = 318,         /* spec_sub_id_list  */
  YYSYMBOL_spec_sub_id = 319,              /* spec_sub_id  */
  YYSYMBOL_spec_term_bracket_list = 320,   /* spec_term_bracket_list  */
  YYSYMBOL_spec_proc_requires = 321,       /* spec_proc_requires  */
  YYSYMBOL_spec_proc_ensures = 322,        /* spec_proc_ensures  */
  YYSYMBOL_spec_except_spec = 323,         /* spec_except_spec  */
  YYSYMBOL_spec_except_spec_list = 324,    /* spec_except_spec_list  */
  YYSYMBOL_qqid = 325,                     /* qqid  */
  YYSYMBOL_qqid_list = 326,                /* qqid_list  */
  YYSYMBOL_qqidp = 327,                    /* qqidp  */
  YYSYMBOL_mixed_qqidp = 328,              /* mixed_qqidp  */
  YYSYMBOL_idp = 329,                      /* idp  */
  YYSYMBOL_expr = 330,                     /* expr  */
  YYSYMBOL_zexpr = 331,                    /* zexpr  */
  YYSYMBOL_e1 = 332,                       /* e1  */
  YYSYMBOL_ze1 = 333,                      /* ze1  */
  YYSYMBOL_e2 = 334,                       /* e2  */
  YYSYMBOL_e3 = 335,                       /* e3  */
  YYSYMBOL_ze3 = 336,                      /* ze3  */
  YYSYMBOL_relop = 337,                    /* relop  */
  YYSYMBOL_e4 = 338,                       /* e4  */
  YYSYMBOL_ze4 = 339,                      /* ze4  */
  YYSYMBOL_addop = 340,                    /* addop  */
  YYSYMBOL_e5 = 341,                       /* e5  */
  YYSYMBOL_ze5 = 342,                      /* ze5  */
  YYSYMBOL_mulop = 343,                    /* mulop  */
  YYSYMBOL_e6 = 344,                       /* e6  */
  YYSYMBOL_e7 = 345,                       /* e7  */
  YYSYMBOL_e8 = 346,                       /* e8  */
  YYSYMBOL_selector_list = 347,            /* selector_list  */
  YYSYMBOL_selector = 348,                 /* selector  */
  YYSYMBOL_expr_t = 349,                   /* expr_t  */
  YYSYMBOL_zexpr_t = 350,                  /* zexpr_t  */
  YYSYMBOL_e1_t = 351,                     /* e1_t  */
  YYSYMBOL_ze1_t = 352,                    /* ze1_t  */
  YYSYMBOL_e2_t = 353,                     /* e2_t  */
  YYSYMBOL_e3_t = 354,                     /* e3_t  */
  YYSYMBOL_ze3_t = 355,                    /* ze3_t  */
  YYSYMBOL_e4_t = 356,                     /* e4_t  */
  YYSYMBOL_ze4_t = 357,                    /* ze4_t  */
  YYSYMBOL_e5_t = 358,                     /* e5_t  */
  YYSYMBOL_ze5_t = 359,                    /* ze5_t  */
  YYSYMBOL_e6_t = 360,                     /* e6_t  */
  YYSYMBOL_e7_t = 361,                     /* e7_t  */
  YYSYMBOL_e8_t = 362,                     /* e8_t  */
  YYSYMBOL_selector_list_t = 363,          /* selector_list_t  */
  YYSYMBOL_selector_t = 364,               /* selector_t  */
  YYSYMBOL_cons_value = 365,               /* cons_value  */
  YYSYMBOL_Str_expr = 366,                 /* Str_expr  */
  YYSYMBOL_e4_s = 367,                     /* e4_s  */
  YYSYMBOL_ze4_s = 368,                    /* ze4_s  */
  YYSYMBOL_e7_s = 369,                     /* e7_s  */
  YYSYMBOL_e8_s = 370,                     /* e8_s  */
  YYSYMBOL_expr_list = 371,                /* expr_list  */
  YYSYMBOL_actual_list = 372,              /* actual_list  */
  YYSYMBOL_actual = 373,                   /* actual  */
  YYSYMBOL_elem_list = 374,                /* elem_list  */
  YYSYMBOL_elem = 375,                     /* elem  */
  YYSYMBOL_elem_tail = 376,                /* elem_tail  */
  YYSYMBOL_opt_id_list = 377,              /* opt_id_list  */
  YYSYMBOL_id_list = 378,                  /* id_list  */
  YYSYMBOL_Ampersand = 379,                /* Ampersand  */
  YYSYMBOL_380_4 = 380,                    /* $@4  */
  YYSYMBOL_Assign = 381,                   /* Assign  */
  YYSYMBOL_382_5 = 382,                    /* $@5  */
  YYSYMBOL_Asterisk = 383,                 /* Asterisk  */
  YYSYMBOL_384_6 = 384,                    /* $@6  */
  YYSYMBOL_Bar = 385,                      /* Bar  */
  YYSYMBOL_386_7 = 386,                    /* $@7  */
  YYSYMBOL_Colon = 387,                    /* Colon  */
  YYSYMBOL_388_8 = 388,                    /* $@8  */
  YYSYMBOL_Comma = 389,                    /* Comma  */
  YYSYMBOL_390_9 = 390,                    /* $@9  */
  YYSYMBOL_Dot = 391,                      /* Dot  */
  YYSYMBOL_392_10 = 392,                   /* $@10  */
  YYSYMBOL_Dotdot = 393,                   /* Dotdot  */
  YYSYMBOL_394_11 = 394,                   /* $@11  */
  YYSYMBOL_Equal = 395,                    /* Equal  */
  YYSYMBOL_396_12 = 396,                   /* $@12  */
  YYSYMBOL_Greater = 397,                  /* Greater  */
  YYSYMBOL_398_13 = 398,                   /* $@13  */
  YYSYMBOL_Grequal = 399,                  /* Grequal  */
  YYSYMBOL_400_14 = 400,                   /* $@14  */
  YYSYMBOL_Less = 401,                     /* Less  */
  YYSYMBOL_402_15 = 402,                   /* $@15  */
  YYSYMBOL_Lsequal = 403,                  /* Lsequal  */
  YYSYMBOL_404_16 = 404,                   /* $@16  */
  YYSYMBOL_Minus = 405,                    /* Minus  */
  YYSYMBOL_406_17 = 406,                   /* $@17  */
  YYSYMBOL_Notequal = 407,                 /* Notequal  */
  YYSYMBOL_408_18 = 408,                   /* $@18  */
  YYSYMBOL_Plus = 409,                     /* Plus  */
  YYSYMBOL_410_19 = 410,                   /* $@19  */
  YYSYMBOL_Rarrow = 411,                   /* Rarrow  */
  YYSYMBOL_412_20 = 412,                   /* $@20  */
  YYSYMBOL_Rbrace = 413,                   /* Rbrace  */
  YYSYMBOL_414_21 = 414,                   /* $@21  */
  YYSYMBOL_Rbracket = 415,                 /* Rbracket  */
  YYSYMBOL_416_22 = 416,                   /* $@22  */
  YYSYMBOL_Rparen = 417,                   /* Rparen  */
  YYSYMBOL_418_23 = 418,                   /* $@23  */
  YYSYMBOL_Rpragma = 419,                  /* Rpragma  */
  YYSYMBOL_420_24 = 420,                   /* $@24  */
  YYSYMBOL_Rpragma1 = 421,                 /* Rpragma1  */
  YYSYMBOL_Semi = 422,                     /* Semi  */
  YYSYMBOL_423_25 = 423,                   /* $@25  */
  YYSYMBOL_Semi1 = 424,                    /* Semi1  */
  YYSYMBOL_Slash = 425,                    /* Slash  */
  YYSYMBOL_426_26 = 426,                   /* $@26  */
  YYSYMBOL_Subtype = 427,                  /* Subtype  */
  YYSYMBOL_428_27 = 428,                   /* $@27  */
  YYSYMBOL_Uparrow = 429,                  /* Uparrow  */
  YYSYMBOL_430_28 = 430,                   /* $@28  */
  YYSYMBOL_UparrowPrime = 431,             /* UparrowPrime  */
  YYSYMBOL_432_29 = 432,                   /* $@29  */
  YYSYMBOL_Lparen = 433,                   /* Lparen  */
  YYSYMBOL_434_30 = 434,                   /* $@30  */
  YYSYMBOL_Lparen2 = 435,                  /* Lparen2  */
  YYSYMBOL_436_31 = 436,                   /* $@31  */
  YYSYMBOL_Lbracket = 437,                 /* Lbracket  */
  YYSYMBOL_438_32 = 438,                   /* $@32  */
  YYSYMBOL_Lbrace = 439,                   /* Lbrace  */
  YYSYMBOL_440_33 = 440,                   /* $@33  */
  YYSYMBOL_Pr_External = 441,              /* Pr_External  */
  YYSYMBOL_442_34 = 442,                   /* $@34  */
  YYSYMBOL_Pr_Inline = 443,                /* Pr_Inline  */
  YYSYMBOL_444_35 = 444,                   /* $@35  */
  YYSYMBOL_Pr_Assert = 445,                /* Pr_Assert  */
  YYSYMBOL_446_36 = 446,                   /* $@36  */
  YYSYMBOL_Pr_Trace = 447,                 /* Pr_Trace  */
  YYSYMBOL_448_37 = 448,                   /* $@37  */
  YYSYMBOL_Pr_Fatal = 449,                 /* Pr_Fatal  */
  YYSYMBOL_450_38 = 450,                   /* $@38  */
  YYSYMBOL_Pr_Unused = 451,                /* Pr_Unused  */
  YYSYMBOL_452_39 = 452,                   /* $@39  */
  YYSYMBOL_Pr_Obsolete = 453,              /* Pr_Obsolete  */
  YYSYMBOL_454_40 = 454,                   /* $@40  */
  YYSYMBOL_Pr_Callback = 455,              /* Pr_Callback  */
  YYSYMBOL_456_41 = 456,                   /* $@41  */
  YYSYMBOL_Pr_Exported = 457,              /* Pr_Exported  */
  YYSYMBOL_458_42 = 458,                   /* $@42  */
  YYSYMBOL_Pr_Pragma = 459,                /* Pr_Pragma  */
  YYSYMBOL_460_43 = 460,                   /* $@43  */
  YYSYMBOL_Pr_Nowarn = 461,                /* Pr_Nowarn  */
  YYSYMBOL_462_44 = 462,                   /* $@44  */
  YYSYMBOL_Pr_Line = 463,                  /* Pr_Line  */
  YYSYMBOL_464_45 = 464,                   /* $@45  */
  YYSYMBOL_Pr_LL = 465,                    /* Pr_LL  */
  YYSYMBOL_466_46 = 466,                   /* $@46  */
  YYSYMBOL_Pr_LLsup = 467,                 /* Pr_LLsup  */
  YYSYMBOL_468_47 = 468,                   /* $@47  */
  YYSYMBOL_Pr_Spec = 469,                  /* Pr_Spec  */
  YYSYMBOL_470_48 = 470,                   /* $@48  */
  YYSYMBOL_Pr_LoopInv = 471,               /* Pr_LoopInv  */
  YYSYMBOL_472_49 = 472,                   /* $@49  */
  YYSYMBOL_Ident = 473,                    /* Ident  */
  YYSYMBOL_474_50 = 474,                   /* $@50  */
  YYSYMBOL_IdentP = 475,                   /* IdentP  */
  YYSYMBOL_476_51 = 476,                   /* $@51  */
  YYSYMBOL_IdentPrime = 477,               /* IdentPrime  */
  YYSYMBOL_478_52 = 478,                   /* $@52  */
  YYSYMBOL_Card_const = 479,               /* Card_const  */
  YYSYMBOL_480_53 = 480,                   /* $@53  */
  YYSYMBOL_Real_const = 481,               /* Real_const  */
  YYSYMBOL_482_54 = 482,                   /* $@54  */
  YYSYMBOL_Char_const = 483,               /* Char_const  */
  YYSYMBOL_484_55 = 484,                   /* $@55  */
  YYSYMBOL_Str_const = 485,                /* Str_const  */
  YYSYMBOL_486_56 = 486,                   /* $@56  */
  YYSYMBOL_And = 487,                      /* And  */
  YYSYMBOL_488_57 = 488,                   /* $@57  */
  YYSYMBOL_Any = 489,                      /* Any  */
  YYSYMBOL_490_58 = 490,                   /* $@58  */
  YYSYMBOL_Array = 491,                    /* Array  */
  YYSYMBOL_492_59 = 492,                   /* $@59  */
  YYSYMBOL_As = 493,                       /* As  */
  YYSYMBOL_494_60 = 494,                   /* $@60  */
  YYSYMBOL_Begin = 495,                    /* Begin  */
  YYSYMBOL_496_61 = 496,                   /* $@61  */
  YYSYMBOL_Bits = 497,                     /* Bits  */
  YYSYMBOL_498_62 = 498,                   /* $@62  */
  YYSYMBOL_Branded = 499,                  /* Branded  */
  YYSYMBOL_500_63 = 500,                   /* $@63  */
  YYSYMBOL_By = 501,                       /* By  */
  YYSYMBOL_502_64 = 502,                   /* $@64  */
  YYSYMBOL_Case = 503,                     /* Case  */
  YYSYMBOL_504_65 = 504,                   /* $@65  */
  YYSYMBOL_Const = 505,                    /* Const  */
  YYSYMBOL_506_66 = 506,                   /* $@66  */
  YYSYMBOL_Div = 507,                      /* Div  */
  YYSYMBOL_508_67 = 508,                   /* $@67  */
  YYSYMBOL_Do = 509,                       /* Do  */
  YYSYMBOL_510_68 = 510,                   /* $@68  */
  YYSYMBOL_Else = 511,                     /* Else  */
  YYSYMBOL_512_69 = 512,                   /* $@69  */
  YYSYMBOL_Elsif = 513,                    /* Elsif  */
  YYSYMBOL_514_70 = 514,                   /* $@70  */
  YYSYMBOL_End = 515,                      /* End  */
  YYSYMBOL_516_71 = 516,                   /* $@71  */
  YYSYMBOL_Eval = 517,                     /* Eval  */
  YYSYMBOL_518_72 = 518,                   /* $@72  */
  YYSYMBOL_Except = 519,                   /* Except  */
  YYSYMBOL_520_73 = 520,                   /* $@73  */
  YYSYMBOL_Exception = 521,                /* Exception  */
  YYSYMBOL_522_74 = 522,                   /* $@74  */
  YYSYMBOL_Exit = 523,                     /* Exit  */
  YYSYMBOL_524_75 = 524,                   /* $@75  */
  YYSYMBOL_Exports = 525,                  /* Exports  */
  YYSYMBOL_526_76 = 526,                   /* $@76  */
  YYSYMBOL_Finally = 527,                  /* Finally  */
  YYSYMBOL_528_77 = 528,                   /* $@77  */
  YYSYMBOL_For = 529,                      /* For  */
  YYSYMBOL_530_78 = 530,                   /* $@78  */
  YYSYMBOL_From = 531,                     /* From  */
  YYSYMBOL_532_79 = 532,                   /* $@79  */
  YYSYMBOL_Generic = 533,                  /* Generic  */
  YYSYMBOL_534_80 = 534,                   /* $@80  */
  YYSYMBOL_If = 535,                       /* If  */
  YYSYMBOL_536_81 = 536,                   /* $@81  */
  YYSYMBOL_Import = 537,                   /* Import  */
  YYSYMBOL_538_82 = 538,                   /* $@82  */
  YYSYMBOL_In = 539,                       /* In  */
  YYSYMBOL_540_83 = 540,                   /* $@83  */
  YYSYMBOL_Interface = 541,                /* Interface  */
  YYSYMBOL_542_84 = 542,                   /* $@84  */
  YYSYMBOL_Lock = 543,                     /* Lock  */
  YYSYMBOL_544_85 = 544,                   /* $@85  */
  YYSYMBOL_Loop = 545,                     /* Loop  */
  YYSYMBOL_546_86 = 546,                   /* $@86  */
  YYSYMBOL_Methods = 547,                  /* Methods  */
  YYSYMBOL_548_87 = 548,                   /* $@87  */
  YYSYMBOL_Mod = 549,                      /* Mod  */
  YYSYMBOL_550_88 = 550,                   /* $@88  */
  YYSYMBOL_Module = 551,                   /* Module  */
  YYSYMBOL_552_89 = 552,                   /* $@89  */
  YYSYMBOL_Not = 553,                      /* Not  */
  YYSYMBOL_554_90 = 554,                   /* $@90  */
  YYSYMBOL_Object = 555,                   /* Object  */
  YYSYMBOL_556_91 = 556,                   /* $@91  */
  YYSYMBOL_Of = 557,                       /* Of  */
  YYSYMBOL_558_92 = 558,                   /* $@92  */
  YYSYMBOL_Or = 559,                       /* Or  */
  YYSYMBOL_560_93 = 560,                   /* $@93  */
  YYSYMBOL_Overrides = 561,                /* Overrides  */
  YYSYMBOL_562_94 = 562,                   /* $@94  */
  YYSYMBOL_Procedure = 563,                /* Procedure  */
  YYSYMBOL_564_95 = 564,                   /* $@95  */
  YYSYMBOL_Raise = 565,                    /* Raise  */
  YYSYMBOL_566_96 = 566,                   /* $@96  */
  YYSYMBOL_Raises = 567,                   /* Raises  */
  YYSYMBOL_568_97 = 568,                   /* $@97  */
  YYSYMBOL_Readonly = 569,                 /* Readonly  */
  YYSYMBOL_570_98 = 570,                   /* $@98  */
  YYSYMBOL_Record = 571,                   /* Record  */
  YYSYMBOL_572_99 = 572,                   /* $@99  */
  YYSYMBOL_Ref = 573,                      /* Ref  */
  YYSYMBOL_574_100 = 574,                  /* $@100  */
  YYSYMBOL_Repeat = 575,                   /* Repeat  */
  YYSYMBOL_576_101 = 576,                  /* $@101  */
  YYSYMBOL_Return = 577,                   /* Return  */
  YYSYMBOL_578_102 = 578,                  /* $@102  */
  YYSYMBOL_Reveal = 579,                   /* Reveal  */
  YYSYMBOL_580_103 = 580,                  /* $@103  */
  YYSYMBOL_Root = 581,                     /* Root  */
  YYSYMBOL_582_104 = 582,                  /* $@104  */
  YYSYMBOL_Set = 583,                      /* Set  */
  YYSYMBOL_584_105 = 584,                  /* $@105  */
  YYSYMBOL_Then = 585,                     /* Then  */
  YYSYMBOL_586_106 = 586,                  /* $@106  */
  YYSYMBOL_To = 587,                       /* To  */
  YYSYMBOL_588_107 = 588,                  /* $@107  */
  YYSYMBOL_Try = 589,                      /* Try  */
  YYSYMBOL_590_108 = 590,                  /* $@108  */
  YYSYMBOL_Type = 591,                     /* Type  */
  YYSYMBOL_592_109 = 592,                  /* $@109  */
  YYSYMBOL_Typecase = 593,                 /* Typecase  */
  YYSYMBOL_594_110 = 594,                  /* $@110  */
  YYSYMBOL_Unsafe = 595,                   /* Unsafe  */
  YYSYMBOL_596_111 = 596,                  /* $@111  */
  YYSYMBOL_Until = 597,                    /* Until  */
  YYSYMBOL_598_112 = 598,                  /* $@112  */
  YYSYMBOL_Untraced = 599,                 /* Untraced  */
  YYSYMBOL_600_113 = 600,                  /* $@113  */
  YYSYMBOL_Value = 601,                    /* Value  */
  YYSYMBOL_602_114 = 602,                  /* $@114  */
  YYSYMBOL_Var = 603,                      /* Var  */
  YYSYMBOL_604_115 = 604,                  /* $@115  */
  YYSYMBOL_While = 605,                    /* While  */
  YYSYMBOL_606_116 = 606,                  /* $@116  */
  YYSYMBOL_With = 607,                     /* With  */
  YYSYMBOL_608_117 = 608,                  /* $@117  */
  YYSYMBOL_Abstract = 609,                 /* Abstract  */
  YYSYMBOL_610_118 = 610,                  /* $@118  */
  YYSYMBOL_All = 611,                      /* All  */
  YYSYMBOL_612_119 = 612,                  /* $@119  */
  YYSYMBOL_Axiom = 613,                    /* Axiom  */
  YYSYMBOL_614_120 = 614,                  /* $@120  */
  YYSYMBOL_Depend = 615,                   /* Depend  */
  YYSYMBOL_616_121 = 616,                  /* $@121  */
  YYSYMBOL_Ensures = 617,                  /* Ensures  */
  YYSYMBOL_618_122 = 618,                  /* $@122  */
  YYSYMBOL_Exists = 619,                   /* Exists  */
  YYSYMBOL_620_123 = 620,                  /* $@123  */
  YYSYMBOL_Func = 621,                     /* Func  */
  YYSYMBOL_622_124 = 622,                  /* $@124  */
  YYSYMBOL_Iff = 623,                      /* Iff  */
  YYSYMBOL_624_125 = 624,                  /* $@125  */
  YYSYMBOL_Implies = 625,                  /* Implies  */
  YYSYMBOL_626_126 = 626,                  /* $@126  */
  YYSYMBOL_Invariant = 627,                /* Invariant  */
  YYSYMBOL_628_127 = 628,                  /* $@127  */
  YYSYMBOL_Is = 629,                       /* Is  */
  YYSYMBOL_630_128 = 630,                  /* $@128  */
  YYSYMBOL_Let = 631,                      /* Let  */
  YYSYMBOL_632_129 = 632,                  /* $@129  */
  YYSYMBOL_Map = 633,                      /* Map  */
  YYSYMBOL_634_130 = 634,                  /* $@130  */
  YYSYMBOL_Modifies = 635,                 /* Modifies  */
  YYSYMBOL_636_131 = 636,                  /* $@131  */
  YYSYMBOL_Pred = 637,                     /* Pred  */
  YYSYMBOL_638_132 = 638,                  /* $@132  */
  YYSYMBOL_Protect = 639,                  /* Protect  */
  YYSYMBOL_640_133 = 640,                  /* $@133  */
  YYSYMBOL_Requires = 641,                 /* Requires  */
  YYSYMBOL_642_134 = 642,                  /* $@134  */
  YYSYMBOL_InitialNPS = 643,               /* InitialNPS  */
  YYSYMBOL_644_135 = 644,                  /* $@135  */
  YYSYMBOL_NPS = 645,                      /* NPS  */
  YYSYMBOL_space_anypragma_list = 646,     /* space_anypragma_list  */
  YYSYMBOL_anypragma_space_list = 647,     /* anypragma_space_list  */
  YYSYMBOL_anypragma_list = 648,           /* anypragma_list  */
  YYSYMBOL_space_list_emit = 649,          /* space_list_emit  */
  YYSYMBOL_space_list = 650,               /* space_list  */
  YYSYMBOL_G = 651,                        /* G  */
  YYSYMBOL_B0 = 652,                       /* B0  */
  YYSYMBOL_B = 653,                        /* B  */
  YYSYMBOL_B2 = 654,                       /* B2  */
  YYSYMBOL_E = 655,                        /* E  */
  YYSYMBOL_EF = 656,                       /* EF  */
  YYSYMBOL_A = 657,                        /* A  */
  YYSYMBOL_AO = 658,                       /* AO  */
  YYSYMBOL_AX = 659,                       /* AX  */
  YYSYMBOL_V = 660,                        /* V  */
  YYSYMBOL_VZ = 661,                       /* VZ  */
  YYSYMBOL_VC = 662,                       /* VC  */
  YYSYMBOL_Z = 663,                        /* Z  */
  YYSYMBOL_SP = 664,                       /* SP  */
  YYSYMBOL_XSP = 665,                      /* XSP  */
  YYSYMBOL_BL = 666,                       /* BL  */
  YYSYMBOL_AL2 = 667,                      /* AL2  */
  YYSYMBOL_AL3 = 668,                      /* AL3  */
  YYSYMBOL_ALZ5 = 669,                     /* ALZ5  */
  YYSYMBOL_EA = 670,                       /* EA  */
  YYSYMBOL_ALNL = 671,                     /* ALNL  */
  YYSYMBOL_SPNL = 672,                     /* SPNL  */
  YYSYMBOL_QSP = 673,                      /* QSP  */
  YYSYMBOL_NL = 674,                       /* NL  */
  YYSYMBOL_Inc = 675,                      /* Inc  */
  YYSYMBOL_Dec = 676                       /* Dec  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_int16 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  5
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   6206

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  137
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  540
/* YYNRULES -- Number of rules.  */
#define YYNRULES  841
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2126

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   391


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,   134,
     135,   136
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   333,   333,   333,   334,   334,   335,   335,   338,   340,
     344,   345,   349,   350,   351,   352,   356,   357,   361,   362,
     363,   367,   368,   373,   374,   375,   376,   380,   384,   389,
     392,   397,   404,   410,   412,   415,   417,   420,   422,   426,
     427,   431,   432,   436,   437,   441,   445,   448,   450,   456,
     457,   458,   459,   460,   461,   462,   463,   464,   465,   466,
     467,   470,   472,   473,   474,   475,   476,   477,   478,   482,
     483,   488,   489,   493,   494,   498,   499,   503,   504,   508,
     511,   514,   521,   522,   527,   528,   532,   536,   539,   541,
     544,   546,   547,   550,   552,   553,   554,   558,   559,   563,
     567,   570,   572,   575,   577,   578,   579,   583,   584,   585,
     589,   590,   591,   596,   598,   602,   604,   609,   613,   616,
     618,   622,   623,   627,   628,   629,   633,   634,   635,   636,
     637,   638,   639,   640,   641,   642,   643,   644,   645,   646,
     647,   648,   649,   650,   654,   659,   663,   667,   668,   671,
     673,   677,   681,   682,   686,   687,   691,   696,   702,   707,
     708,   712,   715,   717,   720,   722,   726,   730,   734,   738,
     739,   743,   747,   748,   752,   756,   757,   760,   762,   766,
     767,   771,   772,   775,   777,   781,   782,   786,   790,   794,
     795,   799,   802,   804,   808,   809,   813,   814,   820,   821,
     825,   826,   827,   828,   829,   833,   837,   838,   842,   843,
     844,   845,   846,   847,   848,   849,   850,   851,   855,   856,
     861,   862,   863,   864,   868,   869,   873,   876,   878,   881,
     883,   886,   888,   889,   892,   894,   895,   896,   900,   901,
     905,   908,   911,   913,   914,   915,   919,   920,   924,   925,
     926,   930,   931,   932,   935,   937,   938,   939,   943,   944,
     948,   952,   958,   959,   960,   961,   964,   965,   967,   969,
     973,   974,   978,   979,   982,   984,   987,   991,   994,   995,
     998,   999,  1000,  1001,  1002,  1008,  1009,  1010,  1011,  1012,
    1019,  1020,  1022,  1023,  1027,  1028,  1029,  1033,  1034,  1037,
    1043,  1044,  1045,  1046,  1047,  1048,  1049,  1050,  1051,  1052,
    1056,  1062,  1064,  1067,  1069,  1072,  1074,  1078,  1079,  1080,
    1081,  1084,  1087,  1090,  1091,  1094,  1096,  1098,  1101,  1104,
    1105,  1108,  1110,  1112,  1114,  1119,  1121,  1123,  1125,  1126,
    1127,  1130,  1132,  1133,  1136,  1136,  1138,  1140,  1141,  1144,
    1146,  1147,  1150,  1152,  1153,  1157,  1158,  1161,  1163,  1164,
    1166,  1166,  1168,  1170,  1171,  1173,  1173,  1173,  1176,  1177,
    1178,  1179,  1180,  1181,  1185,  1186,  1190,  1191,  1195,  1196,
    1202,  1203,  1206,  1209,  1210,  1211,  1214,  1214,  1214,  1214,
    1214,  1214,  1216,  1219,  1220,  1223,  1225,  1227,  1229,  1232,
    1233,  1237,  1238,  1242,  1243,  1247,  1248,  1252,  1253,  1257,
    1258,  1259,  1262,  1264,  1277,  1277,  1282,  1283,  1283,  1285,
    1286,  1286,  1288,  1288,  1290,  1291,  1291,  1292,  1292,  1292,
    1292,  1292,  1292,  1292,  1294,  1295,  1295,  1296,  1296,  1296,
    1298,  1299,  1299,  1300,  1300,  1300,  1300,  1302,  1302,  1302,
    1305,  1307,  1307,  1307,  1307,  1307,  1308,  1308,  1310,  1312,
    1316,  1317,  1320,  1321,  1322,  1323,  1324,  1330,  1331,  1331,
    1333,  1334,  1334,  1336,  1336,  1338,  1339,  1339,  1341,  1342,
    1342,  1344,  1345,  1345,  1347,  1347,  1347,  1349,  1350,  1351,
    1352,  1353,  1354,  1355,  1356,  1357,  1360,  1360,  1360,  1360,
    1363,  1364,  1368,  1369,  1372,  1373,  1374,  1375,  1376,  1380,
    1381,  1388,  1390,  1391,  1391,  1393,  1395,  1396,  1397,  1398,
    1404,  1405,  1409,  1410,  1414,  1415,  1419,  1420,  1424,  1425,
    1426,  1429,  1431,  1434,  1436,  1440,  1441,  1447,  1447,  1448,
    1448,  1449,  1449,  1450,  1450,  1451,  1451,  1452,  1452,  1453,
    1453,  1454,  1454,  1455,  1455,  1456,  1456,  1457,  1457,  1458,
    1458,  1459,  1459,  1460,  1460,  1461,  1461,  1462,  1462,  1463,
    1463,  1464,  1464,  1465,  1465,  1466,  1466,  1467,  1467,  1468,
    1469,  1469,  1470,  1471,  1471,  1472,  1472,  1473,  1473,  1474,
    1474,  1477,  1477,  1478,  1478,  1479,  1479,  1480,  1480,  1486,
    1486,  1487,  1487,  1488,  1488,  1489,  1489,  1490,  1490,  1491,
    1491,  1492,  1492,  1493,  1493,  1494,  1494,  1496,  1496,  1497,
    1497,  1498,  1498,  1499,  1499,  1500,  1500,  1501,  1501,  1502,
    1502,  1504,  1504,  1505,  1505,  1506,  1506,  1507,  1507,  1508,
    1508,  1509,  1509,  1510,  1510,  1512,  1512,  1513,  1513,  1514,
    1514,  1515,  1515,  1516,  1516,  1517,  1517,  1518,  1518,  1519,
    1519,  1520,  1520,  1521,  1521,  1522,  1522,  1523,  1523,  1524,
    1524,  1525,  1525,  1526,  1526,  1527,  1527,  1528,  1528,  1529,
    1529,  1530,  1530,  1531,  1531,  1532,  1532,  1533,  1533,  1534,
    1534,  1535,  1535,  1536,  1536,  1537,  1537,  1538,  1538,  1539,
    1539,  1540,  1540,  1541,  1541,  1542,  1542,  1543,  1543,  1544,
    1544,  1545,  1545,  1546,  1546,  1547,  1547,  1548,  1548,  1549,
    1549,  1550,  1550,  1551,  1551,  1554,  1554,  1555,  1555,  1556,
    1556,  1557,  1557,  1558,  1558,  1559,  1559,  1560,  1560,  1561,
    1561,  1562,  1562,  1563,  1563,  1564,  1564,  1565,  1565,  1566,
    1566,  1567,  1567,  1568,  1568,  1569,  1569,  1570,  1570,  1571,
    1571,  1572,  1572,  1573,  1573,  1574,  1574,  1577,  1577,  1578,
    1578,  1579,  1579,  1580,  1580,  1581,  1581,  1582,  1582,  1583,
    1583,  1584,  1584,  1585,  1585,  1586,  1586,  1587,  1587,  1588,
    1588,  1589,  1589,  1590,  1590,  1591,  1591,  1592,  1592,  1593,
    1593,  1611,  1612,  1612,  1616,  1617,  1618,  1625,  1626,  1630,
    1631,  1635,  1636,  1640,  1645,  1646,  1651,  1652,  1653,  1654,
    1655,  1656,  1658,  1659,  1660,  1662,  1663,  1664,  1666,  1667,
    1668,  1670,  1672,  1673,  1674,  1675,  1677,  1679,  1680,  1681,
    1683,  1684
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "ENDOFFILE", "error", "\"invalid token\"", "AMPERSAND", "ASSIGN",
  "ASTERISK", "BAR", "COLON", "COMMA", "DOT", "DOTDOT", "EQUAL", "GREATER",
  "GREQUAL", "LESS", "LSEQUAL", "MINUS", "SHARP", "PLUS", "RARROW",
  "RPRAGMA", "RBRACE", "RBRACKET", "RPAREN", "SEMICOLON", "SLASH",
  "SUBTYPE", "UPARROW", "LPAREN", "LBRACKET", "LBRACE", "IDENT",
  "CARD_CONST", "REAL_CONST", "CHAR_CONST", "STR_CONST", "PR_EXTERNAL",
  "PR_INLINE", "PR_OBSOLETE", "PR_UNUSED", "PR_FATAL", "PR_NOWARN",
  "PR_ASSERT", "PR_TRACE", "PR_LINE", "PR_PRAGMA", "PR_CALLBACK", "PR_LL",
  "PR_LLsup", "PR_EXPORTED", "PR_SPEC", "PR_LOOPINV", "IDENTPRIME",
  "UPARROWPRIME", "ALL", "AXIOM", "DEPEND", "ENSURES", "EXISTS", "FUNC",
  "IFF", "IMPLIES", "INVARIANT", "IS", "LET", "MAP", "MODIFIES", "ON",
  "PRED", "PROTECT", "ABSTRACT", "REQUIRES", "AND", "ANY", "ARRAY", "AS",
  "BGN", "BITS", "BRANDED", "BY", "CASE", "CONST", "DIV", "DO", "ELSE",
  "ELSIF", "END", "EVAL", "EXCEPT", "EXCEPTION", "EXIT", "EXPORTS",
  "FINALLY", "FOR", "FROM", "GENERIC", "IF", "IMPORT", "IN", "INTERFACE",
  "LOCK", "LOOP", "METHODS", "MOD", "MODULE", "NOT", "OBJECT", "OF", "OR",
  "OVERRIDES", "PROCEDURE", "RAISE", "RAISES", "READONLY", "RECORD", "REF",
  "REPEAT", "RETURN", "REVEAL", "ROOT", "SET", "THEN", "TO", "TRY", "TYPE",
  "TYPECASE", "UNSAFE", "UNTIL", "UNTRACED", "VALUE", "VAR", "WHILE",
  "WITH", "BAD", "WHITESPACE", "MODUNIT", "DEFUNIT", "$accept",
  "FormattingUnit", "$@1", "$@2", "$@3", "InitialBlankLines",
  "ModUnit_list", "ModUnit", "DefUnit_list", "DefUnit", "CompilationUnit",
  "interface", "module", "generic_interface", "generic_module",
  "generic_params", "exports", "import_nl_list", "import_nl",
  "import_module_list", "import_module", "block", "named_block",
  "declaration_nl_list", "declaration_nl", "decl_pragma",
  "const_decl_list", "const_decl", "type_decl_list", "type_decl",
  "var_decl_list", "var_decl", "exception_decl_list", "exception_decl",
  "procedure_head", "signature", "return_type", "raises", "formals",
  "formal_semi_list", "formal_semi", "formal", "formal_pragma", "mode",
  "type_and_or_val_semi", "type_and_or_val", "stmts", "stmts_group",
  "stmts1", "stmt_list", "stmt_inner_list", "stmt_inner", "stmt_end",
  "stmt", "stmt_pragma", "assignment_stmt", "call_stmt", "case_stmt",
  "case_list", "case", "labels_list", "labels", "exit_stmt", "eval_stmt",
  "for_stmt", "by", "if_stmt", "else", "elsif_list", "elsif", "lock_stmt",
  "loop_stmt", "raise_stmt", "repeat_stmt", "return_stmt",
  "try_finally_stmt", "try_stmt", "handler_list", "handler",
  "typecase_stmt", "tcase_list", "tcase", "while_stmt", "with_stmt",
  "binding_list", "binding", "opt_qid_list", "qid_list", "qid",
  "type_list", "type", "type_name", "type_constructor",
  "type_constructor1", "root_type", "type_constructor2",
  "simple_object_type_list", "simple_object_type", "methods_part",
  "overrides_part", "brand", "fields", "field_semi_list", "field_semi",
  "field", "methods", "method_semi_list", "method_semi", "method",
  "overrides", "override_semi_list", "override_semi", "override",
  "external_pragma", "vtrace_pragma", "strace_pragma", "var_trace",
  "begin_trace", "assert_pragma", "loopinv", "loopinv_pragma",
  "fatal_pragma", "fatal_exc_list", "inline_pragma", "unused_pragma",
  "obsolete_pragma", "callback_pragma", "exported_pragma", "anypragma",
  "pragma_pragma", "nowarn_pragma", "ll_pragma", "ll_set", "line_pragma",
  "spec_pragma", "esc_spec", "spec_proc", "spec_proc_opt_modifies",
  "spec_proc_opt_requires", "spec_proc_opt_ensures", "spec_proc_signature",
  "spec_var", "spec_depend", "spec_abstract", "spec_abstract_lhs",
  "spec_opt_typed_id", "spec_pred_def", "spec_func_def", "spec_axiom",
  "spec_protect", "spec_inv", "spec_let", "spec_term", "spec_pred",
  "spec_quant", "spec_zquant", "spec_concl", "spec_zconcl",
  "spec_weak_pred_op", "spec_disj", "spec_zdisj", "spec_conj",
  "spec_zconj", "spec_literal", "spec_zliteral", "spec_atom",
  "spec_term_sum", "spec_zterm_sum", "spec_addop", "spec_term_prod",
  "spec_zterm_prod", "spec_mulop", "spec_term_selector", "spec_term_paren",
  "spec_term_list", "spec_prim_term", "spec_typed_id_list",
  "spec_typed_id", "spec_type", "spec_bin_rel", "spec_proc_modifies",
  "spec_sub_id_list", "spec_sub_id", "spec_term_bracket_list",
  "spec_proc_requires", "spec_proc_ensures", "spec_except_spec",
  "spec_except_spec_list", "qqid", "qqid_list", "qqidp", "mixed_qqidp",
  "idp", "expr", "zexpr", "e1", "ze1", "e2", "e3", "ze3", "relop", "e4",
  "ze4", "addop", "e5", "ze5", "mulop", "e6", "e7", "e8", "selector_list",
  "selector", "expr_t", "zexpr_t", "e1_t", "ze1_t", "e2_t", "e3_t",
  "ze3_t", "e4_t", "ze4_t", "e5_t", "ze5_t", "e6_t", "e7_t", "e8_t",
  "selector_list_t", "selector_t", "cons_value", "Str_expr", "e4_s",
  "ze4_s", "e7_s", "e8_s", "expr_list", "actual_list", "actual",
  "elem_list", "elem", "elem_tail", "opt_id_list", "id_list", "Ampersand",
  "$@4", "Assign", "$@5", "Asterisk", "$@6", "Bar", "$@7", "Colon", "$@8",
  "Comma", "$@9", "Dot", "$@10", "Dotdot", "$@11", "Equal", "$@12",
  "Greater", "$@13", "Grequal", "$@14", "Less", "$@15", "Lsequal", "$@16",
  "Minus", "$@17", "Notequal", "$@18", "Plus", "$@19", "Rarrow", "$@20",
  "Rbrace", "$@21", "Rbracket", "$@22", "Rparen", "$@23", "Rpragma",
  "$@24", "Rpragma1", "Semi", "$@25", "Semi1", "Slash", "$@26", "Subtype",
  "$@27", "Uparrow", "$@28", "UparrowPrime", "$@29", "Lparen", "$@30",
  "Lparen2", "$@31", "Lbracket", "$@32", "Lbrace", "$@33", "Pr_External",
  "$@34", "Pr_Inline", "$@35", "Pr_Assert", "$@36", "Pr_Trace", "$@37",
  "Pr_Fatal", "$@38", "Pr_Unused", "$@39", "Pr_Obsolete", "$@40",
  "Pr_Callback", "$@41", "Pr_Exported", "$@42", "Pr_Pragma", "$@43",
  "Pr_Nowarn", "$@44", "Pr_Line", "$@45", "Pr_LL", "$@46", "Pr_LLsup",
  "$@47", "Pr_Spec", "$@48", "Pr_LoopInv", "$@49", "Ident", "$@50",
  "IdentP", "$@51", "IdentPrime", "$@52", "Card_const", "$@53",
  "Real_const", "$@54", "Char_const", "$@55", "Str_const", "$@56", "And",
  "$@57", "Any", "$@58", "Array", "$@59", "As", "$@60", "Begin", "$@61",
  "Bits", "$@62", "Branded", "$@63", "By", "$@64", "Case", "$@65", "Const",
  "$@66", "Div", "$@67", "Do", "$@68", "Else", "$@69", "Elsif", "$@70",
  "End", "$@71", "Eval", "$@72", "Except", "$@73", "Exception", "$@74",
  "Exit", "$@75", "Exports", "$@76", "Finally", "$@77", "For", "$@78",
  "From", "$@79", "Generic", "$@80", "If", "$@81", "Import", "$@82", "In",
  "$@83", "Interface", "$@84", "Lock", "$@85", "Loop", "$@86", "Methods",
  "$@87", "Mod", "$@88", "Module", "$@89", "Not", "$@90", "Object", "$@91",
  "Of", "$@92", "Or", "$@93", "Overrides", "$@94", "Procedure", "$@95",
  "Raise", "$@96", "Raises", "$@97", "Readonly", "$@98", "Record", "$@99",
  "Ref", "$@100", "Repeat", "$@101", "Return", "$@102", "Reveal", "$@103",
  "Root", "$@104", "Set", "$@105", "Then", "$@106", "To", "$@107", "Try",
  "$@108", "Type", "$@109", "Typecase", "$@110", "Unsafe", "$@111",
  "Until", "$@112", "Untraced", "$@113", "Value", "$@114", "Var", "$@115",
  "While", "$@116", "With", "$@117", "Abstract", "$@118", "All", "$@119",
  "Axiom", "$@120", "Depend", "$@121", "Ensures", "$@122", "Exists",
  "$@123", "Func", "$@124", "Iff", "$@125", "Implies", "$@126",
  "Invariant", "$@127", "Is", "$@128", "Let", "$@129", "Map", "$@130",
  "Modifies", "$@131", "Pred", "$@132", "Protect", "$@133", "Requires",
  "$@134", "InitialNPS", "$@135", "NPS", "space_anypragma_list",
  "anypragma_space_list", "anypragma_list", "space_list_emit",
  "space_list", "G", "B0", "B", "B2", "E", "EF", "A", "AO", "AX", "V",
  "VZ", "VC", "Z", "SP", "XSP", "BL", "AL2", "AL3", "ALZ5", "EA", "ALNL",
  "SPNL", "QSP", "NL", "Inc", "Dec", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-1822)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-841)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     346,   148,    74,   132,   123, -1822, -1822,   440, -1822,   443,
      74,    74, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822,   121,   514,   514,
     308,   224,   224,   308, -1822,   515, -1822, -1822,   515, -1822,
   -1822,   633,   679,   708, -1822,   120, -1822, -1822, -1822, -1822,
   -1822,   105,   189, -1822, -1822, -1822, -1822,   210, -1822, -1822,
   -1822,   900,   198, -1822, -1822, -1822, -1822, -1822,   372,   372,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822,   224, -1822, -1822,   407, -1822,   418, -1822,   337, -1822,
   -1822, -1822,   372,   372, -1822,   424,   383,    92,   222,   472,
     478,   224,   633,   633,   633,   407,   406,   406, -1822, -1822,
   -1822, -1822,   224, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822,   424, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822,   496,   496, -1822,   496,   496,   496, -1822, -1822,   473,
     528,   537,   537,   633, -1822, -1822,   571,   424,   520, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,   636,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822,   737,   633,   210,
   -1822,   633,    92,    92,    92,   156,    92,    92,   633,   633,
     103,   633,   633,   633, -1822,   571, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822,   526,   637, -1822,   637,   637,   637,   637,
   -1822, -1822, -1822, -1822,   372, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822,   633, -1822, -1822, -1822,   424, -1822,   424, -1822,
     633, -1822, -1822, -1822,   571, -1822, -1822,   633,   427, -1822,
   -1822,   472,   633,   633,   633,   633,   633,   633,   633,   645,
     645, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822,   382, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822,   687,   407, -1822, -1822, -1822,   118, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822,   684, -1822,   407, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822,   436, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822,   678, -1822,   711, -1822, -1822, -1822, -1822, -1822,
   -1822,   224, -1822, -1822, -1822, -1822, -1822,   633,   407, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
     720, -1822, -1822,   633,   224,   224,   633,   224,   224,   224,
     224, -1822,   682, -1822, -1822,   268,   407,   407, -1822,   407,
   -1822,   407,   407, -1822,   407,   407, -1822, -1822, -1822, -1822,
     633,   633,   407,   637, -1822, -1822, -1822, -1822,   636, -1822,
   -1822,   736, -1822,   723, -1822, -1822, -1822, -1822,   711,   729,
     734, -1822, -1822,   734,   723, -1822, -1822, -1822, -1822, -1822,
     680, -1822,  2489, -1822,   751, -1822, -1822, -1822,   643,   407,
   -1822, -1822, -1822,   407,   633,   472, -1822, -1822,   633,   472,
   -1822,   720, -1822, -1822,   664, -1822,   686,   472, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822,   472, -1822,   712, -1822,
   -1822,   633,   633, -1822, -1822,   280, -1822, -1822,   455, -1822,
   -1822,   280, -1822, -1822,   202,   280,   711, -1822, -1822,   711,
     776,   407,   407, -1822, -1822, -1822, -1822, -1822,   637, -1822,
     784, -1822,   568,   633, -1822,   637,   407, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822,   772, -1822, -1822,
   -1822,   407, -1822, -1822, -1822, -1822, -1822, -1822, -1822,   407,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822,   779,   433, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822,   800, -1822, -1822,
   -1822, -1822, -1822, -1822,   749, -1822,   749,   384,   553, -1822,
   -1822, -1822,   124,   633, -1822,   643,   407,   643, -1822,   680,
     210,   537,   643,   407,   636, -1822, -1822, -1822, -1822,   633,
   -1822,   636, -1822, -1822,   700, -1822,   738, -1822, -1822, -1822,
   -1822, -1822, -1822,   224, -1822,   762, -1822, -1822, -1822, -1822,
   -1822, -1822,   151, -1822,   268,   268, -1822,   687,   776,   407,
     130,   687,   407, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
     687, -1822,   687,   816, -1822,   743,   636,   736,   636, -1822,
   -1822,   637, -1822, -1822,   826, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822,   728,   496, -1822, -1822,   407, -1822, -1822,
   -1822, -1822,   407, -1822,   424,   633,   807, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822,   407, -1822, -1822,   780,   633,
     407,   633,   633,   633,   633,   633,   633,   633,   224,   633,
     633,   633,   633,   224, -1822,   424,   816, -1822, -1822, -1822,
   -1822, -1822,   473,   786,   407,   732, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,   680, -1822,
     636, -1822, -1822, -1822, -1822, -1822, -1822,   537, -1822, -1822,
   -1822, -1822, -1822,   633, -1822,   788,   686,   690, -1822, -1822,
   -1822,   633, -1822, -1822, -1822, -1822, -1822, -1822,   776,   407,
   -1822,   633,   840, -1822,   633,   268, -1822,   636, -1822,   597,
   -1822, -1822, -1822, -1822,   840,   128,   128, -1822,   832,   832,
   -1822,   638, -1822, -1822,   807, -1822, -1822, -1822,   807, -1822,
   -1822, -1822,   424,   424, -1822, -1822, -1822, -1822,   633,   250,
      73, -1822,   645,   601, -1822, -1822,   758, -1822, -1822,   758,
   -1822, -1822, -1822, -1822, -1822,  1598, -1822, -1822,   687, -1822,
   -1822,   489, -1822, -1822,   636, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822,   488, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822,   353, -1822, -1822,   605, -1822,
   -1822, -1822, -1822,   633,   633,   407, -1822,   637, -1822, -1822,
     736,   407, -1822,   736, -1822, -1822,   633, -1822, -1822, -1822,
   -1822, -1822, -1822,   406,   498, -1822,   701, -1822,   633, -1822,
   -1822, -1822,   636, -1822,   736, -1822, -1822, -1822,   744, -1822,
   -1822, -1822, -1822,   633, -1822, -1822,   407, -1822, -1822, -1822,
   -1822, -1822, -1822,   650,   664, -1822, -1822, -1822, -1822,   268,
     633, -1822, -1822,   633, -1822,   407, -1822, -1822,   418,   633,
   -1822, -1822, -1822, -1822,   847, -1822,   146,   807,   728, -1822,
   -1822, -1822,   680, -1822, -1822, -1822,   807, -1822, -1822, -1822,
   -1822,    65, -1822, -1822,    65, -1822,   273, -1822,   449,   760,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822,   645,   764, -1822,
     851, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822,   776,   758, -1822,   763,   794,   680,   633,
   -1822, -1822, -1822, -1822, -1822,   848,   473,   794,   636, -1822,
   -1822,   418, -1822, -1822,   844, -1822, -1822,   407,   807, -1822,
     736, -1822, -1822, -1822, -1822, -1822, -1822,   633,   686, -1822,
   -1822,   647, -1822, -1822, -1822, -1822, -1822,   363, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822,   832,   633, -1822, -1822,   407,   268, -1822, -1822,
   -1822,   280,   840,   840, -1822, -1822, -1822, -1822, -1822,   700,
     738, -1822,   686, -1822,   687, -1822, -1822, -1822, -1822,   820,
   -1822,   776, -1822,   736, -1822, -1822,   337, -1822, -1822, -1822,
     720, -1822, -1822,   407, -1822,   796, -1822,   636,   758,   633,
   -1822, -1822, -1822, -1822,   687, -1822,   633,   633,   633,   633,
   -1822,   811, -1822,   811,   811, -1822, -1822,   496, -1822, -1822,
   -1822, -1822, -1822,   479,   729,    86, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822,   633, -1822, -1822,
     687, -1822, -1822,   818, -1822, -1822, -1822,   636,   136, -1822,
   -1822,   756, -1822,   473, -1822, -1822, -1822, -1822,   769,   633,
     633,   812, -1822, -1822,   749, -1822, -1822,   407,   337, -1822,
     678,   758,   736, -1822, -1822,   680, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822,   633,   633,   633,   633, -1822,   370,
     601,   701,   701, -1822,   809,   407, -1822, -1822,   633,   268,
   -1822, -1822,   407, -1822, -1822,   633, -1822, -1822, -1822,   788,
   -1822, -1822, -1822, -1822,   776, -1822, -1822, -1822,   636, -1822,
   -1822,   473, -1822, -1822,   633, -1822,   877, -1822,   847, -1822,
   -1822, -1822, -1822,   734, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822,   273,   795,   273, -1822,   633,
     633,   802, -1822,   729,   807,   895, -1822, -1822, -1822,   273,
     449, -1822,   407,    78,   115, -1822, -1822, -1822, -1822, -1822,
     867, -1822,   424,   424, -1822, -1822, -1822, -1822, -1822,   637,
   -1822,   812, -1822,   633, -1822,   633, -1822,   637,   807, -1822,
   -1822, -1822, -1822, -1822,   164, -1822, -1822,   680,   473, -1822,
   -1822,   844,   473,   816, -1822, -1822, -1822, -1822, -1822,   680,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822,   807, -1822,   821,
   -1822,   840, -1822, -1822,   529, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822,   686,   839, -1822, -1822,   268,   633, -1822, -1822,
   -1822, -1822,   776, -1822, -1822, -1822,   807, -1822, -1822,   720,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822,   407, -1822, -1822,
     424, -1822, -1822, -1822, -1822,   805,   813, -1822, -1822, -1822,
   -1822, -1822,   823, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822,   633, -1822, -1822, -1822,
   -1822, -1822,   164, -1822, -1822, -1822,   812, -1822,   680, -1822,
   -1822,   633, -1822,   680, -1822, -1822, -1822,   473,   812,   407,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,   217,   407,
   -1822, -1822,   633,   633,   633,   633,   701, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822,  1011,   607, -1822,
     397, -1822, -1822, -1822, -1822, -1822, -1822,   636,   321, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822,   636, -1822,   571, -1822, -1822,   808, -1822,   496, -1822,
     633, -1822, -1822, -1822, -1822,   407, -1822,   376, -1822, -1822,
     800,   816, -1822, -1822, -1822,   680, -1822,   812,   903, -1822,
   -1822, -1822, -1822,   550, -1822, -1822, -1822, -1822,   633,   633,
   -1822, -1822, -1822,   190,   680, -1822,   794, -1822, -1822, -1822,
   -1822,   812, -1822,   736,   407, -1822, -1822, -1822, -1822, -1822,
     407, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
     669, -1822,   205,   345, -1822, -1822,   388, -1822, -1822, -1822,
   -1822, -1822,   680, -1822,   916,   522,   776, -1822,   776,   776,
   -1822,   926, -1822, -1822,   680, -1822,   496,   296, -1822, -1822,
   -1822, -1822, -1822, -1822,   720, -1822,   636, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822,   844, -1822, -1822, -1822, -1822,
   -1822, -1822,   636,   917,   916,   680, -1822, -1822, -1822, -1822,
   -1822, -1822,   680,   711,   917, -1822, -1822,   473, -1822,   680,
   -1822,   637, -1822,   164, -1822, -1822, -1822,   736, -1822,   931,
     353, -1822, -1822, -1822, -1822,   633, -1822,   240,   593,   375,
     711, -1822,   736, -1822,   720, -1822,   895, -1822, -1822, -1822,
     636, -1822, -1822, -1822, -1822, -1822, -1822, -1822,   687, -1822,
   -1822, -1822,   250, -1822, -1822, -1822, -1822,   840, -1822, -1822,
     720,   224,   224, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
     637, -1822,   680, -1822, -1822,   310, -1822,   895, -1822,   633,
   -1822, -1822,   407, -1822, -1822,   571, -1822,   744, -1822,   680,
   -1822,   812,   776, -1822, -1822,   933,   407,   917, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
     807,   807,   375,   407, -1822, -1822, -1822, -1822, -1822,   807,
   -1822, -1822, -1822,   840, -1822, -1822, -1822, -1822, -1822,   571,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,   407,
     918,   816, -1822, -1822, -1822, -1822,   633, -1822, -1822, -1822,
   -1822, -1822,   844,   310,   680, -1822, -1822, -1822, -1822, -1822,
   -1822,   569, -1822,   807, -1822, -1822, -1822,   397,   633,   840,
     736,   807, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822,   664, -1822,   686, -1822,   720, -1822, -1822,
   -1822, -1822, -1822,   734, -1822, -1822, -1822,   636, -1822, -1822,
   -1822,   844, -1822, -1822, -1822, -1822, -1822,   763, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822,   680,   310,   407,   407, -1822,
   -1822,   345, -1822, -1822, -1822, -1822, -1822, -1822,   700,   738,
   -1822, -1822, -1822, -1822, -1822,   816,   302, -1822,   424,   863,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,   407,   926,
   -1822, -1822, -1822, -1822, -1822, -1822,   756,   807, -1822, -1822,
     870, -1822, -1822,   844, -1822,   917, -1822, -1822, -1822,   788,
     686,   342, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822,   407, -1822, -1822, -1822, -1822,
   -1822,   844,   844,   633, -1822,   424, -1822, -1822, -1822, -1822,
   -1822,   794,   743, -1822, -1822,   807, -1822, -1822, -1822, -1822,
   -1822,   406,   498, -1822,  2121, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
     844, -1822,   917, -1822, -1822, -1822, -1822, -1822,   686, -1822,
   -1822,   647,   804, -1822,    65,   924,   363, -1822, -1822, -1822,
   -1822, -1822, -1822,   574, -1822, -1822, -1822, -1822,   779, -1822,
     816,   424, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822,   749, -1822,   917, -1822, -1822, -1822, -1822,   601,
   -1822,   407, -1822,   811,   796,   811, -1822, -1822,   370,   701,
     701, -1822, -1822,   816, -1822,   424,   633, -1822, -1822, -1822,
     729, -1822,   424, -1822,   310,   473, -1822,   310, -1822, -1822,
     146, -1822,   273,   877, -1822,   847,   273,   601, -1822, -1822,
   -1822,   807,   686, -1822, -1822, -1822, -1822, -1822,   816,   437,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,   807,
   -1822, -1822,   720,   701, -1822, -1822, -1822, -1822, -1822,   779,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,   680, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822,   601, -1822, -1822, -1822,
   -1822,   424,   407, -1822, -1822, -1822,   816, -1822, -1822,   776,
     776,   776,  2121, -1822, -1822,   424, -1822,   816, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822,   407,   807,   840,
   -1822, -1822, -1822, -1822, -1822,   407,   424, -1822, -1822,   720,
   -1822, -1822,   424, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,     0,     8,     0,     0,     1,   814,     0,   839,   801,
       8,     8,   691,   699,   709,   753,   839,    21,    23,    25,
      26,   829,   829,   829,   829,     9,   815,   826,   818,   818,
     826,   826,   826,   826,     3,     0,   818,   818,     0,   811,
     803,   809,     0,     0,   689,   818,    10,   839,    13,    12,
     829,    61,   818,    16,   839,    19,    18,    61,   692,   805,
     806,   807,   813,   700,   710,   754,   829,   829,     0,     0,
      22,    24,   812,   810,   619,   621,   286,   287,   829,   829,
     617,   623,   625,   627,   285,   288,   289,   829,   829,   829,
     829,   826,    11,    14,     0,   695,     0,   822,     0,    17,
      20,   808,     0,     0,   633,   829,    35,   826,   826,     0,
       0,   826,   804,   804,   804,     0,     0,     0,   818,   690,
     631,   829,   826,   599,   601,   611,   609,   607,   613,   615,
     663,   679,   721,   737,   749,   761,   829,   839,   839,   839,
     839,   839,   839,   839,   829,   829,   829,   829,   829,   829,
     829,   820,   820,   829,   820,   820,   820,   834,   653,   825,
     270,    33,    33,   804,   580,   820,     0,   829,     0,   620,
     622,   579,   291,   637,   829,   618,   624,   626,   628,   829,
     535,   553,   555,   557,   559,   561,   565,   697,   829,   427,
     431,   432,   429,   430,   428,   433,   829,     0,   804,     0,
     696,   804,   826,   826,   826,   826,   826,   826,   804,   804,
     826,   804,   804,   804,   820,     0,    62,    67,    63,    66,
      65,    64,    68,   818,     0,   817,     0,     0,     0,     0,
     831,   833,   831,   840,     0,   831,   832,   831,   832,   831,
     833,   840,   804,   826,   119,   818,     0,   838,     0,   634,
     804,   839,   823,   820,     0,   683,   829,   804,     0,   547,
     822,     0,   804,   804,   804,   804,   804,   804,   804,   818,
     818,   771,   773,   779,   785,   789,   795,   797,   767,   820,
     300,   839,   301,   303,   302,   304,   305,   306,   307,   308,
     309,   317,   405,   829,   829,   829,   829,   829,   829,   829,
     829,   829,   632,   829,   600,   602,   612,   610,   608,   614,
     616,   664,   680,   722,   738,   750,   762,   831,   831,   545,
     577,   829,     0,   262,   818,   280,     0,   281,   282,   283,
     284,    51,   840,    59,   820,    82,     0,   822,    55,   816,
      53,   816,    57,   840,     0,    41,   816,   654,   841,   820,
     818,   271,     0,   839,     0,   839,   581,    37,   818,   839,
     823,   826,   817,   638,   643,   297,   829,   804,     0,   290,
     554,   556,   558,   560,   562,   566,   698,   597,   829,   294,
     828,   818,   829,   804,   826,   826,   804,   826,   826,   826,
     826,   822,   311,   549,   591,     0,     0,     0,   818,     0,
     818,     0,     0,   818,     0,     0,   828,   817,    50,   817,
     804,   804,     0,     0,   829,   820,   818,   647,   278,   194,
     829,   196,   279,   835,   836,   816,   831,   840,   828,     0,
     835,   836,   818,   835,   835,   836,   816,   830,   820,   816,
       0,   114,   817,   605,   825,    37,   817,    37,    47,     0,
      37,   818,   684,     0,   804,     0,   548,   536,   804,     0,
     571,   828,   295,   818,   820,   417,   818,     0,   772,   774,
     780,   786,   790,   796,   798,   768,     0,   793,   313,   839,
     829,   804,   804,   791,   319,   383,   829,   406,     0,   828,
     822,   326,   331,   336,   818,   326,   828,   333,   822,     0,
     822,     0,     0,    47,   546,   578,   829,   263,     0,   511,
     820,   513,     0,   804,   822,     0,     0,   836,   820,    69,
     816,    60,    83,   824,   841,   593,    86,   818,   836,   820,
      73,     0,   820,   836,   820,    77,   816,   820,   820,     0,
     673,   829,   603,   661,   675,   681,   693,   701,   703,   723,
     733,   735,   747,   763,   765,   820,   118,   123,   122,   126,
     128,   129,   130,   131,   132,   133,   134,   135,   136,   137,
     138,   139,   140,   141,   142,   143,   144,   146,   829,   829,
     823,   156,   829,   829,   274,   823,   274,   823,   825,   829,
     829,    47,     0,   804,   820,    47,   533,    47,    38,   818,
       0,    33,    47,     0,   820,   644,   298,   598,   292,   804,
     296,     0,   520,   416,     0,   711,   820,   420,   423,   829,
     818,   293,   299,   826,   799,   315,   839,   829,   312,   828,
     550,   592,   826,   595,     0,     0,   575,   318,   820,     0,
       0,     0,     0,   769,   777,   820,   338,   829,   829,   818,
       0,   828,     0,     0,   828,     0,     0,   407,   820,   828,
     818,     0,   264,   512,     0,   649,   729,   741,   838,   458,
     818,   516,   517,   822,   234,   829,   648,     0,   820,   197,
      70,   831,     0,   818,     0,   804,     0,   834,    74,   831,
     205,   829,   831,    78,   831,     0,   820,   835,    43,   804,
       0,   804,   804,   804,   804,   804,   804,   804,   826,   804,
     804,   804,   804,   826,   825,     0,     0,   818,   818,   818,
     818,   818,   825,     0,     0,   825,   818,   826,   818,   832,
     820,   818,   687,   751,   829,   822,   606,   829,   818,   820,
     534,   828,    48,   829,    61,   820,   828,    33,    36,   572,
     822,   717,   829,   804,   419,     0,   818,   820,   425,   818,
     794,   804,   775,   310,   839,   829,   314,   818,   392,     0,
     792,   804,     0,   829,   804,     0,   321,     0,   380,     0,
     781,   829,   829,   822,     0,   826,   826,   337,     0,     0,
     820,   342,   818,   822,   828,   822,   539,   829,   828,   659,
     829,   822,     0,     0,    61,   265,   537,   829,   804,   826,
     826,   519,     0,   820,   828,   818,     0,   837,   816,     0,
     195,   277,    52,   821,   828,   231,    84,   594,    88,   816,
      56,     0,    54,    58,   821,   836,   820,   816,   674,   820,
     604,   662,   676,   682,   694,   702,   704,   724,   734,   736,
     748,   764,   766,   120,   121,   823,   818,   829,   818,   829,
     829,   837,   629,   275,   829,   169,   826,   173,     0,   829,
     816,   127,    61,   804,   804,     0,   818,     0,   829,   828,
       0,     0,   820,     0,   820,   818,   804,   818,   712,   645,
     829,   422,   424,     0,   820,   435,     0,   800,   804,   316,
     818,   398,     0,   393,   396,   596,   573,   384,     0,   576,
     320,   822,   829,   804,   818,   818,     0,   327,   770,   778,
     828,   828,   341,     0,   820,   347,   818,   818,   828,     0,
     804,   818,   822,   804,   828,     0,   839,   820,   840,   804,
     818,   650,   730,   742,   824,   515,   838,     0,   828,   198,
     715,   825,     0,   833,   825,   816,     0,   655,   657,   739,
     757,   820,   820,   206,   217,   207,   212,   224,     0,     0,
     818,   818,   822,   829,   823,   219,   829,     0,    90,   822,
     816,   836,   835,   818,   585,   822,   822,   816,   840,   839,
     829,   839,   818,   820,     0,   820,     0,     0,     0,   804,
     818,   818,   818,   677,   685,   177,   825,     0,   820,   189,
     816,   840,   688,   752,   268,   822,   267,     0,     0,    32,
       0,   839,    29,   820,   521,   718,   418,   804,   818,   829,
     434,     0,   563,   567,   639,   641,   457,   820,   441,   447,
     458,   828,   828,   818,   451,   452,   453,   454,   455,   776,
     818,   822,   395,   804,   745,   829,     0,     0,   782,   324,
     323,     0,   828,   828,   783,   822,   345,   344,   346,     0,
     820,   350,   818,   828,     0,   329,   540,   334,   335,     0,
     660,   332,   818,   408,    39,   820,     0,   538,   514,   817,
       0,   587,   459,     0,   461,     0,   518,     0,     0,   804,
     818,   222,   816,   818,   829,   828,   804,   804,   804,   804,
     200,   231,   203,   231,   231,   713,   731,   234,   822,   823,
     828,   829,   818,   818,     0,   231,   817,   725,    87,   829,
     818,   836,   835,   820,    97,   820,   101,   804,   818,   818,
     829,    42,    40,     0,    15,   818,   829,     0,   827,   157,
     743,   825,   667,   825,   168,   630,   829,   828,     0,   804,
     804,   826,   818,   837,   274,   820,   820,     0,     0,   822,
       0,     0,     0,    34,    27,     0,   839,   646,   421,   818,
     829,   439,   438,   437,   804,   804,   804,   804,   440,     0,
     450,     0,     0,   828,   820,     0,   818,   574,   804,     0,
     381,   382,     0,   822,   822,   804,   818,   829,   349,     0,
     820,   353,   829,   818,   820,   822,   787,   829,     0,   376,
     831,   825,   828,   510,   804,   460,   824,   824,   824,   822,
     825,   716,   221,   835,   836,   835,   818,   223,   829,   821,
     841,   656,   658,   740,   758,   820,     0,   820,   225,   804,
     804,   227,   818,     0,     0,     0,   822,   829,   209,   211,
       0,   218,   533,   826,     0,    89,    98,   820,    95,    94,
     821,   586,     0,     0,   829,   821,   651,   829,   820,     0,
     822,   826,   818,   804,   164,   804,   837,     0,     0,   755,
     822,   678,   686,   837,     0,   177,   819,     0,   825,   820,
     835,   268,   825,     0,   269,   829,   820,    31,   829,     0,
     426,   818,   564,   568,   640,   642,   541,   583,   665,   707,
     829,   443,   444,   445,   446,   449,   448,     0,   399,     0,
     394,     0,   746,   385,     0,   818,   818,   784,   343,   818,
     829,   352,   818,   820,   818,   322,     0,   804,   818,   822,
      49,   839,   531,   818,   588,   817,     0,   817,   817,     0,
     818,   818,   836,   835,   820,   238,   820,     0,   818,   816,
       0,   201,   202,   714,   732,   229,     0,   214,   210,   820,
     551,   825,     0,   233,   822,   820,   726,   817,    92,    96,
     822,   818,   820,   820,   818,   816,   804,   818,   145,   272,
     818,   837,     0,   149,   818,   744,   826,   668,     0,   276,
     170,   804,   818,     0,   543,   669,   829,   825,   826,     0,
     174,   837,   836,   829,   829,   839,   829,   818,   827,     0,
     829,   436,   804,   804,   804,   804,     0,   456,   829,   397,
     325,   339,   340,   348,   818,   354,   356,     0,   820,   358,
       0,   330,   788,   328,   818,   841,   820,     0,   528,   526,
     828,   464,   828,   828,   466,   199,   220,   239,   820,   236,
     235,   820,   829,     0,    85,   837,     0,   705,   242,   204,
     804,   818,   825,   818,   828,   192,   102,   103,   820,   820,
     828,     0,   652,    44,   820,     0,   829,   826,   828,   152,
     818,   837,   165,     0,   167,   756,   820,   176,   804,   804,
     818,   163,   837,   822,     0,   816,     0,   820,   841,   818,
     829,   826,   818,     0,     0,   542,   584,   666,   708,   442,
       0,   351,   829,   390,   387,   389,   386,   388,   391,   357,
       0,   635,   820,   363,   368,   374,   409,   379,   818,   412,
     378,   377,     0,   828,   822,     0,   820,   816,   820,   531,
     237,   816,   821,   829,     0,   719,   254,   826,   228,   816,
     552,   828,   818,   213,     0,   820,   193,   727,   759,   821,
     829,   829,   829,    75,    76,   268,   821,   829,   829,   148,
     818,   837,     0,     0,   820,     0,   671,   829,   171,   544,
     670,   178,     0,     0,     0,   187,   190,   825,   816,     0,
     822,     0,   837,     0,   183,   818,    28,     0,   403,   820,
     829,   818,   829,   361,   360,   804,   362,     0,     0,     0,
       0,   828,   411,   829,     0,   818,     0,   822,   822,   828,
       0,   522,   818,   828,   820,   820,   820,   820,     0,   816,
     818,   226,   826,   230,   816,   706,   833,     0,   208,   820,
       0,   826,   826,   818,   106,   104,   105,   841,   816,   818,
       0,   150,     0,   822,   569,   825,   154,     0,   161,   804,
     818,   175,     0,   820,   837,     0,   828,     0,   266,     0,
     829,   826,   829,    30,   400,     0,     0,     0,   820,   818,
     636,   829,   365,   366,   367,   589,   372,   373,   828,   412,
     828,     0,     0,     0,   509,   527,   532,   818,   818,     0,
     822,   820,   818,     0,   828,   240,   241,   818,   829,     0,
     828,   720,   832,   816,   216,   215,    91,   728,   760,     0,
       0,     0,   828,   273,   147,   818,   804,   820,   816,   825,
     672,   829,   268,   825,     0,   829,   820,   829,   182,   818,
     837,     0,   829,     0,   829,   355,   359,     0,   804,   828,
     410,   828,   369,   375,   413,   414,   415,   820,   530,   529,
     463,   816,   524,   820,   468,   818,   462,     0,   829,   818,
     829,   841,   816,   835,   836,   835,   818,   821,   582,   820,
     829,   268,   153,   570,   151,   119,   818,     0,   828,   820,
     188,   818,    45,   818,   184,     0,   825,     0,     0,   829,
     818,   364,   590,   371,   370,    46,   523,   467,     0,   820,
     471,   474,   829,   818,   465,     0,   820,   818,     0,   816,
     836,   835,   818,   836,   835,   820,   246,   820,     0,   816,
     816,   818,   841,   820,   820,   820,   825,     0,   180,   191,
     822,   181,   820,   268,   404,     0,   401,   829,   470,     0,
     818,   822,   476,   818,   829,   830,   110,   818,   828,   820,
     836,   835,   820,   258,   820,     0,   247,   820,   244,   243,
     821,   268,   268,   804,   828,     0,   116,   117,   155,   166,
     829,     0,     0,   159,   185,     0,   829,   818,   829,   473,
     475,     0,   820,   479,   231,   818,   820,   829,   841,   820,
     259,   820,   256,   255,   821,   245,   818,   820,   820,   820,
     268,   820,     0,   820,   829,   829,   818,   469,   818,   829,
     478,     0,   487,   491,   217,   492,   820,   482,   484,   495,
     828,   828,   818,   196,   496,   497,   498,   499,   820,   816,
       0,     0,    72,   257,   818,   829,    99,   100,   820,   841,
     820,   820,   274,   818,     0,   402,   472,   818,   829,   488,
     500,     0,   503,   231,     0,   231,   458,   481,     0,     0,
       0,   828,   818,     0,   112,     0,   804,   829,   820,   829,
     821,    79,     0,    81,   825,   825,   829,   825,   477,   818,
     838,   502,   489,   824,   824,   824,   490,   493,   829,   486,
     485,     0,   818,   822,   830,   820,   818,   820,     0,   820,
     818,   820,   820,   837,   160,   820,   480,   501,   817,     0,
     817,   817,     0,     0,   458,   818,   818,   820,   107,   820,
      71,   829,   820,   820,   818,   829,   820,   179,     0,   186,
     828,   506,   828,   828,   508,   483,   494,   818,   820,   109,
     111,     0,     0,   820,   251,   829,     0,    80,   158,   820,
     820,   531,   231,   820,   830,   820,   248,     0,   829,   828,
     828,   820,   820,   820,   820,   820,   829,     0,     0,     0,
     828,   820,   108,   820,   261,     0,   820,   505,   504,     0,
     820,   260,   820,   820,   820,   507,   820,   820,   820,   820,
     253,   525,   820,   252,   250,   249
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1822, -1822, -1822, -1822, -1822,   752, -1822,   911, -1822,   907,
     340,   922,   923, -1822, -1822,   -99, -1822,   364,   639, -1822,
     -26, -1822,    67,  -338,   644,  -686, -1822,   540,   625,   297,
   -1822,   533, -1822,   541, -1822, -1087, -1822, -1822, -1822, -1822,
     -10,    -9, -1822, -1822,  -877,  -874,  -427, -1640, -1822,  -828,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,  -422,  -608,
   -1822,  -762, -1822, -1822, -1822, -1822, -1822, -1193, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822,  -310,  -524, -1822,
    -625,  -772, -1822, -1822, -1822,  -522, -1822, -1284,  -317,  -624,
     824,  -448, -1822,   169,   170,  -502, -1051,  -118, -1822, -1822,
    -805,  -119, -1822,  -234,  -231, -1822, -1822,  -788,  -787, -1822,
   -1822,  -829,  -825, -1822, -1822, -1822, -1145,  -965, -1822,  -575,
   -1822, -1822, -1822, -1822,  -259, -1822,  -801, -1822,   975, -1822,
   -1822, -1822,   747, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822,   523, -1822, -1822, -1822, -1822,
   -1822, -1822, -1011,  -384, -1822, -1822, -1095, -1822, -1822, -1822,
   -1822,  -308, -1822,  -415,  -307, -1822,  -589, -1822, -1822,  -663,
   -1822, -1822,  -728, -1822, -1018, -1822,  -550,  -577,  -528, -1822,
   -1822, -1822,  -155, -1822, -1822, -1822,  -777, -1822,  -157, -1822,
   -1822,  -666, -1822,  2900, -1822,  -859, -1822,  -726, -1822, -1822,
    -112, -1130, -1822,  -897, -1258, -1822,  -942, -1144, -1822, -1822,
   -1007,  -961,  -905, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,  -895,  -620,
   -1822, -1822,   111, -1822, -1403, -1008,  -724, -1402,  -582, -1495,
    -207,  -100,   392, -1822,  -627, -1822,  -568, -1822, -1317, -1822,
    -128, -1822,   398, -1822,  -403, -1822, -1425, -1822,  -145, -1822,
    -387, -1822,  -386, -1822,  -385, -1822,  -382, -1822, -1006, -1822,
    -381, -1822,  -990, -1822, -1523, -1822,  -447, -1822,  -750, -1822,
    -323, -1822,  -149, -1822,  -121,   -94, -1822,  -773, -1822, -1822,
   -1822, -1822, -1556, -1822, -1822, -1822,  -278, -1822, -1822, -1822,
    -459, -1822,  -781, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
    -103, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822,   476, -1822,    36, -1822,
   -1513, -1822,  -102, -1822, -1821, -1822, -1811, -1822,  -254, -1822,
   -1155, -1822,  -196, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822,  -833, -1822, -1822, -1822, -1822, -1822,
    -557, -1822,  -963, -1822, -1822, -1822, -1822, -1822,  -175, -1822,
   -1822, -1822,  -258, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822,  -305, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
     875, -1822, -1822, -1822,  1040, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822,  -549, -1822,  1046, -1822, -1021, -1822, -1822, -1822,
    -725, -1822, -1031, -1822, -1822, -1822,  -786, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822,  -178, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822,   -41, -1822, -1822, -1822,  -722, -1822,
    -601, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822,  -174, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822,   165, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,
   -1822, -1822, -1822, -1822, -1822, -1822, -1822, -1822,  3561,  1049,
     278, -1822, -1822,   590,  1545,  -203,   866,  -209,  1609,  -412,
    4183,  -200,  -909,    76,  -187, -1052,  3409,   -21, -1773,  -140,
    -232,  -228,   404,  -343,  -349,  -763,  -240,   294,    -8,  -497
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     1,     2,     3,     4,     7,    45,    46,    52,    53,
      47,    17,    18,    19,    20,   246,   167,   448,   598,   344,
     345,   730,   659,   660,   742,    96,   423,   424,   430,   431,
     434,   435,   334,   335,   136,   526,   978,  1128,   686,   980,
     981,   982,  1270,  1579,  1645,  1646,   243,  1747,  1853,   349,
     350,   555,   556,   557,   558,   559,   560,   561,  1281,  1403,
    1498,  1499,   562,   563,   564,  1901,   565,  1293,  1406,  1502,
     566,   567,   568,   569,   570,   571,   572,  1161,  1295,   573,
    1521,  1614,   574,   575,  1008,  1009,  1575,   418,   690,   948,
     949,  1942,   962,  1943,  1944,  1036,   966,   967,  1375,  1475,
    1246,   817,  1233,  1234,  1235,  1568,  1793,  1794,  1795,  1653,
    1839,  1840,  1841,   137,  1304,   351,  1169,   159,   576,   722,
     863,   138,   420,   139,   140,   141,   142,   143,    39,    84,
      76,    85,   378,    77,    86,   279,   280,   478,   625,   763,
     281,   282,   283,   284,   490,   641,   285,   286,   287,   288,
     289,   290,  1077,  1078,   493,   645,   646,   790,  1065,   791,
     924,   925,  1070,  1071,  1210,  1211,  1343,  1448,  1622,  1449,
    1542,  1701,  1543,  1544,  1081,  1545,   638,   778,   484,  1532,
     479,   768,   903,  1052,   626,   764,  1618,  1619,   485,   500,
    1547,  1632,  1774,   379,   464,   465,   616,   617,   618,   757,
     188,   758,   894,  1180,   895,  1037,  1320,  1038,  1039,  1040,
     813,  1092,  1721,  1783,  1784,  1829,  1830,  1831,  1871,  1872,
    1912,  1913,  1946,  1947,  1948,  1949,  1979,  1980,   811,   321,
     415,   510,   511,   669,   461,  1556,  1641,  1352,  1459,  1456,
     739,   779,  1181,   939,   797,   930,  1321,  1432,  1416,  1508,
     322,   410,   260,   367,   396,   481,  1381,  1480,   189,   262,
     190,   263,   191,   264,   192,   265,   193,   266,  1041,  1184,
     194,   267,  1042,  1185,  1675,  1746,   462,   609,   907,  1053,
     637,   774,   323,   411,   172,   165,   250,  1799,  1322,  1433,
     986,  1137,  1094,  1224,  1707,  1768,  1043,   482,   527,   685,
     971,   771,   380,   458,   144,   201,   145,   202,   578,   701,
     444,   593,   146,   205,   147,   204,   148,   203,   149,   206,
     150,   207,    87,   111,    78,   107,    79,   108,    88,   112,
      89,   113,    90,   114,   864,   999,   292,   198,   105,   163,
    1549,  1625,  1045,   257,  1046,  1186,  1047,  1187,  1048,   454,
     890,  1027,   422,   513,   673,   808,  1277,  1396,   160,   242,
     972,  1106,   973,  1107,   800,   933,   579,   702,   151,   208,
    1323,  1434,  1153,  1285,  1417,  1509,  1597,  1679,   541,   699,
     580,   703,  1005,  1159,   152,   209,   581,   704,   256,   361,
    1006,  1160,   734,   873,    50,    91,    21,    30,   582,   705,
      97,   122,   195,   268,    22,    31,   583,   706,   584,   707,
    1478,  1567,  1324,  1435,    23,    32,   619,   753,  1117,  1249,
     951,  1099,   752,   886,  1566,  1652,   153,   210,   585,   708,
    1129,  1263,  1580,  1661,   674,   809,  1118,  1250,   586,   709,
     587,   710,   154,   211,   975,  1108,   675,   810,  1151,  1283,
    1055,  1198,   588,   711,   155,   212,   735,   874,    24,    33,
    1290,  1411,   976,  1109,  1581,  1662,   156,   213,   589,   712,
     590,   713,   294,   390,   647,   785,   295,   383,   296,   384,
     765,   898,   648,   786,   297,   385,   782,   913,  1067,  1205,
     298,   386,  1217,  1347,   299,   387,   486,   632,   480,   623,
     300,   388,   301,   389,   627,   761,     8,    27,    58,    59,
      60,    41,    61,    62,   432,   977,   381,   592,   230,   837,
      42,   168,   683,   244,    43,  1162,   639,  1170,   537,   331,
     339,   332,   241,   518,   519,   952,   812,    25,   336,   440
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      35,    36,    37,    38,   366,   196,   341,   354,   174,   419,
     668,   725,   343,   397,   610,   179,   492,   594,   516,   497,
     968,   252,   326,   293,   969,  1182,   634,   684,  1026,    94,
     891,   944,   642,  1190,   917,  1089,   642,  1258,  1207,   974,
     291,  1183,   214,    98,  1164,   102,   103,  1325,  1326,  1310,
     814,  1212,   358,  1431,  1340,  1214,   348,   109,   110,  1558,
    1245,  1559,  1247,   248,  1644,   784,   115,   116,   117,   118,
     318,  1219,  1706,   253,  1259,   325,   446,   327,   328,   329,
     330,  1683,   530,   691,   166,  1496,   535,   529,  1401,   855,
     532,   534,   333,  1955,   954,   338,  1282,   340,   998,   342,
     199,   794,  1916,  1956,   798,   106,   772,   773,  -804,   360,
     599,  1338,  -804,  1809,  -822,   215,  1709,  -822,   938,  -822,
      -5,  1221,  -822,   223,   224,   225,   226,   227,   228,   229,
    1638,  -804,   234,  -822,  -804,  1513,  -822,   365,   161,   162,
     369,   181,  -149,  -829,  -822,   377,   254,  -822,     5,   120,
     523,  -804,   353,   258,   355,   393,  1424,  -804,   261,   453,
     451,   727,  -822,   395,   958,  -822,  1378,   269,   680,  -822,
    1414,  -829,  -822,  1091,  1764,   270,  1862,   408,   409,   688,
    -804,  -840,  -804,  1119,   693,  1331,  1011,  -804,   417,    -7,
     780,   417,  -822,   412,   427,  -822,  1126,  -822,   259,  1776,
    -822,  1576,    95,  1302,   502,   959,   503,     6,     6,  -829,
    -822,  1716,     6,  1501,    44,    12,  -804,   732,   651,    13,
    -149,   654,  -149,  -183,    14,  1512,     6,   231,   233,  -804,
     236,   238,   240,   346,   670,   362,   245,     6,  1816,   591,
    1441,  1442,   491,   596,   495,  1316,    15,   910,  1415,   733,
     438,  2047,  1749,   731,  -804,  -804,   643,   738,   672,    11,
     644,  1955,     6,  -822,   507,  -822,  -822,    10,  -822,  1148,
     337,  1956,   398,   399,   400,   401,   402,   403,   404,   405,
     406,  -804,   407,    44,    12,     6,   521,  -822,    13,   393,
       6,  -822,  1529,    14,  -822,   861,  1690,   488,   866,   120,
     413,  -183,  1178,  -183,  1591,    40,  -821,    95,  -822,   633,
      34,  2093,  1203,  1204,  1228,    15,  -115,  1355,  1357,  1358,
    1260,  1212,  1318,   965,   425,  -829,  1798,  -804,  1612,   920,
     921,  -829,    26,   483,   606,   436,  -804,  -822,   608,   101,
    -822,    93,  1906,  1319,   657,   455,   621,    16,   100,  -822,
    -820,  -825,  -822,   604,   836,   622,     6,   459,     6,   662,
     820,   467,   393,   828,  2032,  -820,   678,  2035,  -822,    54,
    1062,  1063,  -828,  1230,  -828,  1316,  1522,   961,  1762,  -825,
     719,   394,  -804,   901,     6,   724,  1982,   726,  -822,   319,
    -172,   393,    54,   508,  -115,  1317,  -115,   393,  -828,   515,
    1297,  1075,   658,   104,  -172,  -804,   120,  -804,  -172,  1971,
     394,   955,  -804,   158,  -820,   425,  -828,   181,   182,   183,
     184,   185,   987,   186,   743,   394,   436,  1541,   120,   173,
       6,   216,   217,   218,   219,   220,   221,   222,   120,  -125,
    1667,  -821,     6,  1551,   259,  -822,  1306,   171,   164,  1541,
    -820,  2007,  1318,  -125,   123,   124,   125,   126,   127,   629,
     164,   164,   364,   259,   128,   635,  -822,   129,  -172,  -172,
    -172,   928,  -172,  1319,  -823,   932,  -172,  1019,   636,  1200,
    1022,    -4,    -6,  1387,  -802,   661,   988,  -802,  -802,  1577,
    -802,  -802,   171,  -802,  -124,   781,   740,   870,  1760,   130,
     181,  -822,   745,  1257,   187,  1578,   135,   131,  -124,   775,
     173,  -172,   805,   783,  -822,   984,  -822,  -125,  -125,  -125,
     700,  -125,   793,  1408,   795,  -125,   796,  -825,   132,  1201,
    1413,  1060,  1380,   319,  1623,    12,   133,  1631,   393,    13,
     868,   822,   134,  1093,    14,   357,   320,   970,   135,   830,
    1624,   906,   832,  1607,   833,  1115,   716,   717,   718,  -113,
    -125,   720,   721,   878,  1116,  -828,    15,   865,   728,   729,
     121,  -825,  -124,  -124,  -124,   392,  -124,    26,  -829,  1163,
    -124,  1439,   181,   393,  1440,  -232,  2091,  1001,  1674,  1298,
     826,   180,     9,  1196,  -232,   834,   394,   394,   756,   120,
       9,     9,  1202,   364,   319,   259,   767,  1808,    44,    12,
    -828,   255,   904,    13,    13,  -124,  1156,  1174,    14,    14,
    1091,   854,   633,  -822,  1096,  -822,   788,   789,  -828,  -828,
    -828,  -828,  1134,  1105,  1415,  1596,  1227,  1133,  1495,  1135,
      15,  -113,   665,  1223,   259,  -113,  1705,   445,   884,   447,
     806,   912,   816,   450,   819,   992,  1852,   320,  1514,  2080,
     723,  2081,   723,  1032,   741,  1033,  1194,    48,    55,   746,
     831,  1333,    49,    56,  -822,   377,  1426,  -822,  -826,  1002,
    -826,  -826,   666,  -826,    48,  1032,   985,  1033,   667,    49,
    1769,    55,  1771,  1003,   319,  1173,    56,  1004,  -829,  -829,
     979,  -822,  -822,  -822,  -822,  -822,  1095,  -822,   936,   937,
     780,  1064,  1564,   875,  1908,  -831,   877,  1032,  1905,  1033,
      74,   443,   881,    75,  1284,  1102,  1286,   528,  1016,   394,
     528,   887,   120,   173,  1034,  1035,   364,    44,  1595,   394,
    -818,   460,   437,  1370,   900,   393,  1927,  1928,   477,  1602,
     818,  2036,   908,    80,  -840,    81,    82,   525,    83,  1061,
     914,   915,    28,    29,  1832,  -816,   540,     6,   120,  1307,
     923,  -113,  -822,   628,  1124,   665,   931,  1101,  1083,   934,
    1494,  1029,  1266,   624,  -828,  1969,   940,  -822,  -822,  1267,
     853,   615,   271,   272,  1351,   -93,   273,  1867,   414,   274,
    -825,   275,   421,  -828,  -829,   276,   277,   278,   751,   595,
    -822,   597,   428,   393,   602,   666,   514,  1226,  1451,   762,
     796,   667,   799,  1154,   394,   633,   377,  1369,  1672,   806,
     636,  1091,  -838,  -838,  -838,  -829,   994,   862,   996,   997,
    -113,  -113,  -113,  1000,   457,  2019,  2020,  2008,  1007,  1689,
    -822,  -822,  -822,  -822,  -822,  -821,  -822,  1017,  1391,  -113,
     889,   633,   906,  1395,  1587,   950,  1054,   135,  -828,  1028,
     132,  1421,   487,   180,  -835,  1425,  1127,  1152,   496,  -827,
     498,   499,  -829,  1216,  1150,  1365,  1222,  -829,   506,   958,
    1364,  1057,  1366,  1276,    51,    57,  1289,  -829,  -162,  2065,
    -828,  1115,    68,    69,  -826,  1380,   126,  1734,  1950,  1003,
    -829,    51,  1464,  2029,  -826,  1477,   732,  1565,    57,  1253,
     766,  1754,  -829,  1262,  1951,   601,  -829,  1098,  1637,   180,
    1086,  1379,  2012,  -818,  2016,  1182,  1674,  -829,  1933,  1414,
    1111,  -822,  1798,  1113,  -822,  -826,  1215,  -826,  -826,  -835,
    -826,  1183,  1123,  -829,  -838,  1125,    92,  1423,  1552,    99,
      70,    71,  1141,   517,  1453,  1410,   433,   533,   522,  1143,
    1131,  1132,  1891,  1786,  1294,  1892,  1238,  1854,   180,  2017,
     346,  1497,  1671,  1802,   197,  1418,  1601,  1814,   671,  1691,
    1511,  1692,   679,  1606,   963,   964,  1248,  1815,  1251,  1362,
    1308,  1832,  1363,  1168,  1437,  1843,  1844,   421,  1179,   750,
    1880,  1390,  1274,  1467,  1881,   698,    72,   382,   650,  1823,
    1468,  1609,   181,   182,   183,   184,   185,  1100,   186,  1531,
    1103,  1443,  1698,  1461,  1199,  1445,  1766,  2066,   904,  1821,
    1330,  1864,  1114,  1770,  1978,  1334,  2018,  1991,  1937,  2037,
    1986,  1088,  2079,  1715,   801,  1385,   807,  1826,   899,  1702,
    1534,  1535,  1536,  1875,  1376,  1537,  1538,  1305,  1388,  1934,
    1703,  1438,   180,  1515,   303,    66,  1950,  1482,  1704,   747,
    1350,    67,  1384,  1239,  1261,  1856,  1757,  1419,  1066,   324,
      73,   829,  1951,     0,  1402,     0,     0,     0,     0,     0,
    1255,     0,   419,     0,  2038,  2040,  2041,     0,  1264,   968,
       0,   352,     0,   969,  1800,   180,     0,     0,   180,  1275,
    1616,     0,  1420,     0,     0,  1279,     0,  1659,   974,     0,
    1399,     0,     0,     0,  1430,  1287,     0,     0,  1409,     0,
       0,     0,     0,  1629,  1909,     0,     0,     0,     0,     0,
    1649,     0,  1460,   421,  1462,  1463,     0,     0,   823,  1311,
       0,     0,   740,     0,     0,     0,     0,  1663,   419,  1708,
    1740,   180,  1548,  1329,  1668,   911,   839,     0,  1392,  1393,
    1684,     0,     0,     0,  1485,     0,  1339,  1714,  1476,     0,
     416,  1342,     0,   818,     0,     0,  1348,     0,     0,     0,
       0,     0,     0,  2015,     0,     0,     0,     0,  1874,     0,
       0,     0,  1976,  1736,  1693,     0,   442,  1368,     0,  1503,
       0,     0,     0,     0,   449,     0,     0,     0,     0,  1712,
    1084,  1294,     0,  1504,     0,     0,  1383,     0,  1507,     0,
     723,     0,     0,     0,     0,     0,     0,   466,     0,     0,
       0,     0,     0,  1394,     0,     0,  1397,     0,     0,     0,
       0,     0,     0,     0,   489,     0,   494,  1471,     0,   494,
    2058,     0,     0,     0,     0,     0,  1474,   968,     0,     0,
       0,   969,   512,  1142,  1427,  1144,     0,  1429,     0,     0,
       0,     0,     0,  1546,  1838,     0,   974,     0,   531,  1436,
    1051,   421,  1533,     0,     0,     0,  1361,     0,     0,     0,
    1402,     0,     0,  1582,   600,  1175,     0,   603,     0,  1444,
    1589,  1114,     0,  1114,     0,  1682,     0,     0,  1563,     0,
       0,     0,   620,  1997,  1613,  1114,     0,     0,     0,  1605,
    1834,     0,  1696,     0,     0,     0,     0,     0,  1550,  2108,
       0,  1014,  1710,     0,     0,  1895,     0,  1020,     0,     0,
     649,     0,     0,     0,     0,     0,  2023,  1712,     0,     0,
       0,     0,  1044,  1620,     0,     0,     0,  1633,     0,     0,
       0,     0,     0,     0,     0,  1849,     0,  1772,  1773,  1651,
       0,     0,     0,   687,     0,  1510,  1780,  2005,     0,     0,
       0,  2051,  1516,  1517,     0,  1519,  1165,     0,     0,  1524,
       0,     0,  1945,     0,     0,     0,     0,  1530,     0,     0,
    1678,  1961,     0,     0,  1877,     0,     0,  1681,  1733,  1899,
       0,     0,     0,     0,  1686,     0,  1866,  1555,     0,     0,
    1819,     0,     0,     0,     0,  1846,     0,     0,  1824,  2088,
    1845,  1562,  1847,     0,     0,     0,     0,  1481,     0,     0,
    2096,   600,  1688,   600,     0,   744,     0,     0,   600,  1586,
    1309,     0,  2002,     0,     0,  1590,     0,  1593,  1926,     0,
       0,     0,     0,  1817,     0,     0,   759,     0,     0,  1548,
       0,  1883,  1604,  1172,  1886,  1229,  1882,  1744,  1884,  1611,
    1792,  1887,     0,     0,  1613,     0,     0,   824,     0,     0,
       0,  1621,  1964,     0,  1758,   792,     0,     0,     0,     0,
    1728,  1743,     0,     0,     0,  2014,   804,     0,     0,     0,
       0,  1920,   180,  1636,  1900,     0,   324,     0,  1921,  1981,
    1755,     0,  1650,     0,     0,  1280,     0,     0,     0,   825,
     516,     0,  1975,     0,  1569,     0,     0,     0,  1572,  1664,
    1665,  1666,     0,     0,     0,     0,  1669,  1670,     0,  1225,
       0,     0,     0,  1677,     0,     0,  1680,     0,  2033,  1810,
    1945,     0,  1935,   856,  1790,   858,     0,     0,  2030,     0,
       0,     0,     0,     0,     0,  2064,     0,   872,  1695,  1697,
       0,  1699,     0,     0,   744,     0,     0,  1093,     0,     0,
    1546,     0,  1713,     0,     0,     0,  1349,  2054,     0,     0,
       0,     0,   620,     0,     0,   896,   394,   633,  -817,   120,
       0,     0,     0,   494,     0,     0,  1952,     0,     0,  1797,
    1861,     0,  1654,  1301,   128,  1455,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   926,     0,
    1957,  1620,  2115,     0,     0,  1550,     0,  1044,  1044,  1759,
       0,  1761,   665,     0,     0,   957,   958,     0,     0,     0,
    1767,   825,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  2044,     0,
       0,     0,  1984,     0,     0,     0,  2013,  1789,   132,     0,
       0,     0,   666,     0,     0,     0,  2061,   959,   667,  1518,
       0,     0,     0,     0,     0,     0,   960,     0,     0,     0,
    1807,     0,     0,     0,  1811,     0,  1813,     0,   180,     0,
       0,  1818,     0,  1820,  1879,     0,     0,     0,     0,     0,
       0,  1748,     0,   466,     0,  2085,     0,     0,     0,     0,
       0,   232,     0,   235,   237,   239,  1050,  1835,     0,  1837,
    1095,     0,     0,     0,   251,  2107,     0,     0,     0,  1851,
    2106,   494,     0,     0,     0,     0,     0,     0,  2112,     0,
       0,     0,  1072,  1073,  1120,     0,     0,   494,  1865,  1939,
       0,  1931,     0,     0,  1952,     0,   512,     0,     0,     0,
       0,  1870,  1954,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   317,     0,  1806,     0,     0,  1957,  1748,
       0,     0,     0,     0,     0,  2060,   825,  2062,  2063,  1903,
       0,     0,     0,   180,     0,     0,  1907,     0,     0,  1136,
       0,     0,     0,  1915,     0,  1554,     0,     0,  1145,     0,
       0,     0,   359,     0,     0,     0,   494,  1998,  1158,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1932,
       0,     0,     0,  2078,     0,  1936,     0,  1938,   391,     0,
       0,   439,  1748,     0,   620,   421,  1960,     0,     0,     0,
       0,  2024,     0,     0,     0,  1523,     0,     0,  2031,     0,
       0,   514,  1044,  1973,  1974,     0,   494,     0,  1977,     0,
       0,  1983,     0,  1985,  1232,     0,     0,  1237,     0,     0,
       0,     0,  1993,     0,     0,  2052,     0,     0,  1213,     0,
       0,     0,     0,   426,  2000,     0,     0,     0,   494,     0,
       0,     0,     0,     0,  1265,     0,     0,  2009,   441,     0,
       0,   421,  1272,  1273,     0,     0,   825,     0,     0,   825,
     520,     0,     0,     0,   514,     0,  2026,  2084,  2028,     0,
    1954,   536,     0,     0,   539,  2034,     0,     0,     0,   324,
    1673,  2094,     0,     0,     0,     0,   825,  2043,     0,     0,
    1617,     0,     0,     0,   825,   825,     0,     0,     0,     0,
       0,     0,  2113,     0,     0,     0,     0,     0,  2117,     0,
       0,     0,     0,     0,   509,     0,     0,     0,  1296,     0,
    2072,     0,     0,     0,  2076,     0,     0,     0,  1720,     0,
       0,     0,     0,     0,     0,   759,     0,   538,   723,     0,
       0,     0,     0,     0,  2087,     0,     0,     0,     0,     0,
       0,     0,   494,     0,     0,   682,     0,  2097,     0,     0,
       0,     0,   649,   613,     0,  2105,  1377,     0,     0,  1344,
    1748,   695,     0,  1748,     0,     0,     0,     0,  1114,     0,
       0,     0,  1114,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1367,     0,     0,   487,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   825,   663,
       0,     0,     0,     0,     0,     0,     0,   681,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1032,   689,  1033,
       0,   692,     0,   694,     0,     0,   696,   697,  1404,   394,
     633,  -817,   120,   173,  1034,  1035,   364,     0,  1752,     0,
       0,     0,     0,     0,   714,     0,     0,   128,     0,     0,
       0,     0,  1763,     0,     0,     0,     0,   896,     0,     0,
       0,     0,     0,     0,  1465,  1466,     0,     0,  1775,  1777,
       0,     0,  1472,     0,     0,   665,     0,     0,   957,   958,
       0,   649,   649,   737,     0,   926,     0,     0,  1213,     0,
    1450,     0,     0,   748,   494,   180,     0,     0,  1490,     0,
       0,  1493,     0,     0,     0,   754,   825,   825,     0,     0,
       0,   132,     0,     0,   825,   666,     0,     0,     0,     0,
     959,   667,     0,     0,     0,     0,     0,   776,     0,   960,
       0,     0,     0,     0,   787,     0,     0,  1487,     0,     0,
     825,     0,     0,   825,     0,     0,   324,   802,     0,     0,
    1500,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   821,     0,     0,
       0,     0,     0,  1863,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   835,     0,  1573,     0,     0,
    1072,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     494,     0,     0,     0,  1890,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   871,
       0,     0,     0,     0,     0,     0,     0,     0,   879,   825,
       0,     0,     0,     0,   882,     0,     0,     0,     0,     0,
       0,  1924,     0,   953,     0,     0,   892,     0,     0,     0,
       0,     0,     0,     0,   983,     0,  1296,     0,     0,     0,
       0,     0,   990,     0,     0,     0,     0,     0,  1615,     0,
    1953,     0,     0,     0,     0,     0,  1658,     0,     0,   922,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   494,  1010,     0,     0,     0,     0,
       0,     0,   945,     0,     0,     0,     0,  1648,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   825,     0,
       0,     0,     0,     0,     0,   989,     0,     0,   991,     0,
       0,     0,     0,     0,     0,     0,  1404,  2011,     0,     0,
       0,     0,     0,     0,     0,  1044,  1044,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   825,     0,     0,     0,     0,     0,  1344,     0,     0,
       0,  1021,     0,  1023,     0,     0,     0,     0,     0,     0,
    1104,     0,     0,  1030,     0,  -818,     0,  -818,  1722,     0,
       0,     0,     0,     0,     0,     0,     0,  -818,     0,  1044,
    -818,  -818,  -818,  -818,  -818,   983,     0,     0,     0,  1739,
       0,   542,  1140,  1068,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1085,     0,   421,     0,
       0,     0,     0,     0,     0,  1167,     0,     0,   421,     0,
       0,     0,     0,  -818,     0,  1450,     0,     0,     0,   543,
    1110,  1112,     0,   421,     0,     0,   544,     0,     0,   545,
       0,   421,  -819,     0,     0,   546,     0,     0,  1785,   547,
     548,     0,     0,  1788,  -818,     0,     0,     0,     0,     0,
     549,     0,  1146,  -818,  1149,   550,   551,     0,     0,  -818,
       0,  1500,   552,  1836,  -819,     0,     0,  1166,     0,     0,
     553,   554,     0,     0,     0,  1615,     0,     0,     0,     0,
       0,     0,  1176,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1188,  1236,     0,     0,
       0,  1833,     0,     0,     0,   825,     0,     0,     0,     0,
       0,     0,  1848,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1208,
       0,     0,     0,     0,     0,     0,   494,     0,     0,     0,
       0,     0,     0,     0,  1220,     0,     0,     0,     0,  1873,
       0,     0,     0,     0,     0,     0,     0,     0,  1885,     0,
       0,     0,     0,     0,     0,  1648,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   620,     0,     0,  1914,
       0,     0,  1268,  1917,  1269,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   466,  1299,  1300,     0,     0,  1236,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1965,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   494,  1328,   620,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1992,  1341,
       0,     0,     0,  1345,     0,     0,     0,     0,     0,     0,
    1999,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   759,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1371,     0,  1372,     0,  2022,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   896,  1389,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1398,  2045,     0,
       0,     0,     0,     0,     0,     0,  2055,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1422,     0,
       0,  2067,     0,     0,  1473,  1428,     0,     0,     0,     0,
    2075,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  2082,     0,     0,     0,     0,     0,     0,
    1491,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1446,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1469,     0,  1470,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1479,     0,
       0,     0,     0,     0,  1484,     0,     0,     0,     0,     0,
       0,  1488,  1489,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1539,     0,     0,
    1010,     0,     0,     0,     0,  1553,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1560,     0,     0,
    1561,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1583,  1584,     0,
       0,     0,  1642,  1588,     0,     0,  1647,     0,     0,     0,
       0,     0,     0,     0,  1656,  1598,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1608,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1626,     0,  1685,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1639,     0,  1643,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1660,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1729,     0,     0,     0,     0,  1732,
       0,     0,     0,  1676,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1741,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1694,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1724,  1725,  1726,  1727,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1735,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1796,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1753,  1805,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1765,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1642,     0,     0,     0,
    1782,     0,     0,     0,     0,     0,     0,  1842,  1796,     0,
       0,     0,   577,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1804,     0,     0,     0,
       0,     0,     0,   612,     0,  1812,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1842,     0,  1825,     0,     0,     0,
       0,     0,  1827,     0,  1647,  1893,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1850,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1858,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1868,     0,
       0,     0,     0,     0,     0,  1876,     0,     0,     0,     0,
       0,     0,     0,     0,  1888,     0,  1889,     0,     0,     0,
       0,     0,  1896,  1897,  1898,     0,     0,     0,     0,     0,
       0,  1904,     0,     0,     0,     0,     0,     0,     0,     0,
    1910,     0,     0,     0,     0,     0,     0,     0,  1919,     0,
       0,  1922,     0,  1923,     0,     0,  1925,     0,     0,     0,
       0,     0,     0,     0,  1996,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1940,     0,     0,     0,  1959,     0,     0,  1962,     0,
    1963,     0,     0,     0,     0,     0,  1966,  1967,  1968,     0,
    1970,     0,  1972,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1987,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1994,     0,     0,
     247,   247,     0,     0,     0,     0,     0,  2001,     0,  2003,
    2004,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,    65,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2027,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   857,     0,
     859,   860,     0,     0,     0,     0,   867,     0,   869,     0,
       0,     0,     0,     0,  2048,     0,  2050,     0,  2053,     0,
    2056,  2057,     0,     0,  2059,     0,     0,     0,     0,     0,
       0,     0,   119,     0,     0,     0,  2069,     0,  2070,     0,
       0,  2073,  2074,     0,     0,  2077,     0,     0,   169,   170,
       0,     0,   175,   176,   177,   178,     0,  2083,     0,     0,
       0,     0,  2086,   200,     0,     0,     0,     0,  2089,  2090,
       0,     0,  2092,     0,  2095,     0,     0,     0,     0,     0,
    2100,  2101,  2102,  2103,  2104,     0,     0,     0,     0,     0,
    2110,     0,  2111,     0,     0,  2114,     0,     0,     0,  2116,
       0,  2118,  2119,  2120,   249,  2121,  2122,  2123,  2124,     0,
       0,  2125,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   993,     0,   995,   302,
       0,     0,   304,   305,   306,   307,   308,   309,   310,   311,
     312,   313,   314,   315,   316,     0,  1015,     0,     0,     0,
       0,     0,     0,     0,     0,  1024,     0,     0,     0,   463,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   347,     0,     0,     0,     0,     0,     0,
       0,   356,     0,     0,  1059,   501,     0,     0,   363,     0,
       0,     0,     0,   370,   371,   372,   373,   374,   375,   376,
       0,     0,     0,     0,     0,     0,     0,   524,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     611,  1121,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1157,     0,     0,     0,   652,     0,     0,     0,   656,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   452,     0,     0,     0,     0,     0,   456,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1193,   468,   469,   470,   471,   472,   473,
     474,   475,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   715,     0,     0,     0,
       0,   504,   505,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     247,     0,     0,     0,     0,   605,     0,     0,     0,   607,
       0,     0,  1256,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   769,     0,
       0,     0,   630,   631,     0,  1278,     0,   777,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   803,     0,
       0,     0,     0,     0,   676,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     880,     0,     0,     0,   736,   883,   247,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     749,     0,     0,     0,     0,     0,     0,   902,     0,     0,
       0,     0,     0,     0,   760,     0,     0,     0,     0,     0,
       0,     0,     0,   770,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   777,     0,     0,     0,   777,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   946,   947,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   956,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   827,     0,     0,     0,
       0,     0,     0,  1458,     0,     0,     0,     0,     0,     0,
     838,     0,   840,   841,   842,   843,   844,   845,   846,   847,
     848,   849,   850,   851,   852,     0,     0,     0,     0,     0,
     157,     0,     0,     0,     0,     0,     0,     0,  1018,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1506,     0,   888,     0,     0,     0,     0,     0,
       0,     0,   897,     0,     0,     0,     0,  1520,     0,     0,
       0,     0,   905,     0,     0,   909,     0,  1074,     0,     0,
       0,     0,     0,  1082,     0,     0,   918,   919,     0,     0,
       0,     0,     0,  1090,     0,     0,     0,  1097,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   941,
     942,   943,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1571,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1594,     0,  1147,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1610,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1012,  1013,     0,     0,     0,     0,
       0,     0,     0,   368,     0,     0,     0,  1025,     0,     0,
    1191,  1192,     0,     0,     0,     0,     0,     0,     0,  1049,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   777,   777,     0,  1058,     0,     0,     0,     0,     0,
       0,     0,  1082,     0,     0,     0,     0,     0,     0,     0,
    1218,  1076,     0,     0,  1080,     0,     0,     0,     0,     0,
    1087,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1240,     0,     0,     0,     0,     0,
     429,     0,     0,     0,     0,     0,     0,     0,     0,  1254,
       0,     0,     0,     0,     0,  1458,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1730,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1155,     0,     0,     0,     0,     0,  1288,     0,     0,  1742,
       0,     0,     0,     0,   476,     0,     0,     0,     0,     0,
    1751,     0,     0,     0,     0,     0,     0,     0,  1177,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   946,
       0,     0,  1327,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1197,     0,     0,  1778,  1779,     0,
       0,     0,     0,  1218,     0,     0,     0,     0,     0,     0,
       0,  1353,     0,     0,     0,  1356,     0,  1359,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   614,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1231,     0,     0,     0,     0,     0,     0,  1241,  1242,  1243,
    1244,     0,     0,   640,     0,     0,     0,     0,     0,     0,
       0,   653,     0,   655,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   664,     0,     0,     0,   677,  1271,     0,
       0,     0,     0,     0,     0,     0,  1855,     0,     0,     0,
       0,  1859,     0,  1860,     0,     0,     0,     0,     0,     0,
    1291,  1292,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1878,     0,     0,
       0,     0,     0,     0,     0,  1312,  1313,  1314,  1315,     0,
       0,  1894,     0,     0,     0,     0,     0,     0,     0,  1332,
       0,  1457,     0,     0,     0,     0,  1337,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1354,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   755,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1373,  1374,     0,     0,     0,  1958,     0,     0,     0,     0,
       0,     0,     0,     0,  1386,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1405,     0,  1407,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   815,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1557,
       0,   463,  1353,  2006,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1574,     0,     0,     0,     0,     0,  1585,
       0,     0,     0,     0,     0,     0,     0,  1592,  1452,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   876,     0,
       0,     0,     0,     0,     0,     0,  2049,     0,     0,     0,
       0,     0,     0,   885,     0,     0,     0,     0,     0,     0,
     893,     0,     0,     0,     0,     0,  2068,     0,     0,     0,
       0,     0,  1628,     0,     0,  1630,     0,  1492,     0,     0,
       0,     0,  1634,     0,     0,  1640,   916,   611,  1457,     0,
       0,     0,  1505,     0,     0,     0,   927,     0,   929,     0,
    1657,     0,     0,     0,   935,     0,     0,     0,     0,     0,
       0,     0,     0,  1525,  1526,  1527,  1528,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1711,  1570,     0,     0,     0,     0,     0,     0,  1719,     0,
       0,     0,  1723,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1599,
    1600,     0,     0,     0,     0,     0,     0,  1031,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1056,  1756,     0,     0,     0,     0,
       0,  1097,     0,     0,     0,     0,     0,  1069,     0,     0,
       0,     0,     0,     0,     0,  1079,     0,  1082,     0,  1082,
       0,     0,     0,     0,     0,     0,     0,     0,  1655,     0,
       0,     0,     0,  1787,     0,     0,     0,     0,     0,  1791,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1801,     0,     0,     0,  1122,     0,     0,     0,     0,
       0,     0,  1130,     0,     0,     0,     0,     0,  1138,  1139,
       0,     0,     0,     0,     0,     0,     0,     0,  1218,     0,
    1218,     0,     0,     0,     0,     0,  1700,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1171,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1731,     0,     0,     0,  1857,     0,     0,
    1189,     0,  1737,  1738,     0,     0,     0,     0,     0,     0,
    1628,     0,     0,     0,  1195,     0,     0,     0,     0,     0,
    1750,     0,     0,     0,     0,     0,     0,     0,  1206,     0,
       0,     0,     0,  1209,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1918,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1252,     0,  1930,     0,     0,     0,  1803,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1822,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1303,     0,     0,     0,     0,     0,     0,  1989,
    1990,     0,     0,     0,     0,     0,     0,  1995,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1335,  1336,  2010,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1346,     0,
    2021,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1360,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  2039,     0,  2042,     0,   946,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1382,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1929,     0,     0,     0,  2071,     0,
       0,     0,     0,  1400,     0,     0,     0,     0,     0,  1557,
       0,   463,  1353,  1412,     0,   946,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1640,   611,
    1457,     0,     0,     0,     0,     0,     0,     0,  2098,  2099,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2109,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1447,     0,     0,     0,
       0,     0,  1454,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2025,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1483,     0,     0,
       0,     0,     0,  1486,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1540,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1603,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1627,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1635,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1687,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1717,  1718,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1745,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1781,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1828,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1869,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1902,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1911,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1941,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1988,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2046
};

static const yytype_int16 yycheck[] =
{
      21,    22,    23,    24,   258,   117,   238,   247,   110,   326,
     512,   586,   240,   291,   461,   115,   400,   444,   421,   403,
     825,   166,   225,   197,   825,  1031,   485,   524,   887,    50,
     756,   812,   491,  1040,   784,   944,   495,  1124,  1069,   825,
     197,  1031,   136,    51,  1007,    66,    67,  1191,  1192,  1179,
     670,  1072,   252,  1311,  1209,  1073,   243,    78,    79,  1462,
    1111,  1463,  1113,   162,  1559,   642,    87,    88,    89,    90,
     215,  1082,  1628,   167,  1125,   224,   354,   226,   227,   228,
     229,  1604,   431,   531,   105,  1402,   435,   430,  1281,   716,
     433,   434,   232,  1914,   819,   235,  1148,   237,   861,   239,
     121,   651,  1875,  1914,   654,    69,   634,   635,    30,   254,
     448,  1206,    20,  1753,    41,   136,  1629,    44,   804,    41,
       0,  1086,    44,   144,   145,   146,   147,   148,   149,   150,
    1555,    28,   153,    41,    31,  1419,    44,   258,   102,   103,
     261,    11,     6,    78,    41,    30,   167,    44,     0,    31,
     428,    73,   246,   174,   248,     9,  1301,    29,   179,   362,
     360,   588,    41,   291,    78,    44,  1253,   188,   517,    41,
       6,   106,    44,    27,  1697,   196,  1816,   317,   318,   528,
     107,    76,    31,   969,   533,  1196,   872,    31,    73,     0,
      60,    73,    41,   321,   334,    44,   977,    41,     8,  1712,
      44,  1485,    97,  1168,   407,   119,   409,   134,   134,    19,
       5,  1636,   134,  1406,    94,    95,    65,    93,   496,    99,
      84,   499,    86,     6,   104,  1418,   134,   151,   152,    73,
     154,   155,   156,   241,   512,   256,   160,   134,  1761,   442,
    1335,  1336,   399,   446,   401,     5,   126,   775,    84,   125,
     344,  2024,  1677,   591,    32,    31,    54,   595,   512,   136,
      58,  2082,   134,    41,   413,    41,    44,   135,    44,   994,
     234,  2082,   293,   294,   295,   296,   297,   298,   299,   300,
     301,    31,   303,    94,    95,   134,   426,    82,    99,     9,
     134,    41,  1436,   104,    44,   722,  1613,   397,   725,    31,
     321,    84,  1028,    86,  1497,    27,     4,    97,   103,    29,
      16,  2084,  1062,  1063,  1095,   126,     6,  1226,  1227,  1228,
    1125,  1342,    82,   825,   332,     4,    24,    31,  1521,   788,
     789,    10,   134,    65,   455,   343,    86,    41,   459,    61,
      44,    47,  1865,   103,   501,   366,   467,     7,    54,    41,
       8,    78,    44,   453,   697,   476,   134,   378,   134,   508,
     677,   382,     9,   686,  2004,    23,   515,  2007,     5,    29,
     920,   921,    27,  1098,    29,     5,  1428,   825,  1695,   106,
     580,    28,    86,   767,   134,   585,  1942,   587,    25,     7,
       6,     9,    52,   414,    84,    25,    86,     9,    53,   420,
    1163,   929,   502,    31,    20,   109,    31,    99,    24,  1932,
      28,   823,   104,    76,    72,   423,    28,    11,    12,    13,
      14,    15,   834,    17,   599,    28,   434,    52,    31,    32,
     134,   137,   138,   139,   140,   141,   142,   143,    31,     6,
    1585,     4,   134,  1454,     8,    82,  1171,    20,    24,    52,
     108,  1974,    82,    20,    36,    37,    38,    39,    40,   480,
      24,    24,    35,     8,    46,   486,   103,    49,    84,    85,
      86,   794,    88,   103,    91,   798,    92,   880,    23,  1056,
     883,   135,   136,  1264,    41,   506,   835,    44,    45,   113,
      47,    48,    20,    50,     6,   640,   596,   729,  1691,    81,
      11,     3,   601,  1123,    98,   129,   130,    89,    20,   637,
      32,   127,   661,   641,    16,    26,    18,    84,    85,    86,
     541,    88,   650,  1286,   652,    92,     4,    31,   110,  1057,
    1293,   915,    10,     7,  1540,    95,   118,  1548,     9,    99,
     727,   681,   124,   946,   104,   251,    20,   825,   130,   689,
    1540,    22,   692,  1516,   694,   106,   577,   578,   579,    86,
     127,   582,   583,   738,   115,    28,   126,   724,   589,   590,
      94,    43,    84,    85,    86,   281,    88,   134,     4,  1006,
      92,  1331,    11,     9,  1334,   106,  2081,   865,    19,  1164,
     684,   115,     2,  1052,   115,   695,    28,    28,   619,    31,
      10,    11,  1061,    35,     7,     8,   627,  1752,    94,    95,
       9,    91,   769,    99,    99,   127,  1000,  1020,   104,   104,
      27,   715,    29,    16,   947,    18,   647,   648,    27,    28,
      29,    30,   981,   956,    84,    85,  1095,   980,  1401,   982,
     126,    88,    74,  1090,     8,    92,    53,   353,   747,   355,
       3,   779,   673,   359,   675,   855,  1801,    20,  1421,  2062,
     584,  2063,   586,    16,   597,    18,  1050,    28,    29,   602,
     691,  1199,    28,    29,    41,    30,  1303,    44,    45,   866,
      47,    48,   114,    50,    45,    16,   831,    18,   120,    45,
    1708,    52,  1710,    88,     7,  1018,    52,    92,    60,    61,
     828,    11,    12,    13,    14,    15,   946,    17,   802,   803,
      60,    61,  1475,   734,  1869,    31,   737,    16,  1863,    18,
      41,    43,   743,    44,  1151,   953,  1153,   430,   877,    28,
     433,   752,    31,    32,    33,    34,    35,    94,  1501,    28,
      97,    21,   344,  1240,   765,     9,  1891,  1892,    66,  1512,
     674,  2009,   773,    45,    31,    47,    48,    28,    50,   916,
     781,   782,    10,    11,  1785,    31,    86,   134,    31,  1172,
     791,    20,   108,   479,   974,    74,   797,   952,   935,   800,
    1400,   893,  1131,    71,     8,  1930,   807,     3,    98,  1132,
     714,   105,    55,    56,  1221,    23,    59,  1828,   322,    62,
      51,    64,   326,    24,     4,    68,    69,    70,   108,   445,
      72,   447,   336,     9,   450,   114,   418,  1095,  1346,    57,
       4,   120,    79,   998,    28,    29,    30,  1239,  1591,     3,
      23,    27,    28,    29,    30,   107,   857,    51,   859,   860,
      84,    85,    86,   864,   368,  1989,  1990,  1977,   869,  1612,
      11,    12,    13,    14,    15,    75,    17,   878,  1270,   127,
      72,    29,    22,  1275,  1491,   107,   122,   130,    21,   890,
     110,  1298,   396,   397,    23,  1302,   112,    83,   402,    31,
     404,   405,    78,    63,   121,  1234,  1089,    43,   412,    78,
    1233,   912,  1235,    75,    28,    29,   127,    88,    86,  2043,
      23,   106,    36,    37,   102,    10,    39,  1657,  1914,    88,
     106,    45,  1359,  2000,   109,   102,    93,   109,    52,  1119,
     626,  1684,    19,  1126,  1914,   449,    10,   948,  1555,   453,
     938,  1254,  1983,     7,  1985,  1941,    19,     6,  1901,     6,
     961,    41,    24,   964,    44,    45,  1074,    47,    48,    86,
      50,  1941,   973,    83,    30,   976,    45,  1300,  1455,    52,
      38,    38,   988,   423,  1348,  1288,   341,   434,   427,   990,
     980,   980,  1849,  1723,  1161,  1849,  1104,  1805,   502,  1986,
     988,  1403,  1590,  1745,   118,  1295,  1510,  1759,   512,  1614,
    1417,  1615,   516,  1515,   825,   825,  1114,  1760,  1117,  1233,
    1175,  2022,  1233,  1011,  1327,  1793,  1793,   531,  1029,   611,
    1839,  1270,  1140,  1362,  1839,   539,    41,   270,   495,  1769,
    1363,  1518,    11,    12,    13,    14,    15,   951,    17,  1444,
     954,  1339,  1621,  1356,  1055,  1342,  1699,  2044,  1195,  1767,
    1195,  1818,   966,  1709,  1941,  1202,  1988,  1952,  1907,  2010,
    1945,   940,  2060,  1635,   656,  1262,   664,  1781,   764,  1627,
    1447,  1447,  1447,  1836,  1251,  1447,  1447,  1170,  1264,  1902,
    1627,  1329,   596,  1422,   199,    35,  2082,  1382,  1627,   603,
    1220,    35,  1260,  1104,  1125,  1807,  1687,  1296,   923,   223,
      41,   687,  2082,    -1,  1281,    -1,    -1,    -1,    -1,    -1,
    1121,    -1,  1419,    -1,  2013,  2014,  2015,    -1,  1129,  1914,
      -1,   245,    -1,  1914,  1741,   639,    -1,    -1,   642,  1140,
    1523,    -1,  1297,    -1,    -1,  1146,    -1,  1574,  1914,    -1,
    1279,    -1,    -1,    -1,  1309,  1156,    -1,    -1,  1287,    -1,
      -1,    -1,    -1,  1546,  1870,    -1,    -1,    -1,    -1,    -1,
    1562,    -1,  1355,   677,  1357,  1358,    -1,    -1,   682,  1180,
      -1,    -1,  1262,    -1,    -1,    -1,    -1,  1579,  1485,  1628,
    1667,   695,  1450,  1194,  1586,   777,   700,    -1,  1272,  1273,
    1607,    -1,    -1,    -1,  1387,    -1,  1207,  1634,  1375,    -1,
     324,  1212,    -1,  1117,    -1,    -1,  1217,    -1,    -1,    -1,
      -1,    -1,    -1,  1984,    -1,    -1,    -1,    -1,  1835,    -1,
      -1,    -1,  1938,  1660,  1617,    -1,   350,  1238,    -1,  1406,
      -1,    -1,    -1,    -1,   358,    -1,    -1,    -1,    -1,  1632,
     936,  1418,    -1,  1408,    -1,    -1,  1257,    -1,  1413,    -1,
    1164,    -1,    -1,    -1,    -1,    -1,    -1,   381,    -1,    -1,
      -1,    -1,    -1,  1274,    -1,    -1,  1277,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   398,    -1,   400,  1367,    -1,   403,
    2033,    -1,    -1,    -1,    -1,    -1,  1370,  2082,    -1,    -1,
      -1,  2082,   416,   989,  1305,   991,    -1,  1308,    -1,    -1,
      -1,    -1,    -1,  1450,  1791,    -1,  2082,    -1,   432,  1320,
     902,   825,  1447,    -1,    -1,    -1,  1230,    -1,    -1,    -1,
    1497,    -1,    -1,  1487,   448,  1021,    -1,   451,    -1,  1340,
    1495,  1245,    -1,  1247,    -1,  1603,    -1,    -1,  1473,    -1,
      -1,    -1,   466,  1960,  1521,  1259,    -1,    -1,    -1,  1514,
    1787,    -1,  1620,    -1,    -1,    -1,    -1,    -1,  1450,  2099,
      -1,   875,  1630,    -1,    -1,  1852,    -1,   881,    -1,    -1,
     494,    -1,    -1,    -1,    -1,    -1,  1993,  1770,    -1,    -1,
      -1,    -1,   896,  1530,    -1,    -1,    -1,  1552,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1797,    -1,  1710,  1711,  1564,
      -1,    -1,    -1,   527,    -1,  1416,  1719,  1972,    -1,    -1,
      -1,  2028,  1423,  1424,    -1,  1426,  1008,    -1,    -1,  1430,
      -1,    -1,  1914,    -1,    -1,    -1,    -1,  1438,    -1,    -1,
    1595,  1918,    -1,    -1,  1836,    -1,    -1,  1602,  1656,  1856,
      -1,    -1,    -1,    -1,  1609,    -1,  1820,  1458,    -1,    -1,
    1763,    -1,    -1,    -1,    -1,  1794,    -1,    -1,  1771,  2076,
    1793,  1472,  1795,    -1,    -1,    -1,    -1,  1381,    -1,    -1,
    2087,   595,  1611,   597,    -1,   599,    -1,    -1,   602,  1490,
    1176,    -1,  1969,    -1,    -1,  1496,    -1,  1498,  1890,    -1,
      -1,    -1,    -1,  1761,    -1,    -1,   620,    -1,    -1,  1767,
      -1,  1840,  1513,  1017,  1843,  1097,  1839,  1672,  1841,  1520,
    1732,  1844,    -1,    -1,  1691,    -1,    -1,   683,    -1,    -1,
      -1,  1532,  1924,    -1,  1689,   649,    -1,    -1,    -1,    -1,
    1648,  1670,    -1,    -1,    -1,  1984,   660,    -1,    -1,    -1,
      -1,  1880,  1056,  1554,  1857,    -1,   670,    -1,  1881,  1942,
    1685,    -1,  1563,    -1,    -1,  1147,    -1,    -1,    -1,   683,
    1953,    -1,  1936,    -1,  1478,    -1,    -1,    -1,  1482,  1580,
    1581,  1582,    -1,    -1,    -1,    -1,  1587,  1588,    -1,  1093,
      -1,    -1,    -1,  1594,    -1,    -1,  1597,    -1,  2005,  1754,
    2082,    -1,  1905,   717,  1729,   719,    -1,    -1,  2000,    -1,
      -1,    -1,    -1,    -1,    -1,  2042,    -1,   731,  1619,  1620,
      -1,  1622,    -1,    -1,   738,    -1,    -1,  2010,    -1,    -1,
    1767,    -1,  1633,    -1,    -1,    -1,  1218,  2029,    -1,    -1,
      -1,    -1,   756,    -1,    -1,   759,    28,    29,    30,    31,
      -1,    -1,    -1,   767,    -1,    -1,  1914,    -1,    -1,  1739,
    1815,    -1,  1566,  1167,    46,  1351,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   792,    -1,
    1914,  1818,  2109,    -1,    -1,  1767,    -1,  1191,  1192,  1690,
      -1,  1692,    74,    -1,    -1,    77,    78,    -1,    -1,    -1,
    1701,   815,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2021,    -1,
      -1,    -1,  1942,    -1,    -1,    -1,  1984,  1728,   110,    -1,
      -1,    -1,   114,    -1,    -1,    -1,  2039,   119,   120,  1425,
      -1,    -1,    -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,
    1751,    -1,    -1,    -1,  1755,    -1,  1757,    -1,  1262,    -1,
      -1,  1762,    -1,  1764,  1838,    -1,    -1,    -1,    -1,    -1,
      -1,  1675,    -1,   887,    -1,  2072,    -1,    -1,    -1,    -1,
      -1,   152,    -1,   154,   155,   156,   900,  1788,    -1,  1790,
    2010,    -1,    -1,    -1,   165,  2098,    -1,    -1,    -1,  1800,
    2097,   915,    -1,    -1,    -1,    -1,    -1,    -1,  2105,    -1,
      -1,    -1,   926,   927,   970,    -1,    -1,   931,  1819,  1911,
      -1,  1895,    -1,    -1,  2082,    -1,   940,    -1,    -1,    -1,
      -1,  1832,  1914,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   214,    -1,  1749,    -1,    -1,  2082,  1753,
      -1,    -1,    -1,    -1,    -1,  2038,   970,  2040,  2041,  1860,
      -1,    -1,    -1,  1367,    -1,    -1,  1867,    -1,    -1,   983,
      -1,    -1,    -1,  1874,    -1,  1457,    -1,    -1,   992,    -1,
      -1,    -1,   253,    -1,    -1,    -1,  1000,  1961,  1002,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1900,
      -1,    -1,    -1,  2058,    -1,  1906,    -1,  1908,   279,    -1,
      -1,   346,  1816,    -1,  1028,  1419,  1917,    -1,    -1,    -1,
      -1,  1995,    -1,    -1,    -1,  1429,    -1,    -1,  2002,    -1,
      -1,  1513,  1436,  1934,  1935,    -1,  1050,    -1,  1939,    -1,
      -1,  1942,    -1,  1944,  1100,    -1,    -1,  1103,    -1,    -1,
      -1,    -1,  1953,    -1,    -1,  2029,    -1,    -1,  1072,    -1,
      -1,    -1,    -1,   334,  1965,    -1,    -1,    -1,  1082,    -1,
      -1,    -1,    -1,    -1,  1130,    -1,    -1,  1978,   349,    -1,
      -1,  1485,  1138,  1139,    -1,    -1,  1100,    -1,    -1,  1103,
     425,    -1,    -1,    -1,  1576,    -1,  1997,  2071,  1999,    -1,
    2082,   436,    -1,    -1,   439,  2006,    -1,    -1,    -1,  1123,
    1592,  2085,    -1,    -1,    -1,    -1,  1130,  2018,    -1,    -1,
    1524,    -1,    -1,    -1,  1138,  1139,    -1,    -1,    -1,    -1,
      -1,    -1,  2106,    -1,    -1,    -1,    -1,    -1,  2112,    -1,
      -1,    -1,    -1,    -1,   415,    -1,    -1,    -1,  1162,    -1,
    2051,    -1,    -1,    -1,  2055,    -1,    -1,    -1,  1640,    -1,
      -1,    -1,    -1,    -1,    -1,  1179,    -1,   438,  1972,    -1,
      -1,    -1,    -1,    -1,  2075,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1196,    -1,    -1,   520,    -1,  2088,    -1,    -1,
      -1,    -1,  1206,   464,    -1,  2096,  1252,    -1,    -1,  1213,
    2004,   536,    -1,  2007,    -1,    -1,    -1,    -1,  2012,    -1,
      -1,    -1,  2016,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1236,    -1,    -1,  1629,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1252,   510,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   518,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    16,   529,    18,
      -1,   532,    -1,   534,    -1,    -1,   537,   538,  1282,    28,
      29,    30,    31,    32,    33,    34,    35,    -1,  1682,    -1,
      -1,    -1,    -1,    -1,   555,    -1,    -1,    46,    -1,    -1,
      -1,    -1,  1696,    -1,    -1,    -1,    -1,  1311,    -1,    -1,
      -1,    -1,    -1,    -1,  1360,  1361,    -1,    -1,  1712,  1713,
      -1,    -1,  1368,    -1,    -1,    74,    -1,    -1,    77,    78,
      -1,  1335,  1336,   594,    -1,  1339,    -1,    -1,  1342,    -1,
    1344,    -1,    -1,   604,  1348,  1739,    -1,    -1,  1394,    -1,
      -1,  1397,    -1,    -1,    -1,   616,  1360,  1361,    -1,    -1,
      -1,   110,    -1,    -1,  1368,   114,    -1,    -1,    -1,    -1,
     119,   120,    -1,    -1,    -1,    -1,    -1,   638,    -1,   128,
      -1,    -1,    -1,    -1,   645,    -1,    -1,  1391,    -1,    -1,
    1394,    -1,    -1,  1397,    -1,    -1,  1400,   658,    -1,    -1,
    1404,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   678,    -1,    -1,
      -1,    -1,    -1,  1817,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   696,    -1,  1483,    -1,    -1,
    1444,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1454,    -1,    -1,    -1,  1848,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   730,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   739,  1483,
      -1,    -1,    -1,    -1,   745,    -1,    -1,    -1,    -1,    -1,
      -1,  1885,    -1,   818,    -1,    -1,   757,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   829,    -1,  1510,    -1,    -1,    -1,
      -1,    -1,   837,    -1,    -1,    -1,    -1,    -1,  1522,    -1,
    1914,    -1,    -1,    -1,    -1,    -1,  1572,    -1,    -1,   790,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1548,   870,    -1,    -1,    -1,    -1,
      -1,    -1,   813,    -1,    -1,    -1,    -1,  1561,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1572,    -1,
      -1,    -1,    -1,    -1,    -1,   836,    -1,    -1,   839,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1590,  1981,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1989,  1990,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1615,    -1,    -1,    -1,    -1,    -1,  1621,    -1,    -1,
      -1,   882,    -1,   884,    -1,    -1,    -1,    -1,    -1,    -1,
     955,    -1,    -1,   894,    -1,    16,    -1,    18,  1642,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    28,    -1,  2043,
      31,    32,    33,    34,    35,   980,    -1,    -1,    -1,  1663,
      -1,    42,   987,   924,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   937,    -1,  2072,    -1,
      -1,    -1,    -1,    -1,    -1,  1010,    -1,    -1,  2082,    -1,
      -1,    -1,    -1,    74,    -1,  1699,    -1,    -1,    -1,    80,
     961,   962,    -1,  2097,    -1,    -1,    87,    -1,    -1,    90,
      -1,  2105,    93,    -1,    -1,    96,    -1,    -1,  1722,   100,
     101,    -1,    -1,  1727,   105,    -1,    -1,    -1,    -1,    -1,
     111,    -1,   993,   114,   995,   116,   117,    -1,    -1,   120,
      -1,  1745,   123,  1789,   125,    -1,    -1,  1008,    -1,    -1,
     131,   132,    -1,    -1,    -1,  1759,    -1,    -1,    -1,    -1,
      -1,    -1,  1023,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1037,  1102,    -1,    -1,
      -1,  1785,    -1,    -1,    -1,  1789,    -1,    -1,    -1,    -1,
      -1,    -1,  1796,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1070,
      -1,    -1,    -1,    -1,    -1,    -1,  1820,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1085,    -1,    -1,    -1,    -1,  1833,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1842,    -1,
      -1,    -1,    -1,    -1,    -1,  1849,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1870,    -1,    -1,  1873,
      -1,    -1,  1133,  1877,  1135,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1907,  1165,  1166,    -1,    -1,  1233,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1926,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1936,  1194,  1938,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1952,  1210,
      -1,    -1,    -1,  1214,    -1,    -1,    -1,    -1,    -1,    -1,
    1964,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1977,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1245,    -1,  1247,    -1,  1992,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2009,  1267,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1278,  2022,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2030,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1299,    -1,
      -1,  2045,    -1,    -1,  1369,  1306,    -1,    -1,    -1,    -1,
    2054,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2067,    -1,    -1,    -1,    -1,    -1,    -1,
    1395,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1343,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1364,    -1,  1366,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1379,    -1,
      -1,    -1,    -1,    -1,  1385,    -1,    -1,    -1,    -1,    -1,
      -1,  1392,  1393,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1448,    -1,    -1,
    1515,    -1,    -1,    -1,    -1,  1456,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1468,    -1,    -1,
    1471,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1488,  1489,    -1,
      -1,    -1,  1557,  1494,    -1,    -1,  1561,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1569,  1506,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1517,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1542,    -1,  1608,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1556,    -1,  1558,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1575,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1649,    -1,    -1,    -1,    -1,  1654,
      -1,    -1,    -1,  1594,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1668,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1619,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1644,  1645,  1646,  1647,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1659,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1733,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1683,  1748,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1698,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1781,    -1,    -1,    -1,
    1721,    -1,    -1,    -1,    -1,    -1,    -1,  1792,  1793,    -1,
      -1,    -1,   442,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1747,    -1,    -1,    -1,
      -1,    -1,    -1,   463,    -1,  1756,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1839,    -1,  1777,    -1,    -1,    -1,
      -1,    -1,  1783,    -1,  1849,  1850,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1799,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1809,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1829,    -1,
      -1,    -1,    -1,    -1,    -1,  1836,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1845,    -1,  1847,    -1,    -1,    -1,
      -1,    -1,  1853,  1854,  1855,    -1,    -1,    -1,    -1,    -1,
      -1,  1862,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1871,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1879,    -1,
      -1,  1882,    -1,  1884,    -1,    -1,  1887,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1959,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1912,    -1,    -1,    -1,  1916,    -1,    -1,  1919,    -1,
    1921,    -1,    -1,    -1,    -1,    -1,  1927,  1928,  1929,    -1,
    1931,    -1,  1933,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1946,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1958,    -1,    -1,
     161,   162,    -1,    -1,    -1,    -1,    -1,  1968,    -1,  1970,
    1971,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    31,    32,    33,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1998,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   718,    -1,
     720,   721,    -1,    -1,    -1,    -1,   726,    -1,   728,    -1,
      -1,    -1,    -1,    -1,  2025,    -1,  2027,    -1,  2029,    -1,
    2031,  2032,    -1,    -1,  2035,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    91,    -1,    -1,    -1,  2047,    -1,  2049,    -1,
      -1,  2052,  2053,    -1,    -1,  2056,    -1,    -1,   107,   108,
      -1,    -1,   111,   112,   113,   114,    -1,  2068,    -1,    -1,
      -1,    -1,  2073,   122,    -1,    -1,    -1,    -1,  2079,  2080,
      -1,    -1,  2083,    -1,  2085,    -1,    -1,    -1,    -1,    -1,
    2091,  2092,  2093,  2094,  2095,    -1,    -1,    -1,    -1,    -1,
    2101,    -1,  2103,    -1,    -1,  2106,    -1,    -1,    -1,  2110,
      -1,  2112,  2113,  2114,   163,  2116,  2117,  2118,  2119,    -1,
      -1,  2122,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   856,    -1,   858,   198,
      -1,    -1,   201,   202,   203,   204,   205,   206,   207,   208,
     209,   210,   211,   212,   213,    -1,   876,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   885,    -1,    -1,    -1,   380,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   242,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   250,    -1,    -1,   914,   406,    -1,    -1,   257,    -1,
      -1,    -1,    -1,   262,   263,   264,   265,   266,   267,   268,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   428,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     461,   971,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1001,    -1,    -1,    -1,   496,    -1,    -1,    -1,   500,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   361,    -1,    -1,    -1,    -1,    -1,   367,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1043,   383,   384,   385,   386,   387,   388,
     389,   390,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   557,    -1,    -1,    -1,
      -1,   410,   411,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     601,    -1,    -1,    -1,    -1,   454,    -1,    -1,    -1,   458,
      -1,    -1,  1122,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   629,    -1,
      -1,    -1,   481,   482,    -1,  1145,    -1,   638,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   659,    -1,
      -1,    -1,    -1,    -1,   513,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     741,    -1,    -1,    -1,   593,   746,   747,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     609,    -1,    -1,    -1,    -1,    -1,    -1,   768,    -1,    -1,
      -1,    -1,    -1,    -1,   623,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   632,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   794,    -1,    -1,    -1,   798,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   813,   814,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   824,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   685,    -1,    -1,    -1,
      -1,    -1,    -1,  1353,    -1,    -1,    -1,    -1,    -1,    -1,
     699,    -1,   701,   702,   703,   704,   705,   706,   707,   708,
     709,   710,   711,   712,   713,    -1,    -1,    -1,    -1,    -1,
      97,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   879,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1412,    -1,   753,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   761,    -1,    -1,    -1,    -1,  1427,    -1,    -1,
      -1,    -1,   771,    -1,    -1,   774,    -1,   928,    -1,    -1,
      -1,    -1,    -1,   934,    -1,    -1,   785,   786,    -1,    -1,
      -1,    -1,    -1,   944,    -1,    -1,    -1,   948,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   808,
     809,   810,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1481,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1500,    -1,   993,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1519,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   873,   874,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   260,    -1,    -1,    -1,   886,    -1,    -1,
    1041,  1042,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   898,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1062,  1063,    -1,   913,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1073,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1081,   930,    -1,    -1,   933,    -1,    -1,    -1,    -1,    -1,
     939,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1105,    -1,    -1,    -1,    -1,    -1,
     337,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1120,
      -1,    -1,    -1,    -1,    -1,  1635,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1650,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     999,    -1,    -1,    -1,    -1,    -1,  1157,    -1,    -1,  1669,
      -1,    -1,    -1,    -1,   391,    -1,    -1,    -1,    -1,    -1,
    1680,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1027,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1190,
      -1,    -1,  1193,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1053,    -1,    -1,  1717,  1718,    -1,
      -1,    -1,    -1,  1214,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1222,    -1,    -1,    -1,  1226,    -1,  1228,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   464,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1099,    -1,    -1,    -1,    -1,    -1,    -1,  1106,  1107,  1108,
    1109,    -1,    -1,   490,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   498,    -1,   500,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   510,    -1,    -1,    -1,   514,  1137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1806,    -1,    -1,    -1,
      -1,  1811,    -1,  1813,    -1,    -1,    -1,    -1,    -1,    -1,
    1159,  1160,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1837,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1184,  1185,  1186,  1187,    -1,
      -1,  1851,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1198,
      -1,  1352,    -1,    -1,    -1,    -1,  1205,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1224,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   616,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1249,  1250,    -1,    -1,    -1,  1915,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1263,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1283,    -1,  1285,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   673,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1460,
      -1,  1462,  1463,  1973,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1484,    -1,    -1,    -1,    -1,    -1,  1490,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1498,  1347,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   735,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2026,    -1,    -1,    -1,
      -1,    -1,    -1,   750,    -1,    -1,    -1,    -1,    -1,    -1,
     757,    -1,    -1,    -1,    -1,    -1,  2046,    -1,    -1,    -1,
      -1,    -1,  1543,    -1,    -1,  1546,    -1,  1396,    -1,    -1,
      -1,    -1,  1553,    -1,    -1,  1556,   783,  1558,  1559,    -1,
      -1,    -1,  1411,    -1,    -1,    -1,   793,    -1,   795,    -1,
    1571,    -1,    -1,    -1,   801,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1432,  1433,  1434,  1435,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1631,  1480,    -1,    -1,    -1,    -1,    -1,    -1,  1639,    -1,
      -1,    -1,  1643,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1508,
    1509,    -1,    -1,    -1,    -1,    -1,    -1,   894,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   911,  1686,    -1,    -1,    -1,    -1,
      -1,  1692,    -1,    -1,    -1,    -1,    -1,   924,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   932,    -1,  1708,    -1,  1710,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1567,    -1,
      -1,    -1,    -1,  1724,    -1,    -1,    -1,    -1,    -1,  1730,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1742,    -1,    -1,    -1,   972,    -1,    -1,    -1,    -1,
      -1,    -1,   979,    -1,    -1,    -1,    -1,    -1,   985,   986,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1769,    -1,
    1771,    -1,    -1,    -1,    -1,    -1,  1625,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1015,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1652,    -1,    -1,    -1,  1808,    -1,    -1,
    1037,    -1,  1661,  1662,    -1,    -1,    -1,    -1,    -1,    -1,
    1821,    -1,    -1,    -1,  1051,    -1,    -1,    -1,    -1,    -1,
    1679,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1065,    -1,
      -1,    -1,    -1,  1070,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1878,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1118,    -1,  1894,    -1,    -1,    -1,  1746,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1768,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1169,    -1,    -1,    -1,    -1,    -1,    -1,  1950,
    1951,    -1,    -1,    -1,    -1,    -1,    -1,  1958,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1203,  1204,  1979,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1215,    -1,
    1991,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1229,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2013,    -1,  2015,    -1,  2017,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1256,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1893,    -1,    -1,    -1,  2049,    -1,
      -1,    -1,    -1,  1280,    -1,    -1,    -1,    -1,    -1,  2060,
      -1,  2062,  2063,  1290,    -1,  2066,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2079,  2080,
    2081,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2089,  2090,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2100,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1343,    -1,    -1,    -1,
      -1,    -1,  1349,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1996,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1384,    -1,    -1,
      -1,    -1,    -1,  1390,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1448,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1513,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1542,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1554,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1610,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1637,  1638,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1673,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1720,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1783,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1829,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1860,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1871,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1912,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1946,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2023
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   138,   139,   140,   141,     0,   134,   142,   643,   650,
     135,   136,    95,    99,   104,   126,   147,   148,   149,   150,
     151,   533,   541,   551,   595,   674,   134,   644,   142,   142,
     534,   542,   552,   596,   674,   664,   664,   664,   664,   265,
     647,   648,   657,   661,    94,   143,   144,   147,   155,   161,
     531,   653,   145,   146,   147,   155,   161,   653,   645,   646,
     647,   649,   650,   645,   645,   645,   541,   551,   653,   653,
     148,   149,   265,   646,    41,    44,   267,   270,   461,   463,
      45,    47,    48,    50,   266,   268,   271,   459,   465,   467,
     469,   532,   144,   674,   664,    97,   162,   537,   675,   146,
     674,   647,   664,   664,    31,   475,   475,   462,   464,   664,
     664,   460,   466,   468,   470,   664,   664,   664,   664,   645,
      31,   473,   538,    36,    37,    38,    39,    40,    46,    49,
      81,    89,   110,   118,   124,   130,   171,   250,   258,   260,
     261,   262,   263,   264,   441,   443,   449,   451,   453,   455,
     457,   505,   521,   563,   579,   591,   603,   657,    76,   254,
     495,   475,   475,   476,    24,   422,   664,   153,   658,   645,
     645,    20,   421,    32,   479,   645,   645,   645,   645,   378,
     473,    11,    12,    13,    14,    15,    17,    98,   337,   395,
     397,   399,   401,   403,   407,   539,   337,   653,   474,   664,
     645,   442,   444,   454,   452,   450,   456,   458,   506,   522,
     564,   580,   592,   604,   422,   664,   674,   674,   674,   674,
     674,   674,   674,   664,   664,   664,   664,   664,   664,   664,
     655,   660,   655,   660,   664,   655,   660,   655,   660,   655,
     660,   669,   496,   183,   660,   660,   152,   663,   152,   645,
     423,   655,   395,   422,   664,    91,   525,   480,   664,     8,
     389,   664,   396,   398,   400,   402,   404,   408,   540,   664,
     664,    55,    56,    59,    62,    64,    68,    69,    70,   272,
     273,   277,   278,   279,   280,   283,   284,   285,   286,   287,
     288,   325,   473,   603,   609,   613,   615,   621,   627,   631,
     637,   639,   645,   537,   645,   645,   645,   645,   645,   645,
     645,   645,   645,   645,   645,   645,   645,   655,   395,     7,
      20,   366,   387,   419,   653,   419,   652,   419,   419,   419,
     419,   666,   668,   666,   169,   170,   675,   475,   666,   667,
     666,   667,   666,   668,   156,   157,   675,   645,   661,   186,
     187,   252,   653,   422,   673,   422,   645,   674,   658,   655,
     395,   526,   664,   645,    35,   421,   485,   390,   657,   421,
     645,   645,   645,   645,   645,   645,   645,    30,   269,   330,
     439,   653,   269,   614,   616,   622,   628,   632,   638,   640,
     610,   655,   674,     9,    28,   387,   391,   433,   664,   664,
     664,   664,   664,   664,   664,   664,   664,   664,   666,   666,
     388,   420,   387,   664,   473,   367,   653,    73,   224,   225,
     259,   473,   489,   163,   164,   675,   655,   666,   473,   657,
     165,   166,   651,   165,   167,   168,   675,   389,   422,   651,
     676,   655,   653,    43,   447,   674,   433,   674,   154,   653,
     674,   658,   645,   652,   486,   664,   645,   473,   440,   664,
      21,   371,   413,   663,   331,   332,   653,   664,   645,   645,
     645,   645,   645,   645,   645,   645,   657,    66,   274,   317,
     635,   392,   434,    65,   315,   325,   633,   473,   378,   653,
     281,   325,   290,   291,   653,   325,   473,   290,   473,   473,
     326,   663,   652,   652,   645,   645,   473,   419,   664,   655,
     368,   369,   653,   490,   389,   664,   391,   164,   670,   671,
     651,   666,   170,   433,   663,    28,   172,   435,   166,   670,
     671,   653,   670,   168,   670,   671,   651,   665,   655,   651,
      86,   515,    42,    80,    87,    90,    96,   100,   101,   111,
     116,   117,   123,   131,   132,   188,   189,   190,   191,   192,
     193,   194,   199,   200,   201,   203,   207,   208,   209,   210,
     211,   212,   213,   216,   219,   220,   255,   330,   445,   503,
     517,   523,   535,   543,   545,   565,   575,   577,   589,   605,
     607,   652,   654,   448,   183,   154,   652,   154,   155,   160,
     653,   473,   154,   653,   378,   645,   421,   645,   421,   414,
     413,   663,   330,   655,   657,   105,   333,   334,   335,   553,
     653,   421,   421,   636,    71,   275,   321,   641,   674,   664,
     645,   645,   634,    29,   437,   664,    23,   417,   313,   663,
     657,   282,   437,    54,    58,   292,   293,   611,   619,   653,
     282,   433,   663,   657,   433,   657,   663,   325,   378,   159,
     160,   664,   419,   655,   657,    74,   114,   120,   232,   370,
     433,   473,   485,   491,   571,   583,   645,   657,   419,   473,
     671,   655,   651,   659,   676,   436,   175,   653,   671,   655,
     225,   228,   655,   671,   655,   651,   655,   655,   473,   516,
     664,   446,   504,   518,   524,   536,   544,   546,   566,   576,
     578,   590,   606,   608,   655,   663,   664,   664,   664,   658,
     664,   664,   256,   660,   658,   256,   658,   183,   664,   664,
     158,   160,    93,   125,   529,   593,   645,   655,   160,   377,
     378,   159,   161,   515,   653,   152,   159,   473,   655,   645,
     389,   108,   559,   554,   655,   657,   664,   336,   338,   653,
     645,   642,    57,   276,   322,   617,   674,   664,   318,   663,
     645,   438,   315,   315,   418,   387,   655,   663,   314,   378,
      60,   395,   623,   387,   314,   612,   620,   655,   664,   664,
     294,   296,   653,   387,   313,   387,     4,   381,   313,    79,
     501,   389,   655,   663,   653,   419,     3,   379,   492,   572,
     584,   365,   673,   347,   366,   657,   664,   238,   660,   664,
     225,   655,   666,   473,   227,   653,   422,   645,   417,   669,
     666,   664,   666,   666,   378,   655,   670,   656,   645,   473,
     645,   645,   645,   645,   645,   645,   645,   645,   645,   645,
     645,   645,   645,   660,   422,   381,   653,   330,   653,   330,
     330,   183,    51,   257,   471,   325,   183,   330,   661,   330,
     667,   655,   653,   530,   594,   664,   657,   664,   515,   655,
     663,   664,   655,   663,   152,   657,   560,   664,   645,    72,
     487,   334,   655,   657,   339,   341,   653,   645,   618,   674,
     664,   290,   663,   319,   325,   645,    22,   415,   664,   645,
     315,   389,   387,   624,   664,   664,   657,   415,   645,   645,
     437,   437,   655,   664,   297,   298,   653,   657,   417,   657,
     382,   664,   417,   502,   664,   657,   422,   422,   162,   380,
     664,   645,   645,   645,   439,   655,   663,   663,   226,   227,
     107,   557,   672,   651,   557,   656,   663,    77,    78,   119,
     128,   228,   229,   230,   231,   232,   233,   234,   237,   263,
     433,   437,   497,   499,   563,   581,   599,   652,   173,   387,
     176,   177,   178,   651,    26,   395,   427,   656,   671,   655,
     651,   655,   658,   330,   664,   330,   664,   664,   672,   472,
     664,   433,   661,    88,    92,   519,   527,   664,   221,   222,
     651,   162,   645,   645,   473,   330,   419,   664,   663,   391,
     473,   655,   391,   655,   330,   645,   332,   488,   664,   337,
     655,   657,    16,    18,    33,    34,   232,   342,   344,   345,
     346,   405,   409,   433,   473,   479,   481,   483,   485,   645,
     653,   389,   320,   416,   122,   587,   657,   664,   645,   330,
     290,   325,   313,   313,    61,   295,   623,   625,   655,   657,
     299,   300,   653,   653,   663,   315,   645,   289,   290,   657,
     645,   311,   663,   325,   674,   655,   675,   645,   369,   659,
     663,    27,   348,   391,   429,   673,   417,   663,   664,   558,
     660,   515,   668,   660,   651,   417,   498,   500,   582,   600,
     655,   664,   655,   664,   660,   106,   115,   555,   573,   563,
     227,   330,   657,   664,   658,   664,   439,   112,   174,   567,
     657,   177,   178,   670,   671,   670,   653,   428,   657,   657,
     651,   157,   674,   664,   674,   653,   655,   663,   557,   655,
     121,   585,    83,   509,   515,   645,   290,   330,   653,   520,
     528,   214,   662,   183,   509,   389,   655,   651,   675,   253,
     664,   657,   473,   417,   391,   674,   655,   645,   334,   664,
     340,   379,   405,   409,   406,   410,   482,   484,   655,   657,
     347,   663,   663,   330,   290,   657,   437,   645,   588,   664,
     314,   315,   437,   415,   415,   626,   657,   559,   655,   657,
     301,   302,   553,   653,   311,   387,    63,   629,   663,   289,
     655,   254,   652,   413,   430,   473,   433,   437,   439,   389,
     557,   645,   227,   239,   240,   241,   651,   227,   387,   664,
     663,   645,   645,   645,   645,   233,   237,   233,   234,   556,
     574,   238,   657,   658,   663,   664,   330,   366,   172,   233,
     237,   581,   652,   568,   664,   227,   671,   670,   655,   655,
     179,   645,   227,   227,   387,   664,    75,   493,   330,   664,
     389,   195,   662,   586,   183,   510,   183,   664,   663,   127,
     597,   645,   645,   204,   661,   215,   653,   672,   256,   655,
     655,   473,   254,   657,   251,   447,   557,   391,   515,   674,
     338,   664,   645,   645,   645,   645,     5,    25,    82,   103,
     343,   383,   425,   507,   549,   344,   344,   663,   655,   664,
     319,   289,   645,   315,   325,   657,   657,   645,   293,   664,
     487,   655,   664,   303,   653,   655,   657,   630,   664,   389,
     666,   183,   374,   663,   645,   659,   663,   659,   659,   663,
     657,   660,   240,   241,   670,   671,   670,   653,   664,   656,
     676,   655,   655,   645,   645,   235,   661,   227,   172,   417,
      10,   393,   657,   664,   573,   377,   645,   439,   489,   655,
     261,   656,   422,   422,   664,   656,   494,   664,   655,   419,
     657,   204,   661,   196,   653,   645,   205,   645,   672,   419,
     417,   598,   657,   672,     6,    84,   385,   511,   214,   654,
     515,   183,   655,   670,   253,   183,   381,   664,   655,   664,
     515,   341,   384,   426,   508,   550,   664,   417,   519,   415,
     415,   293,   293,   298,   664,   301,   655,   657,   304,   306,
     653,   315,   645,   290,   657,   674,   376,   663,   330,   375,
     652,   417,   652,   652,   413,   227,   227,   671,   670,   655,
     655,   378,   227,   651,   422,   236,   661,   102,   547,   655,
     394,   660,   529,   657,   655,   652,   657,   653,   655,   655,
     227,   651,   645,   227,   366,   672,   385,   195,   197,   198,
     653,   204,   206,   661,   515,   645,   330,   515,   386,   512,
     664,   183,   204,   224,   672,   671,   664,   664,   674,   664,
     330,   217,   662,   473,   664,   645,   645,   645,   645,   344,
     664,   300,   316,   395,   397,   399,   401,   403,   407,   655,
     657,    52,   307,   309,   310,   312,   325,   327,   433,   477,
     479,   289,   676,   655,   389,   664,   372,   663,   371,   374,
     655,   655,   664,   395,   672,   109,   561,   548,   242,   660,
     645,   330,   660,   227,   663,   223,   224,   113,   129,   180,
     569,   601,   603,   655,   655,   663,   664,   381,   655,   515,
     664,   204,   663,   664,   330,   672,    85,   513,   655,   645,
     645,   215,   672,   657,   664,   515,   222,   509,   655,   676,
     330,   664,   204,   661,   218,   653,   391,   473,   323,   324,
     325,   664,   305,   405,   409,   478,   655,   657,   663,   391,
     663,   289,   328,   515,   663,   657,   664,   381,   393,   655,
     663,   373,   651,   655,   376,   181,   182,   651,   653,   656,
     664,   515,   562,   246,   660,   645,   651,   663,   227,   413,
     655,   570,   602,   656,   664,   664,   664,   253,   656,   664,
     664,   196,   672,   389,    19,   411,   655,   664,   515,   514,
     664,   515,   433,   411,   183,   651,   515,   657,   419,   672,
     385,   217,   226,   391,   655,   664,   433,   664,   303,   664,
     645,   308,   383,   507,   549,    53,   429,   431,   437,   477,
     433,   663,   391,   664,   413,   375,   393,   657,   657,   663,
     389,   349,   653,   663,   655,   655,   655,   655,   387,   651,
     330,   645,   651,   668,   415,   655,   413,   645,   645,   653,
     676,   651,   330,   419,   515,   657,   412,   184,   660,   393,
     645,   330,   473,   655,   672,   395,   663,   587,   515,   664,
     204,   664,   385,   473,   411,   655,   306,   664,   432,   311,
     328,   311,   417,   417,   329,   473,   477,   473,   330,   330,
     417,   657,   655,   350,   351,   653,   415,   663,   653,   664,
     395,   663,   667,   243,   244,   245,   651,   378,    24,   424,
     381,   663,   198,   645,   655,   651,   660,   664,   253,   184,
     515,   664,   655,   664,   218,   672,   411,   433,   664,   417,
     664,   309,   645,   415,   417,   655,   373,   655,   657,   352,
     353,   354,   553,   653,   413,   664,   227,   664,   676,   247,
     248,   249,   651,   244,   245,   670,   671,   670,   653,   656,
     655,   664,   253,   185,   186,   330,   585,   663,   655,   330,
     330,   515,   184,   473,   323,   664,   290,   559,   655,   657,
     664,   355,   356,   653,   381,   424,   655,   656,   330,   422,
     248,   249,   670,   671,   670,   653,   671,   670,   655,   655,
     473,   181,   182,   651,   330,   676,   655,   655,   655,   183,
     417,   202,   657,   664,   655,   253,   411,   664,   487,   334,
     655,   657,   357,   358,   653,   664,   665,   653,   663,   655,
     671,   670,   655,   655,   473,   655,   656,   253,   253,   645,
     663,   422,   664,   509,   501,   417,   664,   332,   664,   337,
     655,   657,   228,   230,   231,   232,   359,   360,   361,   362,
     405,   409,   433,   473,   479,   481,   483,   485,   330,   655,
     664,   676,   655,   655,   656,   653,   655,   655,   655,   253,
     655,   411,   655,   664,   664,   290,   334,   664,   340,   363,
     364,   391,   429,   664,   673,   664,   365,   655,   657,   663,
     663,   349,   653,   664,   655,   663,   651,   381,   422,   653,
     664,   655,   676,   655,   655,   256,   330,   411,   338,   664,
     663,   473,   233,   433,   437,   439,   233,   347,   343,   344,
     344,   663,   653,   381,   422,   645,   664,   655,   664,   172,
     656,   422,   184,   183,   664,   184,   341,   348,   659,   663,
     659,   659,   663,   664,   417,   653,   657,   665,   655,   330,
     655,   381,   422,   655,   656,   653,   655,   655,   672,   655,
     652,   417,   652,   652,   413,   344,   347,   653,   330,   655,
     655,   663,   664,   655,   655,   653,   664,   655,   515,   372,
     371,   374,   653,   655,   422,   225,   655,   664,   381,   655,
     655,   376,   655,   665,   422,   655,   381,   664,   663,   663,
     655,   655,   655,   655,   655,   664,   225,   417,   415,   663,
     655,   655,   225,   422,   655,   413,   655,   422,   655,   655,
     655,   655,   655,   655,   655,   655
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int16 yyr1[] =
{
       0,   137,   139,   138,   140,   138,   141,   138,   142,   142,
     143,   143,   144,   144,   144,   144,   145,   145,   146,   146,
     146,   147,   147,   147,   147,   147,   147,   148,   148,   149,
     149,   150,   151,   152,   152,   153,   153,   154,   154,   155,
     155,   156,   156,   157,   157,   158,   159,   160,   160,   161,
     161,   161,   161,   161,   161,   161,   161,   161,   161,   161,
     161,   162,   162,   162,   162,   162,   162,   162,   162,   163,
     163,   164,   164,   165,   165,   166,   166,   167,   167,   168,
     168,   168,   169,   169,   170,   170,   171,   172,   173,   173,
     174,   174,   174,   175,   175,   175,   175,   176,   176,   177,
     178,   179,   179,   180,   180,   180,   180,   181,   181,   181,
     182,   182,   182,   183,   183,   184,   184,   185,   186,   187,
     187,   188,   188,   189,   189,   189,   190,   190,   190,   190,
     190,   190,   190,   190,   190,   190,   190,   190,   190,   190,
     190,   190,   190,   190,   191,   192,   193,   194,   194,   195,
     195,   196,   197,   197,   198,   198,   199,   200,   201,   202,
     202,   203,   204,   204,   205,   205,   206,   207,   208,   209,
     209,   210,   211,   211,   212,   213,   213,   214,   214,   215,
     215,   216,   216,   217,   217,   218,   218,   219,   220,   221,
     221,   222,   223,   223,   224,   224,   225,   225,   226,   226,
     227,   227,   227,   227,   227,   228,   229,   229,   230,   230,
     230,   230,   230,   230,   230,   230,   230,   230,   231,   231,
     232,   232,   232,   232,   233,   233,   234,   235,   235,   236,
     236,   237,   237,   237,   238,   238,   238,   238,   239,   239,
     240,   241,   242,   242,   242,   242,   243,   243,   244,   244,
     244,   245,   245,   245,   246,   246,   246,   246,   247,   247,
     248,   249,   250,   250,   250,   250,   251,   252,   253,   253,
     254,   254,   255,   255,   256,   256,   257,   258,   259,   259,
     260,   261,   262,   263,   264,   265,   265,   265,   265,   265,
     266,   267,   268,   268,   269,   269,   269,   270,   270,   271,
     272,   272,   272,   272,   272,   272,   272,   272,   272,   272,
     273,   274,   274,   275,   275,   276,   276,   277,   277,   277,
     277,   278,   279,   280,   280,   281,   282,   282,   283,   284,
     284,   285,   286,   287,   288,   289,   290,   291,   292,   292,
     292,   293,   294,   294,   295,   295,   296,   297,   297,   298,
     299,   299,   300,   301,   301,   302,   302,   303,   304,   304,
     305,   305,   306,   307,   307,   308,   308,   308,   309,   309,
     309,   309,   309,   309,   310,   310,   311,   311,   312,   312,
     313,   313,   314,   315,   315,   315,   316,   316,   316,   316,
     316,   316,   317,   318,   318,   319,   320,   320,   321,   322,
     322,   323,   323,   324,   324,   325,   325,   326,   326,   327,
     327,   327,   328,   328,   329,   329,   330,   331,   331,   332,
     333,   333,   334,   334,   335,   336,   336,   337,   337,   337,
     337,   337,   337,   337,   338,   339,   339,   340,   340,   340,
     341,   342,   342,   343,   343,   343,   343,   344,   344,   344,
     345,   346,   346,   346,   346,   346,   346,   346,   347,   347,
     348,   348,   348,   348,   348,   348,   348,   349,   350,   350,
     351,   352,   352,   353,   353,   354,   355,   355,   356,   357,
     357,   358,   359,   359,   360,   360,   360,   361,   361,   361,
     361,   361,   361,   361,   361,   361,   362,   362,   362,   362,
     363,   363,   364,   364,   364,   364,   364,   364,   364,   365,
     365,   366,   367,   368,   368,   369,   370,   370,   370,   370,
     371,   371,   372,   372,   373,   373,   374,   374,   375,   375,
     375,   376,   376,   377,   377,   378,   378,   380,   379,   382,
     381,   384,   383,   386,   385,   388,   387,   390,   389,   392,
     391,   394,   393,   396,   395,   398,   397,   400,   399,   402,
     401,   404,   403,   406,   405,   408,   407,   410,   409,   412,
     411,   414,   413,   416,   415,   418,   417,   420,   419,   421,
     423,   422,   424,   426,   425,   428,   427,   430,   429,   432,
     431,   434,   433,   436,   435,   438,   437,   440,   439,   442,
     441,   444,   443,   446,   445,   448,   447,   450,   449,   452,
     451,   454,   453,   456,   455,   458,   457,   460,   459,   462,
     461,   464,   463,   466,   465,   468,   467,   470,   469,   472,
     471,   474,   473,   476,   475,   478,   477,   480,   479,   482,
     481,   484,   483,   486,   485,   488,   487,   490,   489,   492,
     491,   494,   493,   496,   495,   498,   497,   500,   499,   502,
     501,   504,   503,   506,   505,   508,   507,   510,   509,   512,
     511,   514,   513,   516,   515,   518,   517,   520,   519,   522,
     521,   524,   523,   526,   525,   528,   527,   530,   529,   532,
     531,   534,   533,   536,   535,   538,   537,   540,   539,   542,
     541,   544,   543,   546,   545,   548,   547,   550,   549,   552,
     551,   554,   553,   556,   555,   558,   557,   560,   559,   562,
     561,   564,   563,   566,   565,   568,   567,   570,   569,   572,
     571,   574,   573,   576,   575,   578,   577,   580,   579,   582,
     581,   584,   583,   586,   585,   588,   587,   590,   589,   592,
     591,   594,   593,   596,   595,   598,   597,   600,   599,   602,
     601,   604,   603,   606,   605,   608,   607,   610,   609,   612,
     611,   614,   613,   616,   615,   618,   617,   620,   619,   622,
     621,   624,   623,   626,   625,   628,   627,   630,   629,   632,
     631,   634,   633,   636,   635,   638,   637,   640,   639,   642,
     641,   643,   644,   643,   645,   645,   645,   646,   646,   647,
     647,   648,   648,   649,   650,   650,   651,   652,   653,   654,
     655,   656,   657,   658,   659,   660,   661,   662,   663,   664,
     665,   666,   667,   668,   669,   670,   671,   672,   673,   674,
     675,   676
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     4,     0,     4,     0,     4,     0,     2,
       1,     2,     1,     1,     2,    11,     1,     2,     1,     1,
       2,     1,     3,     1,     3,     1,     1,    13,    17,    12,
      18,    14,    12,     0,     8,     0,     6,     0,     2,    11,
      11,     1,     7,     4,    10,    11,    12,     0,     2,    13,
       6,     5,     9,     5,     9,     5,     9,     5,     9,     5,
       7,     0,     3,     3,     3,     3,     3,     3,     3,     2,
       3,    20,    17,     2,     3,    10,    10,     2,     3,    18,
      21,    18,     1,     3,     5,    10,     5,     5,     0,     3,
       0,     7,     3,     0,     5,     5,     6,     2,     3,    13,
      13,     0,     3,     0,     2,     2,     2,    10,    14,    11,
       5,    11,     8,     0,     3,     0,     4,     2,     3,     0,
       5,     3,     1,     1,     3,     1,     1,     3,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     7,     1,    11,     9,     0,
       5,     6,     1,     5,     3,     7,     1,     5,    20,     1,
       5,    10,     0,     3,     0,     2,     7,     8,     5,     3,
       7,     9,     1,     3,     7,    10,     8,     0,     5,    14,
       8,    13,    11,     0,     5,     6,    11,     9,    12,     1,
       6,    10,     0,     1,     1,     4,     1,     3,     1,     5,
       3,     5,     5,     3,     6,     1,     1,     1,     7,     3,
       4,     3,     1,     6,     4,     8,     8,     1,     3,     1,
       7,     5,     4,     5,     1,     3,     7,     0,     3,     0,
       3,     0,     2,     4,     0,     6,     6,     7,     2,     3,
       6,     6,     0,     6,     6,     7,     2,     3,    10,    16,
      15,     9,    15,    14,     0,     6,     6,     7,     2,     3,
      12,    11,     3,     5,     6,     7,     5,     6,     0,     2,
       1,     3,     7,    11,     0,     2,     5,     7,     1,     1,
       3,     3,     3,     3,     3,     2,     2,     2,     2,     2,
       5,     3,     7,     7,     1,     2,     3,     5,     7,     7,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       5,     0,     2,     0,     2,     0,     2,     1,     4,     3,
       6,     5,     9,     7,     7,     8,     0,     3,    10,     7,
      10,     3,     7,     3,     7,     1,     1,     3,     1,     7,
       7,     3,     1,     5,     1,     1,     3,     1,     5,     3,
       1,     5,     3,     1,     3,     7,     3,     3,     1,     5,
       1,     1,     3,     1,     5,     1,     1,     1,     1,     4,
       5,     5,     3,     3,     1,     4,     2,     5,     1,     1,
       2,     5,     4,     1,     4,     7,     1,     1,     1,     1,
       1,     1,     3,     2,     5,     2,     0,     4,     3,     5,
       9,     5,     8,     1,     5,     1,     3,     2,     5,     1,
       4,     2,     0,     3,     1,     1,     3,     1,     5,     3,
       1,     5,     3,     1,     3,     1,     5,     1,     1,     1,
       1,     1,     1,     1,     3,     1,     5,     1,     1,     1,
       3,     1,     5,     1,     1,     1,     1,     1,     3,     3,
       2,     1,     1,     1,     1,     1,     4,     1,     0,     3,
       2,     1,     8,     8,     4,     9,     4,     3,     1,     5,
       3,     1,     5,     3,     1,     3,     1,     5,     3,     1,
       5,     3,     1,     5,     1,     3,     3,     1,     2,     3,
       3,     1,     1,     3,     5,     1,     1,     1,     1,     1,
       1,     3,     2,     1,     8,     8,     4,     9,     4,     9,
       4,     3,     3,     1,     5,     4,     1,     1,     4,     2,
       2,     5,     2,     5,     3,    17,     2,     5,     1,     5,
       5,     0,     4,     0,     1,     1,     4,     0,     3,     0,
       3,     0,     3,     0,     3,     0,     3,     0,     3,     0,
       3,     0,     3,     0,     3,     0,     3,     0,     3,     0,
       3,     0,     3,     0,     3,     0,     3,     0,     3,     0,
       3,     0,     3,     0,     3,     0,     3,     0,     3,     1,
       0,     3,     1,     0,     3,     0,     3,     0,     3,     0,
       3,     0,     3,     0,     3,     0,     3,     0,     3,     0,
       3,     0,     3,     0,     3,     0,     3,     0,     3,     0,
       3,     0,     3,     0,     3,     0,     3,     0,     3,     0,
       3,     0,     3,     0,     3,     0,     3,     0,     3,     0,
       3,     0,     3,     0,     3,     0,     3,     0,     3,     0,
       3,     0,     3,     0,     3,     0,     3,     0,     3,     0,
       3,     0,     3,     0,     3,     0,     3,     0,     3,     0,
       3,     0,     3,     0,     3,     0,     3,     0,     3,     0,
       3,     0,     3,     0,     3,     0,     3,     0,     3,     0,
       3,     0,     3,     0,     3,     0,     3,     0,     3,     0,
       3,     0,     3,     0,     3,     0,     3,     0,     3,     0,
       3,     0,     3,     0,     3,     0,     3,     0,     3,     0,
       3,     0,     3,     0,     3,     0,     3,     0,     3,     0,
       3,     0,     3,     0,     3,     0,     3,     0,     3,     0,
       3,     0,     3,     0,     3,     0,     3,     0,     3,     0,
       3,     0,     3,     0,     3,     0,     3,     0,     3,     0,
       3,     0,     3,     0,     3,     0,     3,     0,     3,     0,
       3,     0,     3,     0,     3,     0,     3,     0,     3,     0,
       3,     0,     3,     0,     3,     0,     3,     0,     3,     0,
       3,     0,     3,     0,     3,     0,     3,     0,     3,     0,
       3,     0,     3,     0,     3,     0,     3,     0,     3,     0,
       3,     1,     0,     3,     0,     1,     1,     1,     2,     1,
       2,     1,     2,     1,     1,     2,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)




# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)]);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep)
{
  YY_USE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= ENDOFFILE)
    {
      yychar = ENDOFFILE;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* $@1: %empty  */
#line 333 "../Parse.yacc"
      { depth=0; }
#line 4569 "y.tab.c"
    break;

  case 4: /* $@2: %empty  */
#line 334 "../Parse.yacc"
      { depth=0; }
#line 4575 "y.tab.c"
    break;

  case 5: /* FormattingUnit: $@2 MODUNIT InitialBlankLines ModUnit_list  */
#line 334 "../Parse.yacc"
                                                             { Flush(); }
#line 4581 "y.tab.c"
    break;

  case 6: /* $@3: %empty  */
#line 335 "../Parse.yacc"
      { depth=0; }
#line 4587 "y.tab.c"
    break;

  case 7: /* FormattingUnit: $@3 DEFUNIT InitialBlankLines DefUnit_list  */
#line 335 "../Parse.yacc"
                                                             { Flush(); }
#line 4593 "y.tab.c"
    break;

  case 537: /* $@4: %empty  */
#line 1447 "../Parse.yacc"
                         { PR ("&");}
#line 4599 "y.tab.c"
    break;

  case 539: /* $@5: %empty  */
#line 1448 "../Parse.yacc"
                      { PR (":=");}
#line 4605 "y.tab.c"
    break;

  case 541: /* $@6: %empty  */
#line 1449 "../Parse.yacc"
                        { PR ("*");}
#line 4611 "y.tab.c"
    break;

  case 543: /* $@7: %empty  */
#line 1450 "../Parse.yacc"
                   { PR ("|");}
#line 4617 "y.tab.c"
    break;

  case 545: /* $@8: %empty  */
#line 1451 "../Parse.yacc"
                     { PR (":");}
#line 4623 "y.tab.c"
    break;

  case 547: /* $@9: %empty  */
#line 1452 "../Parse.yacc"
                     { PR (",");}
#line 4629 "y.tab.c"
    break;

  case 549: /* $@10: %empty  */
#line 1453 "../Parse.yacc"
                   { PR (".");}
#line 4635 "y.tab.c"
    break;

  case 551: /* $@11: %empty  */
#line 1454 "../Parse.yacc"
                      { PR ("..");}
#line 4641 "y.tab.c"
    break;

  case 553: /* $@12: %empty  */
#line 1455 "../Parse.yacc"
                     { PR ("=");}
#line 4647 "y.tab.c"
    break;

  case 555: /* $@13: %empty  */
#line 1456 "../Parse.yacc"
                       { PR (">");}
#line 4653 "y.tab.c"
    break;

  case 557: /* $@14: %empty  */
#line 1457 "../Parse.yacc"
                       { PR (">=");}
#line 4659 "y.tab.c"
    break;

  case 559: /* $@15: %empty  */
#line 1458 "../Parse.yacc"
                    { PR ("<");}
#line 4665 "y.tab.c"
    break;

  case 561: /* $@16: %empty  */
#line 1459 "../Parse.yacc"
                       { PR ("<=");}
#line 4671 "y.tab.c"
    break;

  case 563: /* $@17: %empty  */
#line 1460 "../Parse.yacc"
                     { PR ("-");}
#line 4677 "y.tab.c"
    break;

  case 565: /* $@18: %empty  */
#line 1461 "../Parse.yacc"
                     { PR ("\043");}
#line 4683 "y.tab.c"
    break;

  case 567: /* $@19: %empty  */
#line 1462 "../Parse.yacc"
                    { PR ("+");}
#line 4689 "y.tab.c"
    break;

  case 569: /* $@20: %empty  */
#line 1463 "../Parse.yacc"
                      { PR ("=>");}
#line 4695 "y.tab.c"
    break;

  case 571: /* $@21: %empty  */
#line 1464 "../Parse.yacc"
                      { PR ("}");}
#line 4701 "y.tab.c"
    break;

  case 573: /* $@22: %empty  */
#line 1465 "../Parse.yacc"
                        { PR ("]");}
#line 4707 "y.tab.c"
    break;

  case 575: /* $@23: %empty  */
#line 1466 "../Parse.yacc"
                      { PR (")");}
#line 4713 "y.tab.c"
    break;

  case 577: /* $@24: %empty  */
#line 1467 "../Parse.yacc"
                       { PR ("*>");}
#line 4719 "y.tab.c"
    break;

  case 579: /* Rpragma1: RPRAGMA  */
#line 1468 "../Parse.yacc"
                       { PR ("*>");}
#line 4725 "y.tab.c"
    break;

  case 580: /* $@25: %empty  */
#line 1469 "../Parse.yacc"
                         { PR (";");}
#line 4731 "y.tab.c"
    break;

  case 582: /* Semi1: SEMICOLON  */
#line 1470 "../Parse.yacc"
                         { PR (";");}
#line 4737 "y.tab.c"
    break;

  case 583: /* $@26: %empty  */
#line 1471 "../Parse.yacc"
                     { PR ("/");}
#line 4743 "y.tab.c"
    break;

  case 585: /* $@27: %empty  */
#line 1472 "../Parse.yacc"
                       { PR ("<:");}
#line 4749 "y.tab.c"
    break;

  case 587: /* $@28: %empty  */
#line 1473 "../Parse.yacc"
                       { PR ("^");}
#line 4755 "y.tab.c"
    break;

  case 589: /* $@29: %empty  */
#line 1474 "../Parse.yacc"
                            { PR ("^'");}
#line 4761 "y.tab.c"
    break;

  case 591: /* $@30: %empty  */
#line 1477 "../Parse.yacc"
                      { PR ("("); }
#line 4767 "y.tab.c"
    break;

  case 593: /* $@31: %empty  */
#line 1478 "../Parse.yacc"
                      { PR ("("); }
#line 4773 "y.tab.c"
    break;

  case 595: /* $@32: %empty  */
#line 1479 "../Parse.yacc"
                        { PR ("["); }
#line 4779 "y.tab.c"
    break;

  case 597: /* $@33: %empty  */
#line 1480 "../Parse.yacc"
                      { PR ("{"); }
#line 4785 "y.tab.c"
    break;

  case 599: /* $@34: %empty  */
#line 1486 "../Parse.yacc"
                           { PF ("<* EXTERNAL", fonts->fixedComment);}
#line 4791 "y.tab.c"
    break;

  case 601: /* $@35: %empty  */
#line 1487 "../Parse.yacc"
                           { PF ("<* INLINE",   fonts->fixedComment);}
#line 4797 "y.tab.c"
    break;

  case 603: /* $@36: %empty  */
#line 1488 "../Parse.yacc"
                           { PF ("<* ASSERT",   fonts->fixedComment);}
#line 4803 "y.tab.c"
    break;

  case 605: /* $@37: %empty  */
#line 1489 "../Parse.yacc"
                           { PF ("<* TRACE",    fonts->fixedComment);}
#line 4809 "y.tab.c"
    break;

  case 607: /* $@38: %empty  */
#line 1490 "../Parse.yacc"
                           { PF ("<* FATAL",    fonts->fixedComment);}
#line 4815 "y.tab.c"
    break;

  case 609: /* $@39: %empty  */
#line 1491 "../Parse.yacc"
                           { PF ("<* UNUSED",   fonts->fixedComment);}
#line 4821 "y.tab.c"
    break;

  case 611: /* $@40: %empty  */
#line 1492 "../Parse.yacc"
                           { PF ("<* OBSOLETE", fonts->fixedComment);}
#line 4827 "y.tab.c"
    break;

  case 613: /* $@41: %empty  */
#line 1493 "../Parse.yacc"
                           { PF ("<* CALLBACK", fonts->fixedComment);}
#line 4833 "y.tab.c"
    break;

  case 615: /* $@42: %empty  */
#line 1494 "../Parse.yacc"
                           { PF ("<* EXPORTED", fonts->fixedComment);}
#line 4839 "y.tab.c"
    break;

  case 617: /* $@43: %empty  */
#line 1496 "../Parse.yacc"
                           { PF ("<* PRAGMA",   fonts->fixedComment);}
#line 4845 "y.tab.c"
    break;

  case 619: /* $@44: %empty  */
#line 1497 "../Parse.yacc"
                           { PF ("<* NOWARN",   fonts->fixedComment);}
#line 4851 "y.tab.c"
    break;

  case 621: /* $@45: %empty  */
#line 1498 "../Parse.yacc"
                           { PF ("<* LINE",     fonts->fixedComment);}
#line 4857 "y.tab.c"
    break;

  case 623: /* $@46: %empty  */
#line 1499 "../Parse.yacc"
                           { PF ("<* LL",       fonts->fixedComment);}
#line 4863 "y.tab.c"
    break;

  case 625: /* $@47: %empty  */
#line 1500 "../Parse.yacc"
                           { PF ("<* LL.sup",   fonts->fixedComment);}
#line 4869 "y.tab.c"
    break;

  case 627: /* $@48: %empty  */
#line 1501 "../Parse.yacc"
                           { PF ("<* SPEC",     fonts->fixedComment);}
#line 4875 "y.tab.c"
    break;

  case 629: /* $@49: %empty  */
#line 1502 "../Parse.yacc"
                           { PF ("<* LOOPINV",  fonts->fixedComment);}
#line 4881 "y.tab.c"
    break;

  case 631: /* $@50: %empty  */
#line 1504 "../Parse.yacc"
                     { PRID (&lexbuf[yyvsp[0]]);}
#line 4887 "y.tab.c"
    break;

  case 633: /* $@51: %empty  */
#line 1505 "../Parse.yacc"
                     { PF (&lexbuf[yyvsp[0]], fonts->procName);}
#line 4893 "y.tab.c"
    break;

  case 635: /* $@52: %empty  */
#line 1506 "../Parse.yacc"
                          { PRID (&lexbuf[yyvsp[0]]);}
#line 4899 "y.tab.c"
    break;

  case 637: /* $@53: %empty  */
#line 1507 "../Parse.yacc"
                          { PR (&lexbuf[yyvsp[0]]);}
#line 4905 "y.tab.c"
    break;

  case 639: /* $@54: %empty  */
#line 1508 "../Parse.yacc"
                          { PR (&lexbuf[yyvsp[0]]);}
#line 4911 "y.tab.c"
    break;

  case 641: /* $@55: %empty  */
#line 1509 "../Parse.yacc"
                          { PF (&lexbuf[yyvsp[0]], fonts->fixed);}
#line 4917 "y.tab.c"
    break;

  case 643: /* $@56: %empty  */
#line 1510 "../Parse.yacc"
                         { PF (&lexbuf[yyvsp[0]], fonts->fixed);}
#line 4923 "y.tab.c"
    break;

  case 645: /* $@57: %empty  */
#line 1512 "../Parse.yacc"
                   { PK ("AND");}
#line 4929 "y.tab.c"
    break;

  case 647: /* $@58: %empty  */
#line 1513 "../Parse.yacc"
                   { PK ("ANY");}
#line 4935 "y.tab.c"
    break;

  case 649: /* $@59: %empty  */
#line 1514 "../Parse.yacc"
                     { PK ("ARRAY");}
#line 4941 "y.tab.c"
    break;

  case 651: /* $@60: %empty  */
#line 1515 "../Parse.yacc"
                  { PK ("AS");}
#line 4947 "y.tab.c"
    break;

  case 653: /* $@61: %empty  */
#line 1516 "../Parse.yacc"
                   { PK ("BEGIN");}
#line 4953 "y.tab.c"
    break;

  case 655: /* $@62: %empty  */
#line 1517 "../Parse.yacc"
                    { PK ("BITS");}
#line 4959 "y.tab.c"
    break;

  case 657: /* $@63: %empty  */
#line 1518 "../Parse.yacc"
                       { PK ("BRANDED");}
#line 4965 "y.tab.c"
    break;

  case 659: /* $@64: %empty  */
#line 1519 "../Parse.yacc"
                  { PK ("BY");}
#line 4971 "y.tab.c"
    break;

  case 661: /* $@65: %empty  */
#line 1520 "../Parse.yacc"
                    { PK ("CASE");}
#line 4977 "y.tab.c"
    break;

  case 663: /* $@66: %empty  */
#line 1521 "../Parse.yacc"
                     { PK ("CONST");}
#line 4983 "y.tab.c"
    break;

  case 665: /* $@67: %empty  */
#line 1522 "../Parse.yacc"
                   { PR ("DIV");}
#line 4989 "y.tab.c"
    break;

  case 667: /* $@68: %empty  */
#line 1523 "../Parse.yacc"
                  { PK ("DO");}
#line 4995 "y.tab.c"
    break;

  case 669: /* $@69: %empty  */
#line 1524 "../Parse.yacc"
                    { PK ("ELSE");}
#line 5001 "y.tab.c"
    break;

  case 671: /* $@70: %empty  */
#line 1525 "../Parse.yacc"
                     { PK ("ELSIF");}
#line 5007 "y.tab.c"
    break;

  case 673: /* $@71: %empty  */
#line 1526 "../Parse.yacc"
                   { PK ("END");}
#line 5013 "y.tab.c"
    break;

  case 675: /* $@72: %empty  */
#line 1527 "../Parse.yacc"
                    { PK ("EVAL");}
#line 5019 "y.tab.c"
    break;

  case 677: /* $@73: %empty  */
#line 1528 "../Parse.yacc"
                      { PK ("EXCEPT");}
#line 5025 "y.tab.c"
    break;

  case 679: /* $@74: %empty  */
#line 1529 "../Parse.yacc"
                         { PK ("EXCEPTION");}
#line 5031 "y.tab.c"
    break;

  case 681: /* $@75: %empty  */
#line 1530 "../Parse.yacc"
                    { PK ("EXIT");}
#line 5037 "y.tab.c"
    break;

  case 683: /* $@76: %empty  */
#line 1531 "../Parse.yacc"
                       { PK ("EXPORTS");}
#line 5043 "y.tab.c"
    break;

  case 685: /* $@77: %empty  */
#line 1532 "../Parse.yacc"
                       { PK ("FINALLY");}
#line 5049 "y.tab.c"
    break;

  case 687: /* $@78: %empty  */
#line 1533 "../Parse.yacc"
                   { PK ("FOR");}
#line 5055 "y.tab.c"
    break;

  case 689: /* $@79: %empty  */
#line 1534 "../Parse.yacc"
                    { PK ("FROM");}
#line 5061 "y.tab.c"
    break;

  case 691: /* $@80: %empty  */
#line 1535 "../Parse.yacc"
                       { PK ("GENERIC");}
#line 5067 "y.tab.c"
    break;

  case 693: /* $@81: %empty  */
#line 1536 "../Parse.yacc"
                  { PK ("IF");}
#line 5073 "y.tab.c"
    break;

  case 695: /* $@82: %empty  */
#line 1537 "../Parse.yacc"
                      { PK ("IMPORT");}
#line 5079 "y.tab.c"
    break;

  case 697: /* $@83: %empty  */
#line 1538 "../Parse.yacc"
                  { PK ("IN");}
#line 5085 "y.tab.c"
    break;

  case 699: /* $@84: %empty  */
#line 1539 "../Parse.yacc"
                         { PK ("INTERFACE");}
#line 5091 "y.tab.c"
    break;

  case 701: /* $@85: %empty  */
#line 1540 "../Parse.yacc"
                    { PK ("LOCK");}
#line 5097 "y.tab.c"
    break;

  case 703: /* $@86: %empty  */
#line 1541 "../Parse.yacc"
                    { PK ("LOOP");}
#line 5103 "y.tab.c"
    break;

  case 705: /* $@87: %empty  */
#line 1542 "../Parse.yacc"
                       { PK ("METHODS");}
#line 5109 "y.tab.c"
    break;

  case 707: /* $@88: %empty  */
#line 1543 "../Parse.yacc"
                   { PK ("MOD");}
#line 5115 "y.tab.c"
    break;

  case 709: /* $@89: %empty  */
#line 1544 "../Parse.yacc"
                      { PK ("MODULE");}
#line 5121 "y.tab.c"
    break;

  case 711: /* $@90: %empty  */
#line 1545 "../Parse.yacc"
                   { PK ("NOT");}
#line 5127 "y.tab.c"
    break;

  case 713: /* $@91: %empty  */
#line 1546 "../Parse.yacc"
                      { PK ("OBJECT");}
#line 5133 "y.tab.c"
    break;

  case 715: /* $@92: %empty  */
#line 1547 "../Parse.yacc"
                  { PK ("OF");}
#line 5139 "y.tab.c"
    break;

  case 717: /* $@93: %empty  */
#line 1548 "../Parse.yacc"
                  { PK ("OR");}
#line 5145 "y.tab.c"
    break;

  case 719: /* $@94: %empty  */
#line 1549 "../Parse.yacc"
                         { PK ("OVERRIDES");}
#line 5151 "y.tab.c"
    break;

  case 721: /* $@95: %empty  */
#line 1550 "../Parse.yacc"
                         { PK ("PROCEDURE");}
#line 5157 "y.tab.c"
    break;

  case 723: /* $@96: %empty  */
#line 1551 "../Parse.yacc"
                     { PK ("RAISE");}
#line 5163 "y.tab.c"
    break;

  case 725: /* $@97: %empty  */
#line 1554 "../Parse.yacc"
                      { DoBreak(1, 2, 0.0); PK ("RAISES");}
#line 5169 "y.tab.c"
    break;

  case 727: /* $@98: %empty  */
#line 1555 "../Parse.yacc"
                        { PK ("READONLY");}
#line 5175 "y.tab.c"
    break;

  case 729: /* $@99: %empty  */
#line 1556 "../Parse.yacc"
                      { PK ("RECORD");}
#line 5181 "y.tab.c"
    break;

  case 731: /* $@100: %empty  */
#line 1557 "../Parse.yacc"
                   { PK ("REF");}
#line 5187 "y.tab.c"
    break;

  case 733: /* $@101: %empty  */
#line 1558 "../Parse.yacc"
                      { PK ("REPEAT");}
#line 5193 "y.tab.c"
    break;

  case 735: /* $@102: %empty  */
#line 1559 "../Parse.yacc"
                      { PK ("RETURN");}
#line 5199 "y.tab.c"
    break;

  case 737: /* $@103: %empty  */
#line 1560 "../Parse.yacc"
                      { PK ("REVEAL");}
#line 5205 "y.tab.c"
    break;

  case 739: /* $@104: %empty  */
#line 1561 "../Parse.yacc"
                    { PK ("ROOT");}
#line 5211 "y.tab.c"
    break;

  case 741: /* $@105: %empty  */
#line 1562 "../Parse.yacc"
                   { PK ("SET");}
#line 5217 "y.tab.c"
    break;

  case 743: /* $@106: %empty  */
#line 1563 "../Parse.yacc"
                    { PK ("THEN");}
#line 5223 "y.tab.c"
    break;

  case 745: /* $@107: %empty  */
#line 1564 "../Parse.yacc"
                  { PK ("TO");}
#line 5229 "y.tab.c"
    break;

  case 747: /* $@108: %empty  */
#line 1565 "../Parse.yacc"
                   { PK ("TRY");}
#line 5235 "y.tab.c"
    break;

  case 749: /* $@109: %empty  */
#line 1566 "../Parse.yacc"
                    { PK ("TYPE");}
#line 5241 "y.tab.c"
    break;

  case 751: /* $@110: %empty  */
#line 1567 "../Parse.yacc"
                        { PK ("TYPECASE");}
#line 5247 "y.tab.c"
    break;

  case 753: /* $@111: %empty  */
#line 1568 "../Parse.yacc"
                      { PK ("UNSAFE");}
#line 5253 "y.tab.c"
    break;

  case 755: /* $@112: %empty  */
#line 1569 "../Parse.yacc"
                     { PK ("UNTIL");}
#line 5259 "y.tab.c"
    break;

  case 757: /* $@113: %empty  */
#line 1570 "../Parse.yacc"
                        { PK ("UNTRACED");}
#line 5265 "y.tab.c"
    break;

  case 759: /* $@114: %empty  */
#line 1571 "../Parse.yacc"
                     { PK ("VALUE");}
#line 5271 "y.tab.c"
    break;

  case 761: /* $@115: %empty  */
#line 1572 "../Parse.yacc"
                   { PK ("VAR");}
#line 5277 "y.tab.c"
    break;

  case 763: /* $@116: %empty  */
#line 1573 "../Parse.yacc"
                     { PK ("WHILE");}
#line 5283 "y.tab.c"
    break;

  case 765: /* $@117: %empty  */
#line 1574 "../Parse.yacc"
                    { PK ("WITH");}
#line 5289 "y.tab.c"
    break;

  case 767: /* $@118: %empty  */
#line 1577 "../Parse.yacc"
                        { PK ("ABSTRACT");}
#line 5295 "y.tab.c"
    break;

  case 769: /* $@119: %empty  */
#line 1578 "../Parse.yacc"
                   { PK ("ALL");}
#line 5301 "y.tab.c"
    break;

  case 771: /* $@120: %empty  */
#line 1579 "../Parse.yacc"
                     { PK ("AXIOM");}
#line 5307 "y.tab.c"
    break;

  case 773: /* $@121: %empty  */
#line 1580 "../Parse.yacc"
                      { PK ("DEPEND");}
#line 5313 "y.tab.c"
    break;

  case 775: /* $@122: %empty  */
#line 1581 "../Parse.yacc"
                       { PK ("ENSURES");}
#line 5319 "y.tab.c"
    break;

  case 777: /* $@123: %empty  */
#line 1582 "../Parse.yacc"
                      { PK ("EXISTS");}
#line 5325 "y.tab.c"
    break;

  case 779: /* $@124: %empty  */
#line 1583 "../Parse.yacc"
                    { PK ("FUNC");}
#line 5331 "y.tab.c"
    break;

  case 781: /* $@125: %empty  */
#line 1584 "../Parse.yacc"
                   { PK ("IFF");}
#line 5337 "y.tab.c"
    break;

  case 783: /* $@126: %empty  */
#line 1585 "../Parse.yacc"
                       { PK ("IMPLIES");}
#line 5343 "y.tab.c"
    break;

  case 785: /* $@127: %empty  */
#line 1586 "../Parse.yacc"
                         { PK ("INVARIANT");}
#line 5349 "y.tab.c"
    break;

  case 787: /* $@128: %empty  */
#line 1587 "../Parse.yacc"
                  { PK ("IS");}
#line 5355 "y.tab.c"
    break;

  case 789: /* $@129: %empty  */
#line 1588 "../Parse.yacc"
                   { PK ("LET");}
#line 5361 "y.tab.c"
    break;

  case 791: /* $@130: %empty  */
#line 1589 "../Parse.yacc"
                   { PK ("MAP");}
#line 5367 "y.tab.c"
    break;

  case 793: /* $@131: %empty  */
#line 1590 "../Parse.yacc"
                        { PK ("MODIFIES");}
#line 5373 "y.tab.c"
    break;

  case 795: /* $@132: %empty  */
#line 1591 "../Parse.yacc"
                    { PK ("PRED");}
#line 5379 "y.tab.c"
    break;

  case 797: /* $@133: %empty  */
#line 1592 "../Parse.yacc"
                       { PK ("PROTECT");}
#line 5385 "y.tab.c"
    break;

  case 799: /* $@134: %empty  */
#line 1593 "../Parse.yacc"
                        { PK ("REQUIRES");}
#line 5391 "y.tab.c"
    break;

  case 801: /* InitialNPS: space_list  */
#line 1611 "../Parse.yacc"
                 { blanklinep = 0; PrintNPS(1); }
#line 5397 "y.tab.c"
    break;

  case 802: /* $@135: %empty  */
#line 1612 "../Parse.yacc"
                 { blanklinep = 0; PrintNPS(1); }
#line 5403 "y.tab.c"
    break;

  case 804: /* NPS: %empty  */
#line 1616 "../Parse.yacc"
                  { blanklinep = 0; }
#line 5409 "y.tab.c"
    break;

  case 813: /* space_list_emit: space_list  */
#line 1640 "../Parse.yacc"
                 { blanklinep = 0; PrintNPS(0); }
#line 5415 "y.tab.c"
    break;

  case 816: /* G: %empty  */
#line 1651 "../Parse.yacc"
       { GR (); }
#line 5421 "y.tab.c"
    break;

  case 817: /* B0: %empty  */
#line 1652 "../Parse.yacc"
       { BE (0.0); }
#line 5427 "y.tab.c"
    break;

  case 818: /* B: %empty  */
#line 1653 "../Parse.yacc"
       { BE (offset); }
#line 5433 "y.tab.c"
    break;

  case 819: /* B2: %empty  */
#line 1654 "../Parse.yacc"
       { BE (offset*2); }
#line 5439 "y.tab.c"
    break;

  case 820: /* E: %empty  */
#line 1655 "../Parse.yacc"
       { EN (); }
#line 5445 "y.tab.c"
    break;

  case 821: /* EF: %empty  */
#line 1656 "../Parse.yacc"
       { ENF (); }
#line 5451 "y.tab.c"
    break;

  case 822: /* A: %empty  */
#line 1658 "../Parse.yacc"
       { DoBreak (1, 2, 0.0); }
#line 5457 "y.tab.c"
    break;

  case 823: /* AO: %empty  */
#line 1659 "../Parse.yacc"
       { DoBreak (1, 3, 0.0); }
#line 5463 "y.tab.c"
    break;

  case 824: /* AX: %empty  */
#line 1660 "../Parse.yacc"
       { DoBreak (0, 3, 0.0); }
#line 5469 "y.tab.c"
    break;

  case 825: /* V: %empty  */
#line 1662 "../Parse.yacc"
       { DoBreak (1, 1, 0.0); }
#line 5475 "y.tab.c"
    break;

  case 826: /* VZ: %empty  */
#line 1663 "../Parse.yacc"
       { DoBreak (1, 1, -offset); }
#line 5481 "y.tab.c"
    break;

  case 827: /* VC: %empty  */
#line 1664 "../Parse.yacc"
       { DoBreak (1, 1, -offset + 2.0 * bodySpaceWidth); }
#line 5487 "y.tab.c"
    break;

  case 828: /* Z: %empty  */
#line 1666 "../Parse.yacc"
       { DoBreak (0, 0, 0.0); }
#line 5493 "y.tab.c"
    break;

  case 829: /* SP: %empty  */
#line 1667 "../Parse.yacc"
       { DoBreak (1, 0, 0.0); }
#line 5499 "y.tab.c"
    break;

  case 830: /* XSP: %empty  */
#line 1668 "../Parse.yacc"
       { P2 (' '); }
#line 5505 "y.tab.c"
    break;

  case 831: /* BL: %empty  */
#line 1670 "../Parse.yacc"
       { BL (); }
#line 5511 "y.tab.c"
    break;

  case 832: /* AL2: %empty  */
#line 1672 "../Parse.yacc"
       { DoAlign (2, 0); }
#line 5517 "y.tab.c"
    break;

  case 833: /* AL3: %empty  */
#line 1673 "../Parse.yacc"
       { DoAlign (3, 0); }
#line 5523 "y.tab.c"
    break;

  case 834: /* ALZ5: %empty  */
#line 1674 "../Parse.yacc"
       { DoAlign (5, 1); }
#line 5529 "y.tab.c"
    break;

  case 835: /* EA: %empty  */
#line 1675 "../Parse.yacc"
       { EndAlign (); }
#line 5535 "y.tab.c"
    break;

  case 836: /* ALNL: %empty  */
#line 1677 "../Parse.yacc"
        { ALNL(); }
#line 5541 "y.tab.c"
    break;

  case 837: /* SPNL: %empty  */
#line 1679 "../Parse.yacc"
       { DoSPNL (); }
#line 5547 "y.tab.c"
    break;

  case 838: /* QSP: %empty  */
#line 1680 "../Parse.yacc"
       { DoQSP (); }
#line 5553 "y.tab.c"
    break;

  case 839: /* NL: %empty  */
#line 1681 "../Parse.yacc"
       { NL (); }
#line 5559 "y.tab.c"
    break;

  case 840: /* Inc: %empty  */
#line 1683 "../Parse.yacc"
       { depth++; }
#line 5565 "y.tab.c"
    break;

  case 841: /* Dec: %empty  */
#line 1684 "../Parse.yacc"
       { depth--; }
#line 5571 "y.tab.c"
    break;


#line 5575 "y.tab.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (YY_("syntax error"));
    }

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= ENDOFFILE)
        {
          /* Return failure if at end of input.  */
          if (yychar == ENDOFFILE)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

#line 1686 "../Parse.yacc"


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
#ifndef USE_PROTOS
typedef void (*PROC)();
typedef double (*FPROC)();
#endif
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

#include <string.h>

#ifdef USE_PROTOS
void PR (const char *s)
#else
PR (s)
char *s;
#endif
{
  while (*s != 0) {
    P (*s);
    s++; }
}

#ifdef USE_PROTOS
void PK (const char *s)
#else
PK (s)
    char *s;
#endif
/* Print a keyword. */
{
    PF(s, fonts->keyword);
}

#ifdef USE_PROTOS
void PF(const char *s, const char *f)
#else
PF(s, f)
    char *s;
    char *f;
#endif
/* Print in arbitrary font. */
{
    Formatter__SetFont(formatter, f);
    PR(s);
    Formatter__SetFont(formatter, fonts->body);
}

static const char *builtins[] = {
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
    "LONGCARD",
    "LONGINT",
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

#ifdef USE_PROTOS
void PRID(const char *s)
#else
PRID(s)
    char *s;
#endif
{
    int i;
    const char *b;

    for (i = 0; (b = builtins[i]) != NULL; ++i) {
	if (*b == *s && strcmp(b, s) == 0) {
	    PF(s, fonts->builtinID);
	    return;
	}
    }
    PR(s);
}

#ifdef USE_PROTOS
void
PRNONL (const char *s)
#else
PRNONL (s)
char *s;
#endif
/* strip newline */
{
  while (*s != 0 && *s != '\n') {
    P (*s);
    s++; }
}

#ifdef USE_PROTOS
void BE (double n)
#else
BE (n) double n;
#endif
{ CheckComm(0); Formatter__Begin (formatter, n, MAXWIDTH); }

#ifdef USE_PROTOS
void EN (void)
#else
EN ()
#endif
{ Formatter__End (formatter); }

#ifdef USE_PROTOS
void ENF (void)
#else
ENF ()
#endif
{ CheckComm(0); Formatter__End (formatter); }

#ifdef USE_PROTOS
void GR(void)
#else
GR ()
#endif
{ CheckComm(0); Formatter__Group (formatter); }

#ifdef USE_PROTOS
void Flush (void)
#else
Flush ()
#endif
{ CheckComm(0); Formatter__Flush (formatter); }

#ifdef USE_PROTOS
void Reset (void) { }
#else
Reset () { }
#endif

#ifdef USE_PROTOS
void P(int n)
#else
P(n) int n;
#endif
{ CheckComm(0); Formatter__PutChar (formatter, n); }

#ifdef USE_PROTOS
void P2(int n)
#else
P2(n) int n;
#endif
{ Formatter__PutChar (formatter, n); }


#ifdef USE_PROTOS
void NL (void)
#else
NL ()
#endif
/* Emit a newline one level out. */
{
    CheckComm(1);
    Formatter__NewLine (formatter, -offset, 0);
    blanklinep = 0;
}

#ifdef USE_PROTOS
void BL (void)
#else
BL ()
#endif
/* Emit a newline at current level. */
{
    CheckComm(1);
    Formatter__NewLine (formatter, 0.0, 0);
    blanklinep = 0;
}

#ifdef USE_PROTOS
void DoSPNL (void)
#else
DoSPNL ()
#endif
{
   if (style == EM_STYLE) {
     DoBreak (1, 0, 0.0);
   } else {
     DoBreak (1, 1, -offset);
   };
}

#ifdef USE_PROTOS
void DoQSP (void)
#else
DoQSP ()
#endif
{
    if (callspace) DoBreak (1, 0, 0);
}

#ifdef USE_PROTOS
void DoAlign (int cols, int oneline)
#else
DoAlign (cols, oneline)
int cols, oneline;
#endif
{
    CheckComm(0);
    ++alignDepth;
    /* Oneline is only true for formals to procedures.  alignDecls does not
       affect them. */
    Formatter__Align(formatter, cols, oneline, (oneline || alignDecls));
}

#ifdef USE_PROTOS
void ALNL(void)
#else
ALNL()
#endif
/* Tell comment code that align is going to insert a newline here. */
{
    /* Only do it if there is comment work left to do. */
    if (moreComments)
	alignRow = 1;
}

#ifdef USE_PROTOS
void EndAlign(void)
#else
EndAlign()
#endif
{
    --alignDepth;
    alignRow = 0;
    Formatter__End(formatter);
}

#ifdef USE_PROTOS
void
DoBreak (int blank, int breakpt, double offs)
#else
DoBreak (blank, breakpt, offs)
    int blank, breakpt;
    double offs;
#endif
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

#ifdef USE_PROTOS
void
initParser (
    char *infile,
    Formatter_t* outfile,
    long emacs,
    long caps,
    FontInfo *fontInfo,
    double offs,
    double ccol,
    STYLE sty,
    long ad,
    long breaktype,
    long follow,
    long callsp,
    FPROC charWidth,
    PROC flush,
    PROC setFont,
    PROC putChar,
    PROC breakF,
    PROC newLine,
    PROC unitedBreak,
    PROC group,
    PROC begin,
    PROC align,
    PROC noAlign,
    PROC col,
    PROC end)
#else
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
#endif
{
    yyin = stdin;
    if ((!emacs) && (infile != 0)) {
	yyin = fopen(infile, "r");
	if (yyin == NULL) {
	    fprintf(stderr, "m3pp: unable to open \"%s\".\n", infile);
	    exit(1);
	};
        /* make a copy of the file name for output in case of an error */
        /* Where can I free the allocated memory ? */
        infileName = (char *) malloc (strlen(infile)+1);
        strcpy (infileName, infile);
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

#ifdef USE_PROTOS
void yyerror(const char *s)
#else
yyerror(s) char *s;
#endif
{
  int temp, temp2; /* must be 'int' instead of 'char'
                      otherwise the test (temp>0)
                      will fail for characters above code 127
                      and we need negative numbers for detecting end of file */
  Reset();
  Flush();
  if (calledFromEmacs == 0) {
        /* XEmacs requires that character counting starts with 1
            - very poor programming */
        fprintf (stderr,
            "%s:%d:%d: (byte %d) %s while pretty-printing\n",
            (infileName != NULL) ? infileName : "",
            currentRow+1, currentCol+1, lexposition, s);
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
  temp2 = yyinput();   /* yyinput comes from the lex library. */
  if ((calledFromEmacs && (temp2 == '\001')) || (temp2 == 0)) return;
  temp = yyinput();
  while ((temp > 0) && (!calledFromEmacs || (temp != '\001')))
    {P (temp2); temp2 = temp; temp = yyinput();}
  if ((temp2 != '\n') || (temp > 0)) P(temp2);
}

#ifdef USE_PROTOS
void PrintOnePragma(void)
#else
PrintOnePragma()
#endif
/* Print out first comment.  Hidden down here so it can see the comment
   structure. */
{
    PR(comments[0].text);
}

#ifdef USE_PROTOS
void PrintNPS(int initNPS)
#else
PrintNPS(initNPS)
    int initNPS;
#endif
{
    HandleComments(1, initNPS, 0);
}

#ifdef USE_PROTOS
int FixedComment(const char *s)
#else
int FixedComment(s)
    char *s;
#endif
/* Determine if a comment should be refilled or not.  Returns TRUE if it is
   "fixed" (should not be refilled). */
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

#ifdef USE_PROTOS
void
HandleComments(
    int firstTime,		/* first time on this comment? */
    int initNPS,		/* is this an InitialNPS? */
    int doBreak)		/* is a Break about to happen? */
#else
HandleComments(firstTime, initNPS, doBreak)
    int firstTime;		/* first time on this comment? */
    int initNPS;		/* is this an InitialNPS? */
    int doBreak;		/* is a Break about to happen? */
#endif
/* Comment and newline handling code. */
{
    int i;
    char *s, c;
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
		    sentenceBreak = strchr(".!?", s[-1]) != 0;
		}
	    }
	    Formatter__SetFont(formatter, fonts->body);
	    EN();
	}
    }
    if (needEnd)
	EN();
}
