/* A Bison parser, made by GNU Bison 1.875.  */

/* Skeleton parser for Yacc-like parsing with Bison,
   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* Written by Richard Stallman by simplifying the original so called
   ``semantic'' parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     IDENTIFIER = 258,
     TYPENAME = 259,
     SCSPEC = 260,
     TYPESPEC = 261,
     TYPE_QUAL = 262,
     CONSTANT = 263,
     STRING = 264,
     ELLIPSIS = 265,
     SIZEOF = 266,
     ENUM = 267,
     STRUCT = 268,
     UNION = 269,
     IF = 270,
     ELSE = 271,
     WHILE = 272,
     DO = 273,
     FOR = 274,
     SWITCH = 275,
     CASE = 276,
     DEFAULT = 277,
     BREAK = 278,
     CONTINUE = 279,
     RETURN = 280,
     GOTO = 281,
     ASM_KEYWORD = 282,
     TYPEOF = 283,
     ALIGNOF = 284,
     ATTRIBUTE = 285,
     EXTENSION = 286,
     LABEL = 287,
     REALPART = 288,
     IMAGPART = 289,
     VA_ARG = 290,
     CHOOSE_EXPR = 291,
     TYPES_COMPATIBLE_P = 292,
     PTR_VALUE = 293,
     PTR_BASE = 294,
     PTR_EXTENT = 295,
     STRING_FUNC_NAME = 296,
     VAR_FUNC_NAME = 297,
     ASSIGN = 298,
     OROR = 299,
     ANDAND = 300,
     EQCOMPARE = 301,
     ARITHCOMPARE = 302,
     RSHIFT = 303,
     LSHIFT = 304,
     MINUSMINUS = 305,
     PLUSPLUS = 306,
     UNARY = 307,
     HYPERUNARY = 308,
     POINTSAT = 309,
     INTERFACE = 310,
     IMPLEMENTATION = 311,
     END = 312,
     SELECTOR = 313,
     DEFS = 314,
     ENCODE = 315,
     CLASSNAME = 316,
     PUBLIC = 317,
     PRIVATE = 318,
     PROTECTED = 319,
     PROTOCOL = 320,
     OBJECTNAME = 321,
     CLASS = 322,
     ALIAS = 323
   };
#endif
#define IDENTIFIER 258
#define TYPENAME 259
#define SCSPEC 260
#define TYPESPEC 261
#define TYPE_QUAL 262
#define CONSTANT 263
#define STRING 264
#define ELLIPSIS 265
#define SIZEOF 266
#define ENUM 267
#define STRUCT 268
#define UNION 269
#define IF 270
#define ELSE 271
#define WHILE 272
#define DO 273
#define FOR 274
#define SWITCH 275
#define CASE 276
#define DEFAULT 277
#define BREAK 278
#define CONTINUE 279
#define RETURN 280
#define GOTO 281
#define ASM_KEYWORD 282
#define TYPEOF 283
#define ALIGNOF 284
#define ATTRIBUTE 285
#define EXTENSION 286
#define LABEL 287
#define REALPART 288
#define IMAGPART 289
#define VA_ARG 290
#define CHOOSE_EXPR 291
#define TYPES_COMPATIBLE_P 292
#define PTR_VALUE 293
#define PTR_BASE 294
#define PTR_EXTENT 295
#define STRING_FUNC_NAME 296
#define VAR_FUNC_NAME 297
#define ASSIGN 298
#define OROR 299
#define ANDAND 300
#define EQCOMPARE 301
#define ARITHCOMPARE 302
#define RSHIFT 303
#define LSHIFT 304
#define MINUSMINUS 305
#define PLUSPLUS 306
#define UNARY 307
#define HYPERUNARY 308
#define POINTSAT 309
#define INTERFACE 310
#define IMPLEMENTATION 311
#define END 312
#define SELECTOR 313
#define DEFS 314
#define ENCODE 315
#define CLASSNAME 316
#define PUBLIC 317
#define PRIVATE 318
#define PROTECTED 319
#define PROTOCOL 320
#define OBJECTNAME 321
#define CLASS 322
#define ALIAS 323




/* Copy the first part of user declarations.  */
#line 34 "c-parse.y"

#include "config.h"
#include "system.h"
#include "tree.h"
#include "input.h"
#include "cpplib.h"
#include "intl.h"
#include "timevar.h"
#include "c-lex.h"
#include "c-tree.h"
#include "c-pragma.h"
#include "flags.h"
#include "output.h"
#include "toplev.h"
#include "ggc.h"
  
#ifdef MULTIBYTE_CHARS
#include <locale.h>
#endif


/* Like YYERROR but do call yyerror.  */
#define YYERROR1 { yyerror ("syntax error"); YYERROR; }

/* Cause the "yydebug" variable to be defined.  */
#define YYDEBUG 1

/* Rename the "yyparse" function so that we can override it elsewhere.  */
#define yyparse yyparse_1


/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

#if ! defined (YYSTYPE) && ! defined (YYSTYPE_IS_DECLARED)
#line 67 "c-parse.y"
typedef union YYSTYPE {long itype; tree ttype; enum tree_code code;
	const char *filename; int lineno; } YYSTYPE;
/* Line 191 of yacc.c.  */
#line 245 "c-p10602.c"
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */
#line 200 "c-parse.y"

/* Number of statements (loosely speaking) and compound statements 
   seen so far.  */
static int stmt_count;
static int compstmt_count;
  
/* Input file and line number of the end of the body of last simple_if;
   used by the stmt-rule immediately after simple_if returns.  */
static const char *if_stmt_file;
static int if_stmt_line;

/* List of types and structure classes of the current declaration.  */
static tree current_declspecs = NULL_TREE;
static tree prefix_attributes = NULL_TREE;

/* List of all the attributes applying to the identifier currently being
   declared; includes prefix_attributes and possibly some more attributes
   just after a comma.  */
static tree all_prefix_attributes = NULL_TREE;

/* Stack of saved values of current_declspecs, prefix_attributes and
   all_prefix_attributes.  */
static tree declspec_stack;

/* PUSH_DECLSPEC_STACK is called from setspecs; POP_DECLSPEC_STACK
   should be called from the productions making use of setspecs.  */
#define PUSH_DECLSPEC_STACK						 \
  do {									 \
    declspec_stack = tree_cons (build_tree_list (prefix_attributes,	 \
						 all_prefix_attributes), \
				current_declspecs,			 \
				declspec_stack);			 \
  } while (0)

#define POP_DECLSPEC_STACK						\
  do {									\
    current_declspecs = TREE_VALUE (declspec_stack);			\
    prefix_attributes = TREE_PURPOSE (TREE_PURPOSE (declspec_stack));	\
    all_prefix_attributes = TREE_VALUE (TREE_PURPOSE (declspec_stack));	\
    declspec_stack = TREE_CHAIN (declspec_stack);			\
  } while (0)

/* For __extension__, save/restore the warning flags which are
   controlled by __extension__.  */
#define SAVE_WARN_FLAGS()			\
	size_int (pedantic			\
		  | (warn_pointer_arith << 1)	\
		  | (warn_traditional << 2))

#define RESTORE_WARN_FLAGS(tval)		\
  do {						\
    int val = tree_low_cst (tval, 0);		\
    pedantic = val & 1;				\
    warn_pointer_arith = (val >> 1) & 1;	\
    warn_traditional = (val >> 2) & 1;		\
  } while (0)


#define OBJC_NEED_RAW_IDENTIFIER(VAL)	/* nothing */

/* Tell yyparse how to print a token's value, if yydebug is set.  */

#define YYPRINT(FILE,YYCHAR,YYLVAL) yyprint(FILE,YYCHAR,YYLVAL)

static void yyprint	  PARAMS ((FILE *, int, YYSTYPE));
static void yyerror	  PARAMS ((const char *));
static int yylexname	  PARAMS ((void));
static inline int _yylex  PARAMS ((void));
static int  yylex	  PARAMS ((void));
static void init_reswords PARAMS ((void));

/* Add GC roots for variables local to this file.  */
void
c_parse_init ()
{
  init_reswords ();

  ggc_add_tree_root (&declspec_stack, 1);
  ggc_add_tree_root (&current_declspecs, 1);
  ggc_add_tree_root (&prefix_attributes, 1);
  ggc_add_tree_root (&all_prefix_attributes, 1);
}



/* Line 214 of yacc.c.  */
#line 341 "c-p10602.c"

#if ! defined (yyoverflow) || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# if YYSTACK_USE_ALLOCA
#  define YYSTACK_ALLOC alloca
# else
#  ifndef YYSTACK_USE_ALLOCA
#   if defined (alloca) || defined (_ALLOCA_H)
#    define YYSTACK_ALLOC alloca
#   else
#    ifdef __GNUC__
#     define YYSTACK_ALLOC __builtin_alloca
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning. */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
# else
#  if defined (__STDC__) || defined (__cplusplus)
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   define YYSIZE_T size_t
#  endif
#  define YYSTACK_ALLOC malloc
#  define YYSTACK_FREE free
# endif
#endif /* ! defined (yyoverflow) || YYERROR_VERBOSE */


#if (! defined (yyoverflow) \
     && (! defined (__cplusplus) \
	 || (YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  short yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (short) + sizeof (YYSTYPE))				\
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  register YYSIZE_T yyi;		\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (0)
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (0)

#endif

#if defined (__STDC__) || defined (__cplusplus)
   typedef signed char yysigned_char;
#else
   typedef short yysigned_char;
#endif

/* YYFINAL -- State number of the termination state. */
#define YYFINAL  4
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   3173

/* YYNTOKENS -- Number of terminals. */
#define YYNTOKENS  91
/* YYNNTS -- Number of nonterminals. */
#define YYNNTS  200
/* YYNRULES -- Number of rules. */
#define YYNRULES  561
/* YYNRULES -- Number of states. */
#define YYNSTATES  901

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   323

#define YYTRANSLATE(YYX) 						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const unsigned char yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    87,     2,     2,     2,    60,    51,     2,
      66,    83,    58,    56,    88,    57,    65,    59,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    46,    84,
       2,    43,     2,    45,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    67,     2,    90,    50,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    89,    49,    85,    86,     2,     2,     2,
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
      35,    36,    37,    38,    39,    40,    41,    42,    44,    47,
      48,    52,    53,    54,    55,    61,    62,    63,    64,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const unsigned short yyprhs[] =
{
       0,     0,     3,     4,     6,     7,    10,    11,    15,    17,
      19,    25,    28,    32,    37,    42,    45,    48,    51,    53,
      54,    55,    65,    70,    71,    72,    82,    87,    88,    89,
      98,   102,   104,   106,   108,   110,   112,   114,   116,   118,
     120,   122,   123,   125,   127,   131,   133,   136,   139,   142,
     145,   148,   153,   156,   161,   164,   167,   169,   171,   173,
     175,   180,   182,   186,   190,   194,   198,   202,   206,   210,
     214,   218,   222,   226,   230,   231,   236,   237,   242,   243,
     244,   252,   253,   259,   263,   267,   269,   271,   273,   275,
     276,   284,   288,   292,   296,   300,   305,   312,   321,   328,
     333,   337,   341,   344,   347,   349,   352,   353,   355,   358,
     362,   364,   366,   369,   372,   377,   382,   385,   388,   392,
     393,   395,   400,   405,   409,   413,   416,   419,   421,   424,
     427,   430,   433,   436,   438,   441,   443,   446,   449,   452,
     455,   458,   461,   463,   466,   469,   472,   475,   478,   481,
     484,   487,   490,   493,   496,   499,   502,   505,   508,   511,
     513,   516,   519,   522,   525,   528,   531,   534,   537,   540,
     543,   546,   549,   552,   555,   558,   561,   564,   567,   570,
     573,   576,   579,   582,   585,   588,   591,   594,   597,   600,
     603,   606,   609,   612,   615,   618,   621,   624,   627,   630,
     633,   636,   639,   642,   645,   647,   649,   651,   653,   655,
     657,   659,   661,   663,   665,   667,   669,   671,   673,   675,
     677,   679,   681,   683,   685,   687,   689,   691,   693,   695,
     697,   699,   701,   703,   705,   707,   709,   711,   713,   715,
     717,   719,   721,   723,   725,   727,   729,   731,   733,   735,
     737,   739,   741,   743,   745,   747,   749,   751,   753,   755,
     757,   758,   760,   762,   764,   766,   768,   770,   772,   774,
     779,   784,   786,   791,   793,   798,   799,   804,   805,   812,
     816,   817,   824,   828,   829,   831,   833,   836,   843,   845,
     849,   850,   852,   857,   864,   869,   871,   873,   875,   877,
     879,   880,   885,   887,   888,   891,   893,   897,   901,   904,
     905,   910,   912,   913,   918,   920,   922,   924,   927,   930,
     936,   940,   941,   942,   950,   951,   952,   960,   962,   964,
     969,   973,   976,   980,   982,   984,   986,   990,   993,   995,
     999,  1002,  1006,  1010,  1015,  1019,  1024,  1028,  1031,  1033,
    1035,  1038,  1040,  1043,  1045,  1048,  1049,  1057,  1063,  1064,
    1072,  1078,  1079,  1088,  1089,  1097,  1100,  1103,  1106,  1107,
    1109,  1110,  1112,  1114,  1117,  1118,  1122,  1125,  1129,  1134,
    1138,  1140,  1142,  1145,  1147,  1152,  1154,  1159,  1164,  1171,
    1177,  1182,  1189,  1195,  1197,  1201,  1203,  1205,  1209,  1210,
    1214,  1215,  1217,  1218,  1220,  1223,  1225,  1227,  1229,  1233,
    1236,  1240,  1245,  1249,  1252,  1255,  1257,  1261,  1266,  1269,
    1273,  1277,  1282,  1287,  1293,  1299,  1301,  1303,  1305,  1307,
    1309,  1312,  1315,  1318,  1321,  1323,  1326,  1329,  1332,  1334,
    1337,  1340,  1343,  1346,  1348,  1351,  1353,  1355,  1357,  1359,
    1362,  1363,  1364,  1365,  1366,  1367,  1369,  1371,  1374,  1378,
    1380,  1383,  1385,  1387,  1393,  1395,  1397,  1400,  1403,  1406,
    1409,  1410,  1416,  1417,  1422,  1423,  1424,  1426,  1429,  1433,
    1437,  1441,  1442,  1447,  1449,  1453,  1454,  1455,  1463,  1469,
    1472,  1473,  1474,  1475,  1476,  1489,  1490,  1497,  1500,  1502,
    1504,  1507,  1511,  1514,  1517,  1520,  1524,  1531,  1540,  1551,
    1564,  1568,  1573,  1575,  1579,  1585,  1588,  1594,  1595,  1597,
    1598,  1600,  1601,  1603,  1605,  1609,  1614,  1622,  1624,  1628,
    1629,  1633,  1636,  1637,  1638,  1645,  1648,  1649,  1651,  1653,
    1657,  1659,  1663,  1668,  1673,  1677,  1682,  1686,  1691,  1696,
    1700,  1705,  1709,  1711,  1712,  1716,  1718,  1721,  1723,  1727,
    1729,  1733
};

/* YYRHS -- A `-1'-separated list of the rules' RHS. */
static const short yyrhs[] =
{
      92,     0,    -1,    -1,    93,    -1,    -1,    94,    96,    -1,
      -1,    93,    95,    96,    -1,    98,    -1,    97,    -1,    27,
      66,   107,    83,    84,    -1,   290,    96,    -1,   129,   163,
      84,    -1,   149,   129,   163,    84,    -1,   148,   129,   162,
      84,    -1,   155,    84,    -1,     1,    84,    -1,     1,    85,
      -1,    84,    -1,    -1,    -1,   148,   129,   191,    99,   124,
     100,   251,   252,   240,    -1,   148,   129,   191,     1,    -1,
      -1,    -1,   149,   129,   196,   101,   124,   102,   251,   252,
     240,    -1,   149,   129,   196,     1,    -1,    -1,    -1,   129,
     196,   103,   124,   104,   251,   252,   240,    -1,   129,   196,
       1,    -1,     3,    -1,     4,    -1,    51,    -1,    57,    -1,
      56,    -1,    62,    -1,    61,    -1,    86,    -1,    87,    -1,
     109,    -1,    -1,   109,    -1,   115,    -1,   109,    88,   115,
      -1,   121,    -1,    58,   114,    -1,   290,   114,    -1,   106,
     114,    -1,    48,   105,    -1,   111,   110,    -1,   111,    66,
     217,    83,    -1,   112,   110,    -1,   112,    66,   217,    83,
      -1,    33,   114,    -1,    34,   114,    -1,    11,    -1,    29,
      -1,    28,    -1,   110,    -1,    66,   217,    83,   114,    -1,
     114,    -1,   115,    56,   115,    -1,   115,    57,   115,    -1,
     115,    58,   115,    -1,   115,    59,   115,    -1,   115,    60,
     115,    -1,   115,    55,   115,    -1,   115,    54,   115,    -1,
     115,    53,   115,    -1,   115,    52,   115,    -1,   115,    51,
     115,    -1,   115,    49,   115,    -1,   115,    50,   115,    -1,
      -1,   115,    48,   116,   115,    -1,    -1,   115,    47,   117,
     115,    -1,    -1,    -1,   115,    45,   118,   107,    46,   119,
     115,    -1,    -1,   115,    45,   120,    46,   115,    -1,   115,
      43,   115,    -1,   115,    44,   115,    -1,     3,    -1,     8,
      -1,   123,    -1,    42,    -1,    -1,    66,   217,    83,    89,
     122,   177,    85,    -1,    66,   107,    83,    -1,    66,     1,
      83,    -1,   244,   242,    83,    -1,   244,     1,    83,    -1,
     121,    66,   108,    83,    -1,    35,    66,   115,    88,   217,
      83,    -1,    36,    66,   115,    88,   115,    88,   115,    83,
      -1,    37,    66,   217,    88,   217,    83,    -1,   121,    67,
     107,    90,    -1,   121,    65,   105,    -1,   121,    68,   105,
      -1,   121,    62,    -1,   121,    61,    -1,     9,    -1,   123,
       9,    -1,    -1,   126,    -1,   126,    10,    -1,   251,   252,
     127,    -1,   125,    -1,   232,    -1,   126,   125,    -1,   125,
     232,    -1,   150,   129,   162,    84,    -1,   151,   129,   163,
      84,    -1,   150,    84,    -1,   151,    84,    -1,   251,   252,
     131,    -1,    -1,   169,    -1,   148,   129,   162,    84,    -1,
     149,   129,   163,    84,    -1,   148,   129,   185,    -1,   149,
     129,   188,    -1,   155,    84,    -1,   290,   131,    -1,     7,
      -1,   132,     7,    -1,   133,     7,    -1,   132,   170,    -1,
     134,     7,    -1,   135,     7,    -1,   170,    -1,   134,   170,
      -1,   157,    -1,   136,     7,    -1,   137,     7,    -1,   136,
     159,    -1,   137,   159,    -1,   132,   157,    -1,   133,   157,
      -1,   158,    -1,   136,   170,    -1,   136,   160,    -1,   137,
     160,    -1,   132,   158,    -1,   133,   158,    -1,   138,     7,
      -1,   139,     7,    -1,   138,   159,    -1,   139,   159,    -1,
     134,   157,    -1,   135,   157,    -1,   138,   170,    -1,   138,
     160,    -1,   139,   160,    -1,   134,   158,    -1,   135,   158,
      -1,     5,    -1,   140,     7,    -1,   141,     7,    -1,   132,
       5,    -1,   133,     5,    -1,   140,     5,    -1,   141,     5,
      -1,   140,   170,    -1,   142,     7,    -1,   143,     7,    -1,
     134,     5,    -1,   135,     5,    -1,   142,     5,    -1,   143,
       5,    -1,   142,   170,    -1,   144,     7,    -1,   145,     7,
      -1,   144,   159,    -1,   145,   159,    -1,   140,   157,    -1,
     141,   157,    -1,   136,     5,    -1,   137,     5,    -1,   144,
       5,    -1,   145,     5,    -1,   144,   170,    -1,   144,   160,
      -1,   145,   160,    -1,   140,   158,    -1,   141,   158,    -1,
     146,     7,    -1,   147,     7,    -1,   146,   159,    -1,   147,
     159,    -1,   142,   157,    -1,   143,   157,    -1,   138,     5,
      -1,   139,     5,    -1,   146,     5,    -1,   147,     5,    -1,
     146,   170,    -1,   146,   160,    -1,   147,   160,    -1,   142,
     158,    -1,   143,   158,    -1,   136,    -1,   137,    -1,   138,
      -1,   139,    -1,   144,    -1,   145,    -1,   146,    -1,   147,
      -1,   132,    -1,   133,    -1,   134,    -1,   135,    -1,   140,
      -1,   141,    -1,   142,    -1,   143,    -1,   136,    -1,   137,
      -1,   144,    -1,   145,    -1,   132,    -1,   133,    -1,   140,
      -1,   141,    -1,   136,    -1,   137,    -1,   138,    -1,   139,
      -1,   132,    -1,   133,    -1,   134,    -1,   135,    -1,   136,
      -1,   137,    -1,   138,    -1,   139,    -1,   132,    -1,   133,
      -1,   134,    -1,   135,    -1,   132,    -1,   133,    -1,   134,
      -1,   135,    -1,   136,    -1,   137,    -1,   138,    -1,   139,
      -1,   140,    -1,   141,    -1,   142,    -1,   143,    -1,   144,
      -1,   145,    -1,   146,    -1,   147,    -1,    -1,   153,    -1,
     159,    -1,   161,    -1,   160,    -1,     6,    -1,   205,    -1,
     200,    -1,     4,    -1,   113,    66,   107,    83,    -1,   113,
      66,   217,    83,    -1,   165,    -1,   162,    88,   130,   165,
      -1,   167,    -1,   163,    88,   130,   167,    -1,    -1,    27,
      66,   123,    83,    -1,    -1,   191,   164,   169,    43,   166,
     175,    -1,   191,   164,   169,    -1,    -1,   196,   164,   169,
      43,   168,   175,    -1,   196,   164,   169,    -1,    -1,   170,
      -1,   171,    -1,   170,   171,    -1,    30,    66,    66,   172,
      83,    83,    -1,   173,    -1,   172,    88,   173,    -1,    -1,
     174,    -1,   174,    66,     3,    83,    -1,   174,    66,     3,
      88,   109,    83,    -1,   174,    66,   108,    83,    -1,   105,
      -1,     5,    -1,     6,    -1,     7,    -1,   115,    -1,    -1,
      89,   176,   177,    85,    -1,     1,    -1,    -1,   178,   206,
      -1,   179,    -1,   178,    88,   179,    -1,   183,    43,   181,
      -1,   184,   181,    -1,    -1,   105,    46,   180,   181,    -1,
     181,    -1,    -1,    89,   182,   177,    85,    -1,   115,    -1,
       1,    -1,   184,    -1,   183,   184,    -1,    65,   105,    -1,
      67,   115,    10,   115,    90,    -1,    67,   115,    90,    -1,
      -1,    -1,   191,   186,   124,   187,   251,   252,   245,    -1,
      -1,    -1,   196,   189,   124,   190,   251,   252,   245,    -1,
     192,    -1,   196,    -1,    66,   169,   192,    83,    -1,   192,
      66,   285,    -1,   192,   225,    -1,    58,   156,   192,    -1,
       4,    -1,   194,    -1,   195,    -1,   194,    66,   285,    -1,
     194,   225,    -1,     4,    -1,   195,    66,   285,    -1,   195,
     225,    -1,    58,   156,   194,    -1,    58,   156,   195,    -1,
      66,   169,   195,    83,    -1,   196,    66,   285,    -1,    66,
     169,   196,    83,    -1,    58,   156,   196,    -1,   196,   225,
      -1,     3,    -1,    13,    -1,    13,   170,    -1,    14,    -1,
      14,   170,    -1,    12,    -1,    12,   170,    -1,    -1,   197,
     105,    89,   201,   208,    85,   169,    -1,   197,    89,   208,
      85,   169,    -1,    -1,   198,   105,    89,   202,   208,    85,
     169,    -1,   198,    89,   208,    85,   169,    -1,    -1,   199,
     105,    89,   203,   215,   207,    85,   169,    -1,    -1,   199,
      89,   204,   215,   207,    85,   169,    -1,   197,   105,    -1,
     198,   105,    -1,   199,   105,    -1,    -1,    88,    -1,    -1,
      88,    -1,   209,    -1,   209,   210,    -1,    -1,   209,   210,
      84,    -1,   209,    84,    -1,   152,   129,   211,    -1,   152,
     129,   251,   252,    -1,   153,   129,   212,    -1,   153,    -1,
       1,    -1,   290,   210,    -1,   213,    -1,   211,    88,   130,
     213,    -1,   214,    -1,   212,    88,   130,   214,    -1,   251,
     252,   191,   169,    -1,   251,   252,   191,    46,   115,   169,
      -1,   251,   252,    46,   115,   169,    -1,   251,   252,   196,
     169,    -1,   251,   252,   196,    46,   115,   169,    -1,   251,
     252,    46,   115,   169,    -1,   216,    -1,   215,    88,   216,
      -1,     1,    -1,   105,    -1,   105,    43,   115,    -1,    -1,
     154,   218,   219,    -1,    -1,   221,    -1,    -1,   221,    -1,
     222,   170,    -1,   223,    -1,   222,    -1,   224,    -1,    58,
     156,   222,    -1,    58,   156,    -1,    58,   156,   223,    -1,
      66,   169,   221,    83,    -1,   224,    66,   275,    -1,   224,
     225,    -1,    66,   275,    -1,   225,    -1,    67,   107,    90,
      -1,    67,   154,   107,    90,    -1,    67,    90,    -1,    67,
     154,    90,    -1,    67,    58,    90,    -1,    67,   154,    58,
      90,    -1,    67,     5,   107,    90,    -1,    67,     5,   154,
     107,    90,    -1,    67,   154,     5,   107,    90,    -1,   227,
      -1,   228,    -1,   229,    -1,   230,    -1,   255,    -1,   227,
     255,    -1,   228,   255,    -1,   229,   255,    -1,   230,   255,
      -1,   128,    -1,   227,   128,    -1,   228,   128,    -1,   230,
     128,    -1,   256,    -1,   227,   256,    -1,   228,   256,    -1,
     229,   256,    -1,   230,   256,    -1,   232,    -1,   231,   232,
      -1,   227,    -1,   228,    -1,   229,    -1,   230,    -1,     1,
      84,    -1,    -1,    -1,    -1,    -1,    -1,   238,    -1,   239,
      -1,   238,   239,    -1,    32,   289,    84,    -1,   245,    -1,
       1,   245,    -1,    89,    -1,    85,    -1,   233,   237,   243,
      85,   234,    -1,   226,    -1,     1,    -1,    66,    89,    -1,
     241,   242,    -1,   247,   254,    -1,   247,     1,    -1,    -1,
      15,   248,    66,   107,    83,    -1,    -1,    18,   250,   254,
      17,    -1,    -1,    -1,   255,    -1,   256,   253,    -1,   235,
     253,   236,    -1,   251,   252,   267,    -1,   251,   252,   268,
      -1,    -1,   246,    16,   258,   254,    -1,   246,    -1,   246,
      16,     1,    -1,    -1,    -1,    17,   259,    66,   107,    83,
     260,   254,    -1,   249,    66,   107,    83,    84,    -1,   249,
       1,    -1,    -1,    -1,    -1,    -1,    19,   261,    66,   266,
     262,   270,    84,   263,   270,    83,   264,   254,    -1,    -1,
      20,    66,   107,    83,   265,   254,    -1,   270,    84,    -1,
     131,    -1,   245,    -1,   107,    84,    -1,   235,   257,   236,
      -1,    23,    84,    -1,    24,    84,    -1,    25,    84,    -1,
      25,   107,    84,    -1,    27,   269,    66,   107,    83,    84,
      -1,    27,   269,    66,   107,    46,   271,    83,    84,    -1,
      27,   269,    66,   107,    46,   271,    46,   271,    83,    84,
      -1,    27,   269,    66,   107,    46,   271,    46,   271,    46,
     274,    83,    84,    -1,    26,   105,    84,    -1,    26,    58,
     107,    84,    -1,    84,    -1,    21,   115,    46,    -1,    21,
     115,    10,   115,    46,    -1,    22,    46,    -1,   105,   251,
     252,    46,   169,    -1,    -1,     7,    -1,    -1,   107,    -1,
      -1,   272,    -1,   273,    -1,   272,    88,   273,    -1,     9,
      66,   107,    83,    -1,    67,   105,    90,     9,    66,   107,
      83,    -1,   123,    -1,   274,    88,   123,    -1,    -1,   169,
     276,   277,    -1,   280,    83,    -1,    -1,    -1,   281,    84,
     278,   169,   279,   277,    -1,     1,    83,    -1,    -1,    10,
      -1,   281,    -1,   281,    88,    10,    -1,   283,    -1,   281,
      88,   282,    -1,   148,   129,   193,   169,    -1,   148,   129,
     196,   169,    -1,   148,   129,   220,    -1,   149,   129,   196,
     169,    -1,   149,   129,   220,    -1,   150,   284,   193,   169,
      -1,   150,   284,   196,   169,    -1,   150,   284,   220,    -1,
     151,   284,   196,   169,    -1,   151,   284,   220,    -1,   129,
      -1,    -1,   169,   286,   287,    -1,   277,    -1,   288,    83,
      -1,     3,    -1,   288,    88,     3,    -1,   105,    -1,   289,
      88,   105,    -1,    31,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const unsigned short yyrline[] =
{
       0,   287,   287,   291,   307,   307,   308,   308,   312,   313,
     314,   322,   327,   334,   336,   338,   340,   341,   342,   349,
     354,   348,   360,   363,   368,   362,   374,   377,   382,   376,
     388,   393,   394,   397,   399,   401,   406,   408,   410,   412,
     416,   422,   423,   427,   429,   434,   435,   438,   441,   445,
     462,   468,   471,   474,   477,   479,   484,   488,   492,   496,
     497,   502,   503,   505,   507,   509,   511,   513,   515,   517,
     519,   521,   523,   525,   528,   527,   534,   533,   540,   543,
     539,   549,   548,   558,   565,   576,   582,   583,   585,   588,
     587,   600,   605,   607,   623,   630,   632,   635,   645,   655,
     657,   661,   667,   669,   675,   676,   693,   695,   696,   707,
     712,   713,   714,   715,   723,   725,   727,   730,   739,   748,
     758,   763,   765,   767,   769,   771,   773,   830,   833,   836,
     842,   848,   851,   857,   860,   866,   869,   872,   875,   878,
     881,   884,   890,   893,   896,   899,   902,   905,   911,   914,
     917,   920,   923,   926,   932,   935,   938,   941,   944,   950,
     953,   956,   959,   965,   971,   977,   986,   992,   995,   998,
    1004,  1010,  1016,  1025,  1031,  1034,  1037,  1040,  1043,  1046,
    1049,  1055,  1061,  1067,  1076,  1079,  1082,  1085,  1088,  1094,
    1097,  1100,  1103,  1106,  1109,  1112,  1118,  1124,  1130,  1139,
    1142,  1145,  1148,  1151,  1158,  1159,  1160,  1161,  1162,  1163,
    1164,  1165,  1169,  1170,  1171,  1172,  1173,  1174,  1175,  1176,
    1180,  1181,  1182,  1183,  1187,  1188,  1189,  1190,  1194,  1195,
    1196,  1197,  1201,  1202,  1203,  1204,  1208,  1209,  1210,  1211,
    1212,  1213,  1214,  1215,  1219,  1220,  1221,  1222,  1223,  1224,
    1225,  1226,  1227,  1228,  1229,  1230,  1231,  1232,  1233,  1234,
    1240,  1241,  1267,  1268,  1272,  1276,  1278,  1282,  1286,  1290,
    1292,  1299,  1300,  1304,  1305,  1310,  1311,  1319,  1318,  1326,
    1335,  1334,  1342,  1351,  1352,  1357,  1359,  1364,  1369,  1371,
    1377,  1378,  1380,  1382,  1384,  1392,  1393,  1394,  1395,  1401,
    1403,  1402,  1406,  1413,  1415,  1419,  1420,  1426,  1429,  1433,
    1432,  1438,  1443,  1442,  1446,  1448,  1452,  1453,  1457,  1462,
    1466,  1472,  1484,  1471,  1502,  1514,  1501,  1534,  1535,  1541,
    1543,  1548,  1550,  1552,  1560,  1561,  1565,  1570,  1572,  1576,
    1581,  1583,  1585,  1587,  1595,  1600,  1602,  1604,  1606,  1610,
    1612,  1617,  1619,  1624,  1626,  1638,  1637,  1643,  1648,  1647,
    1651,  1656,  1655,  1661,  1660,  1668,  1670,  1672,  1680,  1682,
    1685,  1687,  1693,  1695,  1701,  1702,  1704,  1710,  1713,  1723,
    1726,  1731,  1733,  1739,  1740,  1745,  1746,  1751,  1754,  1758,
    1764,  1767,  1771,  1782,  1783,  1788,  1794,  1796,  1802,  1801,
    1810,  1811,  1816,  1819,  1823,  1830,  1831,  1835,  1836,  1841,
    1843,  1848,  1850,  1852,  1854,  1856,  1863,  1865,  1867,  1869,
    1871,  1873,  1875,  1879,  1883,  1896,  1897,  1898,  1902,  1906,
    1907,  1908,  1909,  1910,  1914,  1915,  1918,  1919,  1923,  1924,
    1925,  1926,  1927,  1931,  1932,  1936,  1937,  1938,  1939,  1942,
    1946,  1953,  1958,  1974,  1988,  1990,  1996,  1997,  2001,  2015,
    2017,  2020,  2024,  2026,  2034,  2035,  2039,  2056,  2064,  2069,
    2082,  2081,  2096,  2095,  2115,  2121,  2127,  2128,  2133,  2139,
    2153,  2163,  2162,  2170,  2182,  2193,  2196,  2192,  2202,  2205,
    2208,  2212,  2215,  2218,  2207,  2222,  2221,  2229,  2231,  2237,
    2239,  2242,  2246,  2249,  2252,  2255,  2258,  2262,  2266,  2271,
    2275,  2287,  2293,  2301,  2304,  2307,  2310,  2327,  2329,  2335,
    2336,  2342,  2343,  2347,  2348,  2353,  2355,  2360,  2362,  2373,
    2372,  2383,  2385,  2393,  2384,  2397,  2404,  2405,  2415,  2417,
    2422,  2424,  2431,  2436,  2441,  2444,  2450,  2458,  2463,  2468,
    2471,  2477,  2483,  2493,  2492,  2503,  2504,  2522,  2524,  2530,
    2532,  2537
};
#endif

#if YYDEBUG || YYERROR_VERBOSE
/* YYTNME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals. */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "IDENTIFIER", "TYPENAME", "SCSPEC", 
  "TYPESPEC", "TYPE_QUAL", "CONSTANT", "STRING", "ELLIPSIS", "SIZEOF", 
  "ENUM", "STRUCT", "UNION", "IF", "ELSE", "WHILE", "DO", "FOR", "SWITCH", 
  "CASE", "DEFAULT", "BREAK", "CONTINUE", "RETURN", "GOTO", "ASM_KEYWORD", 
  "TYPEOF", "ALIGNOF", "ATTRIBUTE", "EXTENSION", "LABEL", "REALPART", 
  "IMAGPART", "VA_ARG", "CHOOSE_EXPR", "TYPES_COMPATIBLE_P", "PTR_VALUE", 
  "PTR_BASE", "PTR_EXTENT", "STRING_FUNC_NAME", "VAR_FUNC_NAME", "'='", 
  "ASSIGN", "'?'", "':'", "OROR", "ANDAND", "'|'", "'^'", "'&'", 
  "EQCOMPARE", "ARITHCOMPARE", "RSHIFT", "LSHIFT", "'+'", "'-'", "'*'", 
  "'/'", "'%'", "MINUSMINUS", "PLUSPLUS", "UNARY", "HYPERUNARY", "'.'", 
  "'('", "'['", "POINTSAT", "INTERFACE", "IMPLEMENTATION", "END", 
  "SELECTOR", "DEFS", "ENCODE", "CLASSNAME", "PUBLIC", "PRIVATE", 
  "PROTECTED", "PROTOCOL", "OBJECTNAME", "CLASS", "ALIAS", "')'", "';'", 
  "'}'", "'~'", "'!'", "','", "'{'", "']'", "$accept", "program", 
  "extdefs", "@1", "@2", "extdef", "datadef", "fndef", "@3", "@4", "@5", 
  "@6", "@7", "@8", "identifier", "unop", "expr", "exprlist", 
  "nonnull_exprlist", "unary_expr", "sizeof", "alignof", "typeof", 
  "cast_expr", "expr_no_commas", "@9", "@10", "@11", "@12", "@13", 
  "primary", "@14", "string", "old_style_parm_decls", "lineno_datadecl", 
  "datadecls", "datadecl", "lineno_decl", "setspecs", "maybe_resetattrs", 
  "decl", "declspecs_nosc_nots_nosa_noea", "declspecs_nosc_nots_nosa_ea", 
  "declspecs_nosc_nots_sa_noea", "declspecs_nosc_nots_sa_ea", 
  "declspecs_nosc_ts_nosa_noea", "declspecs_nosc_ts_nosa_ea", 
  "declspecs_nosc_ts_sa_noea", "declspecs_nosc_ts_sa_ea", 
  "declspecs_sc_nots_nosa_noea", "declspecs_sc_nots_nosa_ea", 
  "declspecs_sc_nots_sa_noea", "declspecs_sc_nots_sa_ea", 
  "declspecs_sc_ts_nosa_noea", "declspecs_sc_ts_nosa_ea", 
  "declspecs_sc_ts_sa_noea", "declspecs_sc_ts_sa_ea", "declspecs_ts", 
  "declspecs_nots", "declspecs_ts_nosa", "declspecs_nots_nosa", 
  "declspecs_nosc_ts", "declspecs_nosc_nots", "declspecs_nosc", 
  "declspecs", "maybe_type_quals_attrs", "typespec_nonattr", 
  "typespec_attr", "typespec_reserved_nonattr", "typespec_reserved_attr", 
  "typespec_nonreserved_nonattr", "initdecls", "notype_initdecls", 
  "maybeasm", "initdcl", "@15", "notype_initdcl", "@16", 
  "maybe_attribute", "attributes", "attribute", "attribute_list", 
  "attrib", "any_word", "init", "@17", "initlist_maybe_comma", 
  "initlist1", "initelt", "@18", "initval", "@19", "designator_list", 
  "designator", "nested_function", "@20", "@21", "notype_nested_function", 
  "@22", "@23", "declarator", "after_type_declarator", "parm_declarator", 
  "parm_declarator_starttypename", "parm_declarator_nostarttypename", 
  "notype_declarator", "struct_head", "union_head", "enum_head", 
  "structsp_attr", "@24", "@25", "@26", "@27", "structsp_nonattr", 
  "maybecomma", "maybecomma_warn", "component_decl_list", 
  "component_decl_list2", "component_decl", "components", 
  "components_notype", "component_declarator", 
  "component_notype_declarator", "enumlist", "enumerator", "typename", 
  "@28", "absdcl", "absdcl_maybe_attribute", "absdcl1", "absdcl1_noea", 
  "absdcl1_ea", "direct_absdcl1", "array_declarator", "stmts_and_decls", 
  "lineno_stmt_decl_or_labels_ending_stmt", 
  "lineno_stmt_decl_or_labels_ending_decl", 
  "lineno_stmt_decl_or_labels_ending_label", 
  "lineno_stmt_decl_or_labels_ending_error", "lineno_stmt_decl_or_labels", 
  "errstmt", "pushlevel", "poplevel", "c99_block_start", "c99_block_end", 
  "maybe_label_decls", "label_decls", "label_decl", "compstmt_or_error", 
  "compstmt_start", "compstmt_nostart", "compstmt_contents_nonempty", 
  "compstmt_primary_start", "compstmt", "simple_if", "if_prefix", "@29", 
  "do_stmt_start", "@30", "save_filename", "save_lineno", 
  "lineno_labeled_stmt", "c99_block_lineno_labeled_stmt", "lineno_stmt", 
  "lineno_label", "select_or_iter_stmt", "@31", "@32", "@33", "@34", 
  "@35", "@36", "@37", "@38", "for_init_stmt", "stmt", "label", 
  "maybe_type_qual", "xexpr", "asm_operands", "nonnull_asm_operands", 
  "asm_operand", "asm_clobbers", "parmlist", "@39", "parmlist_1", "@40", 
  "@41", "parmlist_2", "parms", "parm", "firstparm", "setspecs_fp", 
  "parmlist_or_identifiers", "@42", "parmlist_or_identifiers_1", 
  "identifiers", "identifiers_or_typenames", "extension", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const unsigned short yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,    61,   298,    63,    58,   299,   300,   124,
      94,    38,   301,   302,   303,   304,    43,    45,    42,    47,
      37,   305,   306,   307,   308,    46,    40,    91,   309,   310,
     311,   312,   313,   314,   315,   316,   317,   318,   319,   320,
     321,   322,   323,    41,    59,   125,   126,    33,    44,   123,
      93
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const unsigned short yyr1[] =
{
       0,    91,    92,    92,    94,    93,    95,    93,    96,    96,
      96,    96,    97,    97,    97,    97,    97,    97,    97,    99,
     100,    98,    98,   101,   102,    98,    98,   103,   104,    98,
      98,   105,   105,   106,   106,   106,   106,   106,   106,   106,
     107,   108,   108,   109,   109,   110,   110,   110,   110,   110,
     110,   110,   110,   110,   110,   110,   111,   112,   113,   114,
     114,   115,   115,   115,   115,   115,   115,   115,   115,   115,
     115,   115,   115,   115,   116,   115,   117,   115,   118,   119,
     115,   120,   115,   115,   115,   121,   121,   121,   121,   122,
     121,   121,   121,   121,   121,   121,   121,   121,   121,   121,
     121,   121,   121,   121,   123,   123,   124,   124,   124,   125,
     126,   126,   126,   126,   127,   127,   127,   127,   128,   129,
     130,   131,   131,   131,   131,   131,   131,   132,   132,   132,
     133,   134,   134,   135,   135,   136,   136,   136,   136,   136,
     136,   136,   137,   137,   137,   137,   137,   137,   138,   138,
     138,   138,   138,   138,   139,   139,   139,   139,   139,   140,
     140,   140,   140,   140,   140,   140,   141,   142,   142,   142,
     142,   142,   142,   143,   144,   144,   144,   144,   144,   144,
     144,   144,   144,   144,   145,   145,   145,   145,   145,   146,
     146,   146,   146,   146,   146,   146,   146,   146,   146,   147,
     147,   147,   147,   147,   148,   148,   148,   148,   148,   148,
     148,   148,   149,   149,   149,   149,   149,   149,   149,   149,
     150,   150,   150,   150,   151,   151,   151,   151,   152,   152,
     152,   152,   153,   153,   153,   153,   154,   154,   154,   154,
     154,   154,   154,   154,   155,   155,   155,   155,   155,   155,
     155,   155,   155,   155,   155,   155,   155,   155,   155,   155,
     156,   156,   157,   157,   158,   159,   159,   160,   161,   161,
     161,   162,   162,   163,   163,   164,   164,   166,   165,   165,
     168,   167,   167,   169,   169,   170,   170,   171,   172,   172,
     173,   173,   173,   173,   173,   174,   174,   174,   174,   175,
     176,   175,   175,   177,   177,   178,   178,   179,   179,   180,
     179,   179,   182,   181,   181,   181,   183,   183,   184,   184,
     184,   186,   187,   185,   189,   190,   188,   191,   191,   192,
     192,   192,   192,   192,   193,   193,   194,   194,   194,   195,
     195,   195,   195,   195,   196,   196,   196,   196,   196,   197,
     197,   198,   198,   199,   199,   201,   200,   200,   202,   200,
     200,   203,   200,   204,   200,   205,   205,   205,   206,   206,
     207,   207,   208,   208,   209,   209,   209,   210,   210,   210,
     210,   210,   210,   211,   211,   212,   212,   213,   213,   213,
     214,   214,   214,   215,   215,   215,   216,   216,   218,   217,
     219,   219,   220,   220,   220,   221,   221,   222,   222,   223,
     223,   224,   224,   224,   224,   224,   225,   225,   225,   225,
     225,   225,   225,   225,   225,   226,   226,   226,   226,   227,
     227,   227,   227,   227,   228,   228,   228,   228,   229,   229,
     229,   229,   229,   230,   230,   231,   231,   231,   231,   232,
     233,   234,   235,   236,   237,   237,   238,   238,   239,   240,
     240,   241,   242,   242,   243,   243,   244,   245,   246,   246,
     248,   247,   250,   249,   251,   252,   253,   253,   254,   255,
     256,   258,   257,   257,   257,   259,   260,   257,   257,   257,
     261,   262,   263,   264,   257,   265,   257,   266,   266,   267,
     267,   267,   267,   267,   267,   267,   267,   267,   267,   267,
     267,   267,   267,   268,   268,   268,   268,   269,   269,   270,
     270,   271,   271,   272,   272,   273,   273,   274,   274,   276,
     275,   277,   278,   279,   277,   277,   280,   280,   280,   280,
     281,   281,   282,   282,   282,   282,   282,   283,   283,   283,
     283,   283,   284,   286,   285,   287,   287,   288,   288,   289,
     289,   290
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const unsigned char yyr2[] =
{
       0,     2,     0,     1,     0,     2,     0,     3,     1,     1,
       5,     2,     3,     4,     4,     2,     2,     2,     1,     0,
       0,     9,     4,     0,     0,     9,     4,     0,     0,     8,
       3,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     0,     1,     1,     3,     1,     2,     2,     2,     2,
       2,     4,     2,     4,     2,     2,     1,     1,     1,     1,
       4,     1,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     0,     4,     0,     4,     0,     0,
       7,     0,     5,     3,     3,     1,     1,     1,     1,     0,
       7,     3,     3,     3,     3,     4,     6,     8,     6,     4,
       3,     3,     2,     2,     1,     2,     0,     1,     2,     3,
       1,     1,     2,     2,     4,     4,     2,     2,     3,     0,
       1,     4,     4,     3,     3,     2,     2,     1,     2,     2,
       2,     2,     2,     1,     2,     1,     2,     2,     2,     2,
       2,     2,     1,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     1,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       0,     1,     1,     1,     1,     1,     1,     1,     1,     4,
       4,     1,     4,     1,     4,     0,     4,     0,     6,     3,
       0,     6,     3,     0,     1,     1,     2,     6,     1,     3,
       0,     1,     4,     6,     4,     1,     1,     1,     1,     1,
       0,     4,     1,     0,     2,     1,     3,     3,     2,     0,
       4,     1,     0,     4,     1,     1,     1,     2,     2,     5,
       3,     0,     0,     7,     0,     0,     7,     1,     1,     4,
       3,     2,     3,     1,     1,     1,     3,     2,     1,     3,
       2,     3,     3,     4,     3,     4,     3,     2,     1,     1,
       2,     1,     2,     1,     2,     0,     7,     5,     0,     7,
       5,     0,     8,     0,     7,     2,     2,     2,     0,     1,
       0,     1,     1,     2,     0,     3,     2,     3,     4,     3,
       1,     1,     2,     1,     4,     1,     4,     4,     6,     5,
       4,     6,     5,     1,     3,     1,     1,     3,     0,     3,
       0,     1,     0,     1,     2,     1,     1,     1,     3,     2,
       3,     4,     3,     2,     2,     1,     3,     4,     2,     3,
       3,     4,     4,     5,     5,     1,     1,     1,     1,     1,
       2,     2,     2,     2,     1,     2,     2,     2,     1,     2,
       2,     2,     2,     1,     2,     1,     1,     1,     1,     2,
       0,     0,     0,     0,     0,     1,     1,     2,     3,     1,
       2,     1,     1,     5,     1,     1,     2,     2,     2,     2,
       0,     5,     0,     4,     0,     0,     1,     2,     3,     3,
       3,     0,     4,     1,     3,     0,     0,     7,     5,     2,
       0,     0,     0,     0,    12,     0,     6,     2,     1,     1,
       2,     3,     2,     2,     2,     3,     6,     8,    10,    12,
       3,     4,     1,     3,     5,     2,     5,     0,     1,     0,
       1,     0,     1,     1,     3,     4,     7,     1,     3,     0,
       3,     2,     0,     0,     6,     2,     0,     1,     1,     3,
       1,     3,     4,     4,     3,     4,     3,     4,     4,     3,
       4,     3,     1,     0,     3,     1,     2,     1,     3,     1,
       3,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const unsigned short yydefact[] =
{
       4,     0,     6,     0,     1,     0,     0,   268,   159,   265,
     127,   353,   349,   351,     0,    58,     0,   561,    18,     5,
       9,     8,     0,     0,   212,   213,   214,   215,   204,   205,
     206,   207,   216,   217,   218,   219,   208,   209,   210,   211,
     119,   119,     0,   135,   142,   262,   264,   263,   133,   285,
       0,     0,     0,   267,   266,     0,     7,    16,    17,   354,
     350,   352,     0,     0,     0,   348,   260,   283,     0,   273,
       0,   162,   128,   140,   146,   130,   163,   129,   141,   147,
     169,   131,   152,   157,   134,   170,   132,   153,   158,   180,
     136,   138,   144,   143,   181,   137,   139,   145,   195,   148,
     150,   155,   154,   196,   149,   151,   156,   164,   160,   178,
     187,   166,   165,   161,   179,   188,   171,   167,   193,   202,
     173,   172,   168,   194,   203,   182,   174,   176,   185,   184,
     183,   175,   177,   186,   197,   189,   191,   200,   199,   198,
     190,   192,   201,     0,     0,    15,   286,    31,    32,   374,
     365,   374,   366,   363,   367,    11,    85,    86,   104,    56,
      57,     0,     0,     0,     0,     0,    88,     0,    33,    35,
      34,     0,    37,    36,     0,    38,    39,     0,     0,    40,
      59,     0,     0,    61,    43,    45,    87,     0,     0,   290,
       0,   240,   241,   242,   243,   236,   237,   238,   239,   398,
       0,   232,   233,   234,   235,   261,     0,     0,   284,    12,
     283,    30,     0,   283,     0,     0,   283,   347,   333,   260,
     283,     0,   271,     0,   327,   328,     0,     0,     0,     0,
     355,     0,   358,     0,   361,    54,    55,     0,     0,     0,
      49,    46,     0,   466,     0,     0,    48,     0,     0,     0,
      50,     0,    52,     0,     0,    78,    76,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     103,   102,     0,    41,     0,     0,   105,     0,   462,   454,
       0,    47,   296,   297,   298,   295,     0,   288,   291,   269,
     400,   270,   346,     0,     0,   120,     0,   553,   344,     0,
       0,   418,     0,     0,     0,    28,     0,   474,   111,   475,
     282,     0,     0,    14,   283,    22,     0,   283,   283,   331,
      13,    26,     0,   283,   381,   376,   232,   233,   234,   235,
     228,   229,   230,   231,   119,   119,   373,     0,   374,   283,
     374,   395,   396,   370,   393,     0,     0,     0,     0,    92,
      91,     0,    10,    44,     0,     0,    83,    84,     0,     0,
       0,     0,    72,    73,    71,    70,    69,    68,    67,    62,
      63,    64,    65,    66,   100,     0,    42,     0,   101,    94,
       0,     0,   455,   456,    93,     0,   290,    41,   260,   283,
     399,   401,   406,   405,   407,   415,   345,   274,   275,     0,
       0,     0,     0,   420,   416,     0,     0,   419,     0,   449,
     474,   113,   108,   112,     0,   280,   332,     0,     0,    20,
     279,   330,    24,   357,   474,   474,   375,   382,     0,   360,
       0,     0,   371,     0,   370,     0,     0,     0,    89,    60,
      51,    53,     0,     0,    77,    75,    95,    99,   559,     0,
     465,   434,   464,   474,   474,   474,   474,     0,   443,     0,
     475,   429,   438,   457,   287,   289,    85,     0,   409,   529,
     414,   283,   413,   276,     0,   557,   537,   224,   225,   220,
     221,   226,   227,   222,   223,   119,   119,   555,     0,   538,
     540,   554,     0,   422,     0,     0,   421,   417,   475,   109,
     119,   119,     0,   329,   272,   275,   474,   277,   474,   377,
     383,   475,   379,   385,   475,   283,   283,   397,   394,   283,
       0,     0,     0,     0,     0,    79,    82,   458,     0,   435,
     430,   439,   436,   431,   440,   475,   432,   441,   437,   433,
     442,   444,   451,   452,   292,     0,   294,   408,   410,     0,
       0,   529,   412,   535,   552,   402,   402,   531,   532,     0,
     556,     0,   423,   424,     0,   116,     0,   117,     0,   302,
     300,   299,   281,   475,     0,   475,   283,   378,   283,     0,
     356,   359,   364,   283,    96,     0,    98,   315,    85,     0,
       0,   312,     0,   314,     0,   368,   305,   311,     0,     0,
       0,   560,   452,   463,   268,     0,     0,     0,     0,     0,
       0,   517,   512,   461,   474,     0,   118,   119,   119,     0,
       0,   450,   499,   479,   480,     0,     0,   411,   530,   338,
     260,   283,   283,   334,   335,   283,   549,   403,   406,   260,
     283,   283,   551,   283,   539,   212,   213,   214,   215,   204,
     205,   206,   207,   216,   217,   218,   219,   208,   209,   210,
     211,   119,   119,   541,   558,     0,    29,   459,     0,     0,
       0,     0,   278,     0,   474,     0,   283,   474,     0,   283,
     362,     0,   318,     0,     0,   309,    90,     0,   304,     0,
     317,   308,    80,     0,   515,   502,   503,   504,     0,     0,
       0,   518,     0,   475,   500,     0,     0,   125,   470,   485,
     472,   490,     0,   483,     0,     0,   453,   467,   126,   293,
     409,   529,   547,   283,   337,   283,   340,   548,   404,   409,
     529,   550,   533,   402,   402,   460,   114,   115,     0,    21,
      25,   384,   475,   283,     0,   387,   386,   283,     0,   390,
      97,     0,   320,     0,     0,   306,   307,     0,   513,   505,
       0,   510,     0,     0,     0,   123,   321,     0,   124,   324,
       0,     0,   452,     0,     0,     0,   469,   474,   468,   489,
       0,   501,   341,   342,     0,   336,   339,     0,   283,   283,
     544,   283,   546,   301,     0,   389,   283,   392,   283,     0,
     313,   310,     0,   511,     0,   283,   121,     0,   122,     0,
       0,     0,     0,   519,     0,   484,   452,   453,   476,   474,
       0,   343,   534,   542,   543,   545,   388,   391,   319,   514,
     521,     0,   516,   322,   325,     0,     0,   473,   520,   498,
     491,     0,   495,   482,   478,   477,     0,     0,     0,     0,
     522,   523,   506,   474,   474,   471,   486,   519,   497,   452,
     488,     0,     0,   521,     0,     0,   475,   475,   452,     0,
     496,     0,     0,     0,   507,   524,     0,     0,   487,   492,
     525,     0,     0,     0,   323,   326,   519,     0,   527,     0,
     508,     0,     0,     0,     0,   493,   526,   509,   528,   452,
     494
};

/* YYDEFGOTO[NTERM-NUM]. */
static const short yydefgoto[] =
{
      -1,     1,     2,     3,     5,    19,    20,    21,   316,   506,
     322,   508,   215,   410,   592,   177,   244,   375,   179,   180,
     181,   182,    22,   183,   184,   361,   360,   358,   600,   359,
     185,   524,   186,   305,   306,   307,   499,   451,    23,   294,
     616,   191,   192,   193,   194,   195,   196,   197,   198,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,   485,
     486,   334,   205,   199,    42,   206,    43,    44,    45,    46,
      47,   221,    68,   216,   222,   574,    69,   502,   295,   208,
      49,   286,   287,   288,   572,   670,   594,   595,   596,   754,
     597,   684,   598,   599,   765,   807,   853,   768,   809,   854,
     505,   224,   632,   633,   634,   225,    50,    51,    52,    53,
     338,   340,   345,   233,    54,   688,   433,   228,   229,   336,
     509,   512,   510,   513,   343,   344,   200,   290,   390,   636,
     637,   392,   393,   394,   217,   452,   453,   454,   455,   456,
     457,   308,   279,   603,   777,   781,   381,   382,   383,   666,
     621,   280,   459,   187,   667,   713,   714,   770,   715,   772,
     309,   414,   817,   778,   818,   819,   716,   816,   771,   868,
     773,   857,   886,   899,   859,   840,   623,   624,   702,   841,
     849,   850,   851,   889,   470,   550,   487,   643,   787,   488,
     489,   663,   490,   555,   298,   400,   491,   492,   449,   188
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -713
static const short yypact[] =
{
     159,   169,   177,   961,  -713,   961,   441,  -713,  -713,  -713,
    -713,   196,   196,   196,   166,  -713,   192,  -713,  -713,  -713,
    -713,  -713,   213,   345,   300,   515,  1115,  1018,   458,   408,
     796,   488,  1236,  1269,  1240,  1454,  1309,   751,  1501,   885,
    -713,  -713,   208,  -713,  -713,  -713,  -713,  -713,   196,  -713,
     127,   133,   150,  -713,  -713,   961,  -713,  -713,  -713,   196,
     196,   196,  2755,   229,  2673,  -713,   120,   196,   163,  -713,
    1167,  -713,  -713,  -713,  -713,   196,  -713,  -713,  -713,  -713,
    -713,  -713,  -713,  -713,   196,  -713,  -713,  -713,  -713,  -713,
    -713,  -713,  -713,   196,  -713,  -713,  -713,  -713,  -713,  -713,
    -713,  -713,   196,  -713,  -713,  -713,  -713,  -713,  -713,  -713,
    -713,   196,  -713,  -713,  -713,  -713,  -713,  -713,  -713,  -713,
     196,  -713,  -713,  -713,  -713,  -713,  -713,  -713,  -713,   196,
    -713,  -713,  -713,  -713,  -713,  -713,  -713,  -713,   196,  -713,
    -713,  -713,  -713,   501,   345,  -713,  -713,  -713,  -713,  -713,
     245,  -713,   248,  -713,   255,  -713,  -713,  -713,  -713,  -713,
    -713,  2755,  2755,   330,   343,   357,  -713,   534,  -713,  -713,
    -713,  2755,  -713,  -713,  1774,  -713,  -713,  2755,   275,   342,
    -713,  2796,  2837,  -713,  3084,   755,   424,  1536,  2755,   615,
     359,  1022,   784,  1163,  3080,  1077,   719,  1743,  1102,  -713,
     367,   258,   460,   287,   469,  -713,   345,   345,   196,  -713,
     196,  -713,   413,   196,  2206,   358,   196,  -713,  -713,   120,
     196,   238,  -713,   447,   499,   502,   289,  2141,   421,   930,
    -713,   425,  -713,   482,  -713,  -713,  -713,  2755,  2755,  3049,
    -713,  -713,   430,  -713,   451,   456,  -713,   446,  2755,  1774,
    -713,  1774,  -713,  2755,  2755,   462,  -713,  -713,  2755,  2755,
    2755,  2755,  2755,  2755,  2755,  2755,  2755,  2755,  2755,  2755,
    -713,  -713,   534,  2755,  2755,   534,  -713,   463,  -713,   542,
     496,  -713,  -713,  -713,  -713,  -713,   122,  -713,   524,  -713,
     224,  -713,   502,   351,   345,  -713,   586,  -713,  -713,  2673,
    2307,  -713,   508,  2247,   518,  -713,   111,    79,  -713,  -713,
     568,   501,   501,  -713,   196,  -713,   358,   196,   196,  -713,
    -713,  -713,   358,   196,  -713,  -713,  1022,   784,  1163,  3080,
    1077,   719,  1743,  1102,  -713,   493,   550,  2169,  -713,   196,
    -713,  -713,   595,   555,  -713,   482,  2942,  2960,   557,  -713,
    -713,  2548,  -713,  3084,   564,   570,  3084,  3084,  2755,   605,
    2755,  2755,  2510,  1799,  1464,  2635,   783,   997,   997,   308,
     308,  -713,  -713,  -713,  -713,   574,   342,   569,  -713,  -713,
     534,  1391,   542,  -713,  -713,   578,   615,  2878,   120,   196,
    -713,  -713,  -713,  -713,   516,  -713,  -713,  -713,   179,    89,
    1211,   585,  2755,  -713,  -713,  2755,  2348,  -713,   601,  -713,
    -713,  -713,  -713,  -713,  1373,  -713,   499,   432,   501,  -713,
     650,  -713,  -713,  -713,  -713,  -713,  -713,  -713,   614,  -713,
     616,  2755,   534,   620,   555,  3049,  2755,  3049,  -713,  -713,
     630,   630,   677,  2755,  3113,  2064,  -713,  -713,  -713,   292,
     518,  -713,  -713,    78,    93,    96,   118,   737,  -713,   659,
    -713,  -713,  -713,  -713,  -713,  -713,   155,   667,   224,   224,
    -713,   196,  -713,  -713,   679,  -713,  -713,  2050,  1458,   690,
     772,  2317,  2079,   740,  1299,  -713,  -713,  -713,   683,   310,
    -713,  -713,   189,  -713,   652,   681,  -713,  -713,  -713,  -713,
     696,   699,  2039,  -713,  -713,   760,  -713,  -713,  -713,   705,
    -713,  -713,   706,  -713,  -713,   196,   196,  3084,  -713,   196,
     682,   717,  2984,   721,  1838,  -713,  3100,  -713,   534,  -713,
    -713,  -713,  -713,  -713,  -713,  -713,  -713,  -713,  -713,  -713,
    -713,  -713,  -713,  2418,  -713,  2755,  -713,  -713,  -713,   722,
     855,  -713,  -713,  -713,  -713,   321,   283,  -713,  -713,  1336,
    -713,   803,  -713,  -713,    73,  -713,   501,  -713,   345,  -713,
    -713,  3084,  -713,  -713,  2039,  -713,   196,   547,   196,   366,
    -713,  -713,  -713,   196,  -713,  2755,  -713,  -713,   761,   534,
    2755,  -713,   767,  3084,   734,   739,  -713,  -713,   211,  1972,
    2755,  -713,  2487,  -713,   786,  2755,   798,   745,   746,  2714,
     265,   842,  -713,  -713,  -713,   768,  -713,  -713,  -713,   769,
     986,   770,  -713,  -713,  -713,  2612,   215,  -713,  -713,  -713,
     120,   196,   196,   598,   644,   218,  -713,  -713,   196,   120,
     196,   218,  -713,   196,  -713,  2050,  1458,  2458,  2527,   690,
     772,  1063,  1569,  2317,  2079,  3045,  3076,   740,  1299,  1377,
    1656,  -713,  -713,  -713,  -713,   774,  -713,  -713,   313,   320,
    1838,    73,  -713,    73,  -713,  2755,   130,  -713,  2755,   457,
    -713,  1556,  -713,  1440,  1838,  -713,  -713,  1905,  -713,  2101,
    -713,  -713,  3100,  2923,  -713,  -713,  -713,  -713,   780,  2755,
     782,  -713,   804,  -713,  -713,   501,   345,  -713,  -713,  -713,
    -713,  -713,   806,   861,  1623,   107,  -713,  -713,  -713,  -713,
     321,   333,  -713,   196,  -713,   196,  -713,  -713,   196,   283,
     283,  -713,  -713,   321,   283,  -713,  -713,  -713,   793,  -713,
    -713,  -713,  -713,  1643,  2755,  -713,  -713,  1643,  2755,  -713,
    -713,  2755,  -713,   794,  2101,  -713,  -713,  2755,  -713,  -713,
     797,  -713,  2755,   836,   322,  -713,   606,   355,  -713,   655,
     820,   821,  -713,   823,  2755,  1710,  -713,  -713,  -713,  -713,
    2755,  -713,   598,   644,   466,  -713,  -713,   855,   196,   218,
    -713,   218,  -713,  -713,   547,  -713,  1643,  -713,  1643,  2898,
    -713,  -713,  3066,  -713,    60,   196,  -713,   358,  -713,   358,
    2755,  2755,   878,  2612,   817,  -713,  -713,  -713,  -713,  -713,
     818,  -713,  -713,  -713,  -713,  -713,  -713,  -713,  -713,  -713,
     140,   819,  -713,  -713,  -713,   833,   834,  -713,  -713,  -713,
    -713,   835,  -713,  -713,  -713,  -713,   838,   852,   534,    63,
     814,  -713,  -713,  -713,  -713,  -713,  -713,  2755,  -713,  -713,
    -713,  2755,   837,   140,   839,   140,  -713,  -713,  -713,   844,
    -713,   843,   924,   112,  -713,  -713,   774,   774,  -713,  -713,
    -713,   873,   586,   875,  -713,  -713,  2755,  2755,   424,   233,
    -713,   874,   880,   886,   586,  -713,  -713,  -713,   424,  -713,
    -713
};

/* YYPGOTO[NTERM-NUM].  */
static const short yypgoto[] =
{
    -713,  -713,  -713,  -713,  -713,   102,  -713,  -713,  -713,  -713,
    -713,  -713,  -713,  -713,    82,  -713,   -62,   584,  -252,   548,
    -713,  -713,  -713,  -107,   687,  -713,  -713,  -713,  -713,  -713,
    -713,  -713,  -270,  -300,   665,  -713,  -713,   100,    11,  -295,
    -580,     2,     5,    37,    38,    41,    44,    -2,    45,  -380,
    -342,   418,   420,  -341,  -331,   426,   427,  -481,  -458,   573,
     576,  -713,  -153,  -109,  -511,  -202,   582,   654,   678,   876,
    -713,  -476,  -133,  -208,   566,  -713,   700,  -713,   253,     1,
      27,  -713,   607,  -713,   428,  -713,  -529,  -713,   325,  -713,
    -563,  -713,  -713,   397,  -713,  -713,  -713,  -713,  -713,  -713,
    -120,   423,   264,   293,    40,     7,  -713,  -713,  -713,  -713,
    -713,  -713,  -713,  -713,  -713,  -713,   583,   -85,  -713,   670,
    -713,  -713,   344,   339,   675,   589,   -52,  -713,  -713,  -474,
    -281,  -373,  -440,  -713,   365,  -713,  -713,  -713,  -713,  -713,
    -713,  -215,  -713,  -713,  -401,   216,  -713,  -713,   658,  -182,
    -713,   416,  -713,  -713,  -525,  -713,  -713,  -713,  -713,  -713,
     657,  -350,   220,  -688,  -192,  -145,  -713,  -713,  -713,  -713,
    -713,  -713,  -713,  -713,  -713,  -713,  -713,  -713,  -713,  -712,
     180,  -713,   197,  -713,   590,  -713,  -512,  -713,  -713,  -713,
    -713,  -713,  -713,   579,  -294,  -713,  -713,  -713,  -713,    58
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -537
static const short yytable[] =
{
     178,    30,   190,    30,    48,    24,    48,    24,    25,   391,
      25,   226,    59,    60,    61,   317,   419,   311,   622,   418,
     481,   376,   422,   223,   421,    75,   399,    84,   548,    93,
      70,   102,   619,   111,   481,   120,   691,   129,   628,   138,
      26,    27,    26,    27,    28,   718,    28,    29,    31,    29,
      31,   143,   144,    30,   235,   236,    48,    24,   482,   483,
      25,    55,   617,    55,   241,    48,   231,    48,   201,   484,
     246,   202,   482,   483,   665,   146,   335,   622,   661,  -445,
    -107,   281,   642,   484,   812,   618,   146,   146,   146,   412,
     668,   411,    26,    27,  -446,   547,    28,  -447,   276,    29,
      31,   662,   146,   203,   204,   303,   830,    56,   779,   863,
     543,   146,   304,    55,   619,  -110,  -110,  -110,  -110,  -448,
     146,  -110,   245,  -110,  -110,  -110,   756,    10,   843,   146,
     147,   148,   150,   152,   154,   376,   147,   148,   146,  -110,
     735,   738,   620,   831,   617,   869,   864,   146,   564,   847,
      16,   227,   302,   147,   148,   753,   146,   155,   882,    -2,
      16,   577,   613,  -425,   579,   146,   458,   618,  -107,     4,
     481,   870,   473,   780,   891,    48,   744,    -3,  -426,   653,
     878,  -427,   638,   638,   335,   602,   468,   348,   549,   461,
     402,   801,    75,   241,    84,   883,    93,   354,   102,   355,
    -110,   620,    75,  -428,    84,   385,   212,   848,   482,   483,
     386,   900,   377,   292,   293,    48,   149,   654,   657,   484,
      48,   201,   151,   671,   202,   673,    16,   332,   658,   764,
      48,   326,    62,   839,   327,   146,   462,   401,   544,   153,
      48,   408,   541,   545,   439,   213,   214,   209,    16,   240,
      48,   210,    48,   428,   689,   430,   203,   204,    63,   790,
     792,   530,   533,   536,   539,    72,   328,   329,   147,   148,
     330,   285,   560,   331,   333,   822,   589,   561,   590,    64,
     548,   674,   388,   677,   213,   214,    65,   337,    16,   548,
     389,   214,   145,   626,    81,   189,   442,   317,   719,   241,
      48,   398,   619,   248,     7,    71,     9,    72,   531,   534,
     537,   540,    11,    12,    13,   342,   893,    16,   292,   293,
     207,   894,   313,   699,    65,   629,   314,    75,    15,    84,
      16,    93,   617,   102,   230,   332,    65,   232,    48,   326,
     494,   639,   327,   495,   234,   424,   425,   547,    65,   640,
     214,   884,   885,   763,   374,   618,   547,   378,   247,   304,
     638,   638,  -474,  -474,  -474,  -474,   267,   268,   269,    65,
    -474,  -474,  -474,   320,   328,   329,   527,   210,   330,   630,
     528,   331,   333,   521,  -244,   523,  -474,   631,   214,    48,
     201,   630,   794,   202,   558,   337,   237,   736,   559,   631,
     214,   314,   477,    66,   737,   478,   806,   481,   210,   238,
     314,    67,   678,    94,     9,    95,   477,   213,   214,   478,
      11,    12,    13,   239,    66,   203,   204,   342,   720,   785,
     248,   786,    67,   276,   396,   669,    48,   729,    48,   808,
     549,   479,   289,   210,   480,   482,   483,  -106,   315,   549,
     291,   -19,   -19,   -19,   -19,   479,   484,   676,   480,   -19,
     -19,   -19,   448,    89,     9,    90,   297,    77,   285,   310,
      11,    12,    13,   312,   212,   -19,    86,  -275,    75,   296,
      93,   615,   111,   341,   129,   147,   148,    16,    16,   739,
    -275,   740,  -249,   103,     9,   104,   554,   554,   318,   214,
      11,    12,    13,   748,    65,   218,   323,   833,   -81,   834,
     339,   566,   568,   349,   342,   503,   876,   877,   281,     7,
      76,     9,    77,   213,   214,    57,    58,    11,    12,    13,
     352,  -275,   725,   214,   350,  -275,   -19,   147,   148,   351,
     615,    30,  -248,    15,    48,    24,   379,   698,    25,   821,
      65,   218,   477,   529,   532,   478,   538,   651,   317,   219,
      48,   645,   635,   641,   646,   318,   214,   220,   213,   214,
     420,   297,  -251,   767,   380,   398,   423,  -380,  -380,   384,
      26,    27,   471,   214,    28,   766,   679,    29,    31,   319,
     387,   479,   429,   675,   480,   158,   647,   648,   404,  -245,
     649,   625,   409,   650,   652,   219,    73,    78,    82,    87,
     601,   415,   888,   220,   109,   114,   118,   123,   147,   148,
     282,   283,   284,    30,   898,   614,    48,    24,   705,   706,
      25,    48,   201,   212,   426,   202,  -275,   760,   431,   728,
      48,   201,   469,   432,   202,   437,    75,   440,    84,  -275,
      93,   443,   102,   441,   111,   395,   120,   446,   129,   447,
     138,   464,    26,    27,   723,   214,    28,   203,   204,    29,
      31,   682,   733,   734,   676,   493,   203,   204,    74,    79,
      83,    88,   212,   625,   614,  -275,   110,   115,   119,   124,
    -275,   497,   700,   507,  -275,    89,     9,    90,  -275,   515,
     804,   516,    11,    12,    13,   519,    91,    96,   100,   105,
     725,   214,   814,   769,   127,   132,   136,   141,   820,   438,
      16,   213,   214,   525,   551,     9,    95,   292,   293,   250,
     252,    11,    12,    13,   416,   417,   292,   293,   304,  -275,
     789,   791,   562,  -275,   542,   125,     9,   126,   835,   836,
     546,   838,    11,    12,    13,   146,   130,     9,   131,   472,
     783,   784,   553,    11,    12,    13,   557,   583,   580,   581,
      16,   563,   582,    73,    78,    82,    87,    94,     9,    95,
     565,   319,   319,   567,    11,    12,    13,   212,     7,   477,
       9,    77,   478,   576,   578,   838,    11,    12,    13,   871,
     584,    98,     9,    99,   586,   627,   664,   -31,    11,    12,
      13,    30,    15,   685,    48,    24,   270,   271,    25,   686,
     272,   273,   274,   275,   838,   892,    16,   687,   479,   695,
     696,   480,   -32,   395,   395,  -257,   680,   263,   264,   265,
     266,   267,   268,   269,   694,    74,    79,    83,    88,   701,
      26,    27,   704,   707,    28,   278,   474,    29,    31,     7,
       8,     9,    10,   613,   759,   476,   761,    11,    12,    13,
     762,   625,   774,    91,    96,   100,   105,   775,   793,   800,
    -250,   803,   805,    15,   721,   722,   810,   811,   727,   813,
     139,     9,   140,   730,   731,   837,   732,    11,    12,    13,
     842,   846,   865,   852,    92,    97,   101,   106,    73,    78,
      82,    87,   128,   133,   137,   142,   855,   856,   861,   858,
     395,   395,   860,   874,   346,   347,   880,   872,   879,   745,
     862,   324,   749,   881,     7,   353,     9,    10,  -536,   887,
     356,   357,    11,    12,    13,   362,   363,   364,   365,   366,
     367,   368,   369,   370,   371,   372,   373,   895,    15,   890,
      16,    17,     6,   896,  -119,     7,     8,     9,    10,  -259,
     897,   467,   413,    11,    12,    13,   297,   655,   297,   656,
      74,    79,    83,    88,   504,   659,   660,   500,    14,    15,
     501,    16,    17,   465,   397,   690,   795,   788,   724,   726,
     797,   708,   672,   709,   710,   711,   712,   427,    91,    96,
     100,   105,   755,   782,   325,  -372,   746,   520,   741,  -119,
     434,   518,     7,    85,     9,    86,     7,  -119,     9,    72,
      11,    12,    13,   844,    11,    12,    13,   717,   460,   845,
     463,   823,   824,   873,   825,    18,    15,   444,   445,   826,
      15,   827,    16,   265,   266,   267,   268,   269,   832,    73,
      78,   552,   875,   109,   114,   556,     0,   498,    98,     9,
      99,    92,    97,   101,   106,    11,    12,    13,     0,     0,
       0,   511,   514,     9,    90,   395,   395,     0,     0,    11,
      12,    13,     0,    16,   395,   395,     0,     0,   395,   395,
       0,     0,  -247,     0,     0,     0,     0,    16,     9,   104,
     460,   460,   535,   460,    11,    12,    13,     0,   517,     7,
      80,     9,    81,   522,     0,     0,     0,    11,    12,    13,
     526,    74,    79,     0,     0,   110,   115,     0,     0,     0,
       0,     0,     0,    15,     0,    16,     0,   724,   726,   726,
       0,     0,     0,     0,     0,     0,     0,    91,    96,     0,
       0,   127,   132,   573,     0,   575,     0,     7,   211,     9,
      81,   -27,   -27,   -27,   -27,    11,    12,    13,     0,   -27,
     -27,   -27,     0,     0,     0,     0,     0,     0,     0,   571,
       0,    15,     0,    16,   212,   -27,     0,  -275,     0,  -246,
       0,     0,     0,     0,     0,     0,    92,    97,   101,   106,
    -275,   593,   474,     0,   475,     7,     8,     9,    10,     0,
       0,   476,     0,    11,    12,    13,     0,    73,    78,    82,
      87,     0,     0,   213,   214,   109,   114,   118,   123,    15,
       7,   107,     9,   108,     7,   116,     9,   117,    11,    12,
      13,  -275,    11,    12,    13,  -275,   -27,     0,     0,     0,
       0,   571,     0,     0,    15,     0,    16,     0,    15,     0,
      16,   703,   681,     7,   112,     9,   113,   683,     0,     0,
       0,    11,    12,    13,     0,     0,   593,   692,     0,     0,
       0,     0,   693,     0,  -536,     0,     0,    15,     0,    74,
      79,    83,    88,     0,   130,     9,   131,   110,   115,   119,
     124,    11,    12,    13,   125,     9,   126,     0,     0,     0,
    -252,    11,    12,    13,  -254,     0,     0,    91,    96,   100,
     105,   742,     0,     0,   514,   127,   132,   136,   141,    16,
       7,     8,     9,    10,     0,     0,   644,     0,    11,    12,
      13,     0,     0,  -253,     0,    92,    97,   593,     0,   128,
     133,     0,   743,     0,    15,   747,    16,     0,     0,     0,
       0,   593,     0,     0,   593,     0,   593,     7,     8,     9,
      10,     0,   134,     9,   135,    11,    12,    13,     0,    11,
      12,    13,   450,  -256,  -474,  -474,  -474,  -474,  -474,  -474,
    -474,    15,  -474,  -474,  -474,  -474,  -474,    16,  -474,  -474,
    -474,  -474,  -474,  -474,  -474,  -474,  -474,  -474,  -474,  -474,
    -474,  -474,  -474,     0,  -474,  -474,  -474,  -474,  -474,     0,
       0,   796,     0,  -474,   535,   798,     0,     0,   799,  -474,
       0,   593,  -474,     0,   802,     0,     0,  -474,  -474,  -474,
     751,     0,  -474,  -474,     0,     0,     0,  -474,     7,   121,
       9,   122,     7,    76,     9,    77,    11,    12,    13,     0,
      11,    12,    13,     0,     0,  -474,   535,  -474,  -474,     0,
    -474,     0,    15,   253,   254,   255,    15,   256,   257,   258,
     259,   260,   261,   262,   263,   264,   265,   266,   267,   268,
     269,     0,     0,     0,     0,     0,   134,     9,   135,     0,
     866,   867,     0,    11,    12,    13,   261,   262,   263,   264,
     265,   266,   267,   268,   269,    92,    97,   101,   106,     0,
     752,    16,     0,   128,   133,   137,   142,   277,  -255,  -450,
    -450,  -450,  -450,  -450,  -450,  -450,     0,  -450,  -450,  -450,
    -450,  -450,     0,  -450,  -450,  -450,  -450,  -450,  -450,  -450,
    -450,  -450,  -450,  -450,  -450,  -450,  -450,  -450,  -450,  -450,
    -450,  -450,  -450,  -450,   103,     9,   104,     0,  -450,     0,
       0,    11,    12,    13,  -450,  -258,     0,  -450,     0,     0,
       0,     0,  -450,  -450,  -450,     0,     0,  -450,  -450,   253,
     254,   255,  -450,   256,   257,   258,   259,   260,   261,   262,
     263,   264,   265,   266,   267,   268,   269,     0,     0,     0,
    -450,   278,  -450,  -450,   776,  -450,  -452,  -452,     0,     0,
       0,  -452,  -452,     0,  -452,     0,     0,     0,  -452,   750,
    -452,  -452,  -452,  -452,  -452,  -452,  -452,  -452,  -452,  -452,
    -452,     0,  -452,     0,  -452,     0,  -452,  -452,  -452,  -452,
    -452,   139,     9,   140,     0,  -452,     0,     0,    11,    12,
      13,  -452,     0,    16,  -452,     0,     0,     0,     0,  -452,
    -452,  -452,     0,     0,  -452,  -452,   253,   254,   255,  -452,
     256,   257,   258,   259,   260,   261,   262,   263,   264,   265,
     266,   267,   268,   269,     0,     0,     0,  -452,     0,  -452,
    -452,   815,  -452,  -481,  -481,     0,     0,     0,  -481,  -481,
       0,  -481,     0,     0,     0,  -481,     0,  -481,  -481,  -481,
    -481,  -481,  -481,  -481,  -481,  -481,  -481,  -481,     0,  -481,
       0,  -481,     0,  -481,  -481,  -481,  -481,  -481,     0,     9,
      99,     0,  -481,     0,     0,    11,    12,    13,  -481,     0,
       0,  -481,     0,     0,     0,     0,  -481,  -481,  -481,     0,
       0,  -481,  -481,    16,     0,   242,  -481,   156,     7,     0,
       9,    10,   157,   158,     0,   159,    11,    12,    13,     0,
       0,     0,     0,     0,  -481,     0,  -481,  -481,     0,  -481,
       0,     0,    15,   160,    16,    17,     0,   161,   162,   163,
     164,   165,     0,     0,     0,     0,   166,     0,     0,     0,
       0,     0,   167,     0,     0,   168,     0,     0,     0,     0,
     169,   170,   171,     0,     0,   172,   173,     0,     0,   587,
     174,   588,   148,     0,     0,     0,   157,   158,     0,   159,
     260,   261,   262,   263,   264,   265,   266,   267,   268,   269,
     175,   176,     0,   243,     0,     0,     0,   160,     0,    17,
       0,   161,   162,   163,   164,   165,     0,     0,     0,     0,
     166,     0,     0,     0,     0,     0,   167,     0,     0,   168,
       0,     0,     0,     0,   169,   170,   171,     0,     0,   172,
     173,     0,     0,   589,   174,   590,   587,     0,   588,   148,
       0,     0,     0,   157,   158,     0,   159,     0,     0,     0,
       0,     0,     0,  -303,   175,   176,     0,   591,     0,     0,
       0,     0,     0,     0,   160,     0,    17,     0,   161,   162,
     163,   164,   165,     0,     0,     0,     0,   166,     0,     0,
       0,     0,     0,   167,     0,     0,   168,     0,     0,     0,
       0,   169,   170,   171,     0,     0,   172,   173,     0,     0,
     589,   174,   590,   587,     0,   156,     0,     0,     0,     0,
     157,   158,     0,   159,     0,     0,     0,     0,     0,     0,
    -369,   175,   176,     0,   591,     0,     0,     0,     0,     0,
       0,   160,     0,    17,     0,   161,   162,   163,   164,   165,
       0,     0,     0,     0,   166,  -316,     0,     0,     0,     0,
     167,     0,     0,   168,     0,     0,     0,     0,   169,   170,
     171,     0,     0,   172,   173,     0,     0,  -316,   174,  -316,
     569,     0,   156,     0,     0,     0,     0,   157,   158,     0,
     159,     0,     0,     0,     7,    71,     9,    72,   175,   176,
       0,   591,    11,    12,    13,     0,     0,     0,   160,     0,
      17,     0,   161,   162,   163,   164,   165,     0,    15,     0,
      16,   166,     0,     7,   112,     9,   113,   167,     0,     0,
     168,    11,    12,    13,     0,   169,   170,   171,     0,     0,
     172,   173,   587,     0,   156,   174,     0,    15,     0,   157,
     158,     0,   159,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   175,   176,     0,   570,     0,
     160,     0,    17,     0,   161,   162,   163,   164,   165,     0,
       0,     0,   321,   166,     0,   -23,   -23,   -23,   -23,   167,
       0,     0,   168,   -23,   -23,   -23,     0,   169,   170,   171,
       0,     0,   172,   173,     0,     0,     0,   174,   212,   -23,
     324,  -275,     0,     7,     0,     9,    10,     0,     0,     0,
       0,    11,    12,    13,  -275,     0,     0,   175,   176,     0,
     591,     0,     0,     0,     0,     0,     0,    15,     0,    16,
      17,     0,     0,     0,     0,     0,     0,   213,   214,   156,
       7,   299,     9,    10,   157,   158,     0,   159,    11,    12,
      13,     0,     0,     0,     0,  -275,     0,     0,     0,  -275,
     -23,     0,     0,     0,    15,   160,    16,    17,     0,   161,
     162,   163,   164,   165,     0,     0,     0,     0,   166,     0,
     156,     0,   405,     0,   167,   157,   158,   168,   159,     0,
       0,     0,   169,   170,   300,     0,     0,   172,   173,     0,
       0,     0,   174,     0,     0,     0,   160,     0,    17,     0,
     161,   162,   163,   164,   165,     0,     0,     0,     0,   166,
       0,     0,   175,   176,     0,   167,   301,     0,   168,     0,
       0,     0,     0,   169,   170,   406,     0,     0,   172,   173,
     156,     0,     0,   174,     0,   157,   158,     0,   159,     0,
       0,     7,   107,     9,   108,     0,     0,     0,     0,    11,
      12,    13,     0,   175,   176,     0,   160,   407,    17,     0,
     161,   162,   163,   164,   165,    15,     0,    16,     0,   166,
       0,   156,     0,     0,     0,   167,   157,   158,   168,   159,
       0,     0,     0,   169,   170,   171,     0,     0,   172,   173,
       0,     0,     0,   174,     0,     0,     0,   160,     0,    17,
       0,   161,   162,   163,   164,   165,     0,     0,     0,     0,
     166,     0,     0,   175,   176,     0,   167,   403,     0,   168,
       0,     0,     0,     0,   169,   170,   171,     0,     0,   172,
     173,     0,     0,     0,   174,     0,     0,     0,     0,     0,
       0,   588,   604,     8,     9,    10,   157,   158,     0,   159,
      11,    12,    13,     0,   175,   176,     0,     0,   496,   605,
     606,   607,   608,   609,   610,   611,    15,   160,    16,    17,
       0,   161,   162,   163,   164,   165,     0,     0,     0,     0,
     166,     0,     7,    80,     9,    81,   167,     0,     0,   168,
      11,    12,    13,     0,   169,   170,   171,     0,     0,   172,
     173,     0,     0,     0,   174,     0,    15,     0,    16,     0,
     588,   148,     0,     0,     0,   157,   158,     0,   159,     0,
       0,     0,   612,     0,   175,   176,     0,   613,   605,   606,
     607,   608,   609,   610,   611,     0,   160,     0,    17,     0,
     161,   162,   163,   164,   165,     0,     0,     0,     0,   166,
       0,     7,    85,     9,    86,   167,     0,     0,   168,    11,
      12,    13,     0,   169,   170,   171,     0,     0,   172,   173,
       0,   156,     0,   174,     0,    15,   157,   158,     0,   159,
     259,   260,   261,   262,   263,   264,   265,   266,   267,   268,
     269,   612,     0,   175,   176,     0,   613,   160,     0,    17,
       0,   161,   162,   163,   164,   165,     0,     0,     0,     0,
     166,     0,     0,     0,     0,     0,   167,     0,     0,   168,
       0,     0,     0,     0,   169,   170,   171,     0,     0,   172,
     173,     0,     0,     0,   174,   156,     7,     8,     9,    10,
     157,   158,     0,   159,    11,    12,    13,     0,     0,     0,
       0,     0,     0,     0,   175,   176,     0,   438,     0,     0,
      15,   160,    16,    17,     0,   161,   162,   163,   164,   165,
       0,     0,     0,     0,   166,     0,     0,     0,     0,     0,
     167,     0,     0,   168,     0,     0,     0,     0,   169,   170,
     171,     0,     0,   172,   173,     0,   156,     7,   174,     9,
      10,   157,   158,     0,   159,    11,    12,    13,   262,   263,
     264,   265,   266,   267,   268,   269,     0,     0,   175,   176,
       0,    15,   160,    16,    17,     0,   161,   162,   163,   164,
     165,     0,     0,     0,     0,   166,     0,   156,     0,     0,
       0,   167,   157,   158,   168,   159,     0,     0,     0,   169,
     170,   171,     0,     0,   172,   173,     0,     0,     0,   174,
       0,     0,     0,   160,     0,    17,     0,   161,   162,   163,
     164,   165,     0,     0,     0,     0,   166,     0,   156,   175,
     176,     0,   167,   157,   158,   168,   159,     0,     0,     0,
     169,   170,   171,     0,     0,   172,   173,     0,     0,     0,
     174,     0,     0,     0,   160,     0,    17,     0,   161,   162,
     163,   164,   165,     0,     0,     0,     0,   166,   697,   156,
     175,   176,     0,   167,   157,   158,   168,   159,     0,     0,
       0,   169,   170,   171,     0,     0,   172,   173,     0,     0,
       0,   174,     0,     0,     0,   160,     0,    17,     0,   161,
     162,   163,   164,   165,     0,     0,     0,     0,   166,     0,
     156,   175,   176,     0,   167,   157,   158,   168,   159,     0,
       0,     0,   169,   170,   171,     0,     0,   172,   173,     0,
       0,     0,   249,     0,     0,     0,   160,     0,    17,     0,
     161,   162,   163,   164,   165,     0,     0,     0,     0,   166,
       0,   466,   175,   176,     0,   167,   157,   158,   168,   159,
       0,     0,     0,   169,   170,   171,     0,     0,   172,   173,
       0,     0,     0,   251,     0,     0,     0,   160,     0,    17,
       0,   161,   162,   163,   164,   165,     0,     0,     0,     0,
     166,     0,     0,   175,   176,     0,   167,     0,     0,   168,
       0,     0,     0,   757,   169,   170,   171,     0,     0,   172,
     173,   253,   254,   255,   174,   256,   257,   258,   259,   260,
     261,   262,   263,   264,   265,   266,   267,   268,   269,     0,
       0,     0,     0,     0,   175,   176,   253,   254,   255,   758,
     256,   257,   258,   259,   260,   261,   262,   263,   264,   265,
     266,   267,   268,   269,     0,   253,   254,   255,   828,   256,
     257,   258,   259,   260,   261,   262,   263,   264,   265,   266,
     267,   268,   269,   253,   254,   255,     0,   256,   257,   258,
     259,   260,   261,   262,   263,   264,   265,   266,   267,   268,
     269,     0,     0,     0,     0,     0,     0,   253,   254,   255,
     435,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,     0,     0,     0,   436,     7,
     116,     9,   117,     7,     0,     9,    10,    11,    12,    13,
       0,    11,    12,    13,     0,     0,     0,     0,     0,     0,
       0,     0,   585,    15,     0,    16,     0,    15,     0,    16,
       7,   121,     9,   122,     7,     0,     9,    86,    11,    12,
      13,     0,    11,    12,    13,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    15,     0,     0,     0,    15,   253,
     254,   255,   829,   256,   257,   258,   259,   260,   261,   262,
     263,   264,   265,   266,   267,   268,   269,   253,   254,   255,
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   255,     0,   256,   257,   258,
     259,   260,   261,   262,   263,   264,   265,   266,   267,   268,
     269,   257,   258,   259,   260,   261,   262,   263,   264,   265,
     266,   267,   268,   269
};

static const short yycheck[] =
{
      62,     3,    64,     5,     3,     3,     5,     5,     3,   290,
       5,   144,    11,    12,    13,   223,   316,   219,   543,   314,
     400,   273,   322,   143,   318,    24,   296,    26,   468,    28,
      23,    30,   543,    32,   414,    34,   599,    36,   550,    38,
       3,     3,     5,     5,     3,   625,     5,     3,     3,     5,
       5,    40,    41,    55,   161,   162,    55,    55,   400,   400,
      55,     3,   543,     5,   171,    64,   151,    66,    66,   400,
     177,    66,   414,   414,     1,    48,   229,   602,   559,     1,
       1,   188,   556,   414,   772,   543,    59,    60,    61,    10,
     566,   306,    55,    55,     1,   468,    55,     1,     9,    55,
      55,   559,    75,    66,    66,   214,    46,     5,     1,    46,
     460,    84,     1,    55,   625,     4,     5,     6,     7,     1,
      93,    10,   174,    12,    13,    14,   689,     7,   816,   102,
       3,     4,    50,    51,    52,   387,     3,     4,   111,    28,
     665,   670,   543,    83,   625,   857,    83,   120,   498,     9,
      30,   144,   214,     3,     4,   684,   129,    55,    46,     0,
      30,   511,    89,    85,   514,   138,   381,   625,    89,     0,
     550,   859,    83,    66,   886,   174,    46,     0,    85,   559,
     868,    85,   555,   556,   337,   535,   388,   239,   469,   381,
     299,   754,   191,   300,   193,    83,   195,   249,   197,   251,
      89,   602,   201,    85,   203,    83,    27,    67,   550,   550,
      88,   899,   274,   206,   207,   214,    89,   559,   559,   550,
     219,   219,    89,   573,   219,   575,    30,   229,   559,   705,
     229,   229,    66,   813,   229,   208,   381,   299,    83,    89,
     239,   303,   457,    88,   351,    66,    67,    84,    30,   167,
     249,    88,   251,   338,    43,   340,   219,   219,    66,   733,
     734,   453,   454,   455,   456,     7,   229,   229,     3,     4,
     229,   189,    83,   229,   229,   787,    65,    88,    67,    66,
     720,   576,    58,   578,    66,    67,     3,   229,    30,   729,
      66,    67,    84,   545,     7,    66,   358,   505,    83,   406,
     299,   294,   813,    88,     4,     5,     6,     7,   453,   454,
     455,   456,    12,    13,    14,   233,    83,    30,   311,   312,
      67,    88,    84,    58,     3,     4,    88,   326,    28,   328,
      30,   330,   813,   332,    89,   337,     3,    89,   337,   337,
     402,    58,   337,   405,    89,   334,   335,   720,     3,    66,
      67,   876,   877,   703,   272,   813,   729,   275,    83,     1,
     733,   734,     4,     5,     6,     7,    58,    59,    60,     3,
      12,    13,    14,    84,   337,   337,    84,    88,   337,    58,
      88,   337,   337,   435,    84,   437,    28,    66,    67,   388,
     388,    58,   742,   388,    84,   337,    66,    84,    88,    66,
      67,    88,   400,    58,    84,   400,    84,   787,    88,    66,
      88,    66,    46,     5,     6,     7,   414,    66,    67,   414,
      12,    13,    14,    66,    58,   388,   388,   345,   630,   723,
      88,   725,    66,     9,    83,   568,   435,   639,   437,    84,
     721,   400,    83,    88,   400,   787,   787,    89,     1,   730,
      83,     4,     5,     6,     7,   414,   787,   577,   414,    12,
      13,    14,   380,     5,     6,     7,   213,     7,   386,   216,
      12,    13,    14,   220,    27,    28,     7,    30,   477,    66,
     479,   543,   481,     1,   483,     3,     4,    30,    30,   671,
      43,   673,    84,     5,     6,     7,   485,   486,    66,    67,
      12,    13,    14,    46,     3,     4,    85,   807,    46,   809,
      85,   500,   501,    83,   432,    83,   866,   867,   625,     4,
       5,     6,     7,    66,    67,    84,    85,    12,    13,    14,
      84,    84,    66,    67,    83,    88,    89,     3,     4,    83,
     602,   543,    84,    28,   543,   543,    83,   609,   543,    83,
       3,     4,   550,   453,   454,   550,   456,   559,   766,    58,
     559,   559,   555,   556,   559,    66,    67,    66,    66,    67,
     317,   318,    84,   706,    32,   568,   323,    84,    85,    83,
     543,   543,    66,    67,   543,   705,   579,   543,   543,   224,
      66,   550,   339,    46,   550,     9,   559,   559,    90,    84,
     559,   543,    84,   559,   559,    58,    24,    25,    26,    27,
     528,    43,   882,    66,    32,    33,    34,    35,     3,     4,
       5,     6,     7,   625,   894,   543,   625,   625,   617,   618,
     625,   630,   630,    27,    84,   630,    30,   699,    43,   638,
     639,   639,   389,    88,   639,    88,   645,    83,   647,    43,
     649,    46,   651,    83,   653,   290,   655,    83,   657,    90,
     659,    83,   625,   625,    66,    67,   625,   630,   630,   625,
     625,   589,   661,   662,   794,    90,   639,   639,    24,    25,
      26,    27,    27,   625,   602,    30,    32,    33,    34,    35,
      84,    90,   610,    43,    88,     5,     6,     7,    43,    85,
     762,    85,    12,    13,    14,    85,    28,    29,    30,    31,
      66,    67,   774,   706,    36,    37,    38,    39,   780,    89,
      30,    66,    67,    46,   471,     6,     7,   720,   721,   181,
     182,    12,    13,    14,   311,   312,   729,   730,     1,    84,
     733,   734,    90,    88,    85,     5,     6,     7,   810,   811,
      83,   813,    12,    13,    14,   728,     5,     6,     7,   394,
     720,   721,    83,    12,    13,    14,    83,    85,   515,   516,
      30,    90,   519,   191,   192,   193,   194,     5,     6,     7,
      84,   416,   417,    84,    12,    13,    14,    27,     4,   787,
       6,     7,   787,    88,    88,   857,    12,    13,    14,   861,
      83,     5,     6,     7,    83,    83,     3,    46,    12,    13,
      14,   813,    28,    46,   813,   813,    61,    62,   813,    85,
      65,    66,    67,    68,   886,   887,    30,    88,   787,    84,
      84,   787,    46,   468,   469,    84,   583,    54,    55,    56,
      57,    58,    59,    60,    46,   191,   192,   193,   194,     7,
     813,   813,    84,    84,   813,    85,     1,   813,   813,     4,
       5,     6,     7,    89,    84,    10,    84,    12,    13,    14,
      66,   813,    66,   195,   196,   197,   198,    16,    85,    85,
      84,    84,    46,    28,   631,   632,    66,    66,   635,    66,
       5,     6,     7,   640,   641,    17,   643,    12,    13,    14,
      83,    83,    88,    84,    28,    29,    30,    31,   326,   327,
     328,   329,    36,    37,    38,    39,    83,    83,    66,    84,
     555,   556,    84,    84,   237,   238,    83,    90,    84,   676,
     848,     1,   679,     9,     4,   248,     6,     7,    83,    66,
     253,   254,    12,    13,    14,   258,   259,   260,   261,   262,
     263,   264,   265,   266,   267,   268,   269,    83,    28,    84,
      30,    31,     1,    83,     3,     4,     5,     6,     7,    84,
      84,   387,   307,    12,    13,    14,   723,   559,   725,   559,
     326,   327,   328,   329,   418,   559,   559,   414,    27,    28,
     414,    30,    31,   386,   294,   598,   743,   733,   633,   634,
     747,    15,   574,    17,    18,    19,    20,   337,   330,   331,
     332,   333,   687,   720,    84,    85,   677,   434,   674,    58,
     345,   432,     4,     5,     6,     7,     4,    66,     6,     7,
      12,    13,    14,   817,    12,    13,    14,   621,   381,   819,
     382,   788,   789,   863,   791,    84,    28,   360,   361,   796,
      28,   798,    30,    56,    57,    58,    59,    60,   805,   477,
     478,   471,   865,   481,   482,   486,    -1,   410,     5,     6,
       7,   195,   196,   197,   198,    12,    13,    14,    -1,    -1,
      -1,   424,   425,     6,     7,   720,   721,    -1,    -1,    12,
      13,    14,    -1,    30,   729,   730,    -1,    -1,   733,   734,
      -1,    -1,    84,    -1,    -1,    -1,    -1,    30,     6,     7,
     453,   454,   455,   456,    12,    13,    14,    -1,   431,     4,
       5,     6,     7,   436,    -1,    -1,    -1,    12,    13,    14,
     443,   477,   478,    -1,    -1,   481,   482,    -1,    -1,    -1,
      -1,    -1,    -1,    28,    -1,    30,    -1,   782,   783,   784,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   479,   480,    -1,
      -1,   483,   484,   506,    -1,   508,    -1,     4,     1,     6,
       7,     4,     5,     6,     7,    12,    13,    14,    -1,    12,
      13,    14,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   502,
      -1,    28,    -1,    30,    27,    28,    -1,    30,    -1,    84,
      -1,    -1,    -1,    -1,    -1,    -1,   330,   331,   332,   333,
      43,   524,     1,    -1,     3,     4,     5,     6,     7,    -1,
      -1,    10,    -1,    12,    13,    14,    -1,   645,   646,   647,
     648,    -1,    -1,    66,    67,   653,   654,   655,   656,    28,
       4,     5,     6,     7,     4,     5,     6,     7,    12,    13,
      14,    84,    12,    13,    14,    88,    89,    -1,    -1,    -1,
      -1,   574,    -1,    -1,    28,    -1,    30,    -1,    28,    -1,
      30,   614,   585,     4,     5,     6,     7,   590,    -1,    -1,
      -1,    12,    13,    14,    -1,    -1,   599,   600,    -1,    -1,
      -1,    -1,   605,    -1,    83,    -1,    -1,    28,    -1,   645,
     646,   647,   648,    -1,     5,     6,     7,   653,   654,   655,
     656,    12,    13,    14,     5,     6,     7,    -1,    -1,    -1,
      84,    12,    13,    14,    84,    -1,    -1,   649,   650,   651,
     652,   674,    -1,    -1,   677,   657,   658,   659,   660,    30,
       4,     5,     6,     7,    -1,    -1,    10,    -1,    12,    13,
      14,    -1,    -1,    84,    -1,   479,   480,   670,    -1,   483,
     484,    -1,   675,    -1,    28,   678,    30,    -1,    -1,    -1,
      -1,   684,    -1,    -1,   687,    -1,   689,     4,     5,     6,
       7,    -1,     5,     6,     7,    12,    13,    14,    -1,    12,
      13,    14,     1,    84,     3,     4,     5,     6,     7,     8,
       9,    28,    11,    12,    13,    14,    15,    30,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    -1,    33,    34,    35,    36,    37,    -1,
      -1,   744,    -1,    42,   777,   748,    -1,    -1,   751,    48,
      -1,   754,    51,    -1,   757,    -1,    -1,    56,    57,    58,
      10,    -1,    61,    62,    -1,    -1,    -1,    66,     4,     5,
       6,     7,     4,     5,     6,     7,    12,    13,    14,    -1,
      12,    13,    14,    -1,    -1,    84,   819,    86,    87,    -1,
      89,    -1,    28,    43,    44,    45,    28,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    58,    59,
      60,    -1,    -1,    -1,    -1,    -1,     5,     6,     7,    -1,
     853,   854,    -1,    12,    13,    14,    52,    53,    54,    55,
      56,    57,    58,    59,    60,   649,   650,   651,   652,    -1,
      90,    30,    -1,   657,   658,   659,   660,     1,    84,     3,
       4,     5,     6,     7,     8,     9,    -1,    11,    12,    13,
      14,    15,    -1,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,     5,     6,     7,    -1,    42,    -1,
      -1,    12,    13,    14,    48,    84,    -1,    51,    -1,    -1,
      -1,    -1,    56,    57,    58,    -1,    -1,    61,    62,    43,
      44,    45,    66,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    58,    59,    60,    -1,    -1,    -1,
      84,    85,    86,    87,     1,    89,     3,     4,    -1,    -1,
      -1,     8,     9,    -1,    11,    -1,    -1,    -1,    15,    83,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    -1,    29,    -1,    31,    -1,    33,    34,    35,    36,
      37,     5,     6,     7,    -1,    42,    -1,    -1,    12,    13,
      14,    48,    -1,    30,    51,    -1,    -1,    -1,    -1,    56,
      57,    58,    -1,    -1,    61,    62,    43,    44,    45,    66,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    -1,    -1,    -1,    84,    -1,    86,
      87,     1,    89,     3,     4,    -1,    -1,    -1,     8,     9,
      -1,    11,    -1,    -1,    -1,    15,    -1,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    -1,    29,
      -1,    31,    -1,    33,    34,    35,    36,    37,    -1,     6,
       7,    -1,    42,    -1,    -1,    12,    13,    14,    48,    -1,
      -1,    51,    -1,    -1,    -1,    -1,    56,    57,    58,    -1,
      -1,    61,    62,    30,    -1,     1,    66,     3,     4,    -1,
       6,     7,     8,     9,    -1,    11,    12,    13,    14,    -1,
      -1,    -1,    -1,    -1,    84,    -1,    86,    87,    -1,    89,
      -1,    -1,    28,    29,    30,    31,    -1,    33,    34,    35,
      36,    37,    -1,    -1,    -1,    -1,    42,    -1,    -1,    -1,
      -1,    -1,    48,    -1,    -1,    51,    -1,    -1,    -1,    -1,
      56,    57,    58,    -1,    -1,    61,    62,    -1,    -1,     1,
      66,     3,     4,    -1,    -1,    -1,     8,     9,    -1,    11,
      51,    52,    53,    54,    55,    56,    57,    58,    59,    60,
      86,    87,    -1,    89,    -1,    -1,    -1,    29,    -1,    31,
      -1,    33,    34,    35,    36,    37,    -1,    -1,    -1,    -1,
      42,    -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,    51,
      -1,    -1,    -1,    -1,    56,    57,    58,    -1,    -1,    61,
      62,    -1,    -1,    65,    66,    67,     1,    -1,     3,     4,
      -1,    -1,    -1,     8,     9,    -1,    11,    -1,    -1,    -1,
      -1,    -1,    -1,    85,    86,    87,    -1,    89,    -1,    -1,
      -1,    -1,    -1,    -1,    29,    -1,    31,    -1,    33,    34,
      35,    36,    37,    -1,    -1,    -1,    -1,    42,    -1,    -1,
      -1,    -1,    -1,    48,    -1,    -1,    51,    -1,    -1,    -1,
      -1,    56,    57,    58,    -1,    -1,    61,    62,    -1,    -1,
      65,    66,    67,     1,    -1,     3,    -1,    -1,    -1,    -1,
       8,     9,    -1,    11,    -1,    -1,    -1,    -1,    -1,    -1,
      85,    86,    87,    -1,    89,    -1,    -1,    -1,    -1,    -1,
      -1,    29,    -1,    31,    -1,    33,    34,    35,    36,    37,
      -1,    -1,    -1,    -1,    42,    43,    -1,    -1,    -1,    -1,
      48,    -1,    -1,    51,    -1,    -1,    -1,    -1,    56,    57,
      58,    -1,    -1,    61,    62,    -1,    -1,    65,    66,    67,
       1,    -1,     3,    -1,    -1,    -1,    -1,     8,     9,    -1,
      11,    -1,    -1,    -1,     4,     5,     6,     7,    86,    87,
      -1,    89,    12,    13,    14,    -1,    -1,    -1,    29,    -1,
      31,    -1,    33,    34,    35,    36,    37,    -1,    28,    -1,
      30,    42,    -1,     4,     5,     6,     7,    48,    -1,    -1,
      51,    12,    13,    14,    -1,    56,    57,    58,    -1,    -1,
      61,    62,     1,    -1,     3,    66,    -1,    28,    -1,     8,
       9,    -1,    11,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    58,    59,    60,    86,    87,    -1,    89,    -1,
      29,    -1,    31,    -1,    33,    34,    35,    36,    37,    -1,
      -1,    -1,     1,    42,    -1,     4,     5,     6,     7,    48,
      -1,    -1,    51,    12,    13,    14,    -1,    56,    57,    58,
      -1,    -1,    61,    62,    -1,    -1,    -1,    66,    27,    28,
       1,    30,    -1,     4,    -1,     6,     7,    -1,    -1,    -1,
      -1,    12,    13,    14,    43,    -1,    -1,    86,    87,    -1,
      89,    -1,    -1,    -1,    -1,    -1,    -1,    28,    -1,    30,
      31,    -1,    -1,    -1,    -1,    -1,    -1,    66,    67,     3,
       4,     5,     6,     7,     8,     9,    -1,    11,    12,    13,
      14,    -1,    -1,    -1,    -1,    84,    -1,    -1,    -1,    88,
      89,    -1,    -1,    -1,    28,    29,    30,    31,    -1,    33,
      34,    35,    36,    37,    -1,    -1,    -1,    -1,    42,    -1,
       3,    -1,     5,    -1,    48,     8,     9,    51,    11,    -1,
      -1,    -1,    56,    57,    58,    -1,    -1,    61,    62,    -1,
      -1,    -1,    66,    -1,    -1,    -1,    29,    -1,    31,    -1,
      33,    34,    35,    36,    37,    -1,    -1,    -1,    -1,    42,
      -1,    -1,    86,    87,    -1,    48,    90,    -1,    51,    -1,
      -1,    -1,    -1,    56,    57,    58,    -1,    -1,    61,    62,
       3,    -1,    -1,    66,    -1,     8,     9,    -1,    11,    -1,
      -1,     4,     5,     6,     7,    -1,    -1,    -1,    -1,    12,
      13,    14,    -1,    86,    87,    -1,    29,    90,    31,    -1,
      33,    34,    35,    36,    37,    28,    -1,    30,    -1,    42,
      -1,     3,    -1,    -1,    -1,    48,     8,     9,    51,    11,
      -1,    -1,    -1,    56,    57,    58,    -1,    -1,    61,    62,
      -1,    -1,    -1,    66,    -1,    -1,    -1,    29,    -1,    31,
      -1,    33,    34,    35,    36,    37,    -1,    -1,    -1,    -1,
      42,    -1,    -1,    86,    87,    -1,    48,    90,    -1,    51,
      -1,    -1,    -1,    -1,    56,    57,    58,    -1,    -1,    61,
      62,    -1,    -1,    -1,    66,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    -1,    11,
      12,    13,    14,    -1,    86,    87,    -1,    -1,    90,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      -1,    33,    34,    35,    36,    37,    -1,    -1,    -1,    -1,
      42,    -1,     4,     5,     6,     7,    48,    -1,    -1,    51,
      12,    13,    14,    -1,    56,    57,    58,    -1,    -1,    61,
      62,    -1,    -1,    -1,    66,    -1,    28,    -1,    30,    -1,
       3,     4,    -1,    -1,    -1,     8,     9,    -1,    11,    -1,
      -1,    -1,    84,    -1,    86,    87,    -1,    89,    21,    22,
      23,    24,    25,    26,    27,    -1,    29,    -1,    31,    -1,
      33,    34,    35,    36,    37,    -1,    -1,    -1,    -1,    42,
      -1,     4,     5,     6,     7,    48,    -1,    -1,    51,    12,
      13,    14,    -1,    56,    57,    58,    -1,    -1,    61,    62,
      -1,     3,    -1,    66,    -1,    28,     8,     9,    -1,    11,
      50,    51,    52,    53,    54,    55,    56,    57,    58,    59,
      60,    84,    -1,    86,    87,    -1,    89,    29,    -1,    31,
      -1,    33,    34,    35,    36,    37,    -1,    -1,    -1,    -1,
      42,    -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,    51,
      -1,    -1,    -1,    -1,    56,    57,    58,    -1,    -1,    61,
      62,    -1,    -1,    -1,    66,     3,     4,     5,     6,     7,
       8,     9,    -1,    11,    12,    13,    14,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    86,    87,    -1,    89,    -1,    -1,
      28,    29,    30,    31,    -1,    33,    34,    35,    36,    37,
      -1,    -1,    -1,    -1,    42,    -1,    -1,    -1,    -1,    -1,
      48,    -1,    -1,    51,    -1,    -1,    -1,    -1,    56,    57,
      58,    -1,    -1,    61,    62,    -1,     3,     4,    66,     6,
       7,     8,     9,    -1,    11,    12,    13,    14,    53,    54,
      55,    56,    57,    58,    59,    60,    -1,    -1,    86,    87,
      -1,    28,    29,    30,    31,    -1,    33,    34,    35,    36,
      37,    -1,    -1,    -1,    -1,    42,    -1,     3,    -1,    -1,
      -1,    48,     8,     9,    51,    11,    -1,    -1,    -1,    56,
      57,    58,    -1,    -1,    61,    62,    -1,    -1,    -1,    66,
      -1,    -1,    -1,    29,    -1,    31,    -1,    33,    34,    35,
      36,    37,    -1,    -1,    -1,    -1,    42,    -1,     3,    86,
      87,    -1,    48,     8,     9,    51,    11,    -1,    -1,    -1,
      56,    57,    58,    -1,    -1,    61,    62,    -1,    -1,    -1,
      66,    -1,    -1,    -1,    29,    -1,    31,    -1,    33,    34,
      35,    36,    37,    -1,    -1,    -1,    -1,    42,    84,     3,
      86,    87,    -1,    48,     8,     9,    51,    11,    -1,    -1,
      -1,    56,    57,    58,    -1,    -1,    61,    62,    -1,    -1,
      -1,    66,    -1,    -1,    -1,    29,    -1,    31,    -1,    33,
      34,    35,    36,    37,    -1,    -1,    -1,    -1,    42,    -1,
       3,    86,    87,    -1,    48,     8,     9,    51,    11,    -1,
      -1,    -1,    56,    57,    58,    -1,    -1,    61,    62,    -1,
      -1,    -1,    66,    -1,    -1,    -1,    29,    -1,    31,    -1,
      33,    34,    35,    36,    37,    -1,    -1,    -1,    -1,    42,
      -1,     3,    86,    87,    -1,    48,     8,     9,    51,    11,
      -1,    -1,    -1,    56,    57,    58,    -1,    -1,    61,    62,
      -1,    -1,    -1,    66,    -1,    -1,    -1,    29,    -1,    31,
      -1,    33,    34,    35,    36,    37,    -1,    -1,    -1,    -1,
      42,    -1,    -1,    86,    87,    -1,    48,    -1,    -1,    51,
      -1,    -1,    -1,    10,    56,    57,    58,    -1,    -1,    61,
      62,    43,    44,    45,    66,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    58,    59,    60,    -1,
      -1,    -1,    -1,    -1,    86,    87,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    -1,    43,    44,    45,    90,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      58,    59,    60,    43,    44,    45,    -1,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    58,    59,
      60,    -1,    -1,    -1,    -1,    -1,    -1,    43,    44,    45,
      88,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    58,    59,    60,    -1,    -1,    -1,    88,     4,
       5,     6,     7,     4,    -1,     6,     7,    12,    13,    14,
      -1,    12,    13,    14,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    88,    28,    -1,    30,    -1,    28,    -1,    30,
       4,     5,     6,     7,     4,    -1,     6,     7,    12,    13,
      14,    -1,    12,    13,    14,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    28,    -1,    -1,    -1,    28,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    58,    59,    60,    43,    44,    45,
      -1,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    58,    59,    60,    45,    -1,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    58,    59,
      60,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    58,    59,    60
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const unsigned short yystos[] =
{
       0,    92,    93,    94,     0,    95,     1,     4,     5,     6,
       7,    12,    13,    14,    27,    28,    30,    31,    84,    96,
      97,    98,   113,   129,   132,   133,   134,   135,   136,   137,
     138,   139,   140,   141,   142,   143,   144,   145,   146,   147,
     148,   149,   155,   157,   158,   159,   160,   161,   170,   171,
     197,   198,   199,   200,   205,   290,    96,    84,    85,   170,
     170,   170,    66,    66,    66,     3,    58,    66,   163,   167,
     196,     5,     7,   157,   158,   170,     5,     7,   157,   158,
       5,     7,   157,   158,   170,     5,     7,   157,   158,     5,
       7,   159,   160,   170,     5,     7,   159,   160,     5,     7,
     159,   160,   170,     5,     7,   159,   160,     5,     7,   157,
     158,   170,     5,     7,   157,   158,     5,     7,   157,   158,
     170,     5,     7,   157,   158,     5,     7,   159,   160,   170,
       5,     7,   159,   160,     5,     7,   159,   160,   170,     5,
       7,   159,   160,   129,   129,    84,   171,     3,     4,    89,
     105,    89,   105,    89,   105,    96,     3,     8,     9,    11,
      29,    33,    34,    35,    36,    37,    42,    48,    51,    56,
      57,    58,    61,    62,    66,    86,    87,   106,   107,   109,
     110,   111,   112,   114,   115,   121,   123,   244,   290,    66,
     107,   132,   133,   134,   135,   136,   137,   138,   139,   154,
     217,   132,   133,   134,   135,   153,   156,   169,   170,    84,
      88,     1,    27,    66,    67,   103,   164,   225,     4,    58,
      66,   162,   165,   191,   192,   196,   163,   196,   208,   209,
      89,   208,    89,   204,    89,   114,   114,    66,    66,    66,
     105,   114,     1,    89,   107,   217,   114,    83,    88,    66,
     110,    66,   110,    43,    44,    45,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    58,    59,    60,
      61,    62,    65,    66,    67,    68,     9,     1,    85,   233,
     242,   114,     5,     6,     7,   105,   172,   173,   174,    83,
     218,    83,   196,   196,   130,   169,    66,   169,   285,     5,
      58,    90,   107,   154,     1,   124,   125,   126,   232,   251,
     169,   156,   169,    84,    88,     1,    99,   164,    66,   225,
      84,     1,   101,    85,     1,    84,   132,   133,   134,   135,
     136,   137,   138,   139,   152,   153,   210,   290,   201,    85,
     202,     1,   105,   215,   216,   203,   115,   115,   217,    83,
      83,    83,    84,   115,   217,   217,   115,   115,   118,   120,
     117,   116,   115,   115,   115,   115,   115,   115,   115,   115,
     115,   115,   115,   115,   105,   108,   109,   107,   105,    83,
      32,   237,   238,   239,    83,    83,    88,    66,    58,    66,
     219,   221,   222,   223,   224,   225,    83,   167,   196,   123,
     286,   107,   154,    90,    90,     5,    58,    90,   107,    84,
     104,   232,    10,   125,   252,    43,   192,   192,   130,   124,
     169,   285,   124,   169,   129,   129,    84,   210,   208,   169,
     208,    43,    88,   207,   215,    88,    88,    88,    89,   114,
      83,    83,   107,    46,   115,   115,    83,    90,   105,   289,
       1,   128,   226,   227,   228,   229,   230,   231,   232,   243,
     251,   255,   256,   239,    83,   173,     3,   108,   156,   169,
     275,    66,   225,    83,     1,     3,    10,   132,   133,   136,
     137,   140,   141,   144,   145,   150,   151,   277,   280,   281,
     283,   287,   288,    90,   107,   107,    90,    90,   251,   127,
     150,   151,   168,    83,   165,   191,   100,    43,   102,   211,
     213,   251,   212,   214,   251,    85,    85,   115,   216,    85,
     207,   217,   115,   217,   122,    46,   115,    84,    88,   128,
     255,   256,   128,   255,   256,   251,   255,   256,   128,   255,
     256,   232,    85,   252,    83,    88,    83,   222,   223,   221,
     276,   169,   275,    83,   129,   284,   284,    83,    84,    88,
      83,    88,    90,    90,   252,    84,   129,    84,   129,     1,
      89,   115,   175,   251,   166,   251,    88,   252,    88,   252,
     169,   169,   169,    85,    83,    88,    83,     1,     3,    65,
      67,    89,   105,   115,   177,   178,   179,   181,   183,   184,
     119,   105,   252,   234,     4,    21,    22,    23,    24,    25,
      26,    27,    84,    89,   105,   107,   131,   148,   149,   155,
     235,   241,   245,   267,   268,   290,   109,    83,   277,     4,
      58,    66,   193,   194,   195,   196,   220,   221,   222,    58,
      66,   196,   220,   278,    10,   132,   133,   134,   135,   136,
     137,   138,   139,   140,   141,   142,   143,   144,   145,   146,
     147,   148,   149,   282,     3,     1,   240,   245,   162,   163,
     176,   252,   175,   252,   130,    46,   191,   130,    46,   196,
     169,   115,   105,   115,   182,    46,    85,    88,   206,    43,
     184,   181,   115,   115,    46,    84,    84,    84,   107,    58,
     105,     7,   269,   251,    84,   129,   129,    84,    15,    17,
      18,    19,    20,   246,   247,   249,   257,   242,   131,    83,
     156,   169,   169,    66,   225,    66,   225,   169,   170,   156,
     169,   169,   169,   129,   129,   245,    84,    84,   177,   240,
     240,   213,   251,   115,    46,   169,   214,   115,    46,   169,
      83,    10,    90,   177,   180,   179,   181,    10,    46,    84,
     107,    84,    66,   252,   162,   185,   191,   163,   188,   196,
     248,   259,   250,   261,    66,    16,     1,   235,   254,     1,
      66,   236,   194,   195,   195,   285,   285,   279,   193,   196,
     220,   196,   220,    85,   252,   169,   115,   169,   115,   115,
      85,   181,   115,    84,   107,    46,    84,   186,    84,   189,
      66,    66,   254,    66,   107,     1,   258,   253,   255,   256,
     107,    83,   277,   169,   169,   169,   169,   169,    90,    46,
      46,    83,   169,   124,   124,   107,   107,    17,   107,   131,
     266,   270,    83,   254,   236,   253,    83,     9,    67,   271,
     272,   273,    84,   187,   190,    83,    83,   262,    84,   265,
      84,    66,   105,    46,    83,    88,   251,   251,   260,   270,
     254,   107,    90,   271,    84,   273,   252,   252,   254,    84,
      83,     9,    46,    83,   245,   245,   263,    66,   123,   274,
      84,   270,   107,    83,    88,    83,    83,    84,   123,   264,
     254
};

#if ! defined (YYSIZE_T) && defined (__SIZE_TYPE__)
# define YYSIZE_T __SIZE_TYPE__
#endif
#if ! defined (YYSIZE_T) && defined (size_t)
# define YYSIZE_T size_t
#endif
#if ! defined (YYSIZE_T)
# if defined (__STDC__) || defined (__cplusplus)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# endif
#endif
#if ! defined (YYSIZE_T)
# define YYSIZE_T unsigned int
#endif

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrlab1

/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { 								\
      yyerror ("syntax error: cannot back up");\
      YYERROR;							\
    }								\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

/* YYLLOC_DEFAULT -- Compute the default location (before the actions
   are run).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)         \
  Current.first_line   = Rhs[1].first_line;      \
  Current.first_column = Rhs[1].first_column;    \
  Current.last_line    = Rhs[N].last_line;       \
  Current.last_column  = Rhs[N].last_column;
#endif

/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (0)

# define YYDSYMPRINT(Args)			\
do {						\
  if (yydebug)					\
    yysymprint Args;				\
} while (0)

# define YYDSYMPRINTF(Title, Token, Value, Location)		\
do {								\
  if (yydebug)							\
    {								\
      YYFPRINTF (stderr, "%s ", Title);				\
      yysymprint (stderr, 					\
                  Token, Value);	\
      YYFPRINTF (stderr, "\n");					\
    }								\
} while (0)

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (cinluded).                                                   |
`------------------------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_stack_print (short *bottom, short *top)
#else
static void
yy_stack_print (bottom, top)
    short *bottom;
    short *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (/* Nothing. */; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_reduce_print (int yyrule)
#else
static void
yy_reduce_print (yyrule)
    int yyrule;
#endif
{
  int yyi;
  unsigned int yylineno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %u), ",
             yyrule - 1, yylineno);
  /* Print the symbols being reduced, and their result.  */
  for (yyi = yyprhs[yyrule]; 0 <= yyrhs[yyi]; yyi++)
    YYFPRINTF (stderr, "%s ", yytname [yyrhs[yyi]]);
  YYFPRINTF (stderr, "-> %s\n", yytname [yyr1[yyrule]]);
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (Rule);		\
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YYDSYMPRINT(Args)
# define YYDSYMPRINTF(Title, Token, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   SIZE_MAX < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#if YYMAXDEPTH == 0
# undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined (__GLIBC__) && defined (_STRING_H)
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
#   if defined (__STDC__) || defined (__cplusplus)
yystrlen (const char *yystr)
#   else
yystrlen (yystr)
     const char *yystr;
#   endif
{
  register const char *yys = yystr;

  while (*yys++ != '\0')
    continue;

  return yys - yystr - 1;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined (__GLIBC__) && defined (_STRING_H) && defined (_GNU_SOURCE)
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
#   if defined (__STDC__) || defined (__cplusplus)
yystpcpy (char *yydest, const char *yysrc)
#   else
yystpcpy (yydest, yysrc)
     char *yydest;
     const char *yysrc;
#   endif
{
  register char *yyd = yydest;
  register const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

#endif /* !YYERROR_VERBOSE */



#if YYDEBUG
/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yysymprint (FILE *yyoutput, int yytype, YYSTYPE *yyvaluep)
#else
static void
yysymprint (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  if (yytype < YYNTOKENS)
    {
      YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
# ifdef YYPRINT
      YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
    }
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  switch (yytype)
    {
      default:
        break;
    }
  YYFPRINTF (yyoutput, ")");
}

#endif /* ! YYDEBUG */
/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yydestruct (int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yytype, yyvaluep)
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  switch (yytype)
    {

      default:
        break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM);
# else
int yyparse ();
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM)
# else
int yyparse (YYPARSE_PARAM)
  void *YYPARSE_PARAM;
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  register int yystate;
  register int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  short	yyssa[YYINITDEPTH];
  short *yyss = yyssa;
  register short *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  register YYSTYPE *yyvsp;



#define YYPOPSTACK   (yyvsp--, yyssp--)

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* When reducing, the number of symbols on the RHS of the reduced
     rule.  */
  int yylen;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed. so pushing a state here evens the stacks.
     */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack. Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	short *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow ("parser stack overflow",
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyoverflowlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyoverflowlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	short *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyoverflowlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YYDSYMPRINTF ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */
  YYDPRINTF ((stderr, "Shifting token %s, ", yytname[yytoken]));

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;


  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  yystate = yyn;
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
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 287 "c-parse.y"
    { if (pedantic)
		    pedwarn ("ISO C forbids an empty source file");
		  finish_file ();
		;}
    break;

  case 3:
#line 292 "c-parse.y"
    {
		  /* In case there were missing closebraces,
		     get us back to the global binding level.  */
		  while (! global_bindings_p ())
		    poplevel (0, 0, 0);
		  finish_fname_decls ();
                  finish_file ();
		;}
    break;

  case 4:
#line 307 "c-parse.y"
    {yyval.ttype = NULL_TREE; ;}
    break;

  case 6:
#line 308 "c-parse.y"
    {yyval.ttype = NULL_TREE; ggc_collect(); ;}
    break;

  case 10:
#line 315 "c-parse.y"
    { STRIP_NOPS (yyvsp[-2].ttype);
		  if ((TREE_CODE (yyvsp[-2].ttype) == ADDR_EXPR
		       && TREE_CODE (TREE_OPERAND (yyvsp[-2].ttype, 0)) == STRING_CST)
		      || TREE_CODE (yyvsp[-2].ttype) == STRING_CST)
		    assemble_asm (yyvsp[-2].ttype);
		  else
		    error ("argument of `asm' is not a constant string"); ;}
    break;

  case 11:
#line 323 "c-parse.y"
    { RESTORE_WARN_FLAGS (yyvsp[-1].ttype); ;}
    break;

  case 12:
#line 328 "c-parse.y"
    { if (pedantic)
		    error ("ISO C forbids data definition with no type or storage class");
		  else if (!flag_traditional)
		    warning ("data definition has no type or storage class"); 

		  POP_DECLSPEC_STACK; ;}
    break;

  case 13:
#line 335 "c-parse.y"
    { POP_DECLSPEC_STACK; ;}
    break;

  case 14:
#line 337 "c-parse.y"
    { POP_DECLSPEC_STACK; ;}
    break;

  case 15:
#line 339 "c-parse.y"
    { shadow_tag (yyvsp[-1].ttype); ;}
    break;

  case 18:
#line 343 "c-parse.y"
    { if (pedantic)
		    pedwarn ("ISO C does not allow extra `;' outside of a function"); ;}
    break;

  case 19:
#line 349 "c-parse.y"
    { if (! start_function (current_declspecs, yyvsp[0].ttype,
					all_prefix_attributes))
		    YYERROR1;
		;}
    break;

  case 20:
#line 354 "c-parse.y"
    { store_parm_decls (); ;}
    break;

  case 21:
#line 356 "c-parse.y"
    { DECL_SOURCE_FILE (current_function_decl) = yyvsp[-2].filename;
		  DECL_SOURCE_LINE (current_function_decl) = yyvsp[-1].lineno;
		  finish_function (0, 1); 
		  POP_DECLSPEC_STACK; ;}
    break;

  case 22:
#line 361 "c-parse.y"
    { POP_DECLSPEC_STACK; ;}
    break;

  case 23:
#line 363 "c-parse.y"
    { if (! start_function (current_declspecs, yyvsp[0].ttype,
					all_prefix_attributes))
		    YYERROR1;
		;}
    break;

  case 24:
#line 368 "c-parse.y"
    { store_parm_decls (); ;}
    break;

  case 25:
#line 370 "c-parse.y"
    { DECL_SOURCE_FILE (current_function_decl) = yyvsp[-2].filename;
		  DECL_SOURCE_LINE (current_function_decl) = yyvsp[-1].lineno;
		  finish_function (0, 1); 
		  POP_DECLSPEC_STACK; ;}
    break;

  case 26:
#line 375 "c-parse.y"
    { POP_DECLSPEC_STACK; ;}
    break;

  case 27:
#line 377 "c-parse.y"
    { if (! start_function (NULL_TREE, yyvsp[0].ttype,
					all_prefix_attributes))
		    YYERROR1;
		;}
    break;

  case 28:
#line 382 "c-parse.y"
    { store_parm_decls (); ;}
    break;

  case 29:
#line 384 "c-parse.y"
    { DECL_SOURCE_FILE (current_function_decl) = yyvsp[-2].filename;
		  DECL_SOURCE_LINE (current_function_decl) = yyvsp[-1].lineno;
		  finish_function (0, 1); 
		  POP_DECLSPEC_STACK; ;}
    break;

  case 30:
#line 389 "c-parse.y"
    { POP_DECLSPEC_STACK; ;}
    break;

  case 33:
#line 398 "c-parse.y"
    { yyval.code = ADDR_EXPR; ;}
    break;

  case 34:
#line 400 "c-parse.y"
    { yyval.code = NEGATE_EXPR; ;}
    break;

  case 35:
#line 402 "c-parse.y"
    { yyval.code = CONVERT_EXPR;
  if (warn_traditional && !in_system_header)
    warning ("traditional C rejects the unary plus operator");
		;}
    break;

  case 36:
#line 407 "c-parse.y"
    { yyval.code = PREINCREMENT_EXPR; ;}
    break;

  case 37:
#line 409 "c-parse.y"
    { yyval.code = PREDECREMENT_EXPR; ;}
    break;

  case 38:
#line 411 "c-parse.y"
    { yyval.code = BIT_NOT_EXPR; ;}
    break;

  case 39:
#line 413 "c-parse.y"
    { yyval.code = TRUTH_NOT_EXPR; ;}
    break;

  case 40:
#line 417 "c-parse.y"
    { yyval.ttype = build_compound_expr (yyvsp[0].ttype); ;}
    break;

  case 41:
#line 422 "c-parse.y"
    { yyval.ttype = NULL_TREE; ;}
    break;

  case 43:
#line 428 "c-parse.y"
    { yyval.ttype = build_tree_list (NULL_TREE, yyvsp[0].ttype); ;}
    break;

  case 44:
#line 430 "c-parse.y"
    { chainon (yyvsp[-2].ttype, build_tree_list (NULL_TREE, yyvsp[0].ttype)); ;}
    break;

  case 46:
#line 436 "c-parse.y"
    { yyval.ttype = build_indirect_ref (yyvsp[0].ttype, "unary *"); ;}
    break;

  case 47:
#line 439 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype;
		  RESTORE_WARN_FLAGS (yyvsp[-1].ttype); ;}
    break;

  case 48:
#line 442 "c-parse.y"
    { yyval.ttype = build_unary_op (yyvsp[-1].code, yyvsp[0].ttype, 0);
		  overflow_warning (yyval.ttype); ;}
    break;

  case 49:
#line 446 "c-parse.y"
    { yyval.ttype = finish_label_address_expr (yyvsp[0].ttype); ;}
    break;

  case 50:
#line 463 "c-parse.y"
    { skip_evaluation--;
		  if (TREE_CODE (yyvsp[0].ttype) == COMPONENT_REF
		      && DECL_C_BIT_FIELD (TREE_OPERAND (yyvsp[0].ttype, 1)))
		    error ("`sizeof' applied to a bit-field");
		  yyval.ttype = c_sizeof (TREE_TYPE (yyvsp[0].ttype)); ;}
    break;

  case 51:
#line 469 "c-parse.y"
    { skip_evaluation--;
		  yyval.ttype = c_sizeof (groktypename (yyvsp[-1].ttype)); ;}
    break;

  case 52:
#line 472 "c-parse.y"
    { skip_evaluation--;
		  yyval.ttype = c_alignof_expr (yyvsp[0].ttype); ;}
    break;

  case 53:
#line 475 "c-parse.y"
    { skip_evaluation--;
		  yyval.ttype = c_alignof (groktypename (yyvsp[-1].ttype)); ;}
    break;

  case 54:
#line 478 "c-parse.y"
    { yyval.ttype = build_unary_op (REALPART_EXPR, yyvsp[0].ttype, 0); ;}
    break;

  case 55:
#line 480 "c-parse.y"
    { yyval.ttype = build_unary_op (IMAGPART_EXPR, yyvsp[0].ttype, 0); ;}
    break;

  case 56:
#line 484 "c-parse.y"
    { skip_evaluation++; ;}
    break;

  case 57:
#line 488 "c-parse.y"
    { skip_evaluation++; ;}
    break;

  case 58:
#line 492 "c-parse.y"
    { skip_evaluation++; ;}
    break;

  case 60:
#line 498 "c-parse.y"
    { yyval.ttype = c_cast_expr (yyvsp[-2].ttype, yyvsp[0].ttype); ;}
    break;

  case 62:
#line 504 "c-parse.y"
    { yyval.ttype = parser_build_binary_op (yyvsp[-1].code, yyvsp[-2].ttype, yyvsp[0].ttype); ;}
    break;

  case 63:
#line 506 "c-parse.y"
    { yyval.ttype = parser_build_binary_op (yyvsp[-1].code, yyvsp[-2].ttype, yyvsp[0].ttype); ;}
    break;

  case 64:
#line 508 "c-parse.y"
    { yyval.ttype = parser_build_binary_op (yyvsp[-1].code, yyvsp[-2].ttype, yyvsp[0].ttype); ;}
    break;

  case 65:
#line 510 "c-parse.y"
    { yyval.ttype = parser_build_binary_op (yyvsp[-1].code, yyvsp[-2].ttype, yyvsp[0].ttype); ;}
    break;

  case 66:
#line 512 "c-parse.y"
    { yyval.ttype = parser_build_binary_op (yyvsp[-1].code, yyvsp[-2].ttype, yyvsp[0].ttype); ;}
    break;

  case 67:
#line 514 "c-parse.y"
    { yyval.ttype = parser_build_binary_op (yyvsp[-1].code, yyvsp[-2].ttype, yyvsp[0].ttype); ;}
    break;

  case 68:
#line 516 "c-parse.y"
    { yyval.ttype = parser_build_binary_op (yyvsp[-1].code, yyvsp[-2].ttype, yyvsp[0].ttype); ;}
    break;

  case 69:
#line 518 "c-parse.y"
    { yyval.ttype = parser_build_binary_op (yyvsp[-1].code, yyvsp[-2].ttype, yyvsp[0].ttype); ;}
    break;

  case 70:
#line 520 "c-parse.y"
    { yyval.ttype = parser_build_binary_op (yyvsp[-1].code, yyvsp[-2].ttype, yyvsp[0].ttype); ;}
    break;

  case 71:
#line 522 "c-parse.y"
    { yyval.ttype = parser_build_binary_op (yyvsp[-1].code, yyvsp[-2].ttype, yyvsp[0].ttype); ;}
    break;

  case 72:
#line 524 "c-parse.y"
    { yyval.ttype = parser_build_binary_op (yyvsp[-1].code, yyvsp[-2].ttype, yyvsp[0].ttype); ;}
    break;

  case 73:
#line 526 "c-parse.y"
    { yyval.ttype = parser_build_binary_op (yyvsp[-1].code, yyvsp[-2].ttype, yyvsp[0].ttype); ;}
    break;

  case 74:
#line 528 "c-parse.y"
    { yyvsp[-1].ttype = truthvalue_conversion (default_conversion (yyvsp[-1].ttype));
		  skip_evaluation += yyvsp[-1].ttype == boolean_false_node; ;}
    break;

  case 75:
#line 531 "c-parse.y"
    { skip_evaluation -= yyvsp[-3].ttype == boolean_false_node;
		  yyval.ttype = parser_build_binary_op (TRUTH_ANDIF_EXPR, yyvsp[-3].ttype, yyvsp[0].ttype); ;}
    break;

  case 76:
#line 534 "c-parse.y"
    { yyvsp[-1].ttype = truthvalue_conversion (default_conversion (yyvsp[-1].ttype));
		  skip_evaluation += yyvsp[-1].ttype == boolean_true_node; ;}
    break;

  case 77:
#line 537 "c-parse.y"
    { skip_evaluation -= yyvsp[-3].ttype == boolean_true_node;
		  yyval.ttype = parser_build_binary_op (TRUTH_ORIF_EXPR, yyvsp[-3].ttype, yyvsp[0].ttype); ;}
    break;

  case 78:
#line 540 "c-parse.y"
    { yyvsp[-1].ttype = truthvalue_conversion (default_conversion (yyvsp[-1].ttype));
		  skip_evaluation += yyvsp[-1].ttype == boolean_false_node; ;}
    break;

  case 79:
#line 543 "c-parse.y"
    { skip_evaluation += ((yyvsp[-4].ttype == boolean_true_node)
				      - (yyvsp[-4].ttype == boolean_false_node)); ;}
    break;

  case 80:
#line 546 "c-parse.y"
    { skip_evaluation -= yyvsp[-6].ttype == boolean_true_node;
		  yyval.ttype = build_conditional_expr (yyvsp[-6].ttype, yyvsp[-3].ttype, yyvsp[0].ttype); ;}
    break;

  case 81:
#line 549 "c-parse.y"
    { if (pedantic)
		    pedwarn ("ISO C forbids omitting the middle term of a ?: expression");
		  /* Make sure first operand is calculated only once.  */
		  yyvsp[0].ttype = save_expr (yyvsp[-1].ttype);
		  yyvsp[-1].ttype = truthvalue_conversion (default_conversion (yyvsp[0].ttype));
		  skip_evaluation += yyvsp[-1].ttype == boolean_true_node; ;}
    break;

  case 82:
#line 556 "c-parse.y"
    { skip_evaluation -= yyvsp[-4].ttype == boolean_true_node;
		  yyval.ttype = build_conditional_expr (yyvsp[-4].ttype, yyvsp[-3].ttype, yyvsp[0].ttype); ;}
    break;

  case 83:
#line 559 "c-parse.y"
    { char class;
		  yyval.ttype = build_modify_expr (yyvsp[-2].ttype, NOP_EXPR, yyvsp[0].ttype);
		  class = TREE_CODE_CLASS (TREE_CODE (yyval.ttype));
		  if (IS_EXPR_CODE_CLASS (class))
		    C_SET_EXP_ORIGINAL_CODE (yyval.ttype, MODIFY_EXPR);
		;}
    break;

  case 84:
#line 566 "c-parse.y"
    { char class;
		  yyval.ttype = build_modify_expr (yyvsp[-2].ttype, yyvsp[-1].code, yyvsp[0].ttype);
		  /* This inhibits warnings in truthvalue_conversion.  */
		  class = TREE_CODE_CLASS (TREE_CODE (yyval.ttype));
		  if (IS_EXPR_CODE_CLASS (class))
		    C_SET_EXP_ORIGINAL_CODE (yyval.ttype, ERROR_MARK);
		;}
    break;

  case 85:
#line 577 "c-parse.y"
    {
		  if (yychar == YYEMPTY)
		    yychar = YYLEX;
		  yyval.ttype = build_external_ref (yyvsp[0].ttype, yychar == '(');
		;}
    break;

  case 87:
#line 584 "c-parse.y"
    { yyval.ttype = combine_strings (yyvsp[0].ttype); ;}
    break;

  case 88:
#line 586 "c-parse.y"
    { yyval.ttype = fname_decl (C_RID_CODE (yyval.ttype), yyval.ttype); ;}
    break;

  case 89:
#line 588 "c-parse.y"
    { start_init (NULL_TREE, NULL, 0);
		  yyvsp[-2].ttype = groktypename (yyvsp[-2].ttype);
		  really_start_incremental_init (yyvsp[-2].ttype); ;}
    break;

  case 90:
#line 592 "c-parse.y"
    { tree constructor = pop_init_level (0);
		  tree type = yyvsp[-5].ttype;
		  finish_init ();

		  if (pedantic && ! flag_isoc99)
		    pedwarn ("ISO C89 forbids compound literals");
		  yyval.ttype = build_compound_literal (type, constructor);
		;}
    break;

  case 91:
#line 601 "c-parse.y"
    { char class = TREE_CODE_CLASS (TREE_CODE (yyvsp[-1].ttype));
		  if (IS_EXPR_CODE_CLASS (class))
		    C_SET_EXP_ORIGINAL_CODE (yyvsp[-1].ttype, ERROR_MARK);
		  yyval.ttype = yyvsp[-1].ttype; ;}
    break;

  case 92:
#line 606 "c-parse.y"
    { yyval.ttype = error_mark_node; ;}
    break;

  case 93:
#line 608 "c-parse.y"
    { tree saved_last_tree;

		   if (pedantic)
		     pedwarn ("ISO C forbids braced-groups within expressions");
		  pop_label_level ();

		  saved_last_tree = COMPOUND_BODY (yyvsp[-2].ttype);
		  RECHAIN_STMTS (yyvsp[-2].ttype, COMPOUND_BODY (yyvsp[-2].ttype));
		  last_tree = saved_last_tree;
		  TREE_CHAIN (last_tree) = NULL_TREE;
		  if (!last_expr_type)
		    last_expr_type = void_type_node;
		  yyval.ttype = build1 (STMT_EXPR, last_expr_type, yyvsp[-2].ttype);
		  TREE_SIDE_EFFECTS (yyval.ttype) = 1;
		;}
    break;

  case 94:
#line 624 "c-parse.y"
    {
		  pop_label_level ();
		  last_tree = COMPOUND_BODY (yyvsp[-2].ttype);
		  TREE_CHAIN (last_tree) = NULL_TREE;
		  yyval.ttype = error_mark_node;
		;}
    break;

  case 95:
#line 631 "c-parse.y"
    { yyval.ttype = build_function_call (yyvsp[-3].ttype, yyvsp[-1].ttype); ;}
    break;

  case 96:
#line 633 "c-parse.y"
    { yyval.ttype = build_va_arg (yyvsp[-3].ttype, groktypename (yyvsp[-1].ttype)); ;}
    break;

  case 97:
#line 636 "c-parse.y"
    {
                  tree c;

                  c = fold (yyvsp[-5].ttype);
                  STRIP_NOPS (c);
                  if (TREE_CODE (c) != INTEGER_CST)
                    error ("first argument to __builtin_choose_expr not a constant");
                  yyval.ttype = integer_zerop (c) ? yyvsp[-1].ttype : yyvsp[-3].ttype;
		;}
    break;

  case 98:
#line 646 "c-parse.y"
    {
		  tree e1, e2;

		  e1 = TYPE_MAIN_VARIANT (groktypename (yyvsp[-3].ttype));
		  e2 = TYPE_MAIN_VARIANT (groktypename (yyvsp[-1].ttype));

		  yyval.ttype = comptypes (e1, e2)
		    ? build_int_2 (1, 0) : build_int_2 (0, 0);
		;}
    break;

  case 99:
#line 656 "c-parse.y"
    { yyval.ttype = build_array_ref (yyvsp[-3].ttype, yyvsp[-1].ttype); ;}
    break;

  case 100:
#line 658 "c-parse.y"
    {
		      yyval.ttype = build_component_ref (yyvsp[-2].ttype, yyvsp[0].ttype);
		;}
    break;

  case 101:
#line 662 "c-parse.y"
    {
                  tree expr = build_indirect_ref (yyvsp[-2].ttype, "->");

			yyval.ttype = build_component_ref (expr, yyvsp[0].ttype);
		;}
    break;

  case 102:
#line 668 "c-parse.y"
    { yyval.ttype = build_unary_op (POSTINCREMENT_EXPR, yyvsp[-1].ttype, 0); ;}
    break;

  case 103:
#line 670 "c-parse.y"
    { yyval.ttype = build_unary_op (POSTDECREMENT_EXPR, yyvsp[-1].ttype, 0); ;}
    break;

  case 105:
#line 677 "c-parse.y"
    {
                  static int last_lineno = 0;
                  static const char *last_input_filename = 0;
                  yyval.ttype = chainon (yyvsp[-1].ttype, yyvsp[0].ttype);
		  if (warn_traditional && !in_system_header
		      && (lineno != last_lineno || !last_input_filename ||
			  strcmp (last_input_filename, input_filename)))
		    {
		      warning ("traditional C rejects string concatenation");
		      last_lineno = lineno;
		      last_input_filename = input_filename;
		    }
		;}
    break;

  case 108:
#line 698 "c-parse.y"
    { c_mark_varargs ();
		  if (pedantic)
		    pedwarn ("ISO C does not permit use of `varargs.h'"); ;}
    break;

  case 109:
#line 708 "c-parse.y"
    { ;}
    break;

  case 114:
#line 724 "c-parse.y"
    { POP_DECLSPEC_STACK; ;}
    break;

  case 115:
#line 726 "c-parse.y"
    { POP_DECLSPEC_STACK; ;}
    break;

  case 116:
#line 728 "c-parse.y"
    { shadow_tag_warned (yyvsp[-1].ttype, 1);
		  pedwarn ("empty declaration"); ;}
    break;

  case 117:
#line 731 "c-parse.y"
    { pedwarn ("empty declaration"); ;}
    break;

  case 118:
#line 740 "c-parse.y"
    { ;}
    break;

  case 119:
#line 748 "c-parse.y"
    { pending_xref_error ();
		  PUSH_DECLSPEC_STACK;
		  split_specs_attrs (yyvsp[0].ttype,
				     &current_declspecs, &prefix_attributes);
		  all_prefix_attributes = prefix_attributes; ;}
    break;

  case 120:
#line 759 "c-parse.y"
    { all_prefix_attributes = chainon (yyvsp[0].ttype, prefix_attributes); ;}
    break;

  case 121:
#line 764 "c-parse.y"
    { POP_DECLSPEC_STACK; ;}
    break;

  case 122:
#line 766 "c-parse.y"
    { POP_DECLSPEC_STACK; ;}
    break;

  case 123:
#line 768 "c-parse.y"
    { POP_DECLSPEC_STACK; ;}
    break;

  case 124:
#line 770 "c-parse.y"
    { POP_DECLSPEC_STACK; ;}
    break;

  case 125:
#line 772 "c-parse.y"
    { shadow_tag (yyvsp[-1].ttype); ;}
    break;

  case 126:
#line 774 "c-parse.y"
    { RESTORE_WARN_FLAGS (yyvsp[-1].ttype); ;}
    break;

  case 127:
#line 831 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, NULL_TREE);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 128:
#line 834 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 129:
#line 837 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 130:
#line 843 "c-parse.y"
    { yyval.ttype = tree_cons (yyvsp[0].ttype, NULL_TREE, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 131:
#line 849 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 132:
#line 852 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 133:
#line 858 "c-parse.y"
    { yyval.ttype = tree_cons (yyvsp[0].ttype, NULL_TREE, NULL_TREE);
		  TREE_STATIC (yyval.ttype) = 0; ;}
    break;

  case 134:
#line 861 "c-parse.y"
    { yyval.ttype = tree_cons (yyvsp[0].ttype, NULL_TREE, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 135:
#line 867 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, NULL_TREE);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 136:
#line 870 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 137:
#line 873 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 138:
#line 876 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 139:
#line 879 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 140:
#line 882 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 141:
#line 885 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 142:
#line 891 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, NULL_TREE);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 143:
#line 894 "c-parse.y"
    { yyval.ttype = tree_cons (yyvsp[0].ttype, NULL_TREE, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 144:
#line 897 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 145:
#line 900 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 146:
#line 903 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 147:
#line 906 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 148:
#line 912 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 149:
#line 915 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 150:
#line 918 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 151:
#line 921 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 152:
#line 924 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 153:
#line 927 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 154:
#line 933 "c-parse.y"
    { yyval.ttype = tree_cons (yyvsp[0].ttype, NULL_TREE, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 155:
#line 936 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 156:
#line 939 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 157:
#line 942 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 158:
#line 945 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 159:
#line 951 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, NULL_TREE);
		  TREE_STATIC (yyval.ttype) = 0; ;}
    break;

  case 160:
#line 954 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 161:
#line 957 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 162:
#line 960 "c-parse.y"
    { if (extra_warnings && TREE_STATIC (yyvsp[-1].ttype))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER (yyvsp[0].ttype));
		  yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 163:
#line 966 "c-parse.y"
    { if (extra_warnings && TREE_STATIC (yyvsp[-1].ttype))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER (yyvsp[0].ttype));
		  yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 164:
#line 972 "c-parse.y"
    { if (extra_warnings && TREE_STATIC (yyvsp[-1].ttype))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER (yyvsp[0].ttype));
		  yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 165:
#line 978 "c-parse.y"
    { if (extra_warnings && TREE_STATIC (yyvsp[-1].ttype))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER (yyvsp[0].ttype));
		  yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 166:
#line 987 "c-parse.y"
    { yyval.ttype = tree_cons (yyvsp[0].ttype, NULL_TREE, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 167:
#line 993 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 168:
#line 996 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 169:
#line 999 "c-parse.y"
    { if (extra_warnings && TREE_STATIC (yyvsp[-1].ttype))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER (yyvsp[0].ttype));
		  yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 170:
#line 1005 "c-parse.y"
    { if (extra_warnings && TREE_STATIC (yyvsp[-1].ttype))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER (yyvsp[0].ttype));
		  yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 171:
#line 1011 "c-parse.y"
    { if (extra_warnings && TREE_STATIC (yyvsp[-1].ttype))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER (yyvsp[0].ttype));
		  yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 172:
#line 1017 "c-parse.y"
    { if (extra_warnings && TREE_STATIC (yyvsp[-1].ttype))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER (yyvsp[0].ttype));
		  yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 173:
#line 1026 "c-parse.y"
    { yyval.ttype = tree_cons (yyvsp[0].ttype, NULL_TREE, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 174:
#line 1032 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 175:
#line 1035 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 176:
#line 1038 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 177:
#line 1041 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 178:
#line 1044 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 179:
#line 1047 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 180:
#line 1050 "c-parse.y"
    { if (extra_warnings && TREE_STATIC (yyvsp[-1].ttype))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER (yyvsp[0].ttype));
		  yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 181:
#line 1056 "c-parse.y"
    { if (extra_warnings && TREE_STATIC (yyvsp[-1].ttype))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER (yyvsp[0].ttype));
		  yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 182:
#line 1062 "c-parse.y"
    { if (extra_warnings && TREE_STATIC (yyvsp[-1].ttype))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER (yyvsp[0].ttype));
		  yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 183:
#line 1068 "c-parse.y"
    { if (extra_warnings && TREE_STATIC (yyvsp[-1].ttype))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER (yyvsp[0].ttype));
		  yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 184:
#line 1077 "c-parse.y"
    { yyval.ttype = tree_cons (yyvsp[0].ttype, NULL_TREE, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 185:
#line 1080 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 186:
#line 1083 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 187:
#line 1086 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 188:
#line 1089 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 189:
#line 1095 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 190:
#line 1098 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 191:
#line 1101 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 192:
#line 1104 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 193:
#line 1107 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 194:
#line 1110 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 195:
#line 1113 "c-parse.y"
    { if (extra_warnings && TREE_STATIC (yyvsp[-1].ttype))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER (yyvsp[0].ttype));
		  yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 196:
#line 1119 "c-parse.y"
    { if (extra_warnings && TREE_STATIC (yyvsp[-1].ttype))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER (yyvsp[0].ttype));
		  yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 197:
#line 1125 "c-parse.y"
    { if (extra_warnings && TREE_STATIC (yyvsp[-1].ttype))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER (yyvsp[0].ttype));
		  yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 198:
#line 1131 "c-parse.y"
    { if (extra_warnings && TREE_STATIC (yyvsp[-1].ttype))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER (yyvsp[0].ttype));
		  yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 199:
#line 1140 "c-parse.y"
    { yyval.ttype = tree_cons (yyvsp[0].ttype, NULL_TREE, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 200:
#line 1143 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 201:
#line 1146 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 202:
#line 1149 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 203:
#line 1152 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 260:
#line 1240 "c-parse.y"
    { yyval.ttype = NULL_TREE; ;}
    break;

  case 261:
#line 1242 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype; ;}
    break;

  case 265:
#line 1277 "c-parse.y"
    { OBJC_NEED_RAW_IDENTIFIER (1);	;}
    break;

  case 268:
#line 1287 "c-parse.y"
    { /* For a typedef name, record the meaning, not the name.
		     In case of `foo foo, bar;'.  */
		  yyval.ttype = lookup_name (yyvsp[0].ttype); ;}
    break;

  case 269:
#line 1291 "c-parse.y"
    { skip_evaluation--; yyval.ttype = TREE_TYPE (yyvsp[-1].ttype); ;}
    break;

  case 270:
#line 1293 "c-parse.y"
    { skip_evaluation--; yyval.ttype = groktypename (yyvsp[-1].ttype); ;}
    break;

  case 275:
#line 1310 "c-parse.y"
    { yyval.ttype = NULL_TREE; ;}
    break;

  case 276:
#line 1312 "c-parse.y"
    { if (TREE_CHAIN (yyvsp[-1].ttype)) yyvsp[-1].ttype = combine_strings (yyvsp[-1].ttype);
		  yyval.ttype = yyvsp[-1].ttype;
		;}
    break;

  case 277:
#line 1319 "c-parse.y"
    { yyval.ttype = start_decl (yyvsp[-3].ttype, current_declspecs, 1,
					  chainon (yyvsp[-1].ttype, all_prefix_attributes));
		  start_init (yyval.ttype, yyvsp[-2].ttype, global_bindings_p ()); ;}
    break;

  case 278:
#line 1324 "c-parse.y"
    { finish_init ();
		  finish_decl (yyvsp[-1].ttype, yyvsp[0].ttype, yyvsp[-4].ttype); ;}
    break;

  case 279:
#line 1327 "c-parse.y"
    { tree d = start_decl (yyvsp[-2].ttype, current_declspecs, 0,
				       chainon (yyvsp[0].ttype, all_prefix_attributes));
		  finish_decl (d, NULL_TREE, yyvsp[-1].ttype); 
                ;}
    break;

  case 280:
#line 1335 "c-parse.y"
    { yyval.ttype = start_decl (yyvsp[-3].ttype, current_declspecs, 1,
					  chainon (yyvsp[-1].ttype, all_prefix_attributes));
		  start_init (yyval.ttype, yyvsp[-2].ttype, global_bindings_p ()); ;}
    break;

  case 281:
#line 1340 "c-parse.y"
    { finish_init ();
		  finish_decl (yyvsp[-1].ttype, yyvsp[0].ttype, yyvsp[-4].ttype); ;}
    break;

  case 282:
#line 1343 "c-parse.y"
    { tree d = start_decl (yyvsp[-2].ttype, current_declspecs, 0,
				       chainon (yyvsp[0].ttype, all_prefix_attributes));
		  finish_decl (d, NULL_TREE, yyvsp[-1].ttype); ;}
    break;

  case 283:
#line 1351 "c-parse.y"
    { yyval.ttype = NULL_TREE; ;}
    break;

  case 284:
#line 1353 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype; ;}
    break;

  case 285:
#line 1358 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype; ;}
    break;

  case 286:
#line 1360 "c-parse.y"
    { yyval.ttype = chainon (yyvsp[-1].ttype, yyvsp[0].ttype); ;}
    break;

  case 287:
#line 1365 "c-parse.y"
    { yyval.ttype = yyvsp[-2].ttype; ;}
    break;

  case 288:
#line 1370 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype; ;}
    break;

  case 289:
#line 1372 "c-parse.y"
    { yyval.ttype = chainon (yyvsp[-2].ttype, yyvsp[0].ttype); ;}
    break;

  case 290:
#line 1377 "c-parse.y"
    { yyval.ttype = NULL_TREE; ;}
    break;

  case 291:
#line 1379 "c-parse.y"
    { yyval.ttype = build_tree_list (yyvsp[0].ttype, NULL_TREE); ;}
    break;

  case 292:
#line 1381 "c-parse.y"
    { yyval.ttype = build_tree_list (yyvsp[-3].ttype, build_tree_list (NULL_TREE, yyvsp[-1].ttype)); ;}
    break;

  case 293:
#line 1383 "c-parse.y"
    { yyval.ttype = build_tree_list (yyvsp[-5].ttype, tree_cons (NULL_TREE, yyvsp[-3].ttype, yyvsp[-1].ttype)); ;}
    break;

  case 294:
#line 1385 "c-parse.y"
    { yyval.ttype = build_tree_list (yyvsp[-3].ttype, yyvsp[-1].ttype); ;}
    break;

  case 300:
#line 1403 "c-parse.y"
    { really_start_incremental_init (NULL_TREE); ;}
    break;

  case 301:
#line 1405 "c-parse.y"
    { yyval.ttype = pop_init_level (0); ;}
    break;

  case 302:
#line 1407 "c-parse.y"
    { yyval.ttype = error_mark_node; ;}
    break;

  case 303:
#line 1413 "c-parse.y"
    { if (pedantic)
		    pedwarn ("ISO C forbids empty initializer braces"); ;}
    break;

  case 307:
#line 1427 "c-parse.y"
    { if (pedantic && ! flag_isoc99)
		    pedwarn ("ISO C89 forbids specifying subobject to initialize"); ;}
    break;

  case 308:
#line 1430 "c-parse.y"
    { if (pedantic)
		    pedwarn ("obsolete use of designated initializer without `='"); ;}
    break;

  case 309:
#line 1433 "c-parse.y"
    { set_init_label (yyvsp[-1].ttype);
		  if (pedantic)
		    pedwarn ("obsolete use of designated initializer with `:'"); ;}
    break;

  case 310:
#line 1437 "c-parse.y"
    {;}
    break;

  case 312:
#line 1443 "c-parse.y"
    { push_init_level (0); ;}
    break;

  case 313:
#line 1445 "c-parse.y"
    { process_init_element (pop_init_level (0)); ;}
    break;

  case 314:
#line 1447 "c-parse.y"
    { process_init_element (yyvsp[0].ttype); ;}
    break;

  case 318:
#line 1458 "c-parse.y"
    { set_init_label (yyvsp[0].ttype); ;}
    break;

  case 319:
#line 1463 "c-parse.y"
    { set_init_index (yyvsp[-3].ttype, yyvsp[-1].ttype);
		  if (pedantic)
		    pedwarn ("ISO C forbids specifying range of elements to initialize"); ;}
    break;

  case 320:
#line 1467 "c-parse.y"
    { set_init_index (yyvsp[-1].ttype, NULL_TREE); ;}
    break;

  case 321:
#line 1472 "c-parse.y"
    { if (pedantic)
		    pedwarn ("ISO C forbids nested functions");

		  push_function_context ();
		  if (! start_function (current_declspecs, yyvsp[0].ttype,
					all_prefix_attributes))
		    {
		      pop_function_context ();
		      YYERROR1;
		    }
		;}
    break;

  case 322:
#line 1484 "c-parse.y"
    { store_parm_decls (); ;}
    break;

  case 323:
#line 1492 "c-parse.y"
    { tree decl = current_function_decl;
		  DECL_SOURCE_FILE (decl) = yyvsp[-2].filename;
		  DECL_SOURCE_LINE (decl) = yyvsp[-1].lineno;
		  finish_function (1, 1);
		  pop_function_context (); 
		  add_decl_stmt (decl); ;}
    break;

  case 324:
#line 1502 "c-parse.y"
    { if (pedantic)
		    pedwarn ("ISO C forbids nested functions");

		  push_function_context ();
		  if (! start_function (current_declspecs, yyvsp[0].ttype,
					all_prefix_attributes))
		    {
		      pop_function_context ();
		      YYERROR1;
		    }
		;}
    break;

  case 325:
#line 1514 "c-parse.y"
    { store_parm_decls (); ;}
    break;

  case 326:
#line 1522 "c-parse.y"
    { tree decl = current_function_decl;
		  DECL_SOURCE_FILE (decl) = yyvsp[-2].filename;
		  DECL_SOURCE_LINE (decl) = yyvsp[-1].lineno;
		  finish_function (1, 1);
		  pop_function_context (); 
		  add_decl_stmt (decl); ;}
    break;

  case 329:
#line 1542 "c-parse.y"
    { yyval.ttype = yyvsp[-2].ttype ? tree_cons (yyvsp[-2].ttype, yyvsp[-1].ttype, NULL_TREE) : yyvsp[-1].ttype; ;}
    break;

  case 330:
#line 1544 "c-parse.y"
    { yyval.ttype = build_nt (CALL_EXPR, yyvsp[-2].ttype, yyvsp[0].ttype, NULL_TREE); ;}
    break;

  case 331:
#line 1549 "c-parse.y"
    { yyval.ttype = set_array_declarator_type (yyvsp[0].ttype, yyvsp[-1].ttype, 0); ;}
    break;

  case 332:
#line 1551 "c-parse.y"
    { yyval.ttype = make_pointer_declarator (yyvsp[-1].ttype, yyvsp[0].ttype); ;}
    break;

  case 336:
#line 1566 "c-parse.y"
    { yyval.ttype = build_nt (CALL_EXPR, yyvsp[-2].ttype, yyvsp[0].ttype, NULL_TREE); ;}
    break;

  case 337:
#line 1571 "c-parse.y"
    { yyval.ttype = set_array_declarator_type (yyvsp[0].ttype, yyvsp[-1].ttype, 0); ;}
    break;

  case 339:
#line 1577 "c-parse.y"
    { yyval.ttype = build_nt (CALL_EXPR, yyvsp[-2].ttype, yyvsp[0].ttype, NULL_TREE); ;}
    break;

  case 340:
#line 1582 "c-parse.y"
    { yyval.ttype = set_array_declarator_type (yyvsp[0].ttype, yyvsp[-1].ttype, 0); ;}
    break;

  case 341:
#line 1584 "c-parse.y"
    { yyval.ttype = make_pointer_declarator (yyvsp[-1].ttype, yyvsp[0].ttype); ;}
    break;

  case 342:
#line 1586 "c-parse.y"
    { yyval.ttype = make_pointer_declarator (yyvsp[-1].ttype, yyvsp[0].ttype); ;}
    break;

  case 343:
#line 1588 "c-parse.y"
    { yyval.ttype = yyvsp[-2].ttype ? tree_cons (yyvsp[-2].ttype, yyvsp[-1].ttype, NULL_TREE) : yyvsp[-1].ttype; ;}
    break;

  case 344:
#line 1596 "c-parse.y"
    { yyval.ttype = build_nt (CALL_EXPR, yyvsp[-2].ttype, yyvsp[0].ttype, NULL_TREE); ;}
    break;

  case 345:
#line 1601 "c-parse.y"
    { yyval.ttype = yyvsp[-2].ttype ? tree_cons (yyvsp[-2].ttype, yyvsp[-1].ttype, NULL_TREE) : yyvsp[-1].ttype; ;}
    break;

  case 346:
#line 1603 "c-parse.y"
    { yyval.ttype = make_pointer_declarator (yyvsp[-1].ttype, yyvsp[0].ttype); ;}
    break;

  case 347:
#line 1605 "c-parse.y"
    { yyval.ttype = set_array_declarator_type (yyvsp[0].ttype, yyvsp[-1].ttype, 0); ;}
    break;

  case 349:
#line 1611 "c-parse.y"
    { yyval.ttype = NULL_TREE; ;}
    break;

  case 350:
#line 1613 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype; ;}
    break;

  case 351:
#line 1618 "c-parse.y"
    { yyval.ttype = NULL_TREE; ;}
    break;

  case 352:
#line 1620 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype; ;}
    break;

  case 353:
#line 1625 "c-parse.y"
    { yyval.ttype = NULL_TREE; ;}
    break;

  case 354:
#line 1627 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype; ;}
    break;

  case 355:
#line 1638 "c-parse.y"
    { yyval.ttype = start_struct (RECORD_TYPE, yyvsp[-1].ttype);
		  /* Start scope of tag before parsing components.  */
		;}
    break;

  case 356:
#line 1642 "c-parse.y"
    { yyval.ttype = finish_struct (yyvsp[-3].ttype, yyvsp[-2].ttype, chainon (yyvsp[-6].ttype, yyvsp[0].ttype)); ;}
    break;

  case 357:
#line 1644 "c-parse.y"
    { yyval.ttype = finish_struct (start_struct (RECORD_TYPE, NULL_TREE),
				      yyvsp[-2].ttype, chainon (yyvsp[-4].ttype, yyvsp[0].ttype));
		;}
    break;

  case 358:
#line 1648 "c-parse.y"
    { yyval.ttype = start_struct (UNION_TYPE, yyvsp[-1].ttype); ;}
    break;

  case 359:
#line 1650 "c-parse.y"
    { yyval.ttype = finish_struct (yyvsp[-3].ttype, yyvsp[-2].ttype, chainon (yyvsp[-6].ttype, yyvsp[0].ttype)); ;}
    break;

  case 360:
#line 1652 "c-parse.y"
    { yyval.ttype = finish_struct (start_struct (UNION_TYPE, NULL_TREE),
				      yyvsp[-2].ttype, chainon (yyvsp[-4].ttype, yyvsp[0].ttype));
		;}
    break;

  case 361:
#line 1656 "c-parse.y"
    { yyval.ttype = start_enum (yyvsp[-1].ttype); ;}
    break;

  case 362:
#line 1658 "c-parse.y"
    { yyval.ttype = finish_enum (yyvsp[-4].ttype, nreverse (yyvsp[-3].ttype),
				    chainon (yyvsp[-7].ttype, yyvsp[0].ttype)); ;}
    break;

  case 363:
#line 1661 "c-parse.y"
    { yyval.ttype = start_enum (NULL_TREE); ;}
    break;

  case 364:
#line 1663 "c-parse.y"
    { yyval.ttype = finish_enum (yyvsp[-4].ttype, nreverse (yyvsp[-3].ttype),
				    chainon (yyvsp[-6].ttype, yyvsp[0].ttype)); ;}
    break;

  case 365:
#line 1669 "c-parse.y"
    { yyval.ttype = xref_tag (RECORD_TYPE, yyvsp[0].ttype); ;}
    break;

  case 366:
#line 1671 "c-parse.y"
    { yyval.ttype = xref_tag (UNION_TYPE, yyvsp[0].ttype); ;}
    break;

  case 367:
#line 1673 "c-parse.y"
    { yyval.ttype = xref_tag (ENUMERAL_TYPE, yyvsp[0].ttype);
		  /* In ISO C, enumerated types can be referred to
		     only if already defined.  */
		  if (pedantic && !COMPLETE_TYPE_P (yyval.ttype))
		    pedwarn ("ISO C forbids forward references to `enum' types"); ;}
    break;

  case 371:
#line 1688 "c-parse.y"
    { if (pedantic && ! flag_isoc99)
		    pedwarn ("comma at end of enumerator list"); ;}
    break;

  case 372:
#line 1694 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype; ;}
    break;

  case 373:
#line 1696 "c-parse.y"
    { yyval.ttype = chainon (yyvsp[-1].ttype, yyvsp[0].ttype);
		  pedwarn ("no semicolon at end of struct or union"); ;}
    break;

  case 374:
#line 1701 "c-parse.y"
    { yyval.ttype = NULL_TREE; ;}
    break;

  case 375:
#line 1703 "c-parse.y"
    { yyval.ttype = chainon (yyvsp[-2].ttype, yyvsp[-1].ttype); ;}
    break;

  case 376:
#line 1705 "c-parse.y"
    { if (pedantic)
		    pedwarn ("extra semicolon in struct or union specified"); ;}
    break;

  case 377:
#line 1711 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype;
		  POP_DECLSPEC_STACK; ;}
    break;

  case 378:
#line 1714 "c-parse.y"
    {
		  /* Support for unnamed structs or unions as members of 
		     structs or unions (which is [a] useful and [b] supports 
		     MS P-SDK).  */
		  if (pedantic)
		    pedwarn ("ISO C doesn't support unnamed structs/unions");

		  yyval.ttype = grokfield(yyvsp[-1].filename, yyvsp[0].lineno, NULL, current_declspecs, NULL_TREE);
		  POP_DECLSPEC_STACK; ;}
    break;

  case 379:
#line 1724 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype;
		  POP_DECLSPEC_STACK; ;}
    break;

  case 380:
#line 1727 "c-parse.y"
    { if (pedantic)
		    pedwarn ("ISO C forbids member declarations with no members");
		  shadow_tag(yyvsp[0].ttype);
		  yyval.ttype = NULL_TREE; ;}
    break;

  case 381:
#line 1732 "c-parse.y"
    { yyval.ttype = NULL_TREE; ;}
    break;

  case 382:
#line 1734 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype;
		  RESTORE_WARN_FLAGS (yyvsp[-1].ttype); ;}
    break;

  case 384:
#line 1741 "c-parse.y"
    { yyval.ttype = chainon (yyvsp[-3].ttype, yyvsp[0].ttype); ;}
    break;

  case 386:
#line 1747 "c-parse.y"
    { yyval.ttype = chainon (yyvsp[-3].ttype, yyvsp[0].ttype); ;}
    break;

  case 387:
#line 1752 "c-parse.y"
    { yyval.ttype = grokfield (yyvsp[-3].filename, yyvsp[-2].lineno, yyvsp[-1].ttype, current_declspecs, NULL_TREE);
		  decl_attributes (&yyval.ttype, chainon (yyvsp[0].ttype, all_prefix_attributes), 0); ;}
    break;

  case 388:
#line 1756 "c-parse.y"
    { yyval.ttype = grokfield (yyvsp[-5].filename, yyvsp[-4].lineno, yyvsp[-3].ttype, current_declspecs, yyvsp[-1].ttype);
		  decl_attributes (&yyval.ttype, chainon (yyvsp[0].ttype, all_prefix_attributes), 0); ;}
    break;

  case 389:
#line 1759 "c-parse.y"
    { yyval.ttype = grokfield (yyvsp[-4].filename, yyvsp[-3].lineno, NULL_TREE, current_declspecs, yyvsp[-1].ttype);
		  decl_attributes (&yyval.ttype, chainon (yyvsp[0].ttype, all_prefix_attributes), 0); ;}
    break;

  case 390:
#line 1765 "c-parse.y"
    { yyval.ttype = grokfield (yyvsp[-3].filename, yyvsp[-2].lineno, yyvsp[-1].ttype, current_declspecs, NULL_TREE);
		  decl_attributes (&yyval.ttype, chainon (yyvsp[0].ttype, all_prefix_attributes), 0); ;}
    break;

  case 391:
#line 1769 "c-parse.y"
    { yyval.ttype = grokfield (yyvsp[-5].filename, yyvsp[-4].lineno, yyvsp[-3].ttype, current_declspecs, yyvsp[-1].ttype);
		  decl_attributes (&yyval.ttype, chainon (yyvsp[0].ttype, all_prefix_attributes), 0); ;}
    break;

  case 392:
#line 1772 "c-parse.y"
    { yyval.ttype = grokfield (yyvsp[-4].filename, yyvsp[-3].lineno, NULL_TREE, current_declspecs, yyvsp[-1].ttype);
		  decl_attributes (&yyval.ttype, chainon (yyvsp[0].ttype, all_prefix_attributes), 0); ;}
    break;

  case 394:
#line 1784 "c-parse.y"
    { if (yyvsp[-2].ttype == error_mark_node)
		    yyval.ttype = yyvsp[-2].ttype;
		  else
		    yyval.ttype = chainon (yyvsp[0].ttype, yyvsp[-2].ttype); ;}
    break;

  case 395:
#line 1789 "c-parse.y"
    { yyval.ttype = error_mark_node; ;}
    break;

  case 396:
#line 1795 "c-parse.y"
    { yyval.ttype = build_enumerator (yyvsp[0].ttype, NULL_TREE); ;}
    break;

  case 397:
#line 1797 "c-parse.y"
    { yyval.ttype = build_enumerator (yyvsp[-2].ttype, yyvsp[0].ttype); ;}
    break;

  case 398:
#line 1802 "c-parse.y"
    { pending_xref_error ();
		  yyval.ttype = yyvsp[0].ttype; ;}
    break;

  case 399:
#line 1805 "c-parse.y"
    { yyval.ttype = build_tree_list (yyvsp[-1].ttype, yyvsp[0].ttype); ;}
    break;

  case 400:
#line 1810 "c-parse.y"
    { yyval.ttype = NULL_TREE; ;}
    break;

  case 402:
#line 1816 "c-parse.y"
    { yyval.ttype = build_tree_list (build_tree_list (current_declspecs,
							 NULL_TREE),
					all_prefix_attributes); ;}
    break;

  case 403:
#line 1820 "c-parse.y"
    { yyval.ttype = build_tree_list (build_tree_list (current_declspecs,
							 yyvsp[0].ttype),
					all_prefix_attributes); ;}
    break;

  case 404:
#line 1824 "c-parse.y"
    { yyval.ttype = build_tree_list (build_tree_list (current_declspecs,
							 yyvsp[-1].ttype),
					chainon (yyvsp[0].ttype, all_prefix_attributes)); ;}
    break;

  case 408:
#line 1837 "c-parse.y"
    { yyval.ttype = make_pointer_declarator (yyvsp[-1].ttype, yyvsp[0].ttype); ;}
    break;

  case 409:
#line 1842 "c-parse.y"
    { yyval.ttype = make_pointer_declarator (yyvsp[0].ttype, NULL_TREE); ;}
    break;

  case 410:
#line 1844 "c-parse.y"
    { yyval.ttype = make_pointer_declarator (yyvsp[-1].ttype, yyvsp[0].ttype); ;}
    break;

  case 411:
#line 1849 "c-parse.y"
    { yyval.ttype = yyvsp[-2].ttype ? tree_cons (yyvsp[-2].ttype, yyvsp[-1].ttype, NULL_TREE) : yyvsp[-1].ttype; ;}
    break;

  case 412:
#line 1851 "c-parse.y"
    { yyval.ttype = build_nt (CALL_EXPR, yyvsp[-2].ttype, yyvsp[0].ttype, NULL_TREE); ;}
    break;

  case 413:
#line 1853 "c-parse.y"
    { yyval.ttype = set_array_declarator_type (yyvsp[0].ttype, yyvsp[-1].ttype, 1); ;}
    break;

  case 414:
#line 1855 "c-parse.y"
    { yyval.ttype = build_nt (CALL_EXPR, NULL_TREE, yyvsp[0].ttype, NULL_TREE); ;}
    break;

  case 415:
#line 1857 "c-parse.y"
    { yyval.ttype = set_array_declarator_type (yyvsp[0].ttype, NULL_TREE, 1); ;}
    break;

  case 416:
#line 1864 "c-parse.y"
    { yyval.ttype = build_array_declarator (yyvsp[-1].ttype, NULL_TREE, 0, 0); ;}
    break;

  case 417:
#line 1866 "c-parse.y"
    { yyval.ttype = build_array_declarator (yyvsp[-1].ttype, yyvsp[-2].ttype, 0, 0); ;}
    break;

  case 418:
#line 1868 "c-parse.y"
    { yyval.ttype = build_array_declarator (NULL_TREE, NULL_TREE, 0, 0); ;}
    break;

  case 419:
#line 1870 "c-parse.y"
    { yyval.ttype = build_array_declarator (NULL_TREE, yyvsp[-1].ttype, 0, 0); ;}
    break;

  case 420:
#line 1872 "c-parse.y"
    { yyval.ttype = build_array_declarator (NULL_TREE, NULL_TREE, 0, 1); ;}
    break;

  case 421:
#line 1874 "c-parse.y"
    { yyval.ttype = build_array_declarator (NULL_TREE, yyvsp[-2].ttype, 0, 1); ;}
    break;

  case 422:
#line 1876 "c-parse.y"
    { if (C_RID_CODE (yyvsp[-2].ttype) != RID_STATIC)
		    error ("storage class specifier in array declarator");
		  yyval.ttype = build_array_declarator (yyvsp[-1].ttype, NULL_TREE, 1, 0); ;}
    break;

  case 423:
#line 1880 "c-parse.y"
    { if (C_RID_CODE (yyvsp[-3].ttype) != RID_STATIC)
		    error ("storage class specifier in array declarator");
		  yyval.ttype = build_array_declarator (yyvsp[-1].ttype, yyvsp[-2].ttype, 1, 0); ;}
    break;

  case 424:
#line 1884 "c-parse.y"
    { if (C_RID_CODE (yyvsp[-2].ttype) != RID_STATIC)
		    error ("storage class specifier in array declarator");
		  yyval.ttype = build_array_declarator (yyvsp[-1].ttype, yyvsp[-3].ttype, 1, 0); ;}
    break;

  case 427:
#line 1899 "c-parse.y"
    {
		  pedwarn ("deprecated use of label at end of compound statement");
		;}
    break;

  case 435:
#line 1916 "c-parse.y"
    { if (pedantic && !flag_isoc99)
		    pedwarn ("ISO C89 forbids mixed declarations and code"); ;}
    break;

  case 450:
#line 1946 "c-parse.y"
    { pushlevel (0);
		  clear_last_expr ();
		  add_scope_stmt (/*begin_p=*/1, /*partial_p=*/0);
		;}
    break;

  case 451:
#line 1953 "c-parse.y"
    { yyval.ttype = add_scope_stmt (/*begin_p=*/0, /*partial_p=*/0); ;}
    break;

  case 452:
#line 1958 "c-parse.y"
    { if (flag_isoc99)
		    {
		      yyval.ttype = c_begin_compound_stmt ();
		      pushlevel (0);
		      clear_last_expr ();
		      add_scope_stmt (/*begin_p=*/1, /*partial_p=*/0);
		    }
		  else
		    yyval.ttype = NULL_TREE;
		;}
    break;

  case 453:
#line 1974 "c-parse.y"
    { if (flag_isoc99)
		    {
		      tree scope_stmt = add_scope_stmt (/*begin_p=*/0, /*partial_p=*/0);
		      yyval.ttype = poplevel (kept_level_p (), 0, 0); 
		      SCOPE_STMT_BLOCK (TREE_PURPOSE (scope_stmt)) 
			= SCOPE_STMT_BLOCK (TREE_VALUE (scope_stmt))
			= yyval.ttype;
		    }
		  else
		    yyval.ttype = NULL_TREE; ;}
    break;

  case 455:
#line 1991 "c-parse.y"
    { if (pedantic)
		    pedwarn ("ISO C forbids label declarations"); ;}
    break;

  case 458:
#line 2002 "c-parse.y"
    { tree link;
		  for (link = yyvsp[-1].ttype; link; link = TREE_CHAIN (link))
		    {
		      tree label = shadow_label (TREE_VALUE (link));
		      C_DECLARED_LABEL_FLAG (label) = 1;
		      add_decl_stmt (label);
		    }
		;}
    break;

  case 459:
#line 2016 "c-parse.y"
    {;}
    break;

  case 461:
#line 2020 "c-parse.y"
    { compstmt_count++;
                      yyval.ttype = c_begin_compound_stmt (); ;}
    break;

  case 462:
#line 2025 "c-parse.y"
    { yyval.ttype = convert (void_type_node, integer_zero_node); ;}
    break;

  case 463:
#line 2027 "c-parse.y"
    { yyval.ttype = poplevel (kept_level_p (), 1, 0); 
		  SCOPE_STMT_BLOCK (TREE_PURPOSE (yyvsp[0].ttype)) 
		    = SCOPE_STMT_BLOCK (TREE_VALUE (yyvsp[0].ttype))
		    = yyval.ttype; ;}
    break;

  case 466:
#line 2040 "c-parse.y"
    { if (current_function_decl == 0)
		    {
		      error ("braced-group within expression allowed only inside a function");
		      YYERROR;
		    }
		  /* We must force a BLOCK for this level
		     so that, if it is not expanded later,
		     there is a way to turn off the entire subtree of blocks
		     that are contained in it.  */
		  keep_next_level ();
		  push_label_level ();
		  compstmt_count++;
		  yyval.ttype = add_stmt (build_stmt (COMPOUND_STMT, last_tree));
		;}
    break;

  case 467:
#line 2057 "c-parse.y"
    { RECHAIN_STMTS (yyvsp[-1].ttype, COMPOUND_BODY (yyvsp[-1].ttype)); 
		  last_expr_type = NULL_TREE;
                  yyval.ttype = yyvsp[-1].ttype; ;}
    break;

  case 468:
#line 2065 "c-parse.y"
    { c_finish_then (); ;}
    break;

  case 470:
#line 2082 "c-parse.y"
    { yyval.ttype = c_begin_if_stmt (); ;}
    break;

  case 471:
#line 2084 "c-parse.y"
    { c_expand_start_cond (truthvalue_conversion (yyvsp[-1].ttype), 
				       compstmt_count,yyvsp[-3].ttype);
		  yyval.itype = stmt_count;
		  if_stmt_file = yyvsp[-7].filename;
		  if_stmt_line = yyvsp[-6].lineno; ;}
    break;

  case 472:
#line 2096 "c-parse.y"
    { stmt_count++;
		  compstmt_count++;
		  yyval.ttype 
		    = add_stmt (build_stmt (DO_STMT, NULL_TREE,
					    NULL_TREE));
		  /* In the event that a parse error prevents
		     parsing the complete do-statement, set the
		     condition now.  Otherwise, we can get crashes at
		     RTL-generation time.  */
		  DO_COND (yyval.ttype) = error_mark_node; ;}
    break;

  case 473:
#line 2107 "c-parse.y"
    { yyval.ttype = yyvsp[-2].ttype;
		  RECHAIN_STMTS (yyval.ttype, DO_BODY (yyval.ttype)); ;}
    break;

  case 474:
#line 2115 "c-parse.y"
    { if (yychar == YYEMPTY)
		    yychar = YYLEX;
		  yyval.filename = input_filename; ;}
    break;

  case 475:
#line 2121 "c-parse.y"
    { if (yychar == YYEMPTY)
		    yychar = YYLEX;
		  yyval.lineno = lineno; ;}
    break;

  case 478:
#line 2134 "c-parse.y"
    { if (flag_isoc99)
		    RECHAIN_STMTS (yyvsp[-2].ttype, COMPOUND_BODY (yyvsp[-2].ttype)); ;}
    break;

  case 479:
#line 2140 "c-parse.y"
    { if (yyvsp[0].ttype)
		    {
		      STMT_LINENO (yyvsp[0].ttype) = yyvsp[-1].lineno;
		      /* ??? We currently have no way of recording
			 the filename for a statement.  This probably
			 matters little in practice at the moment,
			 but I suspect that problems will occur when
			 doing inlining at the tree level.  */
		    }
		;}
    break;

  case 480:
#line 2154 "c-parse.y"
    { if (yyvsp[0].ttype)
		    {
		      STMT_LINENO (yyvsp[0].ttype) = yyvsp[-1].lineno;
		    }
		;}
    break;

  case 481:
#line 2163 "c-parse.y"
    { c_expand_start_else ();
		  yyvsp[-1].itype = stmt_count; ;}
    break;

  case 482:
#line 2166 "c-parse.y"
    { c_finish_else ();
		  c_expand_end_cond ();
		  if (extra_warnings && stmt_count == yyvsp[-3].itype)
		    warning ("empty body in an else-statement"); ;}
    break;

  case 483:
#line 2171 "c-parse.y"
    { c_expand_end_cond ();
		  /* This warning is here instead of in simple_if, because we
		     do not want a warning if an empty if is followed by an
		     else statement.  Increment stmt_count so we don't
		     give a second error if this is a nested `if'.  */
		  if (extra_warnings && stmt_count++ == yyvsp[0].itype)
		    warning_with_file_and_line (if_stmt_file, if_stmt_line,
						"empty body in an if-statement"); ;}
    break;

  case 484:
#line 2183 "c-parse.y"
    { c_expand_end_cond (); ;}
    break;

  case 485:
#line 2193 "c-parse.y"
    { stmt_count++; 
		  yyval.ttype = c_begin_while_stmt (); ;}
    break;

  case 486:
#line 2196 "c-parse.y"
    { yyvsp[-1].ttype = truthvalue_conversion (yyvsp[-1].ttype);
		  c_finish_while_stmt_cond (truthvalue_conversion (yyvsp[-1].ttype),
					    yyvsp[-3].ttype);
		  yyval.ttype = add_stmt (yyvsp[-3].ttype); ;}
    break;

  case 487:
#line 2201 "c-parse.y"
    { RECHAIN_STMTS (yyvsp[-1].ttype, WHILE_BODY (yyvsp[-1].ttype)); ;}
    break;

  case 488:
#line 2204 "c-parse.y"
    { DO_COND (yyvsp[-4].ttype) = truthvalue_conversion (yyvsp[-2].ttype); ;}
    break;

  case 489:
#line 2206 "c-parse.y"
    { ;}
    break;

  case 490:
#line 2208 "c-parse.y"
    { yyval.ttype = build_stmt (FOR_STMT, NULL_TREE, NULL_TREE,
					  NULL_TREE, NULL_TREE);
		  add_stmt (yyval.ttype); ;}
    break;

  case 491:
#line 2212 "c-parse.y"
    { stmt_count++;
		  RECHAIN_STMTS (yyvsp[-2].ttype, FOR_INIT_STMT (yyvsp[-2].ttype)); ;}
    break;

  case 492:
#line 2215 "c-parse.y"
    { if (yyvsp[-1].ttype) 
		    FOR_COND (yyvsp[-5].ttype) = truthvalue_conversion (yyvsp[-1].ttype); ;}
    break;

  case 493:
#line 2218 "c-parse.y"
    { FOR_EXPR (yyvsp[-8].ttype) = yyvsp[-1].ttype; ;}
    break;

  case 494:
#line 2220 "c-parse.y"
    { RECHAIN_STMTS (yyvsp[-10].ttype, FOR_BODY (yyvsp[-10].ttype)); ;}
    break;

  case 495:
#line 2222 "c-parse.y"
    { stmt_count++;
		  yyval.ttype = c_start_case (yyvsp[-1].ttype); ;}
    break;

  case 496:
#line 2225 "c-parse.y"
    { c_finish_case (); ;}
    break;

  case 497:
#line 2230 "c-parse.y"
    { add_stmt (build_stmt (EXPR_STMT, yyvsp[-1].ttype)); ;}
    break;

  case 498:
#line 2232 "c-parse.y"
    { check_for_loop_decls (); ;}
    break;

  case 499:
#line 2238 "c-parse.y"
    { stmt_count++; yyval.ttype = yyvsp[0].ttype; ;}
    break;

  case 500:
#line 2240 "c-parse.y"
    { stmt_count++;
		  yyval.ttype = c_expand_expr_stmt (yyvsp[-1].ttype); ;}
    break;

  case 501:
#line 2243 "c-parse.y"
    { if (flag_isoc99)
		    RECHAIN_STMTS (yyvsp[-2].ttype, COMPOUND_BODY (yyvsp[-2].ttype));
		  yyval.ttype = NULL_TREE; ;}
    break;

  case 502:
#line 2247 "c-parse.y"
    { stmt_count++;
		  yyval.ttype = add_stmt (build_break_stmt ()); ;}
    break;

  case 503:
#line 2250 "c-parse.y"
    { stmt_count++;
		  yyval.ttype = add_stmt (build_continue_stmt ()); ;}
    break;

  case 504:
#line 2253 "c-parse.y"
    { stmt_count++;
		  yyval.ttype = c_expand_return (NULL_TREE); ;}
    break;

  case 505:
#line 2256 "c-parse.y"
    { stmt_count++;
		  yyval.ttype = c_expand_return (yyvsp[-1].ttype); ;}
    break;

  case 506:
#line 2259 "c-parse.y"
    { stmt_count++;
		  yyval.ttype = simple_asm_stmt (yyvsp[-2].ttype); ;}
    break;

  case 507:
#line 2263 "c-parse.y"
    { stmt_count++;
		  yyval.ttype = build_asm_stmt (yyvsp[-6].ttype, yyvsp[-4].ttype, yyvsp[-2].ttype, NULL_TREE, NULL_TREE); ;}
    break;

  case 508:
#line 2268 "c-parse.y"
    { stmt_count++;
		  yyval.ttype = build_asm_stmt (yyvsp[-8].ttype, yyvsp[-6].ttype, yyvsp[-4].ttype, yyvsp[-2].ttype, NULL_TREE); ;}
    break;

  case 509:
#line 2273 "c-parse.y"
    { stmt_count++;
		  yyval.ttype = build_asm_stmt (yyvsp[-10].ttype, yyvsp[-8].ttype, yyvsp[-6].ttype, yyvsp[-4].ttype, yyvsp[-2].ttype); ;}
    break;

  case 510:
#line 2276 "c-parse.y"
    { tree decl;
		  stmt_count++;
		  decl = lookup_label (yyvsp[-1].ttype);
		  if (decl != 0)
		    {
		      TREE_USED (decl) = 1;
		      yyval.ttype = add_stmt (build_stmt (GOTO_STMT, decl));
		    }
		  else
		    yyval.ttype = NULL_TREE;
		;}
    break;

  case 511:
#line 2288 "c-parse.y"
    { if (pedantic)
		    pedwarn ("ISO C forbids `goto *expr;'");
		  stmt_count++;
		  yyvsp[-1].ttype = convert (ptr_type_node, yyvsp[-1].ttype);
		  yyval.ttype = add_stmt (build_stmt (GOTO_STMT, yyvsp[-1].ttype)); ;}
    break;

  case 512:
#line 2294 "c-parse.y"
    { yyval.ttype = NULL_TREE; ;}
    break;

  case 513:
#line 2302 "c-parse.y"
    { stmt_count++;
		  yyval.ttype = do_case (yyvsp[-1].ttype, NULL_TREE); ;}
    break;

  case 514:
#line 2305 "c-parse.y"
    { stmt_count++;
		  yyval.ttype = do_case (yyvsp[-3].ttype, yyvsp[-1].ttype); ;}
    break;

  case 515:
#line 2308 "c-parse.y"
    { stmt_count++;
		  yyval.ttype = do_case (NULL_TREE, NULL_TREE); ;}
    break;

  case 516:
#line 2311 "c-parse.y"
    { tree label = define_label (yyvsp[-3].filename, yyvsp[-2].lineno, yyvsp[-4].ttype);
		  stmt_count++;
		  if (label)
		    {
		      decl_attributes (&label, yyvsp[0].ttype, 0);
		      yyval.ttype = add_stmt (build_stmt (LABEL_STMT, label));
		    }
		  else
		    yyval.ttype = NULL_TREE;
		;}
    break;

  case 517:
#line 2327 "c-parse.y"
    { emit_line_note (input_filename, lineno);
		  yyval.ttype = NULL_TREE; ;}
    break;

  case 518:
#line 2330 "c-parse.y"
    { emit_line_note (input_filename, lineno); ;}
    break;

  case 519:
#line 2335 "c-parse.y"
    { yyval.ttype = NULL_TREE; ;}
    break;

  case 521:
#line 2342 "c-parse.y"
    { yyval.ttype = NULL_TREE; ;}
    break;

  case 524:
#line 2349 "c-parse.y"
    { yyval.ttype = chainon (yyvsp[-2].ttype, yyvsp[0].ttype); ;}
    break;

  case 525:
#line 2354 "c-parse.y"
    { yyval.ttype = build_tree_list (build_tree_list (NULL_TREE, yyvsp[-3].ttype), yyvsp[-1].ttype); ;}
    break;

  case 526:
#line 2356 "c-parse.y"
    { yyval.ttype = build_tree_list (build_tree_list (yyvsp[-5].ttype, yyvsp[-3].ttype), yyvsp[-1].ttype); ;}
    break;

  case 527:
#line 2361 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, combine_strings (yyvsp[0].ttype), NULL_TREE); ;}
    break;

  case 528:
#line 2363 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, combine_strings (yyvsp[0].ttype), yyvsp[-2].ttype); ;}
    break;

  case 529:
#line 2373 "c-parse.y"
    { pushlevel (0);
		  clear_parm_order ();
		  declare_parm_level (0); ;}
    break;

  case 530:
#line 2377 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype;
		  parmlist_tags_warning ();
		  poplevel (0, 0, 0); ;}
    break;

  case 532:
#line 2385 "c-parse.y"
    { tree parm;
		  if (pedantic)
		    pedwarn ("ISO C forbids forward parameter declarations");
		  /* Mark the forward decls as such.  */
		  for (parm = getdecls (); parm; parm = TREE_CHAIN (parm))
		    TREE_ASM_WRITTEN (parm) = 1;
		  clear_parm_order (); ;}
    break;

  case 533:
#line 2393 "c-parse.y"
    { /* Dummy action so attributes are in known place
		     on parser stack.  */ ;}
    break;

  case 534:
#line 2396 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype; ;}
    break;

  case 535:
#line 2398 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, NULL_TREE, NULL_TREE); ;}
    break;

  case 536:
#line 2404 "c-parse.y"
    { yyval.ttype = get_parm_info (0); ;}
    break;

  case 537:
#line 2406 "c-parse.y"
    { yyval.ttype = get_parm_info (0);
		  /* Gcc used to allow this as an extension.  However, it does
		     not work for all targets, and thus has been disabled.
		     Also, since func (...) and func () are indistinguishable,
		     it caused problems with the code in expand_builtin which
		     tries to verify that BUILT_IN_NEXT_ARG is being used
		     correctly.  */
		  error ("ISO C requires a named argument before `...'");
		;}
    break;

  case 538:
#line 2416 "c-parse.y"
    { yyval.ttype = get_parm_info (1); ;}
    break;

  case 539:
#line 2418 "c-parse.y"
    { yyval.ttype = get_parm_info (0); ;}
    break;

  case 540:
#line 2423 "c-parse.y"
    { push_parm_decl (yyvsp[0].ttype); ;}
    break;

  case 541:
#line 2425 "c-parse.y"
    { push_parm_decl (yyvsp[0].ttype); ;}
    break;

  case 542:
#line 2432 "c-parse.y"
    { yyval.ttype = build_tree_list (build_tree_list (current_declspecs,
							 yyvsp[-1].ttype),
					chainon (yyvsp[0].ttype, all_prefix_attributes));
		  POP_DECLSPEC_STACK; ;}
    break;

  case 543:
#line 2437 "c-parse.y"
    { yyval.ttype = build_tree_list (build_tree_list (current_declspecs,
							 yyvsp[-1].ttype),
					chainon (yyvsp[0].ttype, all_prefix_attributes)); 
		  POP_DECLSPEC_STACK; ;}
    break;

  case 544:
#line 2442 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype;
		  POP_DECLSPEC_STACK; ;}
    break;

  case 545:
#line 2445 "c-parse.y"
    { yyval.ttype = build_tree_list (build_tree_list (current_declspecs,
							 yyvsp[-1].ttype),
					chainon (yyvsp[0].ttype, all_prefix_attributes));
		  POP_DECLSPEC_STACK; ;}
    break;

  case 546:
#line 2451 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype;
		  POP_DECLSPEC_STACK; ;}
    break;

  case 547:
#line 2459 "c-parse.y"
    { yyval.ttype = build_tree_list (build_tree_list (current_declspecs,
							 yyvsp[-1].ttype),
					chainon (yyvsp[0].ttype, all_prefix_attributes));
		  POP_DECLSPEC_STACK; ;}
    break;

  case 548:
#line 2464 "c-parse.y"
    { yyval.ttype = build_tree_list (build_tree_list (current_declspecs,
							 yyvsp[-1].ttype),
					chainon (yyvsp[0].ttype, all_prefix_attributes)); 
		  POP_DECLSPEC_STACK; ;}
    break;

  case 549:
#line 2469 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype;
		  POP_DECLSPEC_STACK; ;}
    break;

  case 550:
#line 2472 "c-parse.y"
    { yyval.ttype = build_tree_list (build_tree_list (current_declspecs,
							 yyvsp[-1].ttype),
					chainon (yyvsp[0].ttype, all_prefix_attributes));
		  POP_DECLSPEC_STACK; ;}
    break;

  case 551:
#line 2478 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype;
		  POP_DECLSPEC_STACK; ;}
    break;

  case 552:
#line 2484 "c-parse.y"
    { prefix_attributes = chainon (prefix_attributes, yyvsp[-3].ttype);
		  all_prefix_attributes = prefix_attributes; ;}
    break;

  case 553:
#line 2493 "c-parse.y"
    { pushlevel (0);
		  clear_parm_order ();
		  declare_parm_level (1); ;}
    break;

  case 554:
#line 2497 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype;
		  parmlist_tags_warning ();
		  poplevel (0, 0, 0); ;}
    break;

  case 556:
#line 2505 "c-parse.y"
    { tree t;
		  for (t = yyvsp[-1].ttype; t; t = TREE_CHAIN (t))
		    if (TREE_VALUE (t) == NULL_TREE)
		      error ("`...' in old-style identifier list");
		  yyval.ttype = tree_cons (NULL_TREE, NULL_TREE, yyvsp[-1].ttype);

		  /* Make sure we have a parmlist after attributes.  */
		  if (yyvsp[-3].ttype != 0
		      && (TREE_CODE (yyval.ttype) != TREE_LIST
			  || TREE_PURPOSE (yyval.ttype) == 0
			  || TREE_CODE (TREE_PURPOSE (yyval.ttype)) != PARM_DECL))
		    YYERROR1;
		;}
    break;

  case 557:
#line 2523 "c-parse.y"
    { yyval.ttype = build_tree_list (NULL_TREE, yyvsp[0].ttype); ;}
    break;

  case 558:
#line 2525 "c-parse.y"
    { yyval.ttype = chainon (yyvsp[-2].ttype, build_tree_list (NULL_TREE, yyvsp[0].ttype)); ;}
    break;

  case 559:
#line 2531 "c-parse.y"
    { yyval.ttype = build_tree_list (NULL_TREE, yyvsp[0].ttype); ;}
    break;

  case 560:
#line 2533 "c-parse.y"
    { yyval.ttype = chainon (yyvsp[-2].ttype, build_tree_list (NULL_TREE, yyvsp[0].ttype)); ;}
    break;

  case 561:
#line 2538 "c-parse.y"
    { yyval.ttype = SAVE_WARN_FLAGS();
		  pedantic = 0;
		  warn_pointer_arith = 0;
		  warn_traditional = 0; ;}
    break;


    }

/* Line 991 of yacc.c.  */
#line 5246 "c-p10602.c"

  yyvsp -= yylen;
  yyssp -= yylen;


  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (YYPACT_NINF < yyn && yyn < YYLAST)
	{
	  YYSIZE_T yysize = 0;
	  int yytype = YYTRANSLATE (yychar);
	  char *yymsg;
	  int yyx, yycount;

	  yycount = 0;
	  /* Start YYX at -YYN if negative to avoid negative indexes in
	     YYCHECK.  */
	  for (yyx = yyn < 0 ? -yyn : 0;
	       yyx < (int) (sizeof (yytname) / sizeof (char *)); yyx++)
	    if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	      yysize += yystrlen (yytname[yyx]) + 15, yycount++;
	  yysize += yystrlen ("syntax error, unexpected ") + 1;
	  yysize += yystrlen (yytname[yytype]);
	  yymsg = (char *) YYSTACK_ALLOC (yysize);
	  if (yymsg != 0)
	    {
	      char *yyp = yystpcpy (yymsg, "syntax error, unexpected ");
	      yyp = yystpcpy (yyp, yytname[yytype]);

	      if (yycount < 5)
		{
		  yycount = 0;
		  for (yyx = yyn < 0 ? -yyn : 0;
		       yyx < (int) (sizeof (yytname) / sizeof (char *));
		       yyx++)
		    if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
		      {
			const char *yyq = ! yycount ? ", expecting " : " or ";
			yyp = yystpcpy (yyp, yyq);
			yyp = yystpcpy (yyp, yytname[yyx]);
			yycount++;
		      }
		}
	      yyerror (yymsg);
	      YYSTACK_FREE (yymsg);
	    }
	  else
	    yyerror ("syntax error; also virtual memory exhausted");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror ("syntax error");
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      /* Return failure if at end of input.  */
      if (yychar == YYEOF)
        {
	  /* Pop the error token.  */
          YYPOPSTACK;
	  /* Pop the rest of the stack.  */
	  while (yyss < yyssp)
	    {
	      YYDSYMPRINTF ("Error: popping", yystos[*yyssp], yyvsp, yylsp);
	      yydestruct (yystos[*yyssp], yyvsp);
	      YYPOPSTACK;
	    }
	  YYABORT;
        }

      YYDSYMPRINTF ("Error: discarding", yytoken, &yylval, &yylloc);
      yydestruct (yytoken, &yylval);
      yychar = YYEMPTY;

    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab2;


/*----------------------------------------------------.
| yyerrlab1 -- error raised explicitly by an action.  |
`----------------------------------------------------*/
yyerrlab1:

  /* Suppress GCC warning that yyerrlab1 is unused when no action
     invokes YYERROR.  */
#if defined (__GNUC_MINOR__) && 2093 <= (__GNUC__ * 1000 + __GNUC_MINOR__)
 __attribute__ ((__unused__));
#endif


  goto yyerrlab2;


/*---------------------------------------------------------------.
| yyerrlab2 -- pop states until the error token can be shifted.  |
`---------------------------------------------------------------*/
yyerrlab2:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;

      YYDSYMPRINTF ("Error: popping", yystos[*yyssp], yyvsp, yylsp);
      yydestruct (yystos[yystate], yyvsp);
      yyvsp--;
      yystate = *--yyssp;

      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  YYDPRINTF ((stderr, "Shifting error token, "));

  *++yyvsp = yylval;


  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*----------------------------------------------.
| yyoverflowlab -- parser overflow comes here.  |
`----------------------------------------------*/
yyoverflowlab:
  yyerror ("parser stack overflow");
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  return yyresult;
}


#line 2544 "c-parse.y"


/* yylex() is a thin wrapper around c_lex(), all it does is translate
   cpplib.h's token codes into yacc's token codes.  */

static enum cpp_ttype last_token;

/* The reserved keyword table.  */
struct resword
{
  const char *word;
  ENUM_BITFIELD(rid) rid : 16;
  unsigned int disable   : 16;
};

/* Disable mask.  Keywords are disabled if (reswords[i].disable & mask) is
   _true_.  */
#define D_TRAD	0x01	/* not in traditional C */
#define D_C89	0x02	/* not in C89 */
#define D_EXT	0x04	/* GCC extension */
#define D_EXT89	0x08	/* GCC extension incorporated in C99 */
#define D_OBJC	0x10	/* Objective C only */

static const struct resword reswords[] =
{
  { "_Bool",		RID_BOOL,	0 },
  { "_Complex",		RID_COMPLEX,	0 },
  { "__FUNCTION__",	RID_FUNCTION_NAME, 0 },
  { "__PRETTY_FUNCTION__", RID_PRETTY_FUNCTION_NAME, 0 },
  { "__alignof",	RID_ALIGNOF,	0 },
  { "__alignof__",	RID_ALIGNOF,	0 },
  { "__asm",		RID_ASM,	0 },
  { "__asm__",		RID_ASM,	0 },
  { "__attribute",	RID_ATTRIBUTE,	0 },
  { "__attribute__",	RID_ATTRIBUTE,	0 },
  { "__bounded",	RID_BOUNDED,	0 },
  { "__bounded__",	RID_BOUNDED,	0 },
  { "__builtin_choose_expr", RID_CHOOSE_EXPR, 0 },
  { "__builtin_types_compatible_p", RID_TYPES_COMPATIBLE_P, 0 },
  { "__builtin_va_arg",	RID_VA_ARG,	0 },
  { "__complex",	RID_COMPLEX,	0 },
  { "__complex__",	RID_COMPLEX,	0 },
  { "__const",		RID_CONST,	0 },
  { "__const__",	RID_CONST,	0 },
  { "__extension__",	RID_EXTENSION,	0 },
  { "__func__",		RID_C99_FUNCTION_NAME, 0 },
  { "__imag",		RID_IMAGPART,	0 },
  { "__imag__",		RID_IMAGPART,	0 },
  { "__inline",		RID_INLINE,	0 },
  { "__inline__",	RID_INLINE,	0 },
  { "__label__",	RID_LABEL,	0 },
  { "__ptrbase",	RID_PTRBASE,	0 },
  { "__ptrbase__",	RID_PTRBASE,	0 },
  { "__ptrextent",	RID_PTREXTENT,	0 },
  { "__ptrextent__",	RID_PTREXTENT,	0 },
  { "__ptrvalue",	RID_PTRVALUE,	0 },
  { "__ptrvalue__",	RID_PTRVALUE,	0 },
  { "__real",		RID_REALPART,	0 },
  { "__real__",		RID_REALPART,	0 },
  { "__restrict",	RID_RESTRICT,	0 },
  { "__restrict__",	RID_RESTRICT,	0 },
  { "__signed",		RID_SIGNED,	0 },
  { "__signed__",	RID_SIGNED,	0 },
  { "__typeof",		RID_TYPEOF,	0 },
  { "__typeof__",	RID_TYPEOF,	0 },
  { "__unbounded",	RID_UNBOUNDED,	0 },
  { "__unbounded__",	RID_UNBOUNDED,	0 },
  { "__volatile",	RID_VOLATILE,	0 },
  { "__volatile__",	RID_VOLATILE,	0 },
  { "asm",		RID_ASM,	D_EXT },
  { "auto",		RID_AUTO,	0 },
  { "break",		RID_BREAK,	0 },
  { "case",		RID_CASE,	0 },
  { "char",		RID_CHAR,	0 },
  { "const",		RID_CONST,	D_TRAD },
  { "continue",		RID_CONTINUE,	0 },
  { "default",		RID_DEFAULT,	0 },
  { "do",		RID_DO,		0 },
  { "double",		RID_DOUBLE,	0 },
  { "else",		RID_ELSE,	0 },
  { "enum",		RID_ENUM,	0 },
  { "extern",		RID_EXTERN,	0 },
  { "float",		RID_FLOAT,	0 },
  { "for",		RID_FOR,	0 },
  { "goto",		RID_GOTO,	0 },
  { "if",		RID_IF,		0 },
  { "inline",		RID_INLINE,	D_TRAD|D_EXT89 },
  { "int",		RID_INT,	0 },
  { "long",		RID_LONG,	0 },
  { "register",		RID_REGISTER,	0 },
  { "restrict",		RID_RESTRICT,	D_TRAD|D_C89 },
  { "return",		RID_RETURN,	0 },
  { "short",		RID_SHORT,	0 },
  { "signed",		RID_SIGNED,	D_TRAD },
  { "sizeof",		RID_SIZEOF,	0 },
  { "static",		RID_STATIC,	0 },
  { "struct",		RID_STRUCT,	0 },
  { "switch",		RID_SWITCH,	0 },
  { "typedef",		RID_TYPEDEF,	0 },
  { "typeof",		RID_TYPEOF,	D_TRAD|D_EXT },
  { "union",		RID_UNION,	0 },
  { "unsigned",		RID_UNSIGNED,	0 },
  { "void",		RID_VOID,	0 },
  { "volatile",		RID_VOLATILE,	D_TRAD },
  { "while",		RID_WHILE,	0 },
};
#define N_reswords (sizeof reswords / sizeof (struct resword))

/* Table mapping from RID_* constants to yacc token numbers.
   Unfortunately we have to have entries for all the keywords in all
   three languages.  */
static const short rid_to_yy[RID_MAX] =
{
  /* RID_STATIC */	SCSPEC,
  /* RID_UNSIGNED */	TYPESPEC,
  /* RID_LONG */	TYPESPEC,
  /* RID_CONST */	TYPE_QUAL,
  /* RID_EXTERN */	SCSPEC,
  /* RID_REGISTER */	SCSPEC,
  /* RID_TYPEDEF */	SCSPEC,
  /* RID_SHORT */	TYPESPEC,
  /* RID_INLINE */	SCSPEC,
  /* RID_VOLATILE */	TYPE_QUAL,
  /* RID_SIGNED */	TYPESPEC,
  /* RID_AUTO */	SCSPEC,
  /* RID_RESTRICT */	TYPE_QUAL,

  /* C extensions */
  /* RID_BOUNDED */	TYPE_QUAL,
  /* RID_UNBOUNDED */	TYPE_QUAL,
  /* RID_COMPLEX */	TYPESPEC,

  /* C++ */
  /* RID_FRIEND */	0,
  /* RID_VIRTUAL */	0,
  /* RID_EXPLICIT */	0,
  /* RID_EXPORT */	0,
  /* RID_MUTABLE */	0,

  /* ObjC */
  /* RID_IN */		TYPE_QUAL,
  /* RID_OUT */		TYPE_QUAL,
  /* RID_INOUT */	TYPE_QUAL,
  /* RID_BYCOPY */	TYPE_QUAL,
  /* RID_BYREF */	TYPE_QUAL,
  /* RID_ONEWAY */	TYPE_QUAL,
  
  /* C */
  /* RID_INT */		TYPESPEC,
  /* RID_CHAR */	TYPESPEC,
  /* RID_FLOAT */	TYPESPEC,
  /* RID_DOUBLE */	TYPESPEC,
  /* RID_VOID */	TYPESPEC,
  /* RID_ENUM */	ENUM,
  /* RID_STRUCT */	STRUCT,
  /* RID_UNION */	UNION,
  /* RID_IF */		IF,
  /* RID_ELSE */	ELSE,
  /* RID_WHILE */	WHILE,
  /* RID_DO */		DO,
  /* RID_FOR */		FOR,
  /* RID_SWITCH */	SWITCH,
  /* RID_CASE */	CASE,
  /* RID_DEFAULT */	DEFAULT,
  /* RID_BREAK */	BREAK,
  /* RID_CONTINUE */	CONTINUE,
  /* RID_RETURN */	RETURN,
  /* RID_GOTO */	GOTO,
  /* RID_SIZEOF */	SIZEOF,

  /* C extensions */
  /* RID_ASM */		ASM_KEYWORD,
  /* RID_TYPEOF */	TYPEOF,
  /* RID_ALIGNOF */	ALIGNOF,
  /* RID_ATTRIBUTE */	ATTRIBUTE,
  /* RID_VA_ARG */	VA_ARG,
  /* RID_EXTENSION */	EXTENSION,
  /* RID_IMAGPART */	IMAGPART,
  /* RID_REALPART */	REALPART,
  /* RID_LABEL */	LABEL,
  /* RID_PTRBASE */	PTR_BASE,
  /* RID_PTREXTENT */	PTR_EXTENT,
  /* RID_PTRVALUE */	PTR_VALUE,

  /* RID_CHOOSE_EXPR */			CHOOSE_EXPR,
  /* RID_TYPES_COMPATIBLE_P */		TYPES_COMPATIBLE_P,

  /* RID_FUNCTION_NAME */		STRING_FUNC_NAME,
  /* RID_PRETTY_FUNCTION_NAME */	STRING_FUNC_NAME,
  /* RID_C99_FUNCTION_NAME */		VAR_FUNC_NAME,

  /* C++ */
  /* RID_BOOL */	TYPESPEC,
  /* RID_WCHAR */	0,
  /* RID_CLASS */	0,
  /* RID_PUBLIC */	0,
  /* RID_PRIVATE */	0,
  /* RID_PROTECTED */	0,
  /* RID_TEMPLATE */	0,
  /* RID_NULL */	0,
  /* RID_CATCH */	0,
  /* RID_DELETE */	0,
  /* RID_FALSE */	0,
  /* RID_NAMESPACE */	0,
  /* RID_NEW */		0,
  /* RID_OPERATOR */	0,
  /* RID_THIS */	0,
  /* RID_THROW */	0,
  /* RID_TRUE */	0,
  /* RID_TRY */		0,
  /* RID_TYPENAME */	0,
  /* RID_TYPEID */	0,
  /* RID_USING */	0,

  /* casts */
  /* RID_CONSTCAST */	0,
  /* RID_DYNCAST */	0,
  /* RID_REINTCAST */	0,
  /* RID_STATCAST */	0,

  /* alternate spellings */
  /* RID_AND */		0,
  /* RID_AND_EQ */	0,
  /* RID_NOT */		0,
  /* RID_NOT_EQ */	0,
  /* RID_OR */		0,
  /* RID_OR_EQ */	0,
  /* RID_XOR */		0,
  /* RID_XOR_EQ */	0,
  /* RID_BITAND */	0,
  /* RID_BITOR */	0,
  /* RID_COMPL */	0,
  
  /* Objective C */
  /* RID_ID */			OBJECTNAME,
  /* RID_AT_ENCODE */		ENCODE,
  /* RID_AT_END */		END,
  /* RID_AT_CLASS */		CLASS,
  /* RID_AT_ALIAS */		ALIAS,
  /* RID_AT_DEFS */		DEFS,
  /* RID_AT_PRIVATE */		PRIVATE,
  /* RID_AT_PROTECTED */	PROTECTED,
  /* RID_AT_PUBLIC */		PUBLIC,
  /* RID_AT_PROTOCOL */		PROTOCOL,
  /* RID_AT_SELECTOR */		SELECTOR,
  /* RID_AT_INTERFACE */	INTERFACE,
  /* RID_AT_IMPLEMENTATION */	IMPLEMENTATION
};

static void
init_reswords ()
{
  unsigned int i;
  tree id;
  int mask = (flag_isoc99 ? 0 : D_C89)
	      | (flag_traditional ? D_TRAD : 0)
	      | (flag_no_asm ? (flag_isoc99 ? D_EXT : D_EXT|D_EXT89) : 0);

  if (c_language != clk_objective_c)
     mask |= D_OBJC;

  /* It is not necessary to register ridpointers as a GC root, because
     all the trees it points to are permanently interned in the
     get_identifier hash anyway.  */
  ridpointers = (tree *) xcalloc ((int) RID_MAX, sizeof (tree));
  for (i = 0; i < N_reswords; i++)
    {
      /* If a keyword is disabled, do not enter it into the table
	 and so create a canonical spelling that isn't a keyword.  */
      if (reswords[i].disable & mask)
	continue;

      id = get_identifier (reswords[i].word);
      C_RID_CODE (id) = reswords[i].rid;
      C_IS_RESERVED_WORD (id) = 1;
      ridpointers [(int) reswords[i].rid] = id;
    }
}

#define NAME(type) cpp_type2name (type)

static void
yyerror (msgid)
     const char *msgid;
{
  const char *string = _(msgid);

  if (last_token == CPP_EOF)
    error ("%s at end of input", string);
  else if (last_token == CPP_CHAR || last_token == CPP_WCHAR)
    {
      unsigned int val = TREE_INT_CST_LOW (yylval.ttype);
      const char *const ell = (last_token == CPP_CHAR) ? "" : "L";
      if (val <= UCHAR_MAX && ISGRAPH (val))
	error ("%s before %s'%c'", string, ell, val);
      else
	error ("%s before %s'\\x%x'", string, ell, val);
    }
  else if (last_token == CPP_STRING
	   || last_token == CPP_WSTRING)
    error ("%s before string constant", string);
  else if (last_token == CPP_NUMBER)
    error ("%s before numeric constant", string);
  else if (last_token == CPP_NAME)
    error ("%s before \"%s\"", string, IDENTIFIER_POINTER (yylval.ttype));
  else
    error ("%s before '%s' token", string, NAME(last_token));
}

static int
yylexname ()
{
  tree decl;
  
  
  if (C_IS_RESERVED_WORD (yylval.ttype))
    {
      enum rid rid_code = C_RID_CODE (yylval.ttype);

      {
	int yycode = rid_to_yy[(int) rid_code];
	if (yycode == STRING_FUNC_NAME)
	  {
	    /* __FUNCTION__ and __PRETTY_FUNCTION__ get converted
	       to string constants.  */
	    const char *name = fname_string (rid_code);
	  
	    yylval.ttype = build_string (strlen (name) + 1, name);
	    C_ARTIFICIAL_STRING_P (yylval.ttype) = 1;
	    last_token = CPP_STRING;  /* so yyerror won't choke */
	    return STRING;
	  }
      
	/* Return the canonical spelling for this keyword.  */
	yylval.ttype = ridpointers[(int) rid_code];
	return yycode;
      }
    }

  decl = lookup_name (yylval.ttype);
  if (decl)
    {
      if (TREE_CODE (decl) == TYPE_DECL)
	return TYPENAME;
    }

  return IDENTIFIER;
}


static inline int
_yylex ()
{
 get_next:
  last_token = c_lex (&yylval.ttype);
  switch (last_token)
    {
    case CPP_EQ:					return '=';
    case CPP_NOT:					return '!';
    case CPP_GREATER:	yylval.code = GT_EXPR;		return ARITHCOMPARE;
    case CPP_LESS:	yylval.code = LT_EXPR;		return ARITHCOMPARE;
    case CPP_PLUS:	yylval.code = PLUS_EXPR;	return '+';
    case CPP_MINUS:	yylval.code = MINUS_EXPR;	return '-';
    case CPP_MULT:	yylval.code = MULT_EXPR;	return '*';
    case CPP_DIV:	yylval.code = TRUNC_DIV_EXPR;	return '/';
    case CPP_MOD:	yylval.code = TRUNC_MOD_EXPR;	return '%';
    case CPP_AND:	yylval.code = BIT_AND_EXPR;	return '&';
    case CPP_OR:	yylval.code = BIT_IOR_EXPR;	return '|';
    case CPP_XOR:	yylval.code = BIT_XOR_EXPR;	return '^';
    case CPP_RSHIFT:	yylval.code = RSHIFT_EXPR;	return RSHIFT;
    case CPP_LSHIFT:	yylval.code = LSHIFT_EXPR;	return LSHIFT;

    case CPP_COMPL:					return '~';
    case CPP_AND_AND:					return ANDAND;
    case CPP_OR_OR:					return OROR;
    case CPP_QUERY:					return '?';
    case CPP_OPEN_PAREN:				return '(';
    case CPP_EQ_EQ:	yylval.code = EQ_EXPR;		return EQCOMPARE;
    case CPP_NOT_EQ:	yylval.code = NE_EXPR;		return EQCOMPARE;
    case CPP_GREATER_EQ:yylval.code = GE_EXPR;		return ARITHCOMPARE;
    case CPP_LESS_EQ:	yylval.code = LE_EXPR;		return ARITHCOMPARE;

    case CPP_PLUS_EQ:	yylval.code = PLUS_EXPR;	return ASSIGN;
    case CPP_MINUS_EQ:	yylval.code = MINUS_EXPR;	return ASSIGN;
    case CPP_MULT_EQ:	yylval.code = MULT_EXPR;	return ASSIGN;
    case CPP_DIV_EQ:	yylval.code = TRUNC_DIV_EXPR;	return ASSIGN;
    case CPP_MOD_EQ:	yylval.code = TRUNC_MOD_EXPR;	return ASSIGN;
    case CPP_AND_EQ:	yylval.code = BIT_AND_EXPR;	return ASSIGN;
    case CPP_OR_EQ:	yylval.code = BIT_IOR_EXPR;	return ASSIGN;
    case CPP_XOR_EQ:	yylval.code = BIT_XOR_EXPR;	return ASSIGN;
    case CPP_RSHIFT_EQ:	yylval.code = RSHIFT_EXPR;	return ASSIGN;
    case CPP_LSHIFT_EQ:	yylval.code = LSHIFT_EXPR;	return ASSIGN;

    case CPP_OPEN_SQUARE:				return '[';
    case CPP_CLOSE_SQUARE:				return ']';
    case CPP_OPEN_BRACE:				return '{';
    case CPP_CLOSE_BRACE:				return '}';
    case CPP_ELLIPSIS:					return ELLIPSIS;

    case CPP_PLUS_PLUS:					return PLUSPLUS;
    case CPP_MINUS_MINUS:				return MINUSMINUS;
    case CPP_DEREF:					return POINTSAT;
    case CPP_DOT:					return '.';

      /* The following tokens may affect the interpretation of any
	 identifiers following, if doing Objective-C.  */
    case CPP_COLON:		OBJC_NEED_RAW_IDENTIFIER (0);	return ':';
    case CPP_COMMA:		OBJC_NEED_RAW_IDENTIFIER (0);	return ',';
    case CPP_CLOSE_PAREN:	OBJC_NEED_RAW_IDENTIFIER (0);	return ')';
    case CPP_SEMICOLON:		OBJC_NEED_RAW_IDENTIFIER (0);	return ';';

    case CPP_EOF:
      return 0;

    case CPP_NAME:
      return yylexname ();

    case CPP_NUMBER:
    case CPP_CHAR:
    case CPP_WCHAR:
      return CONSTANT;

    case CPP_STRING:
    case CPP_WSTRING:
      return STRING;
      
      /* This token is Objective-C specific.  It gives the next token
	 special significance.  */
    case CPP_ATSIGN:

      /* These tokens are C++ specific (and will not be generated
         in C mode, but let's be cautious).  */
    case CPP_SCOPE:
    case CPP_DEREF_STAR:
    case CPP_DOT_STAR:
    case CPP_MIN_EQ:
    case CPP_MAX_EQ:
    case CPP_MIN:
    case CPP_MAX:
      /* These tokens should not survive translation phase 4.  */
    case CPP_HASH:
    case CPP_PASTE:
      error ("syntax error at '%s' token", NAME(last_token));
      goto get_next;

    default:
      abort ();
    }
  /* NOTREACHED */
}

static int
yylex()
{
  int r;
  timevar_push (TV_LEX);
  r = _yylex();
  timevar_pop (TV_LEX);
  return r;
}

/* Sets the value of the 'yydebug' variable to VALUE.
   This is a function so we don't have to have YYDEBUG defined
   in order to build the compiler.  */

void
c_set_yydebug (value)
     int value;
{
#if YYDEBUG != 0
  yydebug = value;
#else
  warning ("YYDEBUG not defined");
#endif
}

/* Function used when yydebug is set, to print a token in more detail.  */

static void
yyprint (file, yychar, yyl)
     FILE *file;
     int yychar;
     YYSTYPE yyl;
{
  tree t = yyl.ttype;

  fprintf (file, " [%s]", NAME(last_token));
  
  switch (yychar)
    {
    case IDENTIFIER:
    case TYPENAME:
    case OBJECTNAME:
    case TYPESPEC:
    case TYPE_QUAL:
    case SCSPEC:
      if (IDENTIFIER_POINTER (t))
	fprintf (file, " `%s'", IDENTIFIER_POINTER (t));
      break;

    case CONSTANT:
      fprintf (file, " %s", GET_MODE_NAME (TYPE_MODE (TREE_TYPE (t))));
      if (TREE_CODE (t) == INTEGER_CST)
	fprintf (file,
#if HOST_BITS_PER_WIDE_INT == 64
#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
		 " 0x%x%016x",
#else
#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_LONG
		 " 0x%lx%016lx",
#else
		 " 0x%llx%016llx",
#endif
#endif
#else
#if HOST_BITS_PER_WIDE_INT != HOST_BITS_PER_INT
		 " 0x%lx%08lx",
#else
		 " 0x%x%08x",
#endif
#endif
		 TREE_INT_CST_HIGH (t), TREE_INT_CST_LOW (t));
      break;
    }
}

/* This is not the ideal place to put these, but we have to get them out
   of c-lex.c because cp/lex.c has its own versions.  */

/* Return something to represent absolute declarators containing a *.
   TARGET is the absolute declarator that the * contains.
   TYPE_QUALS_ATTRS is a list of modifiers such as const or volatile
   to apply to the pointer type, represented as identifiers, possible mixed
   with attributes.

   We return an INDIRECT_REF whose "contents" are TARGET (inside a TREE_LIST,
   if attributes are present) and whose type is the modifier list.  */

tree
make_pointer_declarator (type_quals_attrs, target)
     tree type_quals_attrs, target;
{
  tree quals, attrs;
  tree itarget = target;
  split_specs_attrs (type_quals_attrs, &quals, &attrs);
  if (attrs != NULL_TREE)
    itarget = tree_cons (attrs, target, NULL_TREE);
  return build1 (INDIRECT_REF, quals, itarget);
}


