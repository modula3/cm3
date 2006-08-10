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
     STATIC = 261,
     TYPESPEC = 262,
     TYPE_QUAL = 263,
     CONSTANT = 264,
     STRING = 265,
     ELLIPSIS = 266,
     SIZEOF = 267,
     ENUM = 268,
     STRUCT = 269,
     UNION = 270,
     IF = 271,
     ELSE = 272,
     WHILE = 273,
     DO = 274,
     FOR = 275,
     SWITCH = 276,
     CASE = 277,
     DEFAULT = 278,
     BREAK = 279,
     CONTINUE = 280,
     RETURN = 281,
     GOTO = 282,
     ASM_KEYWORD = 283,
     TYPEOF = 284,
     ALIGNOF = 285,
     ATTRIBUTE = 286,
     EXTENSION = 287,
     LABEL = 288,
     REALPART = 289,
     IMAGPART = 290,
     VA_ARG = 291,
     CHOOSE_EXPR = 292,
     TYPES_COMPATIBLE_P = 293,
     PTR_VALUE = 294,
     PTR_BASE = 295,
     PTR_EXTENT = 296,
     FUNC_NAME = 297,
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
     ALIAS = 323,
     AT_THROW = 324,
     AT_TRY = 325,
     AT_CATCH = 326,
     AT_FINALLY = 327,
     AT_SYNCHRONIZED = 328,
     OBJC_STRING = 329
   };
#endif
#define IDENTIFIER 258
#define TYPENAME 259
#define SCSPEC 260
#define STATIC 261
#define TYPESPEC 262
#define TYPE_QUAL 263
#define CONSTANT 264
#define STRING 265
#define ELLIPSIS 266
#define SIZEOF 267
#define ENUM 268
#define STRUCT 269
#define UNION 270
#define IF 271
#define ELSE 272
#define WHILE 273
#define DO 274
#define FOR 275
#define SWITCH 276
#define CASE 277
#define DEFAULT 278
#define BREAK 279
#define CONTINUE 280
#define RETURN 281
#define GOTO 282
#define ASM_KEYWORD 283
#define TYPEOF 284
#define ALIGNOF 285
#define ATTRIBUTE 286
#define EXTENSION 287
#define LABEL 288
#define REALPART 289
#define IMAGPART 290
#define VA_ARG 291
#define CHOOSE_EXPR 292
#define TYPES_COMPATIBLE_P 293
#define PTR_VALUE 294
#define PTR_BASE 295
#define PTR_EXTENT 296
#define FUNC_NAME 297
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
#define AT_THROW 324
#define AT_TRY 325
#define AT_CATCH 326
#define AT_FINALLY 327
#define AT_SYNCHRONIZED 328
#define OBJC_STRING 329




/* Copy the first part of user declarations.  */
#line 34 "c-parse.y"

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "input.h"
#include "cpplib.h"
#include "intl.h"
#include "timevar.h"
#include "c-pragma.h"		/* For YYDEBUG definition, and parse_in.  */
#include "c-tree.h"
#include "flags.h"
#include "varray.h"
#include "output.h"
#include "toplev.h"
#include "ggc.h"


/* Like YYERROR but do call yyerror.  */
#define YYERROR1 { yyerror ("syntax error"); YYERROR; }

/* Like the default stack expander, except (1) use realloc when possible,
   (2) impose no hard maxiumum on stack size, (3) REALLY do not use alloca.

   Irritatingly, YYSTYPE is defined after this %{ %} block, so we cannot
   give malloced_yyvs its proper type.  This is ok since all we need from
   it is to be able to free it.  */

static short *malloced_yyss;
static void *malloced_yyvs;

#define yyoverflow(MSG, SS, SSSIZE, VS, VSSIZE, YYSSZ)			\
do {									\
  size_t newsize;							\
  short *newss;								\
  YYSTYPE *newvs;							\
  newsize = *(YYSSZ) *= 2;						\
  if (malloced_yyss)							\
    {									\
      newss = really_call_realloc (*(SS), newsize * sizeof (short));	\
      newvs = really_call_realloc (*(VS), newsize * sizeof (YYSTYPE));	\
    }									\
  else									\
    {									\
      newss = really_call_malloc (newsize * sizeof (short));		\
      newvs = really_call_malloc (newsize * sizeof (YYSTYPE));		\
      if (newss)							\
        memcpy (newss, *(SS), (SSSIZE));				\
      if (newvs)							\
        memcpy (newvs, *(VS), (VSSIZE));				\
    }									\
  if (!newss || !newvs)							\
    {									\
      yyerror (MSG);							\
      return 2;								\
    }									\
  *(SS) = newss;							\
  *(VS) = newvs;							\
  malloced_yyss = newss;						\
  malloced_yyvs = (void *) newvs;					\
} while (0)


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
#line 100 "c-parse.y"
typedef union YYSTYPE {long itype; tree ttype; enum tree_code code;
	location_t location; } YYSTYPE;
/* Line 191 of yacc.c.  */
#line 290 "c-parse.c"
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */
#line 234 "c-parse.y"

/* Number of statements (loosely speaking) and compound statements
   seen so far.  */
static int stmt_count;
static int compstmt_count;

/* Input location of the end of the body of last simple_if;
   used by the stmt-rule immediately after simple_if returns.  */
static location_t if_stmt_locus;


/* List of types and structure classes of the current declaration.  */
static GTY(()) tree current_declspecs;
static GTY(()) tree prefix_attributes;

/* List of all the attributes applying to the identifier currently being
   declared; includes prefix_attributes and possibly some more attributes
   just after a comma.  */
static GTY(()) tree all_prefix_attributes;

/* Stack of saved values of current_declspecs, prefix_attributes and
   all_prefix_attributes.  */
static GTY(()) tree declspec_stack;

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
#define SAVE_EXT_FLAGS()		\
	(pedantic			\
	 | (warn_pointer_arith << 1)	\
	 | (warn_traditional << 2)	\
	 | (flag_iso << 3))

#define RESTORE_EXT_FLAGS(val)			\
  do {						\
    pedantic = val & 1;				\
    warn_pointer_arith = (val >> 1) & 1;	\
    warn_traditional = (val >> 2) & 1;		\
    flag_iso = (val >> 3) & 1;			\
  } while (0)


#define OBJC_NEED_RAW_IDENTIFIER(VAL)	/* nothing */

static bool parsing_iso_function_signature;

/* Tell yyparse how to print a token's value, if yydebug is set.  */

#define YYPRINT(FILE,YYCHAR,YYLVAL) yyprint(FILE,YYCHAR,YYLVAL)

static void yyprint (FILE *, int, YYSTYPE);
static void yyerror (const char *);
static int yylexname (void);
static inline int _yylex (void);
static int  yylex (void);
static void init_reswords (void);

  /* Initialisation routine for this file.  */
void
c_parse_init (void)
{
  init_reswords ();
}



/* Line 214 of yacc.c.  */
#line 384 "c-parse.c"

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
#define YYLAST   3335

/* YYNTOKENS -- Number of terminals. */
#define YYNTOKENS  97
/* YYNNTS -- Number of nonterminals. */
#define YYNNTS  201
/* YYNRULES -- Number of rules. */
#define YYNRULES  557
/* YYNRULES -- Number of states. */
#define YYNSTATES  881

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   329

#define YYTRANSLATE(YYX) 						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const unsigned char yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    93,     2,     2,     2,    60,    51,     2,
      66,    89,    58,    56,    94,    57,    65,    59,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    46,    90,
       2,    43,     2,    45,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    67,     2,    96,    50,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    95,    49,    91,    92,     2,     2,     2,
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
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const unsigned short yyprhs[] =
{
       0,     0,     3,     4,     6,     7,    10,    11,    15,    17,
      19,    21,    27,    30,    34,    39,    44,    47,    50,    53,
      55,    56,    57,    66,    71,    72,    73,    82,    87,    88,
      89,    97,   101,   103,   105,   107,   109,   111,   113,   115,
     117,   119,   121,   122,   124,   126,   130,   132,   135,   138,
     141,   144,   147,   152,   155,   160,   163,   166,   168,   170,
     172,   174,   179,   181,   185,   189,   193,   197,   201,   205,
     209,   213,   217,   221,   225,   229,   230,   235,   236,   241,
     242,   243,   251,   252,   258,   262,   266,   268,   270,   272,
     274,   275,   283,   287,   291,   295,   299,   304,   311,   320,
     327,   332,   336,   340,   343,   346,   348,   349,   351,   354,
     356,   358,   361,   364,   369,   374,   377,   380,   383,   384,
     386,   391,   396,   400,   404,   407,   410,   412,   415,   418,
     421,   424,   427,   429,   432,   434,   437,   440,   443,   446,
     449,   452,   454,   457,   460,   463,   466,   469,   472,   475,
     478,   481,   484,   487,   490,   493,   496,   499,   502,   504,
     507,   510,   513,   516,   519,   522,   525,   528,   531,   534,
     537,   540,   543,   546,   549,   552,   555,   558,   561,   564,
     567,   570,   573,   576,   579,   582,   585,   588,   591,   594,
     597,   600,   603,   606,   609,   612,   615,   618,   621,   624,
     627,   630,   633,   636,   638,   640,   642,   644,   646,   648,
     650,   652,   654,   656,   658,   660,   662,   664,   666,   668,
     670,   672,   674,   676,   678,   680,   682,   684,   686,   688,
     690,   692,   694,   696,   698,   700,   702,   704,   706,   708,
     710,   712,   714,   716,   718,   720,   722,   724,   726,   728,
     730,   732,   734,   736,   738,   740,   742,   744,   746,   748,
     749,   751,   753,   755,   757,   759,   761,   763,   765,   770,
     775,   777,   782,   784,   789,   790,   795,   796,   803,   807,
     808,   815,   819,   820,   822,   824,   827,   834,   836,   840,
     841,   843,   848,   855,   860,   862,   864,   866,   868,   870,
     872,   874,   875,   880,   882,   883,   886,   888,   892,   896,
     899,   900,   905,   907,   908,   913,   915,   917,   919,   922,
     925,   931,   935,   936,   937,   944,   945,   946,   953,   955,
     957,   962,   966,   969,   973,   975,   977,   979,   983,   986,
     988,   992,   995,   999,  1003,  1008,  1012,  1017,  1021,  1024,
    1026,  1028,  1031,  1033,  1036,  1038,  1041,  1042,  1050,  1056,
    1057,  1065,  1071,  1072,  1081,  1082,  1090,  1093,  1096,  1099,
    1100,  1102,  1103,  1105,  1107,  1110,  1111,  1115,  1118,  1122,
    1125,  1129,  1131,  1133,  1136,  1138,  1143,  1145,  1150,  1153,
    1158,  1162,  1165,  1170,  1174,  1176,  1180,  1182,  1184,  1188,
    1189,  1193,  1194,  1196,  1197,  1199,  1202,  1204,  1206,  1208,
    1212,  1215,  1219,  1224,  1228,  1231,  1234,  1236,  1241,  1245,
    1250,  1256,  1262,  1264,  1266,  1268,  1270,  1272,  1275,  1278,
    1281,  1284,  1286,  1289,  1292,  1295,  1297,  1300,  1303,  1306,
    1309,  1311,  1314,  1316,  1318,  1320,  1322,  1325,  1326,  1327,
    1328,  1329,  1330,  1332,  1334,  1337,  1341,  1343,  1346,  1348,
    1350,  1356,  1358,  1360,  1363,  1366,  1369,  1372,  1373,  1379,
    1380,  1385,  1386,  1388,  1391,  1395,  1398,  1401,  1402,  1407,
    1409,  1413,  1414,  1415,  1423,  1429,  1432,  1433,  1434,  1435,
    1436,  1449,  1450,  1457,  1460,  1462,  1464,  1467,  1471,  1474,
    1477,  1480,  1484,  1491,  1500,  1511,  1524,  1528,  1533,  1535,
    1539,  1545,  1548,  1553,  1554,  1556,  1557,  1559,  1560,  1562,
    1564,  1568,  1573,  1581,  1583,  1587,  1588,  1592,  1595,  1596,
    1597,  1604,  1607,  1608,  1610,  1612,  1616,  1618,  1622,  1627,
    1632,  1636,  1641,  1645,  1650,  1655,  1659,  1664,  1668,  1670,
    1671,  1675,  1677,  1680,  1682,  1686,  1688,  1692
};

/* YYRHS -- A `-1'-separated list of the rules' RHS. */
static const short yyrhs[] =
{
      98,     0,    -1,    -1,    99,    -1,    -1,   100,   102,    -1,
      -1,    99,   101,   102,    -1,   103,    -1,   105,    -1,   104,
      -1,    28,    66,   114,    89,    90,    -1,   297,   102,    -1,
     136,   170,    90,    -1,   156,   136,   170,    90,    -1,   155,
     136,   169,    90,    -1,   162,    90,    -1,     1,    90,    -1,
       1,    91,    -1,    90,    -1,    -1,    -1,   155,   136,   199,
     106,   130,   259,   107,   248,    -1,   155,   136,   199,     1,
      -1,    -1,    -1,   156,   136,   204,   108,   130,   259,   109,
     248,    -1,   156,   136,   204,     1,    -1,    -1,    -1,   136,
     204,   110,   130,   259,   111,   248,    -1,   136,   204,     1,
      -1,     3,    -1,     4,    -1,    51,    -1,    57,    -1,    56,
      -1,    62,    -1,    61,    -1,    92,    -1,    93,    -1,   116,
      -1,    -1,   116,    -1,   122,    -1,   116,    94,   122,    -1,
     128,    -1,    58,   121,    -1,   297,   121,    -1,   113,   121,
      -1,    48,   112,    -1,   118,   117,    -1,   118,    66,   225,
      89,    -1,   119,   117,    -1,   119,    66,   225,    89,    -1,
      34,   121,    -1,    35,   121,    -1,    12,    -1,    30,    -1,
      29,    -1,   117,    -1,    66,   225,    89,   121,    -1,   121,
      -1,   122,    56,   122,    -1,   122,    57,   122,    -1,   122,
      58,   122,    -1,   122,    59,   122,    -1,   122,    60,   122,
      -1,   122,    55,   122,    -1,   122,    54,   122,    -1,   122,
      53,   122,    -1,   122,    52,   122,    -1,   122,    51,   122,
      -1,   122,    49,   122,    -1,   122,    50,   122,    -1,    -1,
     122,    48,   123,   122,    -1,    -1,   122,    47,   124,   122,
      -1,    -1,    -1,   122,    45,   125,   114,    46,   126,   122,
      -1,    -1,   122,    45,   127,    46,   122,    -1,   122,    43,
     122,    -1,   122,    44,   122,    -1,     3,    -1,     9,    -1,
      10,    -1,    42,    -1,    -1,    66,   225,    89,    95,   129,
     185,    91,    -1,    66,   114,    89,    -1,    66,     1,    89,
      -1,   252,   250,    89,    -1,   252,     1,    89,    -1,   128,
      66,   115,    89,    -1,    36,    66,   122,    94,   225,    89,
      -1,    37,    66,   122,    94,   122,    94,   122,    89,    -1,
      38,    66,   225,    94,   225,    89,    -1,   128,    67,   114,
      96,    -1,   128,    65,   112,    -1,   128,    68,   112,    -1,
     128,    62,    -1,   128,    61,    -1,   131,    -1,    -1,   133,
      -1,   259,   134,    -1,   132,    -1,   240,    -1,   133,   132,
      -1,   132,   240,    -1,   157,   136,   169,    90,    -1,   158,
     136,   170,    90,    -1,   157,    90,    -1,   158,    90,    -1,
     259,   138,    -1,    -1,   176,    -1,   155,   136,   169,    90,
      -1,   156,   136,   170,    90,    -1,   155,   136,   193,    -1,
     156,   136,   196,    -1,   162,    90,    -1,   297,   138,    -1,
       8,    -1,   139,     8,    -1,   140,     8,    -1,   139,   177,
      -1,   141,     8,    -1,   142,     8,    -1,   177,    -1,   141,
     177,    -1,   164,    -1,   143,     8,    -1,   144,     8,    -1,
     143,   166,    -1,   144,   166,    -1,   139,   164,    -1,   140,
     164,    -1,   165,    -1,   143,   177,    -1,   143,   167,    -1,
     144,   167,    -1,   139,   165,    -1,   140,   165,    -1,   145,
       8,    -1,   146,     8,    -1,   145,   166,    -1,   146,   166,
      -1,   141,   164,    -1,   142,   164,    -1,   145,   177,    -1,
     145,   167,    -1,   146,   167,    -1,   141,   165,    -1,   142,
     165,    -1,   182,    -1,   147,     8,    -1,   148,     8,    -1,
     139,   182,    -1,   140,   182,    -1,   147,   182,    -1,   148,
     182,    -1,   147,   177,    -1,   149,     8,    -1,   150,     8,
      -1,   141,   182,    -1,   142,   182,    -1,   149,   182,    -1,
     150,   182,    -1,   149,   177,    -1,   151,     8,    -1,   152,
       8,    -1,   151,   166,    -1,   152,   166,    -1,   147,   164,
      -1,   148,   164,    -1,   143,   182,    -1,   144,   182,    -1,
     151,   182,    -1,   152,   182,    -1,   151,   177,    -1,   151,
     167,    -1,   152,   167,    -1,   147,   165,    -1,   148,   165,
      -1,   153,     8,    -1,   154,     8,    -1,   153,   166,    -1,
     154,   166,    -1,   149,   164,    -1,   150,   164,    -1,   145,
     182,    -1,   146,   182,    -1,   153,   182,    -1,   154,   182,
      -1,   153,   177,    -1,   153,   167,    -1,   154,   167,    -1,
     149,   165,    -1,   150,   165,    -1,   143,    -1,   144,    -1,
     145,    -1,   146,    -1,   151,    -1,   152,    -1,   153,    -1,
     154,    -1,   139,    -1,   140,    -1,   141,    -1,   142,    -1,
     147,    -1,   148,    -1,   149,    -1,   150,    -1,   143,    -1,
     144,    -1,   151,    -1,   152,    -1,   139,    -1,   140,    -1,
     147,    -1,   148,    -1,   143,    -1,   144,    -1,   145,    -1,
     146,    -1,   139,    -1,   140,    -1,   141,    -1,   142,    -1,
     143,    -1,   144,    -1,   145,    -1,   146,    -1,   139,    -1,
     140,    -1,   141,    -1,   142,    -1,   139,    -1,   140,    -1,
     141,    -1,   142,    -1,   143,    -1,   144,    -1,   145,    -1,
     146,    -1,   147,    -1,   148,    -1,   149,    -1,   150,    -1,
     151,    -1,   152,    -1,   153,    -1,   154,    -1,    -1,   160,
      -1,   166,    -1,   168,    -1,   167,    -1,     7,    -1,   213,
      -1,   208,    -1,     4,    -1,   120,    66,   114,    89,    -1,
     120,    66,   225,    89,    -1,   172,    -1,   169,    94,   137,
     172,    -1,   174,    -1,   170,    94,   137,   174,    -1,    -1,
      28,    66,    10,    89,    -1,    -1,   199,   171,   176,    43,
     173,   183,    -1,   199,   171,   176,    -1,    -1,   204,   171,
     176,    43,   175,   183,    -1,   204,   171,   176,    -1,    -1,
     177,    -1,   178,    -1,   177,   178,    -1,    31,    66,    66,
     179,    89,    89,    -1,   180,    -1,   179,    94,   180,    -1,
      -1,   181,    -1,   181,    66,     3,    89,    -1,   181,    66,
       3,    94,   116,    89,    -1,   181,    66,   115,    89,    -1,
     112,    -1,   182,    -1,     7,    -1,     8,    -1,     6,    -1,
       5,    -1,   122,    -1,    -1,    95,   184,   185,    91,    -1,
       1,    -1,    -1,   186,   214,    -1,   187,    -1,   186,    94,
     187,    -1,   191,    43,   189,    -1,   192,   189,    -1,    -1,
     112,    46,   188,   189,    -1,   189,    -1,    -1,    95,   190,
     185,    91,    -1,   122,    -1,     1,    -1,   192,    -1,   191,
     192,    -1,    65,   112,    -1,    67,   122,    11,   122,    96,
      -1,    67,   122,    96,    -1,    -1,    -1,   199,   194,   130,
     259,   195,   253,    -1,    -1,    -1,   204,   197,   130,   259,
     198,   253,    -1,   200,    -1,   204,    -1,    66,   176,   200,
      89,    -1,   200,    66,   292,    -1,   200,   233,    -1,    58,
     163,   200,    -1,     4,    -1,   202,    -1,   203,    -1,   202,
      66,   292,    -1,   202,   233,    -1,     4,    -1,   203,    66,
     292,    -1,   203,   233,    -1,    58,   163,   202,    -1,    58,
     163,   203,    -1,    66,   176,   203,    89,    -1,   204,    66,
     292,    -1,    66,   176,   204,    89,    -1,    58,   163,   204,
      -1,   204,   233,    -1,     3,    -1,    14,    -1,    14,   177,
      -1,    15,    -1,    15,   177,    -1,    13,    -1,    13,   177,
      -1,    -1,   205,   112,    95,   209,   216,    91,   176,    -1,
     205,    95,   216,    91,   176,    -1,    -1,   206,   112,    95,
     210,   216,    91,   176,    -1,   206,    95,   216,    91,   176,
      -1,    -1,   207,   112,    95,   211,   223,   215,    91,   176,
      -1,    -1,   207,    95,   212,   223,   215,    91,   176,    -1,
     205,   112,    -1,   206,   112,    -1,   207,   112,    -1,    -1,
      94,    -1,    -1,    94,    -1,   217,    -1,   217,   218,    -1,
      -1,   217,   218,    90,    -1,   217,    90,    -1,   159,   136,
     219,    -1,   159,   136,    -1,   160,   136,   220,    -1,   160,
      -1,     1,    -1,   297,   218,    -1,   221,    -1,   219,    94,
     137,   221,    -1,   222,    -1,   220,    94,   137,   222,    -1,
     199,   176,    -1,   199,    46,   122,   176,    -1,    46,   122,
     176,    -1,   204,   176,    -1,   204,    46,   122,   176,    -1,
      46,   122,   176,    -1,   224,    -1,   223,    94,   224,    -1,
       1,    -1,   112,    -1,   112,    43,   122,    -1,    -1,   161,
     226,   227,    -1,    -1,   229,    -1,    -1,   229,    -1,   230,
     177,    -1,   231,    -1,   230,    -1,   232,    -1,    58,   163,
     230,    -1,    58,   163,    -1,    58,   163,   231,    -1,    66,
     176,   229,    89,    -1,   232,    66,   282,    -1,   232,   233,
      -1,    66,   282,    -1,   233,    -1,    67,   163,   122,    96,
      -1,    67,   163,    96,    -1,    67,   163,    58,    96,    -1,
      67,     6,   163,   122,    96,    -1,    67,   160,     6,   122,
      96,    -1,   235,    -1,   236,    -1,   237,    -1,   238,    -1,
     262,    -1,   235,   262,    -1,   236,   262,    -1,   237,   262,
      -1,   238,   262,    -1,   135,    -1,   235,   135,    -1,   236,
     135,    -1,   238,   135,    -1,   263,    -1,   235,   263,    -1,
     236,   263,    -1,   237,   263,    -1,   238,   263,    -1,   240,
      -1,   239,   240,    -1,   235,    -1,   236,    -1,   237,    -1,
     238,    -1,     1,    90,    -1,    -1,    -1,    -1,    -1,    -1,
     246,    -1,   247,    -1,   246,   247,    -1,    33,   296,    90,
      -1,   253,    -1,     1,   253,    -1,    95,    -1,    91,    -1,
     241,   245,   251,    91,   242,    -1,   234,    -1,     1,    -1,
      66,    95,    -1,   249,   250,    -1,   255,   261,    -1,   255,
       1,    -1,    -1,    16,   256,    66,   114,    89,    -1,    -1,
      19,   258,   261,    18,    -1,    -1,   262,    -1,   263,   260,
      -1,   243,   260,   244,    -1,   259,   274,    -1,   259,   275,
      -1,    -1,   254,    17,   265,   261,    -1,   254,    -1,   254,
      17,     1,    -1,    -1,    -1,    18,   266,    66,   114,    89,
     267,   261,    -1,   257,    66,   114,    89,    90,    -1,   257,
       1,    -1,    -1,    -1,    -1,    -1,    20,   268,    66,   273,
     269,   277,    90,   270,   277,    89,   271,   261,    -1,    -1,
      21,    66,   114,    89,   272,   261,    -1,   277,    90,    -1,
     138,    -1,   253,    -1,   114,    90,    -1,   243,   264,   244,
      -1,    24,    90,    -1,    25,    90,    -1,    26,    90,    -1,
      26,   114,    90,    -1,    28,   276,    66,   114,    89,    90,
      -1,    28,   276,    66,   114,    46,   278,    89,    90,    -1,
      28,   276,    66,   114,    46,   278,    46,   278,    89,    90,
      -1,    28,   276,    66,   114,    46,   278,    46,   278,    46,
     281,    89,    90,    -1,    27,   112,    90,    -1,    27,    58,
     114,    90,    -1,    90,    -1,    22,   122,    46,    -1,    22,
     122,    11,   122,    46,    -1,    23,    46,    -1,   112,   259,
      46,   176,    -1,    -1,     8,    -1,    -1,   114,    -1,    -1,
     279,    -1,   280,    -1,   279,    94,   280,    -1,    10,    66,
     114,    89,    -1,    67,   112,    96,    10,    66,   114,    89,
      -1,    10,    -1,   281,    94,    10,    -1,    -1,   176,   283,
     284,    -1,   287,    89,    -1,    -1,    -1,   288,    90,   285,
     176,   286,   284,    -1,     1,    89,    -1,    -1,    11,    -1,
     288,    -1,   288,    94,    11,    -1,   290,    -1,   288,    94,
     289,    -1,   155,   136,   201,   176,    -1,   155,   136,   204,
     176,    -1,   155,   136,   228,    -1,   156,   136,   204,   176,
      -1,   156,   136,   228,    -1,   157,   291,   201,   176,    -1,
     157,   291,   204,   176,    -1,   157,   291,   228,    -1,   158,
     291,   204,   176,    -1,   158,   291,   228,    -1,   136,    -1,
      -1,   176,   293,   294,    -1,   284,    -1,   295,    89,    -1,
       3,    -1,   295,    94,     3,    -1,   112,    -1,   296,    94,
     112,    -1,    32,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const unsigned short yyrline[] =
{
       0,   319,   319,   322,   330,   330,   331,   331,   335,   340,
     341,   342,   350,   355,   362,   364,   366,   368,   369,   370,
     377,   382,   376,   387,   390,   395,   389,   400,   403,   408,
     402,   413,   418,   419,   422,   424,   426,   431,   433,   435,
     437,   441,   447,   448,   452,   454,   459,   460,   463,   466,
     470,   472,   478,   481,   484,   487,   489,   494,   498,   502,
     506,   507,   512,   513,   515,   517,   519,   521,   523,   525,
     527,   529,   531,   533,   535,   538,   537,   545,   544,   552,
     556,   551,   562,   561,   572,   579,   591,   597,   598,   599,
     602,   601,   614,   619,   621,   635,   641,   643,   646,   656,
     666,   668,   672,   678,   680,   685,   693,   702,   713,   718,
     719,   720,   721,   729,   731,   733,   736,   745,   754,   764,
     769,   771,   773,   775,   777,   779,   836,   839,   842,   848,
     854,   857,   863,   866,   872,   875,   878,   881,   884,   887,
     890,   896,   899,   902,   905,   908,   911,   917,   920,   923,
     926,   929,   932,   938,   941,   944,   947,   950,   956,   959,
     962,   965,   971,   977,   983,   992,   998,  1001,  1004,  1010,
    1016,  1022,  1031,  1037,  1040,  1043,  1046,  1049,  1052,  1055,
    1061,  1067,  1073,  1082,  1085,  1088,  1091,  1094,  1100,  1103,
    1106,  1109,  1112,  1115,  1118,  1124,  1130,  1136,  1145,  1148,
    1151,  1154,  1157,  1164,  1165,  1166,  1167,  1168,  1169,  1170,
    1171,  1175,  1176,  1177,  1178,  1179,  1180,  1181,  1182,  1186,
    1187,  1188,  1189,  1193,  1194,  1195,  1196,  1200,  1201,  1202,
    1203,  1207,  1208,  1209,  1210,  1214,  1215,  1216,  1217,  1218,
    1219,  1220,  1221,  1225,  1226,  1227,  1228,  1229,  1230,  1231,
    1232,  1233,  1234,  1235,  1236,  1237,  1238,  1239,  1240,  1246,
    1247,  1273,  1274,  1278,  1282,  1284,  1288,  1292,  1296,  1302,
    1309,  1310,  1314,  1315,  1320,  1321,  1327,  1326,  1334,  1343,
    1342,  1350,  1359,  1360,  1365,  1367,  1372,  1377,  1379,  1385,
    1386,  1388,  1390,  1392,  1400,  1401,  1402,  1403,  1407,  1408,
    1414,  1416,  1415,  1419,  1426,  1428,  1432,  1433,  1439,  1442,
    1446,  1445,  1451,  1456,  1455,  1459,  1461,  1465,  1466,  1470,
    1472,  1476,  1482,  1495,  1481,  1513,  1526,  1512,  1546,  1547,
    1553,  1555,  1560,  1562,  1564,  1572,  1573,  1577,  1582,  1584,
    1588,  1593,  1595,  1597,  1599,  1607,  1612,  1614,  1616,  1618,
    1622,  1624,  1629,  1631,  1636,  1638,  1650,  1649,  1656,  1661,
    1660,  1665,  1670,  1669,  1675,  1674,  1682,  1684,  1686,  1694,
    1696,  1699,  1701,  1719,  1721,  1727,  1728,  1730,  1736,  1739,
    1749,  1752,  1757,  1759,  1765,  1766,  1771,  1772,  1777,  1781,
    1785,  1792,  1796,  1800,  1810,  1811,  1816,  1822,  1824,  1830,
    1829,  1838,  1839,  1844,  1847,  1851,  1858,  1859,  1863,  1864,
    1869,  1871,  1876,  1878,  1880,  1882,  1884,  1891,  1893,  1895,
    1897,  1900,  1911,  1912,  1913,  1917,  1921,  1922,  1923,  1924,
    1925,  1929,  1930,  1936,  1937,  1941,  1942,  1943,  1944,  1945,
    1949,  1950,  1954,  1955,  1956,  1957,  1960,  1964,  1971,  1978,
    1994,  2008,  2010,  2016,  2017,  2021,  2035,  2037,  2040,  2044,
    2046,  2054,  2055,  2059,  2076,  2084,  2089,  2102,  2101,  2115,
    2114,  2137,  2143,  2144,  2149,  2155,  2169,  2179,  2178,  2186,
    2198,  2209,  2212,  2208,  2220,  2223,  2226,  2230,  2233,  2237,
    2225,  2243,  2242,  2252,  2254,  2260,  2262,  2265,  2269,  2278,
    2287,  2290,  2293,  2297,  2301,  2306,  2310,  2322,  2328,  2336,
    2339,  2342,  2345,  2362,  2363,  2369,  2370,  2376,  2377,  2381,
    2382,  2387,  2389,  2396,  2398,  2409,  2408,  2417,  2419,  2421,
    2418,  2425,  2432,  2433,  2444,  2448,  2455,  2457,  2464,  2469,
    2474,  2477,  2483,  2491,  2496,  2501,  2504,  2510,  2516,  2526,
    2525,  2534,  2535,  2553,  2555,  2561,  2563,  2568
};
#endif

#if YYDEBUG || YYERROR_VERBOSE
/* YYTNME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals. */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "IDENTIFIER", "TYPENAME", "SCSPEC", 
  "STATIC", "TYPESPEC", "TYPE_QUAL", "CONSTANT", "STRING", "ELLIPSIS", 
  "SIZEOF", "ENUM", "STRUCT", "UNION", "IF", "ELSE", "WHILE", "DO", "FOR", 
  "SWITCH", "CASE", "DEFAULT", "BREAK", "CONTINUE", "RETURN", "GOTO", 
  "ASM_KEYWORD", "TYPEOF", "ALIGNOF", "ATTRIBUTE", "EXTENSION", "LABEL", 
  "REALPART", "IMAGPART", "VA_ARG", "CHOOSE_EXPR", "TYPES_COMPATIBLE_P", 
  "PTR_VALUE", "PTR_BASE", "PTR_EXTENT", "FUNC_NAME", "'='", "ASSIGN", 
  "'?'", "':'", "OROR", "ANDAND", "'|'", "'^'", "'&'", "EQCOMPARE", 
  "ARITHCOMPARE", "RSHIFT", "LSHIFT", "'+'", "'-'", "'*'", "'/'", "'%'", 
  "MINUSMINUS", "PLUSPLUS", "UNARY", "HYPERUNARY", "'.'", "'('", "'['", 
  "POINTSAT", "INTERFACE", "IMPLEMENTATION", "END", "SELECTOR", "DEFS", 
  "ENCODE", "CLASSNAME", "PUBLIC", "PRIVATE", "PROTECTED", "PROTOCOL", 
  "OBJECTNAME", "CLASS", "ALIAS", "AT_THROW", "AT_TRY", "AT_CATCH", 
  "AT_FINALLY", "AT_SYNCHRONIZED", "OBJC_STRING", "')'", "';'", "'}'", 
  "'~'", "'!'", "','", "'{'", "']'", "$accept", "program", "extdefs", 
  "@1", "@2", "extdef", "extdef_1", "datadef", "fndef", "@3", "@4", "@5", 
  "@6", "@7", "@8", "identifier", "unop", "expr", "exprlist", 
  "nonnull_exprlist", "unary_expr", "sizeof", "alignof", "typeof", 
  "cast_expr", "expr_no_commas", "@9", "@10", "@11", "@12", "@13", 
  "primary", "@14", "old_style_parm_decls", "old_style_parm_decls_1", 
  "lineno_datadecl", "datadecls", "datadecl", "lineno_decl", "setspecs", 
  "maybe_resetattrs", "decl", "declspecs_nosc_nots_nosa_noea", 
  "declspecs_nosc_nots_nosa_ea", "declspecs_nosc_nots_sa_noea", 
  "declspecs_nosc_nots_sa_ea", "declspecs_nosc_ts_nosa_noea", 
  "declspecs_nosc_ts_nosa_ea", "declspecs_nosc_ts_sa_noea", 
  "declspecs_nosc_ts_sa_ea", "declspecs_sc_nots_nosa_noea", 
  "declspecs_sc_nots_nosa_ea", "declspecs_sc_nots_sa_noea", 
  "declspecs_sc_nots_sa_ea", "declspecs_sc_ts_nosa_noea", 
  "declspecs_sc_ts_nosa_ea", "declspecs_sc_ts_sa_noea", 
  "declspecs_sc_ts_sa_ea", "declspecs_ts", "declspecs_nots", 
  "declspecs_ts_nosa", "declspecs_nots_nosa", "declspecs_nosc_ts", 
  "declspecs_nosc_nots", "declspecs_nosc", "declspecs", 
  "maybe_type_quals_attrs", "typespec_nonattr", "typespec_attr", 
  "typespec_reserved_nonattr", "typespec_reserved_attr", 
  "typespec_nonreserved_nonattr", "initdecls", "notype_initdecls", 
  "maybeasm", "initdcl", "@15", "notype_initdcl", "@16", 
  "maybe_attribute", "attributes", "attribute", "attribute_list", 
  "attrib", "any_word", "scspec", "init", "@17", "initlist_maybe_comma", 
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
  "do_stmt_start", "@30", "save_location", "lineno_labeled_stmt", 
  "c99_block_lineno_labeled_stmt", "lineno_stmt", "lineno_label", 
  "select_or_iter_stmt", "@31", "@32", "@33", "@34", "@35", "@36", "@37", 
  "@38", "for_init_stmt", "stmt", "label", "maybe_type_qual", "xexpr", 
  "asm_operands", "nonnull_asm_operands", "asm_operand", "asm_clobbers", 
  "parmlist", "@39", "parmlist_1", "@40", "@41", "parmlist_2", "parms", 
  "parm", "firstparm", "setspecs_fp", "parmlist_or_identifiers", "@42", 
  "parmlist_or_identifiers_1", "identifiers", "identifiers_or_typenames", 
  "extension", 0
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
     321,   322,   323,   324,   325,   326,   327,   328,   329,    41,
      59,   125,   126,    33,    44,   123,    93
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const unsigned short yyr1[] =
{
       0,    97,    98,    98,   100,    99,   101,    99,   102,   103,
     103,   103,   103,   104,   104,   104,   104,   104,   104,   104,
     106,   107,   105,   105,   108,   109,   105,   105,   110,   111,
     105,   105,   112,   112,   113,   113,   113,   113,   113,   113,
     113,   114,   115,   115,   116,   116,   117,   117,   117,   117,
     117,   117,   117,   117,   117,   117,   117,   118,   119,   120,
     121,   121,   122,   122,   122,   122,   122,   122,   122,   122,
     122,   122,   122,   122,   122,   123,   122,   124,   122,   125,
     126,   122,   127,   122,   122,   122,   128,   128,   128,   128,
     129,   128,   128,   128,   128,   128,   128,   128,   128,   128,
     128,   128,   128,   128,   128,   130,   131,   131,   132,   133,
     133,   133,   133,   134,   134,   134,   134,   135,   136,   137,
     138,   138,   138,   138,   138,   138,   139,   139,   139,   140,
     141,   141,   142,   142,   143,   143,   143,   143,   143,   143,
     143,   144,   144,   144,   144,   144,   144,   145,   145,   145,
     145,   145,   145,   146,   146,   146,   146,   146,   147,   147,
     147,   147,   147,   147,   147,   148,   149,   149,   149,   149,
     149,   149,   150,   151,   151,   151,   151,   151,   151,   151,
     151,   151,   151,   152,   152,   152,   152,   152,   153,   153,
     153,   153,   153,   153,   153,   153,   153,   153,   154,   154,
     154,   154,   154,   155,   155,   155,   155,   155,   155,   155,
     155,   156,   156,   156,   156,   156,   156,   156,   156,   157,
     157,   157,   157,   158,   158,   158,   158,   159,   159,   159,
     159,   160,   160,   160,   160,   161,   161,   161,   161,   161,
     161,   161,   161,   162,   162,   162,   162,   162,   162,   162,
     162,   162,   162,   162,   162,   162,   162,   162,   162,   163,
     163,   164,   164,   165,   166,   166,   167,   168,   168,   168,
     169,   169,   170,   170,   171,   171,   173,   172,   172,   175,
     174,   174,   176,   176,   177,   177,   178,   179,   179,   180,
     180,   180,   180,   180,   181,   181,   181,   181,   182,   182,
     183,   184,   183,   183,   185,   185,   186,   186,   187,   187,
     188,   187,   187,   190,   189,   189,   189,   191,   191,   192,
     192,   192,   194,   195,   193,   197,   198,   196,   199,   199,
     200,   200,   200,   200,   200,   201,   201,   202,   202,   202,
     203,   203,   203,   203,   203,   204,   204,   204,   204,   204,
     205,   205,   206,   206,   207,   207,   209,   208,   208,   210,
     208,   208,   211,   208,   212,   208,   213,   213,   213,   214,
     214,   215,   215,   216,   216,   217,   217,   217,   218,   218,
     218,   218,   218,   218,   219,   219,   220,   220,   221,   221,
     221,   222,   222,   222,   223,   223,   223,   224,   224,   226,
     225,   227,   227,   228,   228,   228,   229,   229,   230,   230,
     231,   231,   232,   232,   232,   232,   232,   233,   233,   233,
     233,   233,   234,   234,   234,   234,   235,   235,   235,   235,
     235,   236,   236,   236,   236,   237,   237,   237,   237,   237,
     238,   238,   239,   239,   239,   239,   240,   241,   242,   243,
     244,   245,   245,   246,   246,   247,   248,   248,   249,   250,
     250,   251,   251,   252,   253,   254,   254,   256,   255,   258,
     257,   259,   260,   260,   261,   262,   263,   265,   264,   264,
     264,   266,   267,   264,   264,   264,   268,   269,   270,   271,
     264,   272,   264,   273,   273,   274,   274,   274,   274,   274,
     274,   274,   274,   274,   274,   274,   274,   274,   274,   275,
     275,   275,   275,   276,   276,   277,   277,   278,   278,   279,
     279,   280,   280,   281,   281,   283,   282,   284,   285,   286,
     284,   284,   287,   287,   287,   287,   288,   288,   289,   289,
     289,   289,   289,   290,   290,   290,   290,   290,   291,   293,
     292,   294,   294,   295,   295,   296,   296,   297
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const unsigned char yyr2[] =
{
       0,     2,     0,     1,     0,     2,     0,     3,     1,     1,
       1,     5,     2,     3,     4,     4,     2,     2,     2,     1,
       0,     0,     8,     4,     0,     0,     8,     4,     0,     0,
       7,     3,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     0,     1,     1,     3,     1,     2,     2,     2,
       2,     2,     4,     2,     4,     2,     2,     1,     1,     1,
       1,     4,     1,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     0,     4,     0,     4,     0,
       0,     7,     0,     5,     3,     3,     1,     1,     1,     1,
       0,     7,     3,     3,     3,     3,     4,     6,     8,     6,
       4,     3,     3,     2,     2,     1,     0,     1,     2,     1,
       1,     2,     2,     4,     4,     2,     2,     2,     0,     1,
       4,     4,     3,     3,     2,     2,     1,     2,     2,     2,
       2,     2,     1,     2,     1,     2,     2,     2,     2,     2,
       2,     1,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     1,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     0,
       1,     1,     1,     1,     1,     1,     1,     1,     4,     4,
       1,     4,     1,     4,     0,     4,     0,     6,     3,     0,
       6,     3,     0,     1,     1,     2,     6,     1,     3,     0,
       1,     4,     6,     4,     1,     1,     1,     1,     1,     1,
       1,     0,     4,     1,     0,     2,     1,     3,     3,     2,
       0,     4,     1,     0,     4,     1,     1,     1,     2,     2,
       5,     3,     0,     0,     6,     0,     0,     6,     1,     1,
       4,     3,     2,     3,     1,     1,     1,     3,     2,     1,
       3,     2,     3,     3,     4,     3,     4,     3,     2,     1,
       1,     2,     1,     2,     1,     2,     0,     7,     5,     0,
       7,     5,     0,     8,     0,     7,     2,     2,     2,     0,
       1,     0,     1,     1,     2,     0,     3,     2,     3,     2,
       3,     1,     1,     2,     1,     4,     1,     4,     2,     4,
       3,     2,     4,     3,     1,     3,     1,     1,     3,     0,
       3,     0,     1,     0,     1,     2,     1,     1,     1,     3,
       2,     3,     4,     3,     2,     2,     1,     4,     3,     4,
       5,     5,     1,     1,     1,     1,     1,     2,     2,     2,
       2,     1,     2,     2,     2,     1,     2,     2,     2,     2,
       1,     2,     1,     1,     1,     1,     2,     0,     0,     0,
       0,     0,     1,     1,     2,     3,     1,     2,     1,     1,
       5,     1,     1,     2,     2,     2,     2,     0,     5,     0,
       4,     0,     1,     2,     3,     2,     2,     0,     4,     1,
       3,     0,     0,     7,     5,     2,     0,     0,     0,     0,
      12,     0,     6,     2,     1,     1,     2,     3,     2,     2,
       2,     3,     6,     8,    10,    12,     3,     4,     1,     3,
       5,     2,     4,     0,     1,     0,     1,     0,     1,     1,
       3,     4,     7,     1,     3,     0,     3,     2,     0,     0,
       6,     2,     0,     1,     1,     3,     1,     3,     4,     4,
       3,     4,     3,     4,     4,     3,     4,     3,     1,     0,
       3,     1,     2,     1,     3,     1,     3,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const unsigned short yydefact[] =
{
       4,     0,     6,     0,     1,     0,     0,   267,   299,   298,
     264,   126,   354,   350,   352,     0,    59,     0,   557,    19,
       5,     8,    10,     9,     0,     0,   211,   212,   213,   214,
     203,   204,   205,   206,   215,   216,   217,   218,   207,   208,
     209,   210,   118,   118,     0,   134,   141,   261,   263,   262,
     132,   284,   158,     0,     0,     0,   266,   265,     0,     7,
      17,    18,   355,   351,   353,     0,     0,     0,   349,   259,
     282,     0,   272,     0,   127,   139,   145,   129,   161,   128,
     140,   146,   162,   130,   151,   156,   133,   168,   131,   152,
     157,   169,   135,   137,   143,   142,   179,   136,   138,   144,
     180,   147,   149,   154,   153,   194,   148,   150,   155,   195,
     159,   177,   186,   165,   163,   160,   178,   187,   164,   166,
     192,   201,   172,   170,   167,   193,   202,   171,   173,   175,
     184,   183,   181,   174,   176,   185,   182,   188,   190,   199,
     198,   196,   189,   191,   200,   197,     0,     0,    16,   285,
      32,    33,   375,   366,   375,   367,   364,   368,    12,    86,
      87,    88,    57,    58,     0,     0,     0,     0,     0,    89,
       0,    34,    36,    35,     0,    38,    37,     0,    39,    40,
       0,     0,    41,    60,     0,     0,    62,    44,    46,     0,
       0,   289,     0,   239,   240,   241,   242,   235,   236,   237,
     238,   399,     0,   231,   232,   233,   234,   260,     0,     0,
     283,    13,   282,    31,     0,   282,   259,     0,   282,   348,
     334,   259,   282,     0,   270,     0,   328,   329,     0,     0,
       0,     0,   356,     0,   359,     0,   362,    55,    56,     0,
       0,     0,    50,    47,     0,   463,     0,     0,    49,     0,
       0,     0,    51,     0,    53,     0,     0,    79,    77,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   104,   103,     0,    42,     0,     0,     0,   459,
     451,     0,    48,   296,   297,   294,     0,   287,   290,   295,
     268,   401,   269,   347,     0,     0,   119,     0,   549,   345,
     259,   260,     0,     0,   471,   105,     0,   471,   110,     0,
     281,     0,     0,    15,   282,    23,     0,   282,   282,   332,
      14,    27,     0,   282,   382,   377,   231,   232,   233,   234,
     227,   228,   229,   230,   118,   118,   374,     0,   375,   282,
     375,   396,   397,   371,   394,     0,     0,     0,     0,    93,
      92,     0,    11,    45,     0,     0,    84,    85,     0,     0,
       0,     0,    73,    74,    72,    71,    70,    69,    68,    63,
      64,    65,    66,    67,   101,     0,    43,     0,   102,    95,
       0,     0,   452,   453,    94,     0,   289,    42,   259,   282,
     400,   402,   407,   406,   408,   416,   346,   273,   274,     0,
       0,     0,     0,     0,   418,     0,   446,    29,   112,   111,
     108,   223,   224,   219,   220,   225,   226,   221,   222,   118,
     118,   279,   333,     0,     0,   471,   278,   331,   471,   358,
     379,     0,   376,   383,     0,   361,     0,     0,   372,     0,
     371,     0,     0,     0,    90,    61,    52,    54,     0,     0,
      78,    76,    96,   100,   555,     0,   462,   431,   461,   471,
     471,   471,   471,     0,   440,     0,   449,   426,   435,   454,
     286,   288,    86,     0,   410,   525,   415,   282,   414,   275,
       0,   553,   533,   118,   118,   551,     0,   534,   536,   550,
       0,     0,     0,   419,   417,     0,   115,     0,   116,     0,
       0,   330,   271,   274,    21,   276,    25,     0,   282,   378,
     384,     0,   282,   380,   386,   282,   282,   398,   395,   282,
       0,     0,     0,     0,     0,    80,    83,   455,     0,   432,
     427,   436,   433,   428,   437,   449,   429,   438,   434,   430,
     439,   441,   448,    86,   267,     0,     0,     0,     0,     0,
       0,   513,   508,   458,   471,     0,   117,   118,   118,     0,
       0,   447,   495,   475,   476,     0,   291,     0,   293,   409,
     411,     0,     0,   525,   413,   531,   548,   403,   403,   527,
     528,     0,   552,     0,   420,   421,     0,    30,   456,     0,
       0,   303,   301,   300,   280,     0,     0,     0,   282,     0,
     388,   282,   282,     0,   391,   282,   357,   360,   365,   282,
      97,     0,    99,   316,     0,     0,   313,     0,   315,     0,
     369,   306,   312,     0,     0,     0,   556,   460,     0,   511,
     498,   499,   500,     0,     0,     0,   514,     0,     0,   496,
       0,     0,   124,   467,   481,   469,   486,     0,   479,     0,
       0,   450,   464,   125,     0,   412,   526,   339,   259,   282,
     282,   335,   336,   282,   545,   404,   407,   259,   282,   282,
     547,   282,   535,   211,   212,   213,   214,   203,   204,   205,
     206,   215,   216,   217,   218,   207,   208,   209,   210,   118,
     118,   537,   554,   457,   113,   114,     0,    22,   277,    26,
     390,   282,     0,   393,   282,     0,   363,     0,   319,     0,
       0,   310,    91,     0,   305,     0,   318,   309,    81,     0,
     509,   501,     0,   506,     0,   282,     0,   122,   322,     0,
     123,   325,     0,     0,   449,     0,     0,     0,   466,   471,
     465,   485,     0,   497,   292,   410,   525,   543,   282,   338,
     282,   341,   544,   405,   410,   525,   546,   529,   403,   403,
       0,   389,   385,   392,   387,    98,     0,   321,     0,     0,
     307,   308,     0,   507,     0,   512,   120,     0,   121,     0,
       0,     0,     0,   515,     0,   480,   449,   450,   472,   471,
       0,   342,   343,     0,   337,   340,     0,   282,   282,   540,
     282,   542,   302,     0,   314,   311,   510,   517,     0,   471,
     471,     0,     0,   470,   516,   494,   487,     0,   491,   478,
     474,   473,     0,   344,   530,   538,   539,   541,   320,     0,
       0,     0,   518,   519,   502,   323,   326,   468,   482,   515,
     493,   449,   484,     0,     0,   517,     0,     0,     0,     0,
     449,     0,   492,     0,     0,     0,   503,   520,   324,   327,
     483,   488,   521,     0,     0,     0,   515,     0,   523,     0,
     504,     0,     0,     0,     0,   489,   522,   505,   524,   449,
     490
};

/* YYDEFGOTO[NTERM-NUM]. */
static const short yydefgoto[] =
{
      -1,     1,     2,     3,     5,    20,    21,    22,    23,   316,
     595,   322,   597,   217,   495,   617,   180,   246,   375,   182,
     183,   184,   185,    24,   186,   187,   361,   360,   358,   625,
     359,   188,   524,   304,   305,   306,   307,   410,   457,    25,
     295,   556,   193,   194,   195,   196,   197,   198,   199,   200,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
     483,   484,   334,   207,   201,    44,   208,    45,    46,    47,
      48,    49,   223,    71,   218,   224,   596,    72,   500,   296,
     210,    51,   286,   287,   288,    52,   594,   696,   619,   620,
     621,   769,   622,   710,   623,   624,   727,   777,   848,   730,
     779,   849,   503,   226,   660,   661,   662,   227,    53,    54,
      55,    56,   338,   340,   345,   235,    57,   714,   439,   230,
     231,   336,   509,   513,   510,   514,   343,   344,   202,   291,
     390,   664,   665,   392,   393,   394,   219,   458,   459,   460,
     461,   462,   463,   308,   280,   627,   739,   743,   381,   382,
     383,   587,   561,   281,   465,   189,   588,   648,   649,   732,
     650,   734,   309,   787,   740,   788,   789,   651,   786,   733,
     850,   735,   839,   866,   879,   841,   816,   563,   564,   637,
     817,   831,   832,   833,   869,   476,   572,   485,   671,   796,
     486,   487,   691,   488,   577,   299,   400,   489,   490,   455,
     190
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -736
static const short yypact[] =
{
      84,    87,    94,  2827,  -736,  2827,   150,  -736,  -736,  -736,
    -736,  -736,    71,    71,    71,    60,  -736,   129,  -736,  -736,
    -736,  -736,  -736,  -736,   138,   300,  1044,   433,  1235,  1148,
     606,   807,   619,   941,  1785,  2593,  2272,  2697,  1990,  1485,
    2453,  1591,  -736,  -736,    70,  -736,  -736,  -736,  -736,  -736,
      71,  -736,  -736,    69,    73,    75,  -736,  -736,  2827,  -736,
    -736,  -736,    71,    71,    71,  2583,   155,  2479,  -736,    97,
      71,   -20,  -736,  1271,  -736,  -736,  -736,    71,  -736,  -736,
    -736,  -736,  -736,  -736,  -736,  -736,    71,  -736,  -736,  -736,
    -736,  -736,  -736,  -736,  -736,    71,  -736,  -736,  -736,  -736,
    -736,  -736,  -736,  -736,    71,  -736,  -736,  -736,  -736,  -736,
    -736,  -736,  -736,    71,  -736,  -736,  -736,  -736,  -736,  -736,
    -736,  -736,    71,  -736,  -736,  -736,  -736,  -736,  -736,  -736,
    -736,    71,  -736,  -736,  -736,  -736,  -736,  -736,  -736,  -736,
      71,  -736,  -736,  -736,  -736,  -736,   103,   300,  -736,  -736,
    -736,  -736,  -736,   154,  -736,   157,  -736,   175,  -736,  -736,
    -736,  -736,  -736,  -736,  2583,  2583,   211,   225,   231,  -736,
     334,  -736,  -736,  -736,  2583,  -736,  -736,  1875,  -736,  -736,
    2583,   216,   219,  -736,  2623,  2687,  -736,  3259,   529,  1529,
    2583,   880,   260,   659,   911,  1057,  1026,   436,   506,   472,
     668,  -736,   268,   184,   355,   277,   374,  -736,   300,   300,
      71,  -736,    71,  -736,   323,    71,   312,  1094,    71,  -736,
    -736,    97,    71,    18,  -736,   995,   475,   481,   134,  2088,
     301,  2793,  -736,   303,  -736,   451,  -736,  -736,  -736,  2583,
    2583,  3196,  -736,  -736,   332,  -736,   339,   346,  -736,   322,
    2583,  1875,  -736,  1875,  -736,  2583,  2583,   399,  -736,  -736,
    2583,  2583,  2583,  2583,  2583,  2583,  2583,  2583,  2583,  2583,
    2583,  2583,  -736,  -736,   334,  2583,  2583,   334,   368,  -736,
     439,   377,  -736,  -736,  -736,  -736,   -31,  -736,   409,  -736,
    -736,   366,  -736,   481,   180,   300,  -736,   467,  -736,  -736,
      97,   484,  2115,   402,  -736,  -736,  1165,    63,  -736,  1661,
     454,   103,   103,  -736,    71,  -736,  1094,    71,    71,  -736,
    -736,  -736,  1094,    71,  -736,  -736,   659,   911,  1057,  1026,
     436,   506,   472,   668,  -736,   427,   416,  1433,  -736,    71,
    -736,  -736,   465,   417,  -736,   451,  2947,  2975,   435,  -736,
    -736,  2347,  -736,  3259,   443,   446,  3259,  3259,  2583,   504,
    2583,  2583,  1164,  2174,  2080,  1128,  1237,   878,   878,   497,
     497,  -736,  -736,  -736,  -736,   469,   219,   468,  -736,  -736,
     334,  1622,   439,  -736,  -736,   482,   880,  2727,    97,    71,
    -736,  -736,  -736,  -736,   510,  -736,  -736,  -736,   146,   492,
    1691,  2583,  2583,  2158,  -736,  2847,  -736,  -736,  -736,  -736,
    -736,  3134,  3208,  1275,   602,  3146,  3225,  1470,  1789,   496,
     512,  -736,   475,   207,   103,  -736,   575,  -736,  -736,  -736,
     205,   341,  -736,  -736,   514,  -736,   532,  2583,   334,   547,
     417,  3196,  2583,  3196,  -736,  -736,   550,   550,   596,  2583,
    3222,  2794,  -736,  -736,  -736,   291,   402,  -736,  -736,    54,
      64,    92,    99,   646,  -736,   558,  2233,  -736,  -736,  -736,
    -736,  -736,   232,   563,   366,   366,  -736,    71,  -736,  -736,
     564,  -736,  -736,  -736,  -736,  -736,   566,   328,  -736,  -736,
     290,  2875,  2897,  -736,  -736,    68,  -736,   103,  -736,   300,
    1394,  -736,  -736,   623,  -736,  -736,  -736,  2583,   142,   567,
    -736,  2583,   330,   568,  -736,    71,    71,  3259,  -736,    71,
     577,   581,  2995,   589,  1915,  -736,  3275,  -736,   334,  -736,
    -736,  -736,  -736,  -736,  -736,  2307,  -736,  -736,  -736,  -736,
    -736,  -736,  -736,   633,   638,  2583,   639,   597,   601,  2519,
      79,   678,  -736,  -736,  -736,   607,  -736,  -736,  -736,   608,
     701,   613,  -736,  -736,  -736,  2414,  -736,  2583,  -736,  -736,
    -736,   616,   722,  -736,  -736,  -736,  -736,   223,   139,  -736,
    -736,  1754,  -736,   703,  -736,  -736,   636,  -736,  -736,   336,
     401,  -736,  -736,  3259,  -736,    68,  1394,    68,  3077,  2583,
    -736,    71,  3077,  2583,  -736,    71,  -736,  -736,  -736,    71,
    -736,  2583,  -736,  -736,   334,  2583,  -736,   686,  3259,   643,
     620,  -736,  -736,   310,  1321,  2583,  -736,  -736,  3047,  -736,
    -736,  -736,  -736,   648,  2583,   649,  -736,   674,   696,  -736,
     103,   300,  -736,  -736,  -736,  -736,  -736,   677,   731,  1715,
      56,  -736,  -736,  -736,   306,  -736,  -736,  -736,    97,    71,
      71,   533,   562,   109,  -736,  -736,    71,    97,    71,   109,
    -736,    71,  -736,  3134,  3208,  3165,  3237,  1275,   602,  1602,
    1847,  3146,  3225,  3177,  3254,  1470,  1789,  2057,  2021,  -736,
    -736,  -736,  -736,  -736,  -736,  -736,  1915,  -736,  -736,  -736,
    -736,  3077,   205,  -736,  3077,   341,  -736,  3023,  -736,  2818,
    1915,  -736,  -736,  1982,  -736,  2049,  -736,  -736,  3275,  2583,
    -736,  -736,   667,  -736,  2583,    71,   415,  -736,   337,   437,
    -736,   189,   693,   697,  -736,   699,  2583,  1808,  -736,  -736,
    -736,  -736,  2583,  -736,  -736,   223,   178,  -736,    71,  -736,
      71,  -736,  -736,    71,   139,   139,  -736,  -736,   223,   139,
     676,  -736,  -736,  -736,  -736,  -736,  2583,  -736,   682,  2049,
    -736,  -736,  3241,  -736,    44,  -736,  -736,  1094,  -736,  1094,
    2583,  2583,   751,  2414,   687,  -736,  -736,  -736,  -736,  -736,
     688,   533,   562,   285,  -736,  -736,   722,    71,   109,  -736,
     109,  -736,  -736,  2925,  -736,  -736,  -736,    49,   685,  -736,
    -736,   690,   691,  -736,  -736,  -736,  -736,   692,  -736,  -736,
    -736,  -736,   694,  -736,  -736,  -736,  -736,  -736,  -736,   717,
     334,    90,   698,  -736,  -736,  -736,  -736,  -736,  -736,  2583,
    -736,  -736,  -736,  2583,   700,    49,   707,    49,   636,   636,
    -736,   713,  -736,   704,   777,   110,  -736,  -736,  -736,  -736,
    -736,  -736,  -736,   741,   798,   719,  2583,  2583,  -736,   325,
    -736,   727,   729,   733,   809,  -736,  -736,  -736,  -736,  -736,
    -736
};

/* YYPGOTO[NTERM-NUM].  */
static const short yypgoto[] =
{
    -736,  -736,  -736,  -736,  -736,    96,  -736,  -736,  -736,  -736,
    -736,  -736,  -736,  -736,  -736,   -13,  -736,   -65,   438,  -258,
     354,  -736,  -736,  -736,    20,   702,  -736,  -736,  -736,  -736,
    -736,  -736,  -736,  -301,  -736,   520,  -736,  -736,     1,    82,
    -278,  -541,    -2,     2,    22,    41,     3,     8,    17,    23,
    -297,  -280,   247,   248,  -266,  -262,   250,   251,  -435,  -414,
     526,   527,  -736,  -178,  -736,  -433,  -202,   517,   666,   714,
     817,  -736,  -463,  -137,  -216,   429,  -736,   545,  -736,   318,
      83,    85,  -736,   477,  -736,   841,   255,  -736,  -642,  -736,
     141,  -736,  -576,  -736,  -736,   241,  -736,  -736,  -736,  -736,
    -736,  -736,  -130,   319,   107,   145,  -102,   147,  -736,  -736,
    -736,  -736,  -736,  -736,  -736,  -736,  -736,  -736,   455,  -103,
    -736,   554,  -736,  -736,   192,   191,   553,   461,   -50,  -736,
    -736,  -528,  -273,  -412,  -429,  -736,   367,  -736,  -736,  -736,
    -736,  -736,  -736,  -267,  -736,  -736,  -417,   113,  -736,  -736,
     521,  -225,  -736,   343,  -736,  -736,  -443,  -736,  -736,  -736,
    -736,  -736,   100,   118,  -697,  -194,  -131,  -736,  -736,  -736,
    -736,  -736,  -736,  -736,  -736,  -736,  -736,  -736,  -736,  -736,
    -735,    72,  -736,    61,  -736,   444,  -736,  -537,  -736,  -736,
    -736,  -736,  -736,  -736,   447,  -314,  -736,  -736,  -736,  -736,
      27
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -533
static const short yytable[] =
{
     181,    26,   192,    26,   427,    27,    30,    27,    30,   317,
     228,    31,   415,    31,   302,   425,   225,   376,   391,   311,
      32,   428,    32,   562,   653,    28,    33,    28,    33,   416,
      58,   557,    58,   559,   589,   656,   424,   782,   301,   408,
     153,   155,   157,   417,    29,   570,    29,   418,   717,   560,
     670,   233,   558,   335,   760,  -442,    26,   741,   385,   829,
      27,    30,   569,   386,  -107,  -443,    31,   203,   768,   586,
     211,   204,   150,   151,   212,    32,   150,   151,   150,   151,
      28,    33,   150,   151,    -2,    58,    50,     4,    50,   819,
     807,   205,   562,  -444,    -3,    62,    63,    64,   401,    29,
    -445,    59,    17,   415,   851,    11,    68,   220,   313,    77,
     206,    86,   314,    95,   464,   104,   830,   113,   560,   122,
     416,   131,   742,   140,   146,   147,    65,   247,    17,   376,
     557,   871,   559,   808,   417,   149,   845,   634,   418,   771,
      17,    50,    68,   693,   852,  -422,   689,   149,   149,   149,
      50,   558,    50,   860,   158,  -423,   864,   242,  -107,   335,
     148,   221,   149,   553,   152,   666,   666,   690,   154,   222,
     156,   149,    73,    17,   214,   215,   216,   726,   285,   846,
     149,    68,   880,  -424,   237,   238,   474,   467,   599,   149,
    -425,   348,    74,   805,   243,    66,   541,   667,   149,   865,
     248,   354,   571,   355,    67,   668,   216,   149,    68,   220,
     282,   377,   215,   216,   203,    17,   149,   214,   204,   203,
    -274,   191,   342,   204,   320,   149,    68,   657,   212,   326,
     799,   801,  -274,   327,   330,   434,   658,   436,   205,   331,
      60,    61,   815,   205,   659,   216,   215,   216,   332,   232,
     468,   507,   234,   328,   333,   215,   216,   206,   337,   824,
      50,   374,   206,   221,   378,   530,   533,   536,   539,   396,
     236,   222,   329,   318,   216,   415,    77,   239,    86,  -274,
      95,   658,   104,  -274,   681,    83,    77,   317,    86,   659,
     216,   240,   416,   448,   229,   149,   501,   241,   203,    50,
     508,   682,   204,    68,    50,   249,   417,   411,    17,   654,
     418,   412,   413,   250,    50,   685,   570,   414,   300,   686,
      11,   566,   205,   702,    50,   570,   567,   705,   531,   534,
     537,   540,   342,   569,    50,   326,    50,   150,   151,   327,
     330,   206,   569,    17,    68,   331,   666,   666,   557,   290,
     559,   750,   216,   715,   332,   293,   294,   292,    69,   328,
     333,    17,   590,    79,   337,   214,    70,   454,  -274,   558,
     697,   445,   699,   285,   823,   614,   603,   615,   329,   582,
    -274,   527,    88,    50,   583,   528,   203,   511,   209,   297,
     204,   521,   323,   523,   339,   744,   215,   216,   411,    69,
     250,   555,   412,   413,   407,   858,   859,    70,   414,    77,
     205,    86,   352,    95,   873,   104,   430,   431,   580,   874,
      50,   349,   581,   243,   388,   342,   694,  -274,   350,   206,
     314,  -274,   389,   216,   794,   351,   795,     7,     8,     9,
      10,    79,   398,    10,    92,   -82,    12,    13,    14,    12,
      13,    14,   341,   554,   150,   151,   745,   379,   293,   294,
     529,   532,    16,   538,    26,   754,   384,    17,    27,    30,
     555,    50,   380,   571,    31,   387,   809,   399,   810,    10,
     101,   466,   571,    32,   633,    12,    13,    14,    28,    33,
     402,   695,   406,   565,    77,   212,    95,   421,   113,   415,
     131,   497,   499,    17,   729,   776,   432,    29,   437,   314,
     728,   438,   317,    10,    97,   626,   416,  -381,  -381,    12,
      13,    14,   554,  -244,    50,   504,    50,   778,   506,   443,
     417,   212,   446,   298,   418,   447,   310,   635,   252,   254,
     312,   318,   216,    75,    80,    84,    89,   215,   216,    50,
     449,   111,   116,   120,   125,   269,   270,   271,   452,   466,
     466,   535,   466,    26,   453,   576,   576,    27,    30,   722,
     411,   470,   508,    31,   412,   413,   477,   216,   512,   673,
     414,   479,    32,   674,   677,   282,   496,    28,    33,   678,
     272,   273,   565,   319,   274,   275,   276,   277,   679,   748,
     216,   708,   498,   675,   680,   515,    29,     8,     9,    10,
      97,     8,     9,    10,    92,    12,    13,    14,   505,    12,
      13,    14,   676,   516,     8,     9,    10,   101,   750,   216,
     422,   423,    12,    13,    14,   426,   298,    17,   519,   640,
     641,   429,   525,   792,   793,   444,   398,   303,    50,   542,
      17,   214,   568,   575,   638,   579,   203,   435,   395,   774,
     204,   601,   605,     7,    50,   203,    10,    74,   609,   204,
     610,   784,    12,    13,    14,    10,   106,   790,   612,   -32,
     205,    12,    13,    14,   -33,   629,   636,   630,    16,   205,
      17,   631,    76,    81,    85,    90,  -247,   639,   642,   206,
     112,   117,   121,   126,   279,   655,   692,   475,   206,  -249,
      75,    80,    84,    89,   713,   811,   812,   643,   814,   644,
     645,   646,   647,   480,   663,   669,     7,     8,     9,    10,
      11,   553,   711,   482,   712,    12,    13,    14,   721,   723,
     724,    50,   725,   736,    93,    98,   102,   107,   737,   753,
      50,    16,   129,   134,   138,   143,    77,   773,    86,   780,
      95,   478,   104,   781,   113,   783,   122,   802,   131,   813,
     140,   758,   759,   804,   814,   834,   818,   822,   853,   837,
     838,    26,   840,   843,   842,    27,    30,   863,   731,   319,
     319,    31,   847,   862,   411,   573,   854,   856,   412,   413,
      32,   814,   872,   861,   414,    28,    33,   867,   868,   870,
     565,  -532,     8,     9,    10,    97,   875,   844,   876,   878,
      12,    13,    14,   877,    29,   473,   600,   409,   683,   684,
     604,   687,   688,   606,   607,   419,   420,   608,   149,   535,
     397,   395,   395,    75,    80,    84,    89,    94,    99,   103,
     108,   698,   512,   502,   770,   130,   135,   139,   144,    76,
      81,    85,    90,   471,   716,   797,    50,    78,    82,    87,
      91,    96,   100,   105,   109,   114,   118,   123,   127,   132,
     136,   141,   145,   150,   151,     8,     9,   283,   284,   535,
     791,   433,   293,   294,   762,   520,   764,  -248,   440,   518,
     820,   293,   294,   469,   652,   798,   800,   821,   857,   835,
     836,    93,    98,   102,   107,     7,   700,   855,    10,    79,
     703,   574,     0,     0,    12,    13,    14,   706,    75,    80,
       0,   578,   111,   116,   267,   268,   269,   270,   271,     0,
      16,   346,   347,     0,   395,   395,     8,     9,    10,   106,
       0,     0,   353,     0,    12,    13,    14,   356,   357,     0,
       0,     0,   362,   363,   364,   365,   366,   367,   368,   369,
     370,   371,   372,   373,     0,     0,     0,   746,   747,     0,
       0,   752,     0,     0,     0,     0,   755,   756,     0,   757,
       0,     0,    76,    81,    85,    90,   315,     0,     0,   -20,
     -20,   -20,   -20,   -20,   405,     0,     0,     0,   -20,   -20,
     -20,     0,     0,     0,    94,    99,   103,   108,     0,   761,
       0,     0,   763,   214,   -20,     0,  -274,     0,   749,   751,
       7,  -250,   289,    10,    88,     0,     0,     0,  -274,    12,
      13,    14,     0,   775,    93,    98,   102,   107,     7,     8,
       9,    10,    74,     0,     0,    16,     0,    12,    13,    14,
       0,     7,   450,   451,    10,    83,   298,     0,   298,     0,
      12,    13,    14,    16,     0,    17,     0,    76,    81,     0,
       0,   112,   117,     0,     0,  -274,    16,     0,    17,  -274,
     -20,     0,     0,     0,     0,   303,     0,     0,  -471,  -471,
    -471,  -471,  -471,   491,   492,     0,     0,  -471,  -471,  -471,
       0,     0,   395,   395,     0,   825,   826,     0,   827,     0,
       0,   395,   395,  -471,     0,   395,   395,    93,    98,     0,
       0,   129,   134,     0,  -243,     0,     0,     0,     0,   517,
       0,     0,     0,     0,   522,     0,     0,    94,    99,   103,
     108,   526,     7,     8,     9,    10,    88,     0,   749,   751,
     751,    12,    13,    14,     0,     0,   303,     0,     0,  -109,
    -109,  -109,  -109,  -109,     0,     0,     0,    16,  -109,  -109,
    -109,   264,   265,   266,   267,   268,   269,   270,   271,  -106,
      75,    80,    84,    89,  -109,     0,     0,     0,   111,   116,
     120,   125,   593,     0,     0,     0,     0,     0,     0,   598,
       0,     0,     0,   602,   261,   262,   263,   264,   265,   266,
     267,   268,   269,   270,   271,     0,   618,   289,     0,     0,
      94,    99,     0,     0,   130,   135,     0,     0,  -246,     7,
       8,     9,    10,    83,     0,     0,     0,   628,    12,    13,
      14,     0,    78,    82,    96,   100,   114,   118,   132,   136,
    -109,     0,     0,     0,    16,     0,    17,     0,     0,     0,
       0,     0,   213,     0,     0,   -28,   -28,   -28,   -28,   -28,
       8,     9,    10,    92,   -28,   -28,   -28,     0,    12,    13,
      14,   265,   266,   267,   268,   269,   270,   271,   593,   214,
     -28,   701,  -274,     0,     0,   704,    17,     0,     0,     0,
       0,     0,     0,   707,  -274,     0,     0,   709,     0,     0,
       0,     0,   613,     0,   159,  -245,   618,   718,     0,     0,
     160,   161,     0,   162,     0,     0,     0,   215,   216,    76,
      81,    85,    90,     0,     0,     0,     0,   112,   117,   121,
     126,   163,     0,    18,     0,   164,   165,   166,   167,   168,
       0,  -274,     0,   169,  -317,  -274,   -28,     0,     0,   170,
       0,     0,   171,     0,     0,     0,     0,   172,   173,   174,
       0,     0,   175,   176,     0,     0,  -317,   177,  -317,     0,
       0,    93,    98,   102,   107,   591,     0,   159,   618,   129,
     134,   138,   143,   160,   161,     0,   162,     0,     0,     0,
       0,     0,   618,   178,   179,   618,   616,   618,     0,     0,
       0,   772,     0,     0,   163,     0,    18,     0,   164,   165,
     166,   167,   168,     0,   324,     0,   169,     7,     0,     0,
      10,    11,   170,     0,     0,   171,    12,    13,    14,     0,
     172,   173,   174,     0,     0,   175,   176,     0,     0,     0,
     177,     0,    16,     0,    17,    18,     0,     0,   803,     0,
       0,   618,     0,     0,     0,     8,     9,    10,   128,     0,
       0,     0,     0,    12,    13,    14,   178,   179,     0,   592,
       8,     9,    10,   133,    94,    99,   103,   108,    12,    13,
      14,    17,   130,   135,   139,   144,     0,     0,     0,     0,
       0,     0,     0,     0,    78,    82,    87,    91,    96,   100,
     105,   109,   114,   118,   123,   127,   132,   136,   141,   145,
     278,     0,  -447,  -447,  -447,  -447,  -447,  -447,  -447,  -447,
       0,  -447,  -447,  -447,  -447,  -447,     0,  -447,  -447,  -447,
    -447,  -447,  -447,  -447,  -447,  -447,  -447,  -447,  -447,  -447,
    -447,  -447,  -447,  -447,  -447,  -447,  -447,  -447,     0,     0,
       0,  -447,     0,     0,     0,  -256,     0,  -447,     0,     0,
    -447,     0,     0,     0,     0,  -447,  -447,  -447,     0,     0,
    -447,  -447,     0,     0,     0,  -447,     8,     9,    10,   142,
       0,     0,     0,     0,    12,    13,    14,     8,     9,    10,
     101,     0,     0,     0,     0,    12,    13,    14,     0,  -447,
     279,  -447,  -447,   456,  -447,  -471,  -471,  -471,  -471,  -471,
    -471,  -471,  -471,    17,  -471,  -471,  -471,  -471,  -471,     0,
    -471,  -471,  -471,  -471,  -471,  -471,  -471,  -471,  -471,  -471,
    -471,  -471,  -471,  -471,  -471,     0,  -471,  -471,  -471,  -471,
    -471,     0,     0,     0,  -471,     7,     8,     9,    10,    11,
    -471,     0,     0,  -471,    12,    13,    14,     0,  -471,  -471,
    -471,  -258,     0,  -471,  -471,     0,     0,     0,  -471,     0,
      16,     0,   480,     0,   481,     7,     8,     9,    10,    11,
       0,     0,   482,     0,    12,    13,    14,     0,     0,     0,
       0,     0,  -471,     0,  -471,  -471,   738,  -471,  -449,  -449,
      16,     0,     0,     0,  -449,  -449,     0,  -449,     0,     0,
       0,  -449,     0,  -449,  -449,  -449,  -449,  -449,  -449,  -449,
    -449,  -449,  -449,  -449,     0,  -449,     0,  -449,     0,  -449,
    -449,  -449,  -449,  -449,     0,     0,     0,  -449,     7,     8,
       9,    10,    11,  -449,     0,   672,  -449,    12,    13,    14,
       0,  -449,  -449,  -449,     0,     0,  -449,  -449,     0,     0,
    -532,  -449,     0,    16,     0,    17,     0,     0,     0,     7,
       8,     9,    10,   110,     8,     9,    10,   133,    12,    13,
      14,     0,    12,    13,    14,  -449,     0,  -449,  -449,   785,
    -449,  -477,  -477,     0,    16,     0,    17,  -477,  -477,     0,
    -477,     0,     0,     0,  -477,     0,  -477,  -477,  -477,  -477,
    -477,  -477,  -477,  -477,  -477,  -477,  -477,     0,  -477,     0,
    -477,     0,  -477,  -477,  -477,  -477,  -477,     0,     0,     0,
    -477,     0,     8,     9,    10,   106,  -477,     0,     0,  -477,
      12,    13,    14,     0,  -477,  -477,  -477,     0,     0,  -477,
    -477,     0,     0,     0,  -477,  -251,   244,     0,   159,     7,
       0,     0,    10,    11,   160,   161,     0,   162,    12,    13,
      14,     0,     0,     0,     0,     0,     0,     0,  -477,     0,
    -477,  -477,     0,  -477,    16,   163,    17,    18,     0,   164,
     165,   166,   167,   168,     0,     0,   613,   169,   543,   151,
       0,     0,     0,   170,   160,   161,   171,   162,     0,     0,
       0,   172,   173,   174,     0,     0,   175,   176,     0,     0,
       0,   177,     0,     0,     0,   163,     0,    18,     0,   164,
     165,   166,   167,   168,     0,     0,     0,   169,     0,     0,
       0,     0,     0,   170,     0,     0,   171,   178,   179,     0,
     245,   172,   173,   174,     0,     0,   175,   176,     0,     0,
     614,   177,   615,   613,     0,   543,   151,     0,     0,     0,
       0,   160,   161,     0,   162,     8,     9,    10,   128,     0,
       0,     0,     0,    12,    13,    14,  -304,   178,   179,     0,
     616,     0,   163,     0,    18,     0,   164,   165,   166,   167,
     168,    17,     0,     0,   169,     0,     8,     9,    10,   142,
     170,     0,     0,   171,    12,    13,    14,     0,   172,   173,
     174,     0,     0,   175,   176,     0,     0,   614,   177,   615,
     613,     0,   159,     0,     0,     0,     0,     0,   160,   161,
       0,   162,     8,     9,    10,   137,     0,     0,     0,     0,
      12,    13,    14,  -370,   178,   179,     0,   616,     0,   163,
    -255,    18,     0,   164,   165,   166,   167,   168,    17,   321,
       0,   169,   -24,   -24,   -24,   -24,   -24,   170,     0,     0,
     171,   -24,   -24,   -24,     0,   172,   173,   174,     0,     0,
     175,   176,     0,     0,     0,   177,   214,   -24,   159,  -274,
       0,     0,     0,     0,   160,   161,     0,   162,     0,     0,
       0,  -274,   263,   264,   265,   266,   267,   268,   269,   270,
     271,   178,   179,     0,   616,   163,     0,    18,     0,   164,
     165,   166,   167,   168,   215,   216,     0,   169,     0,     0,
       0,   159,     0,   170,     0,     0,   171,   160,   161,     0,
     162,   172,   173,   403,     0,     0,   175,   176,  -274,     0,
       0,   177,  -274,   -24,     0,     0,     0,     0,   163,     0,
      18,     0,   164,   165,   166,   167,   168,     0,     0,     0,
     169,     0,     0,     0,     0,     0,   170,   178,   179,   171,
       0,   404,     0,     0,   172,   173,   174,     0,     0,   175,
     176,     0,     0,     0,   177,   262,   263,   264,   265,   266,
     267,   268,   269,   270,   271,     0,   543,   544,     8,     9,
      10,    11,   160,   161,     0,   162,    12,    13,    14,     0,
     178,   179,     0,     0,   493,   545,   546,   547,   548,   549,
     550,   551,    16,   163,    17,    18,     0,   164,   165,   166,
     167,   168,     0,     0,     0,   169,     7,     8,     9,    10,
     119,   170,     0,     0,   171,    12,    13,    14,     0,   172,
     173,   174,     0,     0,   175,   176,     0,     0,     0,   177,
       0,    16,     0,    17,     0,     0,     0,     0,     0,     0,
     543,   151,     0,     0,     0,     0,   160,   161,     0,   162,
       0,     0,     0,   552,     0,   178,   179,     0,   553,   545,
     546,   547,   548,   549,   550,   551,     0,   163,     0,    18,
       0,   164,   165,   166,   167,   168,     0,     0,     0,   169,
     159,     0,     0,     0,     0,   170,   160,   161,   171,   162,
       0,     0,  -253,   172,   173,   174,     0,     0,   175,   176,
       0,     0,     0,   177,     0,     0,     0,   163,     0,    18,
       0,   164,   165,   166,   167,   168,     0,     0,     0,   169,
       0,     0,     0,     0,     0,   170,     0,   552,   171,   178,
     179,     0,   553,   172,   173,   174,     0,     0,   175,   176,
       0,     0,     0,   177,     0,     0,     0,   159,     7,     8,
       9,    10,    11,   160,   161,     0,   162,    12,    13,    14,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   178,
     179,     0,   444,    16,   163,    17,    18,     0,   164,   165,
     166,   167,   168,     0,     0,     0,   169,     0,     8,     9,
      10,   137,   170,     0,     0,   171,    12,    13,    14,     0,
     172,   173,   174,     0,     0,   175,   176,     0,     0,     0,
     177,     0,   159,     7,    17,     0,    10,    11,   160,   161,
       0,   162,    12,    13,    14,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   178,   179,    16,   163,
      17,    18,     0,   164,   165,   166,   167,   168,     0,     0,
       0,   169,   159,     0,     0,     0,     0,   170,   160,   161,
     171,   162,     0,     0,     0,   172,   173,   174,     0,     0,
     175,   176,     0,  -257,     0,   177,     0,     0,     0,   163,
       0,    18,     0,   164,   165,   166,   167,   168,     0,     0,
       0,   169,     0,     0,     0,     0,     0,   170,     0,     0,
     171,   178,   179,     0,     0,   172,   173,   174,     0,     0,
     175,   176,     0,     0,     0,   177,   159,     0,     0,     0,
       0,     0,   160,   161,     0,   162,     0,     7,     8,     9,
      10,   115,     0,     0,     0,     0,    12,    13,    14,   632,
       0,   178,   179,   163,     0,    18,     0,   164,   165,   166,
     167,   168,    16,     0,     0,   169,   159,     0,     0,     0,
       0,   170,   160,   161,   171,   162,     0,     0,     0,   172,
     173,   174,     0,     0,   175,   176,     0,     0,     0,   177,
       0,     0,     0,   163,     0,    18,     0,   164,   165,   166,
     167,   168,     0,     0,     0,   169,     0,     0,     0,     0,
       0,   170,     0,     0,   171,   178,   179,     0,     0,   172,
     173,   174,     0,  -252,   175,   176,     0,     0,     0,   251,
     159,     0,     0,     0,     0,     0,   160,   161,     0,   162,
       0,     7,     8,     9,    10,   124,     0,     0,     0,     0,
      12,    13,    14,     0,     0,   178,   179,   163,     0,    18,
       0,   164,   165,   166,   167,   168,    16,     0,     0,   169,
     472,     0,     0,     0,     0,   170,   160,   161,   171,   162,
       0,     0,     0,   172,   173,   174,     0,     0,   175,   176,
       0,     0,     0,   253,     0,     0,     0,   163,     0,    18,
       0,   164,   165,   166,   167,   168,     0,     0,     0,   169,
       0,     0,     0,     0,     0,   170,     0,     0,   171,   178,
     179,     0,     0,   172,   173,   174,     0,  -254,   175,   176,
       0,     0,     0,   177,   324,     0,     0,     7,     0,     0,
      10,    11,     0,     0,     0,     0,    12,    13,    14,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   178,
     179,     0,    16,     0,    17,    18,     0,     0,     6,   766,
    -118,     7,     8,     9,    10,    11,     0,     0,     0,     0,
      12,    13,    14,   260,   261,   262,   263,   264,   265,   266,
     267,   268,   269,   270,   271,    15,    16,     0,    17,    18,
       0,   255,   256,   257,     0,   258,   259,   260,   261,   262,
     263,   264,   265,   266,   267,   268,   269,   270,   271,     0,
       0,     0,     0,   325,  -373,  -118,     0,     0,     0,     0,
     255,   256,   257,  -118,   258,   259,   260,   261,   262,   263,
     264,   265,   266,   267,   268,   269,   270,   271,     0,     0,
       0,     0,     0,     0,   767,     0,     0,    19,   255,   256,
     257,     0,   258,   259,   260,   261,   262,   263,   264,   265,
     266,   267,   268,   269,   270,   271,     0,     0,     0,     0,
     255,   256,   257,   494,   258,   259,   260,   261,   262,   263,
     264,   265,   266,   267,   268,   269,   270,   271,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   255,   256,
     257,   584,   258,   259,   260,   261,   262,   263,   264,   265,
     266,   267,   268,   269,   270,   271,     0,     0,     0,     0,
     255,   256,   257,   585,   258,   259,   260,   261,   262,   263,
     264,   265,   266,   267,   268,   269,   270,   271,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   255,   256,
     257,   828,   258,   259,   260,   261,   262,   263,   264,   265,
     266,   267,   268,   269,   270,   271,     0,     0,   255,   256,
     257,   441,   258,   259,   260,   261,   262,   263,   264,   265,
     266,   267,   268,   269,   270,   271,     0,     0,   719,     0,
       0,     0,     0,     0,     0,     0,   255,   256,   257,   442,
     258,   259,   260,   261,   262,   263,   264,   265,   266,   267,
     268,   269,   270,   271,     0,     0,     0,     0,     0,   611,
     255,   256,   257,   720,   258,   259,   260,   261,   262,   263,
     264,   265,   266,   267,   268,   269,   270,   271,    17,     0,
       0,     0,   765,     0,     0,     0,     0,     0,     0,     0,
     255,   256,   257,     0,   258,   259,   260,   261,   262,   263,
     264,   265,   266,   267,   268,   269,   270,   271,     7,     8,
       9,    10,    74,     0,     0,     0,     0,    12,    13,    14,
       7,     8,     9,    10,   110,     0,     0,     0,     0,    12,
      13,    14,     0,    16,     0,    17,     0,     0,     0,     7,
       8,     9,    10,    83,     0,    16,     0,    17,    12,    13,
      14,     7,     8,     9,    10,   119,     0,     0,     0,     0,
      12,    13,    14,     0,    16,     0,    17,     0,     0,     0,
       7,     0,     0,    10,    11,     0,    16,     0,    17,    12,
      13,    14,     7,     8,     9,    10,    79,     0,     0,     0,
       0,    12,    13,    14,     0,    16,     0,    17,     0,     7,
       8,     9,    10,   115,     0,     0,     0,    16,    12,    13,
      14,     7,     8,     9,    10,    88,     0,     0,     0,     0,
      12,    13,    14,     0,    16,     0,     0,     0,     7,     8,
       9,    10,   124,     0,     0,     0,    16,    12,    13,    14,
     259,   260,   261,   262,   263,   264,   265,   266,   267,   268,
     269,   270,   271,    16,   255,   256,   257,   806,   258,   259,
     260,   261,   262,   263,   264,   265,   266,   267,   268,   269,
     270,   271,   255,   256,   257,     0,   258,   259,   260,   261,
     262,   263,   264,   265,   266,   267,   268,   269,   270,   271,
     257,     0,   258,   259,   260,   261,   262,   263,   264,   265,
     266,   267,   268,   269,   270,   271
};

static const short yycheck[] =
{
      65,     3,    67,     5,   318,     3,     3,     5,     5,   225,
     147,     3,   309,     5,   216,   316,   146,   275,   291,   221,
       3,   322,     5,   466,   565,     3,     3,     5,     5,   309,
       3,   466,     5,   466,   497,   572,   314,   734,   216,   306,
      53,    54,    55,   309,     3,   474,     5,   309,   624,   466,
     578,   154,   466,   231,   696,     1,    58,     1,    89,    10,
      58,    58,   474,    94,     1,     1,    58,    69,   710,     1,
      90,    69,     3,     4,    94,    58,     3,     4,     3,     4,
      58,    58,     3,     4,     0,    58,     3,     0,     5,   786,
      46,    69,   535,     1,     0,    12,    13,    14,   300,    58,
       1,     5,    31,   400,   839,     8,     3,     4,    90,    26,
      69,    28,    94,    30,   381,    32,    67,    34,   535,    36,
     400,    38,    66,    40,    42,    43,    66,   177,    31,   387,
     565,   866,   565,    89,   400,    50,    46,    58,   400,   715,
      31,    58,     3,   586,   841,    91,   581,    62,    63,    64,
      67,   565,    69,   850,    58,    91,    46,   170,    95,   337,
      90,    58,    77,    95,    95,   577,   578,   581,    95,    66,
      95,    86,    25,    31,    28,    66,    67,   640,   191,    89,
      95,     3,   879,    91,   164,   165,   388,   381,    46,   104,
      91,   241,     8,   769,   174,    66,   463,    58,   113,    89,
     180,   251,   475,   253,    66,    66,    67,   122,     3,     4,
     190,   276,    66,    67,   216,    31,   131,    28,   216,   221,
      31,    66,   235,   221,    90,   140,     3,     4,    94,   231,
     758,   759,    43,   231,   231,   338,    58,   340,   216,   231,
      90,    91,   783,   221,    66,    67,    66,    67,   231,    95,
     381,    46,    95,   231,   231,    66,    67,   216,   231,   796,
     177,   274,   221,    58,   277,   459,   460,   461,   462,    89,
      95,    66,   231,    66,    67,   572,   193,    66,   195,    90,
     197,    58,   199,    94,   581,     8,   203,   503,   205,    66,
      67,    66,   572,   358,   147,   210,    89,    66,   300,   216,
     430,   581,   300,     3,   221,    89,   572,   309,    31,   567,
     572,   309,   309,    94,   231,   581,   745,   309,     6,   581,
       8,    89,   300,   601,   241,   754,    94,   605,   459,   460,
     461,   462,   345,   745,   251,   337,   253,     3,     4,   337,
     337,   300,   754,    31,     3,   337,   758,   759,   783,    89,
     783,    66,    67,    43,   337,   208,   209,    89,    58,   337,
     337,    31,   499,     8,   337,    28,    66,   380,    31,   783,
     595,   351,   597,   386,    89,    65,    46,    67,   337,    89,
      43,    90,     8,   300,    94,    94,   388,    46,    70,    66,
     388,   441,    91,   443,    91,    89,    66,    67,   400,    58,
      94,   466,   400,   400,   304,   848,   849,    66,   400,   326,
     388,   328,    90,   330,    89,   332,   334,   335,    90,    94,
     337,    89,    94,   403,    58,   438,    90,    90,    89,   388,
      94,    94,    66,    67,   748,    89,   750,     4,     5,     6,
       7,     8,   295,     7,     8,    46,    13,    14,    15,    13,
      14,    15,     1,   466,     3,     4,   658,    89,   311,   312,
     459,   460,    29,   462,   466,   667,    89,    31,   466,   466,
     535,   388,    33,   746,   466,    66,   777,    10,   779,     7,
       8,   381,   755,   466,   549,    13,    14,    15,   466,   466,
       6,    90,    90,   466,   411,    94,   413,    43,   415,   796,
     417,   419,   420,    31,   641,    90,    90,   466,    43,    94,
     640,    94,   728,     7,     8,   528,   796,    90,    91,    13,
      14,    15,   535,    90,   441,   425,   443,    90,   428,    94,
     796,    94,    89,   215,   796,    89,   218,   550,   184,   185,
     222,    66,    67,    26,    27,    28,    29,    66,    67,   466,
      46,    34,    35,    36,    37,    58,    59,    60,    89,   459,
     460,   461,   462,   565,    96,   483,   484,   565,   565,   634,
     572,    89,   702,   565,   572,   572,    66,    67,   431,   581,
     572,    89,   565,   581,   581,   565,    90,   565,   565,   581,
      61,    62,   565,   226,    65,    66,    67,    68,   581,    66,
      67,   614,    90,   581,   581,    91,   565,     5,     6,     7,
       8,     5,     6,     7,     8,    13,    14,    15,    43,    13,
      14,    15,   581,    91,     5,     6,     7,     8,    66,    67,
     311,   312,    13,    14,    15,   317,   318,    31,    91,   557,
     558,   323,    46,   745,   746,    95,   499,     1,   565,    91,
      31,    28,    89,    89,   554,    89,   658,   339,   291,   724,
     658,    94,    94,     4,   581,   667,     7,     8,    91,   667,
      89,   736,    13,    14,    15,     7,     8,   742,    89,    46,
     658,    13,    14,    15,    46,    46,     8,    90,    29,   667,
      31,    90,    26,    27,    28,    29,    90,    90,    90,   658,
      34,    35,    36,    37,    91,    89,     3,   389,   667,    90,
     193,   194,   195,   196,    94,   780,   781,    16,   783,    18,
      19,    20,    21,     1,   577,   578,     4,     5,     6,     7,
       8,    95,    46,    11,    91,    13,    14,    15,    90,    90,
      66,   658,    46,    66,    30,    31,    32,    33,    17,   666,
     667,    29,    38,    39,    40,    41,   673,    90,   675,    66,
     677,   394,   679,    66,   681,    66,   683,    91,   685,    18,
     687,   689,   690,    91,   839,    90,    89,    89,   843,    89,
      89,   783,    90,    66,    90,   783,   783,    10,   641,   422,
     423,   783,    94,    89,   796,   477,    96,    90,   796,   796,
     783,   866,   867,    90,   796,   783,   783,    66,    10,    90,
     783,    89,     5,     6,     7,     8,    89,   830,    89,    10,
      13,    14,    15,    90,   783,   387,   508,   307,   581,   581,
     512,   581,   581,   515,   516,   309,   309,   519,   753,   739,
     295,   474,   475,   326,   327,   328,   329,    30,    31,    32,
      33,   596,   705,   424,   713,    38,    39,    40,    41,   193,
     194,   195,   196,   386,   623,   758,   783,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,     3,     4,     5,     6,     7,     8,   789,
     745,   337,   745,   746,   702,   440,   705,    90,   345,   438,
     787,   754,   755,   382,   561,   758,   759,   789,   847,   809,
     810,   197,   198,   199,   200,     4,   598,   845,     7,     8,
     602,   477,    -1,    -1,    13,    14,    15,   609,   411,   412,
      -1,   484,   415,   416,    56,    57,    58,    59,    60,    -1,
      29,   239,   240,    -1,   577,   578,     5,     6,     7,     8,
      -1,    -1,   250,    -1,    13,    14,    15,   255,   256,    -1,
      -1,    -1,   260,   261,   262,   263,   264,   265,   266,   267,
     268,   269,   270,   271,    -1,    -1,    -1,   659,   660,    -1,
      -1,   663,    -1,    -1,    -1,    -1,   668,   669,    -1,   671,
      -1,    -1,   326,   327,   328,   329,     1,    -1,    -1,     4,
       5,     6,     7,     8,   302,    -1,    -1,    -1,    13,    14,
      15,    -1,    -1,    -1,   197,   198,   199,   200,    -1,   701,
      -1,    -1,   704,    28,    29,    -1,    31,    -1,   661,   662,
       4,    90,   191,     7,     8,    -1,    -1,    -1,    43,    13,
      14,    15,    -1,   725,   330,   331,   332,   333,     4,     5,
       6,     7,     8,    -1,    -1,    29,    -1,    13,    14,    15,
      -1,     4,   360,   361,     7,     8,   748,    -1,   750,    -1,
      13,    14,    15,    29,    -1,    31,    -1,   411,   412,    -1,
      -1,   415,   416,    -1,    -1,    90,    29,    -1,    31,    94,
      95,    -1,    -1,    -1,    -1,     1,    -1,    -1,     4,     5,
       6,     7,     8,   401,   402,    -1,    -1,    13,    14,    15,
      -1,    -1,   745,   746,    -1,   797,   798,    -1,   800,    -1,
      -1,   754,   755,    29,    -1,   758,   759,   413,   414,    -1,
      -1,   417,   418,    -1,    90,    -1,    -1,    -1,    -1,   437,
      -1,    -1,    -1,    -1,   442,    -1,    -1,   330,   331,   332,
     333,   449,     4,     5,     6,     7,     8,    -1,   791,   792,
     793,    13,    14,    15,    -1,    -1,     1,    -1,    -1,     4,
       5,     6,     7,     8,    -1,    -1,    -1,    29,    13,    14,
      15,    53,    54,    55,    56,    57,    58,    59,    60,    95,
     673,   674,   675,   676,    29,    -1,    -1,    -1,   681,   682,
     683,   684,   500,    -1,    -1,    -1,    -1,    -1,    -1,   507,
      -1,    -1,    -1,   511,    50,    51,    52,    53,    54,    55,
      56,    57,    58,    59,    60,    -1,   524,   386,    -1,    -1,
     413,   414,    -1,    -1,   417,   418,    -1,    -1,    90,     4,
       5,     6,     7,     8,    -1,    -1,    -1,   545,    13,    14,
      15,    -1,   411,   412,   413,   414,   415,   416,   417,   418,
      95,    -1,    -1,    -1,    29,    -1,    31,    -1,    -1,    -1,
      -1,    -1,     1,    -1,    -1,     4,     5,     6,     7,     8,
       5,     6,     7,     8,    13,    14,    15,    -1,    13,    14,
      15,    54,    55,    56,    57,    58,    59,    60,   596,    28,
      29,   599,    31,    -1,    -1,   603,    31,    -1,    -1,    -1,
      -1,    -1,    -1,   611,    43,    -1,    -1,   615,    -1,    -1,
      -1,    -1,     1,    -1,     3,    90,   624,   625,    -1,    -1,
       9,    10,    -1,    12,    -1,    -1,    -1,    66,    67,   673,
     674,   675,   676,    -1,    -1,    -1,    -1,   681,   682,   683,
     684,    30,    -1,    32,    -1,    34,    35,    36,    37,    38,
      -1,    90,    -1,    42,    43,    94,    95,    -1,    -1,    48,
      -1,    -1,    51,    -1,    -1,    -1,    -1,    56,    57,    58,
      -1,    -1,    61,    62,    -1,    -1,    65,    66,    67,    -1,
      -1,   677,   678,   679,   680,     1,    -1,     3,   696,   685,
     686,   687,   688,     9,    10,    -1,    12,    -1,    -1,    -1,
      -1,    -1,   710,    92,    93,   713,    95,   715,    -1,    -1,
      -1,   719,    -1,    -1,    30,    -1,    32,    -1,    34,    35,
      36,    37,    38,    -1,     1,    -1,    42,     4,    -1,    -1,
       7,     8,    48,    -1,    -1,    51,    13,    14,    15,    -1,
      56,    57,    58,    -1,    -1,    61,    62,    -1,    -1,    -1,
      66,    -1,    29,    -1,    31,    32,    -1,    -1,   766,    -1,
      -1,   769,    -1,    -1,    -1,     5,     6,     7,     8,    -1,
      -1,    -1,    -1,    13,    14,    15,    92,    93,    -1,    95,
       5,     6,     7,     8,   677,   678,   679,   680,    13,    14,
      15,    31,   685,   686,   687,   688,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   673,   674,   675,   676,   677,   678,
     679,   680,   681,   682,   683,   684,   685,   686,   687,   688,
       1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      -1,    12,    13,    14,    15,    16,    -1,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    -1,    -1,
      -1,    42,    -1,    -1,    -1,    90,    -1,    48,    -1,    -1,
      51,    -1,    -1,    -1,    -1,    56,    57,    58,    -1,    -1,
      61,    62,    -1,    -1,    -1,    66,     5,     6,     7,     8,
      -1,    -1,    -1,    -1,    13,    14,    15,     5,     6,     7,
       8,    -1,    -1,    -1,    -1,    13,    14,    15,    -1,    90,
      91,    92,    93,     1,    95,     3,     4,     5,     6,     7,
       8,     9,    10,    31,    12,    13,    14,    15,    16,    -1,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    -1,    34,    35,    36,    37,
      38,    -1,    -1,    -1,    42,     4,     5,     6,     7,     8,
      48,    -1,    -1,    51,    13,    14,    15,    -1,    56,    57,
      58,    90,    -1,    61,    62,    -1,    -1,    -1,    66,    -1,
      29,    -1,     1,    -1,     3,     4,     5,     6,     7,     8,
      -1,    -1,    11,    -1,    13,    14,    15,    -1,    -1,    -1,
      -1,    -1,    90,    -1,    92,    93,     1,    95,     3,     4,
      29,    -1,    -1,    -1,     9,    10,    -1,    12,    -1,    -1,
      -1,    16,    -1,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    -1,    30,    -1,    32,    -1,    34,
      35,    36,    37,    38,    -1,    -1,    -1,    42,     4,     5,
       6,     7,     8,    48,    -1,    11,    51,    13,    14,    15,
      -1,    56,    57,    58,    -1,    -1,    61,    62,    -1,    -1,
      89,    66,    -1,    29,    -1,    31,    -1,    -1,    -1,     4,
       5,     6,     7,     8,     5,     6,     7,     8,    13,    14,
      15,    -1,    13,    14,    15,    90,    -1,    92,    93,     1,
      95,     3,     4,    -1,    29,    -1,    31,     9,    10,    -1,
      12,    -1,    -1,    -1,    16,    -1,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    -1,    30,    -1,
      32,    -1,    34,    35,    36,    37,    38,    -1,    -1,    -1,
      42,    -1,     5,     6,     7,     8,    48,    -1,    -1,    51,
      13,    14,    15,    -1,    56,    57,    58,    -1,    -1,    61,
      62,    -1,    -1,    -1,    66,    90,     1,    -1,     3,     4,
      -1,    -1,     7,     8,     9,    10,    -1,    12,    13,    14,
      15,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,
      92,    93,    -1,    95,    29,    30,    31,    32,    -1,    34,
      35,    36,    37,    38,    -1,    -1,     1,    42,     3,     4,
      -1,    -1,    -1,    48,     9,    10,    51,    12,    -1,    -1,
      -1,    56,    57,    58,    -1,    -1,    61,    62,    -1,    -1,
      -1,    66,    -1,    -1,    -1,    30,    -1,    32,    -1,    34,
      35,    36,    37,    38,    -1,    -1,    -1,    42,    -1,    -1,
      -1,    -1,    -1,    48,    -1,    -1,    51,    92,    93,    -1,
      95,    56,    57,    58,    -1,    -1,    61,    62,    -1,    -1,
      65,    66,    67,     1,    -1,     3,     4,    -1,    -1,    -1,
      -1,     9,    10,    -1,    12,     5,     6,     7,     8,    -1,
      -1,    -1,    -1,    13,    14,    15,    91,    92,    93,    -1,
      95,    -1,    30,    -1,    32,    -1,    34,    35,    36,    37,
      38,    31,    -1,    -1,    42,    -1,     5,     6,     7,     8,
      48,    -1,    -1,    51,    13,    14,    15,    -1,    56,    57,
      58,    -1,    -1,    61,    62,    -1,    -1,    65,    66,    67,
       1,    -1,     3,    -1,    -1,    -1,    -1,    -1,     9,    10,
      -1,    12,     5,     6,     7,     8,    -1,    -1,    -1,    -1,
      13,    14,    15,    91,    92,    93,    -1,    95,    -1,    30,
      90,    32,    -1,    34,    35,    36,    37,    38,    31,     1,
      -1,    42,     4,     5,     6,     7,     8,    48,    -1,    -1,
      51,    13,    14,    15,    -1,    56,    57,    58,    -1,    -1,
      61,    62,    -1,    -1,    -1,    66,    28,    29,     3,    31,
      -1,    -1,    -1,    -1,     9,    10,    -1,    12,    -1,    -1,
      -1,    43,    52,    53,    54,    55,    56,    57,    58,    59,
      60,    92,    93,    -1,    95,    30,    -1,    32,    -1,    34,
      35,    36,    37,    38,    66,    67,    -1,    42,    -1,    -1,
      -1,     3,    -1,    48,    -1,    -1,    51,     9,    10,    -1,
      12,    56,    57,    58,    -1,    -1,    61,    62,    90,    -1,
      -1,    66,    94,    95,    -1,    -1,    -1,    -1,    30,    -1,
      32,    -1,    34,    35,    36,    37,    38,    -1,    -1,    -1,
      42,    -1,    -1,    -1,    -1,    -1,    48,    92,    93,    51,
      -1,    96,    -1,    -1,    56,    57,    58,    -1,    -1,    61,
      62,    -1,    -1,    -1,    66,    51,    52,    53,    54,    55,
      56,    57,    58,    59,    60,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    -1,    12,    13,    14,    15,    -1,
      92,    93,    -1,    -1,    96,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    -1,    34,    35,    36,
      37,    38,    -1,    -1,    -1,    42,     4,     5,     6,     7,
       8,    48,    -1,    -1,    51,    13,    14,    15,    -1,    56,
      57,    58,    -1,    -1,    61,    62,    -1,    -1,    -1,    66,
      -1,    29,    -1,    31,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,    -1,    -1,    -1,    -1,     9,    10,    -1,    12,
      -1,    -1,    -1,    90,    -1,    92,    93,    -1,    95,    22,
      23,    24,    25,    26,    27,    28,    -1,    30,    -1,    32,
      -1,    34,    35,    36,    37,    38,    -1,    -1,    -1,    42,
       3,    -1,    -1,    -1,    -1,    48,     9,    10,    51,    12,
      -1,    -1,    90,    56,    57,    58,    -1,    -1,    61,    62,
      -1,    -1,    -1,    66,    -1,    -1,    -1,    30,    -1,    32,
      -1,    34,    35,    36,    37,    38,    -1,    -1,    -1,    42,
      -1,    -1,    -1,    -1,    -1,    48,    -1,    90,    51,    92,
      93,    -1,    95,    56,    57,    58,    -1,    -1,    61,    62,
      -1,    -1,    -1,    66,    -1,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    -1,    12,    13,    14,    15,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,
      93,    -1,    95,    29,    30,    31,    32,    -1,    34,    35,
      36,    37,    38,    -1,    -1,    -1,    42,    -1,     5,     6,
       7,     8,    48,    -1,    -1,    51,    13,    14,    15,    -1,
      56,    57,    58,    -1,    -1,    61,    62,    -1,    -1,    -1,
      66,    -1,     3,     4,    31,    -1,     7,     8,     9,    10,
      -1,    12,    13,    14,    15,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    92,    93,    29,    30,
      31,    32,    -1,    34,    35,    36,    37,    38,    -1,    -1,
      -1,    42,     3,    -1,    -1,    -1,    -1,    48,     9,    10,
      51,    12,    -1,    -1,    -1,    56,    57,    58,    -1,    -1,
      61,    62,    -1,    90,    -1,    66,    -1,    -1,    -1,    30,
      -1,    32,    -1,    34,    35,    36,    37,    38,    -1,    -1,
      -1,    42,    -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,
      51,    92,    93,    -1,    -1,    56,    57,    58,    -1,    -1,
      61,    62,    -1,    -1,    -1,    66,     3,    -1,    -1,    -1,
      -1,    -1,     9,    10,    -1,    12,    -1,     4,     5,     6,
       7,     8,    -1,    -1,    -1,    -1,    13,    14,    15,    90,
      -1,    92,    93,    30,    -1,    32,    -1,    34,    35,    36,
      37,    38,    29,    -1,    -1,    42,     3,    -1,    -1,    -1,
      -1,    48,     9,    10,    51,    12,    -1,    -1,    -1,    56,
      57,    58,    -1,    -1,    61,    62,    -1,    -1,    -1,    66,
      -1,    -1,    -1,    30,    -1,    32,    -1,    34,    35,    36,
      37,    38,    -1,    -1,    -1,    42,    -1,    -1,    -1,    -1,
      -1,    48,    -1,    -1,    51,    92,    93,    -1,    -1,    56,
      57,    58,    -1,    90,    61,    62,    -1,    -1,    -1,    66,
       3,    -1,    -1,    -1,    -1,    -1,     9,    10,    -1,    12,
      -1,     4,     5,     6,     7,     8,    -1,    -1,    -1,    -1,
      13,    14,    15,    -1,    -1,    92,    93,    30,    -1,    32,
      -1,    34,    35,    36,    37,    38,    29,    -1,    -1,    42,
       3,    -1,    -1,    -1,    -1,    48,     9,    10,    51,    12,
      -1,    -1,    -1,    56,    57,    58,    -1,    -1,    61,    62,
      -1,    -1,    -1,    66,    -1,    -1,    -1,    30,    -1,    32,
      -1,    34,    35,    36,    37,    38,    -1,    -1,    -1,    42,
      -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,    51,    92,
      93,    -1,    -1,    56,    57,    58,    -1,    90,    61,    62,
      -1,    -1,    -1,    66,     1,    -1,    -1,     4,    -1,    -1,
       7,     8,    -1,    -1,    -1,    -1,    13,    14,    15,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,
      93,    -1,    29,    -1,    31,    32,    -1,    -1,     1,    11,
       3,     4,     5,     6,     7,     8,    -1,    -1,    -1,    -1,
      13,    14,    15,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    58,    59,    60,    28,    29,    -1,    31,    32,
      -1,    43,    44,    45,    -1,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    58,    59,    60,    -1,
      -1,    -1,    -1,    90,    91,    58,    -1,    -1,    -1,    -1,
      43,    44,    45,    66,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    60,    -1,    -1,
      -1,    -1,    -1,    -1,    96,    -1,    -1,    90,    43,    44,
      45,    -1,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    -1,    -1,    -1,    -1,
      43,    44,    45,    96,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    60,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    43,    44,
      45,    96,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    -1,    -1,    -1,    -1,
      43,    44,    45,    96,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    60,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    43,    44,
      45,    96,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    -1,    -1,    43,    44,
      45,    94,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    -1,    -1,    11,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    43,    44,    45,    94,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    -1,    -1,    -1,    -1,    -1,    94,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    60,    31,    -1,
      -1,    -1,    89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      43,    44,    45,    -1,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    60,     4,     5,
       6,     7,     8,    -1,    -1,    -1,    -1,    13,    14,    15,
       4,     5,     6,     7,     8,    -1,    -1,    -1,    -1,    13,
      14,    15,    -1,    29,    -1,    31,    -1,    -1,    -1,     4,
       5,     6,     7,     8,    -1,    29,    -1,    31,    13,    14,
      15,     4,     5,     6,     7,     8,    -1,    -1,    -1,    -1,
      13,    14,    15,    -1,    29,    -1,    31,    -1,    -1,    -1,
       4,    -1,    -1,     7,     8,    -1,    29,    -1,    31,    13,
      14,    15,     4,     5,     6,     7,     8,    -1,    -1,    -1,
      -1,    13,    14,    15,    -1,    29,    -1,    31,    -1,     4,
       5,     6,     7,     8,    -1,    -1,    -1,    29,    13,    14,
      15,     4,     5,     6,     7,     8,    -1,    -1,    -1,    -1,
      13,    14,    15,    -1,    29,    -1,    -1,    -1,     4,     5,
       6,     7,     8,    -1,    -1,    -1,    29,    13,    14,    15,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      58,    59,    60,    29,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    58,
      59,    60,    43,    44,    45,    -1,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    58,    59,    60,
      45,    -1,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const unsigned short yystos[] =
{
       0,    98,    99,   100,     0,   101,     1,     4,     5,     6,
       7,     8,    13,    14,    15,    28,    29,    31,    32,    90,
     102,   103,   104,   105,   120,   136,   139,   140,   141,   142,
     143,   144,   145,   146,   147,   148,   149,   150,   151,   152,
     153,   154,   155,   156,   162,   164,   165,   166,   167,   168,
     177,   178,   182,   205,   206,   207,   208,   213,   297,   102,
      90,    91,   177,   177,   177,    66,    66,    66,     3,    58,
      66,   170,   174,   204,     8,   164,   165,   177,   182,     8,
     164,   165,   182,     8,   164,   165,   177,   182,     8,   164,
     165,   182,     8,   166,   167,   177,   182,     8,   166,   167,
     182,     8,   166,   167,   177,   182,     8,   166,   167,   182,
       8,   164,   165,   177,   182,     8,   164,   165,   182,     8,
     164,   165,   177,   182,     8,   164,   165,   182,     8,   166,
     167,   177,   182,     8,   166,   167,   182,     8,   166,   167,
     177,   182,     8,   166,   167,   182,   136,   136,    90,   178,
       3,     4,    95,   112,    95,   112,    95,   112,   102,     3,
       9,    10,    12,    30,    34,    35,    36,    37,    38,    42,
      48,    51,    56,    57,    58,    61,    62,    66,    92,    93,
     113,   114,   116,   117,   118,   119,   121,   122,   128,   252,
     297,    66,   114,   139,   140,   141,   142,   143,   144,   145,
     146,   161,   225,   139,   140,   141,   142,   160,   163,   176,
     177,    90,    94,     1,    28,    66,    67,   110,   171,   233,
       4,    58,    66,   169,   172,   199,   200,   204,   170,   204,
     216,   217,    95,   216,    95,   212,    95,   121,   121,    66,
      66,    66,   112,   121,     1,    95,   114,   225,   121,    89,
      94,    66,   117,    66,   117,    43,    44,    45,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    58,
      59,    60,    61,    62,    65,    66,    67,    68,     1,    91,
     241,   250,   121,     7,     8,   112,   179,   180,   181,   182,
      89,   226,    89,   204,   204,   137,   176,    66,   176,   292,
       6,   160,   163,     1,   130,   131,   132,   133,   240,   259,
     176,   163,   176,    90,    94,     1,   106,   171,    66,   233,
      90,     1,   108,    91,     1,    90,   139,   140,   141,   142,
     143,   144,   145,   146,   159,   160,   218,   297,   209,    91,
     210,     1,   112,   223,   224,   211,   122,   122,   225,    89,
      89,    89,    90,   122,   225,   225,   122,   122,   125,   127,
     124,   123,   122,   122,   122,   122,   122,   122,   122,   122,
     122,   122,   122,   122,   112,   115,   116,   114,   112,    89,
      33,   245,   246,   247,    89,    89,    94,    66,    58,    66,
     227,   229,   230,   231,   232,   233,    89,   174,   204,    10,
     293,   163,     6,    58,    96,   122,    90,   259,   240,   132,
     134,   139,   140,   143,   144,   147,   148,   151,   152,   157,
     158,    43,   200,   200,   137,   130,   176,   292,   130,   176,
     136,   136,    90,   218,   216,   176,   216,    43,    94,   215,
     223,    94,    94,    94,    95,   121,    89,    89,   114,    46,
     122,   122,    89,    96,   112,   296,     1,   135,   234,   235,
     236,   237,   238,   239,   240,   251,   259,   262,   263,   247,
      89,   180,     3,   115,   163,   176,   282,    66,   233,    89,
       1,     3,    11,   157,   158,   284,   287,   288,   290,   294,
     295,   122,   122,    96,    96,   111,    90,   136,    90,   136,
     175,    89,   172,   199,   259,    43,   259,    46,   199,   219,
     221,    46,   204,   220,   222,    91,    91,   122,   224,    91,
     215,   225,   122,   225,   129,    46,   122,    90,    94,   135,
     262,   263,   135,   262,   263,   259,   262,   263,   135,   262,
     263,   240,    91,     3,     4,    22,    23,    24,    25,    26,
      27,    28,    90,    95,   112,   114,   138,   155,   156,   162,
     243,   249,   253,   274,   275,   297,    89,    94,    89,   230,
     231,   229,   283,   176,   282,    89,   136,   291,   291,    89,
      90,    94,    89,    94,    96,    96,     1,   248,   253,   169,
     170,     1,    95,   122,   183,   107,   173,   109,   122,    46,
     176,    94,   122,    46,   176,    94,   176,   176,   176,    91,
      89,    94,    89,     1,    65,    67,    95,   112,   122,   185,
     186,   187,   189,   191,   192,   126,   112,   242,   122,    46,
      90,    90,    90,   114,    58,   112,     8,   276,   259,    90,
     136,   136,    90,    16,    18,    19,    20,    21,   254,   255,
     257,   264,   250,   138,   116,    89,   284,     4,    58,    66,
     201,   202,   203,   204,   228,   229,   230,    58,    66,   204,
     228,   285,    11,   139,   140,   141,   142,   143,   144,   145,
     146,   147,   148,   149,   150,   151,   152,   153,   154,   155,
     156,   289,     3,   253,    90,    90,   184,   248,   183,   248,
     176,   122,   137,   176,   122,   137,   176,   122,   112,   122,
     190,    46,    91,    94,   214,    43,   192,   189,   122,    11,
      46,    90,   114,    90,    66,    46,   169,   193,   199,   170,
     196,   204,   256,   266,   258,   268,    66,    17,     1,   243,
     261,     1,    66,   244,    89,   163,   176,   176,    66,   233,
      66,   233,   176,   177,   163,   176,   176,   176,   136,   136,
     185,   176,   221,   176,   222,    89,    11,    96,   185,   188,
     187,   189,   122,    90,   114,   176,    90,   194,    90,   197,
      66,    66,   261,    66,   114,     1,   265,   260,   262,   263,
     114,   202,   203,   203,   292,   292,   286,   201,   204,   228,
     204,   228,    91,   122,    91,   189,    46,    46,    89,   130,
     130,   114,   114,    18,   114,   138,   273,   277,    89,   261,
     244,   260,    89,    89,   284,   176,   176,   176,    96,    10,
      67,   278,   279,   280,    90,   259,   259,    89,    89,   269,
      90,   272,    90,    66,   112,    46,    89,    94,   195,   198,
     267,   277,   261,   114,    96,   278,    90,   280,   253,   253,
     261,    90,    89,    10,    46,    89,   270,    66,    10,   281,
      90,   277,   114,    89,    94,    89,    89,    90,    10,   271,
     261
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
#line 319 "c-parse.y"
    { if (pedantic)
		    pedwarn ("ISO C forbids an empty source file");
		;}
    break;

  case 4:
#line 330 "c-parse.y"
    {yyval.ttype = NULL_TREE; ;}
    break;

  case 6:
#line 331 "c-parse.y"
    {yyval.ttype = NULL_TREE; ggc_collect(); ;}
    break;

  case 8:
#line 336 "c-parse.y"
    { parsing_iso_function_signature = false; ;}
    break;

  case 11:
#line 343 "c-parse.y"
    { STRIP_NOPS (yyvsp[-2].ttype);
		  if ((TREE_CODE (yyvsp[-2].ttype) == ADDR_EXPR
		       && TREE_CODE (TREE_OPERAND (yyvsp[-2].ttype, 0)) == STRING_CST)
		      || TREE_CODE (yyvsp[-2].ttype) == STRING_CST)
		    assemble_asm (yyvsp[-2].ttype);
		  else
		    error ("argument of `asm' is not a constant string"); ;}
    break;

  case 12:
#line 351 "c-parse.y"
    { RESTORE_EXT_FLAGS (yyvsp[-1].itype); ;}
    break;

  case 13:
#line 356 "c-parse.y"
    { if (pedantic)
		    error ("ISO C forbids data definition with no type or storage class");
		  else
		    warning ("data definition has no type or storage class");

		  POP_DECLSPEC_STACK; ;}
    break;

  case 14:
#line 363 "c-parse.y"
    { POP_DECLSPEC_STACK; ;}
    break;

  case 15:
#line 365 "c-parse.y"
    { POP_DECLSPEC_STACK; ;}
    break;

  case 16:
#line 367 "c-parse.y"
    { shadow_tag (yyvsp[-1].ttype); ;}
    break;

  case 19:
#line 371 "c-parse.y"
    { if (pedantic)
		    pedwarn ("ISO C does not allow extra `;' outside of a function"); ;}
    break;

  case 20:
#line 377 "c-parse.y"
    { if (! start_function (current_declspecs, yyvsp[0].ttype,
					all_prefix_attributes))
		    YYERROR1;
		;}
    break;

  case 21:
#line 382 "c-parse.y"
    { DECL_SOURCE_LOCATION (current_function_decl) = yyvsp[0].location;
		  store_parm_decls (); ;}
    break;

  case 22:
#line 385 "c-parse.y"
    { finish_function ();
		  POP_DECLSPEC_STACK; ;}
    break;

  case 23:
#line 388 "c-parse.y"
    { POP_DECLSPEC_STACK; ;}
    break;

  case 24:
#line 390 "c-parse.y"
    { if (! start_function (current_declspecs, yyvsp[0].ttype,
					all_prefix_attributes))
		    YYERROR1;
		;}
    break;

  case 25:
#line 395 "c-parse.y"
    { DECL_SOURCE_LOCATION (current_function_decl) = yyvsp[0].location;
		  store_parm_decls (); ;}
    break;

  case 26:
#line 398 "c-parse.y"
    { finish_function ();
		  POP_DECLSPEC_STACK; ;}
    break;

  case 27:
#line 401 "c-parse.y"
    { POP_DECLSPEC_STACK; ;}
    break;

  case 28:
#line 403 "c-parse.y"
    { if (! start_function (NULL_TREE, yyvsp[0].ttype,
					all_prefix_attributes))
		    YYERROR1;
		;}
    break;

  case 29:
#line 408 "c-parse.y"
    { DECL_SOURCE_LOCATION (current_function_decl) = yyvsp[0].location;
		  store_parm_decls (); ;}
    break;

  case 30:
#line 411 "c-parse.y"
    { finish_function ();
		  POP_DECLSPEC_STACK; ;}
    break;

  case 31:
#line 414 "c-parse.y"
    { POP_DECLSPEC_STACK; ;}
    break;

  case 34:
#line 423 "c-parse.y"
    { yyval.code = ADDR_EXPR; ;}
    break;

  case 35:
#line 425 "c-parse.y"
    { yyval.code = NEGATE_EXPR; ;}
    break;

  case 36:
#line 427 "c-parse.y"
    { yyval.code = CONVERT_EXPR;
  if (warn_traditional && !in_system_header)
    warning ("traditional C rejects the unary plus operator");
		;}
    break;

  case 37:
#line 432 "c-parse.y"
    { yyval.code = PREINCREMENT_EXPR; ;}
    break;

  case 38:
#line 434 "c-parse.y"
    { yyval.code = PREDECREMENT_EXPR; ;}
    break;

  case 39:
#line 436 "c-parse.y"
    { yyval.code = BIT_NOT_EXPR; ;}
    break;

  case 40:
#line 438 "c-parse.y"
    { yyval.code = TRUTH_NOT_EXPR; ;}
    break;

  case 41:
#line 442 "c-parse.y"
    { yyval.ttype = build_compound_expr (yyvsp[0].ttype); ;}
    break;

  case 42:
#line 447 "c-parse.y"
    { yyval.ttype = NULL_TREE; ;}
    break;

  case 44:
#line 453 "c-parse.y"
    { yyval.ttype = build_tree_list (NULL_TREE, yyvsp[0].ttype); ;}
    break;

  case 45:
#line 455 "c-parse.y"
    { chainon (yyvsp[-2].ttype, build_tree_list (NULL_TREE, yyvsp[0].ttype)); ;}
    break;

  case 47:
#line 461 "c-parse.y"
    { yyval.ttype = build_indirect_ref (yyvsp[0].ttype, "unary *"); ;}
    break;

  case 48:
#line 464 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype;
		  RESTORE_EXT_FLAGS (yyvsp[-1].itype); ;}
    break;

  case 49:
#line 467 "c-parse.y"
    { yyval.ttype = build_unary_op (yyvsp[-1].code, yyvsp[0].ttype, 0);
		  overflow_warning (yyval.ttype); ;}
    break;

  case 50:
#line 471 "c-parse.y"
    { yyval.ttype = finish_label_address_expr (yyvsp[0].ttype); ;}
    break;

  case 51:
#line 473 "c-parse.y"
    { skip_evaluation--;
		  if (TREE_CODE (yyvsp[0].ttype) == COMPONENT_REF
		      && DECL_C_BIT_FIELD (TREE_OPERAND (yyvsp[0].ttype, 1)))
		    error ("`sizeof' applied to a bit-field");
		  yyval.ttype = c_sizeof (TREE_TYPE (yyvsp[0].ttype)); ;}
    break;

  case 52:
#line 479 "c-parse.y"
    { skip_evaluation--;
		  yyval.ttype = c_sizeof (groktypename (yyvsp[-1].ttype)); ;}
    break;

  case 53:
#line 482 "c-parse.y"
    { skip_evaluation--;
		  yyval.ttype = c_alignof_expr (yyvsp[0].ttype); ;}
    break;

  case 54:
#line 485 "c-parse.y"
    { skip_evaluation--;
		  yyval.ttype = c_alignof (groktypename (yyvsp[-1].ttype)); ;}
    break;

  case 55:
#line 488 "c-parse.y"
    { yyval.ttype = build_unary_op (REALPART_EXPR, yyvsp[0].ttype, 0); ;}
    break;

  case 56:
#line 490 "c-parse.y"
    { yyval.ttype = build_unary_op (IMAGPART_EXPR, yyvsp[0].ttype, 0); ;}
    break;

  case 57:
#line 494 "c-parse.y"
    { skip_evaluation++; ;}
    break;

  case 58:
#line 498 "c-parse.y"
    { skip_evaluation++; ;}
    break;

  case 59:
#line 502 "c-parse.y"
    { skip_evaluation++; ;}
    break;

  case 61:
#line 508 "c-parse.y"
    { yyval.ttype = c_cast_expr (yyvsp[-2].ttype, yyvsp[0].ttype); ;}
    break;

  case 63:
#line 514 "c-parse.y"
    { yyval.ttype = parser_build_binary_op (yyvsp[-1].code, yyvsp[-2].ttype, yyvsp[0].ttype); ;}
    break;

  case 64:
#line 516 "c-parse.y"
    { yyval.ttype = parser_build_binary_op (yyvsp[-1].code, yyvsp[-2].ttype, yyvsp[0].ttype); ;}
    break;

  case 65:
#line 518 "c-parse.y"
    { yyval.ttype = parser_build_binary_op (yyvsp[-1].code, yyvsp[-2].ttype, yyvsp[0].ttype); ;}
    break;

  case 66:
#line 520 "c-parse.y"
    { yyval.ttype = parser_build_binary_op (yyvsp[-1].code, yyvsp[-2].ttype, yyvsp[0].ttype); ;}
    break;

  case 67:
#line 522 "c-parse.y"
    { yyval.ttype = parser_build_binary_op (yyvsp[-1].code, yyvsp[-2].ttype, yyvsp[0].ttype); ;}
    break;

  case 68:
#line 524 "c-parse.y"
    { yyval.ttype = parser_build_binary_op (yyvsp[-1].code, yyvsp[-2].ttype, yyvsp[0].ttype); ;}
    break;

  case 69:
#line 526 "c-parse.y"
    { yyval.ttype = parser_build_binary_op (yyvsp[-1].code, yyvsp[-2].ttype, yyvsp[0].ttype); ;}
    break;

  case 70:
#line 528 "c-parse.y"
    { yyval.ttype = parser_build_binary_op (yyvsp[-1].code, yyvsp[-2].ttype, yyvsp[0].ttype); ;}
    break;

  case 71:
#line 530 "c-parse.y"
    { yyval.ttype = parser_build_binary_op (yyvsp[-1].code, yyvsp[-2].ttype, yyvsp[0].ttype); ;}
    break;

  case 72:
#line 532 "c-parse.y"
    { yyval.ttype = parser_build_binary_op (yyvsp[-1].code, yyvsp[-2].ttype, yyvsp[0].ttype); ;}
    break;

  case 73:
#line 534 "c-parse.y"
    { yyval.ttype = parser_build_binary_op (yyvsp[-1].code, yyvsp[-2].ttype, yyvsp[0].ttype); ;}
    break;

  case 74:
#line 536 "c-parse.y"
    { yyval.ttype = parser_build_binary_op (yyvsp[-1].code, yyvsp[-2].ttype, yyvsp[0].ttype); ;}
    break;

  case 75:
#line 538 "c-parse.y"
    { yyvsp[-1].ttype = c_common_truthvalue_conversion
		    (default_conversion (yyvsp[-1].ttype));
		  skip_evaluation += yyvsp[-1].ttype == truthvalue_false_node; ;}
    break;

  case 76:
#line 542 "c-parse.y"
    { skip_evaluation -= yyvsp[-3].ttype == truthvalue_false_node;
		  yyval.ttype = parser_build_binary_op (TRUTH_ANDIF_EXPR, yyvsp[-3].ttype, yyvsp[0].ttype); ;}
    break;

  case 77:
#line 545 "c-parse.y"
    { yyvsp[-1].ttype = c_common_truthvalue_conversion
		    (default_conversion (yyvsp[-1].ttype));
		  skip_evaluation += yyvsp[-1].ttype == truthvalue_true_node; ;}
    break;

  case 78:
#line 549 "c-parse.y"
    { skip_evaluation -= yyvsp[-3].ttype == truthvalue_true_node;
		  yyval.ttype = parser_build_binary_op (TRUTH_ORIF_EXPR, yyvsp[-3].ttype, yyvsp[0].ttype); ;}
    break;

  case 79:
#line 552 "c-parse.y"
    { yyvsp[-1].ttype = c_common_truthvalue_conversion
		    (default_conversion (yyvsp[-1].ttype));
		  skip_evaluation += yyvsp[-1].ttype == truthvalue_false_node; ;}
    break;

  case 80:
#line 556 "c-parse.y"
    { skip_evaluation += ((yyvsp[-4].ttype == truthvalue_true_node)
				      - (yyvsp[-4].ttype == truthvalue_false_node)); ;}
    break;

  case 81:
#line 559 "c-parse.y"
    { skip_evaluation -= yyvsp[-6].ttype == truthvalue_true_node;
		  yyval.ttype = build_conditional_expr (yyvsp[-6].ttype, yyvsp[-3].ttype, yyvsp[0].ttype); ;}
    break;

  case 82:
#line 562 "c-parse.y"
    { if (pedantic)
		    pedwarn ("ISO C forbids omitting the middle term of a ?: expression");
		  /* Make sure first operand is calculated only once.  */
		  yyvsp[0].ttype = save_expr (yyvsp[-1].ttype);
		  yyvsp[-1].ttype = c_common_truthvalue_conversion
		    (default_conversion (yyvsp[0].ttype));
		  skip_evaluation += yyvsp[-1].ttype == truthvalue_true_node; ;}
    break;

  case 83:
#line 570 "c-parse.y"
    { skip_evaluation -= yyvsp[-4].ttype == truthvalue_true_node;
		  yyval.ttype = build_conditional_expr (yyvsp[-4].ttype, yyvsp[-3].ttype, yyvsp[0].ttype); ;}
    break;

  case 84:
#line 573 "c-parse.y"
    { char class;
		  yyval.ttype = build_modify_expr (yyvsp[-2].ttype, NOP_EXPR, yyvsp[0].ttype);
		  class = TREE_CODE_CLASS (TREE_CODE (yyval.ttype));
		  if (IS_EXPR_CODE_CLASS (class))
		    C_SET_EXP_ORIGINAL_CODE (yyval.ttype, MODIFY_EXPR);
		;}
    break;

  case 85:
#line 580 "c-parse.y"
    { char class;
		  yyval.ttype = build_modify_expr (yyvsp[-2].ttype, yyvsp[-1].code, yyvsp[0].ttype);
		  /* This inhibits warnings in
		     c_common_truthvalue_conversion.  */
		  class = TREE_CODE_CLASS (TREE_CODE (yyval.ttype));
		  if (IS_EXPR_CODE_CLASS (class))
		    C_SET_EXP_ORIGINAL_CODE (yyval.ttype, ERROR_MARK);
		;}
    break;

  case 86:
#line 592 "c-parse.y"
    {
		  if (yychar == YYEMPTY)
		    yychar = YYLEX;
		  yyval.ttype = build_external_ref (yyvsp[0].ttype, yychar == '(');
		;}
    break;

  case 89:
#line 600 "c-parse.y"
    { yyval.ttype = fname_decl (C_RID_CODE (yyval.ttype), yyval.ttype); ;}
    break;

  case 90:
#line 602 "c-parse.y"
    { start_init (NULL_TREE, NULL, 0);
		  yyvsp[-2].ttype = groktypename (yyvsp[-2].ttype);
		  really_start_incremental_init (yyvsp[-2].ttype); ;}
    break;

  case 91:
#line 606 "c-parse.y"
    { tree constructor = pop_init_level (0);
		  tree type = yyvsp[-5].ttype;
		  finish_init ();

		  if (pedantic && ! flag_isoc99)
		    pedwarn ("ISO C90 forbids compound literals");
		  yyval.ttype = build_compound_literal (type, constructor);
		;}
    break;

  case 92:
#line 615 "c-parse.y"
    { char class = TREE_CODE_CLASS (TREE_CODE (yyvsp[-1].ttype));
		  if (IS_EXPR_CODE_CLASS (class))
		    C_SET_EXP_ORIGINAL_CODE (yyvsp[-1].ttype, ERROR_MARK);
		  yyval.ttype = yyvsp[-1].ttype; ;}
    break;

  case 93:
#line 620 "c-parse.y"
    { yyval.ttype = error_mark_node; ;}
    break;

  case 94:
#line 622 "c-parse.y"
    { tree saved_last_tree;

		   if (pedantic)
		     pedwarn ("ISO C forbids braced-groups within expressions");
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

  case 95:
#line 636 "c-parse.y"
    {
		  last_tree = COMPOUND_BODY (yyvsp[-2].ttype);
		  TREE_CHAIN (last_tree) = NULL_TREE;
		  yyval.ttype = error_mark_node;
		;}
    break;

  case 96:
#line 642 "c-parse.y"
    { yyval.ttype = build_function_call (yyvsp[-3].ttype, yyvsp[-1].ttype); ;}
    break;

  case 97:
#line 644 "c-parse.y"
    { yyval.ttype = build_va_arg (yyvsp[-3].ttype, groktypename (yyvsp[-1].ttype)); ;}
    break;

  case 98:
#line 647 "c-parse.y"
    {
                  tree c;

                  c = fold (yyvsp[-5].ttype);
                  STRIP_NOPS (c);
                  if (TREE_CODE (c) != INTEGER_CST)
                    error ("first argument to __builtin_choose_expr not a constant");
                  yyval.ttype = integer_zerop (c) ? yyvsp[-1].ttype : yyvsp[-3].ttype;
		;}
    break;

  case 99:
#line 657 "c-parse.y"
    {
		  tree e1, e2;

		  e1 = TYPE_MAIN_VARIANT (groktypename (yyvsp[-3].ttype));
		  e2 = TYPE_MAIN_VARIANT (groktypename (yyvsp[-1].ttype));

		  yyval.ttype = comptypes (e1, e2, COMPARE_STRICT)
		    ? build_int_2 (1, 0) : build_int_2 (0, 0);
		;}
    break;

  case 100:
#line 667 "c-parse.y"
    { yyval.ttype = build_array_ref (yyvsp[-3].ttype, yyvsp[-1].ttype); ;}
    break;

  case 101:
#line 669 "c-parse.y"
    {
		      yyval.ttype = build_component_ref (yyvsp[-2].ttype, yyvsp[0].ttype);
		;}
    break;

  case 102:
#line 673 "c-parse.y"
    {
                  tree expr = build_indirect_ref (yyvsp[-2].ttype, "->");

			yyval.ttype = build_component_ref (expr, yyvsp[0].ttype);
		;}
    break;

  case 103:
#line 679 "c-parse.y"
    { yyval.ttype = build_unary_op (POSTINCREMENT_EXPR, yyvsp[-1].ttype, 0); ;}
    break;

  case 104:
#line 681 "c-parse.y"
    { yyval.ttype = build_unary_op (POSTDECREMENT_EXPR, yyvsp[-1].ttype, 0); ;}
    break;

  case 105:
#line 686 "c-parse.y"
    {
	  parsing_iso_function_signature = false; /* Reset after decls.  */
	;}
    break;

  case 106:
#line 693 "c-parse.y"
    {
	  if (warn_traditional && !in_system_header
	      && parsing_iso_function_signature)
	    warning ("traditional C rejects ISO C style function definitions");
	  if (warn_old_style_definition && !in_system_header
	      && !parsing_iso_function_signature)
	    warning ("old-style parameter declaration");
	  parsing_iso_function_signature = false; /* Reset after warning.  */
	;}
    break;

  case 107:
#line 703 "c-parse.y"
    {
	  if (warn_old_style_definition && !in_system_header)
	    warning ("old-style parameter declaration");
	;}
    break;

  case 108:
#line 714 "c-parse.y"
    { ;}
    break;

  case 113:
#line 730 "c-parse.y"
    { POP_DECLSPEC_STACK; ;}
    break;

  case 114:
#line 732 "c-parse.y"
    { POP_DECLSPEC_STACK; ;}
    break;

  case 115:
#line 734 "c-parse.y"
    { shadow_tag_warned (yyvsp[-1].ttype, 1);
		  pedwarn ("empty declaration"); ;}
    break;

  case 116:
#line 737 "c-parse.y"
    { pedwarn ("empty declaration"); ;}
    break;

  case 117:
#line 746 "c-parse.y"
    { ;}
    break;

  case 118:
#line 754 "c-parse.y"
    { pending_xref_error ();
		  PUSH_DECLSPEC_STACK;
		  split_specs_attrs (yyvsp[0].ttype,
				     &current_declspecs, &prefix_attributes);
		  all_prefix_attributes = prefix_attributes; ;}
    break;

  case 119:
#line 765 "c-parse.y"
    { all_prefix_attributes = chainon (yyvsp[0].ttype, prefix_attributes); ;}
    break;

  case 120:
#line 770 "c-parse.y"
    { POP_DECLSPEC_STACK; ;}
    break;

  case 121:
#line 772 "c-parse.y"
    { POP_DECLSPEC_STACK; ;}
    break;

  case 122:
#line 774 "c-parse.y"
    { POP_DECLSPEC_STACK; ;}
    break;

  case 123:
#line 776 "c-parse.y"
    { POP_DECLSPEC_STACK; ;}
    break;

  case 124:
#line 778 "c-parse.y"
    { shadow_tag (yyvsp[-1].ttype); ;}
    break;

  case 125:
#line 780 "c-parse.y"
    { RESTORE_EXT_FLAGS (yyvsp[-1].itype); ;}
    break;

  case 126:
#line 837 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, NULL_TREE);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 127:
#line 840 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 128:
#line 843 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 129:
#line 849 "c-parse.y"
    { yyval.ttype = tree_cons (yyvsp[0].ttype, NULL_TREE, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 130:
#line 855 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 131:
#line 858 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 132:
#line 864 "c-parse.y"
    { yyval.ttype = tree_cons (yyvsp[0].ttype, NULL_TREE, NULL_TREE);
		  TREE_STATIC (yyval.ttype) = 0; ;}
    break;

  case 133:
#line 867 "c-parse.y"
    { yyval.ttype = tree_cons (yyvsp[0].ttype, NULL_TREE, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 134:
#line 873 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, NULL_TREE);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 135:
#line 876 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 136:
#line 879 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 137:
#line 882 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 138:
#line 885 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 139:
#line 888 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 140:
#line 891 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 141:
#line 897 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, NULL_TREE);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 142:
#line 900 "c-parse.y"
    { yyval.ttype = tree_cons (yyvsp[0].ttype, NULL_TREE, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 143:
#line 903 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 144:
#line 906 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 145:
#line 909 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 146:
#line 912 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 147:
#line 918 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 148:
#line 921 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 149:
#line 924 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 150:
#line 927 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 151:
#line 930 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 152:
#line 933 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 153:
#line 939 "c-parse.y"
    { yyval.ttype = tree_cons (yyvsp[0].ttype, NULL_TREE, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 154:
#line 942 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 155:
#line 945 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 156:
#line 948 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 157:
#line 951 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 158:
#line 957 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, NULL_TREE);
		  TREE_STATIC (yyval.ttype) = 0; ;}
    break;

  case 159:
#line 960 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 160:
#line 963 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 161:
#line 966 "c-parse.y"
    { if (extra_warnings && TREE_STATIC (yyvsp[-1].ttype))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER (yyvsp[0].ttype));
		  yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 162:
#line 972 "c-parse.y"
    { if (extra_warnings && TREE_STATIC (yyvsp[-1].ttype))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER (yyvsp[0].ttype));
		  yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 163:
#line 978 "c-parse.y"
    { if (extra_warnings && TREE_STATIC (yyvsp[-1].ttype))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER (yyvsp[0].ttype));
		  yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 164:
#line 984 "c-parse.y"
    { if (extra_warnings && TREE_STATIC (yyvsp[-1].ttype))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER (yyvsp[0].ttype));
		  yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 165:
#line 993 "c-parse.y"
    { yyval.ttype = tree_cons (yyvsp[0].ttype, NULL_TREE, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 166:
#line 999 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 167:
#line 1002 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 168:
#line 1005 "c-parse.y"
    { if (extra_warnings && TREE_STATIC (yyvsp[-1].ttype))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER (yyvsp[0].ttype));
		  yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 169:
#line 1011 "c-parse.y"
    { if (extra_warnings && TREE_STATIC (yyvsp[-1].ttype))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER (yyvsp[0].ttype));
		  yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 170:
#line 1017 "c-parse.y"
    { if (extra_warnings && TREE_STATIC (yyvsp[-1].ttype))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER (yyvsp[0].ttype));
		  yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 171:
#line 1023 "c-parse.y"
    { if (extra_warnings && TREE_STATIC (yyvsp[-1].ttype))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER (yyvsp[0].ttype));
		  yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 172:
#line 1032 "c-parse.y"
    { yyval.ttype = tree_cons (yyvsp[0].ttype, NULL_TREE, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 173:
#line 1038 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 174:
#line 1041 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 175:
#line 1044 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 176:
#line 1047 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 177:
#line 1050 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 178:
#line 1053 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 179:
#line 1056 "c-parse.y"
    { if (extra_warnings && TREE_STATIC (yyvsp[-1].ttype))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER (yyvsp[0].ttype));
		  yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 180:
#line 1062 "c-parse.y"
    { if (extra_warnings && TREE_STATIC (yyvsp[-1].ttype))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER (yyvsp[0].ttype));
		  yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 181:
#line 1068 "c-parse.y"
    { if (extra_warnings && TREE_STATIC (yyvsp[-1].ttype))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER (yyvsp[0].ttype));
		  yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 182:
#line 1074 "c-parse.y"
    { if (extra_warnings && TREE_STATIC (yyvsp[-1].ttype))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER (yyvsp[0].ttype));
		  yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 183:
#line 1083 "c-parse.y"
    { yyval.ttype = tree_cons (yyvsp[0].ttype, NULL_TREE, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 184:
#line 1086 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 185:
#line 1089 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 186:
#line 1092 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 187:
#line 1095 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 188:
#line 1101 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 189:
#line 1104 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 190:
#line 1107 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 191:
#line 1110 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 192:
#line 1113 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 193:
#line 1116 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 194:
#line 1119 "c-parse.y"
    { if (extra_warnings && TREE_STATIC (yyvsp[-1].ttype))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER (yyvsp[0].ttype));
		  yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 195:
#line 1125 "c-parse.y"
    { if (extra_warnings && TREE_STATIC (yyvsp[-1].ttype))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER (yyvsp[0].ttype));
		  yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 196:
#line 1131 "c-parse.y"
    { if (extra_warnings && TREE_STATIC (yyvsp[-1].ttype))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER (yyvsp[0].ttype));
		  yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 197:
#line 1137 "c-parse.y"
    { if (extra_warnings && TREE_STATIC (yyvsp[-1].ttype))
		    warning ("`%s' is not at beginning of declaration",
			     IDENTIFIER_POINTER (yyvsp[0].ttype));
		  yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 198:
#line 1146 "c-parse.y"
    { yyval.ttype = tree_cons (yyvsp[0].ttype, NULL_TREE, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = TREE_STATIC (yyvsp[-1].ttype); ;}
    break;

  case 199:
#line 1149 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 200:
#line 1152 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 201:
#line 1155 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 202:
#line 1158 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-1].ttype);
		  TREE_STATIC (yyval.ttype) = 1; ;}
    break;

  case 259:
#line 1246 "c-parse.y"
    { yyval.ttype = NULL_TREE; ;}
    break;

  case 260:
#line 1248 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype; ;}
    break;

  case 264:
#line 1283 "c-parse.y"
    { OBJC_NEED_RAW_IDENTIFIER (1);	;}
    break;

  case 267:
#line 1293 "c-parse.y"
    { /* For a typedef name, record the meaning, not the name.
		     In case of `foo foo, bar;'.  */
		  yyval.ttype = lookup_name (yyvsp[0].ttype); ;}
    break;

  case 268:
#line 1297 "c-parse.y"
    { skip_evaluation--;
		  if (TREE_CODE (yyvsp[-1].ttype) == COMPONENT_REF
		      && DECL_C_BIT_FIELD (TREE_OPERAND (yyvsp[-1].ttype, 1)))
		    error ("`typeof' applied to a bit-field");
		  yyval.ttype = TREE_TYPE (yyvsp[-1].ttype); ;}
    break;

  case 269:
#line 1303 "c-parse.y"
    { skip_evaluation--; yyval.ttype = groktypename (yyvsp[-1].ttype); ;}
    break;

  case 274:
#line 1320 "c-parse.y"
    { yyval.ttype = NULL_TREE; ;}
    break;

  case 275:
#line 1322 "c-parse.y"
    { yyval.ttype = yyvsp[-1].ttype; ;}
    break;

  case 276:
#line 1327 "c-parse.y"
    { yyval.ttype = start_decl (yyvsp[-3].ttype, current_declspecs, 1,
					  chainon (yyvsp[-1].ttype, all_prefix_attributes));
		  start_init (yyval.ttype, yyvsp[-2].ttype, global_bindings_p ()); ;}
    break;

  case 277:
#line 1332 "c-parse.y"
    { finish_init ();
		  finish_decl (yyvsp[-1].ttype, yyvsp[0].ttype, yyvsp[-4].ttype); ;}
    break;

  case 278:
#line 1335 "c-parse.y"
    { tree d = start_decl (yyvsp[-2].ttype, current_declspecs, 0,
				       chainon (yyvsp[0].ttype, all_prefix_attributes));
		  finish_decl (d, NULL_TREE, yyvsp[-1].ttype);
                ;}
    break;

  case 279:
#line 1343 "c-parse.y"
    { yyval.ttype = start_decl (yyvsp[-3].ttype, current_declspecs, 1,
					  chainon (yyvsp[-1].ttype, all_prefix_attributes));
		  start_init (yyval.ttype, yyvsp[-2].ttype, global_bindings_p ()); ;}
    break;

  case 280:
#line 1348 "c-parse.y"
    { finish_init ();
		  finish_decl (yyvsp[-1].ttype, yyvsp[0].ttype, yyvsp[-4].ttype); ;}
    break;

  case 281:
#line 1351 "c-parse.y"
    { tree d = start_decl (yyvsp[-2].ttype, current_declspecs, 0,
				       chainon (yyvsp[0].ttype, all_prefix_attributes));
		  finish_decl (d, NULL_TREE, yyvsp[-1].ttype); ;}
    break;

  case 282:
#line 1359 "c-parse.y"
    { yyval.ttype = NULL_TREE; ;}
    break;

  case 283:
#line 1361 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype; ;}
    break;

  case 284:
#line 1366 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype; ;}
    break;

  case 285:
#line 1368 "c-parse.y"
    { yyval.ttype = chainon (yyvsp[-1].ttype, yyvsp[0].ttype); ;}
    break;

  case 286:
#line 1373 "c-parse.y"
    { yyval.ttype = yyvsp[-2].ttype; ;}
    break;

  case 287:
#line 1378 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype; ;}
    break;

  case 288:
#line 1380 "c-parse.y"
    { yyval.ttype = chainon (yyvsp[-2].ttype, yyvsp[0].ttype); ;}
    break;

  case 289:
#line 1385 "c-parse.y"
    { yyval.ttype = NULL_TREE; ;}
    break;

  case 290:
#line 1387 "c-parse.y"
    { yyval.ttype = build_tree_list (yyvsp[0].ttype, NULL_TREE); ;}
    break;

  case 291:
#line 1389 "c-parse.y"
    { yyval.ttype = build_tree_list (yyvsp[-3].ttype, build_tree_list (NULL_TREE, yyvsp[-1].ttype)); ;}
    break;

  case 292:
#line 1391 "c-parse.y"
    { yyval.ttype = build_tree_list (yyvsp[-5].ttype, tree_cons (NULL_TREE, yyvsp[-3].ttype, yyvsp[-1].ttype)); ;}
    break;

  case 293:
#line 1393 "c-parse.y"
    { yyval.ttype = build_tree_list (yyvsp[-3].ttype, yyvsp[-1].ttype); ;}
    break;

  case 301:
#line 1416 "c-parse.y"
    { really_start_incremental_init (NULL_TREE); ;}
    break;

  case 302:
#line 1418 "c-parse.y"
    { yyval.ttype = pop_init_level (0); ;}
    break;

  case 303:
#line 1420 "c-parse.y"
    { yyval.ttype = error_mark_node; ;}
    break;

  case 304:
#line 1426 "c-parse.y"
    { if (pedantic)
		    pedwarn ("ISO C forbids empty initializer braces"); ;}
    break;

  case 308:
#line 1440 "c-parse.y"
    { if (pedantic && ! flag_isoc99)
		    pedwarn ("ISO C90 forbids specifying subobject to initialize"); ;}
    break;

  case 309:
#line 1443 "c-parse.y"
    { if (pedantic)
		    pedwarn ("obsolete use of designated initializer without `='"); ;}
    break;

  case 310:
#line 1446 "c-parse.y"
    { set_init_label (yyvsp[-1].ttype);
		  if (pedantic)
		    pedwarn ("obsolete use of designated initializer with `:'"); ;}
    break;

  case 311:
#line 1450 "c-parse.y"
    {;}
    break;

  case 313:
#line 1456 "c-parse.y"
    { push_init_level (0); ;}
    break;

  case 314:
#line 1458 "c-parse.y"
    { process_init_element (pop_init_level (0)); ;}
    break;

  case 315:
#line 1460 "c-parse.y"
    { process_init_element (yyvsp[0].ttype); ;}
    break;

  case 319:
#line 1471 "c-parse.y"
    { set_init_label (yyvsp[0].ttype); ;}
    break;

  case 320:
#line 1473 "c-parse.y"
    { set_init_index (yyvsp[-3].ttype, yyvsp[-1].ttype);
		  if (pedantic)
		    pedwarn ("ISO C forbids specifying range of elements to initialize"); ;}
    break;

  case 321:
#line 1477 "c-parse.y"
    { set_init_index (yyvsp[-1].ttype, NULL_TREE); ;}
    break;

  case 322:
#line 1482 "c-parse.y"
    { if (pedantic)
		    pedwarn ("ISO C forbids nested functions");

		  push_function_context ();
		  if (! start_function (current_declspecs, yyvsp[0].ttype,
					all_prefix_attributes))
		    {
		      pop_function_context ();
		      YYERROR1;
		    }
		  parsing_iso_function_signature = false; /* Don't warn about nested functions.  */
		;}
    break;

  case 323:
#line 1495 "c-parse.y"
    { tree decl = current_function_decl;
		  DECL_SOURCE_LOCATION (decl) = yyvsp[0].location;
		  store_parm_decls (); ;}
    break;

  case 324:
#line 1505 "c-parse.y"
    { tree decl = current_function_decl;
		  finish_function ();
		  pop_function_context ();
		  add_decl_stmt (decl); ;}
    break;

  case 325:
#line 1513 "c-parse.y"
    { if (pedantic)
		    pedwarn ("ISO C forbids nested functions");

		  push_function_context ();
		  if (! start_function (current_declspecs, yyvsp[0].ttype,
					all_prefix_attributes))
		    {
		      pop_function_context ();
		      YYERROR1;
		    }
		  parsing_iso_function_signature = false; /* Don't warn about nested functions.  */
		;}
    break;

  case 326:
#line 1526 "c-parse.y"
    { tree decl = current_function_decl;
		  DECL_SOURCE_LOCATION (decl) = yyvsp[0].location;
		  store_parm_decls (); ;}
    break;

  case 327:
#line 1536 "c-parse.y"
    { tree decl = current_function_decl;
		  finish_function ();
		  pop_function_context ();
		  add_decl_stmt (decl); ;}
    break;

  case 330:
#line 1554 "c-parse.y"
    { yyval.ttype = yyvsp[-2].ttype ? tree_cons (yyvsp[-2].ttype, yyvsp[-1].ttype, NULL_TREE) : yyvsp[-1].ttype; ;}
    break;

  case 331:
#line 1556 "c-parse.y"
    { yyval.ttype = build_nt (CALL_EXPR, yyvsp[-2].ttype, yyvsp[0].ttype, NULL_TREE); ;}
    break;

  case 332:
#line 1561 "c-parse.y"
    { yyval.ttype = set_array_declarator_type (yyvsp[0].ttype, yyvsp[-1].ttype, 0); ;}
    break;

  case 333:
#line 1563 "c-parse.y"
    { yyval.ttype = make_pointer_declarator (yyvsp[-1].ttype, yyvsp[0].ttype); ;}
    break;

  case 337:
#line 1578 "c-parse.y"
    { yyval.ttype = build_nt (CALL_EXPR, yyvsp[-2].ttype, yyvsp[0].ttype, NULL_TREE); ;}
    break;

  case 338:
#line 1583 "c-parse.y"
    { yyval.ttype = set_array_declarator_type (yyvsp[0].ttype, yyvsp[-1].ttype, 0); ;}
    break;

  case 340:
#line 1589 "c-parse.y"
    { yyval.ttype = build_nt (CALL_EXPR, yyvsp[-2].ttype, yyvsp[0].ttype, NULL_TREE); ;}
    break;

  case 341:
#line 1594 "c-parse.y"
    { yyval.ttype = set_array_declarator_type (yyvsp[0].ttype, yyvsp[-1].ttype, 0); ;}
    break;

  case 342:
#line 1596 "c-parse.y"
    { yyval.ttype = make_pointer_declarator (yyvsp[-1].ttype, yyvsp[0].ttype); ;}
    break;

  case 343:
#line 1598 "c-parse.y"
    { yyval.ttype = make_pointer_declarator (yyvsp[-1].ttype, yyvsp[0].ttype); ;}
    break;

  case 344:
#line 1600 "c-parse.y"
    { yyval.ttype = yyvsp[-2].ttype ? tree_cons (yyvsp[-2].ttype, yyvsp[-1].ttype, NULL_TREE) : yyvsp[-1].ttype; ;}
    break;

  case 345:
#line 1608 "c-parse.y"
    { yyval.ttype = build_nt (CALL_EXPR, yyvsp[-2].ttype, yyvsp[0].ttype, NULL_TREE); ;}
    break;

  case 346:
#line 1613 "c-parse.y"
    { yyval.ttype = yyvsp[-2].ttype ? tree_cons (yyvsp[-2].ttype, yyvsp[-1].ttype, NULL_TREE) : yyvsp[-1].ttype; ;}
    break;

  case 347:
#line 1615 "c-parse.y"
    { yyval.ttype = make_pointer_declarator (yyvsp[-1].ttype, yyvsp[0].ttype); ;}
    break;

  case 348:
#line 1617 "c-parse.y"
    { yyval.ttype = set_array_declarator_type (yyvsp[0].ttype, yyvsp[-1].ttype, 0); ;}
    break;

  case 350:
#line 1623 "c-parse.y"
    { yyval.ttype = NULL_TREE; ;}
    break;

  case 351:
#line 1625 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype; ;}
    break;

  case 352:
#line 1630 "c-parse.y"
    { yyval.ttype = NULL_TREE; ;}
    break;

  case 353:
#line 1632 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype; ;}
    break;

  case 354:
#line 1637 "c-parse.y"
    { yyval.ttype = NULL_TREE; ;}
    break;

  case 355:
#line 1639 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype; ;}
    break;

  case 356:
#line 1650 "c-parse.y"
    { yyval.ttype = start_struct (RECORD_TYPE, yyvsp[-1].ttype);
		  /* Start scope of tag before parsing components.  */
		;}
    break;

  case 357:
#line 1654 "c-parse.y"
    { yyval.ttype = finish_struct (yyvsp[-3].ttype, nreverse (yyvsp[-2].ttype),
				      chainon (yyvsp[-6].ttype, yyvsp[0].ttype)); ;}
    break;

  case 358:
#line 1657 "c-parse.y"
    { yyval.ttype = finish_struct (start_struct (RECORD_TYPE, NULL_TREE),
				      nreverse (yyvsp[-2].ttype), chainon (yyvsp[-4].ttype, yyvsp[0].ttype));
		;}
    break;

  case 359:
#line 1661 "c-parse.y"
    { yyval.ttype = start_struct (UNION_TYPE, yyvsp[-1].ttype); ;}
    break;

  case 360:
#line 1663 "c-parse.y"
    { yyval.ttype = finish_struct (yyvsp[-3].ttype, nreverse (yyvsp[-2].ttype),
				      chainon (yyvsp[-6].ttype, yyvsp[0].ttype)); ;}
    break;

  case 361:
#line 1666 "c-parse.y"
    { yyval.ttype = finish_struct (start_struct (UNION_TYPE, NULL_TREE),
				      nreverse (yyvsp[-2].ttype), chainon (yyvsp[-4].ttype, yyvsp[0].ttype));
		;}
    break;

  case 362:
#line 1670 "c-parse.y"
    { yyval.ttype = start_enum (yyvsp[-1].ttype); ;}
    break;

  case 363:
#line 1672 "c-parse.y"
    { yyval.ttype = finish_enum (yyvsp[-4].ttype, nreverse (yyvsp[-3].ttype),
				    chainon (yyvsp[-7].ttype, yyvsp[0].ttype)); ;}
    break;

  case 364:
#line 1675 "c-parse.y"
    { yyval.ttype = start_enum (NULL_TREE); ;}
    break;

  case 365:
#line 1677 "c-parse.y"
    { yyval.ttype = finish_enum (yyvsp[-4].ttype, nreverse (yyvsp[-3].ttype),
				    chainon (yyvsp[-6].ttype, yyvsp[0].ttype)); ;}
    break;

  case 366:
#line 1683 "c-parse.y"
    { yyval.ttype = xref_tag (RECORD_TYPE, yyvsp[0].ttype); ;}
    break;

  case 367:
#line 1685 "c-parse.y"
    { yyval.ttype = xref_tag (UNION_TYPE, yyvsp[0].ttype); ;}
    break;

  case 368:
#line 1687 "c-parse.y"
    { yyval.ttype = xref_tag (ENUMERAL_TYPE, yyvsp[0].ttype);
		  /* In ISO C, enumerated types can be referred to
		     only if already defined.  */
		  if (pedantic && !COMPLETE_TYPE_P (yyval.ttype))
		    pedwarn ("ISO C forbids forward references to `enum' types"); ;}
    break;

  case 372:
#line 1702 "c-parse.y"
    { if (pedantic && ! flag_isoc99)
		    pedwarn ("comma at end of enumerator list"); ;}
    break;

  case 373:
#line 1720 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype; ;}
    break;

  case 374:
#line 1722 "c-parse.y"
    { yyval.ttype = chainon (yyvsp[0].ttype, yyvsp[-1].ttype);
		  pedwarn ("no semicolon at end of struct or union"); ;}
    break;

  case 375:
#line 1727 "c-parse.y"
    { yyval.ttype = NULL_TREE; ;}
    break;

  case 376:
#line 1729 "c-parse.y"
    { yyval.ttype = chainon (yyvsp[-1].ttype, yyvsp[-2].ttype); ;}
    break;

  case 377:
#line 1731 "c-parse.y"
    { if (pedantic)
		    pedwarn ("extra semicolon in struct or union specified"); ;}
    break;

  case 378:
#line 1737 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype;
		  POP_DECLSPEC_STACK; ;}
    break;

  case 379:
#line 1740 "c-parse.y"
    {
		  /* Support for unnamed structs or unions as members of
		     structs or unions (which is [a] useful and [b] supports
		     MS P-SDK).  */
		  if (pedantic)
		    pedwarn ("ISO C doesn't support unnamed structs/unions");

		  yyval.ttype = grokfield(NULL, current_declspecs, NULL_TREE);
		  POP_DECLSPEC_STACK; ;}
    break;

  case 380:
#line 1750 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype;
		  POP_DECLSPEC_STACK; ;}
    break;

  case 381:
#line 1753 "c-parse.y"
    { if (pedantic)
		    pedwarn ("ISO C forbids member declarations with no members");
		  shadow_tag_warned (yyvsp[0].ttype, pedantic);
		  yyval.ttype = NULL_TREE; ;}
    break;

  case 382:
#line 1758 "c-parse.y"
    { yyval.ttype = NULL_TREE; ;}
    break;

  case 383:
#line 1760 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype;
		  RESTORE_EXT_FLAGS (yyvsp[-1].itype); ;}
    break;

  case 385:
#line 1767 "c-parse.y"
    { TREE_CHAIN (yyvsp[0].ttype) = yyvsp[-3].ttype; yyval.ttype = yyvsp[0].ttype; ;}
    break;

  case 387:
#line 1773 "c-parse.y"
    { TREE_CHAIN (yyvsp[0].ttype) = yyvsp[-3].ttype; yyval.ttype = yyvsp[0].ttype; ;}
    break;

  case 388:
#line 1778 "c-parse.y"
    { yyval.ttype = grokfield (yyvsp[-1].ttype, current_declspecs, NULL_TREE);
		  decl_attributes (&yyval.ttype,
				   chainon (yyvsp[0].ttype, all_prefix_attributes), 0); ;}
    break;

  case 389:
#line 1782 "c-parse.y"
    { yyval.ttype = grokfield (yyvsp[-3].ttype, current_declspecs, yyvsp[-1].ttype);
		  decl_attributes (&yyval.ttype,
				   chainon (yyvsp[0].ttype, all_prefix_attributes), 0); ;}
    break;

  case 390:
#line 1786 "c-parse.y"
    { yyval.ttype = grokfield (NULL_TREE, current_declspecs, yyvsp[-1].ttype);
		  decl_attributes (&yyval.ttype,
				   chainon (yyvsp[0].ttype, all_prefix_attributes), 0); ;}
    break;

  case 391:
#line 1793 "c-parse.y"
    { yyval.ttype = grokfield (yyvsp[-1].ttype, current_declspecs, NULL_TREE);
		  decl_attributes (&yyval.ttype,
				   chainon (yyvsp[0].ttype, all_prefix_attributes), 0); ;}
    break;

  case 392:
#line 1797 "c-parse.y"
    { yyval.ttype = grokfield (yyvsp[-3].ttype, current_declspecs, yyvsp[-1].ttype);
		  decl_attributes (&yyval.ttype,
				   chainon (yyvsp[0].ttype, all_prefix_attributes), 0); ;}
    break;

  case 393:
#line 1801 "c-parse.y"
    { yyval.ttype = grokfield (NULL_TREE, current_declspecs, yyvsp[-1].ttype);
		  decl_attributes (&yyval.ttype,
				   chainon (yyvsp[0].ttype, all_prefix_attributes), 0); ;}
    break;

  case 395:
#line 1812 "c-parse.y"
    { if (yyvsp[-2].ttype == error_mark_node)
		    yyval.ttype = yyvsp[-2].ttype;
		  else
		    TREE_CHAIN (yyvsp[0].ttype) = yyvsp[-2].ttype, yyval.ttype = yyvsp[0].ttype; ;}
    break;

  case 396:
#line 1817 "c-parse.y"
    { yyval.ttype = error_mark_node; ;}
    break;

  case 397:
#line 1823 "c-parse.y"
    { yyval.ttype = build_enumerator (yyvsp[0].ttype, NULL_TREE); ;}
    break;

  case 398:
#line 1825 "c-parse.y"
    { yyval.ttype = build_enumerator (yyvsp[-2].ttype, yyvsp[0].ttype); ;}
    break;

  case 399:
#line 1830 "c-parse.y"
    { pending_xref_error ();
		  yyval.ttype = yyvsp[0].ttype; ;}
    break;

  case 400:
#line 1833 "c-parse.y"
    { yyval.ttype = build_tree_list (yyvsp[-1].ttype, yyvsp[0].ttype); ;}
    break;

  case 401:
#line 1838 "c-parse.y"
    { yyval.ttype = NULL_TREE; ;}
    break;

  case 403:
#line 1844 "c-parse.y"
    { yyval.ttype = build_tree_list (build_tree_list (current_declspecs,
							 NULL_TREE),
					all_prefix_attributes); ;}
    break;

  case 404:
#line 1848 "c-parse.y"
    { yyval.ttype = build_tree_list (build_tree_list (current_declspecs,
							 yyvsp[0].ttype),
					all_prefix_attributes); ;}
    break;

  case 405:
#line 1852 "c-parse.y"
    { yyval.ttype = build_tree_list (build_tree_list (current_declspecs,
							 yyvsp[-1].ttype),
					chainon (yyvsp[0].ttype, all_prefix_attributes)); ;}
    break;

  case 409:
#line 1865 "c-parse.y"
    { yyval.ttype = make_pointer_declarator (yyvsp[-1].ttype, yyvsp[0].ttype); ;}
    break;

  case 410:
#line 1870 "c-parse.y"
    { yyval.ttype = make_pointer_declarator (yyvsp[0].ttype, NULL_TREE); ;}
    break;

  case 411:
#line 1872 "c-parse.y"
    { yyval.ttype = make_pointer_declarator (yyvsp[-1].ttype, yyvsp[0].ttype); ;}
    break;

  case 412:
#line 1877 "c-parse.y"
    { yyval.ttype = yyvsp[-2].ttype ? tree_cons (yyvsp[-2].ttype, yyvsp[-1].ttype, NULL_TREE) : yyvsp[-1].ttype; ;}
    break;

  case 413:
#line 1879 "c-parse.y"
    { yyval.ttype = build_nt (CALL_EXPR, yyvsp[-2].ttype, yyvsp[0].ttype, NULL_TREE); ;}
    break;

  case 414:
#line 1881 "c-parse.y"
    { yyval.ttype = set_array_declarator_type (yyvsp[0].ttype, yyvsp[-1].ttype, 1); ;}
    break;

  case 415:
#line 1883 "c-parse.y"
    { yyval.ttype = build_nt (CALL_EXPR, NULL_TREE, yyvsp[0].ttype, NULL_TREE); ;}
    break;

  case 416:
#line 1885 "c-parse.y"
    { yyval.ttype = set_array_declarator_type (yyvsp[0].ttype, NULL_TREE, 1); ;}
    break;

  case 417:
#line 1892 "c-parse.y"
    { yyval.ttype = build_array_declarator (yyvsp[-1].ttype, yyvsp[-2].ttype, 0, 0); ;}
    break;

  case 418:
#line 1894 "c-parse.y"
    { yyval.ttype = build_array_declarator (NULL_TREE, yyvsp[-1].ttype, 0, 0); ;}
    break;

  case 419:
#line 1896 "c-parse.y"
    { yyval.ttype = build_array_declarator (NULL_TREE, yyvsp[-2].ttype, 0, 1); ;}
    break;

  case 420:
#line 1898 "c-parse.y"
    { yyval.ttype = build_array_declarator (yyvsp[-1].ttype, yyvsp[-2].ttype, 1, 0); ;}
    break;

  case 421:
#line 1901 "c-parse.y"
    { yyval.ttype = build_array_declarator (yyvsp[-1].ttype, yyvsp[-3].ttype, 1, 0); ;}
    break;

  case 424:
#line 1914 "c-parse.y"
    {
		  error ("label at end of compound statement");
		;}
    break;

  case 432:
#line 1931 "c-parse.y"
    {
		  if ((pedantic && !flag_isoc99)
		      || warn_declaration_after_statement)
		    pedwarn_c90 ("ISO C90 forbids mixed declarations and code");
		;}
    break;

  case 447:
#line 1964 "c-parse.y"
    { pushlevel (0);
		  clear_last_expr ();
		  add_scope_stmt (/*begin_p=*/1, /*partial_p=*/0);
		;}
    break;

  case 448:
#line 1971 "c-parse.y"
    {
		  yyval.ttype = add_scope_stmt (/*begin_p=*/0, /*partial_p=*/0);
		;}
    break;

  case 449:
#line 1978 "c-parse.y"
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

  case 450:
#line 1994 "c-parse.y"
    { if (flag_isoc99)
		    {
		      tree scope_stmt = add_scope_stmt (/*begin_p=*/0, /*partial_p=*/0);
		      yyval.ttype = poplevel (KEEP_MAYBE, 0, 0);
		      SCOPE_STMT_BLOCK (TREE_PURPOSE (scope_stmt))
			= SCOPE_STMT_BLOCK (TREE_VALUE (scope_stmt))
			= yyval.ttype;
		    }
		  else
		    yyval.ttype = NULL_TREE; ;}
    break;

  case 452:
#line 2011 "c-parse.y"
    { if (pedantic)
		    pedwarn ("ISO C forbids label declarations"); ;}
    break;

  case 455:
#line 2022 "c-parse.y"
    { tree link;
		  for (link = yyvsp[-1].ttype; link; link = TREE_CHAIN (link))
		    {
		      tree label = declare_label (TREE_VALUE (link));
		      C_DECLARED_LABEL_FLAG (label) = 1;
		      add_decl_stmt (label);
		    }
		;}
    break;

  case 456:
#line 2036 "c-parse.y"
    {;}
    break;

  case 458:
#line 2040 "c-parse.y"
    { compstmt_count++;
                      yyval.ttype = c_begin_compound_stmt (); ;}
    break;

  case 459:
#line 2045 "c-parse.y"
    { yyval.ttype = convert (void_type_node, integer_zero_node); ;}
    break;

  case 460:
#line 2047 "c-parse.y"
    { yyval.ttype = poplevel (KEEP_MAYBE, 0, 0);
		  SCOPE_STMT_BLOCK (TREE_PURPOSE (yyvsp[0].ttype))
		    = SCOPE_STMT_BLOCK (TREE_VALUE (yyvsp[0].ttype))
		    = yyval.ttype; ;}
    break;

  case 463:
#line 2060 "c-parse.y"
    { if (last_tree == NULL)
		    {
		      error ("braced-group within expression allowed only inside a function");
		      YYERROR;
		    }
		  /* We must force a BLOCK for this level
		     so that, if it is not expanded later,
		     there is a way to turn off the entire subtree of blocks
		     that are contained in it.  */
		  keep_next_level ();
		  compstmt_count++;
		  yyval.ttype = add_stmt (build_stmt (COMPOUND_STMT, last_tree));
		  last_expr_type = NULL_TREE;
		;}
    break;

  case 464:
#line 2077 "c-parse.y"
    { RECHAIN_STMTS (yyvsp[-1].ttype, COMPOUND_BODY (yyvsp[-1].ttype));
		  last_expr_type = NULL_TREE;
                  yyval.ttype = yyvsp[-1].ttype; ;}
    break;

  case 465:
#line 2085 "c-parse.y"
    { c_finish_then (); ;}
    break;

  case 467:
#line 2102 "c-parse.y"
    { yyval.ttype = c_begin_if_stmt (); ;}
    break;

  case 468:
#line 2104 "c-parse.y"
    { c_expand_start_cond (c_common_truthvalue_conversion (yyvsp[-1].ttype),
				       compstmt_count,yyvsp[-3].ttype);
		  yyval.itype = stmt_count;
		  if_stmt_locus = yyvsp[-6].location; ;}
    break;

  case 469:
#line 2115 "c-parse.y"
    { stmt_count++;
		  compstmt_count++;
		  c_in_iteration_stmt++;
		  yyval.ttype
		    = add_stmt (build_stmt (DO_STMT, NULL_TREE,
					    NULL_TREE));
		  /* In the event that a parse error prevents
		     parsing the complete do-statement, set the
		     condition now.  Otherwise, we can get crashes at
		     RTL-generation time.  */
		  DO_COND (yyval.ttype) = error_mark_node; ;}
    break;

  case 470:
#line 2127 "c-parse.y"
    { yyval.ttype = yyvsp[-2].ttype;
		  RECHAIN_STMTS (yyval.ttype, DO_BODY (yyval.ttype));
		  c_in_iteration_stmt--; ;}
    break;

  case 471:
#line 2137 "c-parse.y"
    { if (yychar == YYEMPTY)
		    yychar = YYLEX;
		  yyval.location = input_location; ;}
    break;

  case 474:
#line 2150 "c-parse.y"
    { if (flag_isoc99)
		    RECHAIN_STMTS (yyvsp[-2].ttype, COMPOUND_BODY (yyvsp[-2].ttype)); ;}
    break;

  case 475:
#line 2156 "c-parse.y"
    { if (yyvsp[0].ttype)
		    {
		      STMT_LINENO (yyvsp[0].ttype) = yyvsp[-1].location.line;
		      /* ??? We currently have no way of recording
			 the filename for a statement.  This probably
			 matters little in practice at the moment,
			 but I suspect that problems will occur when
			 doing inlining at the tree level.  */
		    }
		;}
    break;

  case 476:
#line 2170 "c-parse.y"
    { if (yyvsp[0].ttype)
		    {
		      STMT_LINENO (yyvsp[0].ttype) = yyvsp[-1].location.line;
		    }
		;}
    break;

  case 477:
#line 2179 "c-parse.y"
    { c_expand_start_else ();
		  yyvsp[-1].itype = stmt_count; ;}
    break;

  case 478:
#line 2182 "c-parse.y"
    { c_finish_else ();
		  c_expand_end_cond ();
		  if (extra_warnings && stmt_count == yyvsp[-3].itype)
		    warning ("empty body in an else-statement"); ;}
    break;

  case 479:
#line 2187 "c-parse.y"
    { c_expand_end_cond ();
		  /* This warning is here instead of in simple_if, because we
		     do not want a warning if an empty if is followed by an
		     else statement.  Increment stmt_count so we don't
		     give a second error if this is a nested `if'.  */
		  if (extra_warnings && stmt_count++ == yyvsp[0].itype)
		    warning ("%Hempty body in an if-statement",
                             &if_stmt_locus); ;}
    break;

  case 480:
#line 2199 "c-parse.y"
    { c_expand_end_cond (); ;}
    break;

  case 481:
#line 2209 "c-parse.y"
    { stmt_count++;
		  yyval.ttype = c_begin_while_stmt (); ;}
    break;

  case 482:
#line 2212 "c-parse.y"
    { c_in_iteration_stmt++;
		  yyvsp[-1].ttype = c_common_truthvalue_conversion (yyvsp[-1].ttype);
		  c_finish_while_stmt_cond
		    (c_common_truthvalue_conversion (yyvsp[-1].ttype), yyvsp[-3].ttype);
		  yyval.ttype = add_stmt (yyvsp[-3].ttype); ;}
    break;

  case 483:
#line 2218 "c-parse.y"
    { c_in_iteration_stmt--;
		  RECHAIN_STMTS (yyvsp[-1].ttype, WHILE_BODY (yyvsp[-1].ttype)); ;}
    break;

  case 484:
#line 2222 "c-parse.y"
    { DO_COND (yyvsp[-4].ttype) = c_common_truthvalue_conversion (yyvsp[-2].ttype); ;}
    break;

  case 485:
#line 2224 "c-parse.y"
    { ;}
    break;

  case 486:
#line 2226 "c-parse.y"
    { yyval.ttype = build_stmt (FOR_STMT, NULL_TREE, NULL_TREE,
					  NULL_TREE, NULL_TREE);
		  add_stmt (yyval.ttype); ;}
    break;

  case 487:
#line 2230 "c-parse.y"
    { stmt_count++;
		  RECHAIN_STMTS (yyvsp[-2].ttype, FOR_INIT_STMT (yyvsp[-2].ttype)); ;}
    break;

  case 488:
#line 2233 "c-parse.y"
    { if (yyvsp[-1].ttype)
		    FOR_COND (yyvsp[-5].ttype)
		      = c_common_truthvalue_conversion (yyvsp[-1].ttype); ;}
    break;

  case 489:
#line 2237 "c-parse.y"
    { c_in_iteration_stmt++;
		  FOR_EXPR (yyvsp[-8].ttype) = yyvsp[-1].ttype; ;}
    break;

  case 490:
#line 2240 "c-parse.y"
    { RECHAIN_STMTS (yyvsp[-10].ttype, FOR_BODY (yyvsp[-10].ttype));
		  c_in_iteration_stmt--;;}
    break;

  case 491:
#line 2243 "c-parse.y"
    { stmt_count++;
		  yyval.ttype = c_start_case (yyvsp[-1].ttype);
		  c_in_case_stmt++; ;}
    break;

  case 492:
#line 2247 "c-parse.y"
    { c_finish_case ();
		  c_in_case_stmt--; ;}
    break;

  case 493:
#line 2253 "c-parse.y"
    { add_stmt (build_stmt (EXPR_STMT, yyvsp[-1].ttype)); ;}
    break;

  case 494:
#line 2255 "c-parse.y"
    { check_for_loop_decls (); ;}
    break;

  case 495:
#line 2261 "c-parse.y"
    { stmt_count++; yyval.ttype = yyvsp[0].ttype; ;}
    break;

  case 496:
#line 2263 "c-parse.y"
    { stmt_count++;
		  yyval.ttype = c_expand_expr_stmt (yyvsp[-1].ttype); ;}
    break;

  case 497:
#line 2266 "c-parse.y"
    { if (flag_isoc99)
		    RECHAIN_STMTS (yyvsp[-2].ttype, COMPOUND_BODY (yyvsp[-2].ttype));
		  yyval.ttype = NULL_TREE; ;}
    break;

  case 498:
#line 2270 "c-parse.y"
    { stmt_count++;
		if (!(c_in_iteration_stmt || c_in_case_stmt))
		  {
		    error ("break statement not within loop or switch");
		    yyval.ttype = NULL_TREE;
		  }
		else
		  yyval.ttype = add_stmt (build_break_stmt ()); ;}
    break;

  case 499:
#line 2279 "c-parse.y"
    { stmt_count++;
		if (!c_in_iteration_stmt)
		  {
		    error ("continue statement not within a loop");
		    yyval.ttype = NULL_TREE;
		  }
		else
		  yyval.ttype = add_stmt (build_continue_stmt ()); ;}
    break;

  case 500:
#line 2288 "c-parse.y"
    { stmt_count++;
		  yyval.ttype = c_expand_return (NULL_TREE); ;}
    break;

  case 501:
#line 2291 "c-parse.y"
    { stmt_count++;
		  yyval.ttype = c_expand_return (yyvsp[-1].ttype); ;}
    break;

  case 502:
#line 2294 "c-parse.y"
    { stmt_count++;
		  yyval.ttype = simple_asm_stmt (yyvsp[-2].ttype); ;}
    break;

  case 503:
#line 2298 "c-parse.y"
    { stmt_count++;
		  yyval.ttype = build_asm_stmt (yyvsp[-6].ttype, yyvsp[-4].ttype, yyvsp[-2].ttype, NULL_TREE, NULL_TREE); ;}
    break;

  case 504:
#line 2303 "c-parse.y"
    { stmt_count++;
		  yyval.ttype = build_asm_stmt (yyvsp[-8].ttype, yyvsp[-6].ttype, yyvsp[-4].ttype, yyvsp[-2].ttype, NULL_TREE); ;}
    break;

  case 505:
#line 2308 "c-parse.y"
    { stmt_count++;
		  yyval.ttype = build_asm_stmt (yyvsp[-10].ttype, yyvsp[-8].ttype, yyvsp[-6].ttype, yyvsp[-4].ttype, yyvsp[-2].ttype); ;}
    break;

  case 506:
#line 2311 "c-parse.y"
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

  case 507:
#line 2323 "c-parse.y"
    { if (pedantic)
		    pedwarn ("ISO C forbids `goto *expr;'");
		  stmt_count++;
		  yyvsp[-1].ttype = convert (ptr_type_node, yyvsp[-1].ttype);
		  yyval.ttype = add_stmt (build_stmt (GOTO_STMT, yyvsp[-1].ttype)); ;}
    break;

  case 508:
#line 2329 "c-parse.y"
    { yyval.ttype = NULL_TREE; ;}
    break;

  case 509:
#line 2337 "c-parse.y"
    { stmt_count++;
		  yyval.ttype = do_case (yyvsp[-1].ttype, NULL_TREE); ;}
    break;

  case 510:
#line 2340 "c-parse.y"
    { stmt_count++;
		  yyval.ttype = do_case (yyvsp[-3].ttype, yyvsp[-1].ttype); ;}
    break;

  case 511:
#line 2343 "c-parse.y"
    { stmt_count++;
		  yyval.ttype = do_case (NULL_TREE, NULL_TREE); ;}
    break;

  case 512:
#line 2346 "c-parse.y"
    { tree label = define_label (yyvsp[-2].location, yyvsp[-3].ttype);
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

  case 513:
#line 2362 "c-parse.y"
    { yyval.ttype = NULL_TREE; ;}
    break;

  case 514:
#line 2364 "c-parse.y"
    { ;}
    break;

  case 515:
#line 2369 "c-parse.y"
    { yyval.ttype = NULL_TREE; ;}
    break;

  case 517:
#line 2376 "c-parse.y"
    { yyval.ttype = NULL_TREE; ;}
    break;

  case 520:
#line 2383 "c-parse.y"
    { yyval.ttype = chainon (yyvsp[-2].ttype, yyvsp[0].ttype); ;}
    break;

  case 521:
#line 2388 "c-parse.y"
    { yyval.ttype = build_tree_list (build_tree_list (NULL_TREE, yyvsp[-3].ttype), yyvsp[-1].ttype); ;}
    break;

  case 522:
#line 2390 "c-parse.y"
    { yyvsp[-5].ttype = build_string (IDENTIFIER_LENGTH (yyvsp[-5].ttype),
				     IDENTIFIER_POINTER (yyvsp[-5].ttype));
		  yyval.ttype = build_tree_list (build_tree_list (yyvsp[-5].ttype, yyvsp[-3].ttype), yyvsp[-1].ttype); ;}
    break;

  case 523:
#line 2397 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, NULL_TREE); ;}
    break;

  case 524:
#line 2399 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, yyvsp[0].ttype, yyvsp[-2].ttype); ;}
    break;

  case 525:
#line 2409 "c-parse.y"
    { pushlevel (0);
		  declare_parm_level (); ;}
    break;

  case 526:
#line 2412 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype;
		  poplevel (0, 0, 0); ;}
    break;

  case 528:
#line 2419 "c-parse.y"
    { mark_forward_parm_decls (); ;}
    break;

  case 529:
#line 2421 "c-parse.y"
    { /* Dummy action so attributes are in known place
		     on parser stack.  */ ;}
    break;

  case 530:
#line 2424 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype; ;}
    break;

  case 531:
#line 2426 "c-parse.y"
    { yyval.ttype = tree_cons (NULL_TREE, NULL_TREE, NULL_TREE); ;}
    break;

  case 532:
#line 2432 "c-parse.y"
    { yyval.ttype = get_parm_info (0); ;}
    break;

  case 533:
#line 2434 "c-parse.y"
    { yyval.ttype = get_parm_info (0);
		  /* Gcc used to allow this as an extension.  However, it does
		     not work for all targets, and thus has been disabled.
		     Also, since func (...) and func () are indistinguishable,
		     it caused problems with the code in expand_builtin which
		     tries to verify that BUILT_IN_NEXT_ARG is being used
		     correctly.  */
		  error ("ISO C requires a named argument before `...'");
		  parsing_iso_function_signature = true;
		;}
    break;

  case 534:
#line 2445 "c-parse.y"
    { yyval.ttype = get_parm_info (1);
		  parsing_iso_function_signature = true;
		;}
    break;

  case 535:
#line 2449 "c-parse.y"
    { yyval.ttype = get_parm_info (0);
		  parsing_iso_function_signature = true;
		;}
    break;

  case 536:
#line 2456 "c-parse.y"
    { push_parm_decl (yyvsp[0].ttype); ;}
    break;

  case 537:
#line 2458 "c-parse.y"
    { push_parm_decl (yyvsp[0].ttype); ;}
    break;

  case 538:
#line 2465 "c-parse.y"
    { yyval.ttype = build_tree_list (build_tree_list (current_declspecs,
							 yyvsp[-1].ttype),
					chainon (yyvsp[0].ttype, all_prefix_attributes));
		  POP_DECLSPEC_STACK; ;}
    break;

  case 539:
#line 2470 "c-parse.y"
    { yyval.ttype = build_tree_list (build_tree_list (current_declspecs,
							 yyvsp[-1].ttype),
					chainon (yyvsp[0].ttype, all_prefix_attributes));
		  POP_DECLSPEC_STACK; ;}
    break;

  case 540:
#line 2475 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype;
		  POP_DECLSPEC_STACK; ;}
    break;

  case 541:
#line 2478 "c-parse.y"
    { yyval.ttype = build_tree_list (build_tree_list (current_declspecs,
							 yyvsp[-1].ttype),
					chainon (yyvsp[0].ttype, all_prefix_attributes));
		  POP_DECLSPEC_STACK; ;}
    break;

  case 542:
#line 2484 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype;
		  POP_DECLSPEC_STACK; ;}
    break;

  case 543:
#line 2492 "c-parse.y"
    { yyval.ttype = build_tree_list (build_tree_list (current_declspecs,
							 yyvsp[-1].ttype),
					chainon (yyvsp[0].ttype, all_prefix_attributes));
		  POP_DECLSPEC_STACK; ;}
    break;

  case 544:
#line 2497 "c-parse.y"
    { yyval.ttype = build_tree_list (build_tree_list (current_declspecs,
							 yyvsp[-1].ttype),
					chainon (yyvsp[0].ttype, all_prefix_attributes));
		  POP_DECLSPEC_STACK; ;}
    break;

  case 545:
#line 2502 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype;
		  POP_DECLSPEC_STACK; ;}
    break;

  case 546:
#line 2505 "c-parse.y"
    { yyval.ttype = build_tree_list (build_tree_list (current_declspecs,
							 yyvsp[-1].ttype),
					chainon (yyvsp[0].ttype, all_prefix_attributes));
		  POP_DECLSPEC_STACK; ;}
    break;

  case 547:
#line 2511 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype;
		  POP_DECLSPEC_STACK; ;}
    break;

  case 548:
#line 2517 "c-parse.y"
    { prefix_attributes = chainon (prefix_attributes, yyvsp[-3].ttype);
		  all_prefix_attributes = prefix_attributes; ;}
    break;

  case 549:
#line 2526 "c-parse.y"
    { pushlevel (0);
		  declare_parm_level (); ;}
    break;

  case 550:
#line 2529 "c-parse.y"
    { yyval.ttype = yyvsp[0].ttype;
		  poplevel (0, 0, 0); ;}
    break;

  case 552:
#line 2536 "c-parse.y"
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

  case 553:
#line 2554 "c-parse.y"
    { yyval.ttype = build_tree_list (NULL_TREE, yyvsp[0].ttype); ;}
    break;

  case 554:
#line 2556 "c-parse.y"
    { yyval.ttype = chainon (yyvsp[-2].ttype, build_tree_list (NULL_TREE, yyvsp[0].ttype)); ;}
    break;

  case 555:
#line 2562 "c-parse.y"
    { yyval.ttype = build_tree_list (NULL_TREE, yyvsp[0].ttype); ;}
    break;

  case 556:
#line 2564 "c-parse.y"
    { yyval.ttype = chainon (yyvsp[-2].ttype, build_tree_list (NULL_TREE, yyvsp[0].ttype)); ;}
    break;

  case 557:
#line 2569 "c-parse.y"
    { yyval.itype = SAVE_EXT_FLAGS();
		  pedantic = 0;
		  warn_pointer_arith = 0;
		  warn_traditional = 0;
		  flag_iso = 0; ;}
    break;


    }

/* Line 991 of yacc.c.  */
#line 5302 "c-parse.c"

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
#if defined (__GNUC_MINOR__) && 2093 <= (__GNUC__ * 1000 + __GNUC_MINOR__) \
    && !defined __cplusplus
  __attribute__ ((__unused__))
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


#line 2576 "c-parse.y"


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
#define D_C89	0x01	/* not in C89 */
#define D_EXT	0x02	/* GCC extension */
#define D_EXT89	0x04	/* GCC extension incorporated in C99 */
#define D_OBJC	0x08	/* Objective C only */

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
  { "__thread",		RID_THREAD,	0 },
  { "__typeof",		RID_TYPEOF,	0 },
  { "__typeof__",	RID_TYPEOF,	0 },
  { "__volatile",	RID_VOLATILE,	0 },
  { "__volatile__",	RID_VOLATILE,	0 },
  { "asm",		RID_ASM,	D_EXT },
  { "auto",		RID_AUTO,	0 },
  { "break",		RID_BREAK,	0 },
  { "case",		RID_CASE,	0 },
  { "char",		RID_CHAR,	0 },
  { "const",		RID_CONST,	0 },
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
  { "inline",		RID_INLINE,	D_EXT89 },
  { "int",		RID_INT,	0 },
  { "long",		RID_LONG,	0 },
  { "register",		RID_REGISTER,	0 },
  { "restrict",		RID_RESTRICT,	D_C89 },
  { "return",		RID_RETURN,	0 },
  { "short",		RID_SHORT,	0 },
  { "signed",		RID_SIGNED,	0 },
  { "sizeof",		RID_SIZEOF,	0 },
  { "static",		RID_STATIC,	0 },
  { "struct",		RID_STRUCT,	0 },
  { "switch",		RID_SWITCH,	0 },
  { "typedef",		RID_TYPEDEF,	0 },
  { "typeof",		RID_TYPEOF,	D_EXT },
  { "union",		RID_UNION,	0 },
  { "unsigned",		RID_UNSIGNED,	0 },
  { "void",		RID_VOID,	0 },
  { "volatile",		RID_VOLATILE,	0 },
  { "while",		RID_WHILE,	0 },
};
#define N_reswords (sizeof reswords / sizeof (struct resword))

/* Table mapping from RID_* constants to yacc token numbers.
   Unfortunately we have to have entries for all the keywords in all
   three languages.  */
static const short rid_to_yy[RID_MAX] =
{
  /* RID_STATIC */	STATIC,
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
  /* RID_COMPLEX */	TYPESPEC,
  /* RID_THREAD */	SCSPEC,

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

  /* RID_FUNCTION_NAME */		FUNC_NAME,
  /* RID_PRETTY_FUNCTION_NAME */	FUNC_NAME,
  /* RID_C99_FUNCTION_NAME */		FUNC_NAME,

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
  /* RID_OFFSETOF */    0,
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
  /* RID_AT_THROW */		AT_THROW,
  /* RID_AT_TRY */		AT_TRY,
  /* RID_AT_CATCH */		AT_CATCH,
  /* RID_AT_FINALLY */		AT_FINALLY,
  /* RID_AT_SYNCHRONIZED */	AT_SYNCHRONIZED,
  /* RID_AT_INTERFACE */	INTERFACE,
  /* RID_AT_IMPLEMENTATION */	IMPLEMENTATION
};

static void
init_reswords (void)
{
  unsigned int i;
  tree id;
  int mask = (flag_isoc99 ? 0 : D_C89)
	      | (flag_no_asm ? (flag_isoc99 ? D_EXT : D_EXT|D_EXT89) : 0);

  if (!c_dialect_objc ())
     mask |= D_OBJC;

  ridpointers = ggc_calloc ((int) RID_MAX, sizeof (tree));
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
yyerror (const char *msgid)
{
  c_parse_error (msgid, last_token, yylval.ttype);
}

static int
yylexname (void)
{
  tree decl;


  if (C_IS_RESERVED_WORD (yylval.ttype))
    {
      enum rid rid_code = C_RID_CODE (yylval.ttype);

      {
	/* Return the canonical spelling for this keyword.  */
	yylval.ttype = ridpointers[(int) rid_code];
	return rid_to_yy[(int) rid_code];
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
_yylex (void)
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

    case CPP_AT_NAME:
      /* This only happens in Objective-C; it must be a keyword.  */
      return rid_to_yy [(int) C_RID_CODE (yylval.ttype)];

    case CPP_NUMBER:
    case CPP_CHAR:
    case CPP_WCHAR:
      return CONSTANT;

    case CPP_STRING:
    case CPP_WSTRING:
      return STRING;

    case CPP_OBJC_STRING:
      return OBJC_STRING;

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
yylex (void)
{
  int r;
  timevar_push (TV_LEX);
  r = _yylex();
  timevar_pop (TV_LEX);
  return r;
}

/* Function used when yydebug is set, to print a token in more detail.  */

static void
yyprint (FILE *file, int yychar, YYSTYPE yyl)
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
    case STATIC:
      if (IDENTIFIER_POINTER (t))
	fprintf (file, " `%s'", IDENTIFIER_POINTER (t));
      break;

    case CONSTANT:
      fprintf (file, " %s", GET_MODE_NAME (TYPE_MODE (TREE_TYPE (t))));
      if (TREE_CODE (t) == INTEGER_CST)
	{
	  fputs (" ", file);
	  fprintf (file, HOST_WIDE_INT_PRINT_DOUBLE_HEX,
		   TREE_INT_CST_HIGH (t), TREE_INT_CST_LOW (t));
	}
      break;
    }
}

/* This is not the ideal place to put these, but we have to get them out
   of c-lex.c because cp/lex.c has its own versions.  */

/* Parse the file.  */
void
c_parse_file (void)
{
  yyparse ();
  /* In case there were missing closebraces, get us back to the global
     binding level.  */
  while (! global_bindings_p ())
    poplevel (0, 0, 0);
  /* __FUNCTION__ is defined at file scope ("").  This
     call may not be necessary as my tests indicate it
     still works without it.  */
  finish_fname_decls ();

  if (malloced_yyss)
    {
      free (malloced_yyss);
      free (malloced_yyvs);
      malloced_yyss = 0;
    }
}

#include "gt-c-parse.h"


