/* C code produced by gperf version 2.1 (K&R C version) */
/* Command-line: gperf -p -j1 -i 1 -g -o -t -G -N is_reserved_word -k1,3,$ ../../gcc/gcc/c-parse.gperf  */


/* Command-line: gperf -L KR-C -F ', 0, 0' -p -j1 -i 1 -g -o -t -N is_reserved_word -k1,3,$ c-parse.gperf  */ 
struct resword { const char *name; short token; enum rid rid; };

#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 20
#define MIN_HASH_VALUE 8
#define MAX_HASH_VALUE 141
/*
   83 keywords
  134 is the maximum key range
*/

#ifdef __GNUC__
inline
#endif
static int
hash (str, len)
     register char *str;
     register unsigned int  len;
{
  static unsigned char hash_table[] =
    {
     141, 141, 141, 141, 141, 141, 141, 141, 141, 141,
     141, 141, 141, 141, 141, 141, 141, 141, 141, 141,
     141, 141, 141, 141, 141, 141, 141, 141, 141, 141,
     141, 141, 141, 141, 141, 141, 141, 141, 141, 141,
     141, 141, 141, 141, 141, 141, 141, 141, 141, 141,
     141, 141, 141, 141, 141, 141, 141, 141, 141, 141,
     141, 141, 141, 141,  35, 141, 141, 141, 141, 141,
     141, 141, 141, 141, 141, 141, 141, 141, 141, 141,
     141, 141, 141, 141, 141, 141, 141, 141, 141, 141,
     141, 141, 141, 141, 141,   1, 141,  90,   1,  28,
      40,   6,   1,  24,   3,  13, 141,  36,  60,  14,
      49,   3,   6, 141,  19,   8,   1,  50,  33,  11,
       2,  23,   4, 141, 141, 141, 141, 141,
    };
  register int hval = len;

  switch (hval)
    {
      default:
      case 3:
        hval += hash_table[str[2]];
      case 2:
      case 1:
        hval += hash_table[str[0]];
    }
  return hval + hash_table[str[len - 1]] ;
}


static struct resword  wordlist[] =
{
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"out",  TYPE_QUAL, RID_OUT},
      {"",}, 
      {"float",  TYPESPEC, RID_FLOAT},
      {"__typeof",  TYPEOF, NORID},
      {"",}, 
      {"__typeof__",  TYPEOF, NORID},
      {"typeof",  TYPEOF, NORID},
      {"typedef",  SCSPEC, RID_TYPEDEF},
      {"if",  IF, NORID},
      {"short",  TYPESPEC, RID_SHORT},
      {"int",  TYPESPEC, RID_INT},
      {"sizeof",  SIZEOF, NORID},
      {"__signed__",  TYPESPEC, RID_SIGNED},
      {"__extension__",  EXTENSION, NORID},
      {"inout",  TYPE_QUAL, RID_INOUT},
      {"__imag__",  IMAGPART, NORID},
      {"else",  ELSE, NORID},
      {"__inline__",  SCSPEC, RID_INLINE},
      {"byref",  TYPE_QUAL, RID_BYREF},
      {"__iterator__",  SCSPEC, RID_ITERATOR},
      {"__inline",  SCSPEC, RID_INLINE},
      {"__real__",  REALPART, NORID},
      {"switch",  SWITCH, NORID},
      {"__restrict",  TYPE_QUAL, RID_RESTRICT},
      {"goto",  GOTO, NORID},
      {"__restrict__",  TYPE_QUAL, RID_RESTRICT},
      {"struct",  STRUCT, NORID},
      {"while",  WHILE, NORID},
      {"restrict",  TYPE_QUAL, RID_RESTRICT},
      {"__const",  TYPE_QUAL, RID_CONST},
      {"oneway",  TYPE_QUAL, RID_ONEWAY},
      {"__const__",  TYPE_QUAL, RID_CONST},
      {"__complex",  TYPESPEC, RID_COMPLEX},
      {"__complex__",  TYPESPEC, RID_COMPLEX},
      {"for",  FOR, NORID},
      {"__iterator",  SCSPEC, RID_ITERATOR},
      {"__imag",  IMAGPART, NORID},
      {"do",  DO, NORID},
      {"case",  CASE, NORID},
      {"__volatile__",  TYPE_QUAL, RID_VOLATILE},
      {"break",  BREAK, NORID},
      {"default",  DEFAULT, NORID},
      {"__volatile",  TYPE_QUAL, RID_VOLATILE},
      {"",}, {"",}, {"",}, 
      {"@defs",  DEFS, NORID},
      {"id",  OBJECTNAME, RID_ID},
      {"",}, 
      {"__signed",  TYPESPEC, RID_SIGNED},
      {"bycopy",  TYPE_QUAL, RID_BYCOPY},
      {"",}, {"",}, {"",}, 
      {"extern",  SCSPEC, RID_EXTERN},
      {"",}, 
      {"in",  TYPE_QUAL, RID_IN},
      {"",}, 
      {"@compatibility_alias",  ALIAS, NORID},
      {"",}, 
      {"@private",  PRIVATE, NORID},
      {"@selector",  SELECTOR, NORID},
      {"register",  SCSPEC, RID_REGISTER},
      {"__label__",  LABEL, NORID},
      {"",}, {"",}, 
      {"enum",  ENUM, NORID},
      {"return",  RETURN, NORID},
      {"",}, {"",}, 
      {"signed",  TYPESPEC, RID_SIGNED},
      {"",}, {"",}, {"",}, {"",}, 
      {"const",  TYPE_QUAL, RID_CONST},
      {"",}, 
      {"inline",  SCSPEC, RID_INLINE},
      {"__real",  REALPART, NORID},
      {"",}, {"",}, {"",}, 
      {"void",  TYPESPEC, RID_VOID},
      {"continue",  CONTINUE, NORID},
      {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"@encode",  ENCODE, NORID},
      {"auto",  SCSPEC, RID_AUTO},
      {"__asm__",  ASM_KEYWORD, NORID},
      {"@interface",  INTERFACE, NORID},
      {"__alignof",  ALIGNOF, NORID},
      {"double",  TYPESPEC, RID_DOUBLE},
      {"__alignof__",  ALIGNOF, NORID},
      {"@protected",  PROTECTED, NORID},
      {"__attribute__",  ATTRIBUTE, NORID},
      {"unsigned",  TYPESPEC, RID_UNSIGNED},
      {"volatile",  TYPE_QUAL, RID_VOLATILE},
      {"__attribute",  ATTRIBUTE, NORID},
      {"@class",  CLASS, NORID},
      {"__asm",  ASM_KEYWORD, NORID},
      {"",}, {"",}, 
      {"@implementation",  IMPLEMENTATION, NORID},
      {"",}, {"",}, {"",}, 
      {"union",  UNION, NORID},
      {"",}, {"",}, 
      {"@public",  PUBLIC, NORID},
      {"asm",  ASM_KEYWORD, NORID},
      {"",}, 
      {"@protocol",  PROTOCOL, NORID},
      {"",}, {"",}, {"",}, {"",}, 
      {"@end",  END, NORID},
      {"",}, {"",}, {"",}, 
      {"static",  SCSPEC, RID_STATIC},
      {"",}, {"",}, {"",}, {"",}, 
      {"long",  TYPESPEC, RID_LONG},
      {"",}, {"",}, {"",}, 
      {"char",  TYPESPEC, RID_CHAR},
};

#ifdef __GNUC__
inline
#endif
struct resword *
is_reserved_word (str, len)
     register char *str;
     register unsigned int len;
{
  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= MIN_HASH_VALUE)
        {
          register char *s = wordlist[key].name;

          if (*s == *str && !strcmp (str + 1, s + 1))
            return &wordlist[key];
        }
    }
  return 0;
}
