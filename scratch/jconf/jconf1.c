/* usage:
cc -c jconf1.c
fgrep -a mark14441E252AE140E9AA2387CFB12DA004 jconf1.o
*/
//#include <limits.h>
#define CHAR_BIT 8
#define SP ' ',
#define NL '\n',
/* mark is a guid with a readable prefix */
#define MARK ' ','/','*','m','a','r','k','1','4','4','4','1','E','2',\
             '5','2','A','E','1','4','0','E','9','A','A','2','3','8',\
             '7','C','F','B','1','2','D','A','0','0','4','*','/',NL
#define DEFINE NL '#','d','e','f','i','n','e',SP
#define SIZEOF 'S','I','Z','E','O','F','_',
#define XSIZEOF 'X',SIZEOF

/* one hex digit */
#define X1(x) (((x) < 10) ? ('0'+(x)) : ('A'+(x)-10)),
/* the nth hex digit */
#define Xn(x, n) X1(((x)>>(4*n)) & 0xF)

/* a hex constant up to 7 digits -- limit 7 for portability of right shift */
/* TODO support very large numbers, e.g. 64bit numbers if implementation
   has them, else 32bit, even if integers are 16 bits */
#define X(x) ,SP '0','x',Xn(x,7)Xn(x,6)Xn(x,5)Xn(x,4)Xn(x,3)Xn(x,2)Xn(x,1)Xn(x,0)

#define I1000(i) (i < 1000) ? ' ' : '0' + (i / 1000 % 10),
#define I100(i)  (i < 100) ? ' ' : '0' + (i / 100 % 10),
#define I10(i)   (i < 10) ? ' ' : '0' + (i / 10 % 10),
#define I1(i)   '0' + (i % 10),
#define I(i)   ,SP I1000(i) I100(i) I10(i) I1(i)

typedef struct t5 { char a[5]; } t5;
typedef struct t12 { char a[12]; } t12;
typedef struct t123 { char a[123]; } t123;
typedef struct t1234 { char a[1234]; } t1234;

char jconf1[] = {
DEFINE SIZEOF 'P','T','R'           I(sizeof(char*)) MARK
DEFINE SIZEOF 'I','N','T'           I(sizeof(int)) MARK
DEFINE SIZEOF 'T','5'               I(sizeof(t5)) MARK
DEFINE SIZEOF 'T','1','2'           I(sizeof(t12)) MARK
DEFINE SIZEOF 'T','1','2','3'       I(sizeof(t123)) MARK
DEFINE SIZEOF 'T','1','2','3','4'   I(sizeof(t1234)) MARK

DEFINE XSIZEOF 'P','T','R'           X(sizeof(char*)) MARK
DEFINE XSIZEOF 'I','N','T'           X(sizeof(int)) MARK
DEFINE XSIZEOF 'T','5'               X(sizeof(t5)) MARK
DEFINE XSIZEOF 'T','1','2'           X(sizeof(t12)) MARK
DEFINE XSIZEOF 'T','1','2','3'       X(sizeof(t123)) MARK
DEFINE XSIZEOF 'T','1','2','3','4'   X(sizeof(t1234)) MARK

/* c is for constant? note that this is backwards/useless */
#define C(a)  ,SP (a) == 0 ? '0' : C2(a)
#define C2(a) (a) == 0 ? ' ' : '0' + (a) % 10, C3((a)/10)
#define C3(a) (a) == 0 ? ' ' : '0' + (a) % 10, C4((a)/10)
#define C4(a) (a) == 0 ? ' ' : '0' + (a) % 10, C5((a)/10)
#define C5(a) (a) == 0 ? ' ' : '0' + (a) % 10, C6((a)/10)
#define C6(a) (a) == 0 ? ' ' : '0' + (a) % 10, C7((a)/10)
#define C7(a) (a) == 0 ? ' ' : '0' + (a) % 10, C8((a)/10)
#define C8(a) (a) == 0 ? ' ' : '0' + (a) % 10, C9((a)/10)
#define C9(a) (a) == 0 ? ' ' : '0' + (a) % 10, CA((a)/10)
#define CA(a) (a) == 0 ? ' ' : '0' + (a) % 10, CB((a)/10)
#define CB(a) (a) == 0 ? ' ' : '0' + (a) % 10, CC((a)/10)
#define CC(a) (a) == 0 ? ' ' : '0' + (a) % 10, CD((a)/10)
#define CD(a) (a) == 0 ? ' ' : '0' + (a) % 10,

//2147483647
DEFINE 'A' C((~0UL>>1)) MARK
// show that zeros anywhere work
DEFINE 'B' C((~0UL>>1) - 7) MARK
DEFINE 'C' C((~0UL>>1) - 47) MARK
DEFINE 'D' C((~0UL>>1) - 40) MARK
DEFINE 'E' C((~0UL>>2)) MARK

//now fix it by reversing it -- must flatten
// D is for decimal?
// first digit is special -- 0 isn't all spaces
#define D1(a) '0' + (a) % 10,
#define D2(a) (a) == 0 ? ' ' : '0' + (a) % 10,

#define D(a) ,SP \
    D2((a)/10/10/10/10/10/10/10/10/10/10/10/10/10/10/10/10/10/10/10) \
    D2((a)/10/10/10/10/10/10/10/10/10/10/10/10/10/10/10/10/10/10) \
    D2((a)/10/10/10/10/10/10/10/10/10/10/10/10/10/10/10/10/10) \
    D2((a)/10/10/10/10/10/10/10/10/10/10/10/10/10/10/10/10) \
    D2((a)/10/10/10/10/10/10/10/10/10/10/10/10/10/10/10) \
    D2((a)/10/10/10/10/10/10/10/10/10/10/10/10/10/10) \
    D2((a)/10/10/10/10/10/10/10/10/10/10/10/10/10) \
    D2((a)/10/10/10/10/10/10/10/10/10/10/10/10) \
    D2((a)/10/10/10/10/10/10/10/10/10/10/10) \
    D2((a)/10/10/10/10/10/10/10/10/10/10) \
    D2((a)/10/10/10/10/10/10/10/10/10) \
    D2((a)/10/10/10/10/10/10/10/10) \
    D2((a)/10/10/10/10/10/10/10) \
    D2((a)/10/10/10/10/10/10) \
    D2((a)/10/10/10/10/10) \
    D2((a)/10/10/10/10) \
    D2((a)/10/10/10) \
    D2((a)/10/10) \
    D2((a)/10) \
    D1((a)) \

MARK

//2147483647

DEFINE 'F' D((~0U>>1)) MARK
// show that zeros anywhere work
DEFINE 'G' D((~0U>>1) - 7) MARK
DEFINE 'H' D((~0U>>1) - 47) MARK
DEFINE 'I' D((~0U>>1) - 40) MARK
DEFINE 'J' D((~0U>>2)) MARK
DEFINE 'K' D((~0U>>3)) MARK
DEFINE 'M' D(0) MARK

#define MAX ,'M','A','X'
/* NOT: This is not a valid implementation of "MIN".
 * You must use -MAX-1 if two's complement.
 * TODO a test for two's complement.
 */
#define MIN ,'M','I','N',' ','-'
DEFINE 'I' MAX D(~0u>>1) MARK
DEFINE 'I' MIN D((~0u>>1)+1) MARK /* not conformant, see limits.h */
DEFINE 'U' MAX D(~0u) MARK

//9223372036854775807
//18446744073709551615
DEFINE 'L' MAX D(~0uL>>1) MARK
DEFINE 'U','L' MAX D(~0uL) MARK

// -1 also works instead of ~0
// be sure cast is last operation, lest the number
// be an int and negative
DEFINE 'U','S' MAX D((unsigned short)~0) MARK
DEFINE 'S' MAX D(((unsigned short)~0)>>1) MARK

DEFINE 'U','C' MAX D((unsigned char)~0) MARK
DEFINE 'C' MAX D(((unsigned char)~0)>>1) MARK

DEFINE 'T','W','O','S','_','C','O','M','P','L','E','M','E','N','T',SP (-1 == ((~1) + 1)) 
  ? '1' : '0', MARK

// NOTE: Only two's complement machines are available these days.
// Testing for ONES_COMPLEMENT and SIGNED_MAGNITUDE is therefore not known to work.

DEFINE 'O','N','E','S','_','C','O','M','P','L','E','M','E','N','T',SP (-1 == ~1) 
  ? '1' : '0', MARK

DEFINE 'S','I','G','N','E','D','_','M','A','G','N','I','T','U','D','E', SP 
 (-1 == (1 | ((1 << sizeof(int)) * CHAR_BIT))) ? '1' : '0', MARK
  
0,

};
