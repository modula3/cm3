
#include "../macroizations"

#define _LN(link, text) \
<A HREF = "link">text</A>

#define _A(name) \
<A NAME = "name"></A>



#define _H(title) <H3>title</H3>
#define _TT(text) <TT>text</TT>
#define _IT(text) <I>text</I>
#define _BF(text) <B>text</B>
#define _C(text) <P><CENTER>text</CENTER><P>


#define _BLN(link,text) _LN(link.html,[text])

#define _BANNER \
_BLN(index,parserlib page) \
_BLN(ktok,ktok) \
_BLN(klex,klex) \
_BLN(kyacc,kyacc) \
_BLN(kext,kext) \
_BLN(m3build,m3build) \

#define PL_END <HR> _BANNER <P>

#define _HTML_BEGIN(title) \
<HTML> \
<HEAD> \
<!-- this file has been processed by the C preprocessor --> \
<TITLE>title</TITLE> \
</HEAD> \
<BODY BGCOLOR="#ffffff" VLINK="#006633"> \
<H2>title</H2> \
<P> \

#define PL_BEGIN(title) _HTML_BEGIN(parserlib: title)

#define HTML_END </BODY></HTML> 


// tables

#define _TR2(forma,formb) <TR><TD><TT>forma,formb</TT></TD><TD>
#define _TR(form) <TR><TD>_TT(form)</TD><TD>
#define _TRP(form) <TR><TD><PRE>form</PRE></TD><TD>
#define TR_ </TD></TR>
#define _TRE </TD></TR>

#define _Q(x) _TT('x')

// tok

#define _TS _LN(ktok.html#spec, token specification)
#define _TI _LN(ktok.html#intf, token interface)


// ext

#define _EXT _LN(kext.html,ext)


// lex and yacc

#define _LS _LN(klex.html#spec, lexer specification)
#define _LI _LN(klex.html#intf, lexer interface)
#define _YS _LN(kyacc.html#spec, parser specification)
#define _YI _LN(kyacc.html#intf, parser interface)

#define LY_BEGIN(lex, klex, lexer, MyLangLex, Lexer, l) \
PL_BEGIN(klex: defining the lexer) \
A _IT(lexer interface) and its implementation are generated \
_LN(m3build.html,automatically) by m3build by running the command \
_C(_TT(lex MyLang.l  [ -t MyLang.t [-ti3 MyLangTok.i3] ]  [ -o MyLangLex.i3 ])) \
where _TT(MyLang.t) is a _TS, _TT(MyLangTok.i3) is a _TI, _TT(MyLang.l) is a \
_IT(lexer specification), and _TT(MyLangLex.i3) is the generated lexer \
interface. \
_A(spec)_H(lexer specification) \


