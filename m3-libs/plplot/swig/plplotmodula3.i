/* 
Copyright 2003 Henning Thielemann
This file is part of PLplot.

This file is free software; you can redistribute it and/or modify
it under the terms of the GNU Library General Public License as published by
the Free Software Foundation; version 2 of the License.

This file is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Library General Public License for more details.

You should have received a copy of the GNU Library General Public License
along with the file; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/

/* 
A SWIG interface to PLplot for Modula3. This wrapper does the following:

   1) it strictly provides the C-API with the usual change of not
      requiring lengths for arrays,

   2) it attempts to provide the entire API *excluding* callbacks for
      plcont and plshade(s) (for now).
      
   3) it works both with the single and double-precision versions of the
      PLplot library.

This is known to work with swig-1.3.20 on Linux.

*/
%module PLPlot
//%include typemaps.i

%{
#include "plplotP.h"      
%}

#ifdef PL_DOUBLE
typedef double PLFLT;
#else
typedef float PLFLT;
#endif

typedef int PLINT;

/***************************
	A trick for docstrings
****************************/

%define DOC(func, string) 
%wrapper %{#define _doc_ ## func string %}
%enddef

%pragma(modula3) enumitem="Escape,PLESC_";

%rename("arrows") plarrows;
%rename("sxwin") plsxwin;
%rename("SetContLabelFormat") pl_setcontlabelformat;
%rename("SetContLabelParam") pl_setcontlabelparam;
%rename("adv") pladv;
%rename("axes") plaxes;
%rename("bin") plbin;
%rename("bop") plbop;
%rename("box") plbox;
%rename("box3") plbox3;
%rename("CalcWorld") plcalc_world;
%rename("SetColorDiscr") plcol0;
%rename("SetColorCont") plcol1;
%rename("PlotContour") plcont;
%rename("cpstrm") plcpstrm;
%rename("Exit") plend;
%rename("end1") plend1;
%rename("env") plenv;
%rename("eop") pleop;
%rename("errx") plerrx;
%rename("erry") plerry;
%rename("famadv") plfamadv;
%rename("fill") plfill;
%rename("fill3") plfill3;
%rename("flush") plflush;
%rename("font") plfont;
%rename("fontld") plfontld;
%rename("gchr") plgchr;
%rename("gcol0") plgcol0;
%rename("gcolbg") plgcolbg;
%rename("gcompression") plgcompression;
%rename("gdev") plgdev;
%rename("gdidev") plgdidev;
%rename("gdiori") plgdiori;
%rename("gdiplt") plgdiplt;
%rename("gfam") plgfam;
%rename("gfnam") plgfnam;
%rename("glevel") plglevel;
%rename("gpage") plgpage;
%rename("gra") plgra;
%rename("gspa") plgspa;
%rename("gstrm") plgstrm;
%rename("gver") plgver;
%rename("gvpd") plgvpd;
%rename("gvpw") plgvpw;
%rename("gxax") plgxax;
%rename("gyax") plgyax;
%rename("gzax") plgzax;
%rename("hist") plhist;
%rename("hls") plhls;
%rename("Init") plinit;
%rename("join") pljoin;
%rename("lab") pllab;
%rename("lightsource") pllightsource;
%rename("PlotLines") plline;
%rename("PlotLines3D") plline3;
%rename("lsty") pllsty;
%rename("Mesh") plmesh;
%rename("MeshColored") plmeshc;
%rename("mkstrm") plmkstrm;
%rename("mtex") plmtex;
%rename("Plot3D") plot3d;
%rename("Plot3DC") plot3dc;
%rename("Surface3D") plsurf3d;
%rename("SetFillPattern") plpat;
%rename("PlotPoints") plpoin;
%rename("PlotPoints3D") plpoin3;
%rename("poly3") plpoly3;
%rename("prec") plprec;
%rename("psty") plpsty;
%rename("ptex") plptex;
%rename("replot") plreplot;
%rename("schr") plschr;
%rename("scmap0") plscmap0;
%rename("scmap0n") plscmap0n;
%rename("scmap1") plscmap1;
%rename("scmap1l") plscmap1l;
%rename("scmap1n") plscmap1n;
%rename("scol0") plscol0;
%rename("scolbg") plscolbg;
%rename("scolor") plscolor;
%rename("scompression") plscompression;
%rename("sdev") plsdev;
%rename("sdidev") plsdidev;
%rename("sdimap") plsdimap;
%rename("sdiori") plsdiori;
%rename("sdiplt") plsdiplt;
%rename("sdiplz") plsdiplz;
%rename("sesc") plsesc;
%rename("setopt") plsetopt;
%rename("sfam") plsfam;
%rename("sfnam") plsfnam;
%rename("shades") plshades;
%rename("shade") plshade;
%rename("smaj") plsmaj;
%rename("smin") plsmin;
%rename("sori") plsori;
%rename("spage") plspage;
%rename("spause") plspause;
%rename("sstrm") plsstrm;
%rename("ssub") plssub;
%rename("ssym") plssym;
%rename("star") plstar;
%rename("start") plstart;
%rename("stripa") plstripa;
%rename("stripc") plstripc;
%rename("stripd") plstripd;
%rename("styl") plstyl;
%rename("svpa") plsvpa;
%rename("sxax") plsxax;
%rename("syax") plsyax;
%rename("sym") plsym;
%rename("szax") plszax;
%rename("text") pltext;
%rename("vasp") plvasp;
%rename("vpas") plvpas;
%rename("vpor") plvpor;
%rename("vsta") plvsta;
%rename("w3d") plw3d;
%rename("wid") plwid;
%rename("wind") plwind;
%rename("xormod") plxormod;
%rename("rgb") plrgb;
%rename("rgb1") plrgb1;
%rename("shade1") plshade1;
%rename("fcont") plfcont;
%rename("map") plmap;
%rename("meridians") plmeridians;
%rename("fshade") plfshade;
%rename("did2pc") pldid2pc;
%rename("dip2dc") pldip2dc;
%rename("image") plimage;
%rename("gFileDevs") plgFileDevs;
%rename("gDevs") plgDevs;
%rename("sKeyEH") plsKeyEH;
%rename("sButtonEH") plsButtonEH;
%rename("sError") plsError;
%rename("sexit") plsexit;
%rename("tr2p") pltr2p;
%rename("tr0f") pltr0f;
%rename("tr2f") pltr2f;
%rename("f2eval2") plf2eval2;
%rename("f2eval") plf2eval;
%rename("f2evalr") plf2evalr;
%rename("ClearOpts") plClearOpts;
%rename("ResetOpts") plResetOpts;
%rename("MergeOpts") plMergeOpts;
%rename("SetUsage") plSetUsage;
%rename("SetOpt") plSetOpt;
%rename("ParseOpts") plParseOpts;
%rename("OptUsage") plOptUsage;
%rename("gfile") plgfile;
%rename("sfile") plsfile;
%rename("gesc") plgesc;
%rename("Cmd") pl_cmd;
%rename("FindName") plFindName;
%rename("FindCommand") plFindCommand;
%rename("GetName") plGetName;
%rename("GetInt") plGetInt;
%rename("GetFlt") plGetFlt;
%rename("Alloc2dGrid") plAlloc2dGrid;
%rename("Free2dGrid") plFree2dGrid;
%rename("MinMax2dGrid") plMinMax2dGrid;
%rename("HLS_RGB") plHLS_RGB;
%rename("RGB_HLS") plRGB_HLS;
%rename("GetCursor") plGetCursor;
%rename("TranslateCursor") plTranslateCursor;

%typemap("m3intype",numinputs=0) PLINT n %{%}
%typemap("m3indecl")             PLINT n %{n:=NUMBER(data);%}

%typemap("m3intype") PLFLT *Array %{READONLY data: V.TBody%}
%typemap("m3rawarg") PLFLT *Array %{data[0]%}

%typemap("m3intype") PLFLT *ArrayCk %{READONLY data$argnum: V.TBody%}
%typemap("m3in")     PLFLT *ArrayCk %{IF NUMBER(data$argnum) # n THEN RAISE NA.Error(Err.bad_size) END;%}
%typemap("m3rawarg") PLFLT *ArrayCk %{data$argnum[0]%}


%typemap("m3intype") PLFLT *ArrayX %{READONLY x: V.TBody%}
%typemap("m3rawarg") PLFLT *ArrayX %{x[0]%}

%typemap("m3intype") PLFLT *ArrayY %{READONLY y: V.TBody%}
%typemap("m3rawarg") PLFLT *ArrayY %{y[0]%}


%typemap("m3intype") PLINT *Array %{READONLY data: ARRAY OF INTEGER%}
%typemap("m3rawarg") PLINT *Array %{data[0]%}

%typemap("m3intype") PLINT *ArrayCk %{READONLY data$argnum: ARRAY OF INTEGER%}
%typemap("m3in")     PLINT *ArrayCk %{IF NUMBER(data$argnum) # n THEN RAISE NA.Error(Err.bad_size) END;%}
%typemap("m3rawarg") PLINT *ArrayCk %{data$argnum[0]%}

%typemap("m3intype") PLINT *ArrayCkMinus1 %{READONLY data$argnum: ARRAY OF INTEGER%}
%typemap("m3in")     PLINT *ArrayCkMinus1 %{IF NUMBER(data$argnum) # n-1 THEN RAISE NA.Error(Err.bad_size) END;%}
%typemap("m3rawarg") PLINT *ArrayCkMinus1 %{data$argnum[0]%}


%typemap("m3intype",numinputs=0) PLINT nx %{%}
%typemap("m3indecl")             PLINT nx %{nx:=NUMBER(matrix);%}

%typemap("m3intype",numinputs=0) PLINT ny %{%}
%typemap("m3indecl")             PLINT ny %{ny:=NUMBER(matrix[0]);%}

%typemap("m3intype") PLFLT **Matrix %{READONLY matrix: M.TBody%}
%typemap("m3indecl") PLFLT **Matrix %{tmpmat:=NEW(REF ARRAY OF ADDRESS,NUMBER(matrix));%}
%typemap("m3in")     PLFLT **Matrix
%{FOR i:=0 TO LAST(matrix) DO tmpmat[i] := ADR(matrix[i,0]) END;%}
%typemap("m3rawarg") PLFLT **Matrix %{tmpmat[0]%}
%typemap("rawintype") PLFLT **Matrix %{VAR matrix: (*ARRAY OF*) ADDRESS (*REF ARRAY OF R.T*)%}

%typemap("m3intype") PLFLT **MatrixCk %{READONLY matrix: M.TBody%}
%typemap("m3indecl") PLFLT **MatrixCk %{tmpmat:=NEW(REF ARRAY OF ADDRESS,NUMBER(matrix));%}
%typemap("m3in")     PLFLT **MatrixCk
%{IF nx#NUMBER(x) THEN RAISE NA.Error(Err.bad_size) END;
IF ny#NUMBER(y) THEN RAISE NA.Error(Err.bad_size) END;
FOR i:=0 TO LAST(matrix) DO tmpmat[i] := ADR(matrix[i,0]) END;%}
%typemap("m3rawarg") PLFLT **MatrixCk %{tmpmat[0]%}
%typemap("rawintype") PLFLT **MatrixCk %{VAR matrix: (*ARRAY OF*) ADDRESS (*REF ARRAY OF R.T*)%}



%rename("plotter") pltr;
%rename("objectData") OBJECT_DATA;

%typemap(m3intype) (pltr_func pltr, PLPointer OBJECT_DATA)
%{plotter: PROCEDURE (data: REF CallbackM3Data); objectData: REFANY%}
%typemap(m3rawarg) (pltr_func pltr, PLPointer OBJECT_DATA)
%{CallbackM3, NEW(REF CallbackM3Data,callback:=plotter,callbackData:=objectData)%}

%typemap(m3intype) defined_func df
%{$1_name: PROCEDURE (x: R.T): R.T%}
%typemap(m3rawarg) defined_func df
%{NIL (*not yet supported*)%}

%typemap(m3intype) fill_func ff
%{$1_name: PROCEDURE (x: R.T): R.T%}
%typemap(m3rawarg) fill_func ff
%{NIL (*not yet supported*)%}


%typemap(m3indecl)  (char *) %{$1 := M3toC.SharedTtoS($input);%}
%typemap(m3freearg) (char *) %{M3toC.FreeSharedS($input,$1);%}
%typemap(m3rawarg)  (char *) %{$1%}

%typemap(rawintype) char *legline[4]
%{READONLY $1_name: ARRAY [0..3] OF C.char_star%}
%typemap(m3intype) char *legline[4]
%{READONLY $1_name: ARRAY [0..3] OF TEXT%}
%typemap(m3indecl) char *legline[4]
%{tmp$1_name: ARRAY [0..3] OF C.char_star;%}
%typemap(m3in) char *legline[4]
%{FOR i:=FIRST($1_name) TO FIRST($1_name) DO
tmp$1_name[i]:=M3toC.SharedTtoS($1_name[i]);
END;%}
%typemap(m3rawarg) char *legline[4]
%{tmp$1_name%}
%typemap(m3freearg) char *legline[4]
%{FOR i:=FIRST($1_name) TO FIRST($1_name) DO
M3toC.FreeSharedS($1_name[i],tmp$1_name[i]);
END;%}


%typemap(m3intype) PLINT *p_argc %{VAR $1_name:CARDINAL%}
%typemap(m3intype) char **argv   %{VAR $1_name:ARRAY OF C.char_star%}


%feature("m3:multiretval") plcalc_world;
%feature("m3:multiretval") plgchr;
%feature("m3:multiretval") plgcol0;
%feature("m3:multiretval") plgcolbg;
%feature("m3:multiretval") plgdidev;
%feature("m3:multiretval") plgdiplt;
%feature("m3:multiretval") plgfam;
%feature("m3:multiretval") plgpage;
%feature("m3:multiretval") plgspa;
%feature("m3:multiretval") plgvpd;
%feature("m3:multiretval") plgvpw;
%feature("m3:multiretval") plgxax;
%feature("m3:multiretval") plgyax;
%feature("m3:multiretval") plgzax;
%feature("m3:multiretval") plHLS_RGB;
%feature("m3:multiretval") plRGB_HLS;

%typemap("m3intype",numinputs=0) PLFLT *OUTPUT ""
%typemap("m3argouttype") PLFLT *OUTPUT "R.T"

%typemap("m3intype",numinputs=0) PLINT *OUTPUT ""
%typemap("m3argouttype") PLINT *OUTPUT "INTEGER"

/* swig compatible PLplot API definitions from here on. */
%include plplotcapi.i
