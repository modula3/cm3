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
A SWIG interface to PLplot for Modula3.

Unfortunately, most things are untested.
I tried to rename the functions to some descriptive names.
This is not easy if you don't know exactly,
what they do.
If you find some names irritating
don't hesitate to suggest better ones.
Be prepared for changes.
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
%pragma(modula3) setitem="DrawMode,DRAW_";
%pragma(modula3) setitem="Option,PL_OPT_";
%pragma(modula3) setitem="Parse,PL_PARSE_";
%pragma(modula3) enumitem="Buffering,PLESPLFLTBUFFERING_";

%insert(m3wrapintf) %{
  Option = {enabled, arg, nodelete, invisible, disabled, dummy5, dummy6,
            dummy7, func, bool, int, float, string};
  OptionSet = SET OF Option;

  Parse =
    {full, quiet, nodelete, showall, override, noprogram, nodash, skip};
  ParseSet = SET OF Parse;

  Escape = {dummy0, setRgb, allocNcol, setLpb, expose, resize, redraw,
            text, graph, fill, di, flush, eh, getc, swin, plfltbuffering,
            xormod, setCompression, clear, dash, hasText, image, imageops};

  Buffering = {dummy0, enable, disable, query};

  DrawMode = {linex, liney, magColor, baseCont, topCont, surfCont, sides,
              faceted, mesh};
  DrawModeSet = SET OF DrawMode;
%}


%rename("SetContLabelFormat") pl_setcontlabelformat;
%rename("SetContLabelParam") pl_setcontlabelparam;
%rename("Advance") pladv;
%rename("DrawAxes") plaxes;
%rename("PlotBins") plbin;
%rename("StartPage") plbop;
%rename("DrawBox") plbox;
%rename("DrawBox3D") plbox3;
%rename("CalcWorld") plcalc_world;
%rename("Clear") plclear;
%rename("SetFGColorDiscr") plcol0;
%rename("SetFGColorCont") plcol1;
%rename("PlotContour") plcont;
%rename("CopyStateFrom") plcpstrm;
%rename("ExitAll") plend;
%rename("Exit") plend1;
%rename("Environment") plenv;
%rename("StopPage") pleop;
%rename("PlotErrorX") plerrx;
%rename("PlotErrorY") plerry;
%rename("AdvanceFamily") plfamadv;
%rename("FillPolygon") plfill;
%rename("FillPolygon3D") plfill3;
%rename("Flush") plflush;
%rename("SetFont") plfont;
%rename("LoadFont") plfontld;
%rename("GetCharacterHeight") plgchr;
%rename("GetFGColorDiscrRGB") plgcol0;
%rename("GetBGColorDiscrRGB") plgcolbg;
%rename("GetCompression") plgcompression;
%rename("GetDevice") plgdev;
%rename("GetWindowDevice") plgdidev;
%rename("GetOrientation") plgdiori;
%rename("GetWindowPlot") plgdiplt;
%rename("GetFamilyFile") plgfam;
%rename("GetFileName") plgfnam;
%rename("GetRunLevel") plglevel;
%rename("GetOutputDeviceParam") plgpage;
%rename("ShowGraphicScreen") plgra;
%rename("GetBoundaries") plgspa;
%rename("GetStream") plgstrm;
%rename("GetVersion") plgver;
%rename("GetVPBoundDev") plgvpd;
%rename("GetVPBoundWorld") plgvpw;
%rename("GetXLabelParam") plgxax;
%rename("GetYLabelParam") plgyax;
%rename("GetZLabelParam") plgzax;
%rename("PlotHistogram") plhist;
%rename("SetColorHLS") plhls;
%rename("Init") plinit;
%rename("PlotLineSegment") pljoin;
%rename("SetLabels") pllab;
%rename("SetLightPos") pllightsource;
%rename("PlotLines") plline;
%rename("PlotLines3D") plline3;
%rename("SetLineStyle") pllsty;
%rename("PlotMesh") plmesh;
%rename("PlotMeshColored") plmeshc;
%rename("CreateStream") plmkstrm;
%rename("PrintTextVP") plmtex;
%rename("Plot3D") plot3d;
%rename("Plot3DC") plot3dc;
%rename("Surface3D") plsurf3d;
%rename("SetFillPattern") plpat;
%rename("PlotPoints") plpoin;
%rename("PlotPoints3D") plpoin3;
%rename("PlotPolygon3D") plpoly3;
%rename("SetLabelPrecision") plprec;
%rename("SetFillPattern") plpsty;
%rename("PrintTextWorld") plptex;
%rename("Replot") plreplot;
%rename("SetCharacterHeight") plschr;
%rename("SetColorMapDiscr") plscmap0;
%rename("SetColorDiscr") plscmap0n;
%rename("SetColorMapCont") plscmap1;
%rename("SetColorCont") plscmap1l;
%rename("SetColorMapSizeCont") plscmap1n;
%rename("SetColorRGB") plscol0;
%rename("SetBGColor") plscolbg;
%rename("ToggleColor") plscolor;
%rename("SetCompression") plscompression;
%rename("SetDevice") plsdev;
%rename("SetWindowDevice") plsdidev;
%rename("LoadTransformation") plsdimap;
%rename("SetOrientation") plsdiori;
%rename("SetWindowPlot") plsdiplt;
%rename("ZoomWindow") plsdiplz;
%rename("SetEscapeChar") plsesc;
%rename("SetOption") plsetopt;
%rename("SetFamilyFile") plsfam;
%rename("SetFileName") plsfnam;
%rename("ShadeRegions") plshades;
%rename("ShadeRegion") plshade;
%rename("SetMajorTickSize") plsmaj;
%rename("SetMinorTickSize") plsmin;
%rename("SetOrientation") plsori;
%rename("SetOutputDeviceParam") plspage;
%rename("SetPause") plspause;
%rename("SetStream") plsstrm;
%rename("SetSubWindows") plssub;
%rename("SetSymbolHeight") plssym;
%rename("Start") plstar;
%rename("StartDev") plstart;
%rename("AddStripchartPoint") plstripa;
%rename("CreateStripchart") plstripc;
%rename("DeleteStripchart") plstripd;
%rename("SetLineStyle") plstyl;
%rename("SetVPAbsolute") plsvpa;
%rename("SetXLabelParam") plsxax;
%rename("SetYLabelParam") plsyax;
%rename("PlotSymbols") plsym;
%rename("SetZLabelParam") plszax;
%rename("ShowTextScreen") pltext;
%rename("SetVPAspect") plvasp;
%rename("CreateVPAspect") plvpas;
%rename("CreateVP") plvpor;
%rename("SetStandardVP") plvsta;
%rename("Init3DWindow") plw3d;
%rename("SetPenWidth") plwid;
%rename("SetWindow") plwind;
%rename("SetXORMode") plxormod;

/* obsolete functions */
%rename("rgb") plrgb;
%rename("rgb1") plrgb1;
%rename("shade1") plshade1;

/* C / C++ functions */
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


/* Test of new interface style. */
typedef PLINT  PLFLT_ARRAY_SIZE;
typedef PLFLT *PLFLT_ARRAY;
typedef PLFLT *PLFLT_ARRAY_CK;

%typemap("m3intype",numinputs=0) PLINT n %{%}

%typemap("m3intype") PLFLT_ARRAY %{READONLY $1_name: V.TBody%}
%typemap("m3indecl") PLFLT_ARRAY %{n:=NUMBER($1_name);%}
%typemap("m3rawarg") PLFLT_ARRAY %{$1_name[0]%}

%typemap("m3intype") PLFLT_ARRAY_CK %{READONLY $1_name: V.TBody%}
%typemap("m3in")     PLFLT_ARRAY_CK %{IF NUMBER($1_name) # n THEN RAISE NA.Error(Err.bad_size) END;%}
%typemap("m3rawarg") PLFLT_ARRAY_CK %{$1_name[0]%}

void
plline_new(PLFLT_ARRAY_SIZE n, PLFLT_ARRAY x, PLFLT_ARRAY_CK y);


/* swig compatible PLplot API definitions from here on. */
%include plplotcapi.i
