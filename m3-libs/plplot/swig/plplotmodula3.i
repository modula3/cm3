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

Since the function names in libplplot.a have a 'c_' prepended
you must further process PLPlotRaw.i3 with
  perl -p -e "s/EXTERNAL pl/EXTERNAL c_pl/"

*/

%module PLPlot
//%include typemaps.i
#define PL_DOUBLE

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
%insert(m3wrapintf) %{(* func: string *)%}
%enddef

/*
%pragma(modula3) enumitem="Escape,PLESC_";
%pragma(modula3) setitem="DrawMode,DRAW_";
%pragma(modula3) setitem="Option,PL_OPT_";
%pragma(modula3) setitem="Parse,PL_PARSE_";
%pragma(modula3) enumitem="Buffering,PLESPLFLTBUFFERING_";
*/
%pragma(modula3) unsafe="true";

%insert(m3rawintf) %{
TYPE
PlotterFunc = PROCEDURE();
PLINT = C.int;
PLFLT = C.double;
%}

%insert(m3wrapintf) %{
(* * * *
Precaution:
 This conversion from the C headers is not well tested
and may contain bugs, irritating function names or
improper types.
 We should use enumerations, sets, subranges
whereever possible to increase safety for parameter passing.
We should use exceptions to indicate errors.
* * * *)

IMPORT NADefinitions AS NA;
IMPORT LongRealBasic  AS R;
IMPORT LongRealVectorFast AS V;
IMPORT LongRealMatrixFast AS M;

TYPE
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

%insert(m3wrapimpl) %{
FROM NADefinitions IMPORT Err;
IMPORT NADefinitions AS NA;
IMPORT LongRealBasic  AS R;
IMPORT LongRealVectorFast AS V;
IMPORT LongRealMatrixFast AS M;
IMPORT M3toC;
IMPORT Ctypes AS C;
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
%rename("SetEnvironment") plenv;
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
%rename("SetFillStyle") plpsty;
%rename("PrintTextWorld") plptex;
%rename("Replot") plreplot;
%rename("SetCharacterHeight") plschr;
%rename("SetColorMapDiscr") plscmap0;
%rename("SetColorMapDiscrSize") plscmap0n;
%rename("SetColorMapCont") plscmap1;
%rename("SetColorCont") plscmap1l;
%rename("SetColorMapContSize") plscmap1n;
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
%rename("SetGlobalOrientation") plsori;
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
%rename("SetNewLineStyle") plstyl;
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
%rename("GetFloat") plGetFlt;
%rename("Alloc2DGrid") plAlloc2dGrid;
%rename("Free2DGrid") plFree2dGrid;
%rename("MinMax2DGrid") plMinMax2dGrid;
%rename("HLS_RGB") plHLS_RGB;
%rename("RGB_HLS") plRGB_HLS;
%rename("GetCursor") plGetCursor;
%rename("TranslateCursor") plTranslateCursor;


%typemap("rawintype") PLINT  * %{C.int%}
%typemap("rawintype") PLFLT  * %{C.double%}
%typemap("rawintype") double * %{C.double%}

%typemap("m3intype",numinputs=0) PLArraySize n %{%}

%typemap("rawinmode") PLFLTArray %{READONLY%}
%typemap("m3inmode")  PLFLTArray %{READONLY%}
%typemap("rawintype") PLFLTArray %{(*ARRAY OF*) C.double%}
%typemap("m3intype")  PLFLTArray %{V.TBody%}
%typemap("m3rawarg")  PLFLTArray %{$1_name[0]%}

%typemap("m3indecl")  PLFLTArrayFst %{n:=NUMBER($1_name);%}
%typemap("m3indecl")  PLFLTArrayX   %{nx:=NUMBER($1_name);%}
%typemap("m3indecl")  PLFLTArrayY   %{ny:=NUMBER($1_name);%}
//%typemap("m3incheck") PLFLTArrayX   %{IF NUMBER($1_name) # nx THEN RAISE NA.Error(Err.bad_size) END;%}
//%typemap("m3incheck") PLFLTArrayY   %{IF NUMBER($1_name) # ny THEN RAISE NA.Error(Err.bad_size) END;%}
%typemap("m3incheck") PLFLTArrayCk  %{IF NUMBER($1_name) # n THEN RAISE NA.Error(Err.bad_size) END;%}
%typemap("m3incheck:throws") PLFLTArrayCk %{NA.Error%}


%typemap("rawinmode") PLINTArray %{READONLY%}
%typemap("m3inmode")  PLINTArray %{READONLY%}
%typemap("rawintype") PLINTArray %{C.int%}
%typemap("m3intype")  PLINTArray %{ARRAY OF INTEGER%}
%typemap("m3rawarg")  PLINTArray %{$1_name[0]%}

%typemap("m3indecl")  PLINTArrayFst %{n:=NUMBER($1_name);%}
%typemap("m3incheck") PLINTArrayCk  %{IF NUMBER($1_name) # n THEN RAISE NA.Error(Err.bad_size) END;%}
%typemap("m3incheck") PLINTArrayCkInterim %{IF NUMBER($1_name) # n-1 THEN RAISE NA.Error(Err.bad_size) END;%}
%typemap("m3incheck:throws") PLINTArrayCk        %{NA.Error%}
%typemap("m3incheck:throws") PLINTArrayCkInterim %{NA.Error%}


%typemap("rawinmode") PLFLTMatrix %{READONLY%}
%typemap("m3inmode")  PLFLTMatrix %{READONLY%}
%typemap("rawintype") PLFLTMatrix %{(*ARRAY OF*) ADDRESS (*REF ARRAY OF R.T*)%}
%typemap("m3intype")  PLFLTMatrix %{M.TBody%}
%typemap("m3rawarg")  PLFLTMatrix %{$1[0]%}

%typemap("m3intype",numinputs=0) PLArraySize nx %{%}
%typemap("m3intype",numinputs=0) PLArraySize ny %{%}

%typemap("m3indecl")  PLFLTMatrixFst %{$1:REF ARRAY OF ADDRESS;
nx:=NUMBER($1_name);
ny:=NUMBER($1_name[0]);%}
%typemap("m3in")      PLFLTMatrixFst
%{$1:=NEW(REF ARRAY OF ADDRESS,NUMBER($1_name));
FOR i:=0 TO LAST($1_name) DO $1[i] := ADR($1_name[i,0]) END;%}

%typemap("m3indecl")  PLFLTMatrixCk %{$1:REF ARRAY OF ADDRESS;%}
%typemap("m3in")      PLFLTMatrixCk
%{$1:=NEW(REF ARRAY OF ADDRESS,NUMBER($1_name));
FOR i:=0 TO LAST($1_name) DO $1[i] := ADR($1_name[i,0]) END;%}
%typemap("m3incheck") PLFLTMatrixCk
%{IF NUMBER($1_name) # nx THEN RAISE NA.Error(Err.bad_size) END;
IF NUMBER($1_name[0]) # ny THEN RAISE NA.Error(Err.bad_size) END;%}
%typemap("m3incheck:throws") PLFLTMatrixCk %{NA.Error%}



//%rename("plotter") pltr;
//%rename("objectData") OBJECT_DATA;

%insert(m3wrapintf) %{
TYPE
  CallbackM3Proc = PROCEDURE (data: REFANY);
%}

%insert(m3wrapimpl) %{
TYPE
  CallbackM3Data =
    RECORD
      callback    : CallbackM3Proc;
      callbackData: REFANY;
    END;

PROCEDURE CallbackM3()=BEGIN END CallbackM3;
%}

%typemap(rawintype) pltr_func %{PlotterFunc%}
%typemap(rawintype) PLPointer %{REFANY%}
%typemap(rawinmode) PLPointer %{%}

%typemap(m3intype)  pltr_func %{CallbackM3Proc%}
%typemap(m3intype)  PLPointer %{REFANY%}
%typemap(m3inmode)  PLPointer %{%}

%typemap(m3rawarg) (pltr_func pltr, PLPointer OBJECT_DATA)
%{CallbackM3, NEW(REF CallbackM3Data,callback:=$1_name,callbackData:=$2_name)%}

%typemap(rawintype) defined_func %{PlotterFunc%}
%typemap(m3intype,numinputs=0)  defined_func %{PROCEDURE (x: R.T): R.T%}
%typemap(m3rawarg)  defined_func %{NIL (*not yet supported*)%}

%typemap(rawintype) fill_func %{PlotterFunc%}
%typemap(m3intype,numinputs=0)  fill_func %{PROCEDURE ()%}
%typemap(m3rawarg)  fill_func %{NIL (*not yet supported*)%}


%typemap(rawintype) char *legline[4] %{READONLY%}
%typemap(m3intype)  char *legline[4] %{READONLY%}
%typemap(rawintype) char *legline[4] %{ARRAY [0..3] OF C.char_star%}
%typemap(m3intype)  char *legline[4] %{ARRAY [0..3] OF TEXT%}
%typemap(m3indecl)  char *legline[4] %{$1: ARRAY [0..3] OF C.char_star;%}
%typemap(m3in)      char *legline[4]
%{FOR i:=FIRST($1_name) TO LAST($1_name) DO
$1[i]:=M3toC.SharedTtoS($1_name[i]);
END;%}
%typemap(m3rawarg)  char *legline[4]
%{$1%}
%typemap(m3freearg) char *legline[4]
%{FOR i:=FIRST($1_name) TO LAST($1_name) DO
M3toC.FreeSharedS($1_name[i],$1[i]);
END;%}

%typemap(m3indefault) PLINT oldwin "0"

%insert(m3wrapintf) %{TYPE
AxesScaling = {none, independent, equal, square};
Tile = {box, ticks, axes, gridMajor, gridMinor, xTicksLog, yTicksLog};
TileSet = SET OF Tile;
%}
%typemap(m3intype)    PLINT just %{AxesScaling%}
%typemap(m3indefault) PLINT just %{AxesScaling.independent%}
%typemap(m3rawarg)    PLINT just %{ORD($1_name)-1%}
%typemap(m3intype)    PLINT axis %{TileSet%}
%typemap(m3indefault) PLINT axis %{TileSet{Tile.box,Tile.ticks}%}
%typemap(m3indecl)    PLINT axis %{$1: C.int;%}
%typemap(m3rawarg)    PLINT axis %{$1%}
%typemap(m3in) PLINT axis
%{IF $1_name = TileSet{} THEN
$1:=-2;
ELSIF $1_name = TileSet{Tile.box} THEN
$1:=-1;
ELSE
$1:=0;
IF Tile.xTicksLog IN $1_name THEN
  INC($1,10);
END;
IF Tile.yTicksLog IN $1_name THEN
  INC($1,20);
END;
$1_name := $1_name - TileSet{Tile.xTicksLog,Tile.yTicksLog};
IF    $1_name = TileSet{Tile.box,Tile.ticks} THEN INC($1,0);
ELSIF $1_name = TileSet{Tile.box,Tile.ticks,Tile.axes} THEN INC($1,1);
ELSIF $1_name = TileSet{Tile.box,Tile.ticks,Tile.axes,Tile.gridMajor} THEN INC($1,2);
ELSIF $1_name = TileSet{Tile.box,Tile.ticks,Tile.axes,Tile.gridMajor,Tile.gridMinor} THEN INC($1,3);
ELSE
<*ASSERT FALSE*> (*combination not supported by PLPlot :-( *)
END;
END;%}

%typemap(m3intype)  PLINT mode    %{BOOLEAN%}
%typemap(m3rawarg)  PLINT mode    %{ORD($1_name)%}

%typemap(m3argout)     PLINTOutput status %{$1#0%}
%typemap(m3argouttype) PLINTOutput status %{BOOLEAN%}

%typemap(m3inmode)  PLINT *p_argc %{VAR%}
%typemap(m3intype)  PLINT *p_argc %{CARDINAL%}

%typemap(rawinmode) char **argv   %{READONLY%}
%typemap(m3inmode)  char **argv   %{READONLY%}
%typemap(rawintype) char **argv   %{(*ARRAY OF*) C.char_star%}
%typemap(m3intype)  char **argv   %{ARRAY OF TEXT%}

%typemap(m3intype,numinputs=0) PLFLTOutput ""
%typemap(m3argouttype) PLFLTOutput "R.T"

%typemap(m3intype,numinputs=0) PLINTOutput ""
%typemap(m3indecl) PLINTOutput %{$1: C.int;%}
%typemap(m3rawarg) PLINTOutput %{$1%}
%typemap(m3argout) PLINTOutput %{$1%}
%typemap(m3argouttype) PLINTOutput %{INTEGER%}

%typemap(m3rawarg) char %{ORD($1_name)%}




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


/* swig compatible PLplot API definitions from here on. */
%include plplotcapi.i
