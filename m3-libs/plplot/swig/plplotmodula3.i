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

%pragma(modula3) unsafe="true";
%pragma(modula3) library="m3plplot";

%insert(m3makefile) %{% compiled / works with with CM3 5.2.6 2003-06-27
%import_lib("plplotd","/usr/lib")
import_lib("plplotd","/home/zetem/daten/pakete/lib" & SL & $SYS)%}

%insert(m3rawintf) %{
TYPE
PLINT = C.int;
PLFLT = C.double;
PLPointer = C.void_star;

DefinedFunc = PROCEDURE (x,y: PLFLT;): PLINT;
FillFunc = PROCEDURE (n: PLINT; READONLY x, y: (*ARRAY OF*) PLFLT; );
PlotterFunc = PROCEDURE (x,y: PLFLT; VAR (*OUT*) tx,ty: PLFLT; data:PLPointer;);
%}

%insert(m3wrapintf) %{
(* * * *
Precaution:

 This conversion from the C headers is not well tested
and may contain bugs, irritating function names or
improper types.
 We should use enumerations, sets, subranges
whereever possible to increase safety for parameter passing.
We should use EXCEPTIONs to indicate non-predictable errors.
We should use ASSERTs to indicate obviously invalid parameter values,
such as arrays of mismatching sizes.
* * * *)

TYPE
Float = LONGREAL;
FloatVector = ARRAY OF Float;
FloatMatrix = ARRAY OF ARRAY OF Float;

DefinedFunc = PROCEDURE (x,y: Float;): BOOLEAN;
FillFunc = PROCEDURE (READONLY x, y: ARRAY OF Float; );
Point = RECORD x,y: Float; END;
PlotterFunc = PROCEDURE (x,y: Float; data:REFANY;) : Point;
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
%rename("PlotShades") plshades;
%rename("PlotShade") plshade;
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
%rename("PlotFShade") plfshade;
%rename("did2pc") pldid2pc;
%rename("dip2dc") pldip2dc;
%rename("image") plimage;
%rename("GetFileDevs") plgFileDevs;
%rename("GetDevs") plgDevs;
%rename("SetKeyEH") plsKeyEH;
%rename("SetButtonEH") plsButtonEH;
%rename("SetError") plsError;
%rename("SetExit") plsexit;
%rename("Plotter0") pltr0;
%rename("Plotter1") pltr1;
%rename("Plotter2") pltr2;
%rename("Plotter2P") pltr2p;
%rename("F2Eval2") plf2eval2;
%rename("F2Eval") plf2eval;
%rename("F2EvalR") plf2evalr;
%rename("ClearOpts") plClearOpts;
%rename("ResetOpts") plResetOpts;
%rename("MergeOpts") plMergeOpts;
%rename("SetUsage") plSetUsage;
%rename("SetOpt") plSetOpt;
%rename("ParseOpts") plParseOpts;
%rename("OptUsage") plOptUsage;
%rename("GetFile") plgfile;
%rename("SetFile") plsfile;
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

/* Ignore FORTRAN routines */
%ignore pltr0f;
%ignore pltr2f;

%pragma(modula3) enumitem="prefix=PLESPLFLTBUFFERING_;int;srcstyle=underscore;Buffering";
%pragma(modula3) enumitem="prefix=PLESC_;set;srcstyle=underscore;Escape";
%pragma(modula3) enumitem="prefix=PLSWIN_;int;srcstyle=underscore;Window";
%pragma(modula3) constint="prefix=PL_MAX;int;srcstyle=underscore,prefix=max;CARDINAL";
%ignore PL_NOTSET;

%pragma(modula3) enumitem="prefix=DRAW_;set;srcstyle=underscore;DrawMode";
%pragma(modula3) makesetofenum="DrawMode";
%pragma(modula3) constset="prefix=DRAW_;set;srcstyle=underscore,prefix=draw;DrawModeSet,DrawMode";

%pragma(modula3) enumitem="prefix=PL_OPT_;set;srcstyle=underscore;Option";
%pragma(modula3) makesetofenum="Option";
%pragma(modula3) constset="prefix=PL_OPT_;set;srcstyle=underscore,prefix=opt;OptionSet,Option";

%pragma(modula3) enumitem="prefix=PL_PARSE_;set;srcstyle=underscore;Parse";
%pragma(modula3) makesetofenum="Parse";
%pragma(modula3) constset="prefix=PL_PARSE_;set;srcstyle=underscore,prefix=parse;ParseSet,Parse";


%typemap("m3rawintype") PLINT  * %{C.int%}
%typemap("m3rawintype") PLFLT  * %{C.double%}
%typemap("m3rawintype") double * %{C.double%}

%typemap("m3wrapintype") PLBOOL %{BOOLEAN%}
%typemap("m3wrapargraw") PLBOOL %{ORD($1_name)%}

%typemap("m3wrapargraw") char %{ORD($1_name)%}

%typemap("m3wrapintype",numinputs=0) PLArraySize n %{%}

%typemap("m3rawinmode")   PLFLTArray %{READONLY%}
%typemap("m3wrapinmode")  PLFLTArray %{READONLY%}
%typemap("m3rawintype")   PLFLTArray %{(*ARRAY OF*) C.double%}
%typemap("m3wrapintype")  PLFLTArray %{FloatVector%}
%typemap("m3wrapargraw")  PLFLTArray %{$1_name[0]%}

%typemap("m3wrapargvar")   PLFLTArrayFst, PLINTArrayFst,
                           PLFLTArraySzd, PLINTArraySzd %{n:=NUMBER($1_name);%}
%typemap("m3wrapargconst") PLFLTArrayFst, PLINTArrayFst %{nName="$1_name";%}
%typemap("m3wrapincheck")  PLFLTArrayCk,  PLINTArrayCk
%{<* ASSERT NUMBER($1_name) = n,
"Array sizes of $1_name (" & Fmt.Int(NUMBER($1_name)) &
") and " & nName & " (" & Fmt.Int(n) & ") mismatch." *> %}
%typemap("m3wrapincheck")  PLINTArrayCkInterim
%{<* ASSERT NUMBER($1_name) = n-1,
"Array size of $1_name (" & Fmt.Int(NUMBER($1_name)) &
" must be one more than that of " & nName & " (" & Fmt.Int(n) & ")." *> %}
%typemap("m3wrapincheck:import")  PLFLTArrayCk,  PLINTArrayCk, PLINTArrayCkInterim  %{Fmt%}


%typemap("m3rawinmode")   PLINTArray %{READONLY%}
%typemap("m3wrapinmode")  PLINTArray %{READONLY%}
%typemap("m3rawintype")   PLINTArray %{C.int%}
%typemap("m3wrapintype")  PLINTArray %{ARRAY OF INTEGER%}
%typemap("m3wrapargraw")  PLINTArray %{$1_name[0]%}


%typemap("m3rawinmode")   PLFLTMatrix %{READONLY%}
%typemap("m3wrapinmode")  PLFLTMatrix %{READONLY%}
%typemap("m3rawintype")   PLFLTMatrix %{(*ARRAY OF*) ADDRESS (*REF ARRAY OF Float*)%}
%typemap("m3wrapintype")  PLFLTMatrix %{FloatMatrix%}
%typemap("m3wrapargraw")  PLFLTMatrix %{$1[0]%}

%typemap("m3wrapargvar")  PLFLTMatrixFst %{$1:REF ARRAY OF ADDRESS;
nx:=NUMBER($1_name);
ny:=NUMBER($1_name[0]);%}
%typemap("m3wrapinconv")  PLFLTMatrixFst
%{$1:=NEW(REF ARRAY OF ADDRESS,NUMBER($1_name));
FOR i:=0 TO LAST($1_name) DO $1[i] := ADR($1_name[i,0]) END;%}

%typemap("m3wrapintype",numinputs=0) PLArraySize nx %{%}
%typemap("m3wrapintype",numinputs=0) PLArraySize ny %{%}

%typemap("m3wrapargvar")   PLFLTArrayX   %{nx:=NUMBER($1_name);%}
%typemap("m3wrapargvar")   PLFLTArrayY   %{ny:=NUMBER($1_name);%}
%typemap("m3wrapargconst") PLFLTArrayX   %{nxName="$1_name";%}
%typemap("m3wrapargconst") PLFLTArrayY   %{nyName="$1_name";%}

%typemap("m3wrapargvar")  PLFLTMatrixCk %{$1:REF ARRAY OF ADDRESS;%}
%typemap("m3wrapinconv")  PLFLTMatrixCk
%{$1:=NEW(REF ARRAY OF ADDRESS,NUMBER($1_name));
FOR i:=0 TO LAST($1_name) DO $1[i] := ADR($1_name[i,0]) END;%}
%typemap("m3wrapincheck") PLFLTMatrixCk
%{<* ASSERT NUMBER($1_name) = nx,
"The x size of $1_name (" & Fmt.Int(NUMBER($1_name)) &
") doesn't match the size of " & nxName & " (" & Fmt.Int(nx) & ")." *>
<* ASSERT NUMBER($1_name[0]) = ny,
"The y size of $1_name (" & Fmt.Int(NUMBER($1_name)) &
") doesn't match the size of " & nyName & " (" & Fmt.Int(ny) & ")." *> %}
%typemap("m3wrapincheck:import")  PLFLTMatrixCk  %{Fmt%}



//%rename("plotter") pltr;
//%rename("objectData") OBJECT_DATA;

%insert(m3wrapimpl) %{
TYPE
  PlotterData =
    RECORD
      callback    : PlotterFunc;
      callbackData: REFANY;
    END;

(* The <*CALLBACK*> pragma may be necessary for use under Windows *)
PROCEDURE PlotterCallback(
 x,y: PLPlotRaw.PLFLT;
 VAR (*OUT*) tx,ty: PLPlotRaw.PLFLT;
 data:PLPlotRaw.PLPointer;)=
BEGIN
WITH
 d = LOOPHOLE(data,UNTRACED REF PlotterData)^,
 t=d.callback(x,y,d.callbackData)
  DO
 tx:=t.x;
 ty:=t.y;
END;
END PlotterCallback;
%}

%typemap(m3rawintype) pltr_func %{PlotterFunc%}
%typemap(m3rawintype) PLPointer %{REFANY%}
%typemap(m3rawinmode) PLPointer %{%}

%typemap(m3wrapintype)  pltr_func %{PlotterFunc%}
%typemap(m3wrapintype)  PLPointer %{REFANY%}
%typemap(m3wrapinmode)  PLPointer %{%}

%typemap(m3wrapoutname) PLFLTOutput tx %{x%}
%typemap(m3wrapoutname) PLFLTOutput ty %{y%}

%typemap(m3wrapargraw) (pltr_func pltr, PLPointer OBJECT_DATA)
%{PlotterCallback, NEW(REF PlotterData,callback:=plotter,callbackData:=plotterData)%}

%typemap(m3wrapinname) pltr_func %{plotter%}
%typemap(m3wrapinname) PLPointer OBJECT_DATA %{plotterData%}

%typemap(m3rawintype)   defined_func %{DefinedFunc%}
%typemap(m3wrapintype)  defined_func %{PLPlotRaw.DefinedFunc%}
%typemap("m3wrapintype:import")  defined_func %{PLPlotRaw%}

%typemap(m3rawintype)   fill_func %{FillFunc%}
%typemap(m3wrapintype)  fill_func %{PLPlotRaw.FillFunc%}


%typemap(m3rawintype)   char *legline[4] %{READONLY%}
%typemap(m3wrapintype)  char *legline[4] %{READONLY%}
%typemap(m3rawintype)   char *legline[4] %{ARRAY [0..3] OF C.char_star%}
%typemap(m3wrapintype)  char *legline[4] %{ARRAY [0..3] OF TEXT%}
%typemap(m3wrapargvar)  char *legline[4] %{$1: ARRAY [0..3] OF C.char_star;%}
%typemap("m3wrapargvar:import") char *legline[4] "Ctypes AS C"
%typemap(m3wrapinconv)  char *legline[4]
%{FOR i:=FIRST($1_name) TO LAST($1_name) DO
$1[i]:=M3toC.SharedTtoS($1_name[i]);
END;%}
%typemap(m3wrapargraw)  char *legline[4]
%{$1%}
%typemap(m3wrapfreearg) char *legline[4]
%{FOR i:=FIRST($1_name) TO LAST($1_name) DO
M3toC.FreeSharedS($1_name[i],$1[i]);
END;%}

%typemap(m3wrapindefault) PLINT oldwin "0"

%insert(m3wrapintf) %{TYPE
AxesScaling = {none, independent, equal, square};
Tile = {box, ticks, axes, gridMajor, gridMinor, xTicksLog, yTicksLog};
TileSet = SET OF Tile;
%}
%typemap(m3wrapintype)    PLINT just %{AxesScaling%}
%typemap(m3wrapindefault) PLINT just %{AxesScaling.independent%}
%typemap(m3wrapargraw)    PLINT just %{ORD($1_name)-1%}
%typemap(m3wrapintype)    PLINT axis %{TileSet%}
%typemap(m3wrapindefault) PLINT axis %{TileSet{Tile.box,Tile.ticks}%}
%typemap(m3wrapargvar)    PLINT axis %{$1: C.int;%}
%typemap(m3wrapargraw)    PLINT axis %{$1%}
%typemap(m3wrapinconv) PLINT axis
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

%insert(m3wrapintf) %{TYPE
DirTile = {axis, lowerBorder, upperBorder, fixedPointLabel,
gridMajor, gridMinor, ticksOutward, logarithmic,
labelMajorUnconv, labelMinorUnconv,
ticksMajor, ticksMinor};
DirTileSet = SET OF DirTile;
%}
%insert(m3wrapimpl) %{CONST
tileToChar = ARRAY DirTile OF CHAR {'a','b','c','f','g','h','i','l','m','n','s','t'};
%}
%typemap(m3rawintype)   const char *xopt, const char *yopt, const char *zopt %{(*ARRAY OF*) CHAR%}
%typemap(m3rawinmode)   const char *xopt, const char *yopt, const char *zopt %{READONLY%}
%typemap(m3wrapintype)  const char *xopt, const char *yopt, const char *zopt %{DirTileSet%}
%typemap(m3wrapargvar)  const char *xopt, const char *yopt, const char *zopt
%{$1: ARRAY [0..ORD(LAST(Tile))+1] OF CHAR;
$1i: CARDINAL := 0;%}
%typemap(m3wrapargraw)  const char *xopt, const char *yopt, const char *zopt %{$1[0]%}
%typemap(m3wrapinconv)  const char *xopt, const char *yopt, const char *zopt
%{FOR t:=FIRST(DirTile) TO LAST(DirTile) DO
IF t IN $1_name THEN
$1[$1i]:=tileToChar[t];
INC($1i);
END;
END;
$1[$1i]:='\000';
%}
%typemap(m3wrapfreearg)  const char *xopt, const char *yopt, const char *zopt %{%}



%insert(m3wrapintf) %{
TYPE
  LineStyle = {none, continuous, shortDash, longDash, longDashShortGap,
                  dotDash, complex0, complex1, complex2};
%}
%typemap(m3wrapintype)  PLINT lin     %{[LineStyle.continuous..LAST(LineStyle)]%}
%typemap(m3wrapargraw)  PLINT lin     %{ORD($1_name)%}

%typemap(m3wrapintype)  PLINT mode    %{BOOLEAN%}
%typemap(m3wrapargraw)  PLINT mode    %{ORD($1_name)%}

%typemap(m3wrapoutconv) PLINTOutput status %{$1#0%}
%typemap(m3wrapouttype) PLINTOutput status %{BOOLEAN%}

%typemap(m3wrapinmode)  PLINT *p_argc %{VAR%}
%typemap(m3wrapintype)  PLINT *p_argc %{CARDINAL%}

%typemap(m3rawinmode)   char **argv   %{READONLY%}
%typemap(m3wrapinmode)  char **argv   %{READONLY%}
%typemap(m3rawintype)   char **argv   %{(*ARRAY OF*) C.char_star%}
%typemap(m3wrapintype)  char **argv   %{ARRAY OF TEXT%}

%typemap("m3wrapintype")         PLFLT "Float"

%typemap("m3wrapargdir")         PLFLTOutput "out"
%typemap("m3wrapouttype")        PLFLTOutput "Float"

%typemap("m3wrapargdir")  PLINTOutput "out"
%typemap("m3wrapargvar")  PLINTOutput %{$1: C.int;%}
%typemap("m3wrapargraw")  PLINTOutput %{$1%}
%typemap("m3wrapoutconv") PLINTOutput %{$1%}
%typemap("m3wrapouttype") PLINTOutput %{INTEGER%}
/*this is a workaround and can be removed sometimes, I hope*/
%typemap("m3outvar")      PLINTOutput %{$1_name:C.int%}
%typemap("m3wrapoutname") PLINTOutput %{$1_name%}
%typemap("m3outvar:import") PLINTOutput %{Ctypes AS C%}




%m3multiretval plcalc_world;
%m3multiretval plgchr;
%m3multiretval plgcol0;
%m3multiretval plgcolbg;
%m3multiretval plgdidev;
%m3multiretval plgdiplt;
%m3multiretval plgfam;
%m3multiretval plgpage;
%m3multiretval plgspa;
%m3multiretval plgvpd;
%m3multiretval plgvpw;
%m3multiretval plgxax;
%m3multiretval plgyax;
%m3multiretval plgzax;
%m3multiretval plHLS_RGB;
%m3multiretval plRGB_HLS;
%m3multiretval pltr0;
%m3multiretval pltr1;
%m3multiretval pltr2;
%m3multiretval pltr2p;


/* swig compatible PLplot API definitions from here on. */
%include plplotcapi.i
