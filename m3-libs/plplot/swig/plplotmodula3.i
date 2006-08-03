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

%module LongRealPLPlot
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
import_lib("plplotd","/usr/local/lib")
include_dir("class")%}

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


// plplot.h overlaps with definitions in plplotcapi.h
//%import "plplot.h"
// thus copy the necessary part from plplot.h

#ifndef __PLSTUBS_H__	/* i.e. do not expand this in the stubs */

#define    pl_setcontlabelformat c_pl_setcontlabelformat
#define    pl_setcontlabelparam c_pl_setcontlabelparam
#define    pladv	c_pladv
#define    plaxes	c_plaxes
#define    plbin	c_plbin
#define    plbop	c_plbop
#define    plbox	c_plbox
#define    plbox3	c_plbox3
#define    plcalc_world	c_plcalc_world
#define    plclear	c_plclear
#define    plcol0	c_plcol0
#define    plcol1	c_plcol1
#define    plcont	c_plcont
#define    plcpstrm	c_plcpstrm
#define    plend	c_plend
#define    plend1	c_plend1
#define    plenv	c_plenv
#define    plenv0	c_plenv0
#define    pleop	c_pleop
#define    plerrx	c_plerrx
#define    plerry	c_plerry
#define    plfamadv	c_plfamadv
#define    plfill	c_plfill
#define    plfill3	c_plfill3
#define    plflush	c_plflush
#define    plfont	c_plfont
#define    plfontld	c_plfontld
#define    plgchr	c_plgchr
#define    plgcol0	c_plgcol0
#define    plgcolbg	c_plgcolbg
#define    plgcompression	c_plgcompression
#define    plgdev	c_plgdev
#define    plgdidev	c_plgdidev
#define    plgdiori	c_plgdiori
#define    plgdiplt	c_plgdiplt
#define    plgfam	c_plgfam
#define    plgfnam	c_plgfnam
#define    plglevel	c_plglevel
#define    plgpage	c_plgpage
#define    plgra	c_plgra
#define    plgriddata   c_plgriddata
#define    plgspa	c_plgspa
#define    plgstrm	c_plgstrm
#define    plgver	c_plgver
#define    plgvpd	c_plgvpd
#define    plgvpw	c_plgvpw
#define    plgxax	c_plgxax
#define    plgyax	c_plgyax
#define    plgzax	c_plgzax
#define    plhist	c_plhist
#define    plhls        c_plhls
#define    plhlsrgb     c_plhlsrgb
#define    plinit	c_plinit
#define    pljoin	c_pljoin
#define    pllab	c_pllab
#define    pllightsource	c_pllightsource
#define    plline	c_plline
#define    plline3	c_plline3
#define    pllsty	c_pllsty
#define    plmesh	c_plmesh
#define    plmeshc	c_plmeshc
#define    plmkstrm	c_plmkstrm
#define    plmtex	c_plmtex
#define    plot3d	c_plot3d
#define    plot3dc	c_plot3dc
#define    plot3dcl	c_plot3dcl
#define    plpat	c_plpat
#define    plpoin	c_plpoin
#define    plpoin3	c_plpoin3
#define    plpoly3	c_plpoly3
#define    plprec	c_plprec
#define    plpsty	c_plpsty
#define    plptex	c_plptex
#define    plreplot	c_plreplot
#define    plrgb	c_plrgb
#define    plrgb1	c_plrgb1
#define    plrgbhls     c_plrgbhls
#define    plschr	c_plschr
#define    plscmap0	c_plscmap0
#define    plscmap0n	c_plscmap0n
#define    plscmap1	c_plscmap1
#define    plscmap1l	c_plscmap1l
#define    plscmap1n	c_plscmap1n
#define    plscol0	c_plscol0
#define    plscolbg	c_plscolbg
#define    plscolor	c_plscolor
#define    plscompression	c_plscompression
#define    plsdev	c_plsdev
#define    plsdidev	c_plsdidev
#define    plsdimap	c_plsdimap
#define    plsdiori	c_plsdiori
#define    plsdiplt	c_plsdiplt
#define    plsdiplz	c_plsdiplz
#define    plsesc	c_plsesc
#define    plsetopt	c_plsetopt
#define    plsfam	c_plsfam
#define    plsfnam	c_plsfnam
#define    plshade	c_plshade
#define    plshade1	c_plshade1
#define    plshades	c_plshades
#define    plsmaj	c_plsmaj
#define    plsmem	c_plsmem
#define    plsmin	c_plsmin
#define    plsori	c_plsori
#define    plspage	c_plspage
#define    plspause	c_plspause
#define    plsstrm	c_plsstrm
#define    plssub	c_plssub
#define    plssym	c_plssym
#define    plstar	c_plstar
#define    plstart	c_plstart
#define    plstripa	c_plstripa
#define    plstripc	c_plstripc
#define    plstripd	c_plstripd
#define    plstyl	c_plstyl
#define    plsurf3d	c_plsurf3d
#define    plsurf3dl	c_plsurf3dl
#define    plsvect      c_plsvect
#define    plsvpa	c_plsvpa
#define    plsxax	c_plsxax
#define    plsyax	c_plsyax
#define    plsym	c_plsym
#define    plszax	c_plszax
#define    pltext	c_pltext
#define    plvasp	c_plvasp
#define    plvect	c_plvect
#define    plvpas	c_plvpas
#define    plvpor	c_plvpor
#define    plvsta	c_plvsta
#define    plw3d	c_plw3d
#define    plwid	c_plwid
#define    plwind	c_plwind
#define    plxormod	c_plxormod

#endif /* __PLSTUBS_H__ */

/* Redefine some old function names for backward compatibility */

#ifndef __PLSTUBS_H__	/* i.e. do not expand this in the stubs */

#define    plclr	pleop
#define    plpage	plbop
#define    plcol	plcol0
#define    plcontf	plfcont
#define	   Alloc2dGrid	plAlloc2dGrid
#define	   Free2dGrid	plFree2dGrid
#define    MinMax2dGrid plMinMax2dGrid
#define    plP_gvpd	plgvpd
#define    plP_gvpw	plgvpw
#define    plotsh3d(x,y,z,nx,ny,opt)     plsurf3d(x,y,z,nx,ny,opt, NULL, 0)

#endif /* __PLSTUBS_H__ */




%rename("SetContLabelFormat") pl_setcontlabelformat;
%rename("SetContLabelParam") pl_setcontlabelparam;
%rename("AdvanceSubPage") pladv;
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
%rename("GetFractOrientation") plgdiori;
%rename("GetWindowPlot") plgdiplt;
%rename("GetFamilyFile") plgfam;
%rename("GetFileName") plgfnam;
%rename("GetRunLevel") plglevel;
%rename("GetPageParam") plgpage;
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
%rename("SetFractOrientation") plsdiori;
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
%rename("SetOrientation") plsori;
%rename("SetPageParam") plspage;
%rename("SetPause") plspause;
%rename("SetStream") plsstrm;
%rename("SetSubWindows") plssub;
%rename("SetSymbolHeight") plssym;
%rename("Start") plstar;
%rename("StartDev") plstart;
%rename("AddStripchartPoint") plstripa;
%rename("CreateStripchart") plstripc;
%rename("DeleteStripchart") plstripd;
%rename("SetCustomLineStyle") plstyl;
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
%rename("PlotImage") plimage;
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
%rename("HLSToRGB") plhlsrgb;
%rename("RGBToHLS") plrgbhls;
%rename("GetCursor") plGetCursor;
%rename("TranslateCursor") plTranslateCursor;

/* Ignore FORTRAN routines */
%ignore pltr0f;
%ignore pltr2f;

%ignore plfcont;
%ignore plfshade;
%ignore plmap;
%ignore plmeridians;
%ignore plgFileDevs;
%ignore plgDevs;
%ignore plsButtonEH;
%ignore plMergeOpts;
%ignore plgfile;
%ignore plGetName;
%ignore plGetCursor;
%ignore plTranslateCursor;
%ignore plsKeyEH;

%ignore plgdev;
%ignore plgfnam;
%ignore plgver;
%ignore pldid2pc;
%ignore pldip2dc;
%ignore plsError;
%ignore plsexit;
%ignore plParseOpts;
%ignore plSetOpt;
%ignore plAlloc2dGrid;
%ignore plFree2dGrid;
%ignore plMinMax2dGrid;
%ignore plgesc;
%ignore pl_cmd;
%ignore plFindName;
%ignore plFindCommand;
%ignore plGetInt;
%ignore plGetFlt;

%ignore xform;


%pragma(modula3) enumitem="prefix=PLESPLFLTBUFFERING_;int;srcstyle=underscore;Buffering";
%pragma(modula3) enumitem="prefix=PLESC_;set;srcstyle=underscore;Escape";
%pragma(modula3) enumitem="prefix=PLSWIN_;int;srcstyle=underscore;Window";
%pragma(modula3) constint="prefix=PL_MAX;int;srcstyle=underscore,prefix=Max;CARDINAL";
%ignore PL_NOTSET;

%pragma(modula3) enumitem="prefix=DRAW_;set;srcstyle=underscore;DrawMode";
%pragma(modula3) makesetofenum="DrawMode";
%pragma(modula3) constset="prefix=DRAW_;set;srcstyle=underscore,prefix=Draw;DrawModeSet,DrawMode";

%pragma(modula3) enumitem="prefix=PL_OPT_;set;srcstyle=underscore;Option";
%pragma(modula3) makesetofenum="Option";
%pragma(modula3) constset="prefix=PL_OPT_;set;srcstyle=underscore,prefix=Opt;OptionSet,Option";

%pragma(modula3) enumitem="prefix=PL_PARSE_;set;srcstyle=underscore;Parse";
%pragma(modula3) makesetofenum="Parse";
%pragma(modula3) constset="prefix=PL_PARSE_;set;srcstyle=underscore,prefix=Parse;ParseSet,Parse";


%typemap("m3rawintype") PLINT  * %{C.int%}
%typemap("m3rawintype") PLFLT  * %{C.double%}
%typemap("m3rawintype") double * %{C.double%}

%typemap("m3wrapintype") PLBOOL %{BOOLEAN%}
%typemap("m3wrapargraw") PLBOOL %{ORD($1_name)%}

%typemap("m3wrapintype") PLCARD %{CARDINAL%}

%typemap("m3wrapargraw") char %{ORD($1_name)%}

%typemap("m3wrapintype",numinputs=0) PLArraySize n %{%}

%typemap("m3rawinmode")   PLFLTArray %{READONLY%}
%typemap("m3wrapinmode")  PLFLTArray %{READONLY%}
%typemap("m3rawintype")   PLFLTArray %{(*ARRAY OF*) C.double%}
%typemap("m3wrapintype")  PLFLTArray %{FloatVector%}
%typemap("m3wrapargraw")  PLFLTArray %{$1_name[0]%}

%typemap("m3wrapargvar")   PLFLTArrayFst, PLINTArrayFst, PLCARDArrayFst,
                           PLFLTArraySzd, PLINTArraySzd
%{n:=NUMBER($1_name);%}
%typemap("m3wrapargconst") PLFLTArrayFst, PLINTArrayFst, PLCARDArrayFst
%{nName="$1_name";%}
%typemap("m3wrapincheck")  PLFLTArrayCk,  PLINTArrayCk,  PLCARDArrayCk
%{<* ASSERT NUMBER($1_name) = n,
"Array sizes of $1_name (" & Fmt.Int(NUMBER($1_name)) &
") and " & nName & " (" & Fmt.Int(n) & ") mismatch." *> %}
%typemap("m3wrapincheck")  PLINTArrayCkInterim
%{<* ASSERT NUMBER($1_name) = n-1,
"Array size of $1_name (" & Fmt.Int(NUMBER($1_name)) &
" must be one more than that of " & nName & " (" & Fmt.Int(n) & ")." *> %}
%typemap("m3wrapincheck:import")  PLFLTArrayCk,  PLINTArrayCk, PLINTArrayCkInterim, PLCARDArrayCk  %{Fmt%}


%typemap("m3rawinmode")   PLINTArray %{READONLY%}
%typemap("m3wrapinmode")  PLINTArray %{READONLY%}
%typemap("m3rawintype")   PLINTArray %{C.int%}
%typemap("m3wrapintype")  PLINTArray %{ARRAY OF INTEGER%}
%typemap("m3wrapargraw")  PLINTArray %{$1_name[0]%}

%typemap("m3wrapintype")  PLCARDArray %{ARRAY OF CARDINAL%}


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
 x,y: LongRealPLPlotRaw.PLFLT;
 VAR (*OUT*) tx,ty: LongRealPLPlotRaw.PLFLT;
 data:LongRealPLPlotRaw.PLPointer;)=
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
%typemap(m3wrapintype)  defined_func %{LongRealPLPlotRaw.DefinedFunc%}
%typemap("m3wrapintype:import")  defined_func %{LongRealPLPlotRaw%}

%typemap(m3rawintype)   fill_func %{FillFunc%}
%typemap(m3wrapintype)  fill_func %{LongRealPLPlotRaw.FillFunc%}


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
AxesScaling = {None, Independent, Equal, Square};
Tile = {Box, Ticks, Axes, GridMajor, GridMinor, XTicksLog, YTicksLog};
TileSet = SET OF Tile;
%}
%typemap(m3wrapintype)    PLINT just %{AxesScaling%}
%typemap(m3wrapindefault) PLINT just %{AxesScaling.Independent%}
%typemap(m3wrapargraw)    PLINT just %{ORD($1_name)-1%}
%typemap(m3wrapintype)    PLINT axis %{TileSet%}
%typemap(m3wrapindefault) PLINT axis %{TileSet{Tile.Box,Tile.Ticks}%}
%typemap(m3wrapargvar)    PLINT axis %{$1: C.int;%}
%typemap(m3wrapargraw)    PLINT axis %{$1%}
%typemap(m3wrapinconv) PLINT axis
%{IF $1_name = TileSet{} THEN
$1:=-2;
ELSIF $1_name = TileSet{Tile.Box} THEN
$1:=-1;
ELSE
$1:=0;
IF Tile.XTicksLog IN $1_name THEN
  INC($1,10);
END;
IF Tile.YTicksLog IN $1_name THEN
  INC($1,20);
END;
$1_name := $1_name - TileSet{Tile.XTicksLog,Tile.YTicksLog};
IF    $1_name = TileSet{Tile.Box,Tile.Ticks} THEN INC($1,0);
ELSIF $1_name = TileSet{Tile.Box,Tile.Ticks,Tile.Axes} THEN INC($1,1);
ELSIF $1_name = TileSet{Tile.Box,Tile.Ticks,Tile.Axes,Tile.GridMajor} THEN INC($1,2);
ELSIF $1_name = TileSet{Tile.Box,Tile.Ticks,Tile.Axes,Tile.GridMajor,Tile.GridMinor} THEN INC($1,3);
ELSE
<*ASSERT FALSE*> (*combination not supported by PLPlot :-( *)
END;
END;%}

%insert(m3wrapintf) %{TYPE
DirTile = {Axis, LowerBorder, UpperBorder, FixedPointLabel,
GridMajor, GridMinor, TicksOutward, Logarithmic,
TickLabelsUnconv, TickLabelsConv,
TicksMajor, TicksMinor,
LabelBaseParallel};
DirTileSet = SET OF DirTile;
%}
%insert(m3wrapimpl) %{CONST
tileToChar = ARRAY DirTile OF CHAR {'a','b','c','f','g','h','i','l','m','n','s','t','v'};
%}
%typemap(m3rawintype)   const char *xopt, const char *yopt, const char *zopt %{(*ARRAY OF*) CHAR%}
%typemap(m3rawinmode)   const char *xopt, const char *yopt, const char *zopt %{READONLY%}
%typemap(m3wrapintype)  const char *xopt, const char *yopt, const char *zopt %{DirTileSet%}
%typemap(m3wrapargvar)  const char *xopt, const char *yopt, const char *zopt
%{$1: ARRAY [0..NUMBER(Tile)] OF CHAR;
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
  LineStyle = {None, Continuous, ShortDash, LongDash, LongDashShortGap,
               DotDash, Complex0, Complex1, Complex2};
%}
%typemap(m3wrapintype)  PLINT lin     %{[LineStyle.Continuous..LAST(LineStyle)]%}
%typemap(m3wrapargraw)  PLINT lin     %{ORD($1_name)%}

%typemap(m3wrapintype)  PLINT mode    %{BOOLEAN%}
%typemap(m3wrapargraw)  PLINT mode    %{ORD($1_name)%}

%typemap(m3wrapoutconv) PLINTOutput status %{$1#0%}
%typemap(m3wrapouttype) PLINTOutput status %{BOOLEAN%}


%insert(m3wrapintf) %{
TYPE
  CharacterSet = {Standard, Extended};
%}
%typemap(m3wrapintype)  PLINT fnt     %{CharacterSet%}
%typemap(m3wrapargraw)  PLINT fnt     %{ORD($1_name)%}

%insert(m3wrapintf) %{
TYPE
  FontType = {Normal, Roman, Italic, Script};
%}
%typemap(m3wrapintype)  PLINT ifont   %{FontType%}
%typemap(m3wrapargraw)  PLINT ifont   %{ORD($1_name)+1%}




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

%typemap(m3wrapindefault) PLCARD page %{0%}



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
%m3multiretval plhlsrgb;
%m3multiretval plrgbhls;
%m3multiretval pltr0;
%m3multiretval pltr1;
%m3multiretval pltr2;
%m3multiretval pltr2p;
%m3multiretval xform;


/* swig compatible PLplot API definitions from here on. */
%include plplotcapi.i
