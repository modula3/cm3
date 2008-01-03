(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Mar 28 17:34:10 PST 1996 by heydon                   *)

MODULE PSImpl;

IMPORT JunoConfig, Drawing, JunoPt, JunoRect, PSFont, JunoRsrc, View;
IMPORT   ExternalProc;
IMPORT JunoScope;
IMPORT JunoRT, RTVal, JunoValue, JunoArgs;
IMPORT VBT, VBTExtras, Filter, PaintOp, Font, Path, Point, Rect, DblBufferVBT;
IMPORT Atom, Rd, Wr, Fmt, Thread, Text, TextRefTbl, Time, Date;
IMPORT   Process, Pickle, Rsrc;
FROM ExternalProc IMPORT Closure, Bind;
FROM Stdio IMPORT stderr;

<* FATAL Thread.Alerted *>

EXCEPTION Error;			 (* internal error *)
<* FATAL Error *>			 (* should never be raised *)

CONST
  MaxCacheSize = 40;  (* # of external procs replaced by StartToFile(). *)
  FindFontProc = "FindFontISO";

TYPE
  ToFileClosure = Closure BRANDED "PSImpl.ToFileClosure" OBJECT
    i: Impl
  END;
  CacheRec = RECORD
    slot: CARDINAL;
    proc: Closure;
  END;

REVEAL
  View.PSImpl = Public BRANDED "View.PSImpl" OBJECT OVERRIDES
    init := Init
  END;

  Impl = ImplPublic BRANDED "PSImpl.Impl" OBJECT
    rt: View.Root;
    wr: Wr.T := NIL;
    extCnt: CARDINAL;
    page: CARDINAL;
    cache: ARRAY [0..MaxCacheSize - 1] OF CacheRec;
  OVERRIDES
    startToFile := StartToFile;
    prologue := Prologue;
    epilogue := Epilogue;
    endToFile := EndToFile;
  END;

(* "StartToFile" replaces the external PostScript procedures that change the
   PostScript state by "ToFileClosure" objects. For "Impl" "i", the method
   call "i.startToFile(wr)" sets "i.wr" to "wr", "i.extCnt" to the number of
   replaced external procedures, and stores the replaced procedures and the
   slots from which they came in "i.cache". *)

PROCEDURE Init(d: T; ch: Drawing.ChildPublic; root: View.Root): T =
  BEGIN
    d.root := root;
    EVAL View.T.init(d, ch);
    d.ps.path := NEW(Path.T);
    d.psStack := NEW(REF ARRAY OF State, 10);
    RETURN d
  END Init;

CONST
  DefaultColor = Color{r := 0.0, g := 0.0, b := 0.0};
  DefaultColorOp = PaintOp.Fg;
  DefaultTextColorOp = PaintOp.TransparentFg;
  DefaultWidth = 1.0;
  DefaultEndStyle = VBT.EndStyle.Butt;
  DefaultJointStyle = VBT.JoinStyle.Miter;
  DefaultWindingStyle = VBT.WindingCondition.NonZero;
  DefaultFaceName = "Times-Roman";
  DefaultFontSize = 4;			 (* PS.Large *)

VAR (* CONST *)
  fontTbl: TextRefTbl.T;
  metricTbl: TextRefTbl.T;
  defaultXFont: Font.T;			 (* cached copy of default X font *)
  defaultXFontPtSize: JunoValue.Real;	 (* cached value of it's point size *)
  defaultPSMetric: PSFont.Metric;	 (* cached metric of default font *)

<* INLINE *>
PROCEDURE ResetPath(VAR (*INOUT*) ps: State) =
  BEGIN
    Path.Reset(ps.path);
    ps.moveto := FALSE
  END ResetPath;

CONST
  PageWidth = 8.5 * 72.0;
  PageHeight = 11.0 * 72.0;
  HalfWidth = PageWidth / 2.0;
  HalfHeight = PageHeight / 2.0;

PROCEDURE DefaultBBox(d: T): JunoRect.T =
  TYPE
    OrientBBox = ARRAY JunoConfig.Orientation OF JunoRect.T;
  CONST
    BBoxSW = OrientBBox{
      JunoRect.T{0.0, PageWidth, PageHeight, 0.0},  (* Portrait *)
      JunoRect.T{0.0, PageHeight, PageWidth, 0.0}}; (* Landscape *)
    BBoxCenter = OrientBBox{
      JunoRect.T{-HalfWidth, HalfWidth, HalfHeight, -HalfHeight},
      JunoRect.T{-HalfHeight, HalfHeight, HalfWidth, -HalfWidth}};
    BBox = ARRAY JunoConfig.Origin OF OrientBBox{BBoxCenter, BBoxSW};
  VAR ch: Drawing.ChildPublic := Filter.Child(d); BEGIN
    RETURN BBox[ch.getOrigin(), JunoConfig.orientation]
  END DefaultBBox;

PROCEDURE Reset(d: T; <*UNUSED*> inExec := TRUE) =
  BEGIN
    WITH ps = d.ps DO
      ps.color := DefaultColor;
      ps.width := DefaultWidth;
      ps.end := DefaultEndStyle;
      ps.join := DefaultJointStyle;
      ps.wind := DefaultWindingStyle;
      ResetPath(ps);
      ps.face := DefaultFaceName;
      ps.size := DefaultFontSize;
      ps.ptSize := defaultXFontPtSize;
      ps.bbox := DefaultBBox(d);
      ps.colorOp := DefaultColorOp;
      ps.textColorOp := DefaultTextColorOp;
      ps.xFont := defaultXFont;
      ps.psMetric := defaultPSMetric
    END
  END Reset;

VAR (*CONST*)
  PSAtom   := Atom.FromText("PS");
  Save     := Atom.FromText("Save");
  Restore  := Atom.FromText("Restore");
  NewPath  := Atom.FromText("NewPath");
  MoveTo   := Atom.FromText("MoveTo");
  LineTo   := Atom.FromText("LineTo");
  CurveTo  := Atom.FromText("CurveTo");
  Close    := Atom.FromText("Close");
  Stroke   := Atom.FromText("Stroke");
  Fill     := Atom.FromText("Fill");
  Type     := Atom.FromText("Type");
  SetWidth := Atom.FromText("SetWidth");
  SetEnd   := Atom.FromText("SetEndStyle");
  SetJoin  := Atom.FromText("SetJointStyle");
  GetWidth := Atom.FromText("GetWidth");
  GetEnd   := Atom.FromText("GetEndStyle");
  GetJoin  := Atom.FromText("GetJointStyle");
  SetColor := Atom.FromText("SetColor");
  SetWind  := Atom.FromText("SetWinding");
  GetColor := Atom.FromText("GetColor");
  GetWind  := Atom.FromText("GetWinding");
  SetFace  := Atom.FromText("SetFontFace");
  SetSize  := Atom.FromText("SetFontSize");
  SetFont  := Atom.FromText("SetFont");
  GetFace  := Atom.FromText("GetFontFace");
  GetSize  := Atom.FromText("GetFontSize");
  GetFont  := Atom.FromText("GetFont");
  GetPtSz  := Atom.FromText("GetFontPtSize");
  FontH    := Atom.FromText("FontHeight");
  StringW  := Atom.FromText("StringWidth");
  StringBB := Atom.FromText("StringBBox");
  CurrPt   := Atom.FromText("CurrentPoint");
  CurrPath := Atom.FromText("CurrentPath");
  SetBBox  := Atom.FromText("SetBBox");
  GetBBox  := Atom.FromText("GetBBox");
  ShowPage := Atom.FromText("ShowPage");
  ResetSym := Atom.FromText("Reset");
  SavePage := Atom.FromText("SavePage");
  RestPage := Atom.FromText("RestorePage");

CONST
  ButtEndsVal    = 0;
  RoundEndsVal   = 1;
  SquareEndsVal  = 2;
  MiterJointsVal = 0;
  RoundJointsVal = 1;
  BevelJointsVal = 2;
  NZWindingVal   = 0;
  OddWindingVal  = 1;

PROCEDURE New(rt: View.Root): Impl =
  VAR
    scp := JunoScope.New(NIL, size := 24);
    res := NEW(Impl, rt := rt, public_scp := scp, scp := scp);
  BEGIN
    ExternalProc.SetupBind(PSAtom, scp, rt);
    Bind(Save,	   NEW(Closure, invoke := SaveProc),	     in := 0);
    Bind(Restore,  NEW(Closure, invoke := RestoreProc),	     in := 0);
    Bind(NewPath,  NEW(Closure, invoke := NewPathProc),	     in := 0);
    Bind(MoveTo,   NEW(Closure, invoke := MoveToProc),	     in := 1);
    Bind(LineTo,   NEW(Closure, invoke := LineToProc),	     in := 1);
    Bind(CurveTo,  NEW(Closure, invoke := CurveToProc),	     in := 3);
    Bind(Close,	   NEW(Closure, invoke := CloseProc),	     in := 0);
    Bind(Fill,	   NEW(Closure, invoke := FillProc),	     in := 0);
    Bind(Stroke,   NEW(Closure, invoke := StrokeProc),	     in := 0);
    Bind(Type,     NEW(Closure, invoke := TypeProc),	     in := 2);
    Bind(SetWidth, NEW(Closure, invoke := SetWidthProc),     in := 1);
    Bind(SetEnd,   NEW(Closure, invoke := SetEndStyleProc),  in := 1);
    Bind(SetJoin,  NEW(Closure, invoke := SetJoinStyleProc), in := 1);
    Bind(SetColor, NEW(Closure, invoke := SetColorProc),     in := 1);
    Bind(SetWind,  NEW(Closure, invoke := SetWindingProc),   in := 1);
    Bind(SetFace,  NEW(Closure, invoke := SetFaceProc),	     in := 1);
    Bind(SetSize,  NEW(Closure, invoke := SetSizeProc),	     in := 1);
    Bind(SetFont,  NEW(Closure, invoke := SetFontProc),	     in := 2);
    Bind(SetBBox,  NEW(Closure, invoke := SetBBoxProc),	     in := 2);
    Bind(ShowPage, NEW(Closure, invoke := ShowPageProc),     in := 0);
    Bind(ResetSym, NEW(Closure, invoke := ResetProc),        in := 0);
    Bind(SavePage, NEW(Closure, invoke := SavePageProc),     in := 0);
    Bind(RestPage, NEW(Closure, invoke := RestorePageProc),  in := 0);
    Bind(GetWidth, NEW(Closure, invoke := GetWidthProc),
      in := 0, out := 1);
    Bind(GetEnd,   NEW(Closure, invoke := GetEndStyleProc),
      in := 0, out := 1);
    Bind(GetJoin,  NEW(Closure, invoke := GetJoinStyleProc),
      in := 0, out := 1);
    Bind(GetColor, NEW(Closure, invoke := GetColorProc),
      in := 0, out := 1);
    Bind(GetWind,  NEW(Closure, invoke := GetWindingProc),
      in := 0, out := 1);
    Bind(GetFace,  NEW(Closure, invoke := GetFaceProc),
      in := 0, out := 1);
    Bind(GetSize,  NEW(Closure, invoke := GetSizeProc),
      in := 0, out := 1);
    Bind(GetFont,  NEW(Closure, invoke := GetFontProc),
      in := 0, out := 2);
    Bind(GetPtSz,  NEW(Closure, invoke := GetPtSizeProc),
      in := 0, out := 1);
    Bind(FontH,    NEW(Closure, invoke := FontHProc),
      in := 0, out := 2);
    Bind(StringW,  NEW(Closure, invoke := StringWProc),
      in := 1, out := 1);
    Bind(StringBB, NEW(Closure, invoke := StringBBProc),
      in := 1, out := 1);
    Bind(CurrPt,  NEW(Closure, invoke := CurrPtProc),
      in := 0, out := 1);
    Bind(CurrPath, NEW(Closure, invoke := CurrPathProc),
      in := 0, out := 1);
    Bind(GetBBox,  NEW(Closure, invoke := GetBBoxProc),
      in := 0, out := 2);
    RETURN res
  END New;

PROCEDURE StartToFile(impl: Impl; wr: Wr.T) =
(* An implementation of the "startToFile" method of an "Impl". *)

  PROCEDURE Replace(name: Atom.T; cl: ToFileClosure) =
  (* Store the current external procedure stored under "name" in "impl"'s
     cache, replace it by "cl" in the external code table, and set the "rt"
     and "i" fields of "cl". *)
    VAR p: JunoScope.Proc := JunoScope.Lookup(impl.scp, name); BEGIN
      WITH entry = impl.cache[impl.extCnt] DO
        entry.slot := p.index;
        entry.proc := JunoRT.ext_code_tbl[p.index];
        cl.rt := entry.proc.rt
      END;
      cl.i := impl;
      JunoRT.ext_code_tbl[p.index] := cl;
      INC(impl.extCnt)
    END Replace;

  (* StartToFile *)
  BEGIN
    impl.wr := wr;
    impl.extCnt := 0;
    impl.page := 1;
    Replace(Save,     NEW(ToFileClosure, invoke := SaveProc2));
    Replace(Restore,  NEW(ToFileClosure, invoke := RestoreProc2));
    Replace(NewPath,  NEW(ToFileClosure, invoke := NewPathProc2));
    Replace(MoveTo,   NEW(ToFileClosure, invoke := MoveToProc2));
    Replace(LineTo,   NEW(ToFileClosure, invoke := LineToProc2));
    Replace(CurveTo,  NEW(ToFileClosure, invoke := CurveToProc2));
    Replace(Close,    NEW(ToFileClosure, invoke := CloseProc2));
    Replace(Stroke,   NEW(ToFileClosure, invoke := StrokeProc2));
    Replace(Fill,     NEW(ToFileClosure, invoke := FillProc2));
    Replace(Type,     NEW(ToFileClosure, invoke := TypeProc2));
    Replace(SetWidth, NEW(ToFileClosure, invoke := SetWidthProc2));
    Replace(SetEnd,   NEW(ToFileClosure, invoke := SetEndStyleProc2));
    Replace(SetJoin,  NEW(ToFileClosure, invoke := SetJoinStyleProc2));
    Replace(SetColor, NEW(ToFileClosure, invoke := SetColorProc2));
    Replace(SetFace,  NEW(ToFileClosure, invoke := SetFaceProc2));
    Replace(SetSize,  NEW(ToFileClosure, invoke := SetSizeProc2));
    Replace(SetFont,  NEW(ToFileClosure, invoke := SetFontProc2));
    Replace(FontH,    NEW(ToFileClosure, invoke := FontHProc2));
    Replace(StringW,  NEW(ToFileClosure, invoke := StringWProc2));
    Replace(StringBB, NEW(ToFileClosure, invoke := StringBBProc2));
    Replace(ShowPage, NEW(ToFileClosure, invoke := ShowPageProc2));
    Replace(SavePage, NEW(ToFileClosure, invoke := SavePageProc2));
    Replace(RestPage, NEW(ToFileClosure, invoke := RestorePageProc2));
  END StartToFile;

PROCEDURE Prologue(impl: Impl) RAISES {Wr.Failure} =
  BEGIN
    <* ASSERT impl.wr # NIL *>
    WriteHeader(impl.wr);
    WritePrologue(impl.wr, impl.rt.currView);
    WriteSetup(impl.wr, impl.rt.currView);
    WritePageHeader(impl.wr, impl.page)
  END Prologue;

PROCEDURE WriteHeader(wr: Wr.T) RAISES {Wr.Failure} =
  BEGIN
    Wr.PutText(wr, "%!PS-Adobe-3.0\n");
    Wr.PutText(wr, "%%Creator: Juno-2\n");
    Wr.PutText(wr, "%%Title: Juno.ps\n");
    Wr.PutText(wr, "%%CreationDate: ");
    WriteTime(wr, Time.Now());
    Wr.PutText(wr, "\n%%BoundingBox: (atend)\n");
    Wr.PutText(wr, "%%Pages: (atend)\n");
    Wr.PutText(wr, "%%PageOrder: Ascend\n");
    Wr.PutText(wr, "%%Orientation: "
      & JunoConfig.OrientName[JunoConfig.orientation] & "\n");
    Wr.PutText(wr, "%%EndComments\n");
  END WriteHeader;

PROCEDURE WritePrologue(wr: Wr.T; d: T) RAISES {Wr.Failure} =
  VAR ch: Drawing.ChildPublic := Filter.Child(d); BEGIN
    Wr.PutText(wr, "\n%%BeginPrologue\n");

    (* define "InitializeJunoPage" procedure *)
    Wr.PutText(wr, "% InitializeJunoPage\n%\n");
    Wr.PutText(wr, "% Sets the initial graphics state for a Juno page\n");
    Wr.PutText(wr, "/InitializeJunoPage {\n  ");
    Wr.PutText(wr, Fmt.Real(DefaultColor.r)); Wr.PutChar(wr, ' ');
    Wr.PutText(wr, Fmt.Real(DefaultColor.g)); Wr.PutChar(wr, ' ');
    Wr.PutText(wr, Fmt.Real(DefaultColor.b));
    Wr.PutText(wr, " setrgbcolor\n  ");
    Wr.PutText(wr, Fmt.Real(DefaultWidth));
    Wr.PutText(wr, " setlinewidth\n  ");
    Wr.PutText(wr, Fmt.Int(EndMapInv[DefaultEndStyle]));
    Wr.PutText(wr, " setlinecap\n  ");
    Wr.PutText(wr, Fmt.Int(JoinMapInv[DefaultJointStyle]));
    Wr.PutText(wr, " setlinejoin\n  ");
    Wr.PutText(wr, "10.435 setmiterlimit\n  ");
    (* Wr.PutText(wr, "newpath\n  "); *)
    Wr.PutChar(wr, '/');
    Wr.PutText(wr, DefaultFaceName);
    WriteFindFont(wr);
    Wr.PutText(wr, Fmt.Real(defaultXFontPtSize));
    Wr.PutText(wr, " scalefont setfont\n  ");

    (* Translate and rotate if necessary, based on "ch.getOrigin()" and
       "JunoConfig.orientation". The PostScript variables "xCenter" and
       "yCenter" are set to the coordinate at the center of the page for use
       by the "showerror.ps" code in case a run-time error needs to be
       displayed. *)
    CASE ch.getOrigin() OF
      JunoConfig.Origin.Center =>
      	Wr.PutText(wr, Fmt.Real(HalfWidth));  Wr.PutChar(wr, ' ');
      	Wr.PutText(wr, Fmt.Real(HalfHeight)); Wr.PutChar(wr, ' ');
      	Wr.PutText(wr, "translate\n")
    | JunoConfig.Origin.SW =>
      	IF JunoConfig.orientation = JunoConfig.Orientation.Landscape THEN
      	  Wr.PutText(wr, Fmt.Real(PageWidth));
      	  Wr.PutText(wr, " 0 translate\n")
      	END
    END;
    IF JunoConfig.orientation = JunoConfig.Orientation.Landscape THEN
      Wr.PutText(wr, "  90 rotate\n");
    END;
    Wr.PutText(wr, "} def\n\n");

    (* copy "prologue.ps" file *)
    <* FATAL Rd.Failure, Rd.EndOfFile, Thread.Alerted, Rsrc.NotFound *>
    VAR rd: Rd.T := Rsrc.Open("prologue.ps", JunoRsrc.Path); BEGIN
      (* copy PostScript code to "wr" *)
      WHILE NOT Rd.EOF(rd) DO Wr.PutChar(wr, Rd.GetChar(rd)) END;
      Rd.Close(rd)
    END;
    Wr.PutText(wr, "%%EndPrologue\n")
  END WritePrologue;

PROCEDURE WriteTime(wr: Wr.T; t: Time.T) RAISES {Wr.Failure} =
(* Writes the time "t" to "wr" in the form:
|    "Wed, Jun 22, 11:19:40 PDT, 1994".
*)
  CONST
    MonthName = ARRAY OF TEXT{
      "Jan", "Feb", "Mar", "Apr", "May", "Jun",
      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
    DayName = ARRAY OF TEXT{
      "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"}; 
  VAR d: Date.T := Date.FromTime(t); BEGIN
    Wr.PutText(wr, DayName[ORD(d.weekDay)]); Wr.PutText(wr, ", ");
    Wr.PutText(wr, MonthName[ORD(d.month)]); Wr.PutChar(wr, ' ');
    Wr.PutText(wr, Fmt.Int(d.day));          Wr.PutText(wr, ", ");
    Wr.PutText(wr, Fmt.Int(d.hour));         Wr.PutChar(wr, ':');
    Wr.PutText(wr, Fmt.Int(d.minute));       Wr.PutChar(wr, ':');
    Wr.PutText(wr, Fmt.Int(d.second));       Wr.PutChar(wr, ' ');
    Wr.PutText(wr, d.zone);                  Wr.PutText(wr, ", ");
    Wr.PutText(wr, Fmt.Int(d.year))
  END WriteTime;

PROCEDURE WriteFindFont(wr: Wr.T) RAISES {Wr.Failure} =
(* Writes the name of the "findfont" procedure to "wr" surrounded by space
   characters. *)
  BEGIN
    Wr.PutChar(wr, ' ');
    Wr.PutText(wr, FindFontProc);
    Wr.PutChar(wr, ' ')
  END WriteFindFont;

PROCEDURE WriteSetup(wr: Wr.T; d: T) RAISES {Wr.Failure} =
  VAR ch: Drawing.ChildPublic := Filter.Child(d); dx, dy := 0.0; BEGIN
    Wr.PutText(wr, "\n%%BeginSetup\n");
    Wr.PutText(wr, "% define the coordinates of the center of the page\n");
    IF ch.getOrigin() = JunoConfig.Origin.SW THEN
      CASE JunoConfig.orientation OF
        JunoConfig.Orientation.Portrait =>  dx := HalfWidth; dy := HalfHeight
      | JunoConfig.Orientation.Landscape => dx := HalfHeight; dy := HalfWidth
      END
    END;
    Wr.PutText(wr, "/xCenter " & Fmt.Real(dx) & " def ");
    Wr.PutText(wr, "/yCenter " & Fmt.Real(dy) & " def\n");
    Wr.PutText(wr, "%%EndSetup\n")
  END WriteSetup;

PROCEDURE WritePageHeader(wr: Wr.T; pageNum: CARDINAL) RAISES {Wr.Failure} =
  VAR pg := Fmt.Int(pageNum); BEGIN
    Wr.PutText(wr, "\n%%Page: ");
    Wr.PutText(wr, pg); Wr.PutChar(wr, ' '); Wr.PutText(wr, pg);
    Wr.PutText(wr, "\nsave\n");
    Wr.PutText(wr, "InitializeJunoPage\n")
  END WritePageHeader;

PROCEDURE WritePageTrailer(wr: Wr.T) RAISES {Wr.Failure} =
(* Invoked at the end of each page; brackets the "save" done in
   "WritePageHeader". *)
  BEGIN
    Wr.PutText(wr, "restore\n");
  END WritePageTrailer;

PROCEDURE Epilogue(impl: Impl; showPage := FALSE) RAISES {Wr.Failure} =
(* An implementation of the "endToFile" method of an "Impl". *)
  VAR
    d := impl.rt.currView;
    ch: Drawing.ChildPublic := Filter.Child(d);
  BEGIN
    <* ASSERT impl.wr # NIL *>
    WritePageTrailer(impl.wr);
    IF showPage THEN Wr.PutText(impl.wr, "showpage\n") END;
    Wr.PutText(impl.wr, "\n%%Trailer\n");
    Wr.PutText(impl.wr, "%%BoundingBox: ");
    VAR bbox := d.ps.bbox; BEGIN
      (* rotate if in "JunoConfig.Orientation.Landscape" *)
      IF JunoConfig.orientation = JunoConfig.Orientation.Landscape THEN
        IF ch.getOrigin() = JunoConfig.Origin.SW THEN
          (* translate to portrait-page origin *)
          bbox := JunoRect.Add(bbox, JunoPt.T{0.0, -PageWidth})
        END;
        bbox := JunoRect.Rotate90(bbox);
      END;
      (* translate if at "Origin.Center" *)
      IF ch.getOrigin() = JunoConfig.Origin.Center THEN
        bbox := JunoRect.Add(bbox, JunoPt.T{HalfWidth, HalfHeight})
      END;
      WriteRect(impl.wr, bbox)
    END;
    Wr.PutText(impl.wr, "\n%%Pages: ");
    Wr.PutText(impl.wr, Fmt.Int(impl.page));
    Wr.PutText(impl.wr, "\n%%EOF\n")
  END Epilogue;

PROCEDURE EndToFile(impl: Impl) =
  BEGIN
    <* ASSERT impl.wr # NIL *>
    FOR i := FIRST(impl.cache) TO impl.extCnt - 1 DO
      JunoRT.ext_code_tbl[impl.cache[i].slot] := impl.cache[i].proc
    END;
    impl.wr := NIL
  END EndToFile;

PROCEDURE CopyState(READONLY from: State; VAR (*OUT*) to: State) =
  BEGIN
    to := from;
    to.path := Path.Copy(from.path);
  END CopyState;

PROCEDURE SaveProc(dc: Closure): BOOLEAN =
  VAR d := dc.rt.currView; BEGIN
    IF d.sp > LAST(d.psStack^) THEN
      VAR new := NEW(REF ARRAY OF State, 2 * NUMBER(d.psStack^)); BEGIN
        SUBARRAY(new^, 0, NUMBER(d.psStack^)) := d.psStack^;
        d.psStack := new
      END
    END;
    CopyState(d.ps, d.psStack[d.sp]);
    INC(d.sp);
    RETURN TRUE
  END SaveProc;

PROCEDURE SaveProc2(cl: ToFileClosure): BOOLEAN =
  BEGIN
    IF NOT SaveProc(cl) THEN RETURN FALSE END;
    TRY Wr.PutText(cl.i.wr, "gsave\n") EXCEPT
      Wr.Failure => RETURN FALSE
    END;
    RETURN TRUE
  END SaveProc2;

PROCEDURE RestoreProc(dc: Closure): BOOLEAN =
  VAR d := dc.rt.currView; BEGIN
    IF d.sp = 0 THEN RETURN FALSE END;
    DEC(d.sp);
    d.ps := d.psStack[d.sp];
    RETURN TRUE
  END RestoreProc;

PROCEDURE RestoreProc2(cl: ToFileClosure): BOOLEAN =
  BEGIN
    IF NOT RestoreProc(cl) THEN RETURN FALSE END;
    TRY Wr.PutText(cl.i.wr, "grestore\n") EXCEPT
      Wr.Failure => RETURN FALSE
    END;
    RETURN TRUE
  END RestoreProc2;

<* INLINE *>
PROCEDURE WritePoint(wr: Wr.T; READONLY pt: JunoPt.T) RAISES {Wr.Failure} =
  BEGIN
    Wr.PutText(wr, Fmt.Real(pt.x)); Wr.PutChar(wr, ' ');
    Wr.PutText(wr, Fmt.Real(pt.y)); Wr.PutChar(wr, ' ')
  END WritePoint;

<* INLINE *>
PROCEDURE WriteRect(wr: Wr.T; READONLY rect: JunoRect.T) RAISES {Wr.Failure} =
  BEGIN
    Wr.PutText(wr, Fmt.Real(rect.west));  Wr.PutChar(wr, ' ');
    Wr.PutText(wr, Fmt.Real(rect.south)); Wr.PutChar(wr, ' ');
    Wr.PutText(wr, Fmt.Real(rect.east));  Wr.PutChar(wr, ' ');
    Wr.PutText(wr, Fmt.Real(rect.north)); Wr.PutChar(wr, ' ')
  END WriteRect;

(* ======================== Callback Procedures ============================ *)

(* Implementation Note:

   In most cases, when examining Juno arguments passed on the Juno machine's
   stack, we must use a "NULL =>" TYPECASE arm to handle the possibility of
   Modula-3 NIL being passed on the stack. However, we can omit this TYPECASE
   arm when the expected value is a point, since the subsequent call to the
   procedure "JunoPt.FromValuePair" on this argument will raise "JunoPt.BadPt"
   in that case. *)

PROCEDURE NewPathProc(dc: Closure): BOOLEAN =
  BEGIN
    ResetPath(dc.rt.currView.ps);
    RETURN TRUE
  END NewPathProc;

PROCEDURE NewPathProc2(cl: ToFileClosure): BOOLEAN =
  BEGIN
    IF NewPathProc(cl) THEN
      TRY
        Wr.PutText(cl.i.wr, "newpath\n");
        RETURN TRUE
      EXCEPT Wr.Failure => (* SKIP *)
      END
    END;
    RETURN TRUE
  END NewPathProc2;

PROCEDURE MoveToProc(dc: Closure): BOOLEAN =
  VAR err := FALSE; pr := JunoArgs.ReadPair(1, err); BEGIN
    IF NOT err THEN
      WITH ps = dc.rt.currView.ps DO
        VAR pt: JunoPt.T; BEGIN
          TRY pt := JunoPt.FromValuePair(pr) EXCEPT
            JunoPt.BadPt => RETURN FALSE
          END;
          ps.moveto := TRUE;
          ps.movetoPt := pt;
          ps.currPt := pt;
          ps.subpathStartPt := pt;
          RETURN TRUE
        END
      END
    END;
    RETURN FALSE
  END MoveToProc;

PROCEDURE MoveToProc2(cl: ToFileClosure): BOOLEAN =
  BEGIN
    IF MoveToProc(cl) THEN
      TRY
        WITH wr = cl.i.wr DO
          WritePoint(wr, cl.rt.currView.ps.currPt);
          Wr.PutText(wr, "moveto\n")
        END;
        RETURN TRUE
      EXCEPT
        Wr.Failure => (* SKIP *)
      END
    ELSE (* SKIP *)
    END;
    RETURN FALSE
  END MoveToProc2;

PROCEDURE StartSegment(VAR (*INOUT*) ps: State; ch: Drawing.ChildPublic):
    BOOLEAN =
(* Code executed when a new straight or curved segment is added to the path to
   maintain the invariants on the "moveto" and "movetoPt" fields. Returns
   FALSE iff the current path is logically empty. *)
  BEGIN
    IF ps.moveto THEN
      ps.moveto := FALSE;
      Path.MoveTo(ps.path, JunoPt.ToHV(ps.movetoPt, ch.xform))
    ELSIF Path.IsClosed(ps.path) THEN
      RETURN FALSE
    END;
    RETURN TRUE
  END StartSegment;

PROCEDURE LineToProc(dc: Closure): BOOLEAN =
  BEGIN
    WITH d = dc.rt.currView, ps = d.ps DO
      VAR ch: Drawing.ChildPublic := Filter.Child(d); BEGIN
      	IF StartSegment(ps, ch) THEN
      	  VAR err := FALSE; pr := JunoArgs.ReadPair(1, err); BEGIN
      	    IF NOT err THEN
	      TRY ps.currPt := JunoPt.FromValuePair(pr) EXCEPT
		JunoPt.BadPt => RETURN FALSE
	      END;
      	      Path.LineTo(ps.path, JunoPt.ToHV(ps.currPt, ch.xform));
	      RETURN TRUE
      	    END
      	  END
      	END
      END
    END;
    RETURN FALSE
  END LineToProc;

PROCEDURE LineToProc2(cl: ToFileClosure): BOOLEAN =
  BEGIN
    IF LineToProc(cl) THEN
      TRY
        WritePoint(cl.i.wr, cl.rt.currView.ps.currPt);
        Wr.PutText(cl.i.wr, "lineto\n");
        RETURN TRUE
      EXCEPT Wr.Failure => (* SKIP *)
      END
    END;
    RETURN FALSE
  END LineToProc2;

PROCEDURE CurveToProc(dc: Closure): BOOLEAN =
  VAR dummy1, dummy2, dummy3: JunoPt.T; BEGIN
    RETURN CurveToWork(dc, dummy1, dummy2, dummy3)
  END CurveToProc;

PROCEDURE CurveToProc2(cl: ToFileClosure): BOOLEAN =
  VAR pt1, pt2, pt3: JunoPt.T; BEGIN
    IF CurveToWork(cl, pt1, pt2, pt3) THEN
      WITH wr = cl.i.wr DO
        TRY
	  WritePoint(wr, pt1);
	  WritePoint(wr, pt2);
	  WritePoint(wr, pt3);
	  Wr.PutText(wr, "curveto\n");
	  RETURN TRUE
        EXCEPT
          Wr.Failure => (* SKIP *)
        END;
      END
    END;
    RETURN FALSE
  END CurveToProc2;

PROCEDURE CurveToWork(dc: Closure; VAR (*OUT*) pt1, pt2, pt3: JunoPt.T):
    BOOLEAN =
  BEGIN
    WITH d = dc.rt.currView, ps = d.ps DO
      VAR ch: Drawing.ChildPublic := Filter.Child(d); BEGIN
      	IF StartSegment(ps, ch) THEN
      	  VAR
      	    err := FALSE;
      	    pr1 := JunoArgs.ReadPair(3, err);
      	    pr2 := JunoArgs.ReadPair(2, err);
      	    pr3 := JunoArgs.ReadPair(1, err);
      	  BEGIN
      	    IF NOT err THEN
	      TRY
      		pt1 := JunoPt.FromValuePair(pr1);
      		pt2 := JunoPt.FromValuePair(pr2);
      		pt3 := JunoPt.FromValuePair(pr3)
	      EXCEPT
      		JunoPt.BadPt => RETURN FALSE
	      END;
	      Path.CurveTo(ps.path,
      		JunoPt.ToHV(pt1, ch.xform),
      		JunoPt.ToHV(pt2, ch.xform),
      		JunoPt.ToHV(pt3, ch.xform));
      	      ps.currPt := pt3;
	      RETURN TRUE
      	    END
	  END
      	END
      END
    END;
    RETURN FALSE
  END CurveToWork;

PROCEDURE CloseProc(dc: Closure): BOOLEAN =
  BEGIN
    WITH d = dc.rt.currView, ps = d.ps DO
      IF ps.moveto THEN
        ps.moveto := FALSE;
        VAR ch: Drawing.ChildPublic := Filter.Child(d); BEGIN
          Path.MoveTo(ps.path, JunoPt.ToHV(ps.movetoPt, ch.xform))
        END
      ELSIF Path.IsClosed(ps.path) THEN
        RETURN FALSE
      END;
      Path.Close(ps.path);
      ps.currPt := ps.subpathStartPt
    END;
    RETURN TRUE
  END CloseProc;

PROCEDURE CloseProc2(cl: ToFileClosure): BOOLEAN =
  BEGIN
    IF CloseProc(cl) THEN
      TRY
        Wr.PutText(cl.i.wr, "closepath\n");
        RETURN TRUE
      EXCEPT Wr.Failure => (* SKIP *)
      END;
    END;
    RETURN FALSE
  END CloseProc2;

PROCEDURE StrokeProc(dc: Closure): BOOLEAN =
  BEGIN
    WITH d = dc.rt.currView, ps = d.ps DO
      VAR ch: Drawing.ChildPublic := Filter.Child(d); BEGIN
      	VBT.Stroke(ch, Rect.Full, ps.path,
      	  width := ROUND(ps.width * ch.xform.widthScale),
      	  end := ps.end, join := ps.join, op := ps.colorOp)
      END;
      ResetPath(ps)
    END;
    RETURN TRUE
  END StrokeProc;

PROCEDURE StrokeProc2(cl: ToFileClosure): BOOLEAN =
  BEGIN
    TRY Wr.PutText(cl.i.wr, "stroke\n") EXCEPT
      Wr.Failure => RETURN FALSE
    END;
    ResetPath(cl.rt.currView.ps);
    RETURN TRUE
  END StrokeProc2;

PROCEDURE FillProc(dc: Closure): BOOLEAN =
  BEGIN
    WITH d = dc.rt.currView, ps = d.ps DO
      VAR ch: Drawing.ChildPublic := Filter.Child(d); BEGIN
        VBT.Fill(ch, Rect.Full, ps.path, wind := ps.wind, op := ps.colorOp)
      END;
      ResetPath(ps)
    END;
    RETURN TRUE
  END FillProc;

PROCEDURE FillProc2(cl: ToFileClosure): BOOLEAN =
  BEGIN
    TRY
      CASE cl.rt.currView.ps.wind OF
    	VBT.WindingCondition.NonZero => Wr.PutText(cl.i.wr, "fill\n")
      | VBT.WindingCondition.Odd     => Wr.PutText(cl.i.wr, "eofill\n")
      END
    EXCEPT
      Wr.Failure => RETURN FALSE
    END;
    ResetPath(cl.rt.currView.ps);
    RETURN TRUE
  END FillProc2;

PROCEDURE TypeProc(dc: Closure): BOOLEAN =
  VAR
    err := FALSE;
    pr := JunoArgs.ReadPair(2, err);
    t := JunoArgs.ReadText(1, err);
  BEGIN
    IF NOT err THEN
      WITH d = dc.rt.currView, ps = d.ps DO
        VAR ch: Drawing.ChildPublic := Filter.Child(d); pt: JunoPt.T; BEGIN
          TRY pt := JunoPt.FromValuePair(pr) EXCEPT
            JunoPt.BadPt => RETURN FALSE
          END;
          VBT.PaintText(ch, fnt := ps.xFont, t := t,
            pt := JunoPt.ToHV(pt, ch.xform),
            op := ps.textColorOp);
          RETURN TRUE
        END
      END
    END;
    RETURN FALSE
  END TypeProc;

PROCEDURE TypeProc2(cl: ToFileClosure): BOOLEAN =
  VAR
    err := FALSE;
    pr := JunoArgs.ReadPair(2, err);
    t := JunoArgs.ReadText(1, err);
  BEGIN
    IF NOT err THEN
      VAR wr := cl.i.wr; pt: JunoPt.T; BEGIN
        TRY
          pt := JunoPt.FromValuePair(pr);
          Wr.PutText(wr, "gsave\n");
          WritePoint(wr, pt);
          Wr.PutText(wr, "moveto\n(");
          Wr.PutText(wr, EscapeString(t));
          Wr.PutText(wr, ") show\n");
          Wr.PutText(wr, "grestore\n")
        EXCEPT
          JunoPt.BadPt, Wr.Failure => RETURN FALSE
        END;
        RETURN TRUE
      END
    END;
    RETURN FALSE
  END TypeProc2;

PROCEDURE EscapeString(t: TEXT): TEXT =
(* Return a text equivalent to "t", but with non-printing and PostScript-
   special characters (namely, '(', ')', and '\') converted to octal escape
   sequences. *)
  CONST
    PSSpecial = SET OF CHAR {'(', ')', '\\'};
    Printing = SET OF CHAR {' ' .. '~'} - PSSpecial;
  PROCEDURE OctalString(c: CHAR): TEXT =
    BEGIN
      IF c IN PSSpecial THEN RETURN Text.FromChar(c) END;
      RETURN Fmt.Pad(Fmt.Int(ORD(c), base := 8), 3, padChar := '0')
    END OctalString;
  VAR res := ""; start := 0; c: CHAR; len := Text.Length(t); BEGIN
    FOR i := 0 TO len - 1 DO
      c := Text.GetChar(t, i);
      IF NOT c IN Printing THEN
        (* flush batch of chars in [start, i) *)
        IF start < i THEN res := res & Text.Sub(t, start, i - start) END;
        res := res & "\\" & OctalString(c);
        start := i + 1
      END
    END;
    (* fast path: no escaped characters *)
    IF start = 0 THEN RETURN t END;
    (* otherwise, flush suffix if necessary *)
    IF start < len THEN res := res & Text.Sub(t, start, len - start) END;
    RETURN res
  END EscapeString;

PROCEDURE SetWidthProc(dc: Closure): BOOLEAN =
  VAR err := FALSE; r := JunoArgs.ReadReal(1, err); BEGIN
    IF NOT err AND r >= 0.0 THEN
      dc.rt.currView.ps.width := r;
      RETURN TRUE
    END;
    RETURN FALSE
  END SetWidthProc;

PROCEDURE SetWidthProc2(cl: ToFileClosure): BOOLEAN =
  BEGIN
    IF SetWidthProc(cl) THEN
      TRY
        Wr.PutText(cl.i.wr, Fmt.Real(cl.rt.currView.ps.width));
        Wr.PutText(cl.i.wr, " setlinewidth\n");
        RETURN TRUE
      EXCEPT Wr.Failure => (* SKIP *)
      END
    END;
    RETURN FALSE
  END SetWidthProc2;

PROCEDURE GetWidthProc(dc: Closure): BOOLEAN =
  BEGIN
    JunoArgs.WriteReal(1, dc.rt.currView.ps.width);
    RETURN TRUE
  END GetWidthProc;

CONST EndMap = ARRAY [ButtEndsVal..SquareEndsVal] OF VBT.EndStyle{
  VBT.EndStyle.Butt, VBT.EndStyle.Round, VBT.EndStyle.Square};

PROCEDURE SetEndStyleProc(dc: Closure): BOOLEAN =
  VAR err := FALSE; es := JunoArgs.ReadInt(1, err); BEGIN
    IF NOT err AND ButtEndsVal <= es AND es <= SquareEndsVal THEN
      dc.rt.currView.ps.end := EndMap[es];
      RETURN TRUE
    END;
    RETURN FALSE
  END SetEndStyleProc;

CONST EndMapInv = ARRAY VBT.EndStyle OF INTEGER{
  RoundEndsVal, ButtEndsVal, SquareEndsVal};

PROCEDURE SetEndStyleProc2(cl: ToFileClosure): BOOLEAN =
  BEGIN
    IF SetEndStyleProc(cl) THEN
      TRY
        Wr.PutText(cl.i.wr, Fmt.Int(EndMapInv[cl.rt.currView.ps.end]));
        Wr.PutText(cl.i.wr, " setlinecap\n")
      EXCEPT Wr.Failure => (* SKIP *)
      END
    END;
    RETURN TRUE
  END SetEndStyleProc2;

PROCEDURE GetEndStyleProc(dc: Closure): BOOLEAN =
  BEGIN
    JunoArgs.WriteInt(1, EndMapInv[dc.rt.currView.ps.end]);
    RETURN TRUE
  END GetEndStyleProc;

CONST JoinMap = ARRAY [MiterJointsVal..BevelJointsVal] OF VBT.JoinStyle {
  VBT.JoinStyle.Miter, VBT.JoinStyle.Round, VBT.JoinStyle.Bevel};

PROCEDURE SetJoinStyleProc(dc: Closure): BOOLEAN =
  VAR err := FALSE; js := JunoArgs.ReadInt(1, err); BEGIN
    IF NOT err AND MiterJointsVal <= js AND js <= BevelJointsVal THEN
      dc.rt.currView.ps.join := JoinMap[js];
      RETURN TRUE
    END;
    RETURN FALSE
  END SetJoinStyleProc;

CONST JoinMapInv = ARRAY VBT.JoinStyle OF INTEGER {
  RoundJointsVal, BevelJointsVal, MiterJointsVal};

PROCEDURE SetJoinStyleProc2(cl: ToFileClosure): BOOLEAN =
  BEGIN
    IF SetJoinStyleProc(cl) THEN
      TRY
        Wr.PutText(cl.i.wr, Fmt.Int(JoinMapInv[cl.rt.currView.ps.join]));
        Wr.PutText(cl.i.wr, " setlinejoin\n")
      EXCEPT Wr.Failure => (* SKIP *)
      END
    END;
    RETURN TRUE
  END SetJoinStyleProc2;

PROCEDURE GetJoinStyleProc(dc: Closure): BOOLEAN =
  BEGIN
    JunoArgs.WriteInt(1, JoinMapInv[dc.rt.currView.ps.join]);
    RETURN TRUE
  END GetJoinStyleProc;

PROCEDURE ReadColor(VAR (*OUT*) color: Color): BOOLEAN =
(* If argument "-1" is a color value, set "color" to that value and return
   TRUE; otherwise, return FALSE. *)
  VAR err := FALSE; p1 := JunoArgs.ReadPair(1, err); BEGIN
    IF NOT err THEN
     TYPECASE p1.cdr OF NULL => | RTVal.Pair (p2) =>
      TYPECASE p2.cdr OF NULL => | RTVal.Pair (p3) =>
       TYPECASE p1.car OF NULL => | RTVal.Number (r) =>
        TYPECASE p2.car OF NULL => | RTVal.Number (g) =>
         TYPECASE p3.car OF NULL => | RTVal.Number (b) =>
          IF p3.cdr = RTVal.nil AND
             0.0 <= r.val AND r.val <= 1.0 AND
             0.0 <= g.val AND g.val <= 1.0 AND
             0.0 <= b.val AND b.val <= 1.0 THEN
            color := Color{r.val, g.val, b.val};
            RETURN TRUE
          END
         ELSE (* SKIP *)
         END
        ELSE (* SKIP *)
        END
       ELSE (* SKIP *)
       END
      ELSE (* SKIP *)
      END
     ELSE (* SKIP *)
     END
    END;
    RETURN FALSE
  END ReadColor;

PROCEDURE SetColorProc(dc: Closure): BOOLEAN =
  BEGIN
    WITH ps = dc.rt.currView.ps, c = ps.color DO
      IF ReadColor(c) THEN
        ps.colorOp := PaintOp.FromRGB(c.r, c.g, c.b,
          mode := PaintOp.Mode.Accurate, bw := PaintOp.BW.UseFg);
        ps.textColorOp := PaintOp.Pair(PaintOp.Transparent, ps.colorOp);
        RETURN TRUE
      END
    END;
    RETURN FALSE
  END SetColorProc;

PROCEDURE SetColorProc2(cl: ToFileClosure): BOOLEAN =
  BEGIN
    WITH c = cl.rt.currView.ps.color DO
      TRY
        IF ReadColor(c) THEN
          WITH wr = cl.i.wr DO
            Wr.PutText(wr, Fmt.Real(c.r)); Wr.PutChar(wr, ' ');
            Wr.PutText(wr, Fmt.Real(c.g)); Wr.PutChar(wr, ' ');
            Wr.PutText(wr, Fmt.Real(c.b));
            Wr.PutText(wr, " setrgbcolor\n")
          END;
          RETURN TRUE
        END
      EXCEPT Wr.Failure => (* SKIP *)
      END
    END;
    RETURN FALSE
  END SetColorProc2;

PROCEDURE GetColorProc(dc: Closure): BOOLEAN =
  BEGIN
    WITH color = dc.rt.currView.ps.color DO
      JunoArgs.WriteValue(1,
        RTVal.FromPair(RTVal.FromReal(color.r),
          RTVal.FromPair(RTVal.FromReal(color.g),
            RTVal.FromPair(RTVal.FromReal(color.b),
              RTVal.nil))))
    END;
    RETURN TRUE
  END GetColorProc;

CONST WindMap = ARRAY [NZWindingVal..OddWindingVal] OF VBT.WindingCondition {
  VBT.WindingCondition.NonZero, VBT.WindingCondition.Odd};

PROCEDURE SetWindingProc(dc: Closure): BOOLEAN =
  VAR err := FALSE; ws := JunoArgs.ReadInt(1, err); BEGIN
    IF NOT err AND NZWindingVal <= ws AND ws <= OddWindingVal THEN
      dc.rt.currView.ps.wind := WindMap[ws];
      RETURN TRUE
    END;
    RETURN FALSE
  END SetWindingProc;

CONST WindMapInv = ARRAY VBT.WindingCondition OF INTEGER {
  OddWindingVal, NZWindingVal};

PROCEDURE GetWindingProc(dc: Closure): BOOLEAN =
  BEGIN
    JunoArgs.WriteInt(1, WindMapInv[dc.rt.currView.ps.wind]);
    RETURN TRUE
  END GetWindingProc;

PROCEDURE GetXInfo(face: TEXT; size: INTEGER): PSFont.XInfo =
  VAR ref: REFANY; BEGIN
    IF NOT fontTbl.get(face & Fmt.Int(size), ref) THEN ref := NIL END;
    RETURN NARROW(ref, PSFont.XInfo)
  END GetXInfo;

PROCEDURE SetFaceProc(dc: Closure): BOOLEAN =
  VAR err := FALSE; nm := JunoArgs.ReadText(1, err); BEGIN
    IF NOT err THEN
      WITH ps = dc.rt.currView.ps DO
	VAR xInfo := GetXInfo(nm, ps.size); BEGIN
          IF xInfo = NIL THEN RETURN FALSE END;
          ps.face := nm; ps.ptSize := xInfo.ptSize;
	  ps.xFont := Font.FromName(ARRAY OF TEXT{xInfo.name});
          RETURN TRUE
	END
      END
    END;
    RETURN FALSE
  END SetFaceProc;

PROCEDURE SetFaceProc2(cl: ToFileClosure): BOOLEAN =
  VAR err := FALSE; nm := JunoArgs.ReadText(1, err); BEGIN
    IF NOT err THEN
      WITH ps = cl.rt.currView.ps, wr = cl.i.wr DO
	VAR ref: REFANY; xInfo := GetXInfo(nm, ps.size); BEGIN
	  IF xInfo = NIL OR NOT metricTbl.get(nm, ref) THEN
            RETURN FALSE
          END;
          ps.face := nm; ps.ptSize := xInfo.ptSize;
	  ps.psMetric := ref;
          TRY
            Wr.PutChar(wr, '/'); Wr.PutText(wr, nm);
            WriteFindFont(wr);
	    Wr.PutText(wr, Fmt.Real(xInfo.ptSize));
            Wr.PutText(wr, " scalefont setfont\n")
          EXCEPT
            Wr.Failure => RETURN FALSE
          END;
          RETURN TRUE
	END
      END
    END;
    RETURN FALSE
  END SetFaceProc2;

PROCEDURE SetSizeProc(dc: Closure): BOOLEAN =
  VAR err := FALSE; sz := JunoArgs.ReadInt(1, err); BEGIN
    IF NOT err AND sz >= 0 THEN
      WITH ps = dc.rt.currView.ps DO
        VAR xInfo := GetXInfo(ps.face, sz); BEGIN
    	  IF xInfo = NIL THEN RETURN FALSE END;
          ps.size := sz; ps.ptSize := xInfo.ptSize;
    	  ps.xFont := Font.FromName(ARRAY OF TEXT{xInfo.name});
          RETURN TRUE
        END
      END
    END;
    RETURN FALSE
  END SetSizeProc;

PROCEDURE SetSizeProc2(cl: ToFileClosure): BOOLEAN =
  VAR err := FALSE; sz := JunoArgs.ReadInt(1, err); BEGIN
    IF NOT err AND sz >= 0 THEN
      WITH ps = cl.rt.currView.ps, wr = cl.i.wr DO
        VAR xInfo := GetXInfo(ps.face, sz); BEGIN
    	  IF xInfo = NIL THEN RETURN FALSE END;
          ps.size := sz; ps.ptSize := xInfo.ptSize;
          TRY
            Wr.PutChar(wr, '/'); Wr.PutText(wr, ps.face);
            WriteFindFont(wr);
            Wr.PutText(wr, Fmt.Real(xInfo.ptSize));
            Wr.PutText(wr, " scalefont setfont\n")
          EXCEPT
            Wr.Failure => RETURN FALSE
          END;
          RETURN TRUE
        END
      END
    END;
    RETURN FALSE
  END SetSizeProc2;

PROCEDURE SetFontProc(dc: Closure): BOOLEAN =
  VAR
    err := FALSE;
    nm := JunoArgs.ReadText(2, err);
    sz := JunoArgs.ReadInt(1, err);
  BEGIN
    IF NOT err AND sz >= 0 THEN
      VAR xInfo := GetXInfo(nm, sz); BEGIN
        IF xInfo = NIL THEN RETURN FALSE END;
        WITH ps = dc.rt.currView.ps DO
          ps.face := nm; ps.size := sz; ps.ptSize := xInfo.ptSize;
	  ps.xFont := Font.FromName(ARRAY OF TEXT{xInfo.name})
	END;
	RETURN TRUE
      END
    END;
    RETURN FALSE
  END SetFontProc;

PROCEDURE SetFontProc2(cl: ToFileClosure): BOOLEAN =
  VAR
    err := FALSE;
    nm := JunoArgs.ReadText(2, err);
    sz := JunoArgs.ReadInt(1, err);
  BEGIN
    IF NOT err AND sz >= 0 THEN
      VAR xInfo := GetXInfo(nm, sz); BEGIN
        IF xInfo = NIL THEN RETURN FALSE END;
        WITH ps = cl.rt.currView.ps, wr = cl.i.wr DO
          VAR ref: REFANY; BEGIN
            IF NOT metricTbl.get(nm, ref) THEN RETURN FALSE END;
            ps.psMetric := ref
	  END;
	  ps.face := nm; ps.size := sz; ps.ptSize := xInfo.ptSize;
          TRY
            Wr.PutChar(wr, '/'); Wr.PutText(wr, nm);
            WriteFindFont(wr);
	    Wr.PutText(wr, Fmt.Real(xInfo.ptSize));
	    Wr.PutText(wr, " scalefont setfont\n")
          EXCEPT
            Wr.Failure => RETURN FALSE
          END;
          RETURN TRUE
        END
      END
    END;
    RETURN FALSE
  END SetFontProc2;

PROCEDURE GetFaceProc(dc: Closure): BOOLEAN =
  BEGIN
    JunoArgs.WriteText(1, dc.rt.currView.ps.face);
    RETURN TRUE
  END GetFaceProc;

PROCEDURE GetSizeProc(dc: Closure): BOOLEAN =
  BEGIN
    JunoArgs.WriteInt(1, dc.rt.currView.ps.size);
    RETURN TRUE
  END GetSizeProc;

PROCEDURE GetFontProc(dc: Closure): BOOLEAN =
  BEGIN
    JunoArgs.WriteText(2, dc.rt.currView.ps.face);
    JunoArgs.WriteInt(1, dc.rt.currView.ps.size);
    RETURN TRUE
  END GetFontProc;

PROCEDURE GetPtSizeProc(dc: Closure): BOOLEAN =
  BEGIN
    JunoArgs.WriteReal(1, dc.rt.currView.ps.ptSize);
    RETURN TRUE
  END GetPtSizeProc;

PROCEDURE FontHProc(dc: Closure): BOOLEAN =
(* Note: We can use a "dummy" string, since we only care about the ascent and
   descent of the font, and "VBT.BoundingBox" returns the same ascent and
   descent regardless of its argument. *)
  BEGIN
    WITH d = dc.rt.currView DO
      VAR
        ch: Drawing.ChildPublic := Filter.Child(d);
        bbox := VBT.BoundingBox(ch, "a", d.ps.xFont);
      BEGIN
        WITH yScale = ch.xform.yScale DO
          JunoArgs.WriteReal(2, -FLOAT(bbox.north, JunoValue.Real) / yScale);
          JunoArgs.WriteReal(1,  FLOAT(bbox.south, JunoValue.Real) / yScale)
        END;
        RETURN TRUE
      END
    END
  END FontHProc;

PROCEDURE FontHProc2(cl: ToFileClosure): BOOLEAN =
  BEGIN
    WITH
      ps = cl.rt.currView.ps,
      sz = ps.ptSize,
      bbox = ps.psMetric.bbox
    DO
      JunoArgs.WriteReal(2,  sz * bbox.north);
      JunoArgs.WriteReal(1, -sz * bbox.south)
    END;
    RETURN TRUE
  END FontHProc2;

PROCEDURE StringWProc(dc: Closure): BOOLEAN =
  VAR err := FALSE; t := JunoArgs.ReadText(1, err); BEGIN
    IF NOT err THEN
      WITH d = dc.rt.currView DO
        VAR
          ch: Drawing.ChildPublic := Filter.Child(d);
          w := VBT.TextWidth(ch, t, d.ps.xFont);
        BEGIN
          JunoArgs.WriteReal(2, FLOAT(w, JunoValue.Real) / ch.xform.xScale)
        END;
        RETURN TRUE
      END
    END;
    RETURN FALSE
  END StringWProc;

PROCEDURE StringWProc2(cl: ToFileClosure): BOOLEAN =
  VAR err := FALSE; t := JunoArgs.ReadText(1, err); BEGIN
    IF NOT err THEN
      WITH metric = cl.rt.currView.ps.psMetric DO
        VAR total: JunoValue.Real := 0.0; BEGIN
          FOR i := 0 TO Text.Length(t) - 1 DO
            VAR code := ORD(Text.GetChar(t, i)); BEGIN
              IF metric.mapped[code] THEN
                total := total + metric.width[code]
              END
            END
          END;
          JunoArgs.WriteReal(2, total * cl.rt.currView.ps.ptSize);
          RETURN TRUE
        END
      END
    END;
    RETURN FALSE
  END StringWProc2;

PROCEDURE StringBBProc(dc: Closure): BOOLEAN =
  VAR err := FALSE; t := JunoArgs.ReadText(1, err); BEGIN
    IF NOT err THEN
      WITH d = dc.rt.currView DO
      	VAR
      	  ch: Drawing.ChildPublic := Filter.Child(d);
          r: Rect.T := VBTExtras.TightBoundingBox(ch, t, d.ps.xFont);
          res: JunoRect.T;
      	BEGIN
          IF r = Rect.Empty THEN
            res := JunoRect.Empty
          ELSE
      	    WITH xScale = ch.xform.xScale, yScale = ch.xform.yScale DO
      	      res := JunoRect.T{
      	  	(FLOAT( r.west,  JunoValue.Real) - 0.49) / xScale,
      	  	(FLOAT( r.east,  JunoValue.Real) - 0.49) / xScale,
      	  	(FLOAT(-r.north, JunoValue.Real) + 0.51) / yScale,
      	  	(FLOAT(-r.south, JunoValue.Real) + 0.51) / yScale}
            END
      	  END;
          JunoArgs.WriteValue(2, JunoRect.ToRTVal(res));
      	  RETURN TRUE
      	END
      END
    END;
    RETURN FALSE
  END StringBBProc;

PROCEDURE StringBBProc2(cl: ToFileClosure): BOOLEAN =
  VAR err := FALSE; t := JunoArgs.ReadText(1, err); BEGIN
    IF NOT err THEN
      WITH ps = cl.rt.currView.ps DO
        VAR res: JunoRect.T; empty := TRUE; refPt := 0.0; BEGIN
          FOR i := 0 TO Text.Length(t) - 1 DO
            VAR code := ORD(Text.GetChar(t, i)); BEGIN
              IF NOT ps.psMetric.mapped[code] THEN
                (* Map unencoded characters to the space character *)
                code := ORD(' ');
                <* ASSERT ps.psMetric.mapped[code] *>
              END;
              VAR
                bbox := ps.psMetric.charBB[code];
                bbox2: JunoRect.T;
              BEGIN
            	IF bbox # NIL THEN
            	  bbox2 := JunoRect.Add(bbox^, JunoPt.T{refPt, 0.0});
            	  IF empty
            	    THEN empty := FALSE; res := bbox2
            	    ELSE res := JunoRect.Join(res, bbox2)
            	  END
            	END;
            	refPt := refPt + ps.psMetric.width[code]
              END
            END
          END;
          IF empty
            THEN res := JunoRect.T{0.0, 0.0, 0.0, 0.0}
            ELSE res := JunoRect.Scale(res, ps.ptSize)
          END;
          JunoArgs.WriteValue(2, JunoRect.ToRTVal(res));
          RETURN TRUE
        END
      END
    END;
    RETURN FALSE
  END StringBBProc2;

PROCEDURE CurrPtProc(dc: Closure): BOOLEAN =
  BEGIN
    WITH ps = dc.rt.currView.ps DO
      IF NOT ps.moveto AND Path.IsClosed(ps.path)
        THEN JunoArgs.WriteValue(1, RTVal.nil)
        ELSE JunoArgs.WriteValue(1, JunoPt.ToValuePair(ps.currPt))
      END
    END;
    RETURN TRUE
  END CurrPtProc;

VAR Nil := RTVal.nil;

TYPE
  JunoMO = Path.MapObject BRANDED "PSImpl.JunoMO" OBJECT
    ch: Drawing.ChildPublic;
    head, curr: RTVal.Pair;
  METHODS
    init(): JunoMO := JunoMOInit
  OVERRIDES
    move := AddMoveTo;
    line := AddLineTo;
    curve := AddCurveTo;
    close := AddClose
  END;

PROCEDURE JunoMOInit(self: JunoMO): JunoMO =
  BEGIN
    self.head := RTVal.FromPair(Nil, Nil);
    self.curr := self.head;
    RETURN self
  END JunoMOInit;

PROCEDURE AddNewList(self: JunoMO; nm: TEXT): RTVal.Pair =
  VAR pr := RTVal.FromPair(RTVal.FromText(nm), Nil); BEGIN
    self.curr.cdr := RTVal.FromPair(pr, Nil);
    self.curr := self.curr.cdr;
    RETURN pr
  END AddNewList;

PROCEDURE AddPt(
    VAR (*INOUT*) pr: RTVal.Pair;
    READONLY pt: Point.T;
    ch: Drawing.ChildPublic) =
  VAR pair := JunoPt.ToValuePair(JunoPt.FromHV(pt, ch.xform)); BEGIN
    pr.cdr := RTVal.FromPair(pair, Nil);
    pr := pr.cdr;
  END AddPt;

PROCEDURE AddMoveTo(self: JunoMO; READONLY pt: Point.T) =
  VAR pr := AddNewList(self, "MoveTo"); BEGIN
    AddPt(pr, pt, self.ch);
    pr.cdr := Nil
  END AddMoveTo;

PROCEDURE AddLineTo(
    self: JunoMO;
    <*UNUSED*> READONLY pt1: Point.T;
    READONLY pt2: Point.T) =
  VAR pr := AddNewList(self, "LineTo"); BEGIN
    AddPt(pr, pt2, self.ch);
    pr.cdr := Nil
  END AddLineTo;

PROCEDURE AddCurveTo(
    self: JunoMO;
    <*UNUSED*> READONLY pt1: Point.T;
    READONLY pt2, pt3, pt4: Point.T)=
  VAR pr := AddNewList(self, "CurveTo"); BEGIN
    AddPt(pr, pt2, self.ch);
    AddPt(pr, pt3, self.ch);
    AddPt(pr, pt4, self.ch);
    pr.cdr := Nil
  END AddCurveTo;

PROCEDURE AddClose(self: JunoMO; <*UNUSED*> READONLY pt1, pt2: Point.T) =
  VAR pr := AddNewList(self, "Close"); BEGIN
    pr.cdr := Nil
  END AddClose;

PROCEDURE CurrPathProc(dc: Closure): BOOLEAN =
  <* FATAL Path.Malformed *>
  VAR jmo: JunoMO; BEGIN
    WITH d = dc.rt.currView, ps = d.ps DO
      jmo := NEW(JunoMO, ch := Filter.Child(d)).init();
      Path.Map(ps.path, jmo);
      IF ps.moveto THEN
        jmo.move(JunoPt.ToHV(ps.movetoPt, jmo.ch.xform))
      END;
      JunoArgs.WriteValue(1, jmo.head.cdr)
    END;
    RETURN TRUE
  END CurrPathProc;

PROCEDURE SetBBoxProc(dc: Closure): BOOLEAN =
  VAR
    err := FALSE;
    pr1 := JunoArgs.ReadPair(2, err);
    pr2 := JunoArgs.ReadPair(1, err);
    pt1, pt2: JunoPt.T;
  BEGIN
    IF NOT err THEN
      TRY
        pt1 := JunoPt.FromValuePair(pr1);
        pt2 := JunoPt.FromValuePair(pr2)
      EXCEPT
        JunoPt.BadPt => RETURN FALSE
      END;
      WITH bbox = dc.rt.currView.ps.bbox DO
        bbox.west := MIN(pt1.x, pt2.x);
        bbox.east := MAX(pt1.x, pt2.x);
        bbox.south := MIN(pt1.y, pt2.y);
        bbox.north := MAX(pt1.y, pt2.y)
      END;
      RETURN TRUE
    END;
    RETURN FALSE
  END SetBBoxProc;

PROCEDURE GetBBoxProc(dc: Closure): BOOLEAN =
  BEGIN
    WITH bbox = dc.rt.currView.ps.bbox DO
      JunoArgs.WriteValue(2, JunoPt.ToValuePair(
        JunoPt.T{x := bbox.west, y := bbox.south}));
      JunoArgs.WriteValue(1, JunoPt.ToValuePair(
        JunoPt.T{x := bbox.east, y := bbox.north}))
    END;
    RETURN TRUE
  END GetBBoxProc;

PROCEDURE ShowPageProc(dc: Closure): BOOLEAN =
  BEGIN
    Drawing.Sync(Filter.Child(dc.rt.currView));
    RETURN TRUE
  END ShowPageProc;

PROCEDURE ShowPageProc2(cl: ToFileClosure): BOOLEAN =
  BEGIN
    INC(cl.i.page);
    TRY
      WITH wr = cl.i.wr DO
        WritePageTrailer(wr);
      	Wr.PutText(wr, "showpage\n");
      	WritePageHeader(wr, cl.i.page)
      END;
      RETURN TRUE
    EXCEPT Wr.Failure => (* SKIP *)
    END;
    RETURN FALSE
  END ShowPageProc2;

PROCEDURE ResetProc(dc: Closure): BOOLEAN =
  VAR d := dc.rt.currView; ch: Drawing.ChildPublic := Filter.Child(d); BEGIN
    VBT.PaintTint(ch, Rect.Full, PaintOp.Bg);
    Reset(d);
    RETURN TRUE
  END ResetProc;

PROCEDURE SavePageProc(dc: Closure): BOOLEAN =
  BEGIN
    DblBufferVBT.Save(Filter.Child(dc.rt.currView));
    RETURN TRUE
  END SavePageProc;

PROCEDURE SavePageProc2(<*UNUSED*> cl: ToFileClosure): BOOLEAN =
  BEGIN RETURN TRUE END SavePageProc2;

PROCEDURE RestorePageProc(dc: Closure): BOOLEAN =
  BEGIN
    DblBufferVBT.Restore(Filter.Child(dc.rt.currView));
    RETURN TRUE
  END RestorePageProc;

PROCEDURE RestorePageProc2(<*UNUSED*> cl: ToFileClosure): BOOLEAN =
  BEGIN RETURN TRUE END RestorePageProc2;

BEGIN
  (* read the font data from the pickle *)
  <* FATAL Rd.Failure, Rd.EndOfFile, Rsrc.NotFound *>
  VAR
    rd := Rsrc.Open("FontData.pkl", JunoRsrc.Path);
    fontData: PSFont.Data;
  BEGIN
    TRY fontData := Pickle.Read(rd) EXCEPT Pickle.Error (msg) =>
      <* FATAL Wr.Failure *> BEGIN
        Wr.PutText(stderr, "Error reading pickled font data: " & msg & "\n");
        Wr.Flush(stderr)
      END;
      Process.Exit(1)
    END;
    Rd.Close(rd);
    fontTbl := fontData.fontTbl;
    metricTbl := fontData.metricTbl
  END;

  (* set defaultXFont *)
  VAR xInfo := GetXInfo(DefaultFaceName, DefaultFontSize); BEGIN
    IF xInfo = NIL THEN RAISE Error END;
    defaultXFont := Font.FromName(ARRAY OF TEXT{xInfo.name});
    defaultXFontPtSize := xInfo.ptSize
  END;

  (* set default metric *)
  VAR ref: REFANY; BEGIN
    IF metricTbl.get(DefaultFaceName, ref)
      THEN defaultPSMetric := ref
      ELSE RAISE Error
    END
  END
END PSImpl.
