(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Aug 22 16:21:08 PDT 1996 by najork                   *)
(*      modified on Tue Jan 31 11:36:29 PST 1995 by kalsow                   *)
(*      modified on Tue May 17 17:38:28 PDT 1994 by mhb                      *)
(*      modified on Tue Jun 16 16:46:31 PDT 1992 by muller                   *)

MODULE Fisheye EXPORTS Main;

IMPORT FisheyeBundle, FileRd, FloatMode, Fmt, Font, FormsVBT,
       GraphData, GraphVBT, Lex, OSError, Process, Rd, Rsrc, Scan, 
       Stdio, Text, Thread, Trestle, TrestleComm, VBT, Wr, XTrestle;

<* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
<* FATAL Rd.Failure, TrestleComm.Failure *>
<* FATAL Wr.Failure, Thread.Alerted *> 
<* FATAL Lex.Error, FloatMode.Trap *>

VAR 
  graph: GraphVBT.T;


<* UNUSED *>
PROCEDURE DoQuit (fv:FormsVBT.T; 
    <* UNUSED *> event:TEXT; 
    <* UNUSED *> cl:REFANY; 
    <* UNUSED *> time:VBT.TimeStamp) =
  BEGIN
    Trestle.Delete(graph);
    Trestle.Delete(fv);
  END DoQuit;

PROCEDURE DoMode (fv:FormsVBT.T;
    <* UNUSED *> event:TEXT; 
    <* UNUSED *> cl:REFANY; 
    <* UNUSED *> time:VBT.TimeStamp) =
  BEGIN
    GraphVBT.SetFisheye(graph, FormsVBT.GetBoolean (fv, "fisheye"))
  END DoMode;

PROCEDURE DoFocus (fv:FormsVBT.T;
    <* UNUSED *> event:TEXT; 
    <* UNUSED *> cl:REFANY; 
    <* UNUSED *> time:VBT.TimeStamp) =
  VAR f: GraphVBT.FocusDevice;
  BEGIN
    IF FormsVBT.GetBoolean (fv, "mouse") THEN
       f := GraphVBT.FocusDevice.Mouse
    ELSE
       f := GraphVBT.FocusDevice.Keyboard
    END;
    GraphVBT.SetFisheyeFocusDevice(graph, f);
  END DoFocus;

PROCEDURE DoDetail (fv:FormsVBT.T;
    <* UNUSED *> event:TEXT; 
    <* UNUSED *> cl:REFANY; 
    <* UNUSED *> time:VBT.TimeStamp) =
  BEGIN
    GraphVBT.SetFisheyeText (graph, FormsVBT.GetBoolean (fv, "on"));
  END DoDetail;

PROCEDURE DoShape (<* UNUSED *> fv:FormsVBT.T;
    <* UNUSED *> event:TEXT; 
    <* UNUSED *> cl:REFANY; 
    <* UNUSED *> time:VBT.TimeStamp) =
  BEGIN
(*
    IF FormsVBT.GetBoolean (fv, "rectangular") THEN 
      GraphVBT.SetShape (graph, GraphVBT.Rectangle);
      GraphVBT.SetStyle (graph, GraphVBT.Style.Border);
    ELSIF FormsVBT.GetBoolean (fv, "open") THEN 
      GraphVBT.SetShape (graph, GraphVBT.Circle);
      GraphVBT.SetStyle (graph, GraphVBT.Style.Border);
    ELSE
      GraphVBT.SetShape (graph, GraphVBT.Circle);
      GraphVBT.SetStyle (graph, GraphVBT.Style.Filled);
    END;
*)
  END DoShape;

PROCEDURE DoStyle (fv:FormsVBT.T;
    <* UNUSED *> event:TEXT; 
    <* UNUSED *> cl:REFANY; 
    <* UNUSED *> time:VBT.TimeStamp) =
  BEGIN
    IF FormsVBT.GetBoolean (fv, "graphical") THEN
      GraphVBT.SetFisheyeType (graph, GraphVBT.FisheyeType.Graphical);
    ELSE
      GraphVBT.SetFisheyeType (graph, GraphVBT.FisheyeType.Semantic);
    END;
  END DoStyle;

PROCEDURE DoMapping (fv:FormsVBT.T;
    <* UNUSED *> event:TEXT; 
    <* UNUSED *> cl:REFANY; 
    <* UNUSED *> time:VBT.TimeStamp) =
  BEGIN
    IF FormsVBT.GetBoolean (fv, "cartesian") THEN
      GraphVBT.SetFisheyeMapType (graph, GraphVBT.FisheyeMapType.Cartesian);
    ELSE
      GraphVBT.SetFisheyeMapType (graph, GraphVBT.FisheyeMapType.Polar);
    END;
  END DoMapping;

PROCEDURE DoFile (             fv   : FormsVBT.T;
                  <* UNUSED *> event: TEXT;
                  <* UNUSED *> cl   : REFANY;
                  <* UNUSED *> time : VBT.TimeStamp) =
  BEGIN
    TRY
      WITH rd = FileRd.Open(FormsVBT.GetText(fv, "file")) DO
        WITH c = FormsVBT.GetChoice(fv, "input") DO
          IF c # NIL THEN FormsVBT.PutBoolean(fv, c, FALSE) END
        END;
        ResetPanel(fv);
        GraphVBT.Setup(graph, GraphData.ReadGraph(rd));
        Rd.Close(rd);
      END;
    EXCEPT
      OSError.E, Rd.Failure => FormsVBT.PutText(fv, "file", "");
    END;
  END DoFile;

PROCEDURE DoNewInput (             fv   : FormsVBT.T;
                                   event: TEXT;
                      <* UNUSED *> cl   : REFANY;
                      <* UNUSED *> time : VBT.TimeStamp) =
  VAR rsrc: TEXT;
  BEGIN
    IF Text.Equal(event, "us") THEN
      rsrc := "USCitiesData"
    ELSIF Text.Equal(event, "metro1") THEN
      rsrc := "ParisMetroData1"
    ELSIF Text.Equal(event, "metro2") THEN
      rsrc := "ParisMetroData2"
    ELSIF Text.Equal(event, "cra") THEN
      rsrc := "CRAData"
(*
    ELSIF Text.Equal(event, "src2") THEN
      rsrc := "SRC2Data"
    ELSIF Text.Equal(event, "src3") THEN
      rsrc := "SRC3Data"
    ELSIF Text.Equal(event, "src4") THEN
      rsrc := "SRC4Data"
*)
    ELSIF Text.Equal(event, "dag") THEN
      rsrc := "DagData"
    ELSIF Text.Equal(event, "sym") THEN
      rsrc := "Sym15x15Data"
    END;

    TRY
      WITH rd = Rsrc.Open(rsrc, path) DO
        ResetPanel(fv);
        GraphVBT.Setup(graph, GraphData.ReadGraph(rd));
        Rd.Close(rd);
      END;
    EXCEPT
      Rsrc.NotFound => Gripe("cannot open resource <" & rsrc & ">")
    END;

  END DoNewInput;


PROCEDURE DoVSize (fv:FormsVBT.T; event:TEXT; 
    <* UNUSED *> cl:REFANY; 
    <* UNUSED *> time:VBT.TimeStamp) =
  CONST Max = 10.0; Min = 0.0;
  BEGIN
    WITH val = GetNum (fv, event, Min, Max) DO
      GraphVBT.SetFisheyeSizeFactor(graph, val);
      Update(fv);
    END;
  END DoVSize;

PROCEDURE DoDistortion (fv:FormsVBT.T; event:TEXT; 
    <* UNUSED *> cl:REFANY; 
    <* UNUSED *> time:VBT.TimeStamp) =
  CONST Max = 20.0; Min = 0.0;
  BEGIN
    WITH val = GetNum (fv, event, Min, Max) DO
      GraphVBT.SetFisheyeDistortion(graph, val);
      Update(fv);
    END
  END DoDistortion;

PROCEDURE DoVWCutoff (fv:FormsVBT.T; event:TEXT; 
    <* UNUSED *> cl:REFANY; 
    <* UNUSED *> time:VBT.TimeStamp) =
  CONST Max = 1.0; Min = 0.0;
  BEGIN
    WITH val = GetNum (fv, event, Min, Max) DO
      GraphVBT.SetFisheyeVWThreshold(graph, val);
      Update(fv);
    END
  END DoVWCutoff;

PROCEDURE DoExp (fv:FormsVBT.T; event:TEXT; 
    <* UNUSED *> cl:REFANY; 
    <* UNUSED *> time:VBT.TimeStamp) =
  CONST Max = 4.0; Min = 0.00001;
  BEGIN
    WITH val = GetNum (fv, event, Min, Max) DO
      GraphVBT.SetFisheyeSizeAPIPower(graph, val);
      Update(fv);
    END
  END DoExp;

PROCEDURE DoCoef (fv:FormsVBT.T; event:TEXT; 
    <* UNUSED *> cl:REFANY; 
    <* UNUSED *> time:VBT.TimeStamp) =
  CONST Max = 4.0; Min = 0.00001;
  BEGIN
    WITH val = GetNum (fv, event, Min, Max) DO
      GraphVBT.SetFisheyeSizeAPICoeff(graph, val);
      Update(fv);
    END
  END DoCoef;


PROCEDURE DoSemanticColor (fv:FormsVBT.T; 
    <* UNUSED *> event:TEXT; 
    <* UNUSED *> cl:REFANY; 
    <* UNUSED *> time:VBT.TimeStamp) =
  BEGIN
    GraphVBT.SetSemanticColor(graph, FormsVBT.GetBoolean(fv, "color"))
  END DoSemanticColor;

PROCEDURE DoNodeColor (fv:FormsVBT.T; 
    <* UNUSED *> event:TEXT; 
    <* UNUSED *> cl:REFANY; 
    <* UNUSED *> time:VBT.TimeStamp) =
  BEGIN
    GraphVBT.SetNodeColor(graph,
      GraphVBT.GetColorFromName(FormsVBT.GetText(fv, "nodeColor")));
  END DoNodeColor;

PROCEDURE DoNodeInteriorColor (fv:FormsVBT.T; 
    <* UNUSED *> event:TEXT; 
    <* UNUSED *> cl:REFANY; 
    <* UNUSED *> time:VBT.TimeStamp) =
  BEGIN
    GraphVBT.SetNodeInteriorColor(graph, 
      GraphVBT.GetColorFromName(FormsVBT.GetText(fv, "nodeInteriorColor")));
  END DoNodeInteriorColor;

PROCEDURE DoFocusColor (fv:FormsVBT.T; 
    <* UNUSED *> event:TEXT; 
    <* UNUSED *> cl:REFANY; 
    <* UNUSED *> time:VBT.TimeStamp) =
  BEGIN
    GraphVBT.SetFocusColor(graph, 
      GraphVBT.GetColorFromName(FormsVBT.GetText(fv, "focusColor")));
  END DoFocusColor;


PROCEDURE DoEdgeColor (fv:FormsVBT.T; 
    <* UNUSED *> event:TEXT;
    <* UNUSED *> cl:REFANY; 
    <* UNUSED *> time:VBT.TimeStamp) =
  BEGIN
    GraphVBT.SetLinkColor(graph, 
      GraphVBT.GetColorFromName(FormsVBT.GetText(fv, "edgeColor")));
  END DoEdgeColor;

PROCEDURE DoTextColor (fv:FormsVBT.T; 
    <* UNUSED *> event:TEXT; 
    <* UNUSED *> cl:REFANY; 
    <* UNUSED *> time:VBT.TimeStamp) =
  BEGIN
    GraphVBT.SetLabelColor(graph, 
      GraphVBT.GetColorFromName(FormsVBT.GetText(fv, "textColor")));
  END DoTextColor;

PROCEDURE DoTextFont (fv:FormsVBT.T; 
    <* UNUSED *> event:TEXT; 
    <* UNUSED *> cl:REFANY; 
    <* UNUSED *> time:VBT.TimeStamp) =
  BEGIN
    GraphVBT.SetFont(graph, 
      Font.FromName(ARRAY OF TEXT{FormsVBT.GetText(fv, "textFont")}))
  END DoTextFont;

PROCEDURE DoEdgeThickness (fv:FormsVBT.T; 
    <* UNUSED *> event:TEXT; 
    <* UNUSED *> cl:REFANY; 
    <* UNUSED *> time:VBT.TimeStamp) =
  BEGIN
    GraphVBT.SetLinkThickness(graph, 
	FormsVBT.GetInteger(fv, "edgeThickness"));
  END DoEdgeThickness;

PROCEDURE GetNum (fv: FormsVBT.T; event: TEXT; min, max: REAL): REAL =
  VAR val: REAL;
  BEGIN
    IF Text.FindChar (event, 'S') # -1 THEN
      WITH pct = 0.01 * FLOAT(FormsVBT.GetInteger (fv, event)) DO
        val := (max - min) * pct + min
      END;
      WITH textField = Text.Sub(event, 0, Text.FindChar (event, 'S')) DO
        FormsVBT.PutText (fv, textField, Fmt.Real(val))
      END;
    ELSE
      val := Scan.Real(FormsVBT.GetText(fv, event));
    END;
    RETURN val;
  END GetNum;

PROCEDURE Update(<* UNUSED *> fv: FormsVBT.T) =
  BEGIN
(*
    WITH sz = GraphVBT.GetFisheyeFocusSize(graph) DO
      FormsVBT.PutText (fv, "fsize", Fmt.Int(sz))
    END;
*)
  END Update;

PROCEDURE ResetPanel (fv: FormsVBT.T) =
  BEGIN
    FormsVBT.PutText(fv, "vsize", "1.667");
    FormsVBT.PutInteger (fv, "vsizeScroll", 16);

    FormsVBT.PutText(fv, "distortion", "0");
    FormsVBT.PutInteger(fv, "distortionScroll", 0);

    FormsVBT.PutText(fv, "exp", "0");
    FormsVBT.PutInteger(fv, "expScroll", 0);

    FormsVBT.PutText(fv, "coef", "0");
    FormsVBT.PutInteger(fv, "coefScroll", 0);

    FormsVBT.PutText(fv, "vw", "0");
    FormsVBT.PutInteger(fv, "vwScroll", 0);

    FormsVBT.MakeEvent(fv, "vsize", 0);
    FormsVBT.MakeEvent(fv, "distortion", 0);
    FormsVBT.MakeEvent(fv, "exp", 0);
    FormsVBT.MakeEvent(fv, "coef", 0);
    FormsVBT.MakeEvent(fv, "vw", 0);
   END ResetPanel;

PROCEDURE Attach(
    fv: FormsVBT.T; 
    proc: FormsVBT.Proc; 
    w1, w2, w3, w4: TEXT := NIL) =
  BEGIN
    IF w1 # NIL THEN FormsVBT.AttachProc(fv, w1, proc) END;
    IF w2 # NIL THEN FormsVBT.AttachProc(fv, w2, proc) END;
    IF w3 # NIL THEN FormsVBT.AttachProc(fv, w3, proc) END;
    IF w4 # NIL THEN FormsVBT.AttachProc(fv, w4, proc) END;
  END Attach;

PROCEDURE Gripe (msg: TEXT) =
  BEGIN
    Wr.PutText(Stdio.stderr, msg & "\n");
    Process.Exit();
  END Gripe;

VAR
  panel: FormsVBT.T;

VAR
  path := Rsrc.BuildPath("$FISHEYEPATH", FisheyeBundle.Get());

BEGIN
  graph := GraphVBT.New();
  TRY
    XTrestle.Install(graph, "Fisheye Viewer")
  EXCEPT
    XTrestle.Error => Gripe("cannot install fisheye viewer");
  END;

  TRY
    panel := NEW(FormsVBT.T).initFromRsrc("panel.fv", path)
  EXCEPT
    Rsrc.NotFound => Gripe("cannot open resource <panel.fv>")
  END;

(*
  Attach(panel, DoShape, "rectangular", "open", "filled");
  Attach(panel, DoQuit, "quit");
*)
  Attach(panel, DoStyle, "graphical", "semantic");
  Attach(panel, DoMode, "fisheye", "normal");
  Attach(panel, DoFocus, "mouse", "keyboard");
  Attach(panel, DoDetail, "on");
  Attach(panel, DoMapping, "cartesian", "polar");
  Attach(panel, DoFile, "file");
  Attach(panel, DoNewInput, "us", "metro1", "metro2");
  Attach(panel, DoNewInput, "dag", "sym", "cra");
  Attach(panel, DoSemanticColor, "color");
  Attach(panel, DoVSize, "vsize", "vsizeScroll");
  Attach(panel, DoDistortion, "distortion", "distortionScroll");
  Attach(panel, DoVWCutoff, "vw", "vwScroll");
  Attach(panel, DoExp, "exp", "expScroll");
  Attach(panel, DoCoef, "coef", "coefScroll");
  Attach(panel, DoNodeColor, "nodeColor");
  Attach(panel, DoNodeInteriorColor, "nodeInteriorColor");
  Attach(panel, DoFocusColor, "focusColor");
  Attach(panel, DoEdgeColor, "edgeColor");
  Attach(panel, DoTextColor, "textColor");
  Attach(panel, DoTextFont, "textFont");
  Attach(panel, DoEdgeThickness, "edgeThickness");
  DoMode(panel, NIL, NIL, 0);
  DoFocus(panel, NIL, NIL, 0);
  DoDetail(panel, NIL, NIL, 0);
  DoShape(panel, NIL, NIL, 0);
  DoStyle(panel, NIL, NIL, 0);
  DoMapping(panel, NIL, NIL, 0);
  DoTextFont(panel, NIL, NIL, 0);
  ResetPanel (panel);
  Trestle.Install(panel, "Fisheye Panel");
  DoNewInput (panel, "us", NIL, 0);

  (* Really: Trestle.AwaitDelete on panel or viewer; then,
     Trestle.Delete the other *)
  Trestle.AwaitDelete(panel);
  Trestle.Delete(graph);

END Fisheye.

