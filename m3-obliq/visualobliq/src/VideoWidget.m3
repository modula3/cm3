(* Copyright (C) 1994, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Wed Feb  1 09:44:59 PST 1995 by kalsow *)
(*      modified on Wed Aug 24 16:33:48 PDT 1994 by bharat *)

<* PRAGMA LL *>

MODULE VideoWidget;

IMPORT Fmt, FormsVBT,  JVSink, Jva,  NodeVBT, Text, Rd, Wr, RW;

REVEAL
  VideoNode = NodeVBT.Widget BRANDED "VO-VideoNode" OBJECT

    (* Video Parameters *)
    vidSource : TEXT := "castle";
    vidQuality : INTEGER := 7;
    vidWidth   : INTEGER := 200;
    vidColours : INTEGER := 50;
    vidHeight  : INTEGER := 150;
    vidMSecs  : INTEGER := 100;
    vidSynchronous : BOOLEAN  := FALSE;
    vidFixedSize : BOOLEAN := FALSE;
    vidPaused : BOOLEAN := FALSE;
      
    (* Audio Parameters *)
    auValue : TEXT := "castle";
    auVolume : INTEGER := 0;
    auMute : BOOLEAN := FALSE;
    auIgnoreMapping : BOOLEAN := FALSE;

    

   OVERRIDES
    loadAttributes  := VidLoadAttributes;
    applyAttributes := VidApplyAttributes;
    computeSX       := VidComputeSX;
    save                       := VidSave;
    load                       := VidLoad;
    initObliqAttrs       := VidObAttrs;
  END;

  
<* FATAL FormsVBT.Error,FormsVBT.Unimplemented *>

PROCEDURE VideoConstructor (): NodeVBT.T =
  BEGIN
    RETURN NEW(VideoNode, 
               BgColor := "Grey35",
               (* Default backdrop when no video is on *)
               FgColor := "Black", 
               Rim := 0,
               Border := 0, Font := "-*-helvetica-bold-*R-*120-*",
               width := 200, height := 150,
               Embellishment := "Flat");
  END VideoConstructor;



(* VideoNode support procs *)

PROCEDURE VidLoadAttributes (nv: VideoNode; as: FormsVBT.T) =
  BEGIN

    (* Load video attributes into attribute sheet widgets *)
    FormsVBT.PutText(as, "vidSource", nv.vidSource, FALSE);
    FormsVBT.PutInteger(as, "vidQuality", nv.vidQuality);
    FormsVBT.PutInteger(as, "vidWidth", nv.vidWidth);
    FormsVBT.PutInteger(as, "vidColours", nv.vidColours);
    FormsVBT.PutInteger(as, "vidHeight", nv.vidHeight);
    FormsVBT.PutInteger(as, "vidMSecs", nv.vidMSecs);
 
    FormsVBT.PutBoolean(as, "vidSynchronous", nv.vidSynchronous);
    FormsVBT.PutBoolean(as, "vidFixedSize", nv.vidFixedSize);
    FormsVBT.PutBoolean(as, "vidPaused", nv.vidPaused);
   
    (* Load audio attributes into attribute sheet widgets *)
    FormsVBT.PutText(as, "auValue", nv.auValue, FALSE);
    FormsVBT.PutInteger(as, "auVolume", nv.auVolume);
    FormsVBT.PutBoolean(as, "auMute", nv.auMute);
    FormsVBT.PutBoolean(as, "auIgnoreMapping", nv.auIgnoreMapping);

    (* Set the ranges of numerics so that invalid data is not entered *)
    FormsVBT.PutIntegerProperty(as, "vidQuality", "Min", FIRST(JVSink.Quality));
    FormsVBT.PutIntegerProperty(as, "vidQuality", "Max", LAST(JVSink.Quality));
    FormsVBT.PutIntegerProperty(as, "auVolume", "Min", FIRST(Jva.Volume));
    FormsVBT.PutIntegerProperty(as, "vidQuality", "Max", LAST(Jva.Volume));

    (* Load attributes for super-type *)
    NodeVBT.T.loadAttributes(nv, as);

  END VidLoadAttributes;

PROCEDURE VidApplyAttributes (nv: VideoNode; as: FormsVBT.T) =
  BEGIN

    (* Load attributes for super type *)
    NodeVBT.T.applyAttributes(nv, as);

    nv.vidSource := FormsVBT.GetText(as, "vidSource");
    nv.vidQuality := FormsVBT.GetInteger(as, "vidQuality");
    nv.vidWidth := FormsVBT.GetInteger(as, "vidWidth");
    nv.vidColours := FormsVBT.GetInteger(as, "vidColours");
    nv.vidHeight := FormsVBT.GetInteger(as, "vidHeight");
    nv.vidMSecs := FormsVBT.GetInteger(as, "vidMSecs");
    nv.vidSynchronous := FormsVBT.GetBoolean(as, "vidSynchronous");
    nv.vidFixedSize := FormsVBT.GetBoolean(as, "vidFixedSize");
    nv.vidPaused := FormsVBT.GetBoolean(as, "vidPaused");
    
    nv.auValue := FormsVBT.GetText(as, "auValue");
    nv.auVolume := FormsVBT.GetInteger(as, "auVolume");
    nv.auMute := FormsVBT.GetBoolean(as, "auMute");
    nv.auIgnoreMapping := FormsVBT.GetBoolean(as,"auIgnoreMapping");

  END VidApplyAttributes;


PROCEDURE VidComputeSX (nv: VideoNode; Final: BOOLEAN := FALSE): TEXT =
  BEGIN

    IF Text.Equal(nv.vidSource, "") THEN
      nv.DialogSX := NodeVBT.FindAndReplace(nv.DialogSX, "PlayVideo", "1");
    ELSE
      nv.DialogSX := NodeVBT.FindAndReplace(nv.DialogSX, "PlayVideo", "0");
    END;

    nv.DialogSX := NodeVBT.FindAndReplace(nv.DialogSX, "VideoSource", nv.vidSource);
    nv.DialogSX := NodeVBT.FindAndReplace(nv.DialogSX, "Quality", Fmt.Int(nv.vidQuality));
    nv.DialogSX := NodeVBT.FindAndReplace(nv.DialogSX, "Width", Fmt.Int(nv.vidWidth));
    nv.DialogSX := NodeVBT.FindAndReplace(nv.DialogSX, "Colours", Fmt.Int(nv.vidColours));
    nv.DialogSX := NodeVBT.FindAndReplace(nv.DialogSX, "Height", Fmt.Int(nv.vidHeight));
    nv.DialogSX := NodeVBT.FindAndReplace(nv.DialogSX, "MSecs", Fmt.Int(nv.vidMSecs));
   IF nv.vidSynchronous THEN
     nv.DialogSX := NodeVBT.FindAndReplace(nv.DialogSX, "Synchronous", "Synchronous");
   ELSE
      nv.DialogSX := NodeVBT.FindAndReplace(nv.DialogSX, "Synchronous", "");
   END;
   IF nv.vidFixedSize THEN
     nv.DialogSX := NodeVBT.FindAndReplace(nv.DialogSX, "FixedSize", "FixedSize");
   ELSE
      nv.DialogSX := NodeVBT.FindAndReplace(nv.DialogSX, "FixedSize", "");
   END;
   IF nv.vidPaused THEN
     nv.DialogSX := NodeVBT.FindAndReplace(nv.DialogSX, "Paused", "Paused");
   ELSE
      nv.DialogSX := NodeVBT.FindAndReplace(nv.DialogSX, "Paused", "");
   END;

    nv.DialogSX := NodeVBT.FindAndReplace(nv.DialogSX, "AudioSource", nv.auValue);
    nv.DialogSX := NodeVBT.FindAndReplace(nv.DialogSX, "Volume", Fmt.Int(nv.auVolume));
    IF nv.auMute THEN
     nv.DialogSX := NodeVBT.FindAndReplace(nv.DialogSX, "Mute", "Mute");
   ELSE
      nv.DialogSX := NodeVBT.FindAndReplace(nv.DialogSX, "Mute", "");
   END;

   IF nv.auIgnoreMapping THEN
     nv.DialogSX := NodeVBT.FindAndReplace(nv.DialogSX, "IgnoreMapping", "IgnoreMapping");
   ELSE
      nv.DialogSX := NodeVBT.FindAndReplace(nv.DialogSX, "IgnoreMapping", "");
   END;
   RETURN NodeVBT.T.computeSX(nv, Final);
  END VidComputeSX;


PROCEDURE VidSave (nv: VideoNode; fv: FormsVBT.T; s: Wr.T) =
  BEGIN
    NodeVBT.T.save(nv, fv, s);
    RW.wtext(s, nv.vidSource);
    RW.wint(s, nv.vidQuality);
    RW.wint(s, nv.vidWidth);
    RW.wint(s, nv.vidColours);
    RW.wint(s, nv.vidHeight); 
    RW.wint(s, nv.vidMSecs);
    RW.wbool(s, nv.vidSynchronous);
    RW.wbool(s, nv.vidFixedSize);
    RW.wbool(s, nv.vidPaused);

    RW.wtext(s, nv.auValue);
    RW.wint(s, nv.auVolume);
    RW.wbool(s, nv.auMute);
    RW.wbool(s, nv.auIgnoreMapping);

  END VidSave;

PROCEDURE VidLoad (nv: VideoNode ; fv: FormsVBT.T; s: Rd.T) =
  BEGIN
    NodeVBT.T.load(nv, fv, s);

    RW.rtext(s, nv.vidSource);
    RW.rint(s, nv.vidQuality);
    RW.rint(s, nv.vidWidth);
    RW.rint(s, nv.vidColours);
    RW.rint(s, nv.vidHeight); 
    RW.rint(s, nv.vidMSecs);
    RW.rbool(s, nv.vidSynchronous);
    RW.rbool(s, nv.vidFixedSize);
    RW.rbool(s, nv.vidPaused);

    RW.rtext(s, nv.auValue);
    RW.rint(s, nv.auVolume);
    RW.rbool(s, nv.auMute);
    RW.rbool(s, nv.auIgnoreMapping);
  END VidLoad;

PROCEDURE VidObAttrs (nv: VideoNode) : TEXT =
VAR code := "";
  BEGIN
    code :=  NodeVBT.TextAttr("Source", nv.vidSource) &
                NodeVBT.IntAttr("Quality", nv.vidQuality) &
                NodeVBT.IntAttr("Width", nv.vidWidth) &
                NodeVBT.IntAttr("Height", nv.vidHeight) &
                NodeVBT.IntAttr("Colours", nv.vidColours) &
                NodeVBT.IntAttr("MSecs", nv.vidMSecs) &
                NodeVBT.BoolAttr("Synchronous", nv.vidSynchronous) &
                NodeVBT.BoolAttr("FixedSize", nv.vidFixedSize) &
                NodeVBT.BoolAttr("Paused", nv.vidPaused) &
                NodeVBT.TextAttr("AuSource", nv.auValue) &
                NodeVBT.IntAttr("Volume", nv.auVolume) &
                NodeVBT.BoolAttr("Mute", nv.auMute) &
                NodeVBT.BoolAttr("IgnoreMapping", nv.auIgnoreMapping);
    RETURN NodeVBT.T.initObliqAttrs(nv) & code ;
  END VidObAttrs; 

PROCEDURE Initialize () =
  BEGIN
    EVAL NodeVBT.Register("video", VideoConstructor);
  END Initialize;

BEGIN

END VideoWidget.





