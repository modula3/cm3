(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Freeman *)
(* Last modified on Thu Sep  7 09:50:39 PDT 1995 by najork   *)
(*      modified on Wed Mar 22 18:21:09 PST 1995 by msm      *)
(*      modified on Mon Oct 25 13:52:17 PDT 1993 by sfreeman *)

MODULE VideoVBT;

IMPORT Axis, IO, JVDecomp, JVSink, Rect, Region, Stdio, VBT, VBTClass;

REVEAL
  T = Public BRANDED OBJECT
        width      : INTEGER;
        height     : INTEGER;
        fixedSize  : BOOLEAN;
        statistics : JVDecomp.Statistics := NIL;
      OVERRIDES
        init     := Init;
        repaint  := Repaint;
        reshape  := Reshape;
        shape    := Shape;

        setQuality       := SetQuality;
        setSize          := SetSize;
        setMinFrameMSecs := SetMinFrameMSecs;
        setSynchronous   := SetSynchronous;
        setFixedSize     := SetFixedSize;

        getDecomp  := GetDecomp;
        getSize    := GetSize;
        setPaused  := SetPaused;
        startStats := StartStats;
        stopStats  := StopStats;
        getStats   := GetStats;
      END;

PROCEDURE Init (           v            : T;
                <*UNUSED*> sourceHost   : TEXT;
                <*UNUSED*> quality      : JVSink.Quality;
                <*UNUSED*> ncolours     : CARDINAL;
                           width        : CARDINAL;
                           height       : CARDINAL;
                <*UNUSED*> synchronous  : BOOLEAN;
                           fixedSize    : BOOLEAN;
                <*UNUSED*> minFrameMSecs: CARDINAL): T =
  BEGIN
    v.width     := width;
    v.height    := height;
    v.fixedSize := fixedSize;

    RETURN v;
  END Init;

PROCEDURE Repaint (<*UNUSED*> v: T; <*UNUSED*> READONLY rgn: Region.T) =
  BEGIN
  END Repaint;

PROCEDURE Reshape (v: T; READONLY cd: VBT.ReshapeRec) =
  BEGIN
    IF Rect.Congruent(cd.prev, cd.new) OR v.st = NIL THEN
      (* there's no video work to do *)
      VBT.Leaf.reshape(v, cd);
      RETURN;
    END;

    IF cd.new = Rect.Empty THEN
    ELSE
      LOCK v DO
        IF NOT v.fixedSize THEN
          v.width  := Rect.HorSize(cd.new);
          v.height := Rect.VerSize(cd.new);
        END;
      END;
      Start(v);
    END;
    VBT.Leaf.reshape(v, cd);
  END Reshape;

PROCEDURE Shape (v: T; ax: Axis.T; <*UNUSED*> n: CARDINAL): VBT.SizeRange =
  VAR res := VBT.DefaultShape;
  BEGIN
    IF v.st # NIL THEN
      LOCK v DO
        IF v.fixedSize THEN
          CASE ax OF
          | Axis.T.Hor => res.pref := v.width;
          | Axis.T.Ver => res.pref := v.height;
          END;
        END;
      END;
    END;
    RETURN res;
  END Shape;

PROCEDURE SetQuality (v: T; <*UNUSED*> quality: JVSink.Quality) =
  BEGIN
    SetPictureParams (v);
  END SetQuality;

PROCEDURE SetSize (v: T; <*UNUSED*> width, height: CARDINAL) =
  BEGIN
    SetPictureParams (v);
  END SetSize;

PROCEDURE SetMinFrameMSecs (<*UNUSED*> v: T; <*UNUSED*> msecs: CARDINAL) =
  BEGIN
  END SetMinFrameMSecs;

PROCEDURE SetSynchronous (<*UNUSED*> v: T; <*UNUSED*> synchronous: BOOLEAN) =
  BEGIN
  END SetSynchronous;

PROCEDURE SetFixedSize (v: T; fixedSize: BOOLEAN) =
  BEGIN
    LOCK v DO v.fixedSize := fixedSize; END;
    VBT.NewShape(v);
  END SetFixedSize;

PROCEDURE GetDecomp (<*UNUSED*> v: T): JVDecomp.T =
  BEGIN
    RETURN NIL;
  END GetDecomp;

PROCEDURE GetSize (v: T; VAR width, height: CARDINAL) =
  BEGIN
    LOCK v DO 
      width  := v.width; 
      height := v.height; 
    END;
  END GetSize;

PROCEDURE SetPaused (<*UNUSED*> v: T; <*UNUSED*> paused : BOOLEAN) =
  BEGIN
  END SetPaused;

PROCEDURE StartStats (t: T) =
  BEGIN
    LOCK t DO
      IF t.statistics = NIL THEN
        t.statistics := NEW (JVDecomp.Statistics);
      END;
      WITH s = t.statistics DO
        s.framesStarted := 0;
        s.framesProcessed := 0;
        s.timesBlocked := 0;
        s.cumLatency := 0;
      END;
    END;
  END StartStats;

PROCEDURE StopStats (t: T) =
  BEGIN
    LOCK t DO 
      t.statistics := NIL; 
    END;
  END StopStats;

PROCEDURE GetStats (t: T): JVDecomp.Statistics =
  BEGIN
    RETURN t.statistics;
  END GetStats;

PROCEDURE SetPictureParams (v : T) =
  BEGIN
    IF NOT v.fixedSize THEN
      VBT.NewShape(v);
    ELSE
      Start(v);
    END;
  END SetPictureParams;

PROCEDURE Start (<*UNUSED*> v: T) =
  BEGIN
    IO.Put ("JVideo not supported\n", Stdio.stderr);
  END Start;


BEGIN
END VideoVBT.
