(* Copyright 1991 Digital Equipment Corporation.             *)
(* Distributed only by permission.                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Dec 21 13:46:48 PST 1992 by mhb      *)
(*      modified on Fri Apr 24 08:51:31 1992 by msm      *)
(*      modified on Sat Apr  4 19:45:04 PST 1992 by gnelson  *)

<*PRAGMA LL*>

MODULE Track EXPORTS Main;

IMPORT VBT, Trestle, TextVBT, Fmt, Wr, Stdio;

TYPE
  TrackVBT = TextVBT.T OBJECT 
  METHODS
    init(): TrackVBT := Init
  OVERRIDES
    position := Position;
    mouse := Mouse;
  END;
  
PROCEDURE Init(v: TrackVBT): TrackVBT =
  BEGIN
    EVAL TextVBT.T.init(v, "Cursor gone");
    RETURN v
  END Init;

PROCEDURE Out (v: TrackVBT; msg: TEXT) =
  BEGIN
    TextVBT.Put(v, msg);
    (* Wr.PutText(Stdio.stderr, msg & "\n") *)
  END Out;

PROCEDURE Mouse (v: TrackVBT; READONLY cd: VBT.MouseRec) =
  BEGIN
    IF cd.clickType = VBT.ClickType.FirstDown THEN
      Out(v, "FirstDown")
    ELSIF cd.clickType = VBT.ClickType.LastUp THEN
      Out(v, "LastUp");
      VBT.SetCage(v, VBT.EverywhereCage)
    ELSE
      Out(v, "Mouse")
    END;
    TextVBT.T.mouse(v, cd)
  END Mouse;

PROCEDURE Position (v: TrackVBT; READONLY cd: VBT.PositionRec) =
  BEGIN
    IF cd.cp.gone THEN
      Out(v, Fmt.F("GONE ... Cursor (h, v) = (%s, %s)",
                   Fmt.Int(cd.cp.pt.h), Fmt.Int(cd.cp.pt.v)));
    ELSE
      Out(v, Fmt.F("Cursor (h, v) = (%s, %s)",
                   Fmt.Int(cd.cp.pt.h), Fmt.Int(cd.cp.pt.v)));
    END;
    VBT.SetCage(v, VBT.CageFromPosition(cd.cp))
  END Position;

VAR v := NEW(TrackVBT).init(); BEGIN
  Trestle.Install(v);
  Trestle.AwaitDelete(v)
END Track.
