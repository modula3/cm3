(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Wed Oct  6 16:10:52 PDT 1993 by mhb      *)
(*      modified on Thu Feb 11 16:15:41 PST 1993 by johnh    *)
(*      modified on Wed Aug 19 17:25:06 PDT 1992 by sclafani *)

MODULE ZeusCodeView;
<* PRAGMA LL *>

IMPORT Algorithm, AlgorithmClass, CodeView, Rd, Thread,
       VBT, View, ViewClass, Wr, Zeus, ZeusClass;

REVEAL View.T <: ViewClass.TT;

REVEAL
  T = Public BRANDED OBJECT
        tc: CARDINAL;
      OVERRIDES
        isCompat := IsCompat;
        startrun := Startrun;
      END;

PROCEDURE IsCompat (v: T; alg: ZeusClass.T): BOOLEAN =
  <* LL = arbitrary *>
  BEGIN
    RETURN v.tc = TYPECODE(alg)
  END IsCompat;

PROCEDURE Startrun (v: T) =
  <* LL = {} *>
  BEGIN
    LOCK VBT.mu DO v.cv.exitAll () END;
    View.T.startrun (v);
  END Startrun;

PROCEDURE New (name   : TEXT;
               source : Rd.T;
               alg    : Algorithm.T;
               errorWr: Wr.T := NIL;
               READONLY fontName := CodeView.DefaultFont;
               paneOffset: CARDINAL := 20;
               background: VBT.T    := NIL ): T =
  <* LL = VBT.mu *>
  (* Creates and returns an initialized T with the given name,
     using CodeView.New with the trailing arguments to create the
     cv field.  The alg argument is the algorithm for which this
     is a code view. *)
  VAR
    v := NEW(T, name := name,
             cv := CodeView.New(source, errorWr, fontName,
                                paneOffset, background),
             tc := TYPECODE(alg));
  BEGIN
    RETURN v.init(v.cv);
  END New;

PROCEDURE Enter (alg: Algorithm.T; procedureName: TEXT; pauseTime := -1)
  RAISES {Thread.Alerted} =
  <* LL = {} *>
  BEGIN
    Event (alg, 0, pauseTime, procedureName);
  END Enter;

PROCEDURE Exit (alg: Algorithm.T; pauseTime := -1)
  RAISES {Thread.Alerted} =
  <* LL = {} *>
  BEGIN
    Event (alg, -1, pauseTime, NIL);
  END Exit;

PROCEDURE At (alg: Algorithm.T; highlight := 0; pauseTime := -1)
  RAISES {Thread.Alerted} =
  <* LL = {} *>
  BEGIN
    Event (alg, highlight, pauseTime, NIL);
  END At;

PROCEDURE Event (initiator    : Algorithm.T;
                 highlight                    := 0;
                 pauseTime                    := -1;
                 procedureName: TEXT          := NIL )
  RAISES {Thread.Alerted} =
  <* LL = {} *>
  <* FATAL Zeus.Error, Zeus.Locked *>
  (* Call this to create a code view event for alg "initiator". *)
  VAR
    r := NEW (Arg, highlight := highlight, pauseTime := pauseTime,
              procedureName := procedureName);
  BEGIN
    initiator.stopAtEvent := initiator.stopatCodeEvents;
    initiator.waitAtEvent := initiator.waitatCodeEvents;
    (* Arguably, waitAtEvent should always be 0, and panel.delayTime should
       control the pause. *)
    IF initiator.waitAtEvent = 0 THEN r.pauseTime := 0 END;
    Zeus.Dispatch (initiator, Zeus.EventStyle.Code, 1, "CodeView", NIL, r);
  END Event;

BEGIN
END ZeusCodeView.
