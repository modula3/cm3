(* Copyright (C) 1995, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Thu Feb  9 08:48:22 PST 1995 by kalsow  *)
(*      modified on Wed Feb 17 16:46:18 PST 1993 by johnh   *)
(*      modified on Thu Sep 24 10:59:20 PDT 1992 by mhb     *)

(*********************************************************************
|*  NOTE: This file is generated automatically from the event 
|*        definition file #(_ALGNAME_).evt.
|*********************************************************************)

MODULE #(_ALGNAME_)AlgClass;

$Algorithm
$Fmt
$FormsVBT
$Rd
$Text
$VBT
$Wr
$ZeusClass
$ZeusPanel
$ZeusUtil
#(_IMPORTS_)

<* PRAGMA LL *>

(* Fix any FormsVBT errors; don't handle exceptions for them. *)
<* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>


REVEAL
  T = Public BRANDED OBJECT
      OVERRIDES
        <* LL = VBT.mu *>
        init := #(_ALGNAME_)DefaultInit;
        snapshot := #(_ALGNAME_)DefaultSnapshot;
        restore := #(_ALGNAME_)DefaultRestore;
        updateEventCounts := #(_ALGNAME_)DefaultUpdateCts;
#{_FEEDBACK
        fe#(_EVENT_) := #(_EVENT_);
#}
      END;

PROCEDURE #(_ALGNAME_)DefaultInit (v: T): Algorithm.T = 
  <* LL = VBT.mu *>
  PROCEDURE Attach (id: TEXT; proc: FormsVBT.Proc) =
    BEGIN
      FormsVBT.AttachProc(v.eventData, id, proc, v);
    END Attach;
  BEGIN
    v.eventData := ZeusPanel.NewForm("#(_ALGNAME_)EventData.fv");
    Attach("stopatCodeEvents", #(_ALGNAME_)DoIt);
    Attach("waitatCodeEvents", #(_ALGNAME_)DoIt);
    Attach("eventCounts", #(_ALGNAME_)RefreshCts);
#{_OUTPUT
    Attach("stopAt#(_EVENT_)", #(_ALGNAME_)DoIt);
    Attach("waitAt#(_EVENT_)", #(_ALGNAME_)DoIt);
#}
    FromFV (v.eventData, v);    (* Get FV and internal data in sync *)
    RETURN Algorithm.T.init(v);
  END #(_ALGNAME_)DefaultInit;

PROCEDURE #(_ALGNAME_)DoIt (           fv : FormsVBT.T;
                           e  : TEXT;
                           arg: REFANY;
                <*UNUSED*> t  : VBT.TimeStamp) =
  <* LL = VBT.mu *>
  BEGIN 
    IF Text.Equal(e, "stopatCodeEvents") THEN
      NARROW(arg, T).stopatCodeEvents :=
          FormsVBT.GetBoolean(fv, "stopatCodeEvents");
    END;
    IF Text.Equal(e, "waitatCodeEvents") THEN
      NARROW(arg, T).waitatCodeEvents :=
          FormsVBT.GetInteger(fv, "waitatCodeEvents");
    END;
#{_OUTPUT
    IF Text.Equal(e, "stopAt#(_EVENT_)") THEN
      NARROW(arg, T).eventDataRec.stopAt#(_EVENT_) :=
          FormsVBT.GetBoolean(fv, "stopAt#(_EVENT_)");
    END;
    IF Text.Equal(e, "waitAt#(_EVENT_)") THEN
      NARROW(arg, T).eventDataRec.waitAt#(_EVENT_) :=
          FormsVBT.GetInteger(fv, "waitAt#(_EVENT_)");
    END;
#}
  END #(_ALGNAME_)DoIt;

PROCEDURE #(_ALGNAME_)RefreshCts (
                <*UNUSED*> fv  : FormsVBT.T;
                <*UNUSED*> e   : TEXT;
                           arg : REFANY;
                <*UNUSED*> t   : VBT.TimeStamp) =
  <* LL = VBT.mu *>
  BEGIN
    NARROW(arg, T).updateEventCounts(FALSE);
  END #(_ALGNAME_)RefreshCts;

PROCEDURE FromFV (fv : FormsVBT.T; alg: T) =
  <* LL = VBT.mu *>
  BEGIN 
    alg.stopatCodeEvents :=
        FormsVBT.GetBoolean(fv, "stopatCodeEvents");
    alg.waitatCodeEvents :=
        FormsVBT.GetInteger(fv, "waitatCodeEvents");
#{_OUTPUT
    alg.eventDataRec.stopAt#(_EVENT_) :=
        FormsVBT.GetBoolean(fv, "stopAt#(_EVENT_)");
    alg.eventDataRec.waitAt#(_EVENT_) :=
        FormsVBT.GetInteger(fv, "waitAt#(_EVENT_)");
#}
  END FromFV;

<*UNUSED*> 
PROCEDURE ToFV (fv : FormsVBT.T; alg: T) =
  <* LL = VBT.mu *>
  BEGIN 
    FormsVBT.PutBoolean(fv, "stopatCodeEvents", alg.stopatCodeEvents);
    FormsVBT.PutInteger(fv, "waitatCodeEvents", alg.waitatCodeEvents);
#{_OUTPUT
    FormsVBT.PutBoolean(fv, "stopAt#(_EVENT_)", 
                        alg.eventDataRec.stopAt#(_EVENT_));
    FormsVBT.PutInteger(fv, "waitAt#(_EVENT_)", 
                        alg.eventDataRec.waitAt#(_EVENT_));
#}
    CountsToFV (fv, alg);
  END ToFV;

PROCEDURE CountsToFV (fv : FormsVBT.T; alg: T) =
  <* LL = VBT.mu *>
  BEGIN 
#{_OUTPUT
    FormsVBT.PutText(fv, "ctOf#(_EVENT_)", 
                        Fmt.Int(alg.eventDataRec.ctOf#(_EVENT_)));
#}
  END CountsToFV;

PROCEDURE #(_ALGNAME_)DefaultUpdateCts ( v: T; reset: BOOLEAN) =
  <* LL = VBT.mu *>
  BEGIN
    IF reset THEN
#{_OUTPUT
      v.eventDataRec.ctOf#(_EVENT_) := 0;
#}
    END;
    CountsToFV (v.eventData, v);
  END #(_ALGNAME_)DefaultUpdateCts;


PROCEDURE #(_ALGNAME_)DefaultSnapshot (v: T; wr: Wr.T) 
  RAISES {ZeusClass.Error} =
  <* LL = VBT.mu *>
  BEGIN
    TRY
      Wr.PutChar(wr, '(')
    EXCEPT
    ELSE
      RAISE ZeusClass.Error(
          "#(_ALGNAME_)AlgClass.#(_ALGNAME_)DefaultSnapshot write error");
    END;
    IF v.eventData = NIL THEN
      RAISE ZeusClass.Error(
          "#(_ALGNAME_)AlgClass.#(_ALGNAME_)DefaultSnapshot: " & 
          "eventData not set!");
    END;
    TRY
      v.eventData.snapshot(wr)
    EXCEPT
      FormsVBT.Error (msg) =>
        RAISE ZeusClass.Error(
          "#(_ALGNAME_)AlgClass.#(_ALGNAME_)DefaultSnapshot FV error: " 
          & msg);
    ELSE
      RAISE ZeusClass.Error(
        "#(_ALGNAME_)AlgClass.#(_ALGNAME_)DefaultSnapshot error");
    END;
    Algorithm.T.snapshot(v, wr);
    TRY
      Wr.PutChar(wr, ')')
    EXCEPT
    ELSE
      RAISE ZeusClass.Error(
          "#(_ALGNAME_)AlgClass.#(_ALGNAME_)DefaultSnapshot write error");
    END;
  END #(_ALGNAME_)DefaultSnapshot;


PROCEDURE #(_ALGNAME_)DefaultRestore (v: T; rd: Rd.T) 
  RAISES {ZeusClass.Error} =
  <* LL = VBT.mu *>
  BEGIN
    IF rd = NIL THEN RETURN END;
    IF NOT ZeusUtil.EatChar(rd, '(') THEN
      RAISE ZeusClass.Error(
          "#(_ALGNAME_)AlgClass.#(_ALGNAME_)DefaultRestore read error");
    END;
    IF v.eventData = NIL THEN
      RAISE ZeusClass.Error(
          "#(_ALGNAME_)AlgClass.#(_ALGNAME_)DefaultRestore: " & 
          "eventData not set!");
    END;
    TRY
      v.eventData.restore(rd);
      v.updateEventCounts(FALSE);
      FromFV(v.eventData, v);
    EXCEPT
    ELSE
      RAISE ZeusClass.Error(
          "#(_ALGNAME_)AlgClass.#(_ALGNAME_)DefaultRestore error");
    END;
    Algorithm.T.restore(v, rd);
    IF NOT ZeusUtil.EatChar(rd, ')') THEN
      RAISE ZeusClass.Error(
          "#(_ALGNAME_)AlgClass.#(_ALGNAME_)DefaultRestore read error");
    END;
  END #(_ALGNAME_)DefaultRestore;

#{_FEEDBACK
PROCEDURE #(_EVENT_) (self: T
#{
    ; <*UNUSED*> #(_ARGMODE_)#(_ARGNAME_): #(_ARGTYPE_)
#}
) =
  <* LL = VBT.mu *>
  BEGIN
    self.evtHandled := FALSE;
  END #(_EVENT_);

#}

BEGIN
END #(_ALGNAME_)AlgClass.
