(* Copyright (C) 1995, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Thu Feb  9 08:56:02 PST 1995 by kalsow  *)
(*      modified on Mon Jun  6 03:18:55 PDT 1994 by mhb   *)
(*      modified on Tue Feb 16 16:31:40 PST 1993 by johnh *)

(*********************************************************************
|*  NOTE: This file is generated automatically from the event 
|*        definition file #(_ALGNAME_).evt.
|*********************************************************************)

<* PRAGMA LL *>

MODULE #(_ALGNAME_)TranscriptView;

$ZFmt
$Fmt
$FormsVBT
$Rd
$Filter
$TextEditVBT
$TextPort
$VBT
$View
$Wr
$ZeusClass
$ZeusPanel
$#(_ALGNAME_)ViewClass
#(_IMPORTS_)

<* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>

REVEAL
  T = Public BRANDED OBJECT
    fv: FormsVBT.T := NIL;
    te: TextEditVBT.T := NIL;
  OVERRIDES
    init       := TViewInit;
    install    := TViewInstall;
    delete     := TViewDelete;
    snapshot   := TViewSnapshot;
    restore    := TViewRestore;
    config     := TViewConfig;
    reactivity := TViewReactivity;
    startrun   := TViewStartrun;
    endrun     := TViewEndrun;
#{_OUTPUT
    oe#(_EVENT_) := #(_EVENT_);
#}
#{_UPDATE
    ue#(_EVENT_) := #(_EVENT_);
#}
  END;

PROCEDURE TViewInit (view: T): T =
  <* LL = VBT.mu *>
  BEGIN
    TViewZTrace (view, "init");
    RETURN #(_ALGNAME_)ViewClass.T.init (view, NIL);
  END TViewInit;

PROCEDURE Clear (<* UNUSED *> fv  : FormsVBT.T;
                 <* UNUSED *> name: TEXT;
                              cl  : REFANY;
                 <* UNUSED *> time: VBT.TimeStamp) =
  BEGIN
    TextPort.SetText(NARROW(cl, T).te.tp, "")
  END Clear;

PROCEDURE TViewInstall (view: T) =
  <* LL = VBT.mu *>
  BEGIN
    view.fv := ZeusPanel.NewForm("#(_ALGNAME_)TranscriptView.fv");
    view.te := FormsVBT.GetVBT(view.fv, "transcript");
    TViewZTrace (view, "install");
    FormsVBT.AttachProc(view.fv, "clear", Clear, view);
    EVAL Filter.Replace (view, view.fv);
    #(_ALGNAME_)ViewClass.T.install (view);
  END TViewInstall;

PROCEDURE TViewDelete (view: T) =
  <* LL = VBT.mu *>
  BEGIN 
    TViewZTrace (view, "delete");
    #(_ALGNAME_)ViewClass.T.delete (view);
   END TViewDelete;

PROCEDURE TViewSnapshot (view: T; wr: Wr.T) RAISES {ZeusClass.Error} =
  <* LL = VBT.mu *>
  BEGIN 
    TViewZTrace (view, "snapshot");
    #(_ALGNAME_)ViewClass.T.snapshot (view, wr);
   END TViewSnapshot;

PROCEDURE TViewRestore (view: T; rd: Rd.T) RAISES {ZeusClass.Error} =
  <* LL = VBT.mu *>
  BEGIN 
    TViewZTrace (view, "restore");
    #(_ALGNAME_)ViewClass.T.restore (view, rd);
   END TViewRestore;

PROCEDURE TViewConfig (
    view: T; 
    state: ZeusClass.StateChange; 
    o: ZeusClass.T) =
  <* LL = VBT.mu *>
  BEGIN 
    TViewZTrace (view, "config");
    #(_ALGNAME_)ViewClass.T.config (view, state, o);
   END TViewConfig;

PROCEDURE TViewReactivity (view: T; <*UNUSED*> on: BOOLEAN) =
  <* LL = VBT.mu *>
  BEGIN
    TViewZTrace(view, "reactivity");
    #(_ALGNAME_)ViewClass.T.reactivity (view, TRUE);
  END TViewReactivity;

PROCEDURE TViewStartrun (view: T) =
  <* LL = {} *>
  BEGIN 
    TViewZTrace (view, "startrun");
    #(_ALGNAME_)ViewClass.T.startrun (view);
   END TViewStartrun;

PROCEDURE TViewEndrun (view: T) =
  <* LL = {} *>
  BEGIN 
    TViewZTrace (view, "endrun");
    #(_ALGNAME_)ViewClass.T.endrun (view);
  END TViewEndrun;


(* event handling methods: *)

#{_OUTPUT
PROCEDURE #(_EVENT_) (view: T; #(_ARGSTR_)) =
  <* LL = {} *>
  BEGIN 
    LOCK VBT.mu DO
    IF FormsVBT.GetBoolean(view.fv, "alg") THEN
       IF NOT FormsVBT.GetBoolean(view.fv, "args") THEN
          TViewTrace (view, "#(_EVENT_) ...")
       ELSE
          TViewTrace (view, "#(_EVENT_) " 
#{
          & #(_ARGFMT_)(#(_ARGNAME_))
#|
          & " "
#}
          )
       END
    END
    END
  END #(_EVENT_);

#}


#{_UPDATE
PROCEDURE #(_EVENT_) (view: T; #(_ARGSTR_)) =
  <* LL = VBT.mu *>
  BEGIN 
    IF FormsVBT.GetBoolean(view.fv, "alg") THEN
       IF NOT FormsVBT.GetBoolean(view.fv, "args") THEN
          TViewTrace (view, "#(_EVENT_) ...")
       ELSE
          TViewTrace (view, "#(_EVENT_) " 
#{
            & #(_ARGFMT_)(#(_ARGNAME_))
#|
            & " "
#}
            )
       END
   END
  END #(_EVENT_);

#}


PROCEDURE TViewZTrace (view: T; t: TEXT) =
  BEGIN
    IF view.fv # NIL THEN
      IF FormsVBT.GetBoolean(view.fv, "zeus") THEN
        TextPort.PutText(view.te.tp, "**zeus:  " & t & "\n");
        TextPort.Normalize(view.te.tp, LAST(INTEGER))
      END
    END
  END TViewZTrace;

PROCEDURE TViewTrace (view: T; t: TEXT) =
  BEGIN
    TextPort.PutText(view.te.tp, "--event: " & t & "\n");
    TextPort.Normalize(view.te.tp, LAST(INTEGER))
  END TViewTrace;


PROCEDURE TViewNew (): View.T =
  BEGIN
    RETURN NEW(T).init()
  END TViewNew;


BEGIN
  ZeusPanel.RegisterView 
      (TViewNew, "#(_ALGNAME_) Transcript View", "#(_ALGNAME_)"); 
END #(_ALGNAME_)TranscriptView.
