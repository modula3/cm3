(* Copyright (C) 1995, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Thu Feb  9 08:59:04 PST 1995 by kalsow  *)
(*      modified on Mon Jun  6 03:16:18 PDT 1994 by mhb     *)
(*      modified on Sun Jun  5 16:47:33 PDT 1994 by heydon  *)

(*********************************************************************
|*  NOTE: This file is generated automatically from the event 
|*        definition file #(_ALGNAME_).evt.
|*********************************************************************)

<* PRAGMA LL *>

MODULE #(_ALGNAME_)#(_VIEWNAME_)JunoView;

$Atom
$AtomList
$Fmt
$NetObj
$Thread
$VBT
$Wr
$Rd
$ZFmt
$ZeusPanel
$RemoteView
$View
$#(_ALGNAME_)ViewClass
#(_IMPORTS_)

TYPE
  T = #(_ALGNAME_)ViewClass.T BRANDED OBJECT
         jz: RemoteView.T := NIL;
      OVERRIDES
        <* LL.sup < VBT.mu *>
        startrun := Startrun;
        endrun := Endrun;
        <* LL.sup < VBT.mu *>
#{_OUTPUT
        oe#(_EVENT_) := #(_EVENT_);
#}
        <* LL.sup = VBT.mu *>
#{_UPDATE
        ue#(_EVENT_) := #(_EVENT_);
#}
        <* LL.sup = VBT.mu *>
        install := NoargNoop;
        delete := NoargNoop;
        snapshot := SnapshotNoop;
        restore := RestoreNoop;
        reactivity := ReactivityNoop
      END;

(* STARTRUN and ENDRUN methods: *)

PROCEDURE Startrun(view: T) =
  <* LL.sup < VBT.mu *>
  BEGIN
    IF view.jz = NIL THEN RETURN END;
    TRY view.jz.startrun() EXCEPT
      NetObj.Error (err) => ZeusPanel.ReportError(NetObjError(err))
    | Thread.Alerted => ZeusPanel.ReportError("NetObj call alerted")
    END
  END Startrun;

PROCEDURE Endrun(view: T) =
  <* LL.sup < VBT.mu *>
  BEGIN
    IF view.jz = NIL THEN RETURN END;
    TRY view.jz.endrun() EXCEPT
      NetObj.Error (err) => ZeusPanel.ReportError(NetObjError(err))
    | Thread.Alerted => ZeusPanel.ReportError("NetObj call alerted")
    END
  END Endrun;

(* OUTPUT and UPDATE event handling methods: *)

#{_OUTPUT
PROCEDURE #(_EVENT_) (view: T; #(_ARGSTR_)) =
  <* LL.sup < VBT.mu *>
  CONST EventName = "#(_EVENT_)";
  VAR tfactor: REAL; BEGIN 
    IF view.jz = NIL THEN RETURN END;
    LOCK VBT.mu DO
      tfactor := ZeusPanel.GetAnimationTime()
    END;
    TRY
      view.jz.event (tfactor, EventName, "("
#{
      & #(_ARGFMT_)(#(_ARGNAME_))
#|
      & " "
#}
      & ")")
    EXCEPT
      RemoteView.Error (txt) => ReportError(EventName, txt)
    | NetObj.Error (err) => ReportError(EventName, NetObjError(err))
    | Thread.Alerted => ReportError(EventName, "NetObj call alerted")
    END
  END #(_EVENT_);

#}
#{_UPDATE
PROCEDURE #(_EVENT_) (view: T; #(_ARGSTR_)) =
  <* LL.sup = VBT.mu *>
  CONST EventName = "#(_EVENT_)";
  VAR tfactor: REAL; BEGIN 
    IF view.jz = NIL THEN RETURN END;
    tfactor := ZeusPanel.GetAnimationTime();
    TRY
      view.jz.event (tfactor, EventName, "("
#{
      & #(_ARGFMT_)(#(_ARGNAME_))
#|
      & " "
#}
      & ")")
    EXCEPT
      RemoteView.Error (txt) => ReportError(EventName, txt)
    | NetObj.Error (err) => ReportError(EventName, NetObjError(err))
    | Thread.Alerted => ReportError(EventName, "NetObj call alerted")
    END
  END #(_EVENT_);

#}
PROCEDURE NoargNoop(<*UNUSED*> v: T) =
  BEGIN END NoargNoop;

PROCEDURE SnapshotNoop(<*UNUSED*> v: T; <*UNUSED*> wr: Wr.T) =
  BEGIN END SnapshotNoop;

PROCEDURE RestoreNoop(<*UNUSED*> v: T; <*UNUSED*> rd: Rd.T) =
  BEGIN END RestoreNoop;

PROCEDURE ReactivityNoop(<*UNUSED*> v: T; <*UNUSED*> on: BOOLEAN) =
  BEGIN END ReactivityNoop;

PROCEDURE New (): View.T =
  VAR
    res: T := NEW(T).init(NIL);
    jz: NetObj.T;
    errMsg: TEXT := NIL;
  BEGIN
    TRY
      jz := NetObj.Import("JunoZeus");
      IF jz = NIL THEN
    	errMsg := "NetObj Error: Can't find JunoZeus object"
      ELSIF NOT ISTYPE(jz, RemoteView.T) THEN
    	errMsg := "NetObj Error: Type mismatch between Juno and Zeus"
      ELSE
    	res.jz := jz;
    	RETURN res
      END
    EXCEPT
      NetObj.Error (err) => errMsg := NetObjError(err)
    | Thread.Alerted =>     errMsg := "NetObj Import alerted"
    END;
    ZeusPanel.ReportError(errMsg);
    RETURN NIL
  END New;

PROCEDURE NetObjError(err: AtomList.T): TEXT =
  VAR res := "NetObj Error: "; BEGIN
    WHILE err # NIL DO
      res := res & Atom.ToText(err.head);
      err := err.tail;
      IF err # NIL THEN res := res & "; " END
    END;
    RETURN res
  END NetObjError;

PROCEDURE ReportError(nm, msg: TEXT) =
  BEGIN
    ZeusPanel.ReportError("\"" & nm & "\" event error: " & msg)
  END ReportError;

PROCEDURE RegisterView () =
  BEGIN 
    ZeusPanel.RegisterView(New, "#(_VIEWNAME_)", "#(_ALGNAME_)")   
  END RegisterView;

BEGIN
  RegisterView ();
END #(_ALGNAME_)#(_VIEWNAME_)JunoView.
