(* Copyright (C) 1995, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Thu Feb  9 09:01:09 PST 1995 by kalsow  *)
(* Last modified on Sat Jul 17 14:28:37 PDT 1993 by mhb     *)
(*      modified on Thu Jul 15 14:12:41 PDT 1993 by steveg  *)
(*      modified on Tue Feb 16 16:31:40 PST 1993 by johnh   *)

(*********************************************************************
|*  NOTE: This file is generated automatically from the event 
|*        definition file #(_ALGNAME_).evt.
|*********************************************************************)

<* PRAGMA LL *>

MODULE #(_ALGNAME_)#(_VIEWNAME_)GEFView;

$Filter
$Fmt
$GEF
$GEFError
$GEFViewClass
$GraphVBT
$Rd
$Rsrc
$SLisp
$Thread
$VBT
$View
$ViewClass
$ZeusClass
$ZeusPanel
$#(_ALGNAME_)AlgClass
$#(_ALGNAME_)ViewClass
#(_IMPORTS_)

TYPE
  T = #(_ALGNAME_)ViewClass.T BRANDED OBJECT
        name := "#(_VIEWNAME_).gef"; 
      OVERRIDES
        <* LL = VBT.mu *>
        install  := Install;
        startrun := Startrun;
        <* LL = 0 *>
#{_OUTPUT
        oe#(_EVENT_) := #(_EVENT_);
#}
      END;

PROCEDURE Install (v: T) =
  <* FATAL SLisp.Error *>
  VAR
    interp := NEW(GEFViewClass.Interp).init();
    gef    := NEW(GEF.T).init(interp);
  BEGIN
    EVAL Filter.Replace (v, gef);
    #(_ALGNAME_)ViewClass.T.install(v)
  END Install;

PROCEDURE Startrun(v: T) =
  BEGIN
    TRY
      GEF.InitFromRsrc(Filter.Child(v), v.name, ZeusPanel.GetPath());
    EXCEPT
    | Thread.Alerted =>
    | Rsrc.NotFound =>
        ZeusPanel.ReportError(
          Fmt.F("GEF View error: Could not find file: %s", v.name));
    | Rd.Failure =>
        ZeusPanel.ReportError(
          Fmt.F("GEF View error finding or parsing file: %s", v.name));
    | GEFError.T (msg) =>
        ZeusPanel.ReportError(
          Fmt.F("GEF View error (%s) parsing file: %s", msg, v.name));
    END;
  END Startrun;

PROCEDURE New (): View.T =
  VAR g := NEW(GraphVBT.T).init(); 
  BEGIN
    RETURN NEW(T).init(g)
  END New;

(* event handling methods: *)

#{_OUTPUT
PROCEDURE #(_EVENT_) (view: T; #(_ARGSTR_)) =
  <* LL = {} *>
  VAR gef: GEF.T := Filter.Child(view);
  BEGIN
    TRY
      GEF.InvokeEvent (gef, "#(_EVENT_)", GEF.EventData(
#{
      #(_ARGFMT_)(#(_ARGNAME_))
#|
      ,
#}
      ));
      gef.redisplay();
      gef.animate(0.0, 1.0);
    EXCEPT
    | Thread.Alerted =>
    | GEFError.T (msg) =>
        ZeusPanel.ReportError(
          Fmt.F("GEF View error (%s) parsing file: %s", msg, view.name));
    END;
  END #(_EVENT_);

#}

BEGIN
  ZeusPanel.RegisterView(New, "#(_VIEWNAME_).gef", "#(_ALGNAME_)"); 
END #(_ALGNAME_)#(_VIEWNAME_)GEFView.
