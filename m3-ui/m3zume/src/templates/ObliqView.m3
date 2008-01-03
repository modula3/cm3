(* Copyright (C) 1995, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Fri Sep  8 15:48:12 PDT 1995 by najork  *)
(*      modified on Thu Feb  9 08:54:15 PST 1995 by kalsow  *)
(* Last modified on Fri Dec  9 15:00:46 PST 1994 by mhb     *)
(*      modified on Sat Jun  4 16:24:49 1994 by heydon      *)
(*      modified on Tue Feb 16 16:31:40 PST 1993 by johnh   *)

(*********************************************************************
|*  NOTE: This file is generated automatically from the event 
|*        definition file #(_ALGNAME_).evt.
|*********************************************************************)

<* PRAGMA LL *>

MODULE #(_ALGNAME_)#(_VIEWNAME_)ObliqView;

$Filter
$Fmt
$Obliq
$ObliqParser
$ObLibAnim
$ObLibM3
$ObLibUI
$ObValue
$Rd
$Rsrc
$SynWr
$TextRd
$TextWr
$Thread
$VBT
$View
$ZeusPanel
$ZFmt
$#(_ALGNAME_)ViewClass
#(_IMPORTS_)

CONST 
  ViewName =  "#(_VIEWNAME_).obl";

TYPE
  T = #(_ALGNAME_)ViewClass.T BRANDED OBJECT
        object  : Obliq.Val;
        env     : Obliq.Env;
        wr      : TextWr.T;
        swr     : SynWr.T;
        parser  : ObliqParser.T;
      OVERRIDES
        <* LL.sup < VBT.mu *>
        startrun := Startrun;
        <* LL.sup < VBT.mu *>
#{_OUTPUT
        oe#(_EVENT_) := #(_EVENT_);
#}
        <* LL.sup = VBT.mu *>
#{_UPDATE
        ue#(_EVENT_) := #(_EVENT_);
#}
      END;

(* OUTPUT and UPDATE event handling methods: *)

#{_OUTPUT
PROCEDURE #(_EVENT_) (view: T; #(_ARGSTR_)) =
  <* LL.sup < VBT.mu *>
  BEGIN 
    IF FieldDefined(view.object, "#(_EVENT_)") THEN
      Invoke (view, "#(_EVENT_)", ""
#{
      & #(_ARGFMT_)(#(_ARGNAME_))
#|
      & ","
#}
      )
    END
  END #(_EVENT_);
#}

#{_UPDATE
PROCEDURE #(_EVENT_) (view: T; #(_ARGSTR_)) =
  <* LL = VBT.mu *>
  BEGIN 
    IF FieldDefined(view.object, "#(_EVENT_)") THEN
      Invoke (view, "#(_EVENT_)", ""
#{
      & #(_ARGFMT_)(#(_ARGNAME_))
#|
      & ","
#}
      )
    END
  END #(_EVENT_);
#}

PROCEDURE RegisterView () =
  BEGIN 
    ZeusPanel.RegisterView(New, "#(_VIEWNAME_).obl", "#(_ALGNAME_)")   
  END RegisterView;

PROCEDURE New (): View.T =
  BEGIN
    RETURN NEW(T).init(NIL)
  END New;

CONST 
  ObliqStackSizeMultiplier = 8;

TYPE
  Closure = Thread.SizedClosure OBJECT
              view: T;
            OVERRIDES
              apply := ForkedStartrun;
            END;

PROCEDURE Startrun (view: T) =
  <* LL.sup < VBT.mu *>
  BEGIN
    EVAL
      Thread.Join(
        Thread.Fork(
          NEW(Closure, view := view,
              stackSize := ObliqStackSizeMultiplier * Thread.GetDefaultStackSize())));
  END Startrun;

PROCEDURE ForkedStartrun (cl: Closure): REFANY =
  VAR rd: Rd.T; view := cl.view;
  BEGIN
    IF view.parser = NIL THEN
      view.wr := TextWr.New();
      view.swr := SynWr.New(view.wr);
      view.parser := ObliqParser.New(view.swr);
    END;
    view.object := NIL;
    TRY
      rd := Rsrc.Open(ViewName, ZeusPanel.GetPath());
      view.env := ParseRd(view.parser, ViewName, rd);
      WITH obj = Obliq.Lookup("view", view.env) DO
        IF NOT ISTYPE(obj, ObValue.ValObj) THEN
          ZeusPanel.ReportError(
            "not an Obliq object in '" & ViewName & "'")
        ELSIF FieldDefined (obj, "graphvbt") THEN
          WITH graphvbt =
            NARROW(Obliq.ObjectSelect(obj, "graphvbt"),
                   ObLibAnim.ValGraph).vbt DO
            LOCK VBT.mu DO
              EVAL Filter.Replace(view, graphvbt)
            END
          END;
          view.object := obj;
        ELSIF FieldDefined (obj, "rectsvbt") THEN
          WITH rectsvbt =
            NARROW(Obliq.ObjectSelect(obj, "rectsvbt"),
                   ObLibAnim.ValRects).vbt DO
            LOCK VBT.mu DO
              EVAL Filter.Replace(view, rectsvbt)
            END
          END;
          view.object := obj;
        ELSIF FieldDefined (obj, "formsvbt") THEN
          WITH formsvbt =
            NARROW(Obliq.ObjectSelect(obj, "formsvbt"),
                   ObLibUI.ValForm).vbt DO
            LOCK VBT.mu DO
              EVAL Filter.Replace(view, formsvbt)
            END
          END;
          view.object := obj;
        ELSE
          ZeusPanel.ReportError(
            "cannot find 'graphvbt', 'rectsvbt', or 'formsvbt' in '" & ViewName & "'")
        END
      END
    EXCEPT
    | Rsrc.NotFound =>
        ZeusPanel.ReportError("cannot find '" & ViewName & "'")
    | ObValue.Error (packet) => OblError(view, packet)
    | ObValue.Exception (packet) => OblException(view, packet)
    END;
    RETURN NIL;
  END ForkedStartrun;

PROCEDURE ParseRd (p: ObliqParser.T; name: TEXT; rd: Rd.T):
  Obliq.Env RAISES {ObValue.Error, ObValue.Exception} =
  VAR env := Obliq.EmptyEnv();
  BEGIN
    ObliqParser.ReadFrom(p, name, rd, TRUE);
    TRY
      LOOP
        EVAL ObliqParser.EvalPhrase(p, ObliqParser.ParsePhrase(p), env)
      END
    EXCEPT
      ObliqParser.Eof => (* clean exit of loop *)
    END;
    RETURN env
  END ParseRd;

PROCEDURE Invoke (view: T; event, args: TEXT) =
  VAR
    exp    := "view." & event & "(" & args & ");";
    name   := "Zeus Event <" & event & ">";
  BEGIN
    ObliqParser.ReadFrom (view.parser, name, TextRd.New(exp), FALSE);
    TRY
      EVAL Obliq.EvalTerm(ObliqParser.ParseTerm(view.parser), view.env)
    EXCEPT
    | ObliqParser.Eof => <* ASSERT FALSE *>
    | ObValue.Error (packet) => OblError(view, packet)
    | ObValue.Exception (packet) => OblException(view, packet)
    END
  END Invoke;

PROCEDURE FieldDefined (object: Obliq.Val; event: TEXT): BOOLEAN =
  BEGIN
    TRY
      RETURN object # NIL AND Obliq.ObjectHas(object, event)
    EXCEPT
    | ObValue.Error =>
    | ObValue.Exception =>
    END;
    RETURN FALSE
  END FieldDefined;

PROCEDURE OblError (view: T; packet: ObValue.ErrorPacket) =
  BEGIN
    Obliq.ReportError(view.swr, packet);
    ZeusPanel.ReportError(
      "Obliq error: " & TextWr.ToText(view.wr))
  END OblError;

PROCEDURE OblException (view: T; packet: ObValue.ExceptionPacket) =
  BEGIN
    Obliq.ReportException(view.swr, packet);
    ZeusPanel.ReportError(
      "Obliq exception: " & TextWr.ToText(view.wr))
  END OblException;

BEGIN
  SynWr.Setup();
  ObliqParser.PackageSetup();
  ObLibM3.PackageSetup();
  ObLibUI.PackageSetup();
  ObLibAnim.PackageSetup();
  RegisterView ();
END #(_ALGNAME_)#(_VIEWNAME_)ObliqView.
