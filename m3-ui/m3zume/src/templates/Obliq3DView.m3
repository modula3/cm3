(* Copyright (C) 1995, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Mon Jul 10 15:31:57 PDT 1995 by najork  *)
(*      modified on Thu Feb  9 08:53:09 PST 1995 by kalsow  *)
(*      modified on Mon Jun  6 03:20:38 PDT 1994 by mhb     *)
(*      modified on Sat Jun  4 16:22:04 1994 by heydon      *)
(*      modified on Tue Feb 16 16:31:40 PST 1993 by johnh   *)

(*********************************************************************
|*  NOTE: This file is generated automatically from the event 
|*        definition file #(_ALGNAME_).evt.
|*********************************************************************)

<* PRAGMA LL *>

MODULE #(_ALGNAME_)#(_VIEWNAME_)Obliq3DView;

$Fmt
$ZFmt
$Obliq
$ObliqParser
$ObLib3D
$ObLibAnim
$ObLibM3
$ObLibUI
$ObValue
$ObView3D
$ObZeus3D
$Rd
$Rsrc
$SynWr
$TextRd
$TextWr
$Thread
$View
$View3DProxy
$ZeusPanel
$SynLocation
$SynParse
$MetaParser
$ObliqPrinter
$ObLibOnline
$#(_ALGNAME_)3DViewClass
#(_IMPORTS_)

CONST 
  ViewName =  "#(_VIEWNAME_).obl";

TYPE
  T = #(_ALGNAME_)3DViewClass.T BRANDED OBJECT
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
      Invoke (view, "#(_EVENT_)", ""
#{
      & #(_ARGFMT_)(#(_ARGNAME_))
#|
      & ","
#}
      )
  END #(_EVENT_);

#}

#{_UPDATE
PROCEDURE #(_EVENT_) (view: T; #(_ARGSTR_)) =
  <* LL.sup = VBT.mu *>
  BEGIN 
      Invoke (view, "#(_EVENT_)", ""
#{
      & #(_ARGFMT_)(#(_ARGNAME_))
#|
      & ","
#}
      )
  END #(_EVENT_);
#}

TYPE 
  Closure = Thread.SizedClosure BRANDED OBJECT
    view : T;
  OVERRIDES
    apply := Apply;
  END;


PROCEDURE Apply (self : Closure) : REFANY =

  PROCEDURE ParseRd (p: ObliqParser.T; name: TEXT; rd: Rd.T) : Obliq.Env 
      RAISES {ObValue.Error, ObValue.Exception} =
    VAR 
      env := Obliq.EmptyEnv ();
    BEGIN
      ObliqParser.ReadFrom (p, name, rd, TRUE);
      TRY
        LOOP
          EVAL ObliqParser.EvalPhrase (p, ObliqParser.ParsePhrase (p), env)
        END
      EXCEPT
        ObliqParser.Eof => (* clean exit of loop *)
      END;
      RETURN env
    END ParseRd;

  BEGIN
    WITH view = self.view DO
      (*** Parse the .obl file ***)
      TRY
        WITH rd = Rsrc.Open (ViewName, ZeusPanel.GetPath ()) DO
          view.env := ParseRd (view.parser, ViewName, rd);
        END;
        WITH obj = Obliq.Lookup ("view", view.env) DO
          ObView3D.PairUp (view, obj);
        END;
      EXCEPT
      | Rsrc.NotFound => 
        ZeusPanel.ReportError("cannot find '" & ViewName & "'");
      | ObValue.Error (packet) => 
        OblError(view, packet);
      | ObValue.Exception (packet) => 
        OblException(view, packet);
      END;

      RETURN NIL;
    END;
  END Apply;


PROCEDURE New (): View.T =
  VAR
    view : T;
  BEGIN
    (*** Create a View3D.T ***)
    view := NEW (T).init ("Obliq3D View");

    (*** Create a new Obliq parser ***)
    view.wr     := TextWr.New ();
    view.swr    := SynWr.New (view.wr);
    view.parser := ObliqParser.New (view.swr);

    (*** Load the .obl file ***)
    WITH thread = Thread.Fork (NEW (Closure, view := view)) DO
      EVAL Thread.Join (thread);
    END;

    (*** invoke the New method of the Obliq object "view" ***)
    Invoke (view, "New", "");

    (*** Return the new view ***)
    RETURN view;
  END New;


PROCEDURE Startrun (view: T) =
  <* LL.sup < VBT.mu *>
  BEGIN
    (*** Reload the .obl file ***)
    WITH thread = Thread.Fork (
                      NEW (Closure,
                           stackSize := 2 * Thread.GetDefaultStackSize (),
                           view := view)) DO
      EVAL Thread.Join (thread);
    END;
  END Startrun;


PROCEDURE Invoke (view: T; event, args: TEXT) =
  VAR
    exp    := "view." & event & "(" & args & ");";
    name   := "Zeus Event <" & event & ">";
    proxy  := ObView3D.M3ToObliq (view);
  BEGIN
    TRY
      IF proxy # NIL AND Obliq.ObjectHas (proxy, event) THEN
        ObliqParser.ReadFrom (view.parser, name, TextRd.New(exp), FALSE);
        EVAL Obliq.EvalTerm (ObliqParser.ParseTerm (view.parser), view.env);
      END;
    EXCEPT
    | ObliqParser.Eof => <* ASSERT FALSE *>
    | ObValue.Error (packet) => OblError(view, packet)
    | ObValue.Exception (packet) => OblException(view, packet)
    END;
  END Invoke;


PROCEDURE OblError (view: T; packet: ObValue.ErrorPacket) =
  BEGIN
    Obliq.ReportError(view.swr, packet);
    ZeusPanel.ReportError("Obliq error: " & TextWr.ToText(view.wr))
  END OblError;


PROCEDURE OblException (view: T; packet: ObValue.ExceptionPacket) =
  BEGIN
    Obliq.ReportException(view.swr, packet);
    ZeusPanel.ReportError(
      "Obliq exception: " & TextWr.ToText(view.wr))
  END OblException;


BEGIN
  SynWr.Setup();
  SynLocation.PackageSetup();
  SynParse.PackageSetup();
  MetaParser.PackageSetup(); <* NOWARN *>
  Obliq.PackageSetup();
  ObliqParser.PackageSetup();
  ObliqPrinter.PackageSetup();
  ObLibOnline.Setup();             (* really needed! *)

  ObLibM3.PackageSetup();
  ObLibUI.PackageSetup();
  ObLibAnim.PackageSetup();
  ObLib3D.PackageSetup();
  ObZeus3D.PackageSetup();

  ZeusPanel.RegisterView (New, 
                          "#(_VIEWNAME_).obl", 
                          "#(_ALGNAME_)", 
                          sample := NEW(T));
END #(_ALGNAME_)#(_VIEWNAME_)Obliq3DView.
