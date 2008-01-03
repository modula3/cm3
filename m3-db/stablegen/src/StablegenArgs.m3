(* Copyright (C) 1991, Digital Equipment Corporation          *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(* Created by Susan Owicki                                    *)
(* Last modified on Mon Oct  9 18:01:57 PDT 1995 by heydon    *)
(*      modified on Mon Sep 26 19:50:33 PDT 1994 by weich     *)

(* The module sets up the tool's parameters (see initialization)
   and reads the parameters in the procedure "Get()" *)

MODULE StablegenArgs;

IMPORT M3Args, Type, Atom, StablegenError;

CONST
  Version      = "Aug-94";
  ArgImplfile  = "ImplementationModule";
  ArgRepModule  = "RepModule";
  ArgDataT     = "Object";


(* \subsection{Procedure Get}
   First check for help option. If present just display help and
   return. Then load the parameters (which checks for correctness
   especially whether the required "Object" parameter is
   present). Then read the name of the object's interface.  Then
   read the implementation and rep module name. Build up the default
   name if the parameter is not present. Finally read the "-i" parameter
   to get the name of the most specific revealation of "object" -- it must
   be in the first interface listed by the "-i" parameter.
*)
PROCEDURE Get(VAR object: Type.Qid; VAR reveal, impl, rep: TEXT)
  RAISES {StablegenError.E} =
  BEGIN
    IF M3Args.CheckHelp(display:= FALSE) THEN
      M3Args.Help(tool);
      RAISE StablegenError.E("nothing was parsed");
    END; (*IF*)
    IF M3Args.Find(tool) THEN
      VAR intf:= M3Args.GetString(tool, ArgDataT);
      BEGIN
        object:= NEW(Type.Qid, intf:= Atom.FromText(intf),
                     item:= Atom.FromText("T"));
        impl:= M3Args.GetString(tool, ArgImplfile);
        IF impl = NIL THEN impl:= "Stable"&intf END;
        rep:= M3Args.GetString(tool, ArgRepModule);
        IF rep = NIL THEN rep:= impl&"Rep" END;
      END
    ELSE
      RAISE StablegenError.E("invalid parameter");
    END; (*IF*)
    VAR ints:= M3Args.GetStringList(tool, "Interfaces");
    BEGIN
      IF ints = NIL THEN
        RAISE StablegenError.E("need -Interfaces (-i) parameter");
      END;
      reveal:= ints[0];
    END
  END Get;

(* \subsection{Initialisation}
   Start by registration of parameters specific to the
   stable stub generator.
*)
VAR
  tool := M3Args.New("stubs",
                     "Generate stubs for stable objects",
                     Version, master := TRUE);
BEGIN
  M3Args.RegisterString(tool, ArgDataT,
                        "name of object interface",
                        opt:= M3Args.Opt.Required);
  M3Args.RegisterString(tool, ArgImplfile,
                        "name of module containing "
                        & "implementation of stable subtype");
  M3Args.RegisterString(tool, ArgRepModule,
                        "name of module containing "
                        & "the instatiation of the StableRep module.");
END StablegenArgs.

