(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)
(**)
(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3CBE_C_Tool;

(**********
IMPORT M3CBE_C_sun3;
IMPORT M3CBE_C_vax;
IMPORT M3CBE_C_sparc;
IMPORT M3CBE_C_i386;
***********)
IMPORT M3CBE_C_mips;

IMPORT M3Args;
IMPORT M3CBackEnd_C_cc;

CONST
  Version = "14-Jan-90";

VAR
  tool_g: M3Args.T;

PROCEDURE ChooseDefault() RAISES {}=
  BEGIN



      M3CBE_C_mips.Init();



  END ChooseDefault;

PROCEDURE ToolInit() RAISES {}=
  VAR
    iter: M3CBackEnd_C_cc.Iter;
    target, message: TEXT;
    proc: REF M3CBackEnd_C_cc.TargetInitProc;
  BEGIN
    message := "one of (";
    iter := M3CBackEnd_C_cc.NewIter();
    WHILE M3CBackEnd_C_cc.Next(iter, target, proc) DO
      message := message & " " & target;
    END; (* while *)
    message := message & " )";
    M3Args.RegisterString(tool_g, CCTarget_Arg, message);
  END ToolInit;

PROCEDURE Init(): INTEGER RAISES {}=
  VAR
    target: TEXT;
    proc: REF M3CBackEnd_C_cc.TargetInitProc;
  BEGIN
    IF M3Args.Find(tool_g) THEN
      target := M3Args.GetString(tool_g, CCTarget_Arg);
      IF target # NIL THEN
        IF M3CBackEnd_C_cc.LookupTarget(target, proc) THEN
          proc^();
          RETURN 0
        ELSE
           RETURN -1;
        END; (* if *)
      ELSE
        ChooseDefault();
        RETURN 0;
      END; (* if *)
    ELSE
      RETURN -1;
    END; (* if *)
  END Init;


BEGIN
  tool_g := M3Args.New("m3cctarget", "Target C compiler control", Version);
END M3CBE_C_Tool.


