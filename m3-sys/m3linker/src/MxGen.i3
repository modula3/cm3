(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: MxGen.i3                                              *)
(* Last Modified On Fri Nov 11 13:00:26 PST 1994 By kalsow     *)

INTERFACE MxGen;

IMPORT Mx, Wr, M3CG;


PROCEDURE GenerateMain (base       : Mx.LinkSet;
                        c_output   : Wr.T;
                        cg_output  : M3CG.T;
                        verbose    : BOOLEAN;
                        windowsGUI : BOOLEAN);
(* write the list of compilation units in 'base' on 'c_output' or
   'cg_output' in a correct Modula-3 initialization order.  If 'c_output'
   is non-NIL, a C source program is generated.  If 'cg_output' is
   non-NIL, a Modula-3 intermediate code file (M3CG) is generated.
   It is a an checked runtime error for both or neither of 'c_output'
   and 'cg_output' to be non-NIL.  It is an error to pass a 'base'
   that hasn't successfully passed through 'MxCheck.IsProgram'. *)

END MxGen.
