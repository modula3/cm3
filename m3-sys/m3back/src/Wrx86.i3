(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Aug  2 14:07:27 PDT 1994 by isard      *)
(*      modified on Mon Jun 27 17:05:36 PDT 1994 by kalsow     *)
(*      modified on Tue May 25 14:22:35 PDT 1993 by muller     *)

INTERFACE Wrx86;
 
IMPORT Wr, Target;

FROM M3CG IMPORT Name, TypeUID;
FROM M3CG IMPORT Var, Proc, Label;
FROM M3CG IMPORT Type;

TYPE T <: Public;
TYPE Public = OBJECT
      METHODS
        Flush ();
        NL ();
        Cmd (cmd: TEXT);
        ZName (n: Name);
        VName (v: Var);
        PName (p: Proc);
        TName (t: Type);
        Flt (READONLY f: Target.Float);
        Bool (b: BOOLEAN);
        Lab (i: Label);
        Tipe (t: TypeUID);
        Int (i: INTEGER);
        TInt (READONLY i: Target.Int);
        BInt (i: INTEGER);
        Txt (t: TEXT);
        OutC (c: CHAR);
        OutT (txt: TEXT);
        OutN (n: Name);
        OutS (READONLY buf: ARRAY OF CHAR);
        OutI (i: INTEGER);
      END;

PROCEDURE New (wr: Wr.T): T;

END Wrx86.
