(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Aug 21 15:00:02 PDT 1992 by heydon                   *)

INTERFACE AtomJVTbl;

(* This is a temporary interface until the interface police produce one of
   their own or a generic table that can be adopted to fit this need.
*)

IMPORT Atom, JunoValue AS Value;

TYPE
  T <: Public;
  Public = OBJECT METHODS
    init(size: INTEGER := 10): T;
    get(a: Atom.T; VAR (* OUT *) p: Value.T): BOOLEAN;
    put(a: Atom.T; p: Value.T): BOOLEAN;
    delete(a: Atom.T; VAR (* OUT *) p: Value.T): BOOLEAN;
    map(p: MapProc)
  END;

  MapProc = PROCEDURE(a: Atom.T; v: Value.T);

END AtomJVTbl.
