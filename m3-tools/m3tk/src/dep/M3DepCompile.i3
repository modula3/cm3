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

INTERFACE M3DepCompile;

IMPORT M3Context, M3CUnit;
IMPORT M3DepFindFile, M3PathElemList;

PROCEDURE Run(
    c: M3Context.T;
    prev, cur: M3DepFindFile.T;
    compile_dirs: M3PathElemList.T
    ): INTEGER
    RAISES {};
(* Compile world using "c, prev, cur". "prev" and "cur" are handles on the 
state of the units in the external world; the former at some earlier time
than that represented by "cur" (which typically denotes the current state).
Any Errors result in result < 0. Typical usage is to begin with 
"prev=NIL, cur=val" and then "prev=cur; cur=prev.rescan()".
*)

PROCEDURE CompileUnits(c: M3Context.T; ut: M3CUnit.Type; 
    units: REF ARRAY OF TEXT;): INTEGER RAISES {};
(* Manually compile 'ut, units' using 'c', errors result in result < 0. 
A subsequent call of 'Run' will not recompile the given units unless
they are changed after this call. *)

PROCEDURE CompileAll(c: M3Context.T; p: M3DepFindFile.T;
                     compile_dirs: M3PathElemList.T): INTEGER RAISES {};
(* Compile all units associated with 'p', that are not already compiled
in 'c' without error. *)

END M3DepCompile.
