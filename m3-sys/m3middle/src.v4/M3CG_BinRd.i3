(* Copyright 1996, Critical Mass, Inc.  All rights reserved. *)

INTERFACE M3CG_BinRd;

IMPORT M3CG, Rd;

PROCEDURE Inhale (rd: Rd.T;  cg: M3CG.T);
(* Parse the binary intermediate code M3CG calls from 'rd'
   and call 'cg' to implement them. *)

END M3CG_BinRd.
