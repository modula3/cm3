(* Copyright 1996, Critical Mass, Inc.  All rights reserved. *)

INTERFACE M3CG_BinWr;

IMPORT M3CG, Wr;

PROCEDURE New (wr: Wr.T): M3CG.T;
(* returns a fresh, initialized code generator that writes its
   calls as binary intermediate code on 'wr'.  See M3CG_Binary
   for the binary format.  *)

END M3CG_BinWr.
