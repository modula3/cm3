(* Copyright (C) 1989, 1990 Digital Equipment Corporation	*)
(* All rights reserved.						*)
(* See the file COPYRIGHT for a full description.		*)

(* File: XMachine.i3                                            *)
(* Last modified on Wed Nov 24 15:16:18 PST 1993 by steveg      *)
(*      modified on Fri May  7 16:14:46 PDT 1993 by mjordan     *)


INTERFACE XMachine;

FROM Ctypes IMPORT unsigned_int, unsigned_long;

TYPE
  XID = unsigned_long;
  KeySym = unsigned_int;
  Time = unsigned_int;
  Dimension = unsigned_int;

END XMachine.
