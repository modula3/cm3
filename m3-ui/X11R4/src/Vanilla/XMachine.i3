(* Copyright (C) 1989, 1990 Digital Equipment Corporation	*)
(* All rights reserved.						*)
(* See the file COPYRIGHT for a full description.		*)

(* File: XMachine.i3                                            *)
(* Last modified on Wed Nov 24 15:16:21 PST 1993 by steveg      *)
(*      modified on Fri May  7 16:14:46 PDT 1993 by mjordan     *)


INTERFACE XMachine;

FROM Ctypes IMPORT unsigned_long, unsigned_short;

TYPE
  XID = unsigned_long;
  KeySym = XID;
  Time = unsigned_long;
  Dimension = unsigned_short;

END XMachine.
