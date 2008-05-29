(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* This file is not much filled in. Let's factor commonality
and eliminate dead. Let's not add stuff for usermode threads. *)

INTERFACE Uucontext;

FROM Ctypes IMPORT unsigned, void_star;

TYPE
  sigset_t = unsigned;
  ucontext_t_star = void_star;

END Uucontext.
