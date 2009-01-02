(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Uucontext;

IMPORT Usysdep;

TYPE
  sigset_t = Usysdep.sigset_t;
  ucontext_t_star = Usysdep.ucontext_t_star;

END Uucontext.
