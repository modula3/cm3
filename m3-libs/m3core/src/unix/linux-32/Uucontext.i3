(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Uucontext;

FROM Ctypes IMPORT void_star;
FROM Cstddef IMPORT size_t;

TYPE
  (* 1024 bits, with size_t alignment, in glibc, though the kernel only supports 64 or 128 signals *)
  sigset_t = ARRAY[0..31] OF size_t;
  ucontext_t_star = void_star;

CONST
  empty_sigset_t = sigset_t { 0, .. };

END Uucontext.
