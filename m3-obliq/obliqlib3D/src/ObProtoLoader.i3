(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Jul 10 16:14:25 PDT 1995 by najork                   *)
(*       Created on Sat Jun  4 17:25:52 PDT 1994 by najork                   *)


INTERFACE ObProtoLoader;

IMPORT Bundle, Obliq;

TYPE
  T <: Public;
  Public = OBJECT
  METHODS
    init (bundle: Bundle.T) : T;
    load (filename : TEXT);
    get (qualname : TEXT) : Obliq.Val;
  END;


END ObProtoLoader.
