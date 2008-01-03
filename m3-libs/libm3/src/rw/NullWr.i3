(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Thu May 13 17:44:33 PDT 1993 by swart          *)
(*      modified on Tue Jun 25 10:52:24 1991 by muller         *)

INTERFACE NullWr;

IMPORT Wr;

TYPE
  T <: Public;
  Public = Wr.T OBJECT METHODS init (): T END;
  (* An initialized NullWr.T returned by NEW(T).init() is an empty
     output stream with

        len (wr) = 0
        c (wr) = ''
        seekable (rd) = FALSE *)

END NullWr.
