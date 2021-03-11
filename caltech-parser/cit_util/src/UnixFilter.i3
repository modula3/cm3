(* $Id$ *)

INTERFACE UnixFilter;
IMPORT Rd, Wr, Pathname;

(* I don't think these routines work properly.  They certainly do not
   handle errors correctly (or at all) *)

PROCEDURE RR(cmd : TEXT; source : Rd.T; wd0 : Pathname.T := NIL) : Rd.T;
  (* read filter *)

PROCEDURE WW(cmd : TEXT; target : Wr.T; wd0 : Pathname.T := NIL) : Wr.T;
  (* write filter *)

PROCEDURE RW(cmd : TEXT; source :Rd.T; target :Wr.T; wd0 : Pathname.T := NIL);
  (* from rd to wr until Rd.EndOfFile *)

END UnixFilter.
