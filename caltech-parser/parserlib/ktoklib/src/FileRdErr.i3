(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: FileRdErr.i3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

INTERFACE FileRdErr;
IMPORT Rd;
IMPORT Pathname;
IMPORT TextList;
TYPE
  T = Rd.T;

(* this is FileRdErr.i3 in ktoklib. This interface exists only to avoid
   bootstrapping issues. You should use SeekRd.i3 in parserlib instead.  *)

PROCEDURE Open(p: Pathname.T; searchDirs: TextList.T := NIL): T;
  (*  Like FileRd.Open, but if the file cannot be openned then
      print a meaningful error and exit. also see "E" below.
  
      if searchDirs=NIL then assume p is a full path
      otherwise try list of prefixes
  *)

PROCEDURE E(rd: T; message: TEXT; fatal := TRUE);
  (*  To stderr:
      print file name (if rd was openned using FileRdErr.Open),
      line number (if rd not NIL and seekable),
      and message, and exit. *)

END FileRdErr.
