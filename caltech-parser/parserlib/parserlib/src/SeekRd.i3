INTERFACE SeekRd;
IMPORT Rd;
IMPORT Pathname;
IMPORT TextList;
TYPE
  T = Rd.T;

PROCEDURE Open(p: Pathname.T; searchDirs: TextList.T := NIL): T;
  (*  Like FileRd.Open, but if the file cannot be openned then
      print a meaningful error and exit. also see "E" below.
  
      if searchDirs=NIL then assume p is a path,
      otherwise try list of prefixes ("" must be given explicitly)
  *)

PROCEDURE Stdin(): T;
  (* Make stdin seekable *)

PROCEDURE E(rd: T; message: TEXT);
  (*  To stderr:
      print file name (if rd was openned using SeekRd.Open),
      line number (if rd not NIL and seekable),
      and message, and exit. *)

PROCEDURE DiscardPrevious(rd: T);
  (* be allowed to forget everything before cur(rd) *)

END SeekRd. 
