(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(* Last modified on Thu Jul 14 17:10:29 PDT 1994 by mcjones                  *)
(*      modified on Sat Feb  6 11:37:20 PST 1993 by mjordan                  *)

INTERFACE FilePosix;

IMPORT File, OSError, Pipe, Ustat;

(* In this interface we reveal that all Unix "File.T"s have a
   file descriptor field, "fd". *)

TYPE T = File.Public OBJECT
    fd: INTEGER;
    ds: DirectionSet
  END;

REVEAL File.T <: T;

TYPE
  Direction = {Read, Write};
  DirectionSet = SET OF Direction;

CONST
  Read = DirectionSet{Direction.Read};
  Write = DirectionSet{Direction.Write};
  ReadWrite = DirectionSet{Direction.Read, Direction.Write};

PROCEDURE New(fd: INTEGER; ds: DirectionSet): File.T RAISES {OSError.E};
(* Create the appropriate subtype of "File.T", based on the characteristics
   of the opened file descriptor "fd" and directions "ds". *)

PROCEDURE NewPipe(fd: INTEGER; ds: DirectionSet): Pipe.T;
(* Create a "Pipe.T" based on "fd" and "ds".  It is an unchecked (but safe)
   error if "fd" is not "S_IFPIPE", "S_IFPORT", or "S_IFSOCK". *)

PROCEDURE FileTypeFromStatbuf(READONLY statbuf: Ustat.struct_stat)
  : File.Type;
(* Return the "File.Type" corresponding mostly closely to
   "Word.And(statbuf.st_mode, Ustat.S_IFMT)". *)
  

END FilePosix.
