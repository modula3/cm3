(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(* Last modified on Wed Jun 30 17:12:13 PDT 1993 by mcjones                  *)
(*      modified on Tue Mar 16 12:30:07 PST 1993 by mjordan                  *)

INTERFACE FileWin32;

IMPORT File, OSError, Pipe, WinNT;

(* In this interface we reveal that every Win32 "File.T" has a handle
   field, "handle". *)

TYPE T = File.Public OBJECT
    handle: WinNT.HANDLE
  END;

REVEAL File.T <: T;

TYPE
  Direction = {Read, Write};
  DirectionSet = SET OF Direction;

CONST
  Read = DirectionSet{Direction.Read};
  Write = DirectionSet{Direction.Write};
  ReadWrite = DirectionSet{Direction.Read, Direction.Write};

PROCEDURE New(h: WinNT.HANDLE; ds: DirectionSet): File.T RAISES {OSError.E};
(* Create the appropriate subtype of "File.T", based on the
   characteristics of the opened file handle "h" and directions "ds".
   If "WinBase.GetFileType(h) returns "FILE_TYPE_UNKNOWN", the result
   will be "NIL". *)

PROCEDURE NewPipe(h: WinNT.HANDLE; ds: DirectionSet): Pipe.T;
(* Create a "Pipe.T" based on "h" and "ds".  It is an unchecked (but safe)
   error if "h" is not "WinBase.FILE_TYPE_PIPE". *)

END FileWin32.
