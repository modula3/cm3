(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* FileSys.i3 *)
(* Last modified on Fri Mar  4 14:52:02 PST 1994 by wobber *)

INTERFACE FileSys;

IMPORT TextList, OSError, Rd, Time, Wr;
(* (semi) OS-independent file system types and ops *)

TYPE
  FN = TEXT;

  FilePerm = [0..65535];
  FileType = {Normal, SLink, Dir, Other};
  FileInfo = RECORD
    type: BITS 16 FOR FileType;
    perm: BITS 16 FOR FilePerm;
    length: BITS 32 FOR [-16_7FFFFFFF-1..16_7FFFFFFF]; (* in bytes *)
    date: Time.T;
  END;

TYPE
  Enumeration = TextList.T;

CONST
  FilePermNorm = 1;        (* 644 on Ultrix *)
  FilePermRWX = 2;         (* 755 on Ultrix *)
  FilePermReadOnly = 3;    (* 444 on Ultrix *)
  FilePermRX = 4;          (* 555 on Ultrix *)

TYPE ErrorClass = {NoRoomInFS, IO, Access, Lookup, Other};

PROCEDURE ClassifyError(e: OSError.Code) : ErrorClass;


(* open/close file *)

PROCEDURE OpenRead(fn: FN) : Rd.T RAISES {OSError.E};
PROCEDURE OpenWrite(fn: FN) : Wr.T RAISES {OSError.E};
PROCEDURE OpenAppend(fn: FN) : Wr.T RAISES {OSError.E};


(* main procedures *)

PROCEDURE Enumerate (path: FN): Enumeration RAISES {OSError.E};
  (* *)

PROCEDURE GetInfo(path: FN; followLink: BOOLEAN := FALSE) : FileInfo
   RAISES {OSError.E};
  (* *)

PROCEDURE GetPath (path: FN): FN RAISES {OSError.E};
  (* *)

PROCEDURE MakeDir (path: FN) RAISES {OSError.E};
  (* *)

PROCEDURE SetModifiedDate (path: FN; date: Time.T) RAISES {OSError.E};
  (* *)

PROCEDURE SetMode (path: FN; perm: FilePerm) RAISES {OSError.E};
  (* *)

PROCEDURE Rename (source, dest: FN) RAISES {OSError.E};
  (* *)

PROCEDURE Remove (path: FN; recursive: BOOLEAN := FALSE) RAISES {OSError.E};
  (* iff recursive, we'll remove entire dir, otherwise the target
     must be a non-directory *)

PROCEDURE ReadLink(path: FN) : FN RAISES {OSError.E};
  (* *)

PROCEDURE HardLink(path: FN; referent: FN) RAISES {OSError.E};
  (* *)

PROCEDURE SymLink(path: FN; referent: FN) RAISES {OSError.E};
  (* *)

PROCEDURE CheckAccess (
    path: FN;
    write: BOOLEAN;
    fail: BOOLEAN := FALSE): BOOLEAN
    RAISES {OSError.E};
  (* returns result if not "fail" *)

PROCEDURE FreeDiskSpace(fn: FN) : CARDINAL RAISES {OSError.E};
   (* Returns the number of user 1KB block available in the
      file system volume which contains "fn". *)

END FileSys.
