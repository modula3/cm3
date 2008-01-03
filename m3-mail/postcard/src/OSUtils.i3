(* Copyright 1994 Digital Equipment Corporation.               *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Nov  1 09:55:08 PST 1994 by kalsow     *)
(*      modified on Wed Apr 27 14:59:30 PDT 1994 by birrell    *)

(* Miscellanous OS operations for Postcard.  The implementation is likely to
   be system-specific. *)

INTERFACE OSUtils;

IMPORT Rd, TextList, Time, Wr;

EXCEPTION FileError(TEXT);
  (* Raised on failing file or process operations *)


(* *)
(* File system operations *)
(* *)

EXCEPTION FileNotFound;

TYPE FileType = { Normal, Dir, SLink, Other };

PROCEDURE GetInfo(path: TEXT; VAR (*OUT*) mtime: Time.T): FileType
                  RAISES { FileNotFound, FileError };
  (* Checks that the file names "path" exists (if not, raises FileNotFound);
     assigns the file's "last modified" time to mtime.  File system errors
     other than "file not found" are reported by raising "FileError" with a
     human-sensible description of the error. *)

PROCEDURE CheckFile(path: TEXT) RAISES { FileNotFound, FileError };
  (* Checks that the file named "path" exists (if not, raises FileNotFound).
     File system errors other than "file not found" are reported by raising
     "FileError" with a human-sensible description of the error. *)

PROCEDURE OpenRead(path: TEXT): Rd.T RAISES { FileNotFound, FileError };
  (* Checks that the file named "path" exists (if not, raises FileNotFound),
     then opens it for reading.  File system errors other than "file not found"
     are reported by raising "FileError" with a human-sensible description of
     the error. *)

PROCEDURE OpenWrite(path: TEXT; append: BOOLEAN): Wr.T RAISES { FileError };
  (* Opens the file named "path" for writing, creating it if needed. File system
     errors are reported by raising "FileError" with a human-sensible
     description of the error. *)

PROCEDURE Delete(path: TEXT) RAISES { FileError };
  (* Deletes a file (but not a directory).  File system errors are reported
     by raising "FileError" with a human-sensible description of the error. *)

PROCEDURE Rename(srce, dest: TEXT) RAISES { FileError };
  (* Renames a file (but not a directory).  File system errors are reported
     by raising "FileError" with a human-sensible description of the error. *)

PROCEDURE MakeDir(path: TEXT) RAISES { FileError };
  (* Creates the directory.  File system errors are reported by raising
     "FileError" with a human-sensible description of the error. *)

PROCEDURE Enumerate(path: TEXT): TextList.T RAISES { FileError };
  (* Enumerates the directory, omitting "." and "..".  File system errors are
     reported by raising "FileError" with a minimal description of
     the error; for good error messages, call CheckFile first. *)


(* *)
(* Time conversions *)
(* *)

PROCEDURE TimeLocalToText(time: Time.T): TEXT;
  (* Convert a Time.T (in local time) into the following format:
       "Fri May 13 13:13:13 1988"
     All fields are fixed width. *)

TYPE CalendarTime = RECORD
    second: [0 .. 59];
    minute: [0 .. 59];
    hour: [0 .. 23];
    day: [1 .. 31];
    month: [0 .. 11];
    year: CARDINAL;         (* 0 = 1900 *)
    dayOfWeek: [0 .. 6]     (* 0 = Sunday *);
    dayOfYear: [0 .. 365];
    daylightTime: BOOLEAN;  (* true if this time is in DST *)
    gmtOffset: INTEGER;     (* offset from GMT in seconds *)
  END;

PROCEDURE FromTimeLocal(time: Time.T; VAR (*OUT*) t: CalendarTime);
  (* Breaks down a time into calendar parts based on local time. *)
    
PROCEDURE ToTime(READONLY t: CalendarTime): Time.T;
(* Reassembles a broken-down time into a Time.T *)


(* *)
(* Sub-processes *)
(* *)

TYPE
  SubProcess <: REFANY;
  Filter = RECORD
    stdin: Wr.T;
    stdout: Rd.T;
    handle: SubProcess;
  END;

PROCEDURE RunProcess(READONLY argv: ARRAY OF TEXT): Filter RAISES {FileError};
  (* Run the command described by argv.  argv[0] is command name, rest
     is parameters.  Returns handle and one end of pipes that go to the
     sub-process's stdin, stdout, stderr.  stdout+stderr are merged.  Raises
     FileError with human-sensible text if process creation is known to fail *)

PROCEDURE WaitProcess(child: SubProcess): INTEGER;
  (* Blocks until child process terminates.  Any errors are fatal.  Returns
     exit status, 0 on success. *)

END OSUtils.
