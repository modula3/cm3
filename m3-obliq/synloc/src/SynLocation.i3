(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Mon Jul 26 04:07:03 PDT 1993 by luca               *)

INTERFACE SynLocation;
IMPORT SynWr;

TYPE
  T <: ROOT;
  (* "Location.T" represents a location in a source file where an error
     may occur. Line numbers and character numbers start at 1.
     There are two styles of error reporting and two
     corresponding styles of locations: (1) "line-style", which
     reports the file name, the line position within the file, and
     the character position within the line; (2) "selection-style",
     which reports the file name and two character positions within
     the file. The choice between these styles is made by using
     either (1) the NewLineLocation or (2) the NewCharLocation
     procedure below.
  *)

  Located =
    BRANDED "Located" OBJECT
      location: T := NIL;
    END;
  (* A located object, to be subtyped. *)

  Info =
    RECORD
      fileName: TEXT;
      char: INTEGER;
      line, lineChar: INTEGER;
    END;
(* "Info" represents a location in a file. "char" is the character 
   position within the file. "line" is  the line position within the file, 
   and "lineChar" is the character position within that line. *)

PROCEDURE PackageSetup();
(* To be called at least once before any other use of the synloc package. *)

VAR (*READONLY*) noLocation: T;
(* No particular location. *)

PROCEDURE NewLocation(where: TEXT): T;
(* The location is described simply by the parameter "where" *)

PROCEDURE NewLineLocation(READONLY info: Info): T;
(* Creates a new "line-style" error location from a Info that
   specifyies "fileName", "line", and "lineChar"; "char" is not used.
   If "fileName" is empty, this indicates "top-level" interaction, and
   the position information is relative to the beginning of the 
   interactive session. *)

PROCEDURE NewCharLocation(READONLY begInfo, endInfo: Info):  T;
(* Creates a new "selection-style" error location from a pair of Info
   each specifying "fileName", and "char"; "line", and "lineChar" are not used.
   If "fileName" is empty, this indicates "top-level" interaction, and
   the position information is relative to the beginning of the 
   interactive session. *)

PROCEDURE PrintLocation(swr: SynWr.T; location: T);
(* Prints the location of an error, in "line-style" or "selection-style"
   as determined by "location". *)

PROCEDURE PrintLineDifference(swr: SynWr.T; location: T; currentLine: INTEGER);
(* This is used only for "line-style" errors that occur during top-level 
   interaction; "currentLine" should provide the current line number from 
   the beginning of the interactive session. Then, PrintLineDifference 
   generates location messages such as "last input line" or "input line -5". *)

END SynLocation.
