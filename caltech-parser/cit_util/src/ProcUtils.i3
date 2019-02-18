(* $Id$ *)

INTERFACE ProcUtils;
IMPORT Rd, Wr, Pathname;
IMPORT AtomList;
IMPORT OSError;

TYPE
  T = TEXT;
  (* a sequence of newline-separated unix commands with arguments
     also understands "cd",";","<",">",">&","|","|&","'","`"
  *)


(* run the command(s) and return their output text *)

EXCEPTION ErrorExit(Error);

TYPE Error = OBJECT error : TEXT END; 
     (* generic errors, e.g., Rd.Failure *)

     OS = Error BRANDED OBJECT al : AtomList.T; END;
     (* OSError.E *)

     ExitCode = Error OBJECT code : INTEGER END;
     (* process set non-zero exit code *)

PROCEDURE FormatError(e : Error) : TEXT;
  

PROCEDURE ToText(source: T;
                 stderr: Writer := NIL;
                 stdin: Reader := NIL;
                 wd0: Pathname.T := NIL): TEXT RAISES { Rd.Failure, ErrorExit, OSError.E } ;

PROCEDURE RdToRd(source: Rd.T;
                 stderr: Writer := NIL;
                 stdin: Reader := NIL;
                 wd0: Pathname.T := NIL;
                 VAR rd: Rd.T): Completion RAISES { OSError.E } ;

TYPE Completion = OBJECT METHODS wait() RAISES { ErrorExit }; END;
     (* starting a process returns a Completion.  When it is desired to
        join the fork, call completion.wait(), which will raise ErrorExit
        if the process in question has exited or does exit with an error *)

(* for more control over i/o: *)
(* these procedures used to raise ErrorExit, but this was folded into
   Completion, to permit asynchronous delivery of errors, rather than
   crashing the program *)

PROCEDURE Run(source: Rd.T;
              stdout,stderr: Writer := NIL;
              stdin: Reader := NIL;
              wd0: Pathname.T := NIL): Completion;

PROCEDURE RunText(source: TEXT;
              stdout,stderr: Writer := NIL;
              stdin: Reader := NIL;
              wd0: Pathname.T := NIL): Completion;

(* the following are helpers for Reader/Writer threads *)
TYPE
  Reader <: ROOT;
  Writer <: ROOT;

PROCEDURE WriteHere(wr: Wr.T): Writer RAISES { OSError.E } ;
  (* allocate a Writer that writes to wr *)

PROCEDURE GimmeRd(VAR rd: Rd.T): Writer RAISES { OSError.E } ;
  (* allocate a Writer that writes to the read stream of rd *)

PROCEDURE Stdout(): Writer;
  (* allocate a Writer that writes to Stdio.stdout *)

PROCEDURE Stderr(): Writer;
  (* allocate a Writer that writes to Stdio.stderr *)

PROCEDURE ReadHere(rd: Rd.T): Reader  RAISES { OSError.E } ;
  (* allocate a Reader that reads from rd *)

PROCEDURE ReadThis(t: TEXT): Reader  RAISES { OSError.E } ;
  (* allocate a Reader that reads from the TEXT t *)

PROCEDURE GimmeWr(VAR wr: Wr.T): Reader RAISES { OSError.E } ;
  (* allocate a Reader that reads from the output stream of wr *)

PROCEDURE Stdin(): Reader;
  (* allocate a Reader that reads from Stdio.stdin *)

CONST Brand = "ProcUtils";

END ProcUtils.
