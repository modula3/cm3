INTERFACE RepeatMe;
IMPORT Time;
IMPORT Process;

(* call my command line recursively but add execflag as argv[1] *)
(* repeat it until it succeeds *)

PROCEDURE Do(execFlag         : TEXT;
             immediateQuit    : Process.ExitCode := 0;
             READONLY addArgs                    := ARRAY OF TEXT {};
             maxTime                             := 0.0d0;
  ) : BOOLEAN;
  (* returns TRUE if exitcode is 0, FALSE otherwise
     
     crashes if exitcode is immediateQuit but not zero
  *)

PROCEDURE Repeat(execFlag         : TEXT;
                 maxAttempts      : CARDINAL;
                 delay            : Time.T           := 0.0d0;
                 immediateQuit    : Process.ExitCode := 0;
                 READONLY addArgs                    := ARRAY OF TEXT {};
                 maxTime                             := 0.0d0);
  (* make up to maxAttempts attempts to run the program, with the execFlag
     given each time.

     After a failure, wait for delay seconds before attempting again.
  *)

  (* example usage:

     In main program:
  
   VAR
     pp := NEW(ParseParams.T).init(Stdio.stderr);

   BEGIN

     IF NOT pp.keywordPresent("-execute") THEN
       RepeatMe.Repeat("-execute", 10, 1.0d0)
     END;

     (* parse command line normally ... *)

     (* run program ... *)

  *)
  
END RepeatMe.
