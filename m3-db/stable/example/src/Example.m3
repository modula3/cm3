(* Copyright (C) 1995, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Thu Jan 19 13:58:42 PST 1995 by kalsow     *)

(* \subsection{Driving the stable object}
   The "Example" module creates the "StableFib" object and allows the
   user to drive it from the keyboard.  This application expects one
   parameter on the command line: the pathname to be used to hold the
   stable state.  It assumes that the user has created the root of the 
   pathname (everything up to the last slash (/) in the filename).

   The application prompts the user for one of six commands:
   \begin{itemize}
   \item[s] Initialize the stable state of the Fibonacci object.  The
            application prompts the user for the two stable state variables.
   \item[c] Crank the Fibonacci object.  The application  prompts the
            user for the number of times to crank.
   \item[d] Dispose of the stable state (terminate stability).
   \item[i] Initialize the stable state (restabilize).  Use this command
            only after disposing of the object.
   \item[k] Checkpoint the object.  Make sure that the object is stable
            before using this command.
   \item[q] Quit the example (terminate the application).
   \end{itemize}
*)
MODULE Example EXPORTS Main;
IMPORT Wr, Stdio, Fmt, IO, StableFib, Pathname, StableError, Params;

(* This module handles the "StableError.E" and "IO.Error" exceptions, 
   but loses on all others: *)
<*FATAL ANY*>

PROCEDURE Main() =
  VAR
    stablePath : Pathname.T;
    recovered : BOOLEAN;
    thefib : StableFib.T;
    running : BOOLEAN := TRUE;
    first, second, crank : INTEGER;
  BEGIN
    (* Get the pathname or emit a usage error: *)
    IF (Params.Count = 2) THEN
      stablePath := Params.Get(1) (* one command-line argument: the path *)
    ELSE
      Wr.PutText(Stdio.stdout, 
                 "usage: example <path of stable storage>\n");
      RETURN;
    END;

    TRY

(* Initialize the stable storage: 
*)

      thefib := NEW(StableFib.T).init(stablePath, recovered);

(* Only initialize the stable state if it is not recovered from
   the given path: *)
      IF (recovered) THEN
        Wr.PutText(Stdio.stdout, 
                   Fmt.F("recovered fib from %s\n", stablePath));
      ELSE
        thefib.initS(0, 1);
        Wr.PutText(Stdio.stdout, "brand new fib!\n");
      END;

(* Always initialize the transient state.  This initialization is
   performed after recovering or initializing the stable state,
   because the transient state might depend on the stable state. *)
      thefib.initT();

(* The user interface loop isn't anything special, but it does allow
   the user to drive the state and stability of the "Fib" object.
*)
      WHILE (running) DO

        Wr.PutText(Stdio.stdout, 
                   "init(S), (c)rank, (d)ispose, (i)nit, chec(k) or (q)uit> "
        );
        Wr.Flush(Stdio.stdout);

        CASE (IO.GetChar()) OF
          'q', 'Q' => running := FALSE;
        | 'k', 'K' => StableFib.Checkpoint(thefib);
        | 's', 'S' => 
          Wr.PutText(Stdio.stdout, "  first value> ");
          Wr.Flush(Stdio.stdout);
          first := GetInt();
          Wr.PutText(Stdio.stdout, "  second value> ");
          Wr.Flush(Stdio.stdout);
          second := GetInt();
          thefib.initS(first, second);
        | 'c', 'C' => 
          Wr.PutText(Stdio.stdout, "  times to crank> ");
          Wr.Flush(Stdio.stdout);
          crank := GetInt();
          Wr.PutText(Stdio.stdout, 
                     Fmt.F("the result is %s\n", 
                           Fmt.Int(thefib.crank(crank))));
          Wr.PutText(Stdio.stdout, 
                     Fmt.F("(%s cranks this session)\n", 
                           Fmt.Int(thefib.count())));
        | 'd', 'D' => 
          thefib.dispose();
          Wr.PutText(Stdio.stdout, "stability cancelled\n");
        | 'i', 'I' => 
          thefib := thefib.init(stablePath, recovered);
          thefib.initT();
          IF (recovered) THEN
            Wr.PutText(Stdio.stdout, 
                       Fmt.F("recovered state from %s\n", stablePath));
          ELSE
            Wr.PutText(Stdio.stdout, "state is now stable\n");
          END;
        ELSE (*skip*)
        END;
        EVAL IO.GetLine(); (* ignore the rest of the line *)
      END;
    EXCEPT
      StableError.E => Wr.PutText(Stdio.stderr, 
                                  "bombed out with stable storage error\n");
    END;
    Wr.PutText(Stdio.stdout, "bye!\n");
  END Main;

(* One little feature for the interface: don't bomb out if the user
   enters a bad integer. *)
PROCEDURE GetInt(): INTEGER =
(* (not a particularly interesting function)
   \comment{{ 
*)
  BEGIN
    TRY
      RETURN IO.GetInt();
    EXCEPT
      IO.Error => Wr.PutText(Stdio.stdout, "    not an integer, try again> ");
                  Wr.Flush(Stdio.stdout);
                  EVAL IO.GetLine(); (* ignore the rest of the line *)
                  RETURN GetInt();
    END;
(* }} % end of comment 
*)
  END GetInt;

BEGIN
  Main();
END Example.
