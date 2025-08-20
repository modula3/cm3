INTERFACE TermHooks;

TYPE
  CharGetter = OBJECT METHODS get(): CHAR; END;
PROCEDURE SetCharInput(c: CharGetter);
(* Default:
   Wr.Flush(Stdio.stdout);
   Rd.GetChar(Stdio.stdin);
*)

END TermHooks.
