

UNSAFE MODULE OK EXPORTS Main;
IMPORT WinUser, M3toC;
(* "WinUser" defines basic Win32 API user-level calls.
   "M3toC" defines mappings from Modula-3 to C strings. *)

IMPORT Params;

VAR
  message: TEXT := "";

BEGIN

(* Loop through all the parameters, paste them together and call
   "MessageBox" with them. *)

  FOR i := 1 TO Params.Count-1 DO
    message := message & Params.Get(i) & " ";
  END;
  EVAL WinUser.MessageBox(NIL,
                          M3toC.TtoS(message), 
                          M3toC.TtoS("A CM3_IDE Example"),
                          0);


END OK.


