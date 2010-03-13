

UNSAFE MODULE OK EXPORTS Main;

IMPORT WinUser, M3toC;

(* "WinUser" defines basic Win32 API user-level calls.
   "M3toC" defines mappings from Modula-3 to C strings. *)

IMPORT Params;

VAR
  message: TEXT := "";
    title: TEXT := "A CM3_IDE Example";

BEGIN

(* Loop through all the parameters, paste them together and call
   "MessageBox" with them. *)

  FOR i := 1 TO Params.Count-1 DO
    message := message & Params.Get(i) & " ";
  END;

  WITH c_msg   = M3toC.SharedTtoS(message),
       c_title = M3toC.SharedTtoS(title)
  DO
    EVAL WinUser.MessageBox(NIL, c_msg, c_title, 0);
    M3toC.FreeSharedS(message, c_msg);
    M3toC.FreeSharedS(title, c_title);
  END; (* with *)

END OK.


