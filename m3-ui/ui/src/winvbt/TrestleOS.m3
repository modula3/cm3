(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Jan 16 16:42:52 PST 1995 by najork                   *)
(*       Created on Mon Jan 16 09:30:58 PST 1995 by najork                   *)


MODULE TrestleOS;

IMPORT WinTrestle;

VAR mu := NEW(MUTEX);
    inited := FALSE;

PROCEDURE Init () =
  BEGIN
    LOCK mu DO
      IF NOT inited THEN
        WinTrestle.Init(); 
        inited := TRUE 
      END 
    END
  END Init;


PROCEDURE UserName (): TEXT =
  BEGIN
    RETURN "Unknown user"
  END UserName;


BEGIN
END TrestleOS.
