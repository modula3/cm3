(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Thu Aug 31 14:22:15 PDT 1995 by steveg  *)

MODULE OSWin32;

IMPORT WinBase, Word;

PROCEDURE Win95(): BOOLEAN =
  BEGIN
    RETURN (Word.And(WinBase.GetVersion(), 16_80000000) # 0);
  END Win95;

BEGIN
END OSWin32.
