(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* by Stephen Harrison                                       *)
(*                                                           *)
(* Last modified on Wed Nov  9 17:07:11 PST 1994 by kalsow   *)
(*      modified on Tue Mar 23 18:08:46 PST 1993 by harrison *)

MODULE WinVer;

IMPORT WinUser;

BEGIN
  VS_FILE_INFO := WinUser.RT_VERSION;
END WinVer.
