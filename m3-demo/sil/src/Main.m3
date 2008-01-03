(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Wed Nov  9 09:10:23 PST 1994 by kalsow    *)

UNSAFE MODULE Main;

IMPORT SilWindow;

BEGIN
  NEW (SilWindow.T).init("WindowSil: ").run();
END Main.
  
  
