(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu Jan 19 00:10:22 PST 1995 by najork                   *)
(*       Created on Wed Jan 18 14:27:10 PST 1995 by najork                   *)


MODULE WinAux;

IMPORT VBT, VBTClass, WinDef, WinTrestle;

PROCEDURE WindowHandle (v: VBT.T): WinDef.HWND =
  BEGIN
    WHILE v # NIL DO
      TYPECASE v.upRef OF
      | WinTrestle.Child (ch) => 
          IF ch # NIL THEN 
            RETURN ch.hwnd
          END
      ELSE
        (* skip *)
      END;
      v := v.parent;
    END;
    RETURN NIL;
  END WindowHandle;

BEGIN
END WinAux.
