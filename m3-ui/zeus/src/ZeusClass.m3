(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Sat Feb  6 01:02:39 PST 1993 by johnh  *)
(*      modified on Tue May 12 04:35:49 1992 by mhb        *)

MODULE ZeusClass;

IMPORT Rd, Wr, ZeusUtil;
IMPORT ReactivityVBT;

REVEAL
   Private = ReactivityVBT.T BRANDED OBJECT END;

REVEAL
  T = Public BRANDED OBJECT
      OVERRIDES
        install    := DefaultInstall;
        delete     := DefaultDelete;
        snapshot   := DefaultSnapshot;
        restore    := DefaultRestore;
        config     := DefaultConfig;
        reactivity := DefaultReactivity;
      END;


PROCEDURE DefaultInstall (<*UNUSED*> v: T) = 
  BEGIN
  END DefaultInstall;

PROCEDURE DefaultDelete (<*UNUSED*> v: T) = 
  BEGIN
  END DefaultDelete;

PROCEDURE DefaultSnapshot (<*UNUSED*> v: T; wr: Wr.T) RAISES {Error} =
  BEGIN
    TRY
      Wr.PutText(wr, "()")
    EXCEPT
    ELSE
      RAISE Error("ZeusClass.DefaultSnapshot write error")
    END;
  END DefaultSnapshot;

PROCEDURE DefaultRestore (<*UNUSED*> v: T; rd: Rd.T)
  RAISES {Error} =
  BEGIN
    IF rd = NIL THEN RETURN END;
    IF NOT (ZeusUtil.EatChar(rd, '(') AND ZeusUtil.EatChar(rd, ')')) THEN
      RAISE Error("ZeusClass.DefaultRestore error")
    END;
  END DefaultRestore;

PROCEDURE DefaultConfig (<*UNUSED*> v: T; 
                         <*UNUSED*> state: StateChange; 
                         <*UNUSED*> object: T) =
  BEGIN
  END DefaultConfig;

PROCEDURE DefaultReactivity (<*UNUSED*> v: T; <*UNUSED*> on: BOOLEAN) = 
  BEGIN
  END DefaultReactivity;

BEGIN
END ZeusClass.
