(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson                           *)
(* Last modified on Wed Jun 14 23:14:02 PDT 1995 by najork                   *)
(*      modified on Mon May 10 15:19:43 PDT 1993 by msm                      *)
(*      modified on Tue Mar 10 19:11:14 PST 1992 by steveg                   *)
(*      modified on Mon Feb 24 13:56:12 PST 1992 by muller                   *)
(*      modified on Sat Nov  2 15:59:41 PST 1991 by gnelson                  *)
<*PRAGMA LL*>


MODULE TrestleConf;

PROCEDURE Init (<*UNUSED*> createUser: UserProc) =
  BEGIN 
  END Init;


REVEAL
  User = UserPublic BRANDED OBJECT 
  OVERRIDES 
    register := Register 
  END;


PROCEDURE Register(<*UNUSED*> user: User) =
  BEGIN     
  END Register;


REVEAL
  App = AppPublic BRANDED OBJECT
  OVERRIDES
    init := AppInit;
    destroy := Destroy
  END;


PROCEDURE AppInit(<*UNUSED*> app: App; <*UNUSED*> user: User) =
  BEGIN
  END AppInit;


PROCEDURE Destroy(<*UNUSED*> app: App) =
  BEGIN
  END Destroy;


BEGIN
END TrestleConf.
