(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright 95 Digital Equipment Corporation.
   Digital Internal Use Only
   Last modified on Tue May  2 20:45:13 PDT 2000 by saxe   
        modified on Fri Mar 15 18:07:28 PST 1996 by detlefs
*)

MODULE OrdNode;

IMPORT Word, FPrint;

PROCEDURE Equal(READONLY on1, on2: T): BOOLEAN =
  BEGIN RETURN on1.e = on2.e
  END Equal;

PROCEDURE Hash(READONLY on: T): Word.T =
  BEGIN RETURN FPrint.Hash(on.e.getFP())
  END Hash;

PROCEDURE Compare(READONLY on1, on2: T): [-1..1] =
  BEGIN RETURN FPrint.Compare(on1.e.getFP(), on2.e.getFP())
  END Compare;

BEGIN
END OrdNode.
