(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE Compiler;

PROCEDURE ThisFile (): TEXT         = BEGIN RETURN "";  END ThisFile;
PROCEDURE ThisPath (): TEXT         = BEGIN RETURN "";  END ThisPath;
PROCEDURE ThisLine (): CARDINAL     = BEGIN RETURN 0;   END ThisLine;
PROCEDURE ThisException (): ADDRESS = BEGIN RETURN NIL; END ThisException;

BEGIN
END Compiler.
