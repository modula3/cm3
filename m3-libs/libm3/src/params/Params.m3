(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Tue Mar 16 20:49:42 PST 1993 by mjordan    *)
(*      modified on Tue Dec  1 15:51:35 PST 1992 by mcjones    *)
(*      modified on Wed Mar  4 08:46:27 PST 1992 by kalsow     *)

UNSAFE MODULE Params;

IMPORT RTArgs;

EXCEPTION FatalError; <* FATAL FatalError *>

PROCEDURE Get(n: CARDINAL): TEXT =
  BEGIN
    IF n >= Count THEN RAISE FatalError END;
    RETURN RTArgs.GetArg(n);
  END Get;

BEGIN
  Count := RTArgs.ArgC();
END Params.

