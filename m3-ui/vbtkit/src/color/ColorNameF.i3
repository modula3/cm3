(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Created by meehan on Fri Aug 14 22:09:04 PDT 1992                         *)
(* Last modified on Fri May 14 16:59:05 PDT 1993 by meehan                   *)
(*      modified on Tue Nov 24 22:09:23 PST 1992 by mhb                      *)


INTERFACE ColorNameF;

IMPORT Color, ColorName, TextIntTbl, TextRefTbl;

VAR table: TextIntTbl.T;
(* Maps NormalizeName(name) to index into Basic.  Used for name -> color. *)

TYPE Cache = MUTEX OBJECT table: TextRefTbl.T END;

VAR nameCache: Cache;
(* cache of normalized names that have been looked up: *)
  
PROCEDURE NormalizeName (a: TEXT): TEXT;
(* Deletes all whitespace in "a" and converts to lower case *)

TYPE NotInTable = PROCEDURE (name: TEXT): Color.T RAISES {ColorName.NotFound};

PROCEDURE LowerCaseToRGB (name: TEXT; p: NotInTable): Color.T
  RAISES {ColorName.NotFound};

END ColorNameF.
