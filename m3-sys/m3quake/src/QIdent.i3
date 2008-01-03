(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved.   *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE QIdent;

IMPORT Quake, QToken;

REVEAL
  Quake.IDMap = BRANDED "Quake.IDMap" REF RECORD
    (* READONLY *)
    str2id      : PROCEDURE (READONLY x: ARRAY OF CHAR): Quake.ID;
    txt2id      : PROCEDURE (t: TEXT): Quake.ID;
    id2txt      : PROCEDURE (i: Quake.ID): TEXT;
    min_keyword : Quake.ID;
    max_keyword : Quake.ID;
    keywords    : ARRAY [0..99] OF QToken.T;
    boolean     : ARRAY BOOLEAN OF Quake.ID;
  END;

END QIdent.
