(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved.   *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE QIdent EXPORTS QIdent, Quake;

IMPORT Quake, QToken;

TYPE TK = QToken.T;

PROCEDURE NewIDMap (str2id : PROCEDURE (READONLY x: ARRAY OF CHAR): Quake.ID;
                    txt2id : PROCEDURE (t: TEXT): Quake.ID;
                    id2txt : PROCEDURE (i: Quake.ID): TEXT        ): Quake.IDMap =
  VAR
    map := NEW (Quake.IDMap);
    id  : Quake.ID;
    ids : ARRAY [TK.And .. TK.Return ] OF Quake.ID;
  BEGIN
    map.str2id := str2id;
    map.txt2id := txt2id;
    map.id2txt := id2txt;

    (* build the keyword map *)
    map.min_keyword := LAST (Quake.ID);
    map.max_keyword := FIRST (Quake.ID);
    FOR tk := TK.And TO TK.Return DO
      id := map.txt2id (QToken.Name [tk]);
      ids [tk] := id;
      map.min_keyword := MIN (map.min_keyword, id);
      map.max_keyword := MAX (map.min_keyword, id);
    END;
    <*ASSERT map.max_keyword - map.min_keyword < NUMBER (map.keywords)*>
    FOR i := FIRST (map.keywords) TO LAST (map.keywords) DO
      map.keywords[i] := TK.Name;
    END;
    FOR tk := TK.And TO TK.Return DO
      map.keywords [ids[tk] - map.min_keyword] := tk;
    END;

    map.boolean [FALSE] := map.txt2id ("");
    map.boolean [TRUE]  := map.txt2id ("TRUE");

    RETURN map;
  END NewIDMap;

BEGIN
END QIdent.
