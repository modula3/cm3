(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3PathElemPosix EXPORTS M3PathElemOS, M3PathElemOSPriv;

IMPORT Atom, Env, Text, TextExtras;

REVEAL T = BRANDED REF Atom.T;

PROCEDURE Uid(dir: TEXT): T=
  VAR result := NEW(T);
  BEGIN
    result^ := Atom.FromText(dir);
    RETURN result
  END Uid;

PROCEDURE Equal(t1, t2: T): BOOLEAN=
  BEGIN
    RETURN t1^ = t2^;
  END Equal;

PROCEDURE EnvExpand(e: TEXT): TEXT=
(* Expand environment variables in a TEXT.  The environment vars are
 * enclosed $(thusly).  If the surounded text is not an environment var,
 * it expands to the null string.  Unmatched or nested $()'s cause trouble.
 * e.g.  'abc$(TERM)def' -> 'abcsundef'; 'abc$(foo)def' -> 'abcdef';
 * 'abc$(def$(TERM)ghi' -> 'abcghi'; etc.
 *)
  VAR
    res: TEXT := "";
    start, end: CARDINAL := 0;
  BEGIN
    WHILE start < Text.Length(e) DO
      IF TextExtras.FindSub(e, "$(", start) THEN
        res := res & TextExtras.Extract(e, end, start);
        start := start + 2;
        end := start;
        IF TextExtras.FindChar(e, ')', end) THEN (* found $(xxx) *)
          WITH env = Env.Get(TextExtras.Extract(e, start, end)) DO
            IF env # NIL THEN res := res & env; END;
          END;
          INC(end);
        ELSE (* unmatched $( *)
          res := res & "$(" & TextExtras.Extract(e, start, Text.Length(e));
        END; (* if $(xxx) found *)
        start := end;
      ELSE (* no $( (done, grab rest) *)
        res := res & TextExtras.Extract(e, end, Text.Length(e));
      END; (* if $( found *)
    END; (* while still to expand *)
    RETURN res;
  END EnvExpand;



BEGIN
  SCurrentS := "/./"; SParentS := "/../"; PathSeparator := ':';
  PathnameSeparator := '/';
END M3PathElemPosix.
