(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Apr 12 10:35:02 PDT 1994 by kalsow                   *)

MODULE FilePath;

IMPORT Text, TextF;

CONST
  Up = ARRAY [0..9] OF TEXT {
    "",
    "../",
    "../../",
    "../../../",
    "../../../../",
    "../../../../../",
    "../../../../../../",
    "../../../../../../../",
    "../../../../../../../../",
    "../../../../../../../../../"
  };


PROCEDURE Normalize (dest, src: TEXT): TEXT =
  VAR
    d_len   := LAST (dest^);
    s_len   := LAST (src^);
    m_len   := MIN (d_len, s_len);
    diff    := -1;
    n_extra := 0;
    result  := "";
    i       := 0;
  BEGIN
    (* find the first arc where they differ *)
    WHILE (i < m_len) AND (dest[i] = src[i]) DO
      IF dest[i] = '/' THEN diff := i; END;
      INC (i);
    END;

    (* count the number of extra arcs in the source *)
    WHILE (i < s_len) DO
      IF src[i] = '/' THEN INC (n_extra) END;
      INC (i);
    END;

    (* build the result *)
    WHILE (n_extra > LAST (Up)) DO
      result := result & Up [LAST (Up)];
      DEC (n_extra, LAST (Up));
    END;
    result := result & Up [n_extra] & Text.Sub (dest, diff+1);

    RETURN result;
  END Normalize;

(********************8
PROCEDURE NormalizePath (dest, src: TEXT): TEXT =
  VAR n := 0;  d := dest;  s := src;
  BEGIN
    WHILE (s # NIL) AND (d # NIL) AND (Text.Length (s) > 0)
      AND NOT Text.Equal (s, d) DO
      s := Pathname.Prefix (s);
      d := Pathname.Prefix (d);
      INC (n);
    END;
    s := "";
    FOR i := 2 TO n DO s := s & "../" END;
    RETURN s & Text.Sub (dest, Text.Length (d));
  END NormalizePath;
*********************)

PROCEDURE Classify (path: TEXT): Kind =
  VAR n := NUMBER (path^) - 4;
  BEGIN
    IF (0 <= n) AND (path[n] = '.') THEN
      INC (n);
      IF path[n] = 'i' THEN
        INC (n);
        IF    path[n] = '3' THEN RETURN Kind.I3;
        ELSIF path[n] = 'g' THEN RETURN Kind.IG;
        END;
      ELSIF path[n] = 'm' THEN
        INC (n);
        IF    path[n] = '3' THEN RETURN Kind.M3;
        ELSIF path[n] = 'g' THEN RETURN Kind.MG;
        END;
      END;
    END;
    RETURN Kind.other;
  END Classify;

BEGIN
END FilePath.


