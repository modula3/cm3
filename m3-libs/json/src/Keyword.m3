UNSAFE MODULE Keyword;

FROM Scanner IMPORT TK, TK_Ident, TK_Null, TK_True, TK_False;

(* Note: the module is unsafe because we use "p^[i]" instead
   of "x[i]", but since "p^" is a fixed length array we avoid
   the runtime subscript checks. *)

PROCEDURE Classify (READONLY x: ARRAY OF CHAR): TK =
  VAR p : UNTRACED REF ARRAY [0..8] OF CHAR := ADR (x[0]);
  VAR n := NUMBER (x);  class := TK_Ident;
  BEGIN
    CASE p[0] OF
    | 'n' =>
              IF    (n = 4)
                AND (p[1] = 'u')
                AND (p[2] = 'l')
                AND (p[3] = 'l') THEN class := TK_Null;
              END;
    | 't' =>
              IF    (n = 4)
                AND (p[1] = 'r')
                AND (p[2] = 'u')
                AND (p[3] = 'e') THEN class := TK_True;
              END;
   | 'f' =>
              IF    (n = 5)
                AND (p[1] = 'a')
                AND (p[2] = 'l')
                AND (p[3] = 's')
                AND (p[4] = 'e') THEN class := TK_False;
              END;
    ELSE (* class := TK_Ident *)
    END;
    RETURN class;
  END Classify;

BEGIN
END Keyword.
