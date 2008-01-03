(* Copyright (C) 1989-1992, Digital Equipment Corporation                    *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jan 30 14:52:52 PST 1995 by kalsow                   *)
(*      modified on Fri Jul  9 21:30:45 PDT 1993 by mhb                      *)
(*      modified on Fri May 14 20:33:49 PDT 1993 by meehan                   *)
(*      modified on Fri Mar 27 03:02:41 1992 by steveg                       *)
(*      modified on Tue May 15 11:28:23 PDT 1990 by mcjones                  *)
(*      modified on Fri Oct 6 13:58:16 PDT 1989 by brooks                    *)
(*      modified on Sun May 21 19:48:08 PDT 1989 by gidi                     *)

MODULE Rsrc;

IMPORT Bundle, FileRd, OSError, Pathname, Rd, TextRd, Thread;
IMPORT Env, RefList, Text;

PROCEDURE Open (name: TEXT; p: Path): Rd.T RAISES {NotFound} =
  BEGIN
    IF Pathname.Absolute(name) THEN
      IF Pathname.Valid(name) THEN
        TRY RETURN FileRd.Open(name) EXCEPT OSError.E => END
      END;
      RAISE NotFound
    END;
    WHILE p # NIL DO
      TYPECASE p.head OF
      | NULL =>                  <* ASSERT FALSE *>
      | Pathname.T (pn) =>
          TRY
            RETURN FileRd.Open(Pathname.Join(pn, name, NIL))
          EXCEPT
          | OSError.E =>
          END
      | Bundle.T (b) =>
          WITH t = Bundle.Get(b, name) DO
            IF t # NIL THEN RETURN TextRd.New(t) END
          END
      ELSE
        <* ASSERT FALSE *>
      END;
      p := p.tail;
    END;
    RAISE NotFound
  END Open;

PROCEDURE Get (name: TEXT; p: Path): TEXT
  RAISES {NotFound, Rd.Failure, Thread.Alerted} =
  BEGIN
    WHILE p # NIL DO
      TYPECASE p.head OF
      | NULL =>                  <* ASSERT FALSE *>
      | Pathname.T (pn) =>
          VAR rd: Rd.T := NIL;
          BEGIN
            TRY
              rd := FileRd.Open (Pathname.Join (pn, name, NIL))
            EXCEPT
            | OSError.E =>
            END;
            IF rd # NIL THEN
              TRY
                RETURN Rd.GetText (rd, LAST (CARDINAL))
              FINALLY
                Rd.Close (rd)
              END
            END
          END
      | Bundle.T (b) =>
          WITH t = Bundle.Get (b, name) DO IF t # NIL THEN RETURN t END END
      ELSE
        <* ASSERT FALSE *>
      END;
      p := p.tail
    END;
    RAISE NotFound
  END Get;

PROCEDURE BuildPath (a1, a2, a3, a4: REFANY := NIL): Path =
  BEGIN
    RETURN RefList.AppendD (
             Convert (a1),
             RefList.AppendD (
               Convert (a2), RefList.AppendD (Convert (a3), Convert (a4))))
  END BuildPath;

PROCEDURE Convert (a: REFANY): Path =
  BEGIN
    TYPECASE a OF
    | NULL => RETURN NIL
    | Bundle.T (b) => RETURN RefList.List1 (b)
    | TEXT (t) => RETURN ExpandPath (t)
    ELSE                         <* ASSERT FALSE *>
    END
  END Convert;

PROCEDURE ExpandPath (path: TEXT): RefList.T =
  BEGIN
    IF NOT Text.Empty (path) AND Text.GetChar (path, 0) = '$' THEN
      path := Env.Get (Text.Sub (path, 1, LAST (CARDINAL)))
    END;
    IF path = NIL OR Text.Empty (path) THEN
      RETURN NIL
    ELSIF Pathname.Valid (path) THEN
      RETURN RefList.List1 (path)
    ELSE                         <* ASSERT FALSE *>
    END
  END ExpandPath;

BEGIN
END Rsrc.
