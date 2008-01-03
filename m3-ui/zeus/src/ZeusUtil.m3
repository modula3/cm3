(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Tue Jan 31 13:24:04 PST 1995 by kalsow *)
(*      modified on Thu Jun 24 12:46:06 PDT 1993 by steveg *)
(*      modified on Sat Feb  6 01:06:39 PST 1993 by johnh  *)

MODULE ZeusUtil;

IMPORT Atom, ASCII, RefList, Point, Rd, Rect, Sx, Text, Thread, Trestle,
       TrestleComm;

<*PRAGMA LL*>
<*FATAL TrestleComm.Failure*>

PROCEDURE KeywordCheck (arg: REFANY; t: TEXT) RAISES {BadSnapshot} =
  (* arg should be a list whose first element is an Atom.T whose name
     is t.  If it isn't, complain. *)
  BEGIN
    IF ISTYPE(arg, RefList.T) AND (arg # NIL)
         AND ISTYPE(RefList.Nth(arg, 0), Atom.T) THEN
      IF NOT Text.Equal(Atom.ToText(RefList.Nth(arg, 0)), t) THEN
        RAISE BadSnapshot("keyword mismatch: " & t);
      END;
    ELSE
      RAISE BadSnapshot("Bad snapshot at keyword: " & t);
    END
  END KeywordCheck;

PROCEDURE ScreenPosOK (scr: Trestle.ScreenID; pt: Point.T): BOOLEAN =
  <* LL = VBT.mu *>
  (* RETURN TRUE iff the screen exists and pt is on it. *)
  VAR screens := Trestle.GetScreens();
  BEGIN
    FOR i := 0 TO LAST(screens^) DO
      IF (scr = screens^[i].id) AND Rect.Member(pt, screens^[i].dom) THEN
        RETURN TRUE
      END;
    END;
    RETURN FALSE;
  END ScreenPosOK;

PROCEDURE RdToList (rd: Rd.T): RefList.T =
  <* LL = arbitrary *>
  (* read one s-expression from rd.  If it's a list, return it, else return
     NIL.  Catch any exceptions and return NIL if one occurs. *)
  BEGIN
    IF rd = NIL THEN RETURN NIL END;
    TRY
      WITH ra = Sx.Read(rd) DO
        TYPECASE ra OF | RefList.T (l) => RETURN l; ELSE RETURN NIL; END;
      END;
    EXCEPT
    | Sx.ReadError, Rd.EndOfFile, Thread.Alerted => RETURN NIL;
    END;
  END RdToList;

PROCEDURE EatChar (rd: Rd.T; c: CHAR): BOOLEAN =
  (* If the next non-white-space character in rd is c, swallow it and
     return TRUE.  Otherwise push it back on the reader and return
     FALSE. *)
  VAR next: CHAR;
  BEGIN
    TRY
      LOOP
        next := Rd.GetChar(rd);
        IF NOT (next IN ASCII.Spaces) THEN EXIT END;
      END;
      IF next = c THEN RETURN TRUE ELSE Rd.UnGetChar(rd); RETURN FALSE END;
    EXCEPT
    | Rd.EndOfFile, Rd.Failure, Thread.Alerted => RETURN FALSE;
    END;
  END EatChar;

BEGIN
END ZeusUtil.
