(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last Modified On Tue Nov  1 08:54:05 PST 1994 By kalsow     *)
(*      Modified On Tue Feb 15 10:19:04 PST 1994 By perl       *)

UNSAFE MODULE Test EXPORTS Main;

IMPORT M3toC, Ctypes, Text, TextF, IO, Fmt, Time;

CONST N = BYTESIZE(INTEGER);
VAR buff: ARRAY[0..99] OF CHAR; (* this buffer must be at least 8 bigger
                                   than the size of any argument to MisAlign *)

PROCEDURE MisAlign(t: TEXT; i: CARDINAL): TEXT =
  (* Return a text whose first character's address MOD N is i and whose
     contents is t. Destroys any texts returned by previous calls. *)
  VAR p: UNTRACED REF Ctypes.char := ADR(buff[0]);
      q: Ctypes.char_star;
      res: TEXT;
  BEGIN
    WHILE (LOOPHOLE(p, INTEGER) MOD N) # (i MOD N) DO INC(p) END;
    q := p;
    FOR j := 0 TO Text.Length(t)-1 DO
      p^ := ORD(Text.GetChar(t, j));
      INC(p)
    END;
    p^ := 0;
    res := M3toC.StoT(q);
    <* ASSERT LOOPHOLE(ADR(res[0]), INTEGER) MOD N = i MOD N *>
    RETURN(res)
  END MisAlign;

PROCEDURE CheckAlign () =
  VAR 
    t := "abcdefghijklmnopqrstuvwxyz";
  BEGIN
    FOR j := 0 TO Text.Length(t) DO
      VAR tt := Text.Sub(t, 0, j);
          tthash := Text.Hash(tt);
      BEGIN
        FOR i := 0 TO 7 DO
          <* ASSERT tthash = Text.Hash(MisAlign(tt, i)) *>
        END
      END
    END
  END CheckAlign;

PROCEDURE TimeIt(): REAL =
  VAR 
    t := "abcdefghij";
    start := Time.Now();
  BEGIN
    FOR i := 1 TO 100000 DO
      EVAL Text.Hash(t)
    END;
    VAR done := Time.Now(); BEGIN
      RETURN FLOAT(done - start) * 10.0
    END
  END TimeIt;

PROCEDURE CheckOther() =
  BEGIN
    (* check all the routines that were changed to use SUBARRAY instead
       of memcpy *)
    IF NOT Text.Equal("abcd", Text.Cat("abc", "d"))
      OR NOT Text.Equal("abc", Text.Cat("", "abc"))
      OR NOT Text.Equal("abc", Text.Cat("abc", "")) THEN
      IO.Put("*** Cat is broken\n")
    END;
    IF NOT Text.Equal("cde", Text.Sub("abcdefg", 2, 3)) THEN
      IO.Put("*** Sub is broken\n")
    END;
    VAR a: ARRAY[0..9] OF CHAR; BEGIN
      Text.SetChars(a, "abcd");
      IF a[0] # 'a' OR a[1] # 'b' OR a[2] # 'c' OR a[3] # 'd' THEN
        IO.Put("*** SetChars is broken\n")
      ELSIF NOT Text.Equal(Text.FromChars(SUBARRAY(a, 0, 4)), "abcd") THEN
        IO.Put("*** FromChars is broken\n")
      END
    END
  END CheckOther;
    

BEGIN
  CheckAlign();
  CheckOther();
  IO.Put(Fmt.Real(TimeIt()) & " microseconds per 10 char text.\n")
END Test.
