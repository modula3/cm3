(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon May 24 16:19:42 PDT 1993 by swart                    *)
(*      modified on Mon Feb 15 12:16:29 PST 1993 by mjordan                  *)
(*      modified on Mon Jun 22 17:07:44 PDT 1992 by muller                   *)
(*      modified on Mon Jun  8 11:40:56 PDT 1992 by meehan                   *)

UNSAFE MODULE RdUtils;

IMPORT ASCII, Rd, TextF, Atom, Wr;
IMPORT Thread, TextWr, AtomList;

PROCEDURE Find (rd           : Rd.T;
                pattern      : TEXT;
                canonicalize: Canonicalize := NIL):
  [-1 .. LAST(CARDINAL)] RAISES {Rd.Failure, Thread.Alerted} =
  BEGIN
    RETURN
      FindString (rd, SUBARRAY (pattern^, 0, NUMBER (pattern^) - 1),
                  canonicalize);

  END Find;

PROCEDURE FindString (         rd          : Rd.T;
                      READONLY pattern     : ARRAY OF CHAR;
                               canonicalize: Canonicalize    := NIL):
  [-1 .. LAST(CARDINAL)] RAISES {Rd.Failure, Thread.Alerted} =
  VAR i: CARDINAL;
  <*FATAL Rd.EndOfFile*>
  BEGIN
    WITH end = NUMBER(pattern) DO
      IF end = 0 THEN
        RETURN Rd.Index(rd);
      ELSE
        LOOP
          IF FindChar(rd, pattern[0], canonicalize) = -1 THEN
            RETURN -1
          ELSE
            WITH restart = Rd.Index(rd) DO
              i := 1;
              LOOP
                IF i = end THEN
                  RETURN restart - 1
                ELSE
                  IF Rd.EOF(rd) THEN RETURN -1; END;
                  WITH x = Rd.GetChar(rd),
                       y = pattern[i]      DO
                    IF x = y OR (canonicalize # NIL
                                   AND canonicalize(x) = canonicalize(y)) THEN
                      INC(i)
                    ELSE
                      Rd.Seek(rd, restart);
                      EXIT       (* to outer loop *)
                    END          (* IF x = y ... *)
                  END            (* WITH x ... *)
                END              (* IF i = end ... *)
              END                (* inner LOOP *)
            END                  (* WITH restart ... *)
          END                    (* IF FindChar ... *)
        END                      (* outer LOOP *)
      END
    END
  END FindString;

PROCEDURE FindChar (rd           : Rd.T;
                    pattern      : CHAR;
                    canonicalize: Canonicalize := NIL):
  [-1 .. LAST(CARDINAL)] RAISES {Rd.Failure, Thread.Alerted} =
  VAR uc: CHAR;
   <*FATAL Rd.EndOfFile*>
  BEGIN
    IF canonicalize # NIL THEN uc := canonicalize(pattern); END;
    LOOP
      IF Rd.EOF(rd) THEN RETURN -1; END;
      WITH c = Rd.GetChar(rd) DO
        IF c = pattern OR (canonicalize # NIL AND canonicalize(c) = uc) THEN
          RETURN Rd.Index(rd) - 1
        END
      END
    END
  END FindChar;

PROCEDURE ToUpperCaseASCII (ch: CHAR): CHAR =
  BEGIN
    IF ch IN ASCII.All THEN RETURN ASCII.Upper[ch] ELSE RETURN ch END
  END ToUpperCaseASCII;

PROCEDURE FailureText (f: AtomList.T): TEXT =
  <*FATAL Wr.Failure*>
  <*FATAL Thread.Alerted*>
  BEGIN
    IF f = NIL THEN RETURN "NIL" END;
    IF f.tail = NIL THEN RETURN Atom.ToText(f.head); END;
    WITH wr = NEW(TextWr.T).init() DO
      LOOP
        Wr.PutText(wr, Atom.ToText(f.head));
        f := f.tail;
        IF f = NIL THEN EXIT; END;
        Wr.PutChar(wr, ':');
      END;
      RETURN TextWr.ToText(wr);
    END
  END FailureText;

BEGIN
END RdUtils.
