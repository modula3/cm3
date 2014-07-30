(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE TextLiteral EXPORTS TextLiteral, RTHooks;

IMPORT TextClass, RTMisc;

TYPE
  CPtr  = UNTRACED REF CHAR;
  WCPtr = UNTRACED REF WIDECHAR;

PROCEDURE TextLitInfo (t: T;  VAR info: TextClass.Info) =
  BEGIN
    info.start  := ADR (t.buf[0]);
    info.length := ABS (t.cnt);
    info.wide   := (t.cnt < 0);
  END TextLitInfo;

PROCEDURE TextLitGetChar (t: T;  i: CARDINAL): CHAR =
  BEGIN
    IF (t.cnt >= 0) THEN
      IF i >= t.cnt THEN (* force a subscript fault *) i := MaxBytes;  END;
      RETURN VAL (t.buf[i], CHAR);
    ELSE
      IF i >= -t.cnt THEN (* force a subscript fault *) i := MaxBytes;  END;
      VAR p := LOOPHOLE (ADR (t.buf[i*BYTESIZE (WIDECHAR)]), WCPtr); BEGIN
      (* RETURN VAL (Word.And (ORD (p^), 16_ff), CHAR); (*CHOP*) *) 
         RETURN VAL (ORD (p^), CHAR); (* Possible range error. *) 
      END;
    END;
  END TextLitGetChar;

PROCEDURE TextLitGetWideChar (t: T;  i: CARDINAL): WIDECHAR =
  BEGIN
    IF (t.cnt >= 0) THEN
      IF i >= t.cnt THEN (* force a subscript fault *) i := MaxBytes;  END;
      RETURN VAL (t.buf[i], WIDECHAR);
    ELSE
      IF i >= -t.cnt THEN (* force a subscript fault *) i := MaxBytes;  END;
      VAR p := LOOPHOLE (ADR (t.buf[i*BYTESIZE (WIDECHAR)]), 
                         UNTRACED REF WIDECHAR); 
      BEGIN
        RETURN p^;
      END;
    END;
  END TextLitGetWideChar;

PROCEDURE TextLitGetChars (t: T;  VAR a: ARRAY OF CHAR;  start: CARDINAL) =
  VAR n: INTEGER;
  BEGIN
    IF (t.cnt >= 0) THEN
      n := MIN (NUMBER (a), t.cnt - start);
      IF (n > 0) THEN
        RTMisc.Copy (ADR (t.buf[start]), ADR (a[0]), n * BYTESIZE (CHAR));
      END;
    ELSE
      n := MIN (NUMBER (a), (-t.cnt) - start);
      IF (n > 0) THEN
        VAR
          tp := LOOPHOLE (ADR (t.buf[start*BYTESIZE (WIDECHAR)]), WCPtr);
          ap := LOOPHOLE (ADR (a[0]), CPtr);
        BEGIN
          WHILE (n > 0) DO
          (* ap^ := VAL (Word.And (ORD (tp^), 16_ff), CHAR); (*CHOP*) *) 
            ap^ := VAL(ORD (tp^), CHAR); (* Possible range error. *) 
            INC (tp, ADRSIZE (tp^));  INC (ap, ADRSIZE (ap^));  DEC (n);
          END;
        END;
      END;
    END;
  END TextLitGetChars;

PROCEDURE TextLitGetWideChars 
  (t: T;  VAR a: ARRAY OF WIDECHAR;  start: CARDINAL) =
  VAR n: INTEGER;
  BEGIN
    IF (t.cnt >= 0) THEN
      n := MIN (NUMBER (a), t.cnt - start);
      IF (n > 0) THEN
        VAR
          tp := LOOPHOLE (ADR (t.buf[start]), CPtr);
          ap := LOOPHOLE (ADR (a[0]), WCPtr);
        BEGIN
          WHILE (n > 0) DO
            ap^ := VAL (ORD (tp^), WIDECHAR);
            INC (tp, ADRSIZE (tp^));  INC (ap, ADRSIZE (ap^));  DEC (n);
          END;
        END;
      END;
    ELSE
      n := MIN (NUMBER (a), (-t.cnt) - start);
      IF (n > 0) THEN
        RTMisc.Copy (ADR (t.buf[start*BYTESIZE (WIDECHAR)]), ADR (a[0]), 
                     n * BYTESIZE (WIDECHAR));
      END;
    END;
  END TextLitGetWideChars;

BEGIN
END TextLiteral.
