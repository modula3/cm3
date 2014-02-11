(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE BuiltinSpecials2;

IMPORT Atom, Pickle2 AS Pickle, PickleStubs, Rd, RefList;
IMPORT Text, Text8, Text16, TextLiteral, Text8CString, TextClass, Thread, Wr;

FROM Pickle2 IMPORT Error, RefID, Special, Reader, Writer;

VAR init_done := FALSE;

PROCEDURE Register () =
  BEGIN
    IF init_done THEN RETURN; END;
    Pickle.RegisterSpecial (NEW (Special, sc := TYPECODE (TextLiteral.T),
                                 write := TextPklWrite, read  := TextPklRead));
    Pickle.RegisterSpecial (NEW (Special, sc := TYPECODE (Text8CString.T),
                                 write := TextPklWrite, read  := TextPklRead));
    Pickle.RegisterSpecial (NEW (Special, sc := TYPECODE (Atom.T),
                                 write := AtomPklWrite, read  := AtomPklRead));
    Pickle.RegisterSpecial (NEW (Special, sc := TYPECODE (RefList.T),
                                 write := ListPklWrite, read  := ListPklRead));
    init_done := TRUE;
  END Register;

(*-------------------------------------------------------- TextLiteral.T ---*)

PROCEDURE TextPklWrite (<*UNUSED*> sp: Special;  r: REFANY;  pwr: Writer)
  RAISES {Error, Wr.Failure, Thread.Alerted} =
  TYPE CPtr  = UNTRACED REF ARRAY [0..TextLiteral.MaxBytes] OF CHAR;
  TYPE WCPtr = UNTRACED REF ARRAY 
                 [0..TextLiteral.MaxBytes DIV BYTESIZE(WIDECHAR)] 
               OF WIDECHAR;
  VAR txt := LOOPHOLE (r, TEXT);  cp: CPtr;  wcp: WCPtr;  info: TextClass.Info;
  BEGIN
    TYPECASE (txt) OF
    | TextLiteral.T  => (* ok *)
    | Text8CString.T => (* ok *)
    ELSE
      RAISE Error ("cannot pickle subtypes of TextLiteral.T or Text8CString.T");
    END;
    txt.get_info (info);
    IF info.wide THEN
      PickleStubs.OutInteger (pwr, -info.length);
      wcp := LOOPHOLE (info.start, WCPtr);
      PickleStubs.OutWideChars (pwr, SUBARRAY (wcp^, 0, info.length));
    ELSE
      PickleStubs.OutInteger (pwr, info.length);
      cp := LOOPHOLE (info.start, CPtr);
      PickleStubs.OutChars (pwr, SUBARRAY (cp^, 0, info.length));
    END;
  END TextPklWrite;

PROCEDURE TextPklRead (<*UNUSED*> sp: Special;  prd: Reader;
                        <*UNUSED*> id: RefID): REFANY
  RAISES {Error, Rd.Failure, Thread.Alerted} =
  VAR len := PickleStubs.InInteger (prd);
  BEGIN
    IF (len >= 0)
      THEN RETURN Text8Read (prd, len);
      ELSE RETURN Text16Read (prd, -len);
    END;
  END TextPklRead;

PROCEDURE Text8Read (prd: Reader;  len: INTEGER): TEXT
  RAISES {Error, Rd.Failure, Thread.Alerted} =
  VAR
    txt : Text8.T;
    buf : ARRAY [0..63] OF CHAR;
  BEGIN
    IF (len <= NUMBER (buf)) THEN
      PickleStubs.InChars (prd, SUBARRAY (buf, 0, len));
      RETURN Text.FromChars (SUBARRAY (buf, 0, len));
    ELSE
      txt := Text8.Create (len);
      PickleStubs.InChars (prd, SUBARRAY (txt.contents^, 0, len));
      RETURN txt;
    END;
  END Text8Read;

PROCEDURE Text16Read (prd: Reader;  len: INTEGER): TEXT
  RAISES {Error, Rd.Failure, Thread.Alerted} =
  VAR
    txt : Text16.T;
    buf : ARRAY [0..63] OF WIDECHAR;
  BEGIN
    IF (len <= NUMBER (buf)) THEN
      PickleStubs.InWideChars (prd, SUBARRAY (buf, 0, len));
      RETURN Text.FromWideChars (SUBARRAY (buf, 0, len));
    ELSE
      txt := Text16.Create (len);
      PickleStubs.InWideChars (prd, SUBARRAY (txt.contents^, 0, len));
      RETURN txt;
    END;
  END Text16Read;

(*--------------------------------------------------------------- Atom.T ---*)

PROCEDURE AtomPklWrite (<*UNUSED*> sp: Special;  r: REFANY;  pwr: Writer)
  RAISES {Error, Wr.Failure, Thread.Alerted } =
  BEGIN
    pwr.write (Atom.ToText (r));
  END AtomPklWrite;

PROCEDURE AtomPklRead (<*UNUSED*> sp: Special;  prd: Reader;
                       <*UNUSED*> id: RefID): REFANY
  RAISES { Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  BEGIN
    RETURN Atom.FromText (prd.read ());
  END AtomPklRead;

(*------------------------------------------------------------ RefList.T ---*)
(* Iterative List Special to prevent small stacks from 
   overflowing on RefList.Ts of length >~ 25. *)

PROCEDURE ListPklWrite (sp: Special;  r: REFANY;  pwr: Writer)
  RAISES { Error, Wr.Failure, Thread.Alerted } =
  VAR
    l: RefList.T := r;
    len := RefList.Length (l);
  BEGIN
    IF PutSubtypeMark (r, TYPECODE (RefList.T), pwr) THEN
      (* we don't know how to marshal subtypes of RefList.T *)
      Special.write (sp, r, pwr);
    ELSE
      PickleStubs.OutCardinal (pwr, len);
      FOR i := 1 TO len DO
        pwr.write (l.head);
        l := l.tail;
      END;
      <* ASSERT l = NIL *>
    END;
  END ListPklWrite;

PROCEDURE ListPklRead (sp: Special;  prd: Reader;  id: RefID) : REFANY
  RAISES { Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted } = 
  VAR
    len: CARDINAL;
    res, tail: RefList.T;
  BEGIN
    IF GetSubtypeMark (prd) THEN
      (* the writer encountered a subtype of RefList.T *)
      res := Special.read (sp, prd, id);
    ELSE
      len := PickleStubs.InCardinal (prd);
      res := NEW (RefList.T);
      tail := res;
      FOR i := 1 TO len - 1 DO
        tail.head := prd.read ();
        tail.tail := NEW (RefList.T);
        tail := tail.tail;
      END;
      tail.head := prd.read ();
      tail.tail := NIL;
    END;
    RETURN res;
  END ListPklRead;

(*------------------------------------------------------------- internal ---*)

PROCEDURE PutSubtypeMark (r: REFANY;  tcode: INTEGER;  pwr: Writer): BOOLEAN
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR isSubtype := TYPECODE (r) # tcode;
  BEGIN
    Wr.PutChar (pwr.wr, VAL (ORD (isSubtype), CHAR));
    RETURN isSubtype;
  END PutSubtypeMark;

PROCEDURE GetSubtypeMark (prd: Reader): BOOLEAN
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  CONST TrueMark = VAL (ORD (TRUE), CHAR); 
  BEGIN
    RETURN Rd.GetChar (prd.rd) = TrueMark;
  END GetSubtypeMark;

BEGIN
END BuiltinSpecials2.
