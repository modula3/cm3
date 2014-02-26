(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE BuiltinSpecials;

IMPORT Atom, Pickle, Rd, RefList;
IMPORT Text, Text8, Text16, TextLiteral, Text8CString, TextClass, Thread, Wr;

FROM Pickle IMPORT Error, RefID, Special, Reader, Writer;

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

(*-------------------------------------------------------- TEXT specials ---*)

PROCEDURE TextPklWrite (<*UNUSED*> sp: Special;  r: REFANY;  pwr: Writer)
  RAISES {Error, Wr.Failure, Thread.Alerted} =
  TYPE CPtr  = UNTRACED REF ARRAY [0..TextLiteral.MaxBytes] OF CHAR;
  VAR txt := LOOPHOLE (r, TEXT);  cp: CPtr; info: TextClass.Info;
  BEGIN
    TYPECASE (txt) OF
    | TextLiteral.T  => (* ok *)
    | Text8CString.T => (* ok *)
    ELSE
      RAISE Error ("cannot pickle subtypes of TextLiteral.T or Text8CString.T");
    END;
    txt.get_info (info);
    IF info.wide THEN 
      pwr.writeInt (-info.length);
      cp := LOOPHOLE (info.start, CPtr);
      Wr.PutString 
        (pwr.wr, SUBARRAY (cp^, 0, info.length*BYTESIZE(WIDECHAR)));
    ELSE
      pwr.writeInt (info.length);
      cp := LOOPHOLE (info.start, CPtr);
      Wr.PutString (pwr.wr, SUBARRAY (cp^, 0, info.length));
    END;
  END TextPklWrite;

PROCEDURE TextPklRead (<*UNUSED*> sp: Special;  prd: Reader;
                              <*UNUSED*> id: RefID): REFANY
  RAISES {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR len: INTEGER := prd.readInt ();
  BEGIN
    IF (len >= 0)
      THEN RETURN Text8Read (prd, len);
      ELSE RETURN Text16Read (prd, -len);
    END;
  END TextPklRead;

PROCEDURE Text8Read (prd: Reader;  len: INTEGER): TEXT
  RAISES {Error, Rd.Failure, Thread.Alerted} =
  VAR
    xx  : INTEGER;
    txt : Text8.T;
    buf : ARRAY [0..63] OF CHAR;
  BEGIN
    IF (len <= NUMBER (buf)) THEN
      xx := Rd.GetSub (prd.rd, SUBARRAY (buf, 0, len));
      IF xx # len THEN TooShort (); END;
      RETURN Text.FromChars (SUBARRAY (buf, 0, len));
    ELSE
      txt := Text8.Create (len);
      xx := Rd.GetSub (prd.rd, SUBARRAY (txt.contents^, 0, len));
      IF xx # len THEN TooShort (); END;
      RETURN txt;
    END;
  END Text8Read;

PROCEDURE Text16Read (prd: Reader;  len: INTEGER): TEXT
  RAISES {Error, Rd.Failure, Thread.Alerted} =
  TYPE CPtr  = UNTRACED REF ARRAY [0..TextLiteral.MaxBytes] OF CHAR;
  VAR
    xx  : INTEGER;
    txt : Text16.T;
    cp: CPtr; 
    byteLen : INTEGER; 
    buf : ARRAY [0..63] OF WIDECHAR;
  BEGIN
    byteLen := len*BYTESIZE(WIDECHAR); 
    IF (len <= NUMBER (buf)) THEN
      cp := LOOPHOLE(ADR(buf), CPtr); 
      xx := Rd.GetSub (prd.rd, SUBARRAY (cp^, 0, byteLen));
      IF xx # byteLen THEN TooShort (); END;
      RETURN Text.FromWideChars (SUBARRAY (buf, 0, len));
    ELSE
      txt := Text16.Create (len); 
             (* ^Which will be for whatever size WIDECHARs we have. *)
      cp := LOOPHOLE(txt.contents, CPtr); 
      xx := Rd.GetSub (prd.rd, SUBARRAY (cp^, 0, byteLen));
      IF xx # byteLen THEN TooShort (); END;
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
      pwr.writeInt (len);
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
      len := ReadLength (prd);
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

PROCEDURE ReadLength (prd: Reader): CARDINAL
  RAISES {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR len: INTEGER := prd.readInt ();
  BEGIN
    IF len < 0 THEN
      RAISE Error("Pickle.Error: negative length in builtin type");
    END;
    RETURN len;
  END ReadLength;

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

PROCEDURE TooShort ()
  RAISES {Error} =
  BEGIN
    RAISE Error ("EOF encountered while reading a TEXT literal");
  END TooShort;

BEGIN
END BuiltinSpecials.
