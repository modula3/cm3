(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Tue Jan 31 11:42:17 PST 1995 by kalsow   *)
(*      modified on Thu Sep 23 09:09:04 PDT 1993 by mhb      *)
(*      modified on Fri Jul 31 15:39:50 PDT 1992 by sclafani *)

MODULE DataView;

IMPORT Fmt, FormsVBT, TextWr, Thread, Wr, VBT;
IMPORT Text AS TextOps;

<* FATAL Wr.Failure, Thread.Alerted, FormsVBT.Unimplemented *>

REVEAL
  T = Public BRANDED OBJECT
      OVERRIDES
        setInteger              := SetInteger;
        setBoolean              := SetBoolean;
        setChar                 := SetChar;
        setReal                 := SetReal;
        setLongReal             := SetLongReal;
        setText                 := SetText;
        setIntegerArray         := SetIntegerArray;
        setBooleanArray         := SetBooleanArray;
        setCharArray            := SetCharArray;
        setRealArray            := SetRealArray;
        setLongRealArray        := SetLongRealArray;
        setTextArray            := SetTextArray;
        setIntegerArray2        := SetIntegerArray2;
        setBooleanArray2        := SetBooleanArray2;
        setCharArray2           := SetCharArray2;
        setRealArray2           := SetRealArray2;
        setLongRealArray2       := SetLongRealArray2;
        setTextArray2           := SetTextArray2;
        setIntegerPair          := SetIntegerPair;
        setIntegerPairArray     := SetIntegerPairArray;
        setIntegerTriple        := SetIntegerTriple;
        setIntegerTripleArray   := SetIntegerTripleArray;
        setIntegerList          := SetIntegerList;
        setIntegerListArray     := SetIntegerListArray;
        setRealList             := SetRealList;
        setRealListArray        := SetRealListArray;
        setIntegerPairList      := SetIntegerPairList;
        setIntegerPairListArray := SetIntegerPairListArray;
        setIntegerTree          := SetIntegerTree;

        setIntegerL              := SetIntegerL;
        setBooleanL              := SetBooleanL;
        setCharL                 := SetCharL;
        setRealL                 := SetRealL;
        setLongRealL             := SetLongRealL;
        setTextL                 := SetTextL;
        setIntegerArrayL         := SetIntegerArrayL;
        setBooleanArrayL         := SetBooleanArrayL;
        setCharArrayL            := SetCharArrayL;
        setRealArrayL            := SetRealArrayL;
        setLongRealArrayL        := SetLongRealArrayL;
        setTextArrayL            := SetTextArrayL;
        setIntegerArray2L        := SetIntegerArray2L;
        setBooleanArray2L        := SetBooleanArray2L;
        setCharArray2L           := SetCharArray2L;
        setRealArray2L           := SetRealArray2L;
        setLongRealArray2L       := SetLongRealArray2L;
        setTextArray2L           := SetTextArray2L;
        setIntegerPairL          := SetIntegerPairL;
        setIntegerPairArrayL     := SetIntegerPairArrayL;
        setIntegerTripleL        := SetIntegerTripleL;
        setIntegerTripleArrayL   := SetIntegerTripleArrayL;
        setIntegerListL          := SetIntegerListL;
        setIntegerListArrayL     := SetIntegerListArrayL;
        setRealListL             := SetRealListL;
        setRealListArrayL        := SetRealListArrayL;
        setIntegerPairListL      := SetIntegerPairListL;
        setIntegerPairListArrayL := SetIntegerPairListArrayL;
        setIntegerTreeL          := SetIntegerTreeL;
      END;

PROCEDURE SetInteger (t: T; var: TEXT; val: Integer) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN Stuff (t, var, FmtInteger (val)); END;
  END SetInteger;

PROCEDURE SetBoolean (t: T; var: TEXT; val: Boolean) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN Stuff (t, var, FmtBoolean (val)); END;
  END SetBoolean;

PROCEDURE SetChar (t: T; var: TEXT; val: Char) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN Stuff (t, var, FmtChar (val)); END;
  END SetChar;

PROCEDURE SetReal (t: T; var: TEXT; val: Real) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN Stuff (t, var, FmtReal (val)); END;
  END SetReal;

PROCEDURE SetLongReal (t: T; var: TEXT; val: LongReal) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN Stuff (t, var, FmtLongReal (val)); END;
  END SetLongReal;

PROCEDURE SetText (t: T; var: TEXT; val: Text) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN Stuff (t, var, FmtText (val)); END;
  END SetText;

PROCEDURE SetIntegerArray (t: T; var: TEXT; READONLY val: IntegerArray) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN
      Stuff (t, var, FmtIntegerArray (val));
    END;
  END SetIntegerArray;

PROCEDURE SetBooleanArray (t: T; var: TEXT; READONLY val: BooleanArray) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN
      Stuff (t, var, FmtBooleanArray (val));
    END;
  END SetBooleanArray;

PROCEDURE SetCharArray (t: T; var: TEXT; READONLY val: CharArray) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN Stuff (t, var, FmtCharArray (val)); END;
  END SetCharArray;

PROCEDURE SetRealArray (t: T; var: TEXT; READONLY val: RealArray) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN Stuff (t, var, FmtRealArray (val)); END;
  END SetRealArray;

PROCEDURE SetLongRealArray (t: T; var: TEXT; READONLY val: LongRealArray) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN
      Stuff (t, var, FmtLongRealArray (val));
    END;
  END SetLongRealArray;

PROCEDURE SetTextArray (t: T; var: TEXT; READONLY val: TextArray) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN Stuff (t, var, FmtTextArray (val)); END;
  END SetTextArray;

PROCEDURE SetIntegerArray2 (t: T; var: TEXT; READONLY val: IntegerArray2) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN
      Stuff (t, var, FmtIntegerArray2 (val));
    END;
  END SetIntegerArray2;

PROCEDURE SetBooleanArray2 (t: T; var: TEXT; READONLY val: BooleanArray2) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN
      Stuff (t, var, FmtBooleanArray2 (val));
    END;
  END SetBooleanArray2;

PROCEDURE SetCharArray2 (t: T; var: TEXT; READONLY val: CharArray2) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN Stuff (t, var, FmtCharArray2 (val)); END;
  END SetCharArray2;

PROCEDURE SetRealArray2 (t: T; var: TEXT; READONLY val: RealArray2) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN Stuff (t, var, FmtRealArray2 (val)); END;
  END SetRealArray2;

PROCEDURE SetLongRealArray2 (t: T; var: TEXT; READONLY val: LongRealArray2) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN
      Stuff (t, var, FmtLongRealArray2 (val));
    END;
  END SetLongRealArray2;

PROCEDURE SetTextArray2 (t: T; var: TEXT; READONLY val: TextArray2) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN Stuff (t, var, FmtTextArray2 (val)); END;
  END SetTextArray2;

PROCEDURE SetIntegerPair (t: T; var: TEXT; READONLY val: IntegerPair) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN Stuff (t, var, FmtIntegerPair (val)); END;
  END SetIntegerPair;

PROCEDURE SetIntegerPairArray (t: T; 
    var: TEXT; READONLY val: IntegerPairArray) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN
      Stuff (t, var, FmtIntegerPairArray (val));
    END;
  END SetIntegerPairArray;

PROCEDURE SetIntegerTriple (t: T; var: TEXT; READONLY val: IntegerTriple) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN
      Stuff (t, var, FmtIntegerTriple (val));
    END;
  END SetIntegerTriple;

PROCEDURE SetIntegerTripleArray (t: T; 
    var: TEXT; READONLY val: IntegerTripleArray) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN
      Stuff (t, var, FmtIntegerTripleArray (val));
    END;
  END SetIntegerTripleArray;

PROCEDURE SetIntegerList (t: T; var: TEXT; val: IntegerList) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN Stuff (t, var, FmtIntegerList (val)); END;
  END SetIntegerList;

PROCEDURE SetIntegerListArray (t: T; 
    var: TEXT; READONLY val: IntegerListArray) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN
      Stuff (t, var, FmtIntegerListArray (val));
    END;
  END SetIntegerListArray;

PROCEDURE SetRealList (t: T; var: TEXT; val: RealList) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN Stuff (t, var, FmtRealList (val)); END;
  END SetRealList;

PROCEDURE SetRealListArray (t: T; var: TEXT; READONLY val: RealListArray) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN
      Stuff (t, var, FmtRealListArray (val));
    END;
  END SetRealListArray;

PROCEDURE SetIntegerPairList (t: T; var: TEXT; val: IntegerPairList) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN
      Stuff (t, var, FmtIntegerPairList (val));
    END;
  END SetIntegerPairList;

PROCEDURE SetIntegerPairListArray (t  : T;
                                   var: TEXT;
                     READONLY val: IntegerPairListArray) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN
      Stuff (t, var, FmtIntegerPairListArray (val));
    END;
  END SetIntegerPairListArray;

PROCEDURE SetIntegerTree (t: T; var: TEXT; val: IntegerTree) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN Stuff (t, var, FmtIntegerTree (val)); END;
  END SetIntegerTree;


PROCEDURE SetIntegerL (t: T; var: TEXT; val: Integer) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN StuffL (t, var, FmtInteger (val)); END;
  END SetIntegerL;

PROCEDURE SetBooleanL (t: T; var: TEXT; val: Boolean) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN StuffL (t, var, FmtBoolean (val)); END;
  END SetBooleanL;

PROCEDURE SetCharL (t: T; var: TEXT; val: Char) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN StuffL (t, var, FmtChar (val)); END;
  END SetCharL;

PROCEDURE SetRealL (t: T; var: TEXT; val: Real) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN StuffL (t, var, FmtReal (val)); END;
  END SetRealL;

PROCEDURE SetLongRealL (t: T; var: TEXT; val: LongReal) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN StuffL (t, var, FmtLongReal (val)); END;
  END SetLongRealL;

PROCEDURE SetTextL (t: T; var: TEXT; val: Text) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN StuffL (t, var, FmtText (val)); END;
  END SetTextL;

PROCEDURE SetIntegerArrayL (t: T; var: TEXT; READONLY val: IntegerArray) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN
      StuffL (t, var, FmtIntegerArray (val));
    END;
  END SetIntegerArrayL;

PROCEDURE SetBooleanArrayL (t: T; var: TEXT; READONLY val: BooleanArray) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN
      StuffL (t, var, FmtBooleanArray (val));
    END;
  END SetBooleanArrayL;

PROCEDURE SetCharArrayL (t: T; var: TEXT; READONLY val: CharArray) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN StuffL (t, var, FmtCharArray (val)); END;
  END SetCharArrayL;

PROCEDURE SetRealArrayL (t: T; var: TEXT; READONLY val: RealArray) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN StuffL (t, var, FmtRealArray (val)); END;
  END SetRealArrayL;

PROCEDURE SetLongRealArrayL (t: T; var: TEXT; READONLY val: LongRealArray) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN
      StuffL (t, var, FmtLongRealArray (val));
    END;
  END SetLongRealArrayL;

PROCEDURE SetTextArrayL (t: T; var: TEXT; READONLY val: TextArray) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN StuffL (t, var, FmtTextArray (val)); END;
  END SetTextArrayL;

PROCEDURE SetIntegerArray2L (t: T; var: TEXT; READONLY val: IntegerArray2) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN
      StuffL (t, var, FmtIntegerArray2 (val));
    END;
  END SetIntegerArray2L;

PROCEDURE SetBooleanArray2L (t: T; var: TEXT; READONLY val: BooleanArray2) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN
      StuffL (t, var, FmtBooleanArray2 (val));
    END;
  END SetBooleanArray2L;

PROCEDURE SetCharArray2L (t: T; var: TEXT; READONLY val: CharArray2) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN StuffL (t, var, FmtCharArray2 (val)); END;
  END SetCharArray2L;

PROCEDURE SetRealArray2L (t: T; var: TEXT; READONLY val: RealArray2) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN StuffL (t, var, FmtRealArray2 (val)); END;
  END SetRealArray2L;

PROCEDURE SetLongRealArray2L (t: T; var: TEXT; READONLY val: LongRealArray2) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN
      StuffL (t, var, FmtLongRealArray2 (val));
    END;
  END SetLongRealArray2L;

PROCEDURE SetTextArray2L (t: T; var: TEXT; READONLY val: TextArray2) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN StuffL (t, var, FmtTextArray2 (val)); END;
  END SetTextArray2L;

PROCEDURE SetIntegerPairL (t: T; var: TEXT; READONLY val: IntegerPair) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN StuffL (t, var, FmtIntegerPair (val)); END;
  END SetIntegerPairL;

PROCEDURE SetIntegerPairArrayL (t: T; var: TEXT; READONLY val: IntegerPairArray) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN
      StuffL (t, var, FmtIntegerPairArray (val));
    END;
  END SetIntegerPairArrayL;

PROCEDURE SetIntegerTripleL (t: T; var: TEXT; READONLY val: IntegerTriple) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN
      StuffL (t, var, FmtIntegerTriple (val));
    END;
  END SetIntegerTripleL;

PROCEDURE SetIntegerTripleArrayL (t: T; 
    var: TEXT; READONLY val: IntegerTripleArray) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN
      StuffL (t, var, FmtIntegerTripleArray (val));
    END;
  END SetIntegerTripleArrayL;

PROCEDURE SetIntegerListL (t: T; var: TEXT; val: IntegerList) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN StuffL (t, var, FmtIntegerList (val)); END;
  END SetIntegerListL;

PROCEDURE SetIntegerListArrayL (t: T; var: TEXT; READONLY val: IntegerListArray) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN
      StuffL (t, var, FmtIntegerListArray (val));
    END;
  END SetIntegerListArrayL;

PROCEDURE SetRealListL (t: T; var: TEXT; val: RealList) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN StuffL (t, var, FmtRealList (val)); END;
  END SetRealListL;

PROCEDURE SetRealListArrayL (t: T; var: TEXT; READONLY val: RealListArray) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN
      StuffL (t, var, FmtRealListArray (val));
    END;
  END SetRealListArrayL;

PROCEDURE SetIntegerPairListL (t: T; var: TEXT; val: IntegerPairList) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN
      StuffL (t, var, FmtIntegerPairList (val));
    END;
  END SetIntegerPairListL;

PROCEDURE SetIntegerPairListArrayL (t  : T;
                                   var: TEXT;
                          READONLY val: IntegerPairListArray) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN
      StuffL (t, var, FmtIntegerPairListArray (val));
    END;
  END SetIntegerPairListArrayL;

PROCEDURE SetIntegerTreeL (t: T; var: TEXT; val: IntegerTree) =
  BEGIN
    IF VBT.Parent (t) # NIL THEN StuffL (t, var, FmtIntegerTree (val)); END;
  END SetIntegerTreeL;


PROCEDURE FmtInteger (val: Integer): TEXT =
  BEGIN
    RETURN Fmt.Int (val);
  END FmtInteger;

PROCEDURE FmtBoolean (val: Boolean): TEXT =
  BEGIN
    RETURN Fmt.Bool (val);
  END FmtBoolean;

PROCEDURE FmtChar (val: Char): TEXT =
  BEGIN
    RETURN "'" & Fmt.Char (val) & "'";
  END FmtChar;

PROCEDURE FmtReal (val: Real): TEXT =
  BEGIN
    RETURN Fmt.Real (val);
  END FmtReal;

PROCEDURE FmtLongReal (val: LongReal): TEXT =
  BEGIN
    RETURN Fmt.LongReal (val);
  END FmtLongReal;

PROCEDURE FmtText (val: Text): TEXT =
  BEGIN
    IF val = NIL THEN RETURN "" ELSE RETURN val END;
  END FmtText;

PROCEDURE FmtIntegerPair (val: IntegerPair): TEXT =
  BEGIN
    RETURN Fmt.F ("<a = %t, b = %t>", Fmt.Int (val.a), Fmt.Int (val.b));
  END FmtIntegerPair;

PROCEDURE FmtIntegerTriple (val: IntegerTriple): TEXT =
  BEGIN
    RETURN Fmt.F ("<a = %t, b = %t, c = %t>", Fmt.Int (val.a),
                  Fmt.Int (val.b), Fmt.Int (val.c));
  END FmtIntegerTriple;

PROCEDURE FmtIntegerArray (READONLY val: IntegerArray): TEXT =
  VAR wr := TextWr.New ();
  BEGIN
    FOR i := 0 TO LAST (val) DO
      IF i > 0 THEN Wr.PutChar (wr, ' '); END;
      Wr.PutText (wr, FmtInteger (val [i]));
    END;
    RETURN TextWr.ToText (wr);
  END FmtIntegerArray;

PROCEDURE FmtBooleanArray (READONLY val: BooleanArray): TEXT =
  VAR wr := TextWr.New ();
  BEGIN
    FOR i := 0 TO LAST (val) DO
      IF i > 0 THEN Wr.PutChar (wr, ' '); END;
      Wr.PutText (wr, FmtBoolean (val [i]));
    END;
    RETURN TextWr.ToText (wr);
  END FmtBooleanArray;

PROCEDURE FmtCharArray (READONLY val: CharArray): TEXT =
  BEGIN
    RETURN TextOps.FromChars (val);
  END FmtCharArray;

PROCEDURE FmtRealArray (READONLY val: RealArray): TEXT =
  VAR wr := TextWr.New ();
  BEGIN
    FOR i := 0 TO LAST (val) DO
      IF i > 0 THEN Wr.PutChar (wr, ' '); END;
      Wr.PutText (wr, FmtReal (val [i]));
    END;
    RETURN TextWr.ToText (wr);
  END FmtRealArray;

PROCEDURE FmtLongRealArray (READONLY val: LongRealArray): TEXT =
  VAR wr := TextWr.New ();
  BEGIN
    FOR i := 0 TO LAST (val) DO
      IF i > 0 THEN Wr.PutChar (wr, ' '); END;
      Wr.PutText (wr, FmtLongReal (val [i]));
    END;
    RETURN TextWr.ToText (wr);
  END FmtLongRealArray;

PROCEDURE FmtTextArray (READONLY val: TextArray): TEXT =
  VAR wr := TextWr.New ();
  BEGIN
    FOR i := 0 TO LAST (val) DO
      IF i > 0 THEN Wr.PutChar (wr, ' '); END;
      Wr.PutText (wr, FmtText (val [i]));
    END;
    RETURN TextWr.ToText (wr);
  END FmtTextArray;

PROCEDURE FmtIntegerArray2 (READONLY val: IntegerArray2): TEXT =
  VAR wr := TextWr.New ();
  BEGIN
    FOR i := 0 TO LAST (val) DO
      IF i > 0 THEN Wr.PutChar (wr, '\n'); END;
      Wr.PutText (wr, FmtIntegerArray (val [i]));
    END;
    RETURN TextWr.ToText (wr);
  END FmtIntegerArray2;

PROCEDURE FmtBooleanArray2 (READONLY val: BooleanArray2): TEXT =
  VAR wr := TextWr.New ();
  BEGIN
    FOR i := 0 TO LAST (val) DO
      IF i > 0 THEN Wr.PutChar (wr, '\n'); END;
      Wr.PutText (wr, FmtBooleanArray (val [i]));
    END;
    RETURN TextWr.ToText (wr);
  END FmtBooleanArray2;

PROCEDURE FmtCharArray2 (READONLY val: CharArray2): TEXT =
  VAR wr := TextWr.New ();
  BEGIN
    FOR i := 0 TO LAST (val) DO
      IF i > 0 THEN Wr.PutChar (wr, '\n'); END;
      Wr.PutText (wr, TextOps.FromChars (val [i]));
    END;
    RETURN TextWr.ToText (wr);
  END FmtCharArray2;

PROCEDURE FmtRealArray2 (READONLY val: RealArray2): TEXT =
  VAR wr := TextWr.New ();
  BEGIN
    FOR i := 0 TO LAST (val) DO
      IF i > 0 THEN Wr.PutChar (wr, '\n'); END;
      Wr.PutText (wr, FmtRealArray (val [i]));
    END;
    RETURN TextWr.ToText (wr);
  END FmtRealArray2;

PROCEDURE FmtLongRealArray2 (READONLY val: LongRealArray2): TEXT =
  VAR wr := TextWr.New ();
  BEGIN
    FOR i := 0 TO LAST (val) DO
      IF i > 0 THEN Wr.PutChar (wr, '\n'); END;
      Wr.PutText (wr, FmtLongRealArray (val [i]));
    END;
    RETURN TextWr.ToText (wr);
  END FmtLongRealArray2;

PROCEDURE FmtTextArray2 (READONLY val: TextArray2): TEXT =
  VAR wr := TextWr.New ();
  BEGIN
    FOR i := 0 TO LAST (val) DO
      IF i > 0 THEN Wr.PutChar (wr, '\n'); END;
      Wr.PutText (wr, FmtTextArray (val [i]));
    END;
    RETURN TextWr.ToText (wr);
  END FmtTextArray2;

PROCEDURE FmtIntegerPairArray (READONLY val: IntegerPairArray): TEXT =
  VAR wr := TextWr.New ();
  BEGIN
    FOR i := 0 TO LAST (val) DO
      IF i > 0 THEN Wr.PutChar (wr, ' '); END;
      Wr.PutText (wr, FmtIntegerPair (val [i]));
    END;
    RETURN TextWr.ToText (wr);
  END FmtIntegerPairArray;

PROCEDURE FmtIntegerTripleArray (READONLY val: IntegerTripleArray): TEXT =
  VAR wr := TextWr.New ();
  BEGIN
    FOR i := 0 TO LAST (val) DO
      IF i > 0 THEN Wr.PutChar (wr, ' '); END;
      Wr.PutText (wr, FmtIntegerTriple (val [i]));
    END;
    RETURN TextWr.ToText (wr);
  END FmtIntegerTripleArray;

PROCEDURE FmtIntegerListArray (READONLY val: IntegerListArray): TEXT =
  VAR wr := TextWr.New ();
  BEGIN
    FOR i := 0 TO LAST (val) DO
      IF i > 0 THEN Wr.PutChar (wr, ' '); END;
      Wr.PutText (wr, FmtIntegerList (val [i]));
    END;
    RETURN TextWr.ToText (wr);
  END FmtIntegerListArray;

PROCEDURE FmtRealListArray (READONLY val: RealListArray): TEXT =
  VAR wr := TextWr.New ();
  BEGIN
    FOR i := 0 TO LAST (val) DO
      IF i > 0 THEN Wr.PutChar (wr, ' '); END;
      Wr.PutText (wr, FmtRealList (val [i]));
    END;
    RETURN TextWr.ToText (wr);
  END FmtRealListArray;

PROCEDURE FmtIntegerPairListArray (READONLY val: IntegerPairListArray): TEXT =
  VAR wr := TextWr.New ();
  BEGIN
    FOR i := 0 TO LAST (val) DO
      IF i > 0 THEN Wr.PutChar (wr, ' '); END;
      Wr.PutText (wr, FmtIntegerPairList (val [i]));
    END;
    RETURN TextWr.ToText (wr);
  END FmtIntegerPairListArray;

PROCEDURE FmtIntegerList (val: IntegerList): TEXT =
  VAR wr := TextWr.New ();
  BEGIN
    Wr.PutChar (wr, '(');
    IF val # NIL THEN
      Wr.PutText (wr, Fmt.Int (val.i));
      val := val.next;
      WHILE val # NIL DO
        Wr.PutChar (wr, ' ');
        Wr.PutText (wr, Fmt.Int (val.i));
        val := val.next;
      END;
    END;
    Wr.PutChar (wr, ')');
    RETURN TextWr.ToText (wr);
  END FmtIntegerList;

PROCEDURE FmtRealList (val: RealList): TEXT =
  VAR wr := TextWr.New ();
  BEGIN
    Wr.PutChar (wr, '(');
    IF val # NIL THEN
      Wr.PutText (wr, Fmt.Real (val.r));
      val := val.next;
      WHILE val # NIL DO
        Wr.PutChar (wr, ' ');
        Wr.PutText (wr, Fmt.Real (val.r));
        val := val.next;
      END;
    END;
    Wr.PutChar (wr, ')');
    RETURN TextWr.ToText (wr);
  END FmtRealList;

PROCEDURE FmtIntegerPairList (val: IntegerPairList): TEXT =
  VAR wr := TextWr.New ();
  BEGIN
    Wr.PutChar (wr, '(');
    IF val # NIL THEN
      Wr.PutText (
        wr, Fmt.F ("<a = %t, b = %t>", Fmt.Int (val.a), Fmt.Int (val.b)));
      val := val.next;
      WHILE val # NIL DO
        Wr.PutChar (wr, ' ');
        Wr.PutText (
          wr, Fmt.F ("<a = %t, b = %t>", Fmt.Int (val.a), Fmt.Int (val.b)));
        val := val.next;
      END;
    END;
    Wr.PutChar (wr, ')');
    RETURN TextWr.ToText (wr);
  END FmtIntegerPairList;

PROCEDURE FmtIntegerTree (val: IntegerTree): TEXT =
  VAR wr := TextWr.New ();
  BEGIN
    IF val = NIL THEN RETURN "()"; END;
    FmtIntegerTree1 (wr, val);
    RETURN TextWr.ToText (wr);
  END FmtIntegerTree;

PROCEDURE FmtIntegerTree1 (wr: Wr.T; val: IntegerTree) =
  BEGIN
    Wr.PutChar (wr, '(');
    IF val.l # NIL THEN
      FmtIntegerTree1 (wr, val.l);
      Wr.PutChar (wr, ' ');
    END;
    Wr.PutText (wr, Fmt.Int (val.i));
    IF val.r # NIL THEN
      Wr.PutChar (wr, ' ');
      FmtIntegerTree1 (wr, val.r);
    END;
    Wr.PutChar (wr, ')');
  END FmtIntegerTree1;

PROCEDURE Stuff (t: T; var: TEXT; text: TEXT) =
  BEGIN
    LOCK VBT.mu DO StuffL (t, var, text) END;
  END Stuff;

PROCEDURE StuffL (t: T; var: TEXT; text: TEXT) =
  BEGIN
    TRY
      FormsVBT.PutText (t, Tail (var), text);
    EXCEPT
      FormsVBT.Error =>
    END;
  END StuffL;

PROCEDURE Tail (name: TEXT): TEXT =
  BEGIN
    WITH pos = TextOps.FindCharR (name, '.') DO
      IF pos < 0 THEN
        RETURN name;
      ELSE
        RETURN TextOps.Sub (name, pos + 1, LAST (CARDINAL));
      END;
    END;
  END Tail;

BEGIN
END DataView.
