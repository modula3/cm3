(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Mon Oct 26 10:29:29 PST 1992 by kalsow *)
(*      modified on Fri Aug 10 12:52:19 PDT 1990 by ellis  *)
(*      modified on Mon Feb 12 15:47:27 1990 by saxe       *)

MODULE Main;
IMPORT Test;

(* Assignment statements on records and arrays. *)

TYPE
  Index = [-23 .. 47];
  IntArray = ARRAY Index OF INTEGER;
  RealArray = ARRAY [0 .. 5] OF ARRAY Index OF REAL;
  Color =
    {Black, Red, Orange, Yellow, Green, Blue, Indigo, Violet, White};
  Record = RECORD
    letter: CHAR;
    number: CARDINAL;
    color: [Color.Red .. Color.Violet];
    array: IntArray;
  END;
  RecordArray = ARRAY [1 .. 10], [1 .. 10] OF Record;

VAR
  real1, real2: REAL;
  intArray1, intArray2: IntArray;
  realArray1, realArray2: RealArray;
  record1, record2, record3: Record;
  recordArray1, recordArray2: RecordArray;

BEGIN

  Test.checkI (FIRST(Index), -23);
  Test.checkI (LAST(Index), 47);
  FOR index := FIRST(Index) TO LAST(Index) DO
    intArray1[index] := index + index;
    intArray2[index] := 0;
  END;
  Test.checkI (intArray2[13], 0);
  Test.checkI (intArray1[13], 26);
  intArray2 := intArray1;
  FOR index := FIRST(Index) TO LAST(Index) DO
    Test.checkI (intArray2[index] - index, index);
  END;

  FOR int1 := 0 TO 5 DO
    FOR index := FIRST(Index) TO LAST(Index) DO
      realArray1[int1][index] := FLOAT(int1) * FLOAT(index);
    END;
  END;
  realArray2 := realArray1;
  real1 := 0.0;
  real2 := 0.0;
  FOR index := -23 TO 47 DO
    real1 := real1 + FLOAT(index);
    Test.checkR (realArray2[0, index], 0.0e20);
    FOR int1 := 0 TO 5 DO real2 := real2 + realArray2[int1, index]; END;
  END;
  (* Real arithmetic on small integers should be exact: *)
  Test.checkR (real2, 15.0 * real1);


  record1.letter := '"';
  record1.number := 1492;
  record1.color := Color.Blue;
  record1.array := intArray1;
  record2 := record1;
  record3.letter := record1.letter;
  record3.number := 1400 + 92;
  record3.color := record2.color;
  record3.array := intArray2;
  Test.checkI (record3.number, record2.number);
  Test.checkC (record3.letter, '"');
  Test.checkI (ORD (record3.color), ORD (Color.Blue));
  Test.checkI (record3.array[20], 40);

  Test.checkI (BYTESIZE(RecordArray), 100 * BYTESIZE(Record));
  FOR int1 := 1 TO 10 DO
    FOR int2 := 1 TO 10 DO
      recordArray1[int1, int2] := record3;
      recordArray1[int2, int1].array[5] := 7;
    END;
  END;
  recordArray2 := recordArray1;
  Test.checkI (recordArray2[1, 6].array[5], 7);
  Test.checkI (recordArray2[6, 1].array[5], 10);

  Test.done ();
END Main.
