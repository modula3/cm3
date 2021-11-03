INTERFACE DataRecs;

IMPORT BTree;

VAR
  fbr : BTree.T;

TYPE
  IdType = ARRAY [0..9] OF CHAR;
  BarcodeType = ARRAY [0..30] OF CHAR;
  StateType = ARRAY [0..3] OF CHAR;
  DescType = ARRAY [0..60] OF CHAR;
  SupplierType = ARRAY [0..30] OF CHAR;
  RecycleType = ARRAY [0..12] OF CHAR;

  RefDat = REF DatRec;
  DatRec = RECORD
             itemId     : IdType;
             desc       : DescType;
             manufDate  : LONGREAL;
             usebyDate  : LONGREAL;
             barcode    : BarcodeType;
             supplier   : SupplierType;
             plantId    : INTEGER;
             plantState : StateType;
             weight     : REAL;
             recycleCat : RecycleType;
           END;

PROCEDURE AddRecTest(pk,key : TEXT);
PROCEDURE DelRecTest (key: TEXT);
PROCEDURE FindRecTest(key : TEXT; keyNum : INTEGER);
PROCEDURE UpdateRecTest (key: TEXT; keyNum: INTEGER);
PROCEDURE SimpleRecs ();
PROCEDURE RandomRecs ();

END DataRecs.

