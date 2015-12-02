INTERFACE Temp;

TYPE
  ArrType = ARRAY[2..57] OF CHAR;
  
  OpenArrType = ARRAY OF INTEGER;

  SRec = RECORD
           c : CHAR;
          END;
  BRec = RECORD
           i,j,k,l : INTEGER;
        END;
  SArray = ARRAY[0..1] OF CHAR;
  BArray = ARRAY[0..100] OF INTEGER;
  
  SSet = SET OF [0..10];
  BSet = SET OF [0..100];

  SPackRec = RECORD
              c : BITS 1 FOR BOOLEAN;
              d : BITS 5 FOR [0..30];
          END;
  BPackRec = RECORD
              c : BITS 1 FOR BOOLEAN;
              d : BITS 5 FOR [0..30];
              e : BITS 15 FOR [0..30];
              f : BITS 35 FOR [0..30];
          END;
          
PROCEDURE SmallArr(a : SArray);
PROCEDURE BigArr(a : BArray);

PROCEDURE OpenArr(a : OpenArrType);


PROCEDURE SmallRec(r : SRec);
PROCEDURE BigRec(r : BRec);
PROCEDURE SmallPackRec(r : SPackRec);
PROCEDURE BigPackRec(r : BPackRec);


PROCEDURE SmallSet(s : SSet);
PROCEDURE BigSet(s : BSet);
  
END Temp.
