
(* test for importing procedures with value parameters checking
if they are passed in a word or a struct *)


MODULE Main;

IMPORT Temp,IO;

PROCEDURE Test() =
VAR
  sArr : Temp.SArray;
  bArr : Temp.BArray;
  sRec : Temp.SRec;
  bRec : Temp.BRec;
  spRec : Temp.SPackRec;
  sSet : Temp.SSet;
  bSet : Temp.BSet;
  oArr : ARRAY[0..1] OF INTEGER;
BEGIN

  Temp.SmallArr(sArr);

  Temp.BigArr(bArr);
  
  Temp.OpenArr(oArr);
  
  Temp.SmallRec(sRec);

  Temp.BigRec(bRec);

  Temp.SmallPackRec(spRec);
  
  Temp.SmallSet(sSet);
  
  Temp.BigSet(bSet);

END Test;

BEGIN
 
 Test();
 
END Main.
