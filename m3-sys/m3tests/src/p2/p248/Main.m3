(* The point of this test is to learn the Modula-3 machine model.
 *)

UNSAFE MODULE Main;
IMPORT RTIO;

TYPE Color = {Red,Orange,Green,Blue,Indigo,Violet}; (* enum *)
<*NOWARN*>TYPE Color2 =  BITS 4 FOR {Red,Orange,Green,Blue,Indigo,Violet}; (* packed enum *)
<*NOWARN*>TYPE ColorR1 = BITS 2 FOR [Color.Red..Color.Orange]; (* packed enum subrange *)
<*NOWARN*>TYPE ColorP1 = BITS 3 FOR Color; (* another packed enum *)
TYPE R1 = BITS 4 FOR [2..10]; (* packed integer subrange *)
VAR r1:R1;
TYPE T1 = RECORD r1,r2,r3:R1 END; (* record with bitfields *)
VAR t1:T1;

<*NOWARN*>PROCEDURE FR1(VAR r:R1; VAR t:T1) = BEGIN
  r := 2;
  t.r1 := 2;
  t.r2 := 3;
  t.r3 := 10;
END FR1;

TYPE T3 = RECORD a:BITS 1 FOR BOOLEAN; END;

VAR A5: T3;

BEGIN
  FR1(r1, t1);
  RTIO.PutAddr(ADR(A5)); RTIO.PutText("\n");
  RTIO.PutText("\n");
  RTIO.Flush();
END Main.
