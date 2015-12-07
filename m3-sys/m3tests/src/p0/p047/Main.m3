(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* LOOPHOLE tests between integer types and real types *)

UNSAFE MODULE  Main;

FROM Test IMPORT checkI,checkR,checkL,checkX,done;

TYPE
  Int32 = [0..16_FFFFFFFF];
  Rec32 = RECORD
            xx : BITS 32 FOR Int32;
          END;

PROCEDURE Test() =
  VAR
    i : INTEGER;
    l : LONGINT;
    a : ADDRESS;
    r1 : REAL;
    l1 : LONGREAL;
    e1 : EXTENDED;
    int32 : Int32;
    rec32 : Rec32;
  BEGIN
(*
    checkI (BITSIZE(INTEGER), 64);
    checkI (BITSIZE(ADDRESS), 64);
    checkI (BITSIZE(int32), 32);
    checkI (BITSIZE(LONGREAL), BITSIZE(EXTENDED));
    checkI (BITSIZE(LONGINT), BITSIZE(INTEGER));
*)
    (* REAL *)
    
    (* REAL to INTEGER will not compile on 64 bit - size mismatch 
    i := LOOPHOLE(r1,INTEGER);
    *)
    
    r1 := 1.234E0;
    int32 := LOOPHOLE(r1,Int32);
    checkI(1067316150,int32);

    r1 := LOOPHOLE(int32,REAL);
    checkR(1.234E0,r1);
    
    rec32 := LOOPHOLE(r1,Rec32);
    checkI(1067316150,rec32.xx);
    
    (* LONGREAL *)
    
    l1 := 1.234D0;
    i := LOOPHOLE(l1,INTEGER);
    checkI(4608236261112822104,i);
    
    l1 := LOOPHOLE(i,LONGREAL);
    checkL(1.234D0,l1);

    l := LOOPHOLE(l1,LONGINT);

    a := LOOPHOLE(l1,ADDRESS);

    (* EXTENDED *)
    
    l1 := 1.234D0;
    e1 := LOOPHOLE(l1,EXTENDED);
    checkX(1.234X0,e1);

  END Test;

BEGIN
  Test();
  done ();
END Main.
