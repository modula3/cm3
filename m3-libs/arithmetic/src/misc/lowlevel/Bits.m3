UNSAFE MODULE Bits;
(*Arithmetic for Modula-3, see doc for details

Abstract: Bits and Bytes

2/17/96  Harry George    Initial version
*)
IMPORT Word, Fmt AS F;
FROM Word IMPORT And,Xor,Not,LeftShift,RightShift;
(*
FROM Arithmetic IMPORT Debug;
*)

CONST Module = "Bits.";
(*==========================*)
(*============================*)
(* Which Endian?              *)
(*============================*)
PROCEDURE WhichEndian():[-1..+1]=
(*-1 means little-endian, +1 means big-endian.*)
<*UNUSED*>
CONST
  ftn = Module & "WhichEndian";
VAR
  datum:=16_40ACB139;
  any:AnyEndian;
BEGIN
  (*---need to check---*)
  any:=LOOPHOLE(datum,AnyEndian);
  IF    any.data[0]=16_40 THEN
    RETURN +1;
  ELSIF any.data[0]=16_39 THEN
    RETURN -1;
  ELSE
    <*ASSERT FALSE*>
    (*
    Debug(1,ftn,"huh?");
    *)
  END;
END WhichEndian;

(*============================*)
(* Other Functions            *)
(*============================*)

(*-------------------------*)
PROCEDURE Fmt(x:Word.T;
              nbits:[1..Word.Size]:=32;
              base :CARDINAL:=2   (*typically 2 or 16*)
              ):TEXT=
BEGIN
  RETURN F.Int(base) & "_" &
         F.Pad(F.Int(x,base:=base),
                 length:=nbits,
                 padChar:='0',
                 align:=F.Align.Right);
END Fmt;
(*----------------*)
PROCEDURE Reverse(x:Word.T;               (*given this number*)
                  nbits:[1..Word.Size];   (*using the low n bits*)
                  ):Word.T=               (*return reversed bit pattern*)
VAR
  y:Word.T:=Word.And(x,1);
BEGIN
  FOR j:=0 TO nbits-2 DO
    x:=Word.RightShift(x,1);
    (*machine oriented implementations would use a
      rotate-through-carry operation instead*)
    y:=Word.Or(
         Word.LeftShift(y,1),
         Word.And(x,1)
       );
  END;
  RETURN y;
END Reverse;
(*--------------------*)
PROCEDURE HashPJW(READONLY str: ARRAY OF CHAR; (*given this string*)
                          n1,nn:CARDINAL        (*using n1..nn*)
                              ):CARDINAL=       (*return hash value*)
(*P. Weinberger's hash*)
CONST
  CHAR_BITS = BITSIZE(CHAR);
  THREE_QUARTERS = (CHAR_BITS * 3) DIV 4;
  ONE_EIGHTH= CHAR_BITS DIV 8;
  HIGH_BITS = Not(RightShift(Not(0),ONE_EIGHTH));
  NOT_HIGH  = Not(HIGH_BITS);
VAR
  hash_value,i:Word.T;
BEGIN
  hash_value:=0;
  FOR j:=n1 TO nn DO
    hash_value:=LeftShift(hash_value,ONE_EIGHTH) + ORD(str[j]);
    i:=And(hash_value,HIGH_BITS);
    IF i # 0 THEN
      hash_value:=And(Xor(hash_value,RightShift(i,THREE_QUARTERS)),
                      NOT_HIGH);
    END;
  END;
  RETURN hash_value;
END HashPJW;
(*--------------------*)
PROCEDURE HashELF(READONLY str: ARRAY OF CHAR; (*given this string*)
                          n1,nn:CARDINAL        (*using n1..nn*)
                              ):CARDINAL=       (*return hash value*)
(*ELF hash*)
VAR
  h:Word.T:=0;
  g:Word.T;
BEGIN
  FOR i:=n1 TO nn DO
    h:=LeftShift(h,4) + ORD(str[i]);
    g:=And(h,16_F0000000);
    IF g#0 THEN
      h:=Xor(h,RightShift(g,24));
    END;
    h:=And(h,Not(g));
  END;
  RETURN h;
END HashELF;
(*==========================*)
BEGIN
END Bits.
