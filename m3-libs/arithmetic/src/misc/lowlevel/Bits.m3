UNSAFE MODULE Bits;
(*Copyright (c) 1996, m3na project

Abstract: Bits and Bytes

2/17/96  Harry George    Initial version
*)
IMPORT Word, Fmt;
FROM Word IMPORT And,Xor,Not,LeftShift,RightShift;
FROM xUtils IMPORT debug;

CONST Module = "Bits.";
(*==========================*)
(*============================*)
(* Which Endian?              *)
(*============================*)
VAR WhichEnd:[-1..+1]:=0;

PROCEDURE whichend():[-1..+1]=
(*-1 means little-endian, +1 means big-endian.*)
CONST
  ftn = Module & "whichend";
VAR
  datum:=16_40ACB139;
  any:AnyEndian;
BEGIN
  IF WhichEnd # 0 THEN RETURN WhichEnd; END;

  (*---need to check---*)
  any:=LOOPHOLE(datum,AnyEndian);
  IF    any.data[0]=16_40 THEN
    WhichEnd:=+1;
  ELSIF any.data[0]=16_39 THEN
    WhichEnd:=-1;
  ELSE
    debug(1,ftn,"huh?");
  END;
  RETURN WhichEnd;
END whichend;

(*============================*)
(* Other Functions            *)
(*============================*)

(*-------------------------*)
PROCEDURE fmt(x:Word.T;
              nbits:CARDINAL:=32;
              base:CARDINAL:=2   (*typically 2 or 16*)
              ):TEXT=
BEGIN
  RETURN Fmt.Int(base) & "_" &
         Fmt.Pad(Fmt.Int(x,base:=base),
                 length:=nbits,
                 padChar:='0',
                 align:=Fmt.Align.Right);
END fmt;
(*----------------*)
PROCEDURE reverse(x:CARDINAL;       (*given this number*)
                  nbits:CARDINAL    (*using the low n bits*)
                  ):CARDINAL=       (*return reversed bit pattern*)
(*The idea is to let the least bit rotate to the
negative bit of a 2's complement integer, and test
for <0.  If <0, then increment the tmp2.  Negative
or not, shift tmp2 up*)
VAR
  tmp1:INTEGER:=x;
  tmp2:=0;
BEGIN
  FOR j:=0 TO nbits-1 DO
    IF tmp1<0 THEN INC(tmp2); END;
    tmp2:=Word.LeftShift(tmp2,1);
    tmp1:=Word.RightRotate(tmp1,1);
  END;
  IF tmp1<0 THEN INC(tmp2); END;
  RETURN tmp2;
END reverse;
(*--------------------*)
PROCEDURE hash_pjw(READONLY str: ARRAY OF CHAR; (*given this string*)
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
END hash_pjw;
(*--------------------*)
PROCEDURE hash_elf(READONLY str: ARRAY OF CHAR; (*given this string*)
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
END hash_elf;
(*==========================*)
BEGIN
END Bits.
