MODULE TestBits EXPORTS Test;
(*Copyright (c) 1996, m3na project
Abstract:  TestS for Utils module.

1/1/96    Harry George   Initial version

*)

IMPORT xBits AS B;
IMPORT Fmt,Text;
(*=======================*)
CONST
  Module = "TestBits.";
(*----------------------*)
PROCEDURE TestABC():BOOLEAN=
CONST
  ftn = Module & "TestABC";
VAR
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");

  RETURN result;   
END TestABC;
(*----------------------*)
PROCEDURE TestWhichend():BOOLEAN=
CONST
  ftn = Module & "TestWhichend";
VAR
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  FOR i:=1 TO 4 DO
    CASE B.whichend() OF
    | -1 => Msg("little endian\n");
    |  0 => Msg("error\n");
    | +1 => Msg("big  endian\n");
    END;
  END;
  RETURN result;   
END TestWhichend;
(*----------------------*)
PROCEDURE TestFmt():BOOLEAN=
CONST
  ftn = Module & "TestFmt";
  x=2_1010;
VAR
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  Msg("2_1010=" & B.fmt(x,nbits:=4) & "\n");
  Msg("2_00001010=" & B.fmt(x,nbits:=8) & "\n");
  Msg("2_000000001010=" & B.fmt(x,nbits:=12) & "\n");
  Msg("2_0000000000001010=" & B.fmt(x,nbits:=16) & "\n");
  
  RETURN result;   
END TestFmt;
(*----------------------*)
PROCEDURE TestReverse():BOOLEAN=
CONST
  ftn = Module & "TestReverse";
  x=2_1011; nbits=4;
VAR
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  Msg(B.fmt(x,nbits+2) & " reverses to "
    & B.fmt(B.reverse(x,nbits),nbits+2)
    & "\n");

  RETURN result;   
END TestReverse;
(*======================*)
CONST
  textdata = "the quick brown fox jumped over the lazy dog";
VAR
  str:ARRAY [0..20] OF CHAR;	           

(*----------------------*)
PROCEDURE TestHash_pjw():BOOLEAN=
CONST
  ftn = Module & "TestHash_pjw";
VAR
  result:=TRUE;
  
BEGIN
  Debug(1,ftn,"begin\n");
  Text.SetChars(str,textdata);
  FOR i:=FIRST(str) TO LAST(str)-6 BY 5 DO
    Msg("hash=" & Fmt.Int(B.hash_pjw(str,i,i+5)) & "\n");
  END;
  RETURN result;   
END TestHash_pjw;
(*----------------------*)
PROCEDURE TestHash_elf():BOOLEAN=
CONST
  ftn = Module & "TestHash_elf";
VAR
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  Text.SetChars(str,textdata);
  FOR i:=FIRST(str) TO LAST(str)-6 BY 5 DO
    Msg("hash=" & Fmt.Int(B.hash_elf(str,i,i+5)) & "\n");
  END;

  RETURN result;   
END TestHash_elf;
(*----------------------*)
PROCEDURE TestBits():BOOLEAN=
CONST ftn = Module & "TestBits";
VAR
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  NewLine(); EVAL TestWhichend();
  NewLine(); EVAL TestFmt();
  NewLine(); EVAL TestReverse();  
  NewLine(); EVAL TestHash_pjw();  
  NewLine(); EVAL TestHash_elf();  
  RETURN result;   
END TestBits;
(*=======================*)
BEGIN
END TestBits.
