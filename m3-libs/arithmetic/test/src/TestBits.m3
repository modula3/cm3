MODULE tBits EXPORTS test;
(*Copyright (c) 1996, m3na project
Abstract:  Tests for Utils module.

1/1/96    Harry George   Initial version

*)

IMPORT xBits AS B;
IMPORT Fmt,Text;
(*=======================*)
CONST
  Module = "tBits.";
(*----------------------*)
PROCEDURE test_ABC():BOOLEAN=
CONST
  ftn = Module & "test_ABC";
VAR
  result:=TRUE;
BEGIN
  debug(1,ftn,"begin\n");

  RETURN result;   
END test_ABC;
(*----------------------*)
PROCEDURE test_whichend():BOOLEAN=
CONST
  ftn = Module & "test_whichend";
VAR
  result:=TRUE;
BEGIN
  debug(1,ftn,"begin\n");
  FOR i:=1 TO 4 DO
    CASE B.whichend() OF
    | -1 => msg("little endian\n");
    |  0 => msg("error\n");
    | +1 => msg("big  endian\n");
    END;
  END;
  RETURN result;   
END test_whichend;
(*----------------------*)
PROCEDURE test_fmt():BOOLEAN=
CONST
  ftn = Module & "test_fmt";
  x=2_1010;
VAR
  result:=TRUE;
BEGIN
  debug(1,ftn,"begin\n");
  msg("2_1010=" & B.fmt(x,nbits:=4) & "\n");
  msg("2_00001010=" & B.fmt(x,nbits:=8) & "\n");
  msg("2_000000001010=" & B.fmt(x,nbits:=12) & "\n");
  msg("2_0000000000001010=" & B.fmt(x,nbits:=16) & "\n");
  
  RETURN result;   
END test_fmt;
(*----------------------*)
PROCEDURE test_reverse():BOOLEAN=
CONST
  ftn = Module & "test_reverse";
  x=2_1011; nbits=4;
VAR
  result:=TRUE;
BEGIN
  debug(1,ftn,"begin\n");
  msg(B.fmt(x,nbits+2) & " reverses to "
    & B.fmt(B.reverse(x,nbits),nbits+2)
    & "\n");

  RETURN result;   
END test_reverse;
(*======================*)
CONST
  textdata = "the quick brown fox jumped over the lazy dog";
VAR
  str:ARRAY [0..20] OF CHAR;	           

(*----------------------*)
PROCEDURE test_hash_pjw():BOOLEAN=
CONST
  ftn = Module & "test_hash_pjw";
VAR
  result:=TRUE;
  
BEGIN
  debug(1,ftn,"begin\n");
  Text.SetChars(str,textdata);
  FOR i:=FIRST(str) TO LAST(str)-6 BY 5 DO
    msg("hash=" & Fmt.Int(B.hash_pjw(str,i,i+5)) & "\n");
  END;
  RETURN result;   
END test_hash_pjw;
(*----------------------*)
PROCEDURE test_hash_elf():BOOLEAN=
CONST
  ftn = Module & "test_hash_elf";
VAR
  result:=TRUE;
BEGIN
  debug(1,ftn,"begin\n");
  Text.SetChars(str,textdata);
  FOR i:=FIRST(str) TO LAST(str)-6 BY 5 DO
    msg("hash=" & Fmt.Int(B.hash_elf(str,i,i+5)) & "\n");
  END;

  RETURN result;   
END test_hash_elf;
(*----------------------*)
PROCEDURE test_Bits():BOOLEAN=
CONST ftn = Module & "test_Bits";
VAR
  result:=TRUE;
BEGIN
  debug(1,ftn,"begin\n");
  newline(); EVAL test_whichend();
  newline(); EVAL test_fmt();
  newline(); EVAL test_reverse();  
  newline(); EVAL test_hash_pjw();  
  newline(); EVAL test_hash_elf();  
  RETURN result;   
END test_Bits;
(*=======================*)
BEGIN
END tBits.
