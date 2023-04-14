(* Copyright (C) 2023, Peter McKinna *)
(* All rights reserved.              *)
(* Licensed under the MIT license.   *)

MODULE Utils;

IMPORT Text, Rd, TextRd;

TYPE
  RefChar = REF ARRAY OF CHAR;

(* This is not ideal. The json text of a module is escaped for some characters
   especially newlines. We need updated text to match the original file
   so that we can match the line and col positions of messages like hover.*)
PROCEDURE Substitute(s : TEXT) : TEXT =
  VAR
    rd : Rd.T;
    ch,ch1,ch2,ch3 : CHAR;
    out : RefChar;
    index : INTEGER := 0;

PROCEDURE Out(ch : CHAR) =
  BEGIN
    out[index] := ch;
    INC(index);
  END Out;

PROCEDURE Out2(ch : CHAR) =
  VAR ch2 : CHAR;
  BEGIN
    ch2 := Rd.GetChar(rd);
    IF ch = '\\' AND ch2 = 'n' THEN
      Out('\n');
    ELSE
      Rd.UnGetChar(rd);
      Out(ch);
    END;
  END Out2;

PROCEDURE OutEsc(ch : CHAR) =
  BEGIN
    Out('\\'); Out(ch);
  END OutEsc;

(*
   in a \" block another " encoded by m3 as \" is \\\" in json
   and a \n in the block is \\n 
*)

PROCEDURE ScanText() =
  BEGIN
    LOOP
      ch := Rd.GetChar(rd);
      IF ch = '\\' THEN
        ch1 := Rd.GetChar(rd); (* should be \  but could be " the exit *)
        IF ch1 = '"' THEN
          Out(ch1);
          EXIT;
        END;
        IF ch1 = 'n' THEN
          Out('\n'); (* eol *)
          EXIT;
        END;
        Out(ch); (* out the \ *)
        ch2 := Rd.GetChar(rd); (* could be anything but possibly \ *)
        IF ch2 = '\\' THEN
          ch3 := Rd.GetChar(rd);
          Out(ch3);
        ELSE
          Out(ch2);
        END;
      ELSE
        Out(ch);
      END;
    END;
  END ScanText;

PROCEDURE ScanChar() =
  BEGIN
    (* legal escape sequences are \n \t \r \f \' \" \\ \nnn
       encoded as \\n \\t \\r \\f \\' \\\" \\\\ \\nnn *)
    ch1 := Rd.GetChar(rd);
    IF ch1 = '\\' THEN
      ch2 := Rd.GetChar(rd); (* eat the \ *)
      ch2 := Rd.GetChar(rd);
      IF ch2 = 'n' OR ch2 = 'r' OR ch2 = 't' OR ch2 = 'f' OR ch2 = '\'' THEN
        OutEsc(ch2);
      ELSIF ch2 = '\\' THEN
        ch3 := Rd.GetChar(rd); 
        IF ch3 = '\"' OR ch3 = '\\' THEN
          OutEsc(ch3);
        END;
      ELSIF ch2 >= '0' AND ch2 <= '7' THEN
        Out('\\'); Out(ch2);
      END;
    ELSE
      Out(ch1);
      ch2 := Rd.GetChar(rd);
      Out2(ch2);
    END
  END ScanChar;

  BEGIN
    out := NEW(RefChar,Text.Length(s));
    rd := TextRd.New(s);
    WHILE NOT Rd.EOF(rd) DO
      ch := Rd.GetChar(rd);
      IF ch = '\'' THEN
        Out(ch);
        ScanChar();
      ELSIF ch = '\\' THEN
        ch1 := Rd.GetChar(rd);
        IF ch1 = 'n' THEN
           Out('\n');
        ELSIF ch1 = 't' THEN
           Out('\t');
        ELSIF ch1 = '"' THEN
          (* skip the \ and out the " *)
           Out(ch1);
           ScanText();
        ELSE
           Out(ch); Out(ch1);
        END;
      ELSE
        Out(ch);
      END;
    END;
    RETURN Text.FromChars(SUBARRAY(out^,0,index-1));
  END Substitute;

BEGIN
END Utils.

