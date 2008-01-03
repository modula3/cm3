(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(***************
Return-Path: <nr@Princeton.EDU>
Received: by jumbo.pa.dec.com; id AA02509; Tue, 21 Jan 92 13:08:33 -0800
Received: by enet-gw.pa.dec.com; id AA14411; Tue, 21 Jan 92 13:07:50 -0800
Received: from fs.Princeton.EDU by Princeton.EDU (5.65b/2.85/princeton)
	id AA29216; Tue, 21 Jan 92 16:07:37 -0500
Received: from cs.Princeton.EDU (dynamic.Princeton.EDU) by fs.Princeton.EDU (4.0/1.105)
	id AA06585; Tue, 21 Jan 92 16:07:36 EST
Received: by cs.Princeton.EDU (5.57/1.105)
	id AA06382; Tue, 21 Jan 92 16:07:33 -0500
Date: Tue, 21 Jan 92 16:07:33 -0500
From: Norman Ramsey <nr@Princeton.EDU>
Message-Id: <9201212107.AA06382@cs.Princeton.EDU>
To: m3-request
Subject: TextRd implementation

The following code demonstrates that the TextRd implementation distributed
with 1.6 is incorrect.  The seekable implementation that I sent to the
newsgroup earlier seems to work OK on this example:

nr@dynamic (66) % m3 Bug27.m3
nr@dynamic (67) % a.out
Text.Length(s1300) == 1318 [should be 1318]
Length after reading is 1000
nr@dynamic (68) % m3 Bug27.m3 ~/src/m3/TextRd.mo
nr@dynamic (69) % a.out
Text.Length(s1300) == 1318 [should be 1318]
Length after reading is 1318

************************)

MODULE Main;
IMPORT Fmt, Rd, Wr, Text, TextRd;
FROM Stdio IMPORT stdout;
<*FATAL ANY*>

VAR sentence := "Now is the time for all good men to come to the aid of the party.";
    s130 := sentence & sentence;
    s1300 := "";

    rd : TextRd.T;

BEGIN
  FOR i := 1 TO 10 DO
    s1300 := s1300 & s130;
(*    
  Wr.PutText(stdout, Fmt.F("Text.Length(s1300) == %s [should be 1318]\n",
			Fmt.Int(Text.Length(s1300))));
  Wr.Flush (stdout);
*)
  END;
  s1300 := s1300 & "That\'s all, folks!";
  Wr.PutText(stdout, Fmt.F("Text.Length(s1300) == %s [should be 1318]\n",
			Fmt.Int(Text.Length(s1300))));
  Wr.Flush (stdout);
  rd := TextRd.New(s1300);
  s1300 := "";
  TRY
    LOOP s1300 := s1300 & Text.FromChar(Rd.GetChar(rd)); END;
  EXCEPT Rd.EndOfFile => (* skip *)
  END;
  Wr.PutText(stdout, Fmt.F("Length after reading is %s\n",
			Fmt.Int(Text.Length(s1300))));
  Wr.Flush (stdout);
END Main.

(***********

Norman
************)
