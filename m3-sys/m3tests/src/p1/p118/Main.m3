(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(*************************************************
Return-Path: goldberg@parc.xerox.com
Received: by jumbo.pa.dec.com; id AA28860; Tue, 11 Feb 92 21:46:04 -0800
Received: by inet-gw-1.pa.dec.com; id AA06992; Tue, 11 Feb 92 21:45:53 -0800
Received: from arcturia.parc.xerox.com ([13.2.116.23]) by alpha.xerox.com with SMTP id <11917>; Tue, 11 Feb 1992 21:45:31 PST
Received: by arcturia.parc.xerox.com id <3177>; Tue, 11 Feb 1992 21:46:32 PST
From: David Goldberg <goldberg@parc.xerox.com>
To: m3-request
Subject: LAST(REAL)
Message-Id: <92Feb11.214632pst.3177@arcturia.parc.xerox.com>
Date: 	Tue, 11 Feb 1992 21:46:22 PST


Twelve changes says LAST() is plus infinity, but the program
belows prints out 
  9.999999E36

*****************************************************)

MODULE Main;

IMPORT Wr, Stdio;
IMPORT Fmt;
<*FATAL ANY*>

PROCEDURE Main () =
  VAR
    txt: TEXT;
    r: REAL;
  BEGIN
    r := LAST(REAL);
    txt := Fmt.Real(r);
    Wr.PutText(Stdio.stdout, txt & "\n");
    Wr.Flush(Stdio.stdout);
  END Main;

BEGIN
  Main();
END Main.


