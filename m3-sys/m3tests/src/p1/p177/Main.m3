(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(*****************
Return-Path: goldberg@parc.xerox.com
Received: by jumbo.pa.dec.com; id AA20657; Thu, 2 Apr 92 15:26:44 -0800
Received: by inet-gw-1.pa.dec.com; id AA13229; Thu, 2 Apr 92 15:26:39 -0800
Received: from arcturia.parc.xerox.com ([13.2.116.23]) by alpha.xerox.com with SMTP id <11661>; Thu, 2 Apr 1992 15:26:15 PST
Received: by arcturia.parc.xerox.com id <40741>; Thu, 2 Apr 1992 15:26:05 -0800
From: David Goldberg <goldberg@parc.xerox.com>
To: m3-request
Subject: 2.04 compiler bug
Cc: theimer@parc.xerox.com
Message-Id: <92Apr2.152605pst.40741@arcturia.parc.xerox.com>
Date: Thu, 2 Apr 1992 15:25:59 PST

This looks like a compiler bug to me.  In the following
program, procedures Foo() and Foo2() are the same except
one of them operates on objects whose fields are enumerate-valued,
the other on objects whose fields are integers.

The output of both Foo's should be "1 1", but when I run the program
I get "1 0" from Foo, "1 1" from Foo1.

	-david
	
*******************************)

MODULE Main;

IMPORT Wr, Stdio, Thread, Fmt;

TYPE

  SubRange = [0 .. 3];
  Int = INTEGER;

  BaseType = OBJECT field: SubRange;  END;
  SubType = BaseType BRANDED OBJECT subField: SubRange;  END;

  BaseType2 = OBJECT field: Int;  END;
  SubType2 = BaseType2 BRANDED OBJECT subField: Int;  END;

PROCEDURE Put (txt: TEXT) =
  <* FATAL Wr.Failure, Thread.Alerted *>
  BEGIN
    Wr.PutText(Stdio.stderr, txt);
    Wr.Flush(Stdio.stderr);
  END Put;

PROCEDURE Foo () =
  VAR
    ret          : BaseType := NIL;
    valIn, valOut: INTEGER;
  BEGIN
    valIn := 1;
    ret := NEW(SubType, subField := valIn);
    valOut := NARROW(ret, SubType).subField;
    Put(Fmt.F("%s %s\n", Fmt.Int(valIn), Fmt.Int(valOut)));
  END Foo;

PROCEDURE Foo2 () =
  VAR
    ret          : BaseType2 := NIL;
    valIn, valOut: INTEGER;
  BEGIN
    valIn := 1;
    ret := NEW(SubType2, subField := valIn);
    valOut := NARROW(ret, SubType2).subField;
    Put(Fmt.F("%s %s\n", Fmt.Int(valIn), Fmt.Int(valOut)));
  END Foo2;

BEGIN
  Foo();
  Foo2();
END Main.

