(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(*************************************
Return-Path: <goldberg@parc.xerox.com>
Received: by jumbo.pa.dec.com; id AA25202; Thu, 2 Jan 92 12:01:18 -0800
Received: by enet-gw.pa.dec.com; id AA08293; Thu, 2 Jan 92 12:01:16 -0800
Received: from arcturia.parc.xerox.com ([13.2.116.23]) by alpha.xerox.com with SMTP id <11580>; Thu, 2 Jan 1992 12:00:34 PST
Received: by arcturia.parc.xerox.com id <3177>; Thu, 2 Jan 1992 12:00:14 PST
From: David Goldberg <goldberg@parc.xerox.com>
To: m3-request
Subject: compiler bug (1.6)
Cc: theimer@parc.xerox.com
Message-Id: <92Jan2.120014pst.3177@arcturia.parc.xerox.com>
Date: 	Thu, 2 Jan 1992 12:00:08 PST

The 1.6 compiler dies with
   M3 runtime error, "Type.m3":473: ASSERT failed
when compiling this program:

********************************)

MODULE t EXPORTS Main;


TYPE
    foo = OBJECT
	  METHODS
	      fooMethod ();
	  END;

    baz = foo OBJECT
	      OVERRIDES
		  fooMethod := BazMethod;
	      END;

    bar = foo OBJECT
	      OVERRIDES
		  fooMethod := BarMethod;
	      END;

PROCEDURE BazMethod(<*UNUSED*> self: bar) =
    BEGIN
    END BazMethod;


PROCEDURE BarMethod(<*UNUSED*> self: bar) =
    BEGIN
    END BarMethod;

BEGIN
  EVAL BITSIZE (baz); (*avoid an "not used" message*)
END t.


