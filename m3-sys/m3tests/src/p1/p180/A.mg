(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(*********
Return-Path: goldberg@parc.xerox.com
Received: by jumbo.pa.dec.com; id AA02529; Tue, 21 Apr 92 13:38:23 -0700
Received: by inet-gw-1.pa.dec.com; id AA02922; Tue, 21 Apr 92 13:38:15 -0700
Received: from arcturia.parc.xerox.com ([13.2.116.23]) by alpha.xerox.com with SMTP id <11793>; Tue, 21 Apr 1992 13:37:50 PDT
Received: by arcturia.parc.xerox.com id <40744>; Tue, 21 Apr 1992 13:37:41 -0700
From: David Goldberg <goldberg@parc.xerox.com>
To: m3-request
Subject: extra warning message
Message-Id: <92Apr21.133741pdt.40744@arcturia.parc.xerox.com>
Date: Tue, 21 Apr 1992 13:37:40 PDT

Compiling the program below gives the message
   "./Tst.mg", line 2: warning: not used (Float)
which seems wrong to me:
***********************)

GENERIC MODULE A (Float);
FROM Float IMPORT Scalb;
<*FATAL ANY*>
BEGIN
  EVAL Scalb(1.0, 10);
END A.


