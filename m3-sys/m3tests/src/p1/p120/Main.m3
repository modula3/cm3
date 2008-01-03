(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(************************************
Return-Path: David.Evers@cl.cam.ac.uk
Received: by jumbo.pa.dec.com; id AA00720; Fri, 14 Feb 92 13:14:53 -0800
Received: by enet-gw.pa.dec.com; id AA05981; Fri, 14 Feb 92 13:14:44 -0800
Received: from kookaburra.cl.cam.ac.uk by swan.cl.cam.ac.uk 
          with SMTP (PP-6.0) to cl id <12118-0@swan.cl.cam.ac.uk>;
          Fri, 14 Feb 1992 21:14:26 +0000
To: m3-request
Cc: Peter.Robinson@cl.cam.ac.uk
Subject: 2.01.SPARC: Byte Exception Args broken on bigendian m/cs ??
Date: Fri, 14 Feb 92 21:14:23 +0000
From: David.Evers@cl.cam.ac.uk
Message-Id: <"swan.cl.ca.120:14.01.92.21.14.29"@cl.cam.ac.uk>

Folks - apologies if this is already fixed in 2.03:

On m3-2.01 built on a Sun4, the following program:

*****************************************************)

MODULE Main;

IMPORT Wr, Stdio, Fmt;
<*FATAL ANY*>

TYPE
  Enum = { a, b };

EXCEPTION
  Excn(Enum);

PROCEDURE Foo() RAISES {Excn}=
  BEGIN
    RAISE Excn(Enum.b);
  END Foo;


BEGIN
  TRY
    Wr.PutText(Stdio.stdout, Fmt.Int(ORD(Enum.b)) & "\n");
    Foo ();
  EXCEPT
  | Excn(i) => Wr.PutText(Stdio.stdout, Fmt.Int(ORD(i)) & "\n");
  END;
END Main.


(***********************************************************

prints

1
0


I think this is because of the following C code generated for the handler for
Excn(i) above:

...
#define _t6be426f3 unsigned char
...

#line 20
 _LOCAL _t6be426f3 i;

         _ZERO1B (& i);
#line 20
         i = *((_t6be426f3 * )&(_h2.exception->a)); /* OK on little-endian */
...



Regards,						---- Dave


**************************************************************)
