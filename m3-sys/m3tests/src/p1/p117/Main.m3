(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(*********************************************
Return-Path: stolfi@src.dec.com
Received: by jumbo.pa.dec.com; id AA15456; Fri, 7 Feb 92 15:53:12 -0800
From: stolfi (Jorge Stolfi)
Message-Id: <9202072353.AA15456@jumbo.pa.dec.com>
Date: Fri,  7 Feb 92 15:53:06 PST
To: kalsow, muller
Cc: swart (Garret Swart), src.m3
X-Folder-Carbon: sw-m3
Subject: Re: Shouldn't CopyBytes.i3 be flagged UNSAFE?
In-Reply-To: Message of Fri,  7 Feb 92 10:55:47 PST
    from swart (Garret Swart)
    <9202071856.AA03912@jumbo.pa.dec.com>


    [Garret:] The general translation from Modula 2+'s safe
    
       Byte.Copy(src, srcIndex, dst, dstIndex, cnt)
    
    is the unsafe
    
       Byte.Copy(
         SUBARRAY(LOOPHOLE(src, ARRAY OF CHAR), srcIndex, cnt),
         SUBARRAY(LOOPHOLE(dst, ARRAY OF CHAR), dstIndex, cnt));
    
The 2.0 compiler seems to have a problem with this construct.
Actually, the M3 compiler think's it is fine, but the C compiler
emits an inscrutable warning, "struct/union or struct/union pointer
required" for each LOOPHOLE.

Her is a sample program:

**********************************************)

    UNSAFE MODULE Main;
    
    PROCEDURE Copy(VAR src, dst: ARRAY OF CHAR) =
      BEGIN
        FOR i := 0 TO MIN(LAST(src), LAST(dst)) DO
          dst[i] := src[i]
        END
      END Copy;
    
    VAR st, dt: INTEGER;
    
    BEGIN
      Copy(
        SUBARRAY(LOOPHOLE(st, ARRAY OF CHAR), 0, 4),
        SUBARRAY(LOOPHOLE(dt, ARRAY OF CHAR), 0, 4)
      )
    END Main.
