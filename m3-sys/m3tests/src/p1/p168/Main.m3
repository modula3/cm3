(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(****************************************
Return-Path: muller@src.dec.com
Received: by jumbo.pa.dec.com; id AA15916; Sat, 22 Feb 92 14:27:18 -0800
Received: by procope.pa.dec.com; id AA00786; Sat, 22 Feb 92 15:27:13 -0700
Message-Id: <9202222227.AA00786@procope.pa.dec.com>
From: Eric Muller <muller>
To: kalsow
Subject: bug in m3compiler
X-Face: "+e#K^#Pdh#:jtB:sj)o[$4J,k13_QQGbh!#rPQ~K{4Eollsb.w)BT!83lr{_<P+*J{bQoO
 8_u)[OmQN~fs$0dRY4f}&8{8v/tO~s8Z*f;5G{o']t]NymjYcU:QouCyzvLN&jM#WXt$(n&PC#4hzl
 ?9pK9sJJ"Cu+LHrBTJea'jelk4qKxO`,8.^};/b=1"}>Fz^j-~
Date: Sat, 22 Feb 92 14:27:12 PST
Sender: muller


************************************************************)

UNSAFE MODULE Main;

EXCEPTION E;

TYPE
  SignalHandler = PROCEDURE (sig, code: INTEGER;
                             scp: UNTRACED REF struct_sigcontext);
TYPE
  struct_sigcontext = RECORD
    a, b, c: INTEGER;
  END;

  sigset_t = INTEGER;

  struct_sigvec  = RECORD
    sv_handler: SignalHandler;     (* signal handler *)
    sv_mask:    sigset_t;          (* signal mask to apply *)
    sv_flags:   INTEGER;           (* see signal options below *)
  END;

PROCEDURE Foo (sig: INTEGER;  code: INTEGER; 
               scp: UNTRACED REF struct_sigcontext) RAISES {E} =
  BEGIN
    EVAL sig;
    EVAL code;
    EVAL scp;
  END Foo;

CONST
  empty_sigset_t : sigset_t = 0;

VAR
  x := struct_sigvec {LOOPHOLE(Foo, SignalHandler), empty_sigset_t, 0};

BEGIN
  EVAL x;
END Main.

(*************************************************************
newport> m3 -c Bug.m3
M3 runtime error: ASSERT failed


Fatal Error: program "/proj/m3/lib.mips/m3compiler" got fatal signal 3

newport> 
*************************************************************)
