(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
Return-Path: meehan@src.dec.com
Received: by jumbo.pa.dec.com; id AA24478; Sun, 16 Feb 92 23:52:13 -0800
From: meehan (Jim Meehan)
Message-Id: <9202170752.AA24478@jumbo.pa.dec.com>
Date: Sun, 16 Feb 92 23:52:09 PST
To: kalsow, muller
X-Folder-Carbon: M
Subject: unimportant bug in compiler


I erroneously duplicated a field ("corpusPort") in an object, which
crashed the compiler.  Transcript:


bamboozle 99> m3make
m3 -w1 -make -why  -g -times -g   -o Argus -F.PGM_SOURCES -lm3lego  -lm3miniformsvbt  -lm3query  -lm3ipcf  -lm3uprocess  -lm3glist  -lm3sgml  -lm3sx  -lm3rsrc  -lm3color  -lm3rdwrutils  -lm3inflect  -lm3vtext  -lm3mtext  -lm3ui -lm3X11R4 -lX11
new source -> compile ../src/CWindow.m3
"../src/CWindow.m3", line 31: symbol redefined (corpusPort)
M3 runtime error: ASSERT failed


 seconds  #times  operation
   15.53      19  inhaling library link info
    0.07      23  checking object timestamps
    5.26      22  checking old link info
   16.67       1  compiling Modula-3 -> C  [m3compiler]
   12.51       1  garbage collection
    1.43          other
---------------------------------------------------
   51.51          TOTAL


Fatal Error: program "/proj/m3/lib.mips/m3compiler" got fatal signal 3

*** Error code 255

Stop.



Here's the code:


REVEAL
  Concordance.Window = PublicWindow BRANDED OBJECT
                         c: Concordance.T;
                         pt  : Point.T;    (* current position of cursor *)
                         ipcf: ArgusIPCF;
                         corpusTextEditor: TextEditVBT.T;
                         corpusPort      : CorpusPort;
                         senseBoxPort    : SenseBoxPort;
                         corpusPort      : CorpusPort;
                         senseTag: TEXT := NIL; (* "ditto" sense-tag *)
                         senseTagExp : REFANY           := NIL;
                         UIDvec      : CLine.UIDvector  := NIL;
                         edited                         := FALSE;
                         lineL, lineR: INTEGER;
                         interval    : VText.Interval; (* highlight *)
                         lineInterval: Interval.T; (* line begin/end *)
                       END;
