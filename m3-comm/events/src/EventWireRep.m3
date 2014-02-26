(*                            -*- Mode: Modula-3 -*- 
 * 
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 * See file COPYRIGHT-COLUMBIA for details.
 * 
 * Author          : Blair MacIntyre
 * Created On      : Wed Jun  7 16:53:58 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Nov 21 17:47:12 1996
 * Update Count    : 7
 * 
 * $Source: /opt/cvs/cm3/m3-comm/events/src/EventWireRep.m3,v $
 *
 * $Date: 2014-02-26 23:16:56 $
 * $Author: rodney $
 * $Revision: 1.4 $
 *
 * $Log: not supported by cvs2svn $
 * Revision 1.2.10.1  2014-02-23 00:26:12  rodney
 * All head changes merged into branch.  Rebuilds itself twice.
 *
 * Revision 1.3  2013-09-06 05:30:44  jkrell
 * m3-comm/events is an out of date fork of m3-obliq/obliqrt
 *
 * m3-comm/events contains this code:
 * Int32 := Time.Now();
 *
 * m3-obliq contains similar:
 * Int32 := Time.Now() - Epoch;
 * Posix.Epoch = 0
 * Win32.Epoch = 1970
 *
 *
 * m3-comm (i.e. starting mentor) fails on AMD64_NT because we are
 * more than 2 billion seconds into the Win32 epoch (starting in 1601)
 * I386_NT succeeds albeit bogosusly -- the whole float to integer
 * conversion lacking any range chech..
 *
 *
 * Port m3-obliq to m3-comm.
 * This should fix AMD64_NT.
 *
 *
 * All platforms will blow up here in 2038.
 *
 * Revision 1.2  2001-12-02 00:20:38  wagner
 * add copyright notes, fix overrides for cm3, and make everything compile
 *
 * added: events/COPYRIGHT-COLUMBIA
 * added: events/src/COPYRIGHT-COLUMBIA
 * modified: events/src/Event.i3
 * modified: events/src/Event.m3
 * modified: events/src/EventConn.i3
 * modified: events/src/EventConn.m3
 * modified: events/src/EventCounter.i3
 * modified: events/src/EventCounter.m3
 * modified: events/src/EventHandle.i3
 * modified: events/src/EventIO.i3
 * modified: events/src/EventNumber.i3
 * modified: events/src/EventNumber.m3
 * modified: events/src/EventNumberF.i3
 * modified: events/src/EventPort.i3
 * modified: events/src/EventPort.m3
 * modified: events/src/EventProtocol.i3
 * modified: events/src/EventRd.i3
 * modified: events/src/EventRd.m3
 * modified: events/src/EventSpaceID.i3
 * modified: events/src/EventSpaceID.m3
 * modified: events/src/EventStubLib.i3
 * modified: events/src/EventStubLib.m3
 * modified: events/src/EventWireRep.i3
 * modified: events/src/EventWireRep.m3
 * modified: events/src/EventWr.i3
 * modified: events/src/EventWr.m3
 * modified: events/src/EventWrF.i3
 * modified: events/src/HostInfo.i3
 * modified: events/src/HostInfo.m3
 * modified: events/src/RdWrMutex.i3
 * modified: events/src/RdWrMutex.m3
 * modified: events/src/Work.i3
 * modified: events/src/WorkerPool.i3
 * modified: events/src/WorkerPool.m3
 * modified: events/src/Zombie.i3
 * modified: events/src/m3makefile
 * modified: events/src/m3overrides
 *
 * Revision 1.1.1.1  2001/12/02 00:06:45  wagner
 * Blair MacIntyre's events library
 *
 * Revision 1.2  1996/11/21 22:47:17  bm
 * fixed header
 *
 * 
 * HISTORY
 * - based on WireRep from the netobj package.
 *)
(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* EventWireRep.m3 *)
(* Last modified on Fri Feb 11 14:38:21 PST 1994 by wobber *)
(*      modified on Wed Jun 10 17:14:36 PDT 1992 by owicki *)

UNSAFE MODULE EventWireRep;

IMPORT NetObjEpoch, EventSpaceID, Time, Word, Fmt;  (* IO *)

TYPE
  Int32 = BITS 32 FOR [-2147483647-1..2147483647];
  TRep = RECORD ts: Int32; objNum: Int32; space: EventSpaceID.T; END;

VAR myTs: Int32 := GetTime();
    myObjNum := 0;
    mu := NEW(MUTEX);

PROCEDURE GetTime (): Int32 =
  BEGIN
    RETURN ROUND (Time.Now () - NetObjEpoch.T);
  END GetTime;

PROCEDURE New() : T =
  VAR wt: TRep;
  BEGIN
    LOCK mu DO
      IF myObjNum = LAST(Int32) THEN myTs := GetTime(); END;
      INC(myObjNum);
      wt.ts := myTs;
      wt.objNum := myObjNum;
    END;
    wt.space := EventSpaceID.Mine();
    RETURN LOOPHOLE(wt, T);
  END New;
  
PROCEDURE Equal(t1, t2: T) : BOOLEAN =
  BEGIN
    RETURN (t1 = t2);
  END Equal;


(* good only for 32-bit words *)
(*CONST Multiplier = -1640531527; 

PROCEDURE Hash(t: T) : Word.T =
  VAR x: Word.T;
  BEGIN
    x := Word.Xor(LOOPHOLE(t, TRep).objNum, LOOPHOLE(t, TRep).ts);
    IO.Put(Fmt.Unsigned(x, 16) & " " &
        Fmt.Unsigned(Word.Times(x, Multiplier), 16)
        & "\n");
    RETURN x;
  END Hash;
*)

PROCEDURE Hash(t: T) : Word.T =
  BEGIN
    RETURN Word.Xor(LOOPHOLE(t, TRep).objNum, LOOPHOLE(t, TRep).ts);
  END Hash;

PROCEDURE GetSpaceID(t: T) : EventSpaceID.T =
  BEGIN
    RETURN LOOPHOLE(t, TRep).space;
  END GetSpaceID;

PROCEDURE ToText(t: T) : TEXT =
  VAR tr := LOOPHOLE(t, TRep);
  BEGIN
    RETURN Fmt.Int(tr.ts) & "." & Fmt.Int(tr.objNum) & "@" &
           EventSpaceID.ToText(tr.space); 
  END ToText;

BEGIN
END EventWireRep.
