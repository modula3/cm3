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
 * Created On      : Thu Jun  8 23:04:24 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Tue Dec  3 13:14:02 1996
 * Update Count    : 47
 * 
 * $Source: /opt/cvs/cm3/m3-comm/events/src/Event.m3,v $
 * $Date: 2001-12-02 00:20:37 $
 * $Author: wagner $
 * $Revision: 1.2 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.1.1.1  2001/12/02 00:06:45  wagner
 * Blair MacIntyre's events library
 *
 * Revision 1.2  1997/01/23 15:26:34  bm
 * Lots of little bug fixes.
 *
 * 
 * HISTORY
 *)

MODULE Event;
IMPORT EventSpaceID, EventNumber, EventNumberF, TimeStamp;
FROM EventProtocol IMPORT StubProtocol, ID, NativeRep;
IMPORT Fmt, IO, ParseParams, Stdio;

REVEAL
  T = Public BRANDED "Event v1.0" OBJECT
    (* for reference counting *)
    count : INTEGER := 1;
  OVERRIDES
    init := Init;
    addRef := AddRef;
    dropRef := DropRef;
  END;

VAR 
  freeEvent: T := NIL;
  mu: MUTEX;

PROCEDURE New (): T = 
  VAR ev: T;
  BEGIN
    LOCK mu DO
      IF freeEvent # NIL THEN
        IF debug THEN
          IO.Put("Event.New: reusing event " & ToText(freeEvent) & "\n");
        END;
        ev := freeEvent;
        freeEvent := ev.next;
        ev.count := 1;
        ev.next := NIL;
      ELSE
        ev := NEW(T);
        ev.rd := NIL;
        ev.num := EventNumber.New();
      END;
    END;
    RETURN ev;
  END New;

PROCEDURE Init (ev: T; id: ID; eventProt: StubProtocol; num: EventNumber.T) = 
  BEGIN
    ev.count := 1;

    ev.num := ev.num.init(num);
    ev.hdr.rep := NativeRep;
    ev.from := EventSpaceID.Mine();
    ev.ts := TimeStamp.New();

    ev.hdr.rep.id := id;
    ev.hdr.prot := eventProt;
    ev.hdr.numHi := ev.num.hi;
    ev.hdr.numLo := ev.num.lo;
    ev.prot := eventProt;
  END Init;

PROCEDURE DropRef(ev: T) =
  BEGIN
    LOCK mu DO
      DEC(ev.count);
      IF debug THEN
        IO.Put("Event.DropRef: dropping ref to " & ToText(ev) & "\n");
      END;
      IF ev.count > 0 THEN RETURN END;

      IF debug THEN
        IO.Put("Event.DropRef: added to free list\n");
      END;
      ev.next := freeEvent;
      ev.sender := NIL;
      freeEvent := ev;
    END;
  END DropRef;

PROCEDURE AddRef(ev: T) =
  BEGIN
    LOCK mu DO
      INC(ev.count);
      IF debug THEN
        IO.Put("Event.AddRef: adding ref to " & ToText(ev) & "\n");
      END;
    END;
  END AddRef;

PROCEDURE ToText(ev: T): TEXT =
  BEGIN
    WITH hdr = ev.hdr, rep = hdr.rep DO
      RETURN "{(" & Fmt.Int(ev.count) & ")" & 
             Fmt.Int(rep.id, 16) & "+" & Fmt.Int(rep.intFmt, 16) &
             "+" & Fmt.Int(rep.floatFmt, 16) & "+" &
             Fmt.Int(rep.charSet, 16) & "/" & Fmt.Int(ev.prot) & 
             "/" & EventSpaceID.ToText(ev.from) & "/" &
             ev.num.fmt(10) & "}";
    END;
  END ToText;

VAR
  debug: BOOLEAN := FALSE;

BEGIN
  mu := NEW(MUTEX);
  debug := NEW(ParseParams.T).init(Stdio.stderr).keywordPresent("@EVdebug");
END Event.
