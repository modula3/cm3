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
 * Created On      : Tue May 30 23:00:08 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Aug  4 12:30:02 1997
 * Update Count    : 21
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.1.1.1  2001/12/02 00:06:45  wagner
 * Blair MacIntyre's events library
 *
 * Revision 1.4  1997/08/04 20:15:10  bm
 * Fixed BRANDs
 *
 * Revision 1.3  1997/01/23 15:26:37  bm
 * Lots of little bug fixes.
 *
 * Revision 1.2  1996/11/21 22:37:42  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

MODULE EventNumber;

IMPORT Fmt, Text, EventNumberF;
FROM EventProtocol IMPORT Word32;

REVEAL
  T = EventNumberF.Private BRANDED "EventNumber.T" OBJECT 
  OVERRIDES
    init := Init;
    inc := Inc;
    dec := Dec;
    compare := Compare;
    fmt := Format;
  END;

PROCEDURE Init(self: T; t: T := NIL): T =
  BEGIN
    IF t # NIL THEN
      self.lo := t.lo;
      self.hi := t.hi;
    ELSE
      self.lo := 0;
      self.hi := 0;
    END;
    RETURN self;
  END Init;

PROCEDURE Inc(self: T) RAISES {Overflow} =
  BEGIN
    IF self.lo = LAST(Word32) THEN
      IF self.hi = LAST(Word32) THEN
        RAISE Overflow;
      ELSE
        self.lo := 0;
        INC(self.hi);
      END;
    ELSE
      INC(self.lo);
    END;
  END Inc;

PROCEDURE Dec(self: T) RAISES {Overflow} =
  BEGIN
    IF self.lo = FIRST(Word32) THEN
      IF self.hi = FIRST(Word32) THEN
        RAISE Overflow;
      ELSE
        self.lo := LAST(Word32);
        DEC(self.hi);
      END;
    ELSE
      DEC(self.lo);
    END;
  END Dec;

PROCEDURE Format(self: T; base: Fmt.Base) : Text.T =
  BEGIN
    RETURN Fmt.Unsigned(self.hi, base) & "." & Fmt.Unsigned(self.lo, base);
  END Format;

PROCEDURE New(t: T := NIL): T =
  BEGIN
    RETURN NEW(T).init(t);
  END New;

PROCEDURE Compare(p1, p2: T): [-1..1] =
  BEGIN
    IF p1.hi > p2.hi THEN
      RETURN 1;
    ELSIF p1.hi < p2.hi THEN
      RETURN -1;
    ELSIF p1.lo > p2.lo THEN
      RETURN 1;
    ELSIF p1.lo < p2.lo THEN
      RETURN -1;
    END;
    RETURN 0;
  END Compare;

BEGIN
END EventNumber.
