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
 * Created On      : Wed Mar  1 20:13:22 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Sat Aug  9 13:49:52 1997
 * Update Count    : 24
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.1.1.1  2001/12/02 00:29:10  wagner
 * Blair MacIntyre's rdwr library
 *
 * Revision 1.3  1997/08/11 20:36:24  bm
 * Various fixes
 *
 * 
 * HISTORY
 *)

MODULE TeeWr;

IMPORT WrClass, RefanyTbl, Text, Wr, Refany, Atom, AtomList;

FROM Wr IMPORT Failure;
FROM Thread IMPORT Alerted;

CONST
  BuffSize = 1024;

REVEAL
  T = Public BRANDED "TeeWr.T" OBJECT
        wrs: RefanyTbl.T := NIL;
      OVERRIDES
        init := Init;
        seek:= Seek;
        putString := PutString;
        flush := Flush;
        close := Close;
        tee := Tee;
        untee := Untee;
      END;

PROCEDURE Init (self: T): T =
  BEGIN
    IF (self.wrs = NIL) THEN
      self.wrs := NEW(RefanyTbl.Default).init();
    END;
    self.st := 0;
    self.lo := 0;
    self.cur := 0;
    self.hi := BuffSize;
    IF (self.buff = NIL) THEN
      self.buff := NEW(REF ARRAY OF CHAR, BuffSize);
    END;
    self.closed := FALSE;
    self.seekable := FALSE;
    self.buffered := TRUE;
    RETURN self;
  END Init;

EXCEPTION Error;                 <*FATAL Error*>

PROCEDURE Seek (self: T; n: CARDINAL) RAISES {Failure, Alerted} =
  BEGIN
    (* This file is not seekable, so only handle the special case. *)
    IF n # self.hi OR n # self.cur THEN RAISE Error; END;

    (* first, flush the output *)
    self.flush();

    (* now, mark the buffer as available *)
    self.lo := n;
    self.cur := n;
    self.hi := n + BuffSize;
  END Seek;

(* write the current buffered amount to a writer *)
PROCEDURE EmptyBuffer(self: T; wr: Wr.T) RAISES {Failure, Alerted} =
  VAR n := self.cur - self.lo;
  BEGIN
    <*ASSERT self.st = 0*>
    wr.putString(SUBARRAY(self.buff^, 0, n)); 
  END EmptyBuffer;

PROCEDURE PutString (self: T; READONLY a: ARRAY OF CHAR) 
      RAISES {Failure, Alerted} =
  VAR
    key: Text.T;
    val: Refany.T;
  BEGIN
    WITH i = self.wrs.iterate() DO
      WHILE i.next(key, val) DO
        WITH wr = NARROW(val, Wr.T) DO
          EmptyBuffer(self, wr);
          wr.putString(a);
        END;
      END;
    END;
    INC(self.cur, NUMBER(a));
    INC(self.hi, NUMBER(a));
    self.lo := self.cur;
  END PutString;

PROCEDURE Flush (self: T) RAISES {Failure, Alerted} =
  VAR
    key: Text.T;
    val: Refany.T;
  BEGIN
    WITH i = self.wrs.iterate() DO
      WHILE i.next(key, val) DO
        WITH wr = NARROW(val, Wr.T) DO
          EmptyBuffer(self, wr);
        END;
      END;
    END;
    self.lo := self.cur;
  END Flush;

PROCEDURE Close(self: T) =
  BEGIN
    self.wrs := NIL;
  END Close;

PROCEDURE Tee(self: T; name: Text.T; wr: Wr.T) RAISES {Failure} =
  VAR
    val: Refany.T;
  BEGIN
    IF self.wrs.get(name, val) THEN
      RAISE Wr.Failure(AtomList.List1(Atom.FromText(
                                          "writer already teed")));
    END;
    val := wr;
    IF NOT self.wrs.put(name, val) THEN 
      RAISE Wr.Failure(AtomList.List1(Atom.FromText(
                                          "writer couldn't be added to tee")));
    END;
  END Tee;

PROCEDURE Untee(self: T; name: Text.T): Wr.T RAISES {Failure} =
  VAR wr: Refany.T;
  BEGIN
    IF NOT self.wrs.delete(name, wr) THEN 
      RAISE Wr.Failure(AtomList.List1(Atom.FromText("writer not in tee")));
    END;
    RETURN NARROW(wr, Wr.T);
  END Untee;

BEGIN
END TeeWr. 
