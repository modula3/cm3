(*                            -*- Mode: Modula-3 -*- 
 * 
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 * 
 * Author          : Blair MacIntyre
 * Created On      : Mon Jul 17 21:23:42 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Aug  4 12:30:02 1997
 * Update Count    : 16
 * 
 * $Log$
 * Revision 1.3  1997/08/04 20:15:05  bm
 * Fixed BRANDs
 *
 * Revision 1.2  1997/03/12 21:46:33  bm
 * small addition
 *
 * 
 * HISTORY
 *)

MODULE Debug;

IMPORT Atom, AtomList, IO, Process, ThreadF, Fmt;

REVEAL
  T = Public BRANDED "Debug.T" OBJECT
    tags: AtomList.T := NIL;
    p: INTEGER := 0;
    name: TEXT := "";
  OVERRIDES
    init := Init;
    print := Print;
    push := Push;
    pop  := Pop;
    getList := GetList;
    getText := GetText;
    crash := CrashT;
  END;

PROCEDURE Init(self: T; name: TEXT; p: INTEGER): T =
  BEGIN
    self.p := p;
    self.name := name;
    RETURN self;
  END Init;

PROCEDURE Push(self: T; p: INTEGER; tag: Atom.T) =
  BEGIN
    self.tags := AtomList.Cons(tag, self.tags);
    IF p < self.p THEN
      IO.Put(self.name & ": " & Atom.ToText(tag) & " begin\n");
    END;
  END Push;

PROCEDURE Pop (self: T; p: INTEGER) =
  BEGIN
    IF self.tags # NIL THEN
      IF p < self.p THEN
        IO.Put(self.name & ": " & Atom.ToText(self.tags.head) & " end\n");
      END;
      self.tags := self.tags.tail;
    END;
  END Pop;

PROCEDURE Print(self: T; p: INTEGER; text: TEXT) =
  BEGIN
    IF p < self.p THEN
      IO.Put(self.name & " (" & Fmt.Int(ThreadF.MyId()) & "): " & text & "\n");
    END;
  END Print;

PROCEDURE GetList(self: T): AtomList.T =
  BEGIN
    RETURN AtomList.Reverse(self.tags);
  END GetList;

(* Pack them together in reverse order. *)
PROCEDURE GetText(self: T): TEXT =
  VAR ret: TEXT;
      al: AtomList.T := self.tags;
  BEGIN
    IF al # NIL THEN
      ret := Atom.ToText(al.head);
      al := al.tail;
    ELSE
      ret := "";
    END;
    WHILE al # NIL DO
      ret := Atom.ToText(al.head) & " " & ret;
      al := al.tail;
    END;
    RETURN ret;
  END GetText;

PROCEDURE CrashT (self: T; t: TEXT; al: AtomList.T) =
  BEGIN
    Crash(t & ": " & AtomListToText(self.tags), al);
  END CrashT;

PROCEDURE PrintAtomList (t: TEXT; al: AtomList.T) =
  BEGIN
    IO.Put(t & ": " & AtomListToText(al) & "\n");
  END PrintAtomList; 

PROCEDURE Crash (t: TEXT; al: AtomList.T) =
  BEGIN
    Process.Crash(t & ": " & AtomListToText(al));
  END Crash;

PROCEDURE AtomListToText (al: AtomList.T) : TEXT =
  VAR ret: TEXT;
  BEGIN
    IF al # NIL THEN
      ret := Atom.ToText(al.head);
      al := al.tail;
    ELSE
      ret := "";
    END;
    WHILE al # NIL DO
      ret := ret & ", " & Atom.ToText(al.head);
      al := al.tail;
    END;
    RETURN ret;
  END AtomListToText; 

BEGIN
END Debug.
