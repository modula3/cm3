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
 * Created On      : Mon Jul 17 20:51:37 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Nov 25 17:39:12 1996
 * Update Count    : 11
 * 
 * $Source: /opt/cvs/cm3/m3-libs/debug/src/Debug.i3,v $
 * $Date: 2001-12-01 14:34:16 $
 * $Author: wagner $
 * $Revision: 1.2 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.2  1996/11/25 22:39:19  bm
 * fixed headers
 *
 * 
 * HISTORY
 *)

(* This interface provides an object for maintaining a trace of
   program flow and controlling the amount of debug printing done at
   run-time.  

   By surrounding each section of code by an appropriate "push()" and
   "pop()" call, at any time the stack of tags can be retrieved and
   printed or used as an exception argument, for example.

   Furthermore, the parameter "p" is used to control the printing of
   tags and other debugging output during run time.  If a tag is
   "push()"ed with a lower "p" than the "Debug.T" object was
   initialized with, it is printed immediately.  Similarly, a call to
   "print()" only results in output if its "p" argument is less than
   the value of the "Debug.T" object.

*)

INTERFACE Debug;

IMPORT Atom, AtomList;

TYPE
  T <: Public;
  Public = OBJECT METHODS
    init(name: TEXT; p: INTEGER): T;
    push(p: INTEGER; tag: Atom.T);
    pop (p: INTEGER);
    print(p: INTEGER; text: TEXT);
    getList(): AtomList.T;
    getText(): TEXT;
    crash (t: TEXT; al: AtomList.T);
  END;

PROCEDURE AtomListToText (al: AtomList.T) : TEXT;

PROCEDURE PrintAtomList (t: TEXT; al: AtomList.T);

PROCEDURE Crash (t: TEXT; al: AtomList.T);

END Debug.
