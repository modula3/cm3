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
 * Last Modified On: Mon Dec  2 15:36:10 1996
 * Update Count    : 13
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
 * Revision 1.3  1997/01/23 15:26:36  bm
 * Lots of little bug fixes.
 *
 * Revision 1.2  1996/11/21 22:37:11  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

INTERFACE EventNumber;

IMPORT Text, Fmt;

CONST Brand = "Event Number";

TYPE
  T <: Public;
  Public = OBJECT METHODS
    init(t: T := NIL): T;
    inc() RAISES {Overflow};
    dec() RAISES {Overflow};
    compare(p2: T): [-1..1];
    fmt(base: Fmt.Base := 16): Text.T;
  END;

(* If inc overflows the event number, it will raise this exception. *)
EXCEPTION Overflow;

PROCEDURE New(t: T := NIL): T;
(* Same as "NEW(T).init(t)" *)

PROCEDURE Compare(p1, p2: T): [-1..1];
(* Same as "p1.compare(p2)" *)

END EventNumber.
