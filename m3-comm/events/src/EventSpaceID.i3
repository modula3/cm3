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
 * Last Modified On: Thu Nov 21 17:44:47 1996
 * Update Count    : 5
 * 
 * $Source: /opt/cvs/cm3/m3-comm/events/src/EventSpaceID.i3,v $
 * $Date: 2001-12-02 00:20:37 $
 * $Author: wagner $
 * $Revision: 1.2 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.1.1.1  2001/12/02 00:06:45  wagner
 * Blair MacIntyre's events library
 *
 * Revision 1.2  1996/11/21 22:44:52  bm
 * fixed header
 *
 * 
 * HISTORY
 * - based on SpaceID from the netobj package.
 *)
(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* EventSpaceID.i3 *)
(* Last modified on Mon Jul 19 13:50:29 PDT 1993 by wobber  *)
(*      modified on Thu Nov 19 21:28:19 1992 by gnelson *)
(*      modified on Wed Jun 10 16:56:10 PDT 1992 by owicki *)

(* The "EventSpaceID" interface is used to generate values which uniquely 
   identify address space instances across space and time. *)

INTERFACE EventSpaceID;

IMPORT Fingerprint;

CONST Brand = "EventSpaceID";

TYPE T = Fingerprint.T;

(* A "EventSpaceID.T" is a value which is sufficiently distinct to
   identify the address space which generated it among the set of all
   such address spaces.  Each value contains both an address and
   a time component, but the exact format is implementation dependent.
*)

PROCEDURE Mine() : T;

(* "Mine" returns the "T" value for the current address space.  It is
    distinct from all other such values in other address spaces.

   Implementation notes:

   The current implementation generates unique values by concatentating
   the local hardware Ethernet address and the real time clock.

   Any given implementation should be able to support aggregate
   "EventSpaceID.T" generation at sustained rate of at least one per second
   per Ethernet address.
   
   More detailed specification as to how values are generated
   is required to ensure uniqueness across multiple implementations.
*)

PROCEDURE ToText(id: T): TEXT;

(* "ToText" returns a textual representation of "T".  This is intended
   to be used for debugging purposes. 
*)

END EventSpaceID.
