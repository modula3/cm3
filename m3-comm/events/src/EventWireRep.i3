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
 * Created On      : Wed Jun  7 16:53:58 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Nov 21 17:46:45 1996
 * Update Count    : 6
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.2  1996/11/21 22:46:50  bm
 * fixed header
 *
 * 
 * HISTORY
 * - based on WireRep from the netobj package.
 *)
(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* EventWireRep.i3 *)
(* Last modified on Sun Sep 25 18:44:29 PDT 1994 by heydon     *)
(*      modified on Mon Jul 19 14:46:12 PDT 1993 by wobber     *)
(*      modified on Wed Jun 10 17:14:36 PDT 1992 by owicki     *)

(* The "EventWireRep" defines the network representation of network objects 
   and provides procedures to generate and manipulate values of this 
   type. *)
   
INTERFACE EventWireRep;

IMPORT EventSpaceID, Word;
FROM EventProtocol IMPORT Byte8;

CONST Brand = "EventWireRep";

TYPE T = RECORD byte: ARRAY [0..15] OF Byte8; END;

CONST NullT = T {byte := ARRAY [0..15] OF Byte8{0, ..}};
CONST SpecialT = T {byte := ARRAY [0..15] OF Byte8
                         {255, 255, 255, 255, 255, 255, 255, 255, 0, ..}};


(* A "EventWireRep.T" is a value which identifies a concrete network object. 
   In general, each "T" corresponds to one and only one real object 
   in a network.  Furthermore, each "T" is identifiable as having 
   been generated relative to a specific "EventSpaceID.T".   However, there 
   are two well-known values which are exceptions to this rule.  The 
   value "NullT" corresponds to the "NIL" network object, and the 
   value "SpecialT" corresponds a {\it special object} which is a 
   distinguished concrete object in every address space.  This special 
   object is private to the implementation of the network object 
   runtime. *)

PROCEDURE New() : T;

(* Generates a new, unique "EventWireRep.T" value.  "GetSpaceID(New()" is
   equal to "EventSpaceID.Mine()". *)

PROCEDURE GetSpaceID(t: T) : EventSpaceID.T;

(* Returns the "EventSpaceID.T" associated with the argument
   "EventWireRep.T". *) 

(* The following are for generic tables involving EventWireRep.T's *)

PROCEDURE Equal(t1, t2: T) : BOOLEAN;
PROCEDURE Hash(t: T) : Word.T;

(* For debugging. *)
PROCEDURE ToText(t: T): TEXT;

END EventWireRep.
