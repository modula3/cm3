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
 * Created On      : Mon May 29 15:33:17 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Fri Nov 22 13:59:34 1996
 * Update Count    : 28
 * 
 * $Source: /opt/cvs/cm3/m3-comm/sharedobj/src/Message.i3,v $
 * $Date: 2001-12-02 13:41:16 $
 * $Author: wagner $
 * $Revision: 1.2 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.1.1.1  2001/12/02 13:14:14  wagner
 * Blair MacIntyre's sharedobj package
 *
 * Revision 1.2  1996/11/22 18:59:38  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

INTERFACE Message;

IMPORT Event, ObjectInfo, EventStubLib;

CONST Brand = "Message";

TYPE 
  T = REF RECORD 
    next: T := NIL;

    ev: Event.T;
    h: EventStubLib.Handle;
    thread: INTEGER;
    objInfo: ObjectInfo.T; 
  END;

(* The Message structure contains all the information carried around
   by a message on a particular host.  "ev" is the event received from
   the Event package.  "h" is the handle to be used to read additional
   data out of the event.  "thread" is an identifier for the thread
   that initiated the call that resulted in this event, and is used to
   wake up that thread when the sequenced event is returned to that
   space.  *)

PROCEDURE Equal(k1, k2: T): BOOLEAN;

(* Create a text version of the message for printing during debugging. *)
PROCEDURE ToText(m: T): TEXT;

END Message.   
