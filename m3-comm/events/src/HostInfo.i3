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
 * Created On      : Tue Apr 25 14:00:50 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Oct 23 14:44:04 1997
 * Update Count    : 28
 * 
 * $Source: /opt/cvs/cm3/m3-comm/events/src/HostInfo.i3,v $
 * $Date: 2001-12-02 00:06:45 $
 * $Author: wagner $
 * $Revision: 1.1.1.1 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.4  1997/10/24 19:31:33  bm
 * Added the ability to flush the readers and worker pool.
 *
 * Revision 1.3  1997/01/23 15:26:40  bm
 * Lots of little bug fixes.
 *
 * Revision 1.2  1996/11/21 22:50:32  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

(* The "HostInfo" type contains the information we need to maintain
   per host connection.
*)

INTERFACE HostInfo;

IMPORT Thread, EventSeq, EventConn;

CONST Brand = "Host Info";

TYPE T = Thread.Mutex OBJECT
           conn: EventConn.T;
           reader: Thread.T;
           writer: Thread.T;
           es: EventSeq.T;
           mu: Thread.Mutex;
           cv: Thread.Condition;
           rdmu: Thread.Mutex;
           rdcv: Thread.Condition;
           blocking: REF BOOLEAN;
         END;

PROCEDURE Equal(k1, k2: T): BOOLEAN;

END HostInfo.
