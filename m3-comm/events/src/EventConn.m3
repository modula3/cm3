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
 * Created On      : Tue Jun 13 21:38:06 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Nov 21 17:33:25 1996
 * Update Count    : 4
 * 
 * $Source: /opt/cvs/cm3/m3-comm/events/src/EventConn.m3,v $
 * $Date: 2001-12-02 00:20:37 $
 * $Author: wagner $
 * $Revision: 1.2 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.1.1.1  2001/12/02 00:06:45  wagner
 * Blair MacIntyre's events library
 *
 * Revision 1.2  1996/11/21 22:33:31  bm
 * fixed the headers
 *
 * 
 * HISTORY
 *)

MODULE EventConn;

IMPORT Fingerprint;

(* We have one entry per address space. *)
PROCEDURE Equal(k1, k2: T): BOOLEAN =
  BEGIN
    RETURN Fingerprint.Equal(k1.space, k2.space);
  END Equal;

BEGIN
END EventConn.
