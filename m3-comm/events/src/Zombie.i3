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
 * Created On      : Thu May 11 13:15:50 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Nov 21 17:52:29 1996
 * Update Count    : 5
 * 
 * $Source: /opt/cvs/cm3/m3-comm/events/src/Zombie.i3,v $
 * $Date: 2001-12-02 00:06:45 $
 * $Author: wagner $
 * $Revision: 1.1.1.1 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.2  1996/11/21 22:52:41  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

INTERFACE Zombie;

IMPORT Thread;

CONST Brand: TEXT = "Zombie";

TYPE T = Thread.T;

END Zombie.
