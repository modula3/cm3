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
 * Created On      : Wed Sep 13 12:18:39 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Fri Nov 22 13:59:06 1996
 * Update Count    : 16
 * 
 * $Source: /opt/cvs/cm3/m3-comm/sharedobj/src/LocalObjectSpace.i3,v $
 * $Date: 2001-12-02 13:14:14 $
 * $Author: wagner $
 * $Revision: 1.1.1.1 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.2  1996/11/22 18:59:10  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

INTERFACE LocalObjectSpace;

IMPORT ObjectSpace;

TYPE T <: ObjectSpace.Local;

END LocalObjectSpace.
