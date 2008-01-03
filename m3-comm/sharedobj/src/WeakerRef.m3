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
 * Created On      : Fri Jul 14 18:09:35 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Fri Nov 22 14:04:19 1996
 * Update Count    : 6
 * 
 * $Source: /opt/cvs/cm3/m3-comm/sharedobj/src/WeakerRef.m3,v $
 * $Date: 2001-12-02 13:41:17 $
 * $Author: wagner $
 * $Revision: 1.2 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.1.1.1  2001/12/02 13:14:14  wagner
 * Blair MacIntyre's sharedobj package
 *
 * Revision 1.2  1996/11/22 19:04:23  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

MODULE WeakerRef;

PROCEDURE Equal(k1, k2: T): BOOLEAN =
  BEGIN
    RETURN k1.weakRef = k2.weakRef;
  END Equal;

BEGIN
END WeakerRef.
