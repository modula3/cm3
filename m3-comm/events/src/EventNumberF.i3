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
 * Last Modified On: Thu Nov 21 17:38:11 1996
 * Update Count    : 20
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
 * Revision 1.2  1996/11/21 22:38:16  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

INTERFACE EventNumberF;

IMPORT EventNumber;
FROM EventProtocol IMPORT Word32;

REVEAL
  EventNumber.T <: Private;

TYPE
  Private = EventNumber.Public OBJECT 
    lo: Word32;
    hi: Word32;
  END;

END EventNumberF.
