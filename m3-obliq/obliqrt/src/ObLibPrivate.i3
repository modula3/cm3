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
 * Created On      : Tue Jun 24 13:26:18 1997
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Tue Jun 24 16:07:15 1997
 * Update Count    : 4
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.1  1997/07/11 18:07:01  bm
 * Forgot to add these!
 *
 * 
 * HISTORY
 *)

INTERFACE ObLibPrivate;

IMPORT ObLib, NetObj, TimeStamp;

TYPE
  Handle = NetObj.T OBJECT
    getOpCodes(ts: TimeStamp.T): REF ObLib.OpCodes;
  END;

END ObLibPrivate.
