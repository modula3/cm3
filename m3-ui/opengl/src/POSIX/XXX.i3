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
 * Created On      : Sun Apr  5 14:03:43 1998
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Sun Apr  5 14:09:49 1998
 * Update Count    : 2
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.1  1998/05/13 17:25:13  bm
 * new files
 *
 * 
 * HISTORY
 *)

UNSAFE INTERFACE XXX;

FROM Ctypes IMPORT int;
FROM Utime IMPORT struct_timeval;
FROM Unix IMPORT FDSet;

(* This procedure will have an external symbol of XXX__S.  As a really
   gross hack, we can manually edit any link library (at least on
   Solaris) and change an external reference to "select" to "XXX__X",
   which will pick up this.  Then, we can redirect the select call to
   SchedulerPosix.IOWait so it doesn't block the process! *)

PROCEDURE S (nfds: int; readfds, writefds, exceptfds: UNTRACED REF FDSet;
             timeout: UNTRACED REF struct_timeval): int;

END XXX.
