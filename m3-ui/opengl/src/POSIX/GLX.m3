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
 * Created On      : Sun Apr  5 14:02:40 1998
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Sun Apr  5 14:47:54 1998
 * Update Count    : 8
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.1  1998/05/13 17:25:12  bm
 * new files
 *
 * 
 * HISTORY
 *)

UNSAFE MODULE GLX;

IMPORT Unix, SchedulerPosix;

FROM Ctypes IMPORT int;
FROM Utime IMPORT struct_timeval;
FROM Unix IMPORT FDSet;

(* This routine is designed to handle one specific case, used inside
   the X libraries by XWaitForReadable:
       select(nfds, readfds, NIL, NIL, NIL)
   where only one fd is set in readfds.
*)
PROCEDURE S (nfds: int; readfds, writefds, exceptfds: UNTRACED REF FDSet;
             timeout: UNTRACED REF struct_timeval): int =
  VAR found := -1;
  BEGIN
    (* obvious violations of our case *)
    IF writefds # NIL OR exceptfds # NIL OR timeout # NIL THEN
      RETURN Unix.select(nfds, readfds, writefds, exceptfds, timeout);
    END;
      
    FOR i := 0 TO nfds-1 DO
      IF i IN readfds^ THEN
        IF found > -1 THEN 
          (* more than one fd is found, so let's just call select and be
             done with it! *)
          RETURN Unix.select(nfds, readfds, writefds, exceptfds, timeout);
        END;
        found := i;
      END;
    END;

    IF found = -1 THEN
      (* not sure what to do if no file descriptor is set, so just
         let the system decide! *)
      RETURN Unix.select(nfds, readfds, writefds, exceptfds, timeout);
    END;
      
    CASE SchedulerPosix.IOWait(found, TRUE) OF
    | SchedulerPosix.WaitResult.Ready =>
      RETURN 1;
    | SchedulerPosix.WaitResult.Error, SchedulerPosix.WaitResult.FDError,
      SchedulerPosix.WaitResult.Timeout =>
      readfds^ := readfds^ - FDSet{found};
      RETURN -1;
    END;
  END S;

BEGIN
END GLX.
