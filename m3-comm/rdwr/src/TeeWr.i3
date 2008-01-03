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
 * Created On      : Wed Mar  1 20:04:22 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Sat Aug  9 13:49:32 1997
 * Update Count    : 12
 * 
 * $Source: /opt/cvs/cm3/m3-comm/rdwr/src/TeeWr.i3,v $
 * $Date: 2001-12-02 00:35:21 $
 * $Author: wagner $
 * $Revision: 1.2 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.1.1.1  2001/12/02 00:29:10  wagner
 * Blair MacIntyre's rdwr library
 *
 * Revision 1.2  1997/08/11 20:36:24  bm
 * Various fixes
 *
 * 
 * HISTORY
 *)

INTERFACE TeeWr;

IMPORT Text, Wr, Thread;

TYPE
  T <: Public;
  Public = Wr.T OBJECT 
  METHODS 
    init (): T;

    (* add the named writer to the output list. *)
    tee (name: Text.T; wr: Wr.T) RAISES {Wr.Failure, Thread.Alerted};
    
    (* remove a named writer from the output list. *)
    untee (name: Text.T): Wr.T RAISES {Wr.Failure, Thread.Alerted};
  END;

  (* An initialized TeeWr.T returned by NEW(T).init() is an 
     output stream with which copies all it's output to all of it's
     output writers. If there are no writers currently on the output
     list, the TeeWr.T behaves like a NullWr.T  *)

END TeeWr. 
