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
 * Created On      : Mon Feb 20 17:43:14 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Sat Aug  9 13:47:31 1997
 * Update Count    : 8
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.2  1997/08/11 20:36:20  bm
 * Various fixes
 *
 * 
 * HISTORY
 *)

(* The FileRdWr module sets up and returns a Rd/Wr pair which are
   linked together. Anything written to the writer is immediately
   available to the reader.  *)

INTERFACE RdWrPipe;

IMPORT Rd, Wr;

CONST
  (* the default size of the shared buffer *)
  BufferSize = 1024;

PROCEDURE New(VAR rd: Rd.T; VAR wr: Wr.T; buff_size: CARDINAL :=
  BufferSize; nm : TEXT := NIL);
(* Returns a read and writer which are connected together. *)

PROCEDURE ResetRdCounter(rd: Rd.T);
(* Reset the cur, lo and hi pointers, to allow this to read more
   characters than LAST(CARDINAL).  Should be called periodically. *)

PROCEDURE ResetWrCounter(wr: Wr.T);
(* Reset the cur, lo and hi pointers, to allow this to write more
   characters than LAST(CARDINAL).  Should be called periodically. *)

END RdWrPipe.
