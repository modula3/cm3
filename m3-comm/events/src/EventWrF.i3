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
 * Created On      : Tue May 23 17:57:29 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Nov 21 17:50:08 1996
 * Update Count    : 13
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.2  1996/11/21 22:50:17  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

INTERFACE EventWrF;

IMPORT EventWr;

REVEAL
  EventWr.T <: T;

TYPE
  T = EventWr.Public OBJECT
        cur_buf   : INTEGER    := 0;  (* index of the active buffer *)
        max_len   : INTEGER    := 0;  (* largest value of wr.cur ever seen *)
        n_buffers : INTEGER    := 0;  (* # of allocated buffers *)
        buffers   : BufferList := NIL;(* overflow array *)
      END;

TYPE
  Buffer     = REF ARRAY OF CHAR;
  BufferList = REF ARRAY OF Buffer;

CONST
  (* These are a multiple of 64-bits, since all the event data will be
     64-bit aligned. *)
  Slop = 24; (* enough so that a buffer doesn't overflow an allocator page *)
  BufferSize = 1024 - Slop;

END EventWrF.
