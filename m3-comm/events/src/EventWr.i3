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
 * Created On      : Tue May 23 17:55:00 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Nov 21 17:48:44 1996
 * Update Count    : 6
 * 
 * $Source: /opt/cvs/cm3/m3-comm/events/src/EventWr.i3,v $
 * $Date: 2001-12-02 00:20:38 $
 * $Author: wagner $
 * $Revision: 1.2 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.1.1.1  2001/12/02 00:06:45  wagner
 * Blair MacIntyre's events library
 *
 * Revision 1.2  1996/11/21 22:48:48  bm
 * fixed header
 *
 * 
 * HISTORY
 *   Based on TextWr.i3 from the modula3 distribution.
 *
 * TODO
 * - allow bits to be added and removed from the front and back. 
 *   To do this, we must change the internals to allow non-full
 *   blocks.
 *)
(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Mon Sep 27 14:35:19 PDT 1993 by mcjones    *)
(*      modified on Thu May 20 15:21:47 PDT 1993 by swart      *)
(*      modified on Mon Mar 23 10:28:06 PST 1992 by kalsow     *)
(*      modified on Thu Nov  2 18:13:34 1989 by muller         *)


(* An "EventWr.T", or event writer, is a writer the contents of whose
   internal buffer can be retrieved as a "ARRAY OF CHAR".  Retrieving the
   buffer resets the target to be empty.  Text writers are buffered,
   seekable, and never raise "Failure" or "Alerted".  The fact that
   they are buffered is essentially unobservable, since there is no
   way for the client to access the target except through the
   writer. *)

INTERFACE EventWr;

IMPORT Wr;

TYPE
  T <: Public;
  Public = Wr.T OBJECT METHODS init(): T END;

(* The call "wr.init()" initializes "wr" to be a seekable writer with
   "c(wr)" set to the empty sequence and "cur(wr)" to 0.  The
   writer has no upper bound on its length.*)

PROCEDURE New(): T;
(* Equivalent to "NEW(T).init()". *)

PROCEDURE ToChars(wr: T): REF ARRAY OF CHAR;
(* Return "c(wr)", resetting "c(wr)" to the empty sequence and
   "cur(wr)" to 0. *)

END EventWr.
