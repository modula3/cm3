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
 * Created On      : Wed Jun  7 16:54:33 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Nov 21 17:45:18 1996
 * Update Count    : 6
 * 
 * $Source: /opt/cvs/cm3/m3-comm/events/src/EventSpaceID.m3,v $
 * $Date: 2001-12-02 00:06:45 $
 * $Author: wagner $
 * $Revision: 1.1.1.1 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.2  1996/11/21 22:45:23  bm
 * fixed header
 *
 * 
 * HISTORY
 * - based on SpaceID from the netobj package.
 *)
(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* EventSpaceID.m3 *)
(* Last modified on Mon Jul 19 15:04:37 PDT 1993 by wobber *)
(*      modified on Fri Aug  7 15:00:56 PDT 1992 by evers  *)

UNSAFE MODULE EventSpaceID;

IMPORT Fingerprint, TimeStamp, Fmt;

VAR myT: T;

PROCEDURE Mine() : T = BEGIN RETURN myT; END Mine;

PROCEDURE ComputeFP() : T =
  VAR ts := TimeStamp.New();
  BEGIN
    RETURN Fingerprint.FromChars(
             LOOPHOLE(ts, ARRAY [0..15] OF CHAR), Fingerprint.OfEmpty);
  END ComputeFP;
   
PROCEDURE ToText(id: T): TEXT =
  VAR t: TEXT := Fmt.Int(id.byte[FIRST(id.byte)]);
  BEGIN
    FOR i := FIRST(id.byte)+1 TO LAST(id.byte) DO
      t := t & "." & Fmt.Int(id.byte[i]);
    END;
    RETURN t;
  END ToText;

BEGIN
  myT := ComputeFP();
END EventSpaceID.
