(* 
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 * 
 * Author          : Blair MacIntyre
 * Created On      : Mon Oct  7 10:37:43 1996
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Oct 14 15:13:16 1996
 * Update Count    : 3
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * 
 * HISTORY
 *)

INTERFACE NetObjF;

IMPORT NetObj, Pickle2 AS Pickle, Thread;

(* The netobj package uses specific subclasses of Pickle.Reader and
   Pickle.Writer to pickle object parameters.  If you need to handle
   netobj pickling differently than other kinds of pickling, you can
   use these routines to check if the pickle is being used as a
   network object parameter or return value. *)

PROCEDURE IsNetObjWriter(wr: Pickle.Writer): BOOLEAN;
PROCEDURE IsNetObjReader(wr: Pickle.Reader): BOOLEAN;

(* We want to be able to pass an identifier for a network object
   between machines outside of the network object system.  "ToWireRep"
   converts a "NetObj.T" into a wire representation and a text string
   describing the location of the object.  "FromWireRep" converts a
   wire representation and location back into a "NetObj.T".  If the
   corresponding network object (or a surrogate) does not already
   exist in the process, "FromWireRep" will raise "NetObj.Error" if it
   cannot be retreived from "loc". *)

TYPE WRep = RECORD byte: ARRAY [0..15] OF BITS 8 FOR [0..255]; END;

PROCEDURE ToWireRep(ref: NetObj.T; 
                    VAR (*OUT*) rep: WRep; VAR addr: NetObj.Address);
PROCEDURE FromWireRep(rep: WRep; addr: NetObj.Address): NetObj.T
  RAISES {NetObj.Error, Thread.Alerted};

END NetObjF.
