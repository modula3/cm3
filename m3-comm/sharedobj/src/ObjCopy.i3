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
 * Created On      : Tue Apr 25 13:57:19 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Fri Nov 22 14:00:00 1996
 * Update Count    : 21
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.2  1996/11/22 19:00:03  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

INTERFACE ObjCopy;

IMPORT SpaceConn;

CONST Brand = "Sequencer Object Copy";

TYPE
  T = OBJECT 
    conn: SpaceConn.T;  
    (* We aren't implementing the whole timeliness thing right now. *)
    (* timeliness: SharedObj.Timeliness := 0; *)
  END;
  
PROCEDURE Equal (k1, k2: T): BOOLEAN;

(* Create a text representation of the copy for debugging. *)
PROCEDURE ToText(c: T): TEXT;

END ObjCopy.
