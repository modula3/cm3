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
 * Created On      : Thu Jul  3 09:16:10 1997
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Jul  3 09:21:10 1997
 * Update Count    : 2
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.1  1997/07/11 18:07:03  bm
 * Forgot to add these!
 *
 * 
 * HISTORY
 *)

(* needed for using the ArraySort generic interface *)
INTERFACE ObValueField;

IMPORT ObValue;

TYPE
  T = ObValue.Field;

PROCEDURE  Compare(a, b: T): [-1 .. 1];

END ObValueField.
