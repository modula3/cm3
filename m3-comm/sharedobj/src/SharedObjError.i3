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
 * Created On      : Mon Sep 18 12:42:29 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Fri Nov 22 14:01:41 1996
 * Update Count    : 5
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.1.1.1  2001/12/02 13:14:14  wagner
 * Blair MacIntyre's sharedobj package
 *
 * Revision 1.2  1996/11/22 19:01:46  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

INTERFACE SharedObjError;

IMPORT SharedObj, Atom, AtomList;

(* Here are some procedures for raising "SharedObj.Error" exceptions
   conveniently: *)

PROCEDURE RaiseNetObjFailure (ec: AtomList.T) RAISES {SharedObj.Error};
(* Raise "SharedObj.Error(AtomList.Cons(SharedObj.NetObjFailure, ec))". *)

PROCEDURE RaiseCommFailure (ec: AtomList.T) RAISES {SharedObj.Error};
(* Raise "SharedObj.Error(AtomList.Cons(SharedObj.CommFailure, ec))". *)

PROCEDURE RaiseEventFailure (ec: AtomList.T) RAISES {SharedObj.Error};
(* Raise "SharedObj.Error(AtomList.Cons(SharedObj.EventFailure, ec))". *)

PROCEDURE RaiseDeadObject () RAISES {SharedObj.Error};
(* Raise "SharedObj.Error(AtomList.List1(SharedObj.DeadObject))". *)

PROCEDURE RaiseAlerted () RAISES {SharedObj.Error};
(* Raise "SharedObj.Error(AtomList.List1(SharedObj.Alerted))". *)

PROCEDURE RaiseRecursiveUpdate() RAISES {SharedObj.Error};
(* Raise "SharedObj.Error(AtomList.List1(SharedObj.RecursiveUpdate))". *)

PROCEDURE RaiseIPFailure (ec: AtomList.T) RAISES {SharedObj.Error};
(* Raise "SharedObj.Error(AtomList.Cons(SharedObj.IPFailure, ec))". *)

PROCEDURE RaiseNetObjAlerted () RAISES {SharedObj.Error};
(* Raise "SharedObj.Error(AtomList.List1(SharedObj.NetObjAlerted))". *)

PROCEDURE RaiseError(a: Atom.T) RAISES {SharedObj.Error};
(* Raise "SharedObj.Error(AtomList.List1(a))". *)

END SharedObjError.
