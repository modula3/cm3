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
 * Created On      : Wed Jul 19 10:55:27 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Fri Sep  5 19:17:42 1997
 * Update Count    : 8
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.1.1.1  2001/12/01 14:42:18  wagner
 * Blair MacIntyre's listfuncs library
 *
 * Revision 1.3  1997/10/22 14:20:58  bm
 * added Count function
 *
 * Revision 1.2  1997/06/29 18:27:57  bm
 * Fixed header
 *
 * 
 * HISTORY
 *)

(* The generic interface "List" provides operations on linked lists of
   arbitrary element types.  This interface extends the operations you
   can perform on the generic lists. *)

GENERIC INTERFACE ListFuncs(Elem, List);
(* Where "Elem.T" is the parameter passed to the generic list "List".
   Thus, "Elem.T" contains 

| CONST Brand = <text-constant>;
| PROCEDURE Equal(k1, k2: Elem.T): BOOLEAN;

   as described in "List.ig".  "List" is the instead of "List.ig"
   instantiated with "Elem".   

 *)

CONST Brand = "(ListFuncs " & Elem.Brand & ")";

TYPE T = List.T;

(* As with "List.ig", none of the operations of this interface modify
   the "head" field of an existing list element.  Operations that may
   modify the "tail" field of existing list elements are called {\it
   destructive}.  By convention, their names end in "D".
   \index{naming conventions!destructive list operations} *)

PROCEDURE DeleteD(VAR list: T; READONLY elem: Elem.T): T;
(* Delete the first occurrance of "elem" in "list".  Return the
   single element list containing this cell.  "list" will be
   modified if required.  This happens if the first cell in "list" is
   "elem", for example. *)

PROCEDURE DeleteAllD(VAR list: T; READONLY elem: Elem.T): T; 

(* Delete the all occurrances of "elem" in "list".  Return the list
   containing these cells.  "list" will be modified if required.  This
   happens if the first cell in "list" is an "elem", for example. *)

PROCEDURE Count(l: T; READONLY e: Elem.T): INTEGER;
(* Return the number of occurances of "e" in "l".  The comparison is
   performed by "Elem.Equal". *)

PROCEDURE New(READONLY elem: Elem.T): T;

(* Allocate a new list element.  Might reuse a cached element which
   was released by "Free" below. *)

PROCEDURE Free(list: T): T;

(* Free the single list element pointed at by "list" and return the
   remainder of the list pointed at by "list.tail".  NOTE:  since
   freed elements may be retained, you should clear "list.head" if it
   contains a reference you don't want to be retained.  *)

END ListFuncs.
