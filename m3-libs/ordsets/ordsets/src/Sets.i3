  
(* -----------------------------------------------------------------------1- *)
(* File Sets.i3  Modula-3 source code.                                       *)
(* Copyright 2010 .. 2012, Rodney M. Bates.                                  *)
(* rbates@acm.org                                                            *)
(* Licensed under the Gnu Public License, version 2 or later.                *)
(* -----------------------------------------------------------------------2- *)

(* Mechanically converted to Modula-3 and modified by 
   Rodney M. Bates, 2001, 2002, from Cocktail, reuse, Sets.md, 
   which was originally written in Modula-2 and part of the reuse 
   library of the Cocktail tool package: 

   Author: Josef Grosch, grosch@cocolab.de,

   at GMD Forschungsstelle at the University of Karlsruhe  Note: GMD 
   (National German Research Centre for Computer Science) does not exist
   in this form any more. GMD has been merged with "Fraunhofergesellschaft".   
*) 

(*
   Revision 1.5  2013-02-17 23:20:25  jkrell
   try to fix CVS keyword expansion, maybe just remove them entirely

*)

(* 

* Revision 1.1  2012-07-16 16:29:16  rodney
*
* Sets.i3 Sets.m3
*
* Sets is an old package that OrdSets replaces.  Used here to compare
* results between the packages, for testing OrdSets.
* 
 
 * RMB Aug 2001 Converted to Modula-3 
 
 * RMB July 98 Documentation/name changes. 
 
 * RMB 97/06/05 Added InitNullSet, since some tree dumps call 
                WriteSet for sets which are not in use and need 
                no allocated array. 
 
 * RMB 93/10/12 Regularized types to compile with WRL. 
 
 * Revision 1.4  1991/11/21  14:33:17  grosch 
 * new version of RCS on SPARC 
 * 
 * Revision 1.3  91/09/18  15:09:49  grosch 
 * reduced size of set type 
 * 
 * Revision 1.2  91/06/07  11:37:49  grosch 
 * increased bounds of flexible arrays 
 * 
 * Revision 1.1  89/01/09  17:13:03  grosch 
 * added functions Size, Minimum, and Maximum 
 * 
 * Revision 1.0  88/10/04  11:47:12  grosch 
 * Initial revision 
 * 
 *) 

(* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 *) 

INTERFACE Sets 

; IMPORT Rd 
; IMPORT Thread 
; IMPORT Word 
; IMPORT Wr 

; EXCEPTION Invalid 

; CONST Brand = "Sets"

; TYPE tElement = CARDINAL 
; TYPE ProcOftElement = PROCEDURE ( p0 : tElement ) RAISES ANY 
; TYPE ProcOftElementToBool = PROCEDURE ( p0 : tElement ) : BOOLEAN RAISES ANY

; TYPE T = tSet 
; TYPE tSet <: REFANY 

; PROCEDURE MakeSet ( VAR Set : tSet ; MaxElementCt : tElement := 256 ) 

; PROCEDURE ReleaseSet ( VAR Set : tSet ) 

; PROCEDURE Union ( VAR Set1 : tSet ; Set2 : tSet ) 

; PROCEDURE Difference ( VAR Set1 : tSet ; Set2 : tSet ) 

; PROCEDURE Project ( VAR Set : tSet ; Min : tElement ; Max : tElement ) 
  (* Remove elements outside the range Min .. Max *) 

; PROCEDURE Intersection ( VAR Set1 : tSet ; Set2 : tSet ) 

; PROCEDURE SymDiff ( VAR Set1 : tSet ; Set2 : tSet ) 

; PROCEDURE Complement ( VAR Set : tSet ) 

; PROCEDURE Include ( VAR Set : tSet ; Elmt : tElement ) 

; PROCEDURE Exclude ( VAR Set : tSet ; Elmt : tElement ) 

; PROCEDURE Card ( VAR Set : tSet ) : tElement 

; PROCEDURE Size ( VAR Set : tSet ) : tElement 

; PROCEDURE Minimum ( VAR Set : tSet ) : tElement 

; PROCEDURE Maximum ( VAR Set : tSet ) : tElement 

; PROCEDURE Select ( VAR Set : tSet ) : tElement 

; PROCEDURE Extract ( VAR Set : tSet ) : tElement 

; PROCEDURE IsSubset ( Set1 , Set2 : tSet ) : BOOLEAN 

; PROCEDURE IsProperSubset ( Set1 , Set2 : tSet ) : BOOLEAN 

; PROCEDURE IsEqual ( Set1 , Set2 : tSet ) : BOOLEAN 
; CONST Equal = IsEqual (* For instantiating things. *) 

; PROCEDURE IsNotEqual ( Set1 , Set2 : tSet ) : BOOLEAN 

; PROCEDURE Hash ( Set : tSet ) : Word . T 

; PROCEDURE IsElement ( Elmt : tElement ; VAR Set : tSet ) : BOOLEAN 

; PROCEDURE IsEmpty ( Set : tSet ) : BOOLEAN 

; PROCEDURE ForAll ( Set : tSet ; Proc : ProcOftElementToBool ) : BOOLEAN 
  RAISES ANY

; PROCEDURE Exists ( Set : tSet ; Proc : ProcOftElementToBool ) : BOOLEAN 
  RAISES ANY

; PROCEDURE Exists1 ( Set : tSet ; Proc : ProcOftElementToBool ) : BOOLEAN 
  RAISES ANY

; PROCEDURE Assign ( VAR Set1 : tSet ; Set2 : tSet ) 
  (* Set1 := Set2.  Set1 must be allocated and have the same size as Set2 *) 

; PROCEDURE Copy ( VAR Set1 : tSet ; Set2 : tSet ) 
  (* Set1 could be unallocated or the wrong max size, in which case,
     it will be (re)allocated with the same max size as Set2.
  *)

; PROCEDURE AssignSingleton 
    ( VAR Set : tSet ; Elmt : tElement ) (* Singleton *) 

; PROCEDURE AssignEmpty ( VAR Set : tSet ) 

; PROCEDURE ForAllDo ( Set : tSet ; Proc : ProcOftElement ) RAISES ANY

; PROCEDURE ReadSet ( Stream : Rd . T ; VAR Set : tSet ) 
  RAISES { Invalid , Rd . Failure , Thread . Alerted } 

; PROCEDURE WriteSet ( Stream : Wr . T ; Set : tSet ) 
  RAISES { Wr . Failure , Thread . Alerted } 

; END Sets 
. 



