(* $Id$ *)

INTERFACE SXSelect;
IMPORT SX, SXRef;

(* the routines in this interface release ALL locks held by a thread
   and re-acquire them before returning; see comments in SX.i3 *)

PROCEDURE Wait(READONLY on : ARRAY OF SX.T;
               touched : REF ARRAY OF BOOLEAN := NIL);
(* normal Wait: returns as soon as any of the elements of on changes.
   
   If touched is non-NIL, it will be updated to show which elements of
   on have changed by setting those to TRUE.

   touched can be any size.  If it is smaller than on, only the first
   variables will be monitored in this array.  If it is larger, the last
   few entries will be set to FALSE.
 *)

PROCEDURE Wait1(on : SX.T); 
(* as above, for a single variable *)
  
PROCEDURE WaitE(READONLY on : ARRAY OF SX.T; 
                except : SXRef.T;
                touched : REF ARRAY OF BOOLEAN := NIL) RAISES { Exception };
(* Exception-Wait: behaves same as Wait, except that it also raises 
   an Exception when except becomes non-NIL.  The argument of the
   exception will be the new value of except *)
EXCEPTION Exception(REFANY);

END SXSelect.
