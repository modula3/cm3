INTERFACE PklFpMap

(* A table of foreign fingerprints of selected types that pickle code needs to 
   recognize, in order to read pickles written by programs compiled and linked
   with different Modula-3 implementations. 
*) 

; IMPORT Fingerprint 
; IMPORT RTType 

; PROCEDURE FromFingerprint ( READONLY Fp : Fingerprint . T ) : RTType . Typecode 

; END PklFpMap 
. 

