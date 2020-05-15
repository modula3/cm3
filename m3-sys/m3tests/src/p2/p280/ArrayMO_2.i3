INTERFACE ArrayMO_2

; IMPORT Common 

(* Used in instantiation of ArrayModes: *)
; TYPE FixedType = Common . Array3DFixed 
; TYPE InnerType = ARRAY OF ARRAY [ 0 .. 2 ] OF INTEGER 
; TYPE PartiallyOpenType = ARRAY OF InnerType 
; TYPE MainType = PartiallyOpenType 
; CONST Label = " Two open dimensions"
; TYPE EltType = INTEGER 

; CONST Val1 = Common . Array3DVal 
; CONST Val2 = Common . Array3DVal2 

; PROCEDURE NewRef ( ) : REF MainType 

; END ArrayMO_2
.
