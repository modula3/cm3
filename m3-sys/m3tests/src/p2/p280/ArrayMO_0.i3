INTERFACE ArrayMO_0

; IMPORT Common 

(* Used in instantiation of ArrayModes: *)
; TYPE FixedType = Common . Array3DFixed 
; TYPE InnerType = Common . Array2DFixed   
; TYPE PartiallyOpenType = FixedType 
; TYPE MainType = PartiallyOpenType 
; CONST Label = " Fixed"
; TYPE EltType = INTEGER 

; CONST Val1 = Common . Array3DVal 
; CONST Val2 = Common . Array3DVal2

; PROCEDURE NewRef ( ) : REF MainType 

; END ArrayMO_0
.
