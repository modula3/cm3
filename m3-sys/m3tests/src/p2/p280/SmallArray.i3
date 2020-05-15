INTERFACE SmallArray
(* Including subranges. *)

; IMPORT Word 

(* Used in instantiation of Modes: *)
; CONST Label = "SmallArray"
; TYPE MainType = ARRAY [ 0 .. 2 ] OF CHAR 
; CONST BitCt = 25
; CONST BytesBitCt = ( ( BitCt + 1 ) DIV 8 ) * 8
; TYPE ShortType = BITS BitCt FOR MainType  
; TYPE BytesType = BITS BytesBitCt FOR MainType
; CONST PadBits = 3
; CONST PadLast = Word . RightShift ( - 1 , BITSIZE ( Word . T ) - PadBits + 1 )
; CONST PadFirst = - PadLast - 1 
; TYPE PadType = BITS PadBits FOR [ PadFirst .. PadLast ] 
; CONST DoPacked = TRUE 
; CONST IsArray = TRUE 

; VAR Val1 := MainType { '3' , 'H' , 'q' }
; CONST NondesigVal1 = MainType { '3' , 'H' , 'q' }
; VAR Val2 := MainType { '%' , '_' , 'x' }

; PROCEDURE Equal ( Left , Right : MainType ) : BOOLEAN 

; PROCEDURE Image ( Val : MainType ) : TEXT 

; END SmallArray
.
