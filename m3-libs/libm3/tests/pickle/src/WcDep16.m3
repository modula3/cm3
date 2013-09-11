MODULE WcDep16 EXPORTS WcDep 

(* Things that need different source code, depending on the range of
   WIDECHAR. Version for when LAST(WIDECHAR) = 16_FFFF. *)  

; CONST CLit1_16 = W"0\x0001\xFFFE\xFFFF\xFFFC\xFFFF"   

; CONST CArray1 
    = ARRAY [ 0 .. 5 ] OF INTEGER 
        { ORD ( '0' ) 
        , 16_0001
        , 16_FFFE 
        , 16_FFFF 
        , 16_10FFFC 
        , 16_10FFFF
        }  

; PROCEDURE Init ( ) 

  = BEGIN 
      VLit1 := CLit1_16 
    ; VArray1 := NEW ( REF ARRAY OF INTEGER , NUMBER ( CArray1 ) )  
    ; VArray1 ^ := CArray1 
    END Init 

; BEGIN 
  END WcDep16 
. 
