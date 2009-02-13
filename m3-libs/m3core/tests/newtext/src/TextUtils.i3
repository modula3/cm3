INTERFACE TextUtils 

; EXCEPTION BadInvariant ( TEXT ) 

; PROCEDURE VerifyText ( T : TEXT ) RAISES { BadInvariant } 

; PROCEDURE MeasureText 
    ( T : TEXT 
    ; VAR MaxDepth : INTEGER  
    ; VAR LeafCt : INTEGER
    ; VAR Imbalance : REAL 
    ; VAR LengthDiff : INTEGER 
    ) 

; END TextUtils 
. 
