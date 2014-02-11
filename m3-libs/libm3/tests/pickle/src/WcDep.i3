INTERFACE WcDep

(* Things that need different source code, depending on the range of
   WIDECHAR. *)  

(* These are initialized in module body, by Init, then constant. *) 

; VAR VLit1 : TEXT 
(* ^We need the contents of VLit1 to be a TextLiteral.T, for testing, but
   we also need some of its characters to be different when writing on
   16-bit vs. Unicode WIDECHAR systems.  So we resort to two versions of
   the exporting module to initialize it.
*) 
; VAR VArray1 : REF ARRAY OF INTEGER 
(* ^This must match the Unicode version of VLit1.  It is used as expected
   values when reading.
*) 

; PROCEDURE Init ( ) 

; END WcDep 
. 
