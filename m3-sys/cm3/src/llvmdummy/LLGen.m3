
(* Copyright (C) 2015, Rodney M. Bates                         *) 
(* All rights reserved.                                        *) 
(* See the file COPYRIGHT-RMB for a full description.              *) 
(*                                                             *) 
 
MODULE LLGen 

; IMPORT M3CG 
; IMPORT Target 
; IMPORT Wr 

; REVEAL CodeGenTyp 
    = M3CG . T BRANDED "LLGenDummyCodeGen" OBJECT 
        BackendMode : Target . M3BackendMode_t 
      ; wr : Wr . T := NIL 
      ; debug := FALSE 
      END 

; PROCEDURE New ( logfile : Wr . T ; BackendMode : Target . M3BackendMode_t ) 
  : CodeGenTyp 

  = VAR CodeGen : CodeGenTyp 
 
  ; BEGIN 
      CodeGen := NEW ( CodeGenTyp ) 
    ; CodeGen . BackendMode := BackendMode 
    ; CodeGen . wr := logfile 
    ; IF logfile # NIL 
      THEN 
        CodeGen . debug := TRUE 
      END 
    ; RETURN CodeGen 
    END New 

(* EXPORTED: *) 
; PROCEDURE CleanupCGAltogether ( <*UNUSED*> CodeGen : CodeGenTyp ) 

  = BEGIN
    END CleanupCGAltogether 

; BEGIN 
  END LLGen 
. 
