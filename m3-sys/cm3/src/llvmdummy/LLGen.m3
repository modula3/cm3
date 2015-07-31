
(* Copyright (C) 2015, Rodney M. Bates                         *) 
(* All rights reserved.                                        *) 
(* See the file COPYRIGHT-RMB for a full description.              *) 
(*                                                             *) 
 
MODULE LLGen 

; IMPORT M3CG 
; IMPORT Target 
; IMPORT Wr 

; REVEAL CodeGenTyp = M3CG . T BRANDED "LLVMDummyCodeGenerator" OBJECT END  

; PROCEDURE New 
    ( <*UNUSED*> logfile : Wr . T ; <*UNUSED*> BackendMode : Target . M3BackendMode_t ) 
  : CodeGenTyp 

  = VAR CodeGen : CodeGenTyp 
 
  ; BEGIN 
      CodeGen := NEW ( CodeGenTyp ) 
    ; RETURN CodeGen 
    END New 

(* EXPORTED: *) 
; PROCEDURE CleanupCGAltogether ( <*UNUSED*> CodeGen : CodeGenTyp ) 

  = BEGIN
    END CleanupCGAltogether 

; BEGIN 
  END LLGen 
. 
