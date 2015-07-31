 
(* Copyright (C) 2015, Rodney M. Bates                         *) 
(* All rights reserved.                                        *) 
(* See the file COPYRIGHT-RMB for a full description.              *) 
(*                                                             *) 
 
INTERFACE LLGen 
 
; IMPORT M3CG 
; IMPORT Target 
; IMPORT Wr 

; TYPE CodeGenTyp <: M3CG . T 

; PROCEDURE New 
    ( logfile : Wr . T ; BackendMode : Target . M3BackendMode_t ) 
  : CodeGenTyp  
  (* A new, initialized code generator that uses llvm. 
     If logfile # NIL and # "", it writes a log on 'logfile'. 
  *) 
 
; PROCEDURE CleanupCGAltogether ( CodeGen : CodeGenTyp ) 

; END LLGen 
. 
 
