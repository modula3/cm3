(* ----------------------------------------------------------------------1- *)
(* File Main.m3 for Modula3 compiler test p289                              *)
(* Copyright 2021, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

(* Test runtime cross-unit access to constant array constructors, alone,
   and in nonconstant record constructors, assigned and passed as
   parameters.
*)

MODULE Main

; IMPORT Fmt 
; IMPORT Stdio
; IMPORT Thread 
; IMPORT Wr
; IMPORT Remote

; CONST ConstValRename = Remote . ConstVal 
; CONST ConstValRenameRename = ConstValRename 

; TYPE Rec1 = RECORD
    VarField : Remote . Arr
  ; ConstField : Remote . Arr
  END

; TYPE Rec2 = RECORD
    VarField : Remote . Arr
  ; ConstField := Remote . Arr { 17 , 19 } 
  END

; PROCEDURE DisplayArr ( Val : Remote . Arr )
  = <* FATAL Thread . Alerted , Wr . Failure *>
    BEGIN
      Wr . PutText ( Stdio . stdout , "{" ) 
    ; Wr . PutText ( Stdio . stdout , Fmt . Int ( Val [ 0 ] ) )
    ; Wr . PutText ( Stdio . stdout , ", " ) 
    ; Wr . PutText ( Stdio . stdout , Fmt . Int ( Val  [ 1 ] ) )
    ; Wr . PutText ( Stdio . stdout , "}" ) 
    ; Wr . PutText ( Stdio . stdout , Wr . EOL ) 
    END DisplayArr 

; PROCEDURE DisplayRec ( Val : Rec1 )
  = <* FATAL Thread . Alerted , Wr . Failure *>
    BEGIN
      Wr . PutText ( Stdio . stdout , "{" ) 
    ; Wr . PutText ( Stdio . stdout , Fmt . Int ( Val . VarField [ 0 ] ) )
    ; Wr . PutText ( Stdio . stdout , ", " ) 
    ; Wr . PutText ( Stdio . stdout , Fmt . Int ( Val . VarField [ 1 ] ) )
    ; Wr . PutText ( Stdio . stdout , "}, {" ) 
    ; Wr . PutText ( Stdio . stdout , Fmt . Int ( Val . ConstField [ 0 ] ) )
    ; Wr . PutText ( Stdio . stdout , ", " ) 
    ; Wr . PutText ( Stdio . stdout , Fmt . Int ( Val . ConstField [ 1 ] ) )
    ; Wr . PutText ( Stdio . stdout , "}" ) 
    ; Wr . PutText ( Stdio . stdout , Wr . EOL ) 
    END DisplayRec 

; PROCEDURE ProcVal ( VALUE Formal : Rec1 )
  = VAR debug : INTEGER
  ; BEGIN
      DisplayRec (Formal) 
    ; debug := 20 
    END ProcVal 

; PROCEDURE ProcRO ( READONLY Formal : Rec1 )
  = VAR debug : INTEGER
  ; BEGIN
      DisplayRec (Formal) 
    ; debug := 22 
    END ProcRO 

; PROCEDURE Work ( )
  = VAR R1 , R2 , R3 , R4 : Rec1
  ; VAR AV , AC : Remote . Arr
  ; BEGIN
  
    (* Arrays: *)
      AV := Remote . VarVal
    ; DisplayArr ( AV ) 
    ; AC := Remote . ConstVal 
    ; DisplayArr ( AC )

    (* Record assignments, remote fields, explicit and default: *)
    ; R1 := Rec1 { Remote . VarVal , Remote . ConstVal }
    ; DisplayRec ( R1 ) 
    ; R2 := Rec2 { Remote . VarVal }
    ; DisplayRec ( R2 ) 

    (* Record assignments, fields renamed, once and twice: *)
    ; R3 := Rec1 { Remote . VarVal , ConstValRename }
    ; DisplayRec ( R3 ) 
    ; R4 := Rec1 { Remote . VarVal , ConstValRenameRename }
    ; DisplayRec ( R4 ) 

    (* Record parameters, constant field remote: *)
    ; ProcVal ( Rec1 { Remote . VarVal , Remote . ConstVal } ) 
    ; ProcRO ( Rec1 { Remote . VarVal , Remote . ConstVal } )

    (* Record parameters, constant field renamed, once and twice: *)
    ; ProcVal ( Rec1 { Remote . VarVal , ConstValRename } ) 
    ; ProcRO ( Rec1 { Remote . VarVal , ConstValRenameRename } )

    (* Record parameters, constant field remote default: *)
    ; ProcVal ( Rec2 { Remote . VarVal } ) 
    ; ProcRO ( Rec2 { Remote . VarVal } ) 
    END Work

; BEGIN
    Work ( )
  END Main
.

