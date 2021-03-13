(* ----------------------------------------------------------------------1- *)
(* File UnsafeUtils.m3 for Modula3 compiler test p269                       *)
(* Copyright 2019, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

UNSAFE MODULE UnsafeUtils

; IMPORT Fmt 
; IMPORT RT0 
; IMPORT RTModule
; IMPORT Stdio 
; IMPORT Text
; IMPORT Thread 
; IMPORT Wr 

; IMPORT Common 
; IMPORT SegList 
; IMPORT SegListSort

(* These should point somewhere within their namesake areas: *)
; VAR GStackAreaADR : ADDRESS

(* EXPORTED: *)
; PROCEDURE Init
    ( VAR LocVar : INTEGER
      (* This should be located somewhere near the bottom of the stack. *)
    ) 
  = BEGIN
      GStackAreaADR := ADR ( LocVar )
    END Init

; CONST WordHexLen = 2 * BYTESIZE ( ADDRESS )

(* EXPORTED: *)
; PROCEDURE AddressImage ( Addr : ADDRESS ) : TEXT
  = VAR IntVal : INTEGER
  ; VAR Result : TEXT

  ; BEGIN
      IntVal := LOOPHOLE ( Addr , INTEGER )
    ; Result := "16_"
        & Fmt . Pad
            ( Fmt . Int ( IntVal , base := 16 ) , WordHexLen , padChar := '0' ) 
    ; RETURN Result 
    END AddressImage 

(* EXPORTED: *)
; PROCEDURE IsInStackArea ( A : ADDRESS ) : BOOLEAN
  = VAR Lo , Hi , LocalADR : ADDRESS 

  ; BEGIN
      LocalADR := ADR ( LocalADR ) 
    (* Wouldn't it be nice if MIN and MAX worked on ADDRESS? *)
    ; IF GStackAreaADR < LocalADR
      THEN
        Lo := GStackAreaADR
      ; Hi := LocalADR
      ELSE
        Hi := GStackAreaADR
      ; Lo := LocalADR
      END (*IF*)
    ; IF Lo <= A AND A <= Hi
      THEN
        RETURN TRUE
      ELSE RETURN FALSE
      END (*IF*)
    (* Or, use RTStack.CurrentFrame and RTStack.PreviousFrame
       to walk the stack? *)
    END IsInStackArea

; VAR SegListRef : REF SegList . SegListTyp := NIL
; VAR SegCt : INTEGER := 0 

; TYPE CharPtrTyp = RT0 . String 

; PROCEDURE INCCharPtr ( VAR Ptr : CharPtrTyp )
  = BEGIN
      Ptr := LOOPHOLE ( LOOPHOLE ( Ptr , ADDRESS ) + 1 , CharPtrTyp ) 
    END INCCharPtr

; CONST NullChar = '\000' 

; PROCEDURE ScanString
    ( Str : CharPtrTyp ; VAR NextAddr : ADDRESS ; VAR T : TEXT )
  = VAR CharPtr : CharPtrTyp
  ; VAR Result : TEXT := "" 
  ; VAR C : CHAR 

  ; BEGIN
      CharPtr := Str 
    ; C := CharPtr ^
    ; WHILE C # NullChar 
      DO (* Thru' chars of Str. *)
        Result := Result & Text . FromChar ( C ) 
      ; INCCharPtr ( CharPtr ) 
      ; C := CharPtr ^
      END (*WHILE*) 
    ; NextAddr := LOOPHOLE ( CharPtr , ADDRESS )
    ; T := Result 
    END ScanString

; VAR DoDump : BOOLEAN := FALSE
; VAR DoDumpUninteresting : BOOLEAN := FALSE

; VAR WrT : Wr . T 

; PROCEDURE Dump ( Seg : SegList . SegDescrTyp )
  = <* FATAL Thread.Alerted , Wr.Failure *>
    BEGIN
      IF DoDump AND ( Seg . IsInteresting OR DoDumpUninteresting )  
      THEN
        IF Seg . IsInteresting 
        THEN
          Wr . PutText ( WrT , "* " )  
        ELSE
          Wr . PutText ( WrT , "  " )  
        END 
      ; Wr . PutText ( WrT , AddressImage ( Seg . ConstLo ) )  
      ; Wr . PutText ( WrT , " .. " )  
      ; Wr . PutText ( WrT , AddressImage ( Seg . ConstHi ) )  
      ; Wr . PutText ( WrT , " " )  
      ; Wr . PutText ( WrT , AddressImage ( Seg . VarLo ) )  
      ; Wr . PutText ( WrT , "  " )  
      ; Wr . PutText ( WrT , Seg .Name )  
      ; Wr . PutText ( WrT , Wr . EOL )  
      END 
    END Dump

; PROCEDURE FindConstAreas ( )
  (* Create a list of the global constant segments of the modules
     in this executing program, with names and approximate memory
     limits. *) 
  = VAR ModCt : CARDINAL
  ; VAR ModPtr : RT0 . ModulePtr
  ; VAR ModName , FileName : TEXT
  ; VAR ModNameFinal : ADDRESS
  ; VAR MaybePtr , StartAddr , EndAddr : ADDRESS
  ; VAR Lo , Hi : ADDRESS
  ; VAR LIsInteresting : BOOLEAN
  ; VAR Debug : INTEGER 
  
  ; BEGIN
      IF SegListRef # NIL THEN RETURN END (* It's already been done. *)
    ; ModCt := RTModule . Count ( )
    ; SegListRef := NEW ( REF SegList . SegListTyp , ModCt  )
    ; SegCt := 0 
    ; FOR RModNo := 0 TO ModCt - 1 (* Thru' the modules. *) 
      DO 
        ModPtr := RTModule . Get ( RModNo )
      (* Get module name. *)
      ; ScanString ( ModPtr ^ . proc_info ^ . name , ModNameFinal , ModName ) 

      (* Get end address of global constant segment. *)
      ; ScanString ( ModPtr ^ . file , EndAddr , FileName )

      ; LIsInteresting := Common . IsInteresting ( ModName )

      ; IF LIsInteresting
        THEN Debug := 27
        ELSE Debug := 0 
        END

      (* Get near-beginning address of global constant segment. *)
      ; MaybePtr := ADR ( ModPtr . gc_flags ) + ADRSIZE ( INTEGER )
      ; StartAddr := LOOPHOLE ( MaybePtr , UNTRACED REF ADDRESS ) ^ 
      (* Oh, is this ever tenuous.  According to experiment, if the
         module contains a use of a constant array constructor, its
         value will be the first thing in the global constant area,
         and it will be pointed-to by the first word in the global
         variable area following the RT0.ModuleInfo record.  This is
         true in the front-end's intermediate output, so should be true
         at runtime, target-independently.  Test code will have to be
         careful to put such a constructor in any module it cares about
         knowing if dope is located therein. *)

      ; IF StartAddr < EndAddr
        THEN Lo := StartAddr ; Hi := EndAddr 
        ELSE Hi := StartAddr ; Lo := EndAddr
        END (*IF*)

      ; WITH WSeg = SegListRef ^ [ SegCt ]
        DO 
          WSeg . Name := ModName
        ; WSeg . ConstLo := Lo 
        ; WSeg . ConstHi := Hi
        ; WSeg . VarLo := ModPtr 
        ; WSeg . IsInteresting := LIsInteresting 
        ; Dump ( WSeg )
        END 
      ; INC ( SegCt )

      END (*FOR*)
      
    (* Now sort the list. *)
    ; SegListSort . Sort ( SUBARRAY ( SegListRef ^ , 0 , SegCt ) )

    ; FOR RSegNo := 0 TO SegCt - 1
      DO Dump ( SegListRef ^ [ RSegNo ] )
      END (*FOR*) 
    END FindConstAreas
    
(*EXPORTED:*)
; PROCEDURE ConstAreaOf ( Addr : ADDRESS ) : TEXT

  = VAR LModNo : CARDINAL
  ; BEGIN
      IF SegListRef = NIL THEN FindConstAreas ( ) END
    ; LModNo := FIRST ( SegListRef ^ )
    ; LOOP
        IF LModNo >= SegCt THEN RETURN NIL END  
      ; WITH WSeg = SegListRef ^ [ LModNo ]
        DO IF WSeg . IsInteresting
              AND WSeg . ConstLo <= Addr  AND Addr <= WSeg . ConstHi 
          THEN RETURN WSeg . Name 
          ELSE INC ( LModNo )
          END (*IF*)
        END (*WITH*) 
      END (*LOOP*) 
    END ConstAreaOf

; CONST ByteStringMax = ( LAST ( INTEGER ) - 20 ) DIV BITSIZE ( CHAR )
; TYPE TYPE ByteStringPtrTyp
  = UNTRACED REF ARRAY [ 0 .. ByteStringMax ] OF CHAR

; PROCEDURE BitsEqual ( Left , Right : ByteStringPtrTyp ; Length : CARDINAL )
  : BOOLEAN

  = VAR I : INTEGER
  
  ; BEGIN
      I := 0 
    ; LOOP
        IF I >= Length THEN RETURN TRUE
        ELSIF Left [ I ] # Right [ I ] THEN RETURN FALSE
        END 
      END 
    END BitsEqual 

(* EXPORTED *) 
; PROCEDURE IsDoped
   ( VAR TestAddr : ADDRESS ; READONLY KnownAddr : ADDRESS ; Length : INTEGER )
  : Boolish  
  = TYPE AdrPtrTyp = UNTRACED REF ADDRESS
  ; VAR TestPtr , KnownPtr , EltsPtr : ByteStringPtrTyp
  ; VAR AdrPtr : AdrPtrTyp

  ; BEGIN
      TestPtr := LOOPHOLE ( TestAddr , ByteStringPtrTyp ) 
    ; KnownPtr := LOOPHOLE ( KnownAddr , ByteStringPtrTyp )
    ; IF BitsEqual ( TestPtr , KnownPtr , Length )
      THEN (* Negligible chance of false equality of an elements pointer
              and expected element values. *)
        RETURN Boolish . False 
      ELSE
        AdrPtr := LOOPHOLE ( TestAddr , AdrPtrTyp )
      ; EltsPtr := LOOPHOLE ( AdrPtr ^ , ByteStringPtrTyp)  
      ; IF BitsEqual ( TestPtr , KnownPtr , Length )
        THEN
          RETURN Boolish . True 
        ELSE RETURN Boolish . Unknown 
        END 
      END
    END IsDoped

; BEGIN
    (* Don't call Init here.  We are too deep in the stack to get
       a good near-bottom-of-stack address.  Calling Init from Main's
       BEGIN block will be nearer the bottom. *)
    WrT := Stdio . stderr 
  END UnsafeUtils
.

