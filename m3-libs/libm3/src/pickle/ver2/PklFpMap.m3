UNSAFE MODULE PklFpMap 

(* A table of foreign fingerprints of selected types that pickle code needs to 
   recognize, in order to read pickles written by programs compiled and linked
   with different Modula-3 implementations. 
*) 

; IMPORT Fingerprint 
; IMPORT FpIntTbl 
; IMPORT RTType 
; IMPORT RTTypeFP 
; IMPORT TextLiteral 
; IMPORT Text16Short
; IMPORT Text16
; IMPORT Text8Short
; IMPORT Text8
; IMPORT Text8CString
; IMPORT TextCat
; IMPORT TextSub

; VAR Map : FpIntTbl . Default 

; TYPE FPA = ARRAY [ 0 .. 7 ] OF BITS 8 FOR [ 0 .. 255 ] 
; TYPE FPT = Fingerprint . T 

(* Some constants here are unused, but are present because it was a lot of work
   to find their values, and they could become needed in the future.
*) 

; CONST 
  (* Fingerprints that differ between pm3 and cm3. *) 
    NULL_uid = 16_48ec756e
  ; pm3_NULL_FP = FPT{FPA{16_24,16_80,16_00,16_00,16_6c,16_6c,16_75,16_6e}} 
  ; cm3_NULL_FP = FPT{FPA{16_6e,16_75,16_6c,16_6c,16_00,16_00,16_80,16_24}}

  ; ROOT_uid = 16_9d8fb489
  ; pm3_ROOT_FP = FPT{FPA{16_f8,16_09,16_19,16_c8,16_65,16_86,16_ad,16_41}}
  ; cm3_ROOT_FP = FPT{FPA{16_41,16_ad,16_86,16_65,16_c8,16_19,16_09,16_f8}}

  ; UNTRACED_ROOT_uid = 16_898ea789
  ; pm3_UNTRACED_ROOT_FP = FPT{FPA{16_f8,16_09,16_19,16_c8,16_71,16_87,16_be,16_41}}
  ; cm3_UNTRACED_ROOT_FP = FPT{FPA{16_41,16_be,16_87,16_71,16_c8,16_19,16_09,16_f8}}

  (* Can the following two occur in a pickle?  Maybe if somebody registered a
     special for them? *) 
  ; ADDRESS_uid = 16_08402063
  ; pm3_ADDRESS_FP = FPT{FPA{16_91,16_21,16_8a,16_62,16_f2,16_01,16_ca,16_6a}}
  ; cm3_ADDRESS_FP = FPT{FPA{16_f2,16_01,16_ca,16_6a,16_91,16_21,16_8a,16_62}}

  ; REFANY_uid = 16_1c1c45e6
  ; pm3_REFANY_FP = FPT{FPA{16_65,16_72,16_24,16_80,16_79,16_6e,16_61,16_66}}
  ; cm3_REFANY_FP = FPT{FPA{16_66,16_61,16_6e,16_79,16_80,16_24,16_72,16_65}}

  (* Release fingerprints. *) 
  ; rel_TextLiteral_T_FP  = FPT{FPA{16_81,16_ef,16_ba,16_63,16_f6,16_28,16_44,16_12}}
  ; rel_Text16Short_T_FP  = FPT{FPA{16_94,16_7b,16_13,16_f5,16_86,16_85,16_66,16_18}}
  ; rel_Text16_T_FP       = FPT{FPA{16_44,16_c5,16_c9,16_96,16_b5,16_ae,16_07,16_1f}}
  ; rel_Text8Short_T_FP   = FPT{FPA{16_58,16_3a,16_2c,16_e6,16_b8,16_85,16_57,16_03}}
  ; rel_Text8_T_FP        = FPT{FPA{16_f5,16_e5,16_72,16_53,16_ca,16_0d,16_a6,16_1a}}
  ; rel_Text8CString_T_FP = FPT{FPA{16_9c,16_d4,16_11,16_a4,16_b2,16_c4,16_3a,16_19}}
  ; rel_TextCat_T_FP      = FPT{FPA{16_fd,16_0d,16_c0,16_56,16_27,16_8f,16_fd,16_1f}}
  ; rel_TextSub_TT_FP     = FPT{FPA{16_58,16_22,16_76,16_f6,16_97,16_78,16_57,16_14}}

  (* Fingerprints before Unicode. *) 
  ; bfu_TextLiteral_T_FP  = FPT{FPA{16_81,16_ef,16_ba,16_63,16_f6,16_28,16_44,16_12}}
  ; bfu_Text16Short_T_FP  = FPT{FPA{16_bf,16_2b,16_31,16_99,16_99,16_4f,16_3d,16_10}}
  ; bfu_Text16_T_FP       = FPT{FPA{16_27,16_0f,16_1b,16_a7,16_c8,16_ed,16_03,16_01}}
  ; bfu_Text8Short_T_FP   = FPT{FPA{16_18,16_f4,16_5c,16_e2,16_b4,16_67,16_fe,16_1e}}
  ; bfu_Text8_T_FP        = FPT{FPA{16_f4,16_ef,16_0e,16_99,16_2f,16_0a,16_7e,16_04}}
  ; bfu_Text8CString_T_FP = FPT{FPA{16_fd,16_ef,16_0d,16_d8,16_f6,16_c9,16_70,16_13}}
  ; bfu_TextCat_T_FP      = FPT{FPA{16_46,16_cd,16_0f,16_ea,16_dd,16_ca,16_a9,16_15}}
  ; bfu_TextSub_TT_FP     = FPT{FPA{16_ed,16_c7,16_84,16_3d,16_37,16_e9,16_44,16_07}}

  (* Fingerprints in Unicode branch. *) 
  ; uni_TextLiteral_T_FP  = FPT{FPA{16_e3,16_d3,16_a1,16_62,16_29,16_28,16_1a,16_19}}
  ; uni_Text16Short_T_FP  = FPT{FPA{16_bf,16_2b,16_31,16_99,16_99,16_4f,16_3d,16_10}}
  ; uni_Text16_T_FP       = FPT{FPA{16_27,16_0f,16_1b,16_a7,16_c8,16_ed,16_03,16_01}}
  ; uni_Text8Short_T_FP   = FPT{FPA{16_18,16_f4,16_5c,16_e2,16_b4,16_67,16_fe,16_1e}}
  ; uni_Text8_T_FP        = FPT{FPA{16_f4,16_ef,16_0e,16_99,16_2f,16_0a,16_7e,16_04}}
  ; uni_Text8CString_T_FP = FPT{FPA{16_fd,16_ef,16_0d,16_d8,16_f6,16_c9,16_70,16_13}}
  ; uni_TextCat_T_FP      = FPT{FPA{16_46,16_cd,16_0f,16_ea,16_dd,16_ca,16_a9,16_15}}
  ; uni_TextSub_TT_FP     = FPT{FPA{16_ed,16_c7,16_84,16_3d,16_37,16_e9,16_44,16_07}}

  (* Other fingerprints. *) 
  ; big_TextLiteral_T_FP  = FPT{FPA{16_d9,16_56,16_04,16_eb,16_a7,16_ec,16_d8,16_02}}
      (* TextLiteral.T once had a pseudo-buffer size that adapted to the maximum
         for the word size.  This gave the above fingerprint on 64-bit machines
         and bfu_TextLiteral_T_FP on 32. *) 

(* Sets of fingerprints for the same type. *) 
; CONST NULL_FPs = ARRAY OF Fingerprint . T 
    { pm3_NULL_FP 
    , cm3_NULL_FP 
    } 

; CONST ROOT_FPs = ARRAY OF Fingerprint . T 
    { pm3_ROOT_FP 
    , cm3_ROOT_FP 
    } 

; CONST UNTRACED_ROOT_FPs = ARRAY OF Fingerprint . T 
    { pm3_UNTRACED_ROOT_FP 
    , cm3_UNTRACED_ROOT_FP 
    } 

; CONST ADDRESS_FPs = ARRAY OF Fingerprint . T 
    { pm3_ADDRESS_FP 
    , cm3_ADDRESS_FP 
    } 

; CONST REFANY_FPs = ARRAY OF Fingerprint . T 
    { pm3_REFANY_FP 
    , cm3_REFANY_FP 
    } 

; CONST TextLit_FPs = ARRAY OF Fingerprint . T 
    { big_TextLiteral_T_FP 
    , rel_TextLiteral_T_FP 
    , bfu_TextLiteral_T_FP 
    , uni_TextLiteral_T_FP 
    } 

; CONST Text16Short_FPs = ARRAY OF Fingerprint . T 
    { rel_Text16Short_T_FP 
    , bfu_Text16Short_T_FP 
    , uni_Text16Short_T_FP 
    } 

; CONST Text16_FPs = ARRAY OF Fingerprint . T 
    { rel_Text16_T_FP 
    , bfu_Text16_T_FP 
    , uni_Text16_T_FP 
    } 

; CONST Text8Short_FPs = ARRAY OF Fingerprint . T 
    { rel_Text8Short_T_FP 
    , bfu_Text8Short_T_FP 
    , uni_Text8Short_T_FP 
    } 

; CONST Text8_FPs = ARRAY OF Fingerprint . T 
    { rel_Text8_T_FP 
    , bfu_Text8_T_FP 
    , uni_Text8_T_FP 
    } 

; CONST Text8CString_FPs = ARRAY OF Fingerprint . T 
    { rel_Text8CString_T_FP 
    , bfu_Text8CString_T_FP 
    , uni_Text8CString_T_FP 
    } 

; CONST TextCat_FPs = ARRAY OF Fingerprint . T 
    { rel_TextCat_T_FP 
    , bfu_TextCat_T_FP 
    , uni_TextCat_T_FP 
    } 

; CONST TextSub_FPs = ARRAY OF Fingerprint . T 
    { rel_TextSub_TT_FP 
    , bfu_TextSub_TT_FP 
    , uni_TextSub_TT_FP 
    } 

; PROCEDURE InitType 
    ( Tc : RTType . Typecode ; READONLY FPs : ARRAY OF Fingerprint . T ) 

  = VAR LTargetFP : Fingerprint . T 

  ; BEGIN 
      LTargetFP := RTTypeFP . ToFingerprint ( Tc ) 
    ; FOR RI := 0 TO LAST ( FPs ) 
      DO 
        WITH WFromFP = FPs [ RI ] 
        DO 
          IF NOT Fingerprint . Equal ( WFromFP , LTargetFP ) 
          THEN 
            EVAL Map . put ( WFromFP , Tc ) 
          END (* IF *) 
        END (* WITH *) 
      END (* FOR *) 
    END InitType 

; PROCEDURE Init ( ) 

  = BEGIN 
      InitType ( TYPECODE ( NULL ) , NULL_FPs )  
    ; InitType ( TYPECODE ( ROOT ) , ROOT_FPs )  
    ; InitType ( TYPECODE ( UNTRACED ROOT ) , UNTRACED_ROOT_FPs )  
(* 
    ; InitType ( TYPECODE ( ADDRESS ) , ADDRESS_FPs )  
    ; InitType ( TYPECODE ( REFANY ) , REFANY_FPs )  
*) 

    ; InitType ( TYPECODE ( TextLiteral . T ) , TextLit_FPs )  
    ; InitType ( TYPECODE ( Text16Short . T ) , Text16Short_FPs )  
    ; InitType ( TYPECODE ( Text16 . T ) , Text16_FPs )  
    ; InitType ( TYPECODE ( Text8Short . T ) , Text8Short_FPs )  
    ; InitType ( TYPECODE ( Text8 . T ) , Text8_FPs )  
    ; InitType ( TYPECODE ( Text8CString . T ) , Text8CString_FPs )  
    ; InitType ( TYPECODE ( TextCat . T ) , TextCat_FPs )  
    ; InitType ( TYPECODE ( TextSub . TT ) , TextSub_FPs )  
    END Init 

(* EXPORTED: *) 
; PROCEDURE FromFingerprint ( READONLY Fp : Fingerprint . T ) : RTType . Typecode 

  = VAR LTc : INTEGER 
  ; VAR LFound : BOOLEAN 

  ; BEGIN 
      LFound := Map . get ( Fp , LTc )
    ; IF LFound 
      THEN RETURN LTc 
      ELSE RETURN RTType . NoSuchType 
      END (* IF *)  
    END FromFingerprint 

; BEGIN 
    Map := NEW ( FpIntTbl . Default ) . init ( ) 
  ; Init ( ) 
  END PklFpMap 
.

