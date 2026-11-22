
(* File: LLVMTypes.i3                                                         *) 
(* Types in llvm from various places, along with some minimal support.        *) 
(* Derived from various files of:                                             *| 
|*                                                                            *|
|*                     The LLVM Compiler Infrastructure                       *|
|*                                                                            *|
|* This file is distributed under the University of Illinois Open Source      *|
|* License. See LICENSE.TXT for details.                                      *|
|*                                                                            *|
\*===----------------------------------------------------------------------===*)


INTERFACE LLVMTypes 

; IMPORT Ctypes 
(*; IMPORT Word *)

(* These could come from almost anywhere. *) 
(* TODO: Figure out where/how to get the right types for these: *) 
; TYPE size_t = INTEGER 
; TYPE int64_t = Ctypes.long_long (*INTEGER*) 
; TYPE int32_t = Ctypes.int 
; TYPE uint64_t = Ctypes.unsigned_long_long (*Word . T*)  
; TYPE uint32_t = Ctypes.unsigned_int  
; TYPE uint8_t = Ctypes.unsigned_char  
; TYPE Bool = BOOLEAN 
(* NOTE: All the bindings to llvm assume a Modula3 BOOLEAN can be passed in
         and out to a C/C++ int and work as expected. *) 
; CONST False = FALSE 
; CONST True = TRUE 
; TYPE unsigned = [ 0 .. 16_7FFFFFFF ] 
      
; TYPE Opaque = RECORD END (* This duplicates a decl in LLVM. *) 

; TYPE MDNodeRef = UNTRACED BRANDED "M3DIBOpaqueMDNode" REF Opaque

(* From llvm/IR/Function.h: *) 

(* class Function* *) 
; TYPE FunctionRef = UNTRACED BRANDED "LLVMTypesOpaqueFunction" REF Opaque

(* From llvm/IR/BasicBlock.h: *) 
(* This is found in LLVM.i3. *) 

(* From llvm/IR/Instruction.h: *) 

(* class Instruction* *) 
; TYPE InstructionRef = UNTRACED BRANDED "LLVMTypesOpaqueInstruction" REF Opaque

(* From llvm/ADT/StringRef.h: *) 

(* class StringRef (NOTE: this is not a pointer). *) 
; TYPE StringRef 
    = RECORD 
        Data : Ctypes.char_star
      ; Length : size_t
      END 

; CONST StringRefEmpty = StringRef { Data := NIL , Length := 0 }

; TYPE MetadataRef = UNTRACED BRANDED "LLVMMetadata" REF Opaque

(* class ArrayRef<Metadata *> (NOTE: this is not a pointer). *) 
; TYPE ArrayRefOfMetadataRef 
    = RECORD 
        Data : UNTRACED REF MetadataRef (* 0th element, a la C/C++ *)
      ; Length : size_t
      END 

; CONST ArrayRefOfMetadataRefEmpty 
    = ArrayRefOfMetadataRef { Data := NIL , Length := 0 }

(* class ArrayRef<int64_t> (NOTE: this is not a pointer). *) 
; TYPE ArrayRefOfint64_t 
    = RECORD 
        Data : UNTRACED REF int64_t (* 0th element, a la C/C++ *)
      ; Length : size_t
      END
      
; CONST ArrayRefOfint64_tEmpty 
    = ArrayRefOfint64_t { Data := NIL , Length := 0 }

(* class ArrayRef<uint64_t> (NOTE: this is not a pointer). *) 
; TYPE ArrayRefOfuint64_t 
    = RECORD 
        Data : UNTRACED REF uint64_t (* 0th element, a la C/C++ *)
      ; Length : size_t
      END
      
(* From Metadata.h *)
; CONST DEBUG_METADATA_VERSION = 3

; END LLVMTypes 
. 

