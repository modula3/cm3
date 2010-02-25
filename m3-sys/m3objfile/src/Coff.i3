(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Wed Sep  7 12:03:23 PDT 1994 by kalsow   *)

(* This interface defines the structures found in COFF object files. *)

INTERFACE Coff;

TYPE
  UINT8     = BITS  8 FOR [0..255];
  UINT16    = BITS 16 FOR [0..16_FFFF];
  INT16     = BITS 16 FOR [-16_8000..16_7FFF];
  INT32     = BITS 32 FOR [-16_7FFFFFFF-1 .. 16_7FFFFFFF];

TYPE
  FileHeader = RECORD
    f_magic  : UINT16;  (* magic number *)
    f_nscns  : UINT16;  (* number of sections *)
    f_timdat : INT32;   (* time & date stamp *)
    f_symptr : INT32;   (* file pointer to symtab *)
    f_nsyms  : INT32;   (* number of symtab entries *)
    f_opthdr : UINT16;  (* sizeof(optional hdr) *)
    f_flags  : UINT16;  (* flags *)
  END;

CONST (* bit field values for f_flags field *)
  F_RELFLG =  8_0001;  (* => file contains no relocation *)
  F_EXEC   =  8_0002;  (* => file is executable *)
  F_LNNO   =  8_0004;  (* => line numbers have been stripped from file *)
  L_SYMS   =  8_0010;  (* => local symbols have been stripped from file *)

TYPE
  Name = ARRAY [0..7] OF CHAR;  (* an 8-character name *)
TYPE
  SectionHeader = RECORD
    s_name    : Name;   (* section name *)
    s_paddr   : INT32;  (* physical address *)
    s_vaddr   : INT32;  (* virtual address *)
    s_size    : INT32;  (* section size *)
    s_scnptr  : INT32;  (* file ptr to raw data for section *)
    s_relptr  : INT32;  (* file ptr to relocation info for section *)
    s_lnnoptr : INT32;  (* file ptr to line numbers *)
    s_nreloc  : UINT16; (* number of relocation entries *)
    s_nlnno   : UINT16; (* number of line number entries *)
    s_flags   : INT32;  (* type and content flags *)
  END;

TYPE
  Relocation = RECORD
    r_vaddr  : INT32;   (* address of reference *)
    r_symndx : INT32;   (* index into symbol table *)
    r_type   : UINT16;  (* relocation type *)
  END;

TYPE
  Symbol = RECORD
    n_name   : Name;    (* n_name[0..3]=0 => SymbolNamePtr *)
    n_value  : INT32;   (* value of symbol *)
    n_scnum  : INT16;   (* section number *)
    n_type   : UINT16;  (* type and derived type *)
    n_sclass : UINT8;   (* storage class *)
    n_numaux : UINT8;   (* number of aux. entries *)
  END;
 
  SymbolNamePtr = RECORD    (* the "other" interpretation of n_name *)
    n_zeros  : INT32;       (* must be zero. *)
    n_offset : INT32;       (* offset into the string table *)
  END;

END Coff.
