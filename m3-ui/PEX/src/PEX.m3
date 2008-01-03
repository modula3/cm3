(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun 13 15:29:24 PDT 1995 by najork                   *)
(*       Created on Thu Feb 17 19:11:40 PST 1994 by najork                   *)


(******************************************************************************

This module contains the Modula-3 equivalent of #define C macro definitions
done in PEX.h, PEXlib.h and PEXocbuf.h.  Here are the copyright notices from 
the original C header files:


Copyright notice of PEX.h:
--------------------------

Copyright 1989, 1990, 1991 by Sun Microsystems, Inc. and the X Consortium.

                            All Rights Reserved
    
    Permission to use, copy, modify, and distribute this software and its 
    documentation for any purpose and without fee is hereby granted, 
    provided that the above copyright notice appear in all copies and that
    both that copyright notice and this permission notice appear in 
    supporting documentation, and that the names of Sun Microsystems,
    the X Consortium, and MIT not be used in advertising or publicity 
    pertaining to distribution of the software without specific, written 
    prior permission.  
    
    SUN MICROSYSTEMS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, 
    INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO 
    EVENT SHALL SUN MICROSYSTEMS BE LIABLE FOR ANY SPECIAL, INDIRECT OR 
    CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF 
    USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR 
    OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR 
    PERFORMANCE OF THIS SOFTWARE.



Copyright notice of PEXlib.h and PEXocbuf.h:
--------------------------------------------

  COPYRIGHT (c) 1988,1989,1990,1991     
  by DIGITAL Equipment Corporation, Maynard, Mass.                        
                                                                          
  This software is furnished under a license and may be used and  copied  
  only  in  accordance  with  the  terms  of  such  license and with the  
  inclusion of the above copyright notice.  This software or  any  other  
  copies  thereof may not be provided or otherwise made available to any  
  other person.  No title to and ownership of  the  software  is  hereby  
  transferred.                                                            
                                                                          
  The information in this software is subject to change  without  notice  
  and  should  not  be  construed  as  a commitment by DIGITAL Equipment  
  Corporation.                                                            
                                                                          
  DIGITAL assumes no responsibility for the use or  reliability  of  its  
  software on equipment which is not supplied by DIGITAL.                 

******************************************************************************)

UNSAFE MODULE PEX;

IMPORT Word;
IMPORT X;
IMPORT Ctypes;

(*****************************************************************************)
(* The following procedures mimic a set of (#define) C macros in PEX.h.      *)
(* C Macros are a tricky thing: They do not contain type information, and    *)
(* can thereby realize ad-hoc polymorphism which Modula-3 cannot reproduce,  *)
(* and worse, they have an Algol-like call-by-name semantics, which can be   *)
(* nasty. Example: given                                                     *)
(*    #define FOO(x) ((x)+(x))                                               *)
(* after executing                                                           *)
(*    i = 3; j = FOO(i++);                                                   *)
(* j is not 6, but 7, and i is not 4, but 5.                                 *)
(* I recommend to look at your code very carefully when you replace one of   *)
(* these C macros with a Modula-3 function!                                  *)
(*****************************************************************************)

(*
 * #define PEX_BITNUM_TO_BITMASK(bitIndex, maskNum, maskValue) \
 *     maskNum	= (bitIndex) / 32; \
 *     maskValue = ((unsigned)1L << ((bitIndex) % 32));
 *)
PROCEDURE PEX_BITNUM_TO_BITMASK(bitIndex : Ctypes.int;
                                VAR maskNum : Ctypes.int;
                                VAR maskValue : Ctypes.int
                               ) =
  BEGIN
    maskNum	:= bitIndex DIV 32;
    maskValue	:= Word.LeftShift(1,bitIndex MOD 32);
  END PEX_BITNUM_TO_BITMASK;

(* 
 * This is not a function, but just a stub of code (an if-statement with 
 * the condition, but where the then- and else part are missing).
 *
 * #define CHECK_BITMASK_ARRAY(mask,bitIndex) \
 *    if (mask[((bitIndex)/32)] & ((unsigned)1L << ((bitIndex) % 32)))
 *)

(*
 * #define PEX_BITMASK(i) ((unsigned)1 << ((i) & 31))
 *)
PROCEDURE PEX_BITMASK(i : Ctypes.int) : Ctypes.int =
  BEGIN
    RETURN Word.LeftShift(1,Word.And(i,31));
  END PEX_BITMASK;

(*
 * #define PEX_MASKIDX(i) ((i) >> 5)
 *)
PROCEDURE PEX_MASKIDX(i : Ctypes.int) : Ctypes.int =
  BEGIN
    RETURN Word.RightShift(i,5);
  END PEX_MASKIDX;

(*
 * #define PEX_MASKWORD(buf, i) buf[PEX_MASKIDX(i)]
 *)
PROCEDURE PEX_MASKWORD (READONLY buf : ARRAY OF Ctypes.int ; 
                        i : Ctypes.int) : Ctypes.int =
  BEGIN
    RETURN buf[PEX_MASKIDX(i)];
  END  PEX_MASKWORD;

(*
 * #define PEX_BITSET(buf, i) PEX_MASKWORD(buf, i) |= PEX_BITMASK(i)
 *)
PROCEDURE PEX_BITSET(VAR buf : ARRAY OF Ctypes.int ; i : Ctypes.int) =
  BEGIN 
    buf[PEX_MASKIDX(i)] := Word.Or (buf[PEX_MASKIDX(i)], PEX_BITMASK(i));
  END PEX_BITSET;

(*
 * #define PEX_BITCLEAR(buf, i) PEX_MASKWORD(buf, i) &= ~PEX_BITMASK(i)
 *)
PROCEDURE PEX_BITCLEAR(VAR buf : ARRAY OF Ctypes.int ; i : Ctypes.int) =
  BEGIN 
     buf[PEX_MASKIDX(i)] := 
             Word.And (buf[PEX_MASKIDX(i)], Word.Not (PEX_BITMASK(i)));
  END PEX_BITCLEAR;

(*
 * #define PEX_GETBIT(buf, i) (PEX_MASKWORD(buf, i) & PEX_BITMASK(i))
 *)
PROCEDURE PEX_GETBIT (READONLY buf : ARRAY OF Ctypes.int ; i : Ctypes.int) : 
  Ctypes.int =
  BEGIN 
    RETURN Word.And (PEX_MASKWORD(buf, i), PEX_BITMASK(i));
  END PEX_GETBIT;

(********** From PEXlib.h **********)

(*
 * #define PEX_SetPCAttrMaskBit(mask, attrNum) \
 *    mask[((attrNum)) >> 5] |= 1L << ( ((attrNum)) & 0x1F)
 *)
PROCEDURE PEX_SetPCAttrMaskBit(VAR mask : ARRAY [0 .. 1] OF Ctypes.int; 
                               attrNum : Ctypes.int) =
  BEGIN
    mask[Word.RightShift(attrNum,5)] := 
      Word.Or(mask[Word.RightShift(attrNum,5)],
              Word.LeftShift(1,Word.And(attrNum,16_1F)));
  END PEX_SetPCAttrMaskBit;

(********** From PEXocbuf.h **********)

(*
 * #define PEXAllocateOCBuffer(_dpy_,_typ_,_targ_,_bufTyp_,_errFn_,_initSiz_)\
 * ( (_bufTyp_ == PEXDefaultTransientOCBuffer) ?\
 *       (PEXAllocateTransientOCBuffer(_dpy_,_typ_,_targ_,_errFn_,_initSiz_)) :\
 *       (PEXAllocateRetainedOCBuffer(_dpy_,_typ_,_targ_,_errFn_,_initSiz_)) )
 *)
PROCEDURE PEXAllocateOCBuffer(display : X.DisplayStar;
                              type    : Ctypes.int;
                              target  : X.XID;
                              bufTyp  : PROCEDURE () : Ctypes.int;
                              errorFn : ErrorFunctionType;
                              initSize: Ctypes.int) 
          : pxlOCBufStar = 
  BEGIN
    IF bufTyp = PEXDefaultTransientOCBuffer THEN
      RETURN PEXAllocateTransientOCBuffer(display,type,target,errorFn,initSize)
    ELSE
      RETURN PEXAllocateRetainedOCBuffer(display,type,target,errorFn,initSize);
    END;
  END PEXAllocateOCBuffer;

(*
 * #define PEXFlushOCBuffer(_ocbuf_) ( *(_ocbuf_)->FlushOCBuf)(_ocbuf_)
 *)
PROCEDURE PEXFlushOCBuffer(ocbuf : pxlOCBufStar) =
  BEGIN
    ocbuf^.FlushOCBuf(ocbuf);
  END PEXFlushOCBuffer;

(*
 * #define PEXSendOCBuffer(_ocbuf_) ( *(_ocbuf_)->SendOCBuf)(_ocbuf_)
 *)
PROCEDURE PEXSendOCBuffer(ocbuf : pxlOCBufStar) =
  BEGIN
    ocbuf^.SendOCBuf(ocbuf);
  END PEXSendOCBuffer;

(********** END OF MACROS **********)

BEGIN
END PEX.
