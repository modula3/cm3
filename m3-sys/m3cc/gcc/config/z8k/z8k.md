;;-  Machine description for GNU compiler, Z8000 Version
;;   Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.

;; This file is part of GNU CC.

;; GNU CC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU CC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU CC; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;; First incarnation by Steve Chamberlain, sac@cygnus.com

;;- Instruction patterns.  When multiple patterns apply,
;;- the first one in the file is chosen.
;;-
;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.
;;-
;; We don't want to allow a constant operand for test insns because
;; (set (cc0) (const_int foo)) has no mode information.  Such insns will
;; be folded while optimizing anyway.

;; Q = ir da
;; R = x
;; S = ba bx

(define_attr "type" "branch,foo,def,djnz,invisible,return" 
  (const_string "def"))

(define_attr "cond" 
  "notrashcc,trashcc,setcc,setrevcc,logcc"
  (const_string "trashcc"))

; the longest possible instruction is the default when we are not
; sure

(define_attr "length" ""
  (cond [(eq_attr "type" "branch")
	 (if_then_else (and (ge (minus (pc) (match_dup 0))
				(const_int -256))
			    (le (minus (pc) (match_dup 0))
				(const_int 254)))
		       (const_int 2)
		       (const_int 4))

	 (eq_attr "type" "djnz")
	 (if_then_else (and (ge (minus (pc) (match_dup 1))
				(const_int -2))
			    (le (minus (pc) (match_dup 1))
				(const_int 252)))
		       (const_int 2)
		       (const_int 4))

	 (eq_attr "type" "foo") (const_int 10)]
	(const_int 6)))


;; ----------------------------------------------------------------------
;; Tests
;; ----------------------------------------------------------------------

(define_insn "testhi_"
  [(set (cc0)
	(match_operand:HI 0 "r_ir_da_x_operand" "r,QR"))]
  ""
  "@
	and	%H0,%H0	
	test	%H0"
  [(set_attr "cond" "logcc,logcc")])

(define_expand "tsthi"
  [(set (cc0)  (match_operand:HI 0 "r_ir_da_operand" ""))]
  ""
  "operands[0] = force_reg (HImode, operands[0]);")
	
(define_insn "tstqi_"
  [(set (cc0)
	(match_operand:QI 0 "r_ir_da_x_operand" "u,QR"))]
  ""
  "@
	andb	%Q0,%Q0	
	testb 	%Q0"
  [(set_attr "cond" "logcc,logcc")])

;(define_expand "tstqi"
;  [(set (cc0) (match_operand:QI 0 "r_ir_da_operand" "rQ"))]
;  ""
; "operands[0] = force_reg (QImode, operands[0]);")

(define_insn "tstsi_"
  [(set (cc0)
	(match_operand:SI 0 "r_ir_da_x_operand" "rQR"))]
  ""
  "testl 	%S0"
  [(set_attr "cond" "logcc")])

(define_expand "tstsi"
  [(set (cc0) (match_operand:SI 0 "r_ir_da_operand" "rQR"))]
  ""
  "operands[0] = force_reg (SImode, operands[0]);")


(define_insn "tstpsi_"
  [(set (cc0)
	(match_operand:PSI 0 "r_ir_da_x_operand" "rQR"))]
  ""
  "testl 	%S0"
  [(set_attr "cond" "logcc")])

(define_expand "tstpsi"
  [(set (cc0) (match_operand:PSI 0 "r_ir_da_operand" "rQR"))]
  ""
  "operands[0] = force_reg (PSImode, operands[0]);")


;; ----------------------------------------------------------------------
;; Compares
;; ----------------------------------------------------------------------

(define_insn "cmphi_imm"
  [(set (cc0)
	(compare (match_operand:HI 0 "r_ir_da_x_operand" "rQR")
		 (match_operand:HI 1 "immediate_operand" "i")))]
  ""
  "cp	%H0,%H1"
  [(set_attr "cond" "setcc")])


(define_insn "cmphi_other"
  [(set (cc0)
	(compare (match_operand:HI 0 "r_ir_da_x_operand" "r,rQR,QR")
		 (match_operand:HI 1 "r_im_ir_da_x_operand" "riQR,i,r")))]
  "r_operand(operands[0], HImode)
  || (r_im_operand(operands[1], HImode))"
  "@
	cp	%H0,%H1
	cp	%H0,%H1
	cp	%H1,%H0"
  [(set_attr "cond" "setcc,setcc,setrevcc")])

(define_expand "cmphi"
  [
   (set (cc0)
	(compare (match_operand:HI 0 "r_ir_da_x_operand" "")
		 (match_operand:HI 1 "r_im_ir_da_x_operand" "")))]
  ""
  "{
    /* Can only cope with r,[r|im|ir|da|x] or [ir|da|x],im */
	
    if (immediate_operand (operands[1], HImode))
      {
	if (!r_ir_da_x_operand (operands[0], HImode))
	  {
	    operands[0] = force_reg (HImode, operands[0]);
	  }
      }
    else if (!r_operand (operands[0]))
      {
	operands[0] = force_reg (HImode, operands[0]);
      }
  }")


(define_insn "_cmpqi"
  [(set (cc0)
	(compare (match_operand:QI 0 "r_ir_da_x_operand" "u,QR")
		 (match_operand:QI 1 "r_im_ir_da_x_operand" "uiQR,i")))]
  "r_operand (operands[0], QImode)
   || im_operand(operands[1], QImode)"
  "cpb	%Q0,%Q1"
  [(set_attr "cond" "setcc")])

(define_expand "cmpqi"
  [(set (cc0)
	(compare (match_operand:QI 0 "r_ir_da_x_operand" "u,QR")
		 (match_operand:QI 1 "r_im_ir_da_x_operand" "uiQR,i")))]
  ""
  "
    /* Can only cope with r,[r|im|ir|da|x] or [ir|da|x],im */
	
    if (immediate_operand (operands[1], QImode))
      {
	if (!r_ir_da_x_operand (operands[0], QImode))
	  {
	    operands[0] = force_reg (QImode, operands[0]);
	  }
      }
    else if (!r_operand (operands[0]))
      {
	operands[0] = force_reg (QImode, operands[0]);
   }")




(define_insn ""
  [(set (cc0)
	(compare (match_operand:SI 0 "r_im_ir_da_x_operand" "r,iQR")
		 (match_operand:SI 1 "r_im_ir_da_x_operand" "rQRi,r")))]
  "r_operand(operands[0], SImode) || r_operand(operands[1], SImode)"
  "@
	cpl	%S0,%S1
	cpl	%S1,%S0"
  [(set_attr "cond" "setcc,setrevcc")])

; this instruction doesnt exist for SImode
;(define_insn ""
;  [(set (cc0)
;	(compare (match_operand:SI 0 "r_da_x_operand" "rQR")
;		 (match_operand:SI 1 "immediate_operand" "i")))]
;  ""
;  "cpl	%S0,%S1"
;  [(set_attr "cond" "setcc")])

(define_expand "cmpsi"
  [ (set (cc0)
	(compare (match_operand:SI 0 "r_im_ir_da_x_operand" "")
		 (match_operand:SI 1 "r_im_ir_da_x_operand" "")))]
  ""
 "{
   if (!r_operand(operands[0], SImode) 
       && !r_operand(operands[1], SImode))
     {
       operands[0]  = force_reg (SImode, operands[0]);
     }
 }")


(define_insn ""
  [(set (cc0)
	(compare (match_operand:PSI 0 "r_im_ir_da_x_operand" "r,iQR")
		 (match_operand:PSI 1 "r_im_ir_da_x_operand" "rQRi,r")))]
  "r_operand(operands[0], PSImode) || r_operand(operands[1], PSImode)"
  "@
	cpl	%S0,%S1
	cpl	%S1,%S0"
  [(set_attr "cond" "setcc,setrevcc")])


(define_expand "cmppsi"
  [ (set (cc0)
	(compare (match_operand:PSI 0 "r_im_ir_da_x_operand" "")
		 (match_operand:PSI 1 "r_im_ir_da_x_operand" "")))]
  ""
 "{
   if (!r_operand(operands[0], PSImode) || r_operand(operands[1], PSImode)) 
     {
       operands[0] = force_reg (PSImode, operands[0]);
     }
 }")

(define_insn ""
  [(set (cc0)
	(zero_extract:HI (match_operand:HI 0 "r_ir_da_x_operand" "r,rQR")
		         (const_int 1)
			 ;; ??? insert right pred later
			 (match_operand:HI 1 "nonmemory_operand" "r,L")))]
  "GET_CODE (operands[1]) == CONST_INT || r_operand (operands[0], HImode)"
  "bit	%H0,%H1"
  [(set_attr "cond" "logcc")])



;; ----------------------------------------------------------------------
;; MOVE INSTRUCTIONS
;; ----------------------------------------------------------------------


;; HI MODE


(define_insn "movhi_matcher"
  [(set (match_operand:HI 0 "r_ir_da_x_ba_bx_prd_operand" "=r,r,r,r,rQR,QR,QR,QRS,<")
	(match_operand:HI 1 "r_im_ir_da_x_ba_bx_poi_operand" "I,L,ri,QRS,>,I,i,r,riQR"))]
  "moveok(operands, HImode)"
  "@
	xor	%H0,%H0
	ldk	%H0,%H1
	ld	%H0,%H1
	$ld	%H0,%H1
	pop	%H0,@%1
	clr	%H0
	ld	%H0,%H1
	$ld	%H0,%H1
	push	@%0,%H1"
  [(set_attr "cond" "trashcc,notrashcc,notrashcc,notrashcc,notrashcc,notrashcc,notrashcc,notrashcc,notrashcc")])

(define_expand "movhi"
  [(set (match_operand:HI 0 "r_ir_da_x_ba_bx_prd_operand" "")
	(match_operand:HI 1 "r_im_ir_da_x_ba_bx_poi_operand" ""))]
  ""
  "
  if (!reload_in_progress && !reload_completed
	 && !moveok(operands, HImode))
    operands[1] = copy_to_mode_reg (HImode, operands[1]);
")


;; SI mode


(define_insn "movsi_matcher"
  [(set (match_operand:SI 0 "r_ir_da_x_ba_bx_prd_operand" "=r,r,r,r,rQR,QRS,<")
	(match_operand:SI 1 "r_im_ir_da_x_ba_bx_poi_operand" "I,L,ri,QRS,>,r,rQR"))]
  "moveok(operands, SImode)"
  "@
	xor	%I0,%I0\;ld	%H0,%I0
	xor	%H0,%H0\;ldk	%I0,%H1
	ldl	%S0,%S1
	$ldl	%S0,%S1
	popl	%S0,@%1
	$ldl	%S0,%S1
	pushl	@%0,%S1"
  [(set_attr "cond" "trashcc,trashcc,notrashcc,notrashcc,notrashcc,notrashcc,notrashcc")])

(define_expand "movsi"
  [(set (match_operand:SI 0 "r_ir_da_x_ba_bx_operand" "")
	(match_operand:SI 1 "r_im_ir_da_x_ba_bx_operand" ""))]
  ""
  "
  if (!reload_in_progress && !reload_completed && !moveok(operands, SImode))
    operands[1] = copy_to_mode_reg (SImode, operands[1]);
")


;; PSI mode

(define_insn "movpsi_matcher"
  [(set (match_operand:PSI 0 "r_ir_da_x_ba_bx_prd_operand" "=r,r,r,r,rQR,QRS,<")
	(match_operand:PSI 1 "r_im_ir_da_x_ba_bx_poi_operand" "I,L,ri,QRS,>,r,rQR"))]
  "moveok(operands, PSImode)"
  "@
	xor	%I0,%I0\;ld	%H0,%I0
	xor	%H0,%H0\;ldk	%I0,%H1
	ldl	%S0,%S1
	$ldl	%S0,%S1
	popl	%S0,@%1
	$ldl	%S0,%S1
	pushl	@%0,%S1"
  [(set_attr "cond" "trashcc,trashcc,notrashcc,notrashcc,notrashcc,notrashcc,notrashcc")])

(define_expand "movpsi"
  [(set (match_operand:PSI 0 "r_ir_da_x_ba_bx_operand" "")
	(match_operand:PSI 1 "r_im_ir_da_x_ba_bx_operand" ""))]
  ""
  "
  if (!reload_in_progress && !reload_completed && !moveok(operands, PSImode))
    operands[1] = copy_to_mode_reg (PSImode, operands[1]);
")
 
(define_insn ""
  [(set (match_operand:PSI 0 "r_operand" "=r")
	(plus:PSI (match_operand:PSI 1 "r_operand" "0")
		  (truncate:PSI (match_operand:SI 2 "r_im_operand" "r"))))]
  ""
  "add	%I0,%I2"
  [(set_attr "cond" "logcc")])
			   
(define_insn "addpsi3_"
  [(set (match_operand:PSI 0 "r_operand"           "=r,r,r,r,r,r,r,r")
	(plus:PSI (match_operand:PSI 1 "r_ir_da_x_operand" "%0,0,0,0,0,0,0,0")
		  (match_operand:PSI 2 "r_im_ir_da_x_operand" "J,K,n,rQRi,r,r,r,r")))]
  "r_operand (operands[0], PSImode)"
  "*
{
  switch (which_alternative)
    {
    case 0: return \"inc	%I0,%H2 ! psi2\";
    case 1: return \"dec	%I0,%N2 ! psi2\";
    case 2: 
      /* This can happen when the fp has been eliminated */
      if (INTVAL(operands[2]) == 0) return \"\";
      return \"add	%I0,%H2 !psi2\";
    case 3: return \"addl	%S0,%S2 !psi2\";
    case 4: return \"ldl	%S0,%S1\;inc	%I0,%H2 !psi2\";
    case 5: return \"ldl	%S0,%S1\;dec	%I0,%N2 !psi2\";
    case 6:
      if (REGNO(operands[0]) == REGNO(operands[1]))
	{
	  return \"addl	%S0,%S2 ! psi2 - a\";
	}
      if (REGNO(operands[0]) == REGNO(operands[2]))
	{
	  return \"addl	%S0,%S1 ! psi2 - b\";
	}
      return \"ldl	%S0,%S2\;addl	%S0,%S1 ! psi2 - c\";
    case 7:
      /* r = r + thing */
      if (REGNO(operands[1]) == REGNO(operands[0])) 
	{
	  /* Rn += thing */
	  saved_reg_on_stack_hack =1;
	  return \"pushl @%X0,rr0\;ldl	rr0,%S2\;addl	%S0,rr0\;popl	rr0,@%X0 ! reg alert %S0=%S1+%S2\";
	}
      return \"ldl	%S0,%S2\;addl	%S0,%S1\";
    }
}"
  [(set_attr "cond" "trashcc")])

(define_expand "addpsi3"
  [(set (match_operand:PSI 0 "r_operand" "")
	(plus:PSI (match_operand:PSI 1 "r_im_ir_da_x_operand" "")	
		  (match_operand:PSI 2 "r_im_ir_da_x_operand" "")))]
  ""
  "")


(define_insn "subpsi3_"
  [(set (match_operand:PSI 0 "r_operand" "=r,r,r,r,r,r,r,r")
	(minus:PSI (match_operand:PSI 1 "r_im_ir_da_x_operand" "0,0,0,0,0,0,0,0")
		   (match_operand:PSI 2 "r_im_ir_da_x_operand" "J,K,n,rQRi,r,r,r,r")))]
  ""
  "*
{
  switch (which_alternative)
    {
    case 0: return \"dec	%I0,%H2 ! psi2\";
    case 1: return \"inc	%I0,%N2 ! psi2\";
    case 2: 
      /* This can happen when the fp has been eliminated */
      if (INTVAL(operands[2]) == 0) return \"\";
      return \"sub	%I0,%H2 !psi2\";
    case 3: return \"subl	%S0,%S2 !psi2\";
    case 4: return \"ldl	%S0,%S1\;dec	%I0,%H2 !psi2\";
    case 5: return \"ldl	%S0,%S1\;inc	%I0,%N2 !psi2\";
    case 6:
      if (REGNO(operands[0]) == REGNO(operands[1]))
	{
	  return \"subl	%S0,%S2 ! psi2 - a\";
	}
      if (REGNO(operands[0]) == REGNO(operands[2]))
	{
	  return \"subl	%S0,%S1 ! psi2 - b\";
	}
      return \"ldl	%S0,%S2\;subl	%S0,%S1 ! psi2 - c\";
    case 7:
      /* r = r - thing */
      if (REGNO(operands[1]) == REGNO(operands[0])) 
	{
	  /* Rn -= thing */
	  saved_reg_on_stack_hack =1;
	  return \"pushl @%X0,rr0\;ldl	rr0,%S2\;subl	%S0,rr0\;popl	rr0,@%X0 ! reg alert %S0=%S1+%S2\";
	}
      return \"ldl	%S0,%S2\;subl	%S0,%S1\";
    }
}"
  [(set_attr "cond" "trashcc")])


(define_expand "subpsi3"
  [(set (match_operand:PSI 0 "register_operand" "")
	(minus:PSI (match_operand:PSI 1 "r_im_ir_da_x_operand" "")	
  		   (match_operand:PSI 2 "r_im_ir_da_x_operand" "")))]
  ""
  "")

;; SF  mode

(define_insn "movsf_matcher"
  [(set (match_operand:SF 0 "r_ir_da_x_ba_bx_prd_operand" "=r,r,r,r,rQR,QRS,<")
	(match_operand:SF 1 "r_im_ir_da_x_ba_bx_poi_operand" "I,L,ri,QRS,>,r,rQR"))]
  "moveok(operands, SFmode)"
  "@
	xor	%I0,%I0\;ld	%H0,%I0
	xor	%H0,%H0\;ldk	%I0,%H1
	ldl	%S0,%S1
	$ldl	%S0,%S1
	popl	%S0,@%1
	$ldl	%S0,%S1
	pushl	@%0,%S1"
  [(set_attr "cond" "trashcc,trashcc,notrashcc,notrashcc,notrashcc,notrashcc,notrashcc")])

(define_expand "movsf"
  [(set (match_operand:SF 0 "r_ir_da_x_ba_bx_operand" "")
	(match_operand:SF 1 "r_ir_da_x_ba_bx_operand" ""))]
  ""
  "{
  if (!reload_in_progress && !reload_completed && !moveok(operands, SFmode))
    operands[1] = copy_to_mode_reg (SFmode, operands[1]);
if (!reload_in_progress && !reload_completed && GET_CODE(operands[0]) != REG 
)
    operands[1] = copy_to_mode_reg (SFmode, operands[1]);

  }")



;; QI mode

; special care has to be taken with the movqi since reload
; likes to put bytes into regs which can't be read byte by byte

(define_expand "reload_inqi"
  [(parallel [(match_operand:QI 0 "register_operand" "=r")
	      (match_operand:QI 1 "memory_operand" "m")
	      (match_operand:QI 2 "register_operand" "=&u")])]
  ""
  "
{ 
 operands[2] = gen_rtx (REG, QImode, REGNO(operands[2]));
  emit_insn (gen_movqiin_using_tmp(operands[0], operands[1], operands[2]));
  DONE;
}")

(define_expand "reload_outqi"
  [(parallel [(match_operand:QI 0 "memory_operand" "=m")
	      (match_operand:QI 1 "register_operand" "r")
	      (match_operand:QI 2 "register_operand" "=&u")])]
  ""
  "
{ 
/* Want to treat temp as a qi */
 operands[2] = gen_rtx(REG, QImode, REGNO(operands[2]));
  emit_insn (gen_movqiout_using_tmp(operands[0], operands[1], operands[2]));
  DONE;
}")
  

(define_expand "movqiin_using_tmp"
  [(set (match_operand:QI 2 "register_operand" "=u")
	(match_operand:QI 1 "memory_operand" "m"))
   (set (match_operand:QI 0 "register_operand" "=r")
	(match_dup 2 ))]
  ""
  "")

(define_expand "movqiout_using_tmp"
  [(set (match_operand:QI 2 "register_operand" "=u")
	(match_operand:QI 1 "register_operand" "r"))
   (set (match_operand:QI 0 "memory_operand" "=m")
	(match_dup 2 ))]
  ""
  "")

; QRS/u must be before ur/rn so that MEM/PSEUDO will match it.  If it matches
; the other way, then we need secondary reloads.
(define_insn "movqi_matcher"
;;  [(set (match_operand:QI 0 "r_ir_da_x_ba_bx_operand" "=r,u,r,r,u,QR,QR,QRS,!*QRS")
;;	(match_operand:QI 1 "r_im_ir_da_x_ba_bx_operand" "I,un,L,rn,QRS,I,n,u,r"))]
  [(set (match_operand:QI 0 "r_ir_da_x_ba_bx_operand" "=ur,u,ur,u,QR,QR,QRS,ur")
	(match_operand:QI 1 "r_im_ir_da_x_ba_bx_operand" "I,un,L,QRS,I,n,u,rn"))]
  "moveok(operands, QImode)"
  "@
	xor	%H0,%H0
	ldb	%Q0,%Q1
	ldk	%H0,%H1
	$ldb	%Q0,%Q1
	clrb	%Q0
	$ldb	%Q0,%Q1
	$ldb	%Q0,%Q1
	ld	%H0,%H1"
  [(set_attr "cond" "trashcc,notrashcc,notrashcc,notrashcc,notrashcc,notrashcc,notrashcc,notrashcc")])
 
(define_expand "movqi"
  [(set (match_operand:QI 0 "r_ir_da_x_ba_bx_operand" "")
	(match_operand:QI 1 "r_im_ir_da_x_ba_bx_operand" ""))]
  ""
  "
  if (!reload_in_progress && !moveok(operands, QImode))
    operands[1] = copy_to_mode_reg (QImode, operands[1]);
    emit_insn (gen_movqi_matcher(operands[0], operands[1]));
   DONE;
")

(define_insn ""
  [(set (match_operand:QI 0 "push_operand" "=<")
	(match_operand:QI 1 "general_operand" "ui"))]
  ""
  "push	@%0,%H1")


;; DI mode

(define_insn "_pushdi"
  [(set (match_operand:DI 0 "push_operand" "=<")
	(match_operand:DI 1 "r_ir_da_x_operand_for_di" "rQR"))]
  ""
  "* return output_move64(operands[0], operands[1]);"
  [(set_attr "cond" "notrashcc")])

(define_insn "_movdi1"
  [(set (match_operand:DI 0 "r_operand" "=r")
	(match_operand:DI 1 "im_operand" "i"))]
  ""
  "*
{
    if ((REGNO (operands[0]) & 3) == 0)
      return \"ldl	%T0,%T1\;extsl	%D0\";

    if (INTVAL (operands[1]) < 0)
      return \"ldl	%T0,%T1\;ldl	%S0,#-1\";
    return \"ldl	%T0,%T1\;subl	%S0,%S0\";
}")
   
(define_insn "_movdi2"
  [(set (match_operand:DI 0 "r_ir_da_x_ba_operand_for_di" "=rQRT,r")
	(match_operand:DI 1 "r_ir_da_x_ba_operand_for_di" "r,rQRT"))]
  ""
  "* return output_move64(operands[0], operands[1]);"
  [(set_attr "cond" "notrashcc")])


(define_expand "movdi"
  [(set (match_operand:DI 0 "r_ir_da_x_ba_operand_for_di" "")
	(match_operand:DI 1 "r_ir_da_x_ba_operand_for_di" ""))]
  ""
  "{
	if (emit_move(operands, DImode, 0)) 
		DONE;
  }")

(define_insn ""
  [(set (match_operand:DF 0 "push_operand" "=<")
	(match_operand:DF 1 "r_ir_da_x_operand_for_di" "rQR"))]
  ""
  "* return output_move64(operands[0], operands[1]);"
  [(set_attr "cond" "notrashcc")])

(define_insn ""
  [(set (match_operand:DF 0 "r_ir_da_x_ba_operand_for_di" "=rQRT,r")
	(match_operand:DF 1 "r_ir_da_x_ba_operand_for_di" "r,rQRT"))]
  ""
  "* return output_move64 (operands[0], operands[1]);"
  [(set_attr "cond" "notrashcc")])


(define_expand "movdf"
  [(set (match_operand:DF 0 "r_ir_da_x_ba_operand_for_di" "")
	(match_operand:DF 1 "r_ir_da_x_ba_operand_for_di" ""))]
  ""
  "{
	if (emit_move(operands, DFmode, 0)) 
		DONE;
}")



;; BLK moves

(define_insn "ldirb_z8001"
  [(parallel [(set (mem:BLK (match_operand 0 "r_operand" "+v"))
		   (mem:BLK (match_operand 1 "r_operand" "+v")))
	      (set (match_dup 0) (plus:PSI (match_dup 0) 
					   (zero_extend:PSI (match_operand 2 "r_operand" "+r"))))
	      (set (match_dup 1) (plus:PSI (match_dup 1) 
					   (zero_extend:PSI (match_dup 2))))
	      (set (match_dup 2) (const_int 0))])]
  "TARGET_LDIR"
  "ldirb	@%P0,@%P1,%H2"
  [(set_attr "cond" "trashcc")])

(define_insn "ldirb_z8002"
  [(parallel [(set (mem:BLK (match_operand 0 "r_operand" "+v"))
		   (mem:BLK (match_operand 1 "r_operand" "+v")))
	      (set (match_dup 0) (plus:HI (match_dup 0) 
					   (match_operand 2 "r_operand" "+r")))
	      (set (match_dup 1) (plus:HI (match_dup 1)  (match_dup 2)))
	      (set (match_dup 2) (const_int 0))])]
  "TARGET_LDIR"
  "ldirb	@%P0,@%P1,%H2"
  [(set_attr "cond" "trashcc")])


(define_insn "ldir_z8001"
  [(parallel [(set (mem:BLK (match_operand 0 "r_operand" "+v"))
		   (mem:BLK (match_operand 1 "r_operand" "+v")))
	      (set (match_dup 0) 
		   (plus:PSI (match_dup 0) 
			     (zero_extend:PSI (mult:HI (match_operand 2 "r_operand" "+r")
						       (const_int 2)))))
	      (set (match_dup 1) (plus:PSI (match_dup 1) 
					   (mult:HI   (zero_extend:PSI (match_dup 2))
						      (const_int 2))))
	      (set (match_dup 2) (const_int 0))])]
  "TARGET_LDIR"
  "ldir	@%P0,@%P1,%H2"
  [(set_attr "cond" "trashcc")])

(define_insn "ldir_z8002"
  [(parallel [(set (mem:BLK (match_operand 0 "r_operand" "+v"))
		   (mem:BLK (match_operand 1 "r_operand" "+v")))
	      (set (match_dup 0) (plus:HI (match_dup 0) 
					  (mult:HI   (match_operand 2 "r_operand" "+r")
						     (const_int 2))))
	      (set (match_dup 1) (plus:HI (match_dup 1) 
					  (mult:HI (match_dup 2)
						   (const_int 2))))
	      (set (match_dup 2) (const_int 0))])]
  "TARGET_LDIR"
  "ldir	@%P0,@%P1,%H2"
  [(set_attr "cond" "trashcc")])

  
(define_expand "movstrhi"
  [(parallel [(set (mem:BLK (match_operand:BLK 0 "general_operand" ""))
		   (mem:BLK (match_operand:BLK 1 "general_operand" "")))
	      (use (match_operand:HI 2 "general_operand" ""))
	      (use (match_operand:HI 3 "immediate_operand" ""))])]
  "TARGET_LDIR"
  "
{
  rtx dst = copy_to_mode_reg (Pmode, convert_to_mode (Pmode, XEXP (operands[0], 0), 0));
  rtx src = copy_to_mode_reg (Pmode, convert_to_mode (Pmode, XEXP (operands[1], 0), 0));

  /* if the alignment is two or more, and we're moving an even sized
     lump of data, then we can use the ldir instruction, if not
     then we have to use the ldrib. */

  if (GET_CODE (operands[2]) == CONST_INT 
      && (INTVAL (operands[2]) %2 == 0)
      && (INTVAL (operands[2]) > 0)
      && (INTVAL (operands[3]) >= 2))
    {
      rtx count = copy_to_mode_reg (HImode,  GEN_INT (INTVAL (operands[2]) / 2));

      if (TARGET_BIG)
	emit_insn (gen_ldir_z8001 (dst, src, count));
      else
	emit_insn (gen_ldir_z8002 (dst, src, count));
    }
  else 
    {
      rtx count = copy_to_mode_reg (HImode, operands[2]);
      rtx zero_count = gen_label_rtx ();
      emit_insn (gen_tsthi (operands[2]));
      emit_jump_insn (gen_beq (zero_count));

      if (TARGET_BIG)
	emit_insn (gen_ldirb_z8001 (dst, src, count));
      else
	emit_insn (gen_ldirb_z8002 (dst, src, count));
      emit_label (zero_count);
    }
  emit_move_insn (src, src);
  DONE;
}
")


;; ----------------------------------------------------------------------
;; Extension and truncation insns.
;; ----------------------------------------------------------------------


(define_insn "truncsipsi2"
  [(set (match_operand:PSI 0 "general_operand" "=g")
	(truncate:PSI (match_operand:SI 1 "general_operand" "0")))]
  ""
  "!truncsipsi2 %0=%1")


(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "r_operand" "=r")
	(zero_extend:SI (match_operand:HI 1 "r_im_ir_da_x_operand" "rQRSo")))]
  ""
  "$ld	%I0,%H1\;sub	%H0,%H0")

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "r_operand" "=u,r")
	(zero_extend:HI (match_operand:QI 1 "r_operand" "0,0")))]
  ""
  "@
	subb	%U0,%U0
	and	%H0,#255")

(define_insn "zero_extendpsisi2"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(zero_extend:SI (match_operand:PSI 1 "general_operand" "0")))]
  ""
  "! zero ext %S0"
  [(set_attr "cond" "notrashcc")])


(define_insn "extendpsisi2"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(sign_extend:SI (match_operand:PSI 1 "general_operand" "0")))]
  ""
  "! extend psi %S0"       
  [(set_attr "cond" "notrashcc")])

(define_insn "extendsidi2"
  [(set (match_operand:DI 0 "r_operand" "=r")
	(sign_extend:DI (match_operand:SI 1 "r_operand" "r")))]
  "0"
  ""
  [(set_attr "cond" "notrashcc")])

(define_insn ""
  [(set (match_operand:SI 0 "r_operand" "=r")
	;; (tege) why subreg here?
	(sign_extend:SI (subreg:HI (match_operand:QI 1 "r_operand" "0") 0)))]

  ""
  "extsb	%H0\;exts	%S0")

;(define_insn ""
;  [(set (match_operand:PSI 0 "r_operand" "=r")
;	;; (tege) why subreg here?
;;n	(sign_extend:PSI (subreg:HI (match_operand:QI 1 "r_operand" "0") 0)))]
;
;  ""
;  "extsb	%H0\;exts	%S0")


(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "r_operand" "=r,r")
	(sign_extend:SI (match_operand:HI 1 "r_ir_da_x_ba_bx_operand" "0,rQRSo")))]
  ""
  "@
	exts	%S0
	$ld	%I0,%H1\;exts	%S0")


(define_insn "extendhipsi2"
  [(set (match_operand:PSI 0 "r_operand" "=r,r")
	(sign_extend:PSI (match_operand:HI 1 "r_ir_da_x_ba_bx_operand" "0,rQRSo")))]
  ""
  "@
	exts	%S0
	$ld	%I0,%H1\;exts	%S0")


(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "r_operand" "=r")
	(sign_extend:HI (match_operand:QI 1 "r_operand" "0")))]

  ""
  "extsb	%H0"
  [(set_attr "cond" "notrashcc")])

; adds to the sp are only done to the bottom 16bits, since the stack cannot 
; go over a 64k boundary.



; sadly this will not work since the bits 16..23 are undefined in a pointer.

(define_insn "combine1"
  [(set (match_operand:SI 0 "r_operand" "=r")
	(plus:SI (match_operand:SI 1 "r_operand" "v")
		 (match_operand:SI 2 "immediate_operand" "O")))]
  "TARGET_BIG && 0"
  "lda	%S0,%S1(%S2)")

(define_insn "addsi3_"
  [(set (match_operand:SI 0 "r_operand" "=r,r,r")
	(plus:SI (match_operand:SI 1 "r_operand" "%0,0,0")
		  (match_operand:SI 2 "r_im_ir_da_x_operand" "rQRi,r,r")))]
  "r_operand (operands[0], SImode)
  || smallint_operand (operands[2], SImode)"
  "*
{
  switch (which_alternative)
    {
    case 0: return \"addl	%S0,%S2 !si2\";
    case 1:
      if (REGNO(operands[0]) == REGNO(operands[1]))
	{
	  return \"addl	%S0,%S2 ! si2 - a\";
	}
      if (REGNO(operands[0]) == REGNO(operands[2]))
	{
	  return \"addl	%S0,%S1 ! si2 - b\";
	}
      return \"ldl	%S0,%S2\;addl	%S0,%S1 ! si2-c\";
    case 2:
      /* r = r + thing */
      if (REGNO(operands[1]) == REGNO(operands[0])) 
	{
	  /* Rn += thing */
	  saved_reg_on_stack_hack =1;
	  return \"pushl @%X0,rr0\;ldl	rr0,%S2\;addl	%S0,rr0\;popl	rr0,@%X0 ! reg alert SI %S0=%S1+%S2\";
	}
      return \"ldl	%S0,%S2\;addl	%S0,%S1 ! si2 - d\";
    }
}"
  [(set_attr "cond" "trashcc")])

(define_expand "addsi3"
  [(set (match_operand:SI 0 "r_operand" "")
	(plus:SI (match_operand:SI 1 "r_operand" "")
		 (match_operand:SI 2 "r_im_ir_da_x_operand" "")))]
  ""
  "operands[2] = force_reg(SImode, operands[2]);")
;; tege: I think this is a pessimation.  The insn is 2 words and
;; takes from 12 cycles and up.  It *does not* decrease reg preasure.
;; If this was here to actually make reload happy (the name "combine2"
;; suggests that this is not the case) we can fix it with SECONDARY_RELOAD.

(define_insn "combine2"
  [(set (match_operand:HI 0 "r_operand" "=r,r")
	(plus:HI (match_operand:HI 1 "r_operand" "v,v")
		 (match_operand:HI 2 "nonmemory_operand" "i,r")))]
  "!TARGET_BIG & 0"
  "lda	%H0,%H1(%H2)")

(define_insn ""
  [(set (match_operand:HI 0 "r_ir_da_x_operand" "=rQR,rQR,r")
	(plus:HI (match_operand:HI 1 "r_ir_da_x_operand" "%0,0,0")
		 (match_operand:HI 2 "r_im_operand" "J,K,ri")))]
  "r_operand (operands[0], HImode) || r_operand (operands[1], HImode)
   || (GET_CODE (operands[2]) == CONST_INT
       && (CONST_OK_FOR_LETTER_P (INTVAL (operands[2]), 'J')
	   || CONST_OK_FOR_LETTER_P (INTVAL (operands[2]), 'K')))"
  "@
	inc	%H0,%H2
	dec	%H0,%N2
	add	%H0,%H2"
  [(set_attr "cond" "setcc")])

; strange rules for adding to the sp 
(define_insn ""
  [(set (match_operand:HI 0 "r_operand" "=r")
	;; this is invalid use of subreg, but this should never happen.
	;; use truncate in situations like this. (tege)
	(subreg:HI (plus:SI (match_operand:SI 1 "r_operand" "r")
			    (match_operand:SI 2 "immediate_operand" "i"))
		  1))]
  ""
  "ld	%H0,%I1\;add	%H0,%H2	! i3")
					     
  
; strange rules for adding to the sp 
(define_insn ""
  [(set (match_operand:HI 0 "r_operand" "=r")
	;; this is invalid use of subreg, but this should never happen.
	;; use truncate in situations like this. (tege)
	(subreg:HI (plus:PSI (match_operand:PSI 1 "r_operand" "r")
			    (match_operand:PSI 2 "immediate_operand" "i"))
		  1))]
  ""
  "ld	%H0,%I1\;add	%H0,%H2	! i3")
					     


(define_expand "addhi3"
  [ (set (match_operand:HI 0 "r_operand" "") 
	 (plus:HI (match_operand:HI 1 "r_operand" "")
		  (match_operand:HI 2 "nonmemory_operand" "")))

    ]
  ""
  "")


; subsi from the stack pointer is done as a 16bit sub, since the stack can't go
; over a 64k boundary.

(define_insn "subsi3"
  [(set (match_operand:SI 0 "r_operand" "=r,q")
	(minus:SI (match_operand:SI 1 "general_operand" "0,0")
		  (match_operand:SI 2 "r_im_ir_da_x_operand" "riQR,riQR")))]
  ""
  "@
	subl	%S0,%S2
	sub	r15,%H2"
  [(set_attr "cond" "setcc")])

(define_insn "_subhi3"
  [(set (match_operand:HI 0 "r_operand" "=r")
	(minus:HI (match_operand:HI 1 "r_operand" "0")
		  (match_operand:HI 2 "r_im_ir_da_x_operand" "riQR")))]
  ""
  "sub	%H0,%H2"
  [(set_attr "cond" "setcc")])

(define_expand "subhi3"
  [(set (match_operand:HI 0 "r_operand" "=r")
	(minus:HI (match_operand:HI 1 "r_operand" "r")
		  (match_operand:HI 2 "r_im_ir_da_x_operand" "riQR")))]
  ""
  "")

;;----------------------------------------------------------------------
;; Multiply instructions.
;;----------------------------------------------------------------------

(define_insn ""
  [(clobber (reg:HI 4))
   (clobber (reg:HI 5))
   (set (reg:DI 4)
	(mult:DI
	 (sign_extend:DI (reg:SI 6))
	 (sign_extend:DI (match_operand 0 "immediate_operand" "i"))))]
  ""
  "multl	rq4,%S0")

(define_insn ""
  [(clobber (reg:HI 4))
   (clobber (reg:HI 5))
   (set (reg:DI 4)
	(mult:DI
	 (sign_extend:DI (reg:SI 6))
	 (sign_extend:DI (match_operand:SI 0 "r_im_ir_da_operand" "irQR"))))]
  ""
  "multl	rq4,%S0")

(define_expand "mulsi3"
  [(set (reg:SI 6) 
	(match_operand:SI 1 "r_im_ir_da_operand" "irQR"))
   (parallel[(clobber (reg:HI 4))
	     (clobber (reg:HI 5))
	     (set (reg:DI 4)
		  (mult:DI
		   (sign_extend:DI (reg:SI 6))
		   (sign_extend:DI (match_operand:SI 2 "r_im_ir_da_operand" "irQR"))))])
   (set (match_operand:SI 0 "general_operand" "") (reg:SI 6))]
  ""
  "")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(mult:SI 
	 (sign_extend:SI (match_operand:HI 1 "r_im_ir_da_operand" "irQR"))
	 (sign_extend:SI (match_operand 2 "immediate_operand" "i"))))]
  ""
  "ld	%I0,%H1\;mult	%S0,%H2")

(define_insn "mulhisi3"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(mult:SI 
	 (sign_extend:SI (match_operand:HI 1 "r_im_ir_da_operand" "irQR"))
	 (sign_extend:SI (match_operand:HI 2 "r_im_ir_da_operand" "irQR"))))]
  ""
  "ld	%I0,%H1\;mult	%S0,%H2")

	

;; ----------------------------------------------------------------------
;; Divide instructions.
;; ----------------------------------------------------------------------


(define_insn ""
  [(set (reg:HI 5)
	(div:HI (reg:HI 5)
		(reg:HI 7)))
   (set (reg:HI 4)
	(mod:HI (reg:HI 5) (reg:HI 7)))]
  ""
  "exts	rr4\;div	rr4,r7")

(define_expand "divmodhi4"
  [(set (reg:HI 5) (match_operand:HI 1 "r_im_ir_da_x_operand" ""))
   (set (reg:HI 7) (match_operand:HI 2 "r_im_ir_da_x_operand" ""))
   (parallel
    [ 
     (set (reg:HI 5)
	  (div:HI (reg:HI 5)
		  (reg:HI 7)))
     (set (reg:HI 4)
	  (mod:HI (reg:HI 5)
		  (reg:HI 7)))])
   (set (match_operand:HI 0 "r_ir_da_x_ba_bx_operand" "")
	(reg:HI 5))
   (set (match_operand:HI 3 "r_ir_da_x_ba_bx_operand" "") 
	(reg:HI 4))]
  ""
  "")


(define_insn ""
  [(set (reg:HI 5)
	(div:HI (reg:HI 5)
		(match_operand:HI 0 "r_im_ir_da_x_operand" "riQR")))
   (clobber (reg:HI 4))]
  ""
  "exts	rr4\;div	rr4,%H0")

(define_expand "divhi3"
  [(set (reg:HI 5) (match_operand:HI 1 "r_im_ir_da_x_operand" ""))
   (parallel [(set (reg:HI 5)
		   (div:HI (reg:HI 5)
			   (match_operand:HI 2 "r_im_ir_da_x_operand" "")))
	      (clobber (reg:HI 4))])
   (set (match_operand:HI 0 "r_ir_da_x_ba_bx_operand" "") (reg:HI 5))]
  ""
  "")

;; SI


(define_insn ""
  [(set (reg:SI 6)
	(div:SI (reg:SI 6)
		(match_operand:SI 0 "r_im_ir_da_x_operand" "riQR")))
   (clobber (reg:SI 4))]
  ""
  "extsl	rq4\;divl	rq4,%S0")

(define_expand "divsi3"
  [(set (reg:SI 6) (match_operand:SI 1 "r_im_ir_da_x_operand" ""))
   (parallel [(set (reg:SI 6)
		   (div:SI (reg:SI 6)
			   (match_operand:SI 2 "r_im_ir_da_x_operand" "")))
	      (clobber (reg:SI 4))])
   (set (match_operand:SI 0 "r_ir_da_x_ba_bx_operand" "") (reg:SI 6))]
  ""
  "")

(define_insn ""
  [(set (reg:SI 6)
	(div:SI (reg:SI 6)
		(reg:SI 8)))
   (set (reg:SI 4)
	(mod:SI (reg:SI 6) (reg:SI 8)))]
  ""
  "extsl	rq4\;divl	rq4,rr8")

(define_expand "divmodsi4"
  [(set (reg:SI 6) (match_operand:SI 1 "r_im_ir_da_x_operand" ""))
   (set (reg:SI 8) (match_operand:SI 2 "r_im_ir_da_x_operand" ""))
   (parallel [(set (reg:SI 6)
		   (div:SI (reg:SI 6)
			   (reg:SI 8)))
	      (set (reg:SI 4)
		   (mod:SI (reg:SI 6)
			   (reg:SI 8)))])
   (set (match_operand:SI 0 "r_ir_da_x_ba_bx_operand" "")
	(reg:SI 6))
   (set (match_operand:SI 3 "r_ir_da_x_ba_bx_operand" "") 
	(reg:SI 4))
   ]
  ""
  "")

(define_insn "_udivmodsi4"
  [(set (reg:SI 2)
	(udiv:SI (reg:SI 2)
		 (match_operand:SI 0 "immediate15_operand" "n")))
   (set (reg:SI 0)
	(umod:SI (reg:SI 2) (match_dup 0)))]
  ""
  "subl	rr0,rr0\;divl rq0,%S0")


(define_expand "udivmodsi4"
  [(set (match_dup 5) (match_operand:SI 1 "general_operand" ""))
   (parallel[(set (match_dup 5)
		  (udiv:SI (match_dup 5)
			   (match_operand:SI 2 "general_operand" "")))
	     (set (match_dup 4)
		  (umod:SI (match_dup 5) (match_dup 2)))])
   (set (match_operand:SI 0 "general_operand" "") (match_dup 5))
   (set (match_operand:SI 3 "general_operand" "") (match_dup 4))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT
      || ((INTVAL (operands[2]) & 0x8000) != 0x0000))
    FAIL;
  operands[4] = gen_rtx (REG, SImode, 0);
  operands[5] = gen_rtx (REG, SImode, 2);
}")



;; ----------------------------------------------------------------------
;; And
;; ----------------------------------------------------------------------


(define_insn "andhi_power"
  [(set (match_operand:HI 0 "r_ir_da_x_operand" "=rQR")
	(and:HI (match_operand:HI 1 "r_ir_da_x_operand" "%0")
		(match_operand:HI 2 "com_power_two_operand" "P")))]
  ""
  "res	%H0,%P2"
  [(set_attr "cond" "notrashcc")])

(define_insn "andhi3"
  [(set (match_operand:HI 0 "r_operand" "=r")
	(and:HI (match_operand:HI 1 "not_subreg_register_operand" "%0")
		(match_operand:HI 2 "r_im_ir_da_x_operand" "riQR")))]
  ""
  "and	%H0,%H2"
  [(set_attr "cond" "setcc")])


;; ----------------------------------------------------------------------
;; IOR
;; ----------------------------------------------------------------------


;; To get optimal code here, we should define two special
;; (set (cc0) (ior ...)) patterns.  Without them, we might "optimize"
;; "ior; insn-using-flags" into "set; test; insn-using-flags".
;; Same for and pattenrs above!

(define_insn "orhi_power"
  [(set (match_operand:HI 0 "r_ir_da_x_operand" "=rQR")
	(ior:HI (match_operand:HI 1 "r_ir_da_x_operand" "%0")
		(match_operand:HI 2 "power_two_operand" "O")))]
  ""
  "set	%H0,%O2"
  [(set_attr "cond" "notrashcc")])

(define_insn "iorhi3"
  [(set (match_operand:HI 0 "r_operand" "=r")
	(ior:HI (match_operand:HI 1 "r_operand" "%0")
		(match_operand:HI 2 "r_im_ir_da_x_operand" "riQR")))]
  ""
  "or	%H0,%H2"
  [(set_attr "cond" "logcc")])



;; ----------------------------------------------------------------------
;; Xor
;; ----------------------------------------------------------------------


(define_insn "xorhi3"
  [(set (match_operand:HI 0 "r_operand" "=r")
	(xor:HI (match_operand:HI 1 "r_operand" "%0")
		(match_operand:HI 2 "r_im_ir_da_x_operand" "riQR")))]
  ""
  "xor	%H0,%H2"
  [(set_attr "cond" "logcc")])


;; ----------------------------------------------------------------------
;; Negate
;; ----------------------------------------------------------------------


(define_insn "neghi2"
  [(set (match_operand:HI 0 "r_ir_da_x_operand" "=rQR")
	(neg:HI (match_operand:HI 1 "r_ir_da_x_operand" "0")))]
  ""
  "neg	%H0"
  [(set_attr "cond" "logcc")])

(define_insn "negsi2"
  [(set (match_operand:SI 0 "r_operand" "=r")
	(neg:SI (match_operand:SI 1 "r_operand" "0")))]
  ""
  "com	%H0\;com	%I0\;addl	%S0,#1"
  [(set_attr "cond" "logcc")])

;; ----------------------------------------------------------------------
;; Not
;; ----------------------------------------------------------------------

(define_insn "one_cmplhi2"
  [(set (match_operand:HI 0 "r_ir_da_x_operand" "=rQR")
	(not:HI (match_operand:HI 1 "r_ir_da_x_operand" "0")))]
  ""
  "com	%H0"
  [(set_attr "cond" "logcc")])


;; ----------------------------------------------------------------------
;; Shifts
;; ----------------------------------------------------------------------


;; arithmetic right

;; SI mode

(define_insn ""
  [(set (match_operand:SI 0 "r_operand" "=r")
	(ashiftrt:SI (match_operand:SI 1 "r_operand" "0")
		     (neg:HI (match_operand:HI 2 "r_operand" "r"))))]
  ""
  "sdal	%S0,%H2"
  [(set_attr "cond" "logcc")])

(define_insn ""
  [(set (match_operand:SI 0 "r_operand" "=r")
	(ashiftrt:SI (match_operand:SI 1 "r_operand" "0")
		     (match_operand:HI 2 "const_int_operand" "n")))]
  ""
  "sral	%S0,%2"
  [(set_attr "cond" "logcc")])

(define_expand "ashrsi3"
  [(set (match_operand:SI 0 "r_operand" "=r,r")
	(ashiftrt:SI (match_operand:SI 1 "r_operand" "0,0")
		     (match_operand:HI 2 "r_im_operand" "r,i")))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT)
    operands[2] = gen_rtx (NEG, HImode, negate_rtx (HImode, operands[2]));
}")


;; logical right 
;; SI mode


(define_insn ""
  [(set (match_operand:SI 0 "r_operand" "=r")
	(lshiftrt:SI (match_operand:SI 1 "r_operand" "0")
		     (neg:HI (match_operand:HI 2 "r_operand" "r"))))]
  ""
  "sdll	%S0,%H2"
  [(set_attr "cond" "logcc")])

(define_insn ""
  [(set (match_operand:SI 0 "r_operand" "=r")
	(lshiftrt:SI (match_operand:SI 1 "r_operand" "0")
		     (match_operand:HI 2 "immediate_operand" "i")))]
  ""
  "srll	%S0,%H2"
  [(set_attr "cond" "logcc")])

(define_expand "lshrsi3"
  [(set (match_operand:SI 0 "r_operand" "=r,r")
	(lshiftrt:SI (match_operand:SI 1 "r_operand" "0,0")
		   (match_operand:HI 2 "r_im_operand" "r,i")))]
  ""
  "{
    if (GET_CODE (operands[2]) != CONST_INT)
      operands[2] = gen_rtx (NEG, HImode, negate_rtx (HImode, operands[2]));
   }")

;; left
;; SI mode

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "r_operand" "=r,r,r,r")
	(ashift:SI (match_operand:SI 1 "r_operand" "0,0,0,0")
		   (match_operand:HI 2 "r_im_operand" "M,N,r,i")))]
  ""
  "@
	addl	%S0,%S0 ! ashlsi3
	addl	%S0,%S0\;addl	%S0,%S0 ! ashlsi3
	sdal	%S0,%H2 ! ashlsi3
	slal	%S0,%H2"
  [(set_attr "cond" "logcc")])




(define_insn "_lshrdi3"
  [(set (match_operand:DI 0 "r_operand" "=r")
	(lshiftrt:DI (match_operand:DI 1 "r_operand" "0")
		     (const_int 1)))]
  ""
  "srll	%S0,#1\;rrc	%J0,#1\;rrc	%K0,#1")

(define_expand "lshrdi3"
  [(set (match_operand:DI 0 "r_operand" "=r")
	(lshiftrt:DI (match_operand:DI 1 "r_operand" "0")
		     (match_operand:HI 2 "immediate_operand" "")))]

  ""
  "
{
if (GET_CODE (operands[2]) != CONST_INT
	|| INTVAL (operands[2]) != 1) FAIL;
}
")


(define_insn "_ashrdi3"
  [(set (match_operand:DI 0 "r_operand" "=r")
	(ashiftrt:DI (match_operand:DI 1 "r_operand" "0")
		     (const_int 1)))]
  ""
  "sral	%S0,#1\;rrc	%J0,#1\;rrc	%K0,#1")

(define_expand "ashrdi3"
  [(set (match_operand:DI 0 "r_operand" "=r")
	(ashiftrt:DI (match_operand:DI 1 "r_operand" "0")
		     (match_operand:HI 2 "immediate_operand" "")))]

  ""
  "
{
if (GET_CODE (operands[2]) != CONST_INT
	|| INTVAL (operands[2]) != 1) FAIL;
}
")
(define_insn "_ashldi3"
  [(set (match_operand:DI 0 "r_operand" "=r")
	(ashift:DI (match_operand:DI 1 "r_operand" "0")
		   (const_int 1)))]
  ""
  "addl	%T0,%T0\;adc	%I0,%I1\;adc	%H0,%H1")

(define_expand "ashldi3"
  [(set (match_operand:DI 0 "r_operand" "=r")
	(ashift:DI (match_operand:DI 1 "r_operand" "0")
		   (match_operand:HI 2 "immediate_operand" "")))]

  ""
  "
{
if (GET_CODE (operands[2]) != CONST_INT
	|| INTVAL (operands[2]) != 1) FAIL;
}
")



;;
;;
;; PSI shifts

(define_insn ""
  [(set (match_operand:PSI 0 "r_operand" "=r")
	(ashiftrt:PSI (match_operand:PSI 1 "r_operand" "0")
		     (neg:HI (match_operand:HI 2 "r_operand" "r"))))]
  ""
  "sdal	%S0,%H2"
  [(set_attr "cond" "logcc")])

(define_insn ""
  [(set (match_operand:PSI 0 "r_operand" "=r")
	(lshiftrt:PSI (match_operand:PSI 1 "r_operand" "0")
		      (match_operand:HI 2 "immediate_operand" "i")))]
  ""
  "srll	%S0,%H2"
  [(set_attr "cond" "logcc")])

(define_insn ""
  [(set (match_operand:PSI 0 "r_operand" "=r")
	(ashiftrt:PSI (match_operand:PSI 1 "r_operand" "0")
		     (match_operand:HI 2 "const_int_operand" "n")))]
  ""
  "sral	%S0,%2"
  [(set_attr "cond" "logcc")])

(define_expand "ashrpsi3"
  [(set (match_operand:PSI 0 "r_operand" "=r,r")
	(ashiftrt:PSI (match_operand:PSI 1 "r_operand" "0,0")
		     (match_operand:HI 2 "r_im_operand" "r,i")))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT)
    operands[2] = gen_rtx (NEG, HImode, negate_rtx (HImode, operands[2]));
}")


;; logical right 
;; PSI mode


(define_insn ""
  [(set (match_operand:PSI 0 "r_operand" "=r")
	(lshiftrt:PSI (match_operand:PSI 1 "r_operand" "0")
		     (neg:HI (match_operand:HI 2 "r_operand" "r"))))]
  ""
  "sdll	%S0,%H2"
  [(set_attr "cond" "logcc")])

(define_expand "lshrpsi3"
  [(set (match_operand:PSI 0 "r_operand" "=r,r")
	(lshiftrt:PSI (match_operand:PSI 1 "r_operand" "0,0")
		   (match_operand:HI 2 "r_im_operand" "r,i")))]
  ""
  "{
    if (GET_CODE (operands[2]) != CONST_INT)
      operands[2] = gen_rtx (NEG, HImode, negate_rtx (HImode, operands[2]));
   }")

;; left
;; PSI mode

(define_insn "ashlpsi3"
  [(set (match_operand:PSI 0 "r_operand" "=r,r,r,r")
	(ashift:PSI (match_operand:PSI 1 "r_operand" "0,0,0,0")
		   (match_operand:HI 2 "r_im_operand" "M,N,r,i")))]
  ""
  "@
	addl	%S0,%S0 ! ashlpsi3
	addl	%S0,%S0\;addl	%S0,%S0 ! ashlpsi3
	sdal	%S0,%H2
	slal	%S0,%H2"
  [(set_attr "cond" "logcc")])


;; HI shifts

;; arithmetic right

(define_insn ""
  [(set (match_operand:HI 0 "r_operand" "=r")
	(ashiftrt:HI (match_operand:HI 1 "r_operand" "0")
		     (match_operand:HI 2 "const_int_operand" "n")))]
  ""
  "sra	%H0,%H2"
  [(set_attr "cond" "logcc")])

(define_insn ""
  [(set (match_operand:HI 0 "r_operand" "=r")
	(ashiftrt:HI (match_operand:HI 1 "r_operand" "0")
		     (neg:HI (match_operand:HI 2 "r_operand" "r"))))]
  ""
  "sda	%H0,%H2"
  [(set_attr "cond" "logcc")])

(define_expand "ashrhi3"
  [(set (match_operand:HI 0 "r_operand" "=r,r")
	(ashiftrt:HI (match_operand:HI 1 "r_operand" "0,0")
		     (match_operand:HI 2 "r_im_operand" "r,i")))]
  ""
  "
  {
  if (GET_CODE (operands[2]) != CONST_INT)
    operands[2] = gen_rtx (NEG, HImode, negate_rtx (HImode, operands[2]));
  }")


;;; logical  right

(define_insn ""
  [(set (match_operand:HI 0 "r_operand" "=r")
	(lshiftrt:HI (match_operand:HI 1 "r_operand" "0")
		     (neg:HI (match_operand:HI 2 "r_operand" "r"))))]
  ""
  "sdl	%H0,%H2"
  [(set_attr "cond" "logcc")])

(define_insn ""
  [(set (match_operand:HI 0 "r_operand" "=r")
	(lshiftrt:HI (match_operand:HI 1 "r_operand" "0")
		     (match_operand:HI 2 "const_int_operand" "n")))]
  ""
  "srl	%H0,%H2"
  [(set_attr "cond" "logcc")])

(define_expand "lshrhi3"
  [(set (match_operand:HI 0 "r_operand" "=r,r")
	(lshiftrt:HI (match_operand:HI 1 "r_operand" "0,0")
		   (match_operand:HI 2 "r_im_operand" "r,i")))]
  ""
  "{
    if (GET_CODE (operands[2]) != CONST_INT)
      operands[2] = gen_rtx (NEG, HImode, negate_rtx (HImode, operands[2]));
   }")

;; left

(define_insn "ashlhi3"
  [(set (match_operand:HI 0 "r_operand" "=r,r,r,r")
	(ashift:HI (match_operand:HI 1 "r_operand" "0,0,0,0")
		   (match_operand:HI 2 "r_im_operand" "M,N,L,r")))]
  ""
  "@
	add	%H0,%H0
	add	%H0,%H0\;add	%H0,%H0
	sla	%H0,%H2
	sda	%H0,%H2"
  [(set_attr "cond" "logcc")])

;; ----------------------------------------------------------------------
;; Special cases of shifting
;; ----------------------------------------------------------------------

; Some of these patterns might look like they would never ever match
; for any real code.  But they do match.

; I suppose I should use DEFINE_SPLIT here to get optimal code

(define_insn ""
  [(set (match_operand:HI 0 "r_operand" "=&r")
	(ashift:HI (const_int 1)
		   (match_operand:HI 1 "r_operand" "r")))]
  ""
  "ldk	%H0,#0\;set	%H0,%H1"
  [(set_attr "cond" "notrashcc")])

(define_insn ""
  [(set (match_operand:HI 0 "r_operand" "=&r")
	(rotate:HI (const_int -2)
		   (match_operand:HI 1 "r_operand" "r")))]
  ""
  "ld	%H0,#-1\;res	%H0,%H1"
  [(set_attr "cond" "notrashcc")])

(define_insn ""
  [(set (match_operand:HI 0 "r_operand" "=r,&r,&r")
	(ior:HI (ashift:HI (const_int 1)
			   (match_operand:HI 2 "r_operand" "r,r,r"))
		(match_operand:HI 1 "r_im_operand" "0,L,i")))]
  ""
  "@
	set	%H0,%H2 !c
	ldk	%H0,%1\;set	%H0,%H2 !c
	ld	%H0,%1\;set	%H0,%H2 !c"
  [(set_attr "cond" "notrashcc,notrashcc,notrashcc")])

(define_insn ""
  [(set (match_operand:HI 0 "r_operand" "=r,&r,&r")
	(and:HI (rotate:HI (const_int -2)
			   (match_operand:HI 2 "r_operand" "r,r,r"))
		(match_operand:HI 1 "r_im_operand" "0,L,i")))]
  ""
  "@
	res	%H0,%H2
	ldk	%H0,%1\;res	%H0,%H2
	ld	%H0,%1\;res	%H0,%H2"
  [(set_attr "cond" "notrashcc,notrashcc,notrashcc")])


;; ----------------------------------------------------------------------
;; SCOND
;; ----------------------------------------------------------------------

(define_insn "slt"
  [(set (match_operand:HI 0 "r_operand" "=r")
	(lt:HI (cc0) (const_int 0)))]
  ""
  "ldk	%H0,#0\;tcc	lt,%H0"
  [(set_attr "cond" "notrashcc")
   (set_attr "type" "invisible")
   (set_attr "length" "4")])

(define_insn "sne"
  [(set (match_operand:HI 0 "r_operand" "=r")
	(ne:HI (cc0) (const_int 0)))]
  ""
  "ldk	%H0,#0\;tcc	ne,%H0"
  [(set_attr "cond" "notrashcc")
   (set_attr "type" "invisible")
   (set_attr "length" "4")])


(define_insn "seq"
  [(set (match_operand:HI 0 "r_operand" "=r")
	(eq:HI (cc0) (const_int 0)))]
  ""
  "ldk	%H0,#0\;tcc	eq,%H0"
  [(set_attr "cond" "notrashcc")
   (set_attr "type" "invisible")
   (set_attr "length" "4")])


(define_insn "sge"
  [(set (match_operand:HI 0 "r_operand" "=r")
	(ge:HI (cc0) (const_int 0)))]
  ""
  "ldk	%H0,#0\;tcc	ge,%H0"
  [(set_attr "cond" "notrashcc")
   (set_attr "type" "invisible")
   (set_attr "length" "4")])

(define_insn "sgt"
  [(set (match_operand:HI 0 "r_operand" "=r")
	(gt:HI (cc0) (const_int 0)))]
  ""
  "ldk	%H0,#0\;%F0tcc	gt,%H0"
  [(set_attr "cond" "notrashcc")
   (set_attr "type" "invisible")
   (set_attr "length" "4")])


(define_insn "sle"
  [(set (match_operand:HI 0 "r_operand" "=r")
	(le:HI (cc0) (const_int 0)))]
  ""
  "ldk	%H0,#0\;%F0tcc	le,%H0"
  [(set_attr "cond" "notrashcc")
   (set_attr "type" "invisible")
   (set_attr "length" "4")])

(define_insn "sleu"
  [(set (match_operand:HI 0 "r_operand" "=r")
	(leu:HI (cc0) (const_int 0)))]
  ""
  "ldk	%H0,#0\;tcc	ule,%H0"
  [(set_attr "cond" "notrashcc")
   (set_attr "type" "invisible")
   (set_attr "length" "4")])

(define_insn "sltu"
  [(set (match_operand:HI 0 "r_operand" "=r")
	(ltu:HI (cc0) (const_int 0)))]
  ""
  "ldk	%H0,#0;\tcc	ult,%H0"
  [(set_attr "cond" "notrashcc")
   (set_attr "type" "invisible")
   (set_attr "length" "4")])

(define_insn "sgeu"
  [(set (match_operand:HI 0 "r_operand" "=r")
	(geu:HI (cc0) (const_int 0)))]
  ""
  "ldk	%H0,#0\;tcc	uge,%H0"
  [(set_attr "cond" "notrashcc")
   (set_attr "type" "invisible")
   (set_attr "length" "4")])

(define_insn "sgtu"
  [(set (match_operand:HI 0 "r_operand" "=r")
	(gtu:HI (cc0) (const_int 0)))]
  ""
  "ldk	%H0,#0\;tcc	ugt,%H0"
  [(set_attr "cond" "notrashcc")
   (set_attr "type" "invisible")
   (set_attr "length" "4")])


;; ----------------------------------------------------------------------
;; Branches
;; ----------------------------------------------------------------------


(define_expand "ble"
  [(set (pc)
	(if_then_else (le (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "")

(define_expand "bleu"
  [(set (pc)
	(if_then_else (leu (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "")

(define_expand "bge"
  [(set (pc)
	(if_then_else (ge (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "")

(define_expand "bgeu"
  [(set (pc)
	(if_then_else (geu (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "")

(define_expand "blt"
  [(set (pc)
	(if_then_else (lt (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "")

(define_expand "bltu"
  [(set (pc)
	(if_then_else (ltu (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "")

(define_expand "bgt"
  [(set (pc)
	(if_then_else (gt (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "")

(define_expand "bgtu"
  [(set (pc)
	(if_then_else (gtu (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "")

(define_expand "beq"
  [(set (pc)
	(if_then_else (eq (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "")

(define_expand "bne"
  [(set (pc)
	(if_then_else (ne (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "")

(define_insn ""
 [(set (pc)
   (if_then_else (match_operator 1 "comparison_operator"
		  [(cc0) (const_int 0)])
    (label_ref (match_operand 0 "" ""))
    (pc)))]
 ""
 "*
{
  maybe_need_resflg (operands[1]);
  if (get_attr_length(insn) == 2) 
    {
      return \"jr	%D1,%l0\";
    }
  else 
    {
      return \"jp	%D1,%l0\";
    }
}" 
 [(set_attr "type" "branch")
  (set_attr "cond" "notrashcc")])

(define_insn ""
  [(set (pc)
	(if_then_else (match_operator 1 "comparison_operator"
				      [(cc0) (const_int 0)])
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "*
{
  maybe_need_resflg (operands[1]);

  if (get_attr_length(insn) == 2) 
    {
      return \"jr	%C1,%l0\";
    }
  else {
    return \"jp	%D1,%l0\";
  }
}"
  [(set_attr "type" "branch")
   (set_attr "cond" "notrashcc")] )

;; Unconditional and other jump instructions

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "*{
    if (get_attr_length(insn) == 2) 
      {
	return \"jr	%l0\";
      }
    else {
      return \"jp	%l0\";
    }
  }"
  [(set_attr "type" "branch")
   (set_attr "cond" "notrashcc")] )



;; ----------------------------------------------------------------------
;; Calls
;; ----------------------------------------------------------------------

(define_insn "call"
  [(call (match_operand:QI 0 "ir_da_x_operand" "m")
	 (match_operand:SI 1 "immediate_operand" "n"))]
  ""
  "call	%S0"
  [(set_attr "cond" "trashcc")])

(define_insn "call_value"
  [(set (match_operand 0 "" "=r")
	(call (match_operand:QI 1 "ir_da_x_operand" "m")
	      (match_operand:QI 2 "immediate_operand" "n")))]
  ""
  "call	%H1"
  [(set_attr "cond" "trashcc")])


;; Call subroutine returning any type.

(define_expand "untyped_call"
  [(parallel [(call (match_operand 0 "" "")
		    (const_int 0))
	      (match_operand 1 "" "")
	      (match_operand 2 "" "")])]
  ""
  "
{
  int i;

  emit_call_insn (gen_call (operands[0], const0_rtx, NULL, const0_rtx));

  for (i = 0; i < XVECLEN (operands[2], 0); i++)
    {
      rtx set = XVECEXP (operands[2], 0, i);
      emit_move_insn (SET_DEST (set), SET_SRC (set));
    }

  /* The optimizer does not know that the call sets the function value
     registers we stored in the result block.  We avoid problems by
     claiming that all hard registers are used and clobbered at this
     point.  */
  emit_insn (gen_blockage ());

  DONE;
}")


;; ----------------------------------------------------------------------
;; Return
;; ----------------------------------------------------------------------

(define_insn ""
  [(set (pc)
	(if_then_else (match_operator 0 "comparison_operator"
				      [(cc0) (const_int 0)])
		      (return)
		      (pc)))]
  "null_epilogue ()"
  "*
{
  maybe_need_resflg (operands[0]);
  return \"ret	%D0\";
}"
  [(set_attr "type" "return")
   (set_attr "length" "2")
   (set_attr "cond" "trashcc")])

(define_insn ""
  [(set (pc)
	(if_then_else (match_operator 0 "comparison_operator"
				      [(cc0) (const_int 0)])
		      (pc)
		      (return)))]
  "null_epilogue ()"
  "*
{
  maybe_need_resflg (operands[0]);
  return \"ret	%C0\";
}"
  [(set_attr "type" "return")
   (set_attr "length" "2")
   (set_attr "cond" "trashcc")])

(define_insn "return"
  [(return)]
  "reload_completed"
  "* return TARGET_YASM ? \"ret\" : \"ret	t\";"
  [(set_attr "type" "return")
   (set_attr "length" "2")
   (set_attr "cond" "trashcc")])


;; ----------------------------------------------------------------------
;; Misc
;; ----------------------------------------------------------------------

(define_insn "nop"
  [(const_int 0)]
  ""
  "ld	r0,r0	! nop"
  [(set_attr "cond" "notrashcc")])

(define_insn "indirect_jump_si"
  [(set (pc) (match_operand:SI 0 "r_operand" "v"))]
  ""
  "jp	@%S0"
  [(set_attr "cond" "notrashcc")])

(define_insn "indirect_jump_psi"
  [(set (pc) (match_operand:PSI 0 "r_operand" "v"))]
  ""
  "jp	@%S0"
  [(set_attr "cond" "notrashcc")])


(define_insn "indirect_jump_hi"
  [(set (pc) (match_operand:HI 0 "r_operand" "v"))]
  ""
  "jp	@%H0"
  [(set_attr "cond" "notrashcc")])


(define_expand "indirect_jump"
  [(set (pc) (match_operand 0 "r_operand" ""))]
	""
	"")
	
	
(define_insn "tablejump"
  [(set (pc) (match_operand 0 "r_operand" "v"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "jp	%l1(%H0)"
  [(set_attr "cond" "trashcc")])

(define_insn "tablejump_ext"
  [(set (pc) 
	(mem:HI (plus:PSI (sign_extend:PSI (match_operand:HI  0 "r_operand" "v"))
			  (label_ref (match_operand 1 "" "")))))
   (use (label_ref (match_dup 1)))]
  ""
  "ld	%H0,%l1(%H0)\;jp	%l1(%H0)"
  [(set_attr "cond" "trashcc")])



;; ----------------------------------------------------------------------
;; Machine specifics
;; ----------------------------------------------------------------------

(define_insn ""
  [(set (match_operand:HI 0 "r_operand" "=&r")
	(unspec:HI [(mem:BLK (match_operand:HI 1 "address_operand" "=r"))
		   (match_operand:QI 2 "r_operand" "u")
		   (match_operand:HI 3 "immediate_operand" "i")
		   (match_operand:HI 4 "r_operand" "0")] 0))
  (set (match_dup 1) (plus:HI (match_dup 1) (not:HI (match_dup 4))))]
  ""
  "cpirb	%Q2,@%H1,%H0,ne")

(define_expand "strlenhi"
  [(set (match_dup 4) (const_int 0))
   (set (match_dup 6) (match_operand:QI 2 "r_operand" ""))
   (parallel [(set (match_dup 4)
		   (unspec:HI [(mem:BLK (match_operand:BLK 1 "r_operand" ""))
			       (match_dup 6)
			       (match_operand:HI 3 "immediate_operand" "")
			       (match_dup 4)] 0))
	      (set (match_dup 1) (plus:HI (match_dup 1) (not:HI (match_dup 4))))])
   (set (match_operand:HI 0 "r_operand" "")
	(not:HI (match_dup 4)))]
  "0"
  "{
   operands[1] = force_reg (HImode, XEXP (operands[1], 0));
   operands[4] = gen_reg_rtx (HImode);
   operands[5] = gen_reg_rtx (HImode);
   operands[6] = gen_rtx(REG, QImode, 0);
   }")


;; ----------------------------------------------------------------------
;; Peepholes
;; ----------------------------------------------------------------------

; trn
; dec Rx,-2
; mov @Rx,Ry
; 
; into 
;
; push @Rx,Ry


(define_peephole
  [(set (match_operand:HI 0 "r_operand" "+r")
	(plus:HI
	 (match_dup 0)
	 (const_int -2)))
  (set (mem:HI  (match_dup 0))
       (match_operand:HI 1 "r_operand" "r"))]
  ""
  "push	@%H0,%H1 !3")

;; ----------------------------------------------------------------------
;; addpsi 
;; ----------------------------------------------------------------------


(define_expand "prologue"
  [(const_int 0)]
  ""
  "z8k_expand_prologue (); DONE;")

(define_expand "epilogue"
  [(return)]
  ""
  "z8k_expand_epilogue ();")

(define_insn "blockage"
 [(unspec_volatile [(const_int 0)] 0)]
  ""
  ""
  [(set_attr "length" "0")])

(define_insn "pophi"
  [(set (match_operand:HI 0 "r_operand" "=r")
	(match_operand:HI 1 "pop_operand" ">"))]
  ""
  "pop	%H0,@%H1")

(define_insn "popsi"
  [(set (match_operand:SI 0 "r_operand" "=r")
	(match_operand:SI 1 "pop_operand" ">"))]
  ""
  "popl	%S0,@%S1")


;	ld	r9,@rr8
;	cp	r9,#48875
; ->
;       cp      @rr8,#48875

(define_peephole
  [(set (match_operand:HI 0 "r_operand" "")
	(match_operand:HI 1 "r_ir_da_x_operand" ""))
   (set (cc0) (compare (match_dup 0) (match_operand:HI 2 "r_im_ir_da_x_operand" "")))]
  "0 && dead_or_set_p (insn, operands[0])
&& ((r_operand(operands[1], HImode) && r_im_ir_da_x_operand(operands[2], HImode))
   || (r_ir_da_x_operand(operands[1], HImode) &&immediate_operand(operands[2], HImode)))"
  "cp	%H1,%H2 ! p2 %H0/%H1/%H2"
  [(set_attr "cond" "setcc")])





  
(define_insn ""
  [(set (match_operand:PSI 0 "r_operand" "=r")
       (plus:PSI (match_operand:PSI 1 "symbol_ref" "i")
		 (match_operand:SI 2 "r_operand" "v")))]
  ""
  "lda	%S0,%A1(%I2) !i8")


(define_insn ""
  [(set (match_operand:PSI 0 "r_operand" "=r")
       (plus:PSI (match_operand:SI 2 "r_operand" "v")
		  (match_operand:PSI 1 "symbol_ref" "i")))]
  ""
  "lda	%S0,%A1(%I2) !i9")



(define_insn ""
  [(set (match_operand:PSI 0 "r_operand" "=r")
       (plus:PSI (match_operand:PSI 2 "r_operand" "v")
		  (match_operand:PSI 1 "symbol_ref" "i")))]
  ""
  "lda	%S0,%A1(%I2) !i10")



(define_insn "combine4"
  [(set (match_operand:PSI 0 "r_operand" "=r,r")
	(ashift:PSI (truncate:PSI (sign_extend:SI (match_operand:HI  1 "r_operand" "0,rQSso")))
		    (const_int 1)))]
  ""
  "@
	add	%I0,%I0\;xor	%H0,%H0 ! combine 4
	$ld	%I0,%H1\;add	%I0,%I0\;xor	%H0,%H0 ! combine 4")


(define_peephole
  [(set (match_operand:HI 0 "r_operand" "=r")
	(plus:HI (match_dup 0) (match_operand:HI 1 "const_int_operand" "n")))
   (set (match_dup 0) (plus:HI (match_dup 0) (match_operand:HI 2 "const_int_operand" "n")))]
  ""
  "*
{
  int val = INTVAL(operands[1]) + INTVAL(operands[2]);
  if (val) 
    {
      operands[1] = GEN_INT(val);
      if (CONST_OK_FOR_LETTER_P(val,'J'))
	{
	  return \"inc	%H0,%H1 !p5\";
	}
      else if (CONST_OK_FOR_LETTER_P(val,'K'))
	{
	  return \"dec	%H0,%N1 !p5\";
	}
      else
	{
	  return \"add	%H0,%H1 !p5\";
	}
    }
  else return \"! p5\";

}")


(define_peephole
  [(set (match_operand:PSI 0 "r_operand" "=r")
	(plus:PSI (match_dup 0) (match_operand:PSI 1 "immediate_operand" "i")))
   (set (match_dup 0) (plus:PSI (match_dup 0) (match_operand:PSI 2 "immediate_operand" "i")))]
  ""
  "*
{
  int val = INTVAL(operands[1]) + INTVAL(operands[2]);
  if (val) 
    {
      operands[1] = GEN_INT(val);
      if (CONST_OK_FOR_LETTER_P(val,'J'))
	{
	  return \"inc	%I0,%H1 !p5\";
	}
      else if (CONST_OK_FOR_LETTER_P(val,'K'))
	{
	  return \"dec	%I0,%N1 !p5\";
	}
    else
      {
	return \"add	%I0,%H1 !p5\";
     }
    }
else return \"! p5\";

}
")

(define_insn "decrement_and_branch_until_zero"
  [(set (pc)
	(if_then_else (ne (plus:HI (match_operand:HI 0 "r_operand" "+!r,!*m")
				   (const_int -1))
			  (const_int 0))
		      (label_ref (match_operand 1 "" ""))
		      (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0) (const_int -1)))]
  "TARGET_DJNZ"
  "*
  if (get_attr_length (insn) == 2 && GET_CODE (operands[0]) == REG)
    return \"djnz	%H0,%l1\";
  else if (get_attr_length (insn) == 4)
    return \"dec	%H0,#1\;jr	nz,%l1\";
  else
    return \"dec	%H0,#1\;jp	nz,%l1\";"
  [(set_attr "type" "djnz")
   (set_attr "cond" "notrashcc")])

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (match_operand:HI 0 "r_operand" "+!r,!*m")
	     (const_int 1))
	 (label_ref (match_operand 1 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:HI (match_dup 0) (const_int -1)))]
  "TARGET_DJNZ"
  "*
  if (get_attr_length (insn) == 2 && GET_CODE(operands[0]) == REG)
    return \"djnz	%H0,%l1\";
  else if (get_attr_length (insn) == 4)
    return \"dec	%H0,#1\;jr	nz,%l1\";
  else
    return \"dec	%H0,#1\;jp	nz,%l1\";"
  [(set_attr "type" "djnz")
   (set_attr "cond" "notrashcc")
   (set_attr "length" "2")])


;(define_insn "combine7"
;  [(set (match_operand:HI 0 "register_operand" "=r")
;	(mem:HI (plus:PSI (sign

(define_insn "combine7"
  [(set (match_operand:PSI 0 "register_operand" "=r,r")
	(truncate:PSI (sign_extend:SI (match_operand:HI 1 "register_operand" "0,r"))))]
  ""
  "@
	exts	%S0 ! combine7 %H0, %S1
	$ld	%I0,%H1\;exts	%S0 ! extendhisi2")

(define_insn "combine8"
 [(set (match_operand:PSI 0 "register_operand" "=r,r")
       (truncate:PSI (lshiftrt:SI (sign_extend:SI (match_operand:HI 1 "register_operand" "0,r"))
				  (match_operand:HI 2 "immediate_operand" "i,i"))))]
 ""
 "@
	srl	%I0,%H2\;xor	%H0,%H0
	ld	%I0,%H1\;srl	%I0,%H2\;xor	%H0,%H0"
 [(set_attr "cond" "trashcc")])


(define_insn "combine9"
  [(set (match_operand:PSI 0 "register_operand" "=&r")
	(plus:PSI (truncate:PSI (lshiftrt:SI (sign_extend:SI (match_operand:HI 1 "register_operand" "r"))
					     (match_operand:HI 2 "immediate_operand" "i")))
		  (match_operand:PSI 3 "register_operand" "r")))]
  ""
  "ld	%I0,%H1\;srl	%I0,%H2\;ld	%H0,%H3\;add	%I0,%I3"
 [(set_attr "cond" "trashcc")])


(define_peephole
  [(set (match_operand:PSI 0 "register_operand" "=r")
	(truncate:PSI (match_operand:SI 1 "register_operand" "r")))
   (set (match_operand:QI 2 "register_operand" "=u")
	(mem:QI (plus:PSI (match_dup 0)
			  (match_operand:PSI 3 "symbol_ref" ""))))]
 "0 && (REGNO (operands[0]) == REGNO (operands[1]) 
  && (dead_or_set_p (insn, operands[0])))"
 "ldb	%Q2,%A3(%I1) ! p9")


(define_peephole
  [(set (subreg:HI (match_operand:PSI 0 "register_operand" "=r") 1)
	(match_operand:HI 1 "register_operand" "r"))
   (set (subreg:HI (match_dup 0) 0) (const_int 0))
   (set (match_dup 0) (plus:PSI (match_dup 0) 
				(match_operand:PSI 2 "general_operand" "g")))]
  "!reg_mentioned_p (operands[0], operands[1])"
  "ldl	%S0,%S2\;add	%I0,%H1")


(define_insn "adddi3"
  [(set (match_operand:DI 0 "register_operand" "=&r,&r")
	(plus:DI (match_operand:DI 1 "register_operand" "%0,0")
		 (match_operand:DI 2 "r_im_ir_da_x_operand_for_di" "r,iQR")))]
  ""
  "@
	addl	%T0,%T2\;adc	%I0,%I2\;adc	%H0,%H2
	addl	%T0,%T2\;jr	nc,DIL%^\;addl	%B0,#1\\nDIL%^:	addl	%B0,%B2"
  [(set_attr "cond" "trashcc")
   (set_attr "length" "6,8")])


(define_insn "subdi3"
  [(set (match_operand:DI 0 "r_operand" "=&r,&r")
	(minus:DI (match_operand:DI 1 "r_operand" "0,0")
		  (match_operand:DI 2 "r_im_ir_da_x_operand_for_di" "r,iQR")))]
  ""
  "@
	subl	%T0,%T2\;sbc	%I0,%I2\;sbc	%H0,%H2
	subl	%T0,%T2\;jr	nc,DIL%^\;subl	%B0,#1\\nDIL%^:	subl	%B0,%B2"
  [(set_attr "cond" "trashcc")
   (set_attr "length" "4,8")])


(define_insn "combine10"
  [(set (match_operand:HI 0 "register_operand" "=u")
	(ior:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "0"))
		(ashift:HI (subreg:HI (match_operand:QI 2 "r_ir_da_x_ba_bx_operand" "uQRS") 0)
			   (const_int 8))))]
  ""
  "$ldb	%U0,%Q2 ! c10")


