/* Subroutines for insn-output.c for the Zilog Z8000
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.

   Written by Steve Chamberlain (sac@cygnus.com)

   This file is part of GNU CC.

   GNU CC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GNU CC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <stdio.h>
#include <string.h>
#include "config.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "tree.h"
#include "flags.h"

/* Modes ok for regs, used in tm.h for HARD_REGNO_MODE_OK */
int hard_regno_mode_ok[FIRST_PSEUDO_REGISTER];


/* We can change which reg has the sp or fp, using this
   to indirect */
static int renumber[FIRST_PSEUDO_REGISTER];      
static int rev_renumber[FIRST_PSEUDO_REGISTER];      


/* If these are ever reversed, then things will have to change, call.c
   assumes that args go in regs in increasing regno order */

#define ARG_REGS "r2,r3,r4,r5,r6,r7"	/* Default -margs-in */

/* Number of HI regs used for arg passing, can be changed on the
   command line (-margs-in) */

int arg_nregs;

/* Regs  for arg passing */
int arg_regs[FIRST_PSEUDO_REGISTER];

/* And their order */
int arg_regs_order[FIRST_PSEUDO_REGISTER];

extern int frame_pointer_needed;
#define pic_reg    (gen_rtx(REG, HImode, 13))

struct reg_info_struct
  {
    char *mode_name[MAX_MACHINE_MODE];
  };

struct reg_info_struct rinfo[FIRST_PSEUDO_REGISTER];

#define regname(x,mode)  (rinfo[renumber[x]].mode_name[mode])

char *pointer_reg[FIRST_PSEUDO_REGISTER];

#define STACK_REGISTER(x) \
  ((x)==STACK_POINTER_REGNUM || (frame_pointer_needed && (x) == FRAME_POINTER_REGNUM))

/* kludge - used if insn does ba bx set in ASM_OUTPUT_OPCODE if the
   insn allows ba, bx addressing modes */

static int can_ba_bx;

int current_function_anonymous_args;

/* Number of bytes pushed for anonymous args */

static int extra_push;

/* Linked list of all externals that are to be emitted if they haven't been
   declared by the end of the program.  */

struct extern_list
  {
    struct extern_list *next;
    char *name;
  }
 *extern_head = 0;

rtx 
getreg (x)
     rtx x;
{
  if (GET_CODE (x) == TRUNCATE)
    x = XEXP (x, 0);
  if (GET_CODE (x) == SIGN_EXTEND)
    x = XEXP (x, 0);
  return x;
}

/* Print the operand address represented by the rtx addr */

void
print_operand_address (file, addr)
     FILE *file;
     register rtx addr;
{
  rtx lhs, rhs;

retry:;

  switch (GET_CODE (addr))
    {
    case MEM:
      fprintf (file, "#");
      addr = XEXP (addr, 0);
      goto retry;

    case REG:
      fprintf (file, "@%s", pointer_reg[REGNO (addr)]);
      break;

    case PRE_DEC:
      fprintf (file, "%s", pointer_reg[REGNO (XEXP (addr, 0))]);
      break;
    case POST_INC:
      fprintf (file, "%s", pointer_reg[REGNO (XEXP (addr, 0))]);
      break;
    case PLUS:
      lhs = XEXP (addr, 0);
      rhs = XEXP (addr, 1);

      if (GET_CODE (rhs) == REG
	  && GET_CODE (lhs) != REG)
	{
	  rtx swap = lhs;
	  lhs = rhs;
	  rhs = swap;
	}


      if (GET_CODE (rhs) == SIGN_EXTEND
	  || GET_CODE (rhs) == TRUNCATE)
	{
	  rtx swap = lhs;
	  lhs = rhs;
	  rhs = swap;
	}

      if (TARGET_BIG && (inside_ba_p (addr, 1)
			 && (STACK_REGISTER (REGNO (lhs)) || can_ba_bx)
			 && GET_CODE (rhs) == CONST_INT))
	{
	  int offset = INTVAL (rhs);
	  if (REGNO (lhs) == STACK_POINTER_REGNUM && saved_reg_on_stack_hack)
	    offset += 4;
	  saved_reg_on_stack_hack = 0;
	  fprintf (file, "%s(#", pointer_reg[REGNO (lhs)]);
	  fprintf (file, "%d", offset);
	  fprintf (file, ")");
	}
      else if (inside_bx_p (addr, 1)
	       && (can_ba_bx || (STACK_REGISTER (REGNO (lhs)))))
	{
	  rtx base;
	  rtx disp;

	  if (GET_CODE (lhs) == REG)
	    {
	      base = lhs;
	      disp = rhs;
	    }
	  else
	    {
	      base = rhs;
	      disp = lhs;
	    }
	  fprintf (file, "%s(%s)", pointer_reg[REGNO (base)],
		   regname(REGNO (getreg (disp)), HImode));
	}
      else if (inside_x_p (addr, 1))
	{
	  int r = REGNO (lhs);

	  if (TARGET_BIG)
	    {
	      /* With a disp(reg) we only use the lsw, so
		 inc the reg number */
	      if (GET_MODE (lhs) == SImode || GET_MODE (lhs) == PSImode)
		r++;
	    }
	  output_address (rhs);
	  fprintf (file, "(%s)", regname(r,HImode));
	}
      else
	{
	  /* This must be an x_operand or a stack reg */
	  output_address (rhs);
	  fprintf (file, "(%s)",  regname(REGNO (lhs),HImode));

	}
      break;

    default:
      output_addr_const (file, addr);

    }
}



void 
maybe_need_resflg (cond)
     rtx cond;
{
  int code = GET_CODE (cond);
  if (cc_prev_status.flags & CC_NO_OVERFLOW
      && (code == GT
	  || code == LE))
    {
      fprintf (asm_out_file, "\tresflg	v\n");
    }
}

/* Turn a condition code into a string */
static char *
cond_name_x (code)
     int code;
{
  if (cc_prev_status.flags & CC_NO_OVERFLOW)
    {
      switch (code)
	{
	case LT:
	  return "mi";
	case GE:
	  return "pl";
	  /* The others V testers are handled by clearing V before the jump */
	}
    }
  switch (code)
    {
    case EQ:
      return "eq";
    case NE:
      return "ne";
    case LT:
      return "lt";
    case LE:
      return "le";
    case GT:
      return "gt";
    case GE:
      return "ge";
    case LTU:
      return "ult";
    case LEU:
      return "ule";
    case GTU:
      return "ugt";
    case GEU:
      return "uge";
    default:
      abort ();
    }
}


/* print either an inc or an add depending upon the size of the value */

static void
incordec (file, n1, n2, size)
     FILE *file;
     char *n1;
     char *n2;
     int size;
{
  if (size)
    {
      fprintf (file, "\t%s\tr15,#%d\n",
	       size > 16 ? n1 : n2, size);
    }
}


#define frameish(x) ((x ==  FRAME_POINTER_REGNUM) || (x == FRAME_POINTER_REGNUM+1))

/* return 1 if the register needs to be saved on function entry */

static int
need (regno)
     int regno;
{
  if (TARGET_BIG && regno == STACK_POINTER_REGNUM)
    return 0;

  return (regs_ever_live[regno]
	  && !call_used_regs[regno]
	  && regno < STACK_POINTER_REGNUM
	  && !((regno == FRAME_POINTER_REGNUM)
	   || (regno == FRAME_POINTER_REGNUM + 1) && frame_pointer_needed));

}




/* return non zero if the rtx supplied can be used as an effective
   address calculation */

static int 
address (op)
     rtx op;
{
  if (GET_CODE (op) == CONST)
    op = XEXP (op, 0);

  if (TARGET_SMALL && GET_CODE (op) == CONST_INT)
    return 1;

  if (GET_MODE (op) != Pmode)
    return 0;

  /* + (symbol_ref, foo) is address */
  if (GET_CODE (op) == PLUS && address (XEXP (op, 0)))
    return 1;

  if (GET_CODE (op) == SYMBOL_REF)
    return 1;

  if (GET_CODE (op) == LABEL_REF)
    return 1;

  return 0;
}

/*
Use S for SI regs
Use B & T for parts of DI regs
  X  - stack pointer name
  Registers
  Q - byte sized register name
  U - high byte of word register
  V - low byte of word register
  H - word register name
  I - next word register name
  S&B - long register name
  T - next long register name
  D - quad register name
  P - register name in size of pointer
  Integers
  O - log two of value
  P - inverted log two
  H - bottom 16 bits
  I - top 16 bits
  N - negative
  B - high 32 bits of 32bit number.
  default:  value
  Memory
  I - adjusted upwards by two
  T - adjusted upwards by four
  default:  value
  Address
  H - low 16 bits
  I - high 16 bits
  A - as long constant
  S - as A but with #
  default: error
  Misc
  C - conditional name
  D - reverse conditional name
  F - clear v flag if necessary
  */


void 
print_operand (file, x, code)
     FILE *file;
     rtx x;
     int code;
{
  if (code == '^')
    {
      static int lab;
      fprintf (file, "%d", lab >> 1);
      lab++;
    }
  else if (code == 'F')
    {
      if (cc_prev_status.flags & CC_NO_OVERFLOW)
	{
	  fprintf (file, "resflg	v\n\t");
	}
    }
  else if (code == 'X')
    {
      fprintf (file, "%s", pointer_reg[STACK_POINTER_REGNUM]);
    }
  else
    {
      /* If reg, output byte reg */
      if (GET_CODE (x) == REG)
	{
	  switch (code)
	    {
	    case 'Q':
	      fprintf (file, "%s", regname (REGNO (x), QImode));
	      break;
	    case 'U':
	      fprintf (file, "rh%d", REGNO (x));
	      break;
	    case 'V':
	      fprintf (file, "rl%d", REGNO (x) + 1);
	      break;

	    default:
	      fprintf (file, "r??%d", REGNO (x));
	      break;
	    case 'H':
	      fprintf (file, "%s", regname (REGNO (x), HImode));
	      break;
	    case 'I':
	      fprintf (file, "%s", regname (REGNO (x) + 1, HImode));
	      break;
	    case 'J':
	      fprintf (file, "%s", regname (REGNO (x) + 2, HImode));
	      break;
	    case 'K':
	      fprintf (file, "%s", regname (REGNO (x) + 3, HImode));
	      break;
	    case 'S':
	    case 'B':
	      fprintf (file, "%s", regname (REGNO (x), SImode));
	      break;
	    case 'T':
	      fprintf (file, "%s", regname (REGNO (x) + 2, SImode));
	      break;
	    case 'D':
	      fprintf (file, "%s", regname (REGNO (x), DImode));
	      break;
	    case 'P':
	      fprintf (file, "%s", regname (REGNO (x), Pmode));
	      break;
	    }

	}

      else if (GET_CODE (x) == CONST_INT)
	{
	  switch (code)
	    {
	    case 'O':
	      fprintf (file, "#%d", exact_log2 (INTVAL (x)));
	      break;
	    case 'P':
	      fprintf (file, "#%d", exact_log2 (~(INTVAL (x) | ~0xffff)));
	      break;
	    case 'H':
	      fprintf (file, "#%d", INTVAL (x) & 0xffff);
	      break;
	    case 'I':
	      fprintf (file, "#%d", (INTVAL (x) >> 16) & 0xffff);
	      break;
	    case 'N':
	      fprintf (file, "#%d", -INTVAL (x));
	      break;
	    case 'B':
	      {
		HOST_WIDE_INT val = INTVAL (x);
		if (sizeof (val) == 4)
		  val = val < 0 ? -1 : 0;
		else {
		  /* only happens with 32 bit values, so even when 
		     HOST_WIDE_INT is 32 bits long this is no error */
		  val >>= 32; 
		}
		fprintf (file, "#%d", val);
	      }
	      break;
	    default:
	      fprintf (file, "#%d", INTVAL (x) & 0xffffffff);
	      break;
	    }
	}
      else if (GET_CODE (x) == MEM)
	{
	  if (code == 'I')
	    {
	      x = adj_offsettable_operand (x, 2);
	    }
	  else if (code == 'T')
	    {
	      x = adj_offsettable_operand (x, 4);
	    }

	  output_address (XEXP (x, 0));

	}
      else if (address (x))
	{
	  switch (code)
	    {
	    case 'H':
	      if (TARGET_BIG)
		{
		  fprintf (file, "#low(");
		  output_addr_const (file, x);
		  fprintf (file, ")");
		}
	      else
		{
		  fprintf (file, "#");
		  output_addr_const (file, x);
		}
	      break;

	    case 'I':
	      fprintf (file, "#high(");
	      output_addr_const (file, x);
	      fprintf (file, ")");
	      break;
	    case 'A':
	      output_addr_const (file, x);
	      break;
	    case 'S':
	      fprintf (file, "#");
	      output_addr_const (file, x);
	      break;
	    default:
	      fprintf (file, "#what%c", code);
	      output_addr_const (file, x);
	      break;
	    }
	}
      else if (code == 'C')
	{
	  fprintf (file, cond_name_x (reverse_condition (GET_CODE (x))));
	}
      else if (code == 'D')
	{
	  fprintf (file, cond_name_x (GET_CODE (x)));
	}
      else if (GET_CODE (x) == CONST_DOUBLE)
	{
	  double d;
	  REAL_VALUE_FROM_CONST_DOUBLE (d, x);
	  fprintf (file, "#");
	  ASM_OUTPUT_FLOAT (file, d);
	}
      else
	{
	  fprintf (file, "#");
	  if (GET_CODE (x) == CONST)
	    x = XEXP (x, 0);
	  if (GET_CODE (x) == TRUNCATE)
	    x = XEXP (x, 0);
	  output_addr_const (file, x);
	}
    }
}


static
int
fill_from_options (value, def, string, into, order)
     int value;
     char *def;
     char *string;
     char *into;
     int *order;
{
  int regno;
  int n = 0;
  char *p;
  char *list;

  list = p = string ? string : def;

  while (*p)
    {
      if (*p == 'r')
	{
	  p++;
	  if (p[0] == '1' && p[1] >= '0' && p[1] <= '5')
	    {
	      regno = 10 + p[1] - '0';
	      p += 2;
	    }
	  else if (p[0] >= '0' && p[0] <= '9')
	    {
	      regno = p[0] - '0';
	      p++;
	    }
	  else
	    {
	      error ("error in register list `%s'", list);
	      return 0;
	    }
	  into[regno] = value;
	  if (order)
	    {
	      order[n] = regno;
	    }
	  n++;

	}
      else if (*p == ',' || *p == '-')
	{
	  p++;
	}
      else
	{
	  error ("error in register list `%s'", list);
	  return 0;
	}
    }
  return n;
}
extern enum debug_info_type write_symbols;
extern enum debug_info_level debug_info_level;
extern int flag_caller_saves;

void
override_options ()
{
  int i;
  int j;
  
  int nfakes;


  if (TARGET_STD)
    {
      target_flags |= TARGET_STD_RET_BIT | TARGET_STD_FRAME_BIT;
      target_flags &= ~TARGET_REGPARMS_BIT;
    }

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    renumber[i] = i;

  if (TARGET_BIG)
    pmode = PSImode;
  else
    pmode = HImode;
  if (TARGET_STD_FRAME)
    flag_omit_frame_pointer = 0;



  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      int mode_index;

      for (mode_index = 0;
	   mode_index <= (int) MAX_MACHINE_MODE;
	   mode_index++)
	{
	  int bsize = GET_MODE_UNIT_SIZE ((enum machine_mode) mode_index);
	  int or;

	  if (bsize > 8)
	    or = 0;
	  else if (bsize == 8 && (i & 1))
	    or = 0; 
	  else if (bsize == 4 && (i & 1))
	    or = 0;
	  else
	    or = 1;

	  hard_regno_mode_ok[i] |= or << mode_index;
	}
    }

  /* Call used - current scheme is all registers are trashed */
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    call_used_regs[i] = 1;

  /* except for ones which are explicitly mention */
  if (TARGET_BIG)
    {
      fill_from_options (0, "r8,r9,r10,r11,r12,r13", call_used_option, call_used_regs, 0);
    }
  else
    {
      fill_from_options (0, "r8,r9,r10,r11,r12,r13,r14", call_used_option, call_used_regs, 0);
    }


  arg_nregs = fill_from_options (1, ARG_REGS, args_in_option,
				 arg_regs,
				 arg_regs_order);

  if (fakes_option)
    {
      nfakes = atoi (fakes_option);
    }
  else
    nfakes = 0;

  /* Make fixed those which aren't going to be fake */
  for (j = 0, i = STACK_POINTER_REGNUM + 1;
       i < FIRST_PSEUDO_REGISTER; i++)
    {
      fixed_regs[i] = !(j < nfakes);
    }

  /* The stack and frame pointer shouldn't be call_used */

  call_used_regs[STACK_POINTER_REGNUM] = 1;
  call_used_regs[FRAME_POINTER_REGNUM] = 0;

  if (TARGET_BIG)
    {
      call_used_regs[STACK_POINTER_REGNUM + 1] = 1;
      call_used_regs[FRAME_POINTER_REGNUM + 1] = 0;
    }

  /* Fill in the ok modes */

  /* Work out the names of the registers */
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {

      regname (i, QImode) = strdup ("     ");
      sprintf (regname (i, QImode), "*QI%d", i);
      regname (i, HImode) = strdup ("     ");
      sprintf (regname (i, HImode), "*HI%d", i);
      regname (i, SImode) = strdup ("     ");
      sprintf (regname (i, SImode), "*SI%d", i);
      regname (i, DImode) = strdup ("     ");
      sprintf (regname (i, DImode), "*DI%d", i);
    }

  regname (0, QImode) = "rl0";
  regname (1, QImode) = "rl1";
  regname (2, QImode) = "rl2";
  regname (3, QImode) = "rl3";
  regname (4, QImode) = "rl4";
  regname (5, QImode) = "rl5";
  regname (6, QImode) = "rl6";
  regname (7, QImode) = "rl7";

  regname (0, HImode) = "r0";
  regname (1, HImode) = "r1";
  regname (2, HImode) = "r2";
  regname (3, HImode) = "r3";
  regname (4, HImode) = "r4";
  regname (5, HImode) = "r5";
  regname (6, HImode) = "r6";
  regname (7, HImode) = "r7";
  regname (8, HImode) = "r8";
  regname (9, HImode) = "r9";
  regname (10, HImode) = "r10";
  regname (11, HImode) = "r11";
  regname (12, HImode) = "r12";
  regname (13, HImode) = "r13";
  regname (14, HImode) = "r14";
  regname (15, HImode) = "r15";

  regname (0, SImode) = "rr0";
  regname (2, SImode) = "rr2";
  regname (4, SImode) = "rr4";
  regname (6, SImode) = "rr6";
  regname (8, SImode) = "rr8";
  regname (10, SImode) = "rr10";
  regname (12, SImode) = "rr12";
  regname (14, SImode) = "rr14";

  regname (0, PSImode) = "rr0";
  regname (2, PSImode) = "rr2";
  regname (4, PSImode) = "rr4";
  regname (6, PSImode) = "rr6";
  regname (8, PSImode) = "rr8";
  regname (10, PSImode) = "rr10";
  regname (12, PSImode) = "rr12";
  regname (14, PSImode) = "rr14";

  if (TARGET_BIG)
    {
      /* We need r11 and r15 for big mode */
      fixed_regs[15] = 1;
      fixed_regs[11] = 1;
      call_used_regs[11] = 1;
    }
  else
    {
      /* for little mode we use 15 as the sp */
      renumber[15]  = 14;
      renumber[14]  = 15;
    }


  /* The frame pointer is odd too.  When in STD_FRAME mode we keep the
     frame pointer inside GCC in r10, but we print it out as rr12 or r14.. */

  if (TARGET_STD_FRAME)
    {
      if (TARGET_BIG) {
	renumber[10] = 12;
	renumber[11] = 13;
	renumber[12] = 10;
	renumber[13] = 11;
      }
      else {
	/* So the internal fp at 10 comes out in r14,
	   the interal sp comes out at 15
	   and r15 is printed as r10 */
	renumber[FRAME_POINTER_REGNUM] = 14;
	renumber[STACK_POINTER_REGNUM] = 15;
	renumber[15] = 10;
      }
    }

  regname (0, DImode) = "rq0";
  regname (4, DImode) = "rq4";
  regname (8, DImode) = "rq8";
  regname (12, DImode) = "rq12";


  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    pointer_reg[i] = regname (i, Pmode);

  if (TARGET_DEFS || TARGET_SOURCE || TARGET_LINE)
    {
      write_symbols = SDB_DEBUG;
      debug_info_level = DINFO_LEVEL_NORMAL;
    }

  if (TARGET_PIC)
    {
      fixed_regs[13] = 1;
    }
  flag_force_mem = 0;
  flag_caller_saves = 0;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    rev_renumber[renumber[i]] = i;

}


/*
  Offset from the stack pointer register to the first location at
  which outgoing arguments are placed.  If not specified, the
  default value of zero is used.  This is the proper value for most
  machines.
  */



rtx
z8k_function_arg (cum, mode, type, named)
     int cum;
     enum machine_mode mode;
     tree type;
     int named;
{
  int t;
  int nregs;
  int rn;
  enum machine_mode rmode = ((mode == BLKmode) ? TYPE_MODE (type) : mode);

  if (mode == BLKmode || mode == VOIDmode)
    return 0;
  else
    nregs = (GET_MODE_SIZE (mode) + 1) / 2;

  if (!TARGET_REGPARMS)
    {
      /* Might not be allowed to do this */
      return 0;
    }

  /* Varargs always go on stack, unless in YASM mode*/
  if (!TARGET_YASM)
    {
      if (!named)
	return 0;
    }

  /* Never put a struct in regs - one reason is that calls.c doesn't
     know if you load your regs backwards or forwards.  We could fix
     this by printing r0 as r15, r1 as r14 etc
     */

  /* Move down till we have used enough regs and
     we're aligned */

  rn = cum - (nregs - 1);
  while (rn >= 2
	 && !HARD_REGNO_MODE_OK (rn, rmode))
    {
      rn--;
    }


  if (rn < 2 || rn > 7)
    return 0;
  return gen_rtx (REG, rmode, rn);
}

/* return 1 if there isn't anything tricky to do */

int
null_epilogue ()
{
  int i;

  if (!reload_completed)
    return 0;
  if (frame_pointer_needed)
    return 0;
  if (get_frame_size ())
    return 0;
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (regs_ever_live[i] && !call_used_regs[i])
	return 0;
    }

  return 1;
}

struct rtx_def *
z8k_builtin_saveregs (arglist)
     tree arglist;
{
  int i;

  for (i = 0; i < arg_nregs; i++)
    {
      emit_insn (gen_rtx (USE, VOIDmode, gen_rtx (REG, HImode, arg_regs_order[i])));
    }
  return 0;
}

void
asm_output_local (file, name, size, rounded)
     FILE *file;
     char *name;
     int size;
     int rounded;
{
  if (TARGET_YASM)
    {
      tree name_tree = get_identifier (name);
      TREE_ASM_WRITTEN (name_tree) = 1;
    }

  data_section ();
  assemble_name (file, name);
  fprintf (file, ":\n\tblock\t%u\n", rounded);
}

void
asm_output_common (file, name, size, rounded)
     FILE *file;
     char *name;
     int size;
     int rounded;
{
  if (TARGET_YASM)
    {
      data_section();
      if (size > 1)
      {
          ASM_OUTPUT_ALIGN (file, 1);
      }
      ASM_GLOBALIZE_LABEL (file, name);
      ASM_OUTPUT_LOCAL (file, name, size, rounded);
    }
  else
    {
      fputs (".comm ", file);
      assemble_name (file, name);
      fprintf (file, ",%u\n", rounded);
    }
}

void
asm_output_name (file, name)
     FILE *file;
     char *name;
{
  if (TARGET_YASM)
    {
      tree name_tree = get_identifier (name);
      TREE_ASM_WRITTEN (name_tree) = 1;
    }

  ASM_OUTPUT_LABEL (asm_out_file, name);
}

/*
  emit instructions to move operands[1] to operands[0].

  Return 1 if we've done everything that needs to be done, otherwise
  return 0 and let the compiler emit a move instruction using possibly
  altered operands.
*/
extern rtx copy_to_mode_reg ();
rtx
simple (mode, operand)
     enum machine_mode mode;
     rtx operand;
{

  if (mode == DFmode
      && GET_CODE (operand) == MEM &&
      GET_CODE (XEXP (operand, 0)) == PLUS)
    {
      /* Operand 0 of the PLUS must be a register, so we can force operand 1
	 into a new pseudo reg, and then safely add operand 0 to it.  */
      rtx x = XEXP (operand, 0);
      rtx t1 = XEXP (x, 0);
      rtx t2 = copy_to_mode_reg (Pmode, XEXP (x, 1));
      emit_insn (gen_rtx (SET, VOIDmode, t2,
			  gen_rtx (PLUS, Pmode, t2, t1)));
      return gen_rtx (MEM, mode, t2);
    }

  return operand;
}

int
emit_move (operands, mode, extra)
     rtx operands[];
     enum machine_mode mode;
     int extra;

{
  extern int reload_in_progress;
  rtx operand0 = operands[0];
  rtx operand1 = operands[1];
  int need_copy = 0;

  if (rtx_equal_p (operand0, operand1))
    return 1;
  if (!reload_in_progress)
    {
      /* Can't push a long immediate */
      if (mode == SImode
	  && push_operand (operand0, mode)
	  && immediate_operand (operand1, mode))
	{
	  need_copy = 1;
	}

      if (mode == SFmode
	  && immediate_operand (operand1, mode))
	{
	  need_copy = 1;
	}

      if (push_operand (operand0, mode))
	{
	  need_copy =
	    !ir_operand (operand1, mode)
	    && !da_operand (operand1, mode)
	    && !x_operand (operand1, mode);
	  {
	    need_copy = 1;
	  }
	}
      if (!register_operand (operand0, mode)
	  && !register_operand (operand1, mode))
	{
	  need_copy = 1;
	}

    }
  if (TARGET_BIG && !reload_in_progress)
    {
      operand1 = simple (mode, operand1);
      operand0 = simple (mode, operand0);
    }
  if (need_copy)
    {
      rtx temp = gen_reg_rtx (mode);
      emit_insn (gen_rtx (SET, VOIDmode, temp, operand1));
      emit_insn (gen_rtx (SET, VOIDmode, operand0, temp));
      return 1;
    }
  operands[0] = operand0;
  operands[1] = operand1;
  return 0;
}

int
r_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (BADSUBREG (op))
    return 0;
  return register_operand (op, mode);
}

int
ir_operand_s (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (mode != GET_MODE (op))
    return 0;
  if (BADSUBREG (op))
    return 0;
  return IR_P (op);
}

int
ir_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ir_operand_s (op, mode);
}

int
da_operand_s (op, mode, strict)
     rtx op;
     enum machine_mode mode;
     int strict;
{
  if (mode != GET_MODE (op))
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  return DA_P (op);
}

int
da_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return da_operand_s (op, mode, reload_in_progress);
}

/*  16 bit Reg  + pointer sized bit index*/

x_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (BADSUBREG (op))
    return 0;
  if (mode != GET_MODE (op))
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  return X_P (op);
}


int
find_reg (op, strict)
     rtx op;
     int strict;
{
  int rn = REGNO (op);
  if (strict && reg_renumber == 0)
    abort ();
  if (strict && rn >= FIRST_PSEUDO_REGISTER)
    return reg_renumber[rn];
  return rn;
}

ok_for_base (op, strict)
     rtx op;
     int strict;
{
  int rn;
  if (GET_CODE (op) != REG)
    return 0;
  if (GET_MODE (op) != Pmode)
    return 0;
  rn = find_reg (op, strict);

  if (reload_in_progress && rn > FIRST_PSEUDO_REGISTER)
    return 1;

  if (strict)
    {
      if (rn <= 0)
	return 0;
      return 1;
    }
  else
    {
      return rn != 0;
    }
}

ok_for_index (op, strict)
     rtx op;
     int strict;
{
  int rn;
  if (GET_CODE (op) != REG)
    return 0;
  if (GET_MODE (op) != HImode
      && GET_MODE (op) != Pmode)
    return 0;
  rn = find_reg (op, strict);
  if (strict)
    {
      if (rn <= 0)
	return 0;
      return 1;
    }
  else
    {
      return rn != 0;
    }
}


inside_bx_p (op, strict)
     rtx op;
{
  if (GET_CODE (op) == PLUS)
    {
      rtx lhs = XEXP (op, 0);
      rtx rhs = XEXP (op, 1);
      if (ok_for_base (rhs, strict))
	{
	  if (GET_CODE (lhs) == TRUNCATE)
	    lhs = XEXP (lhs, 0);
	  if (GET_CODE (lhs) == SIGN_EXTEND)
	    {
	      lhs = XEXP (lhs, 0);
	      if (ok_for_index (lhs, strict))
		return 1;
	    }
	}

      if (ok_for_base (lhs, strict))
	{
	  if (GET_CODE (rhs) == TRUNCATE)
	    rhs = XEXP (rhs, 0);
	  if (GET_CODE (rhs) == SIGN_EXTEND)
	    {
	      rhs = XEXP (rhs, 0);
	      if (ok_for_index (rhs, strict))
		return 1;
	    }
	}
    }
  return 0;
}


/* 16 bit register + pointer sized address */
inside_x_p (op, strict)
     rtx op;

{
  if (strict != 0 && strict != 1)
    abort ();
  if (GET_CODE (op) == PLUS)
    {
      rtx lhs = XEXP (op, 0);
      rtx rhs = XEXP (op, 1);
      if (TARGET_SMALL)
	{
	  if (GET_CODE (lhs) == CONST_INT && ok_for_index (rhs, strict))
	    return 1;

	  if (GET_CODE (rhs) == CONST_INT && ok_for_index (lhs, strict))
	    return 1;
	}

      if (data_ref_p (lhs))
	{

	  if (ok_for_index (rhs, strict))
	    {
	      return 1;
	    }
	}

      if (data_ref_p (rhs))
	{
	  if (ok_for_index (lhs, strict))
	    {
	      return 1;
	    }
	}

    }
  return 0;
}


inside_da_p (op, strict)
     rtx op;
     int strict;
{
  if (DATA_REF_P (op))
    return 1;
  return 0;
}

inside_ba_p (op, strict)
     rtx op;
{
  if (strict != 0 && strict != 1)
    abort ();
  if (GET_CODE (op) == PLUS)
    {
      rtx lhs = XEXP (op, 0);
      rtx rhs = XEXP (op, 1);
      if (ok_for_base (lhs, strict))
	{
	  if (GET_CODE (rhs) == CONST_INT)
	    {
	      if (INTVAL (rhs) & 1)
		return 0;
	      return 1;
	    }
	}
    }
  return 0;
}

bx_p (op, strict)
     rtx op;
{
  if (strict != 0 && strict != 1)
    abort ();
  if (GET_CODE (op) != MEM)
    return 0;
  return inside_bx_p (XEXP (op, 0), strict);
}

ba_p (op, strict)
     rtx op;
{
  if (strict != 0 && strict != 1)
    abort ();
  if (GET_CODE (op) != MEM)
    return 0;
  return inside_ba_p (XEXP (op, 0), strict);
}

x_p (op, strict)
     rtx op;
{
  if (GET_CODE (op) != MEM)
    return 0;
  return inside_x_p (XEXP (op, 0), 0);
}


int
ba_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (BADSUBREG (op))
    return 0;
  if (mode != GET_MODE (op))
    return 0;

  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (ba_p (op, 0))
    return 1;
  return 0;
}

int
bx_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (BADSUBREG (op))
    return 0;

  if (bx_p (op, 0))
    return 1;
  return 0;
}

int
im_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return immediate_operand (op, mode);
}

int
r_ir_da_x_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  int r;

  r = r_operand (op, mode)
    || ir_operand (op, mode)
    || da_operand (op, mode)
    || x_operand (op, mode);
  return r;

}


/* When operating on a DI or DF we'll always
   need to get to another double word.  This makes
   the ir mode not work (since 4(rn) is invalid unless
   bx can also be done).  This works on the Z8002 since
   x mode will suffice */
int
r_ir_da_x_operand_for_di (op, mode)
     rtx op;
     enum machine_mode mode;
{
  int r;

  r = r_operand (op, mode)
    || (TARGET_SMALL && ir_operand (op, mode))
    || da_operand (op, mode)
    || (TARGET_SMALL && x_operand (op, mode));
  return r;

}

int
r_im_ir_da_x_operand_for_di (op, mode)
     rtx op;
     enum machine_mode mode;
{
  int r;

  r = r_operand (op, mode)
    || im_operand (op, mode)
    || (TARGET_SMALL && ir_operand (op, mode))
    || da_operand (op, mode)
    || (TARGET_SMALL && x_operand (op, mode));
  return r;

}



int
r_ir_da_x_ba_operand_for_di (op, mode)
     rtx op;
     enum machine_mode mode;
{
  int r;

  r = r_operand (op, mode)
    || ba_operand (op, mode)
    || ir_operand (op, mode)	/* Note we can use this because ba is here */
    || da_operand (op, mode)
    || x_operand (op, mode);
  return r;

}



int
r_ir_da_x_ba_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  int r;

  r = r_operand (op, mode)
    || ir_operand (op, mode)
    || da_operand (op, mode)
    || x_operand (op, mode)
    || ba_operand (op, mode);
  return r;

}

int
r_im_ir_da_x_ba_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  int r;

  r = r_operand (op, mode)
    || ir_operand (op, mode)
    || da_operand (op, mode)
    || im_operand (op, mode)
    || x_operand (op, mode)
    || ba_operand (op, mode);
  return r;

}

int
r_im_ir_da_x_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  int r;

  r = r_operand (op, mode)
    || im_operand (op, mode)
    || ir_operand (op, mode)
    || da_operand (op, mode)
    || x_operand (op, mode);
  return r;
}


int
r_im_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return r_operand (op, mode) || im_operand (op, mode);
}

int
r_im_ir_da_x_ba_bx_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  int r;

  r = r_operand (op, mode)
    || im_operand (op, mode)
    || ir_operand (op, mode)
    || da_operand (op, mode)
    || x_operand (op, mode)
    || ba_operand (op, mode)
    || bx_operand (op, mode);
  return r;
}

int
r_ir_da_x_ba_bx_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  int r;

  r = r_operand (op, mode)
    || ir_operand (op, mode)
    || da_operand (op, mode)
    || x_operand (op, mode)
    || ba_operand (op, mode)
    || bx_operand (op, mode);
  return r;
}

int
r_ir_da_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  int r;

  r = r_operand (op, mode)
    || ir_operand (op, mode)
    || da_operand (op, mode);
  return r;
}

int
move_check (operands, mode)
     rtx operands[];
     enum machine_mode mode;
{
  return 1;
}

int
register_move_cost (x, y)
     int x;
     int y;
{
  if ((x == QI_REGS) != (y == QI_REGS))
    return 5;

  return 2;
}

int
r_im_ir_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  int r;

  r = r_operand (op, mode)
    || im_operand (op, mode)
    || ir_operand (op, mode);
  return r;
}

int
r_im_ir_da_x_operand_or_r (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (TARGET_BIG) ? r_operand (op, mode) : r_im_ir_da_x_operand (op, mode);
}

int
r_da_x_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return r_operand (op, mode)
    || da_operand (op, mode)
    || x_operand (op, mode);
}

int
r_im_ir_da_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return r_operand (op, mode)
    || im_operand (op, mode)
    || ir_operand (op, mode)
    || da_operand (op, mode);


}

ir_da_x_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ir_operand (op, mode)
    || da_operand (op, mode)
    || x_operand (op, mode);
}

char *
z8k_asm_output_opcode (file, string)
     FILE *file;
     char *string;
{
  if (string[0] == '$')
    {
      can_ba_bx = 1;
      string++;
    }
  else
    {
      can_ba_bx = 0;
    }
  return string;
}


int
data_ref_p_1 (X)
     rtx X;
{
  if (TARGET_PIC)
    return 0;

  return (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF
	  || (GET_CODE (X) == PLUS
	      && GET_CODE (XEXP (X, 0)) == SYMBOL_REF
	      && GET_CODE (XEXP (X, 1)) == CONST_INT));
}

int
data_ref_p (X)
     rtx X;
{
  if (TARGET_PIC)
    return 0;
  if (TARGET_BIG)
    return (DATA_REF_P_1 (X) || (GET_CODE (X) == CONST && DATA_REF_P_1 (XEXP (X, 0))));
  return CONSTANT_P (X);
}



int
disp_p (X)
     rtx X;
{
  return
    ((GET_CODE (X) == CONST_INT && (((unsigned) INTVAL (X) + 0xffff) < 0x1ffff)) || (!TARGET_HUGE && DATA_REF_P (X)));
}


int
ptr_reg (x)
     rtx x;
{
  if (reload_completed || !TARGET_HUGE)
    return 1;
  if (STACK_REGISTER (REGNO (x)))
    return 1;
  return 0;
}


/* Work out the registers which need to be saved, both as a mask and a
   count */

int
calc_live_regs (count)
     int *count;
{
  int reg;
  int live_regs_mask = 0;
  *count = 0;

  for (reg = 0; reg < FIRST_PSEUDO_REGISTER; reg++)
    {
      if (regs_ever_live[reg] && !call_used_regs[reg])
	{
	  (*count)++;
	  live_regs_mask |= (1 << reg);

	  if (reg == FRAME_POINTER_REGNUM && frame_pointer_needed && TARGET_BIG)
	    {
	      /* Count r11 too */
	      (*count)++;
	      reg++;
	      live_regs_mask |= (1 << reg);
	    }
	}
    }
  return live_regs_mask;
}

io (from, to)
{
  int regs_saved;
  int d = calc_live_regs (&regs_saved);
  int total_saved_regs_space = (regs_saved) * 2;
  int total_auto_space = get_frame_size ();
  int pcsize = TARGET_BIG ? 4 : 2;

  if (from == ARG_POINTER_REGNUM && to == FRAME_POINTER_REGNUM)
    {
      return total_saved_regs_space + pcsize;
    }

  if (from == ARG_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    {
      return total_saved_regs_space + total_auto_space + pcsize;
    }

  if (from == FRAME_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    {
      return total_auto_space;
    }

  if (from == RETURN_ADDRESS_POINTER_REGNUM && to == FRAME_POINTER_REGNUM)
    {
      return total_saved_regs_space;
    }

  if (from == RETURN_ADDRESS_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    {
      return total_saved_regs_space + total_auto_space;
    }
}

fualign (direction, x, y)
{

  return (direction > 0 ? ((x + (y - 1)) & -y) : (x & (y - 1)));

}

faa (CUM, MODE, TYPE, NAMED)
     int CUM;
     enum machine_mode MODE;
     tree TYPE;
     int NAMED;
{
  int nrs;
  int align;

  switch (MODE)
    {
    case QImode:
    case HImode:
      nrs = 1;
      break;

    case SFmode:
    case PSImode:
    case SImode:
      nrs = 2;
      break;
    case DFmode:
    case DImode:
      nrs = 4;
      break;
    case VOIDmode:
    case BLKmode:
    default:
      nrs = (int_size_in_bytes (TYPE) + 1) / 2;
      break;
    }

  CUM = CUM - (nrs - 1);
  while (CUM >= 2
	 && !HARD_REGNO_MODE_OK (CUM, MODE))
    {
      CUM--;
    }

  return CUM - 1;

}

/* Stuff taken from m88k.c */

/* Output to FILE the start of the assembler file.  */

struct option
{
  char *string;
  int *variable;
  int on_value;
};

static int
output_option (file, sep, type, name, indent, pos, max)
     FILE *file;
     char *sep;
     char *type;
     char *name;
     char *indent;
     int pos;
     int max;
{
  if (strlen (sep) + strlen (type) + strlen (name) + pos > max)
    {
      fprintf (file, indent);
      return fprintf (file, "%s%s", type, name);
    }
  return pos + fprintf (file, "%s%s%s", sep, type, name);
}

static struct
{
  char *name;
  int value;
}

m_options[] = TARGET_SWITCHES;

static void
output_options (file, f_options, f_len, W_options, W_len,
		pos, max, sep, indent, term)
     FILE *file;
     struct option *f_options;
     struct option *W_options;
     int f_len, W_len;
     int pos;
     int max;
     char *sep;
     char *indent;
     char *term;
{
  register int j;
  extern int flag_traditional;

  if (optimize)
    pos = output_option (file, sep, "-O", "", indent, pos, max);
  if (write_symbols != NO_DEBUG)
    pos = output_option (file, sep, "-g", "", indent, pos, max);
  if (flag_traditional)
    pos = output_option (file, sep, "-traditional", "", indent, pos, max);
  if (profile_flag)
    pos = output_option (file, sep, "-p", "", indent, pos, max);
  if (profile_block_flag)
    pos = output_option (file, sep, "-a", "", indent, pos, max);

  for (j = 0; j < f_len; j++)
    if (*f_options[j].variable == f_options[j].on_value)
      pos = output_option (file, sep, "-f", f_options[j].string,
			   indent, pos, max);

  for (j = 0; j < W_len; j++)
    if (*W_options[j].variable == W_options[j].on_value)
      pos = output_option (file, sep, "-W", W_options[j].string,
			   indent, pos, max);

  for (j = 0; j < sizeof m_options / sizeof m_options[0]; j++)
    if (m_options[j].name[0] != '\0'
	&& m_options[j].value > 0
	&& ((m_options[j].value & target_flags)
	    == m_options[j].value))
      pos = output_option (file, sep, "-m", m_options[j].name,
			   indent, pos, max);



}


asm_file_start (file, f_options, f_len, W_options, W_len)
     FILE *file;
     struct option *f_options;
     struct option *W_options;
     int f_len, W_len;
{
  int i;

  extern int arg_regs_order[];
  extern char *main_input_filename;
  extern int arg_nregs;
  extern char call_used_regs[];
  extern char *input_filename;

  char *quote = TARGET_YASM ? "" : "\"";

  fprintf (file, "!\tGCC  Z8000\n");
  fprintf (file, "!\tCygnus Support\n");
  fprintf (file, "!\tsizeof(size_t)=%d\n", TARGET_BIG ? 4 : 2);

  output_file_directive (file, main_input_filename);
  fprintf (file, "!");
  output_options (file, f_options, f_len, W_options, W_len,
		  0, 75, " ", "\n! ", "\n\n");

  fprintf (file, "\n\n");

  if (TARGET_BIG)
    fprintf (file, "\tsegm\n");
  else
    fprintf (file, "\tunseg\n");

  if (TARGET_YASM)
    {
      fprintf (file, "\trsect   USRROM\n");
      fprintf (file, "\teven\n");
      fprintf (file, "\trsect   USRRAM\n");
      fprintf (file, "\teven\n");
      fprintf (file, "\trsect   USRTXT\n");
      fprintf (file, "\teven\n");
    }

  load_source_file (input_filename);
}

void
asm_file_end (file)
     FILE *file;
{
  if (TARGET_YASM)
    {
      struct extern_list *p;

      for (p = extern_head; p != 0; p = p->next)
	{
	  tree name_tree = get_identifier (p->name);

	  if (!TREE_ASM_WRITTEN (name_tree))
	    {
	      TREE_ASM_WRITTEN (name_tree) = 1;
	      fputs ("\textern\t", file);
	      assemble_name (file, p->name);
	      fputs ("\n", file);
	    }
	}

      fprintf (file, "\tend\n");
    }
}


/*********************************************************************************************
 * Source Program Listing Generation Management Routines                                     *
 *********************************************************************************************/

#ifndef  LINE_LEN
#define  LINE_LEN        	1000
#endif


/* 'line_no' - contains actual source line number starting from 1.
   'line' - points to the actual source line. */
struct source_line_node
{
  struct source_line_node *prev, *next;
  int line_no;
  char *line;
};
typedef struct source_line_node line_node;

/* 'name' - points to the file name.
   'head' - points to the first source line.
   'current_line' - points to the last referenced source line. */
struct source_file_node
  {
    struct source_file_node *next;
    char *name;
    line_node *head, *current_line;
  };
typedef struct source_file_node file_node;

static file_node *file_head = 0, *current_file = 0;

void
add_line (line_no, source_line)
     int line_no;
     char source_line[];
{
  if (file_head)
    {
      line_node *temp_node;

      temp_node = (line_node *) (malloc (sizeof (line_node)));
      temp_node->line_no = line_no;
      temp_node->line = (char *) (malloc (strlen (source_line) + 1));
      strcpy (temp_node->line, source_line);

      if (current_file->head)
	{
	  temp_node->prev = current_file->current_line;
	  temp_node->next = 0;
	  current_file->current_line->next = temp_node;
	  current_file->current_line = temp_node;
	}
      else
	{
	  temp_node->prev = temp_node->next = 0;
	  current_file->head = current_file->current_line = temp_node;
	}
    }
}

void
load_single_file (file_name)
     char *file_name;
{
  FILE *source_fp;
  char source_line[LINE_LEN];
  int ch;
  int line_no = 1;
  int col_no = 0;

  if ((source_fp = fopen (file_name, "r")) == 0)
    {
      char global_file_name[LINE_LEN];
      sprintf (global_file_name, "%s%s", "/usr/include", file_name);
      source_fp = fopen (global_file_name, "r");
      /* If file is pre-processed with cpp not all files can be opened. */
    }

  /* If not found leave current_file->head alone. This is an indication
     that the source file is not found. */
  if (source_fp != 0)
    {
      while ((ch = fgetc (source_fp)) != EOF)
	switch (ch)
	  {
	  case '\n':
	    /* End-of-line character (\n) is added in print_source_line. */
	    source_line[col_no] = '\0';
	    add_line (line_no, source_line);
	    col_no = 0;
	    line_no++;
	    break;
	  default:
	    if (col_no < LINE_LEN - 1)
	      source_line[col_no++] = ch;
	    break;
	  }
      fclose (source_fp);
    }
}


load_source_file (file_name)
     char *file_name;
{
  file_node *temp_node, *tail_node = file_head;

  for (temp_node = file_head; temp_node != 0; tail_node = temp_node, temp_node = temp_node->next)
    if (strcmp (temp_node->name, file_name) == 0)
      {
	current_file = temp_node;
	break;
      }

  if (temp_node == 0)
    {
      temp_node = (file_node *) (malloc (sizeof (file_node)));
      temp_node->next = 0;
      temp_node->name = (char *) (malloc (strlen (file_name) + 1));
      strcpy (temp_node->name, file_name);
      temp_node->head = temp_node->current_line = 0;

      if (file_head)
	tail_node->next = temp_node;
      else
	file_head = temp_node;

      current_file = temp_node;
      load_single_file (file_name);
    }
}

void
print_source_line (file, line_no)
     FILE *file;
     int line_no;
{
  if (TARGET_SOURCE && current_file)
    {

      /* If current_file->head is null it definitely implies that no source file
	 has been found in the working directory. However, if current_line is null
	 while head is not null, it means that no source line has been printed. */
      if (!current_file->head)
	fprintf (file, "! Line number:%d\n", line_no);
      else if (!current_file->current_line)
	{
	  for (current_file->current_line = current_file->head;
	       current_file->current_line
	       && current_file->current_line->line_no != line_no;
	       current_file->current_line = current_file->current_line->next)
	    ;

	  if (current_file->current_line && current_file->current_line->line)
	    fprintf (file, "! %s\n", current_file->current_line->line);
	}
      else if (current_file->current_line->line_no < line_no)
	{
	  for (current_file->current_line = current_file->current_line->next;
	       current_file->current_line
	       && current_file->current_line->line_no <= line_no;
	       current_file->current_line = current_file->current_line->next)
	    {
	      fprintf (file, "! %s\n", current_file->current_line->line);
	      if (current_file->current_line->line_no == line_no)
		break;
	    }
	}
      else
	{
	  for (; current_file->current_line->line_no != line_no;
	       current_file->current_line = current_file->current_line->prev)
	    ;
	  if (current_file->current_line)
	    fprintf (file, "! %s\n", current_file->current_line->line);
	}
    }

  if (TARGET_LINE)
    {
      fprintf (file, ".line %d\n", line_no);
    }
}



/**********************************************************************/

/* Code to generate prologue and epilogue sequences */



rtx
gen_push (operand0, mode)
     rtx operand0;
     enum machine_mode mode;
{
  return gen_rtx (SET, VOIDmode,
		  gen_rtx (MEM, mode, gen_rtx (PRE_DEC, mode,
		    gen_rtx (REG, pmode, STACK_POINTER_REGNUM))), operand0);
}

rtx
gen_pop (operand0, mode)
     rtx operand0;
     enum machine_mode mode;
{
  return gen_rtx (SET, VOIDmode,
		  operand0,
		  gen_rtx (MEM, mode, gen_rtx (POST_INC, mode,
			      gen_rtx (REG, pmode, STACK_POINTER_REGNUM))));
}

void
push (rn, mode)
     int rn;
     enum machine_mode mode;
{
  emit_insn (gen_push (gen_rtx (REG, mode, rn), mode));
}

void
pop (rn, mode)
     enum machine_mode mode;
{
  emit_insn (gen_pop (gen_rtx (REG, mode, rn), mode));
}


/* Generate code to push the regs specified in the mask.  remember
   that the mask is of the internal shape of the regs, not the
   external shape - so go through the renumber vector */

static void
push_regs (mask)
     int mask;
{
  int i;

  int size = 0;
  /* The mask is a bit-for-bit for the internal register numbering system,
     so r10 has the fp in it.  We work out the external register numbering
     so that we can push the registers efficiently */
  int exregs[FIRST_PSEUDO_REGISTER];

  memset (exregs, 0, sizeof (exregs));

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (mask & (1<<i))
	{
	  exregs[renumber[i]] = 1;
	}
    }

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i+=2)
    {
      if (exregs[i] && exregs[i+1])
	{
	  push (rev_renumber[i], SImode);
	}
      else if (exregs[i])
	{
	  push (rev_renumber[i], HImode);
	}
      else if (exregs[i+1])
	{
	  push (rev_renumber[i+1], HImode);
	}
    }

}


static void
pop_regs (mask)
     int mask;
{
  int i;
  int j;

  int size = 0;
  /* The mask is a bit-for-bit for the internal register numbering system,
     so r10 has the fp in it.  We work out the external register numbering
     so that we can push the registers efficiently */
  int exregs[16];

  memset (exregs, 0, sizeof (exregs));

  for (i = 0; i < 16; i++)
    {
      if (mask & (1<<i))
	{
	  exregs[renumber[i]] = 1;
	}
    }

  for (j = 0; j < 16; j+=2)
    {
      i = 14 - j;
      if (exregs[i] && exregs[i+1])
	{
	  pop (rev_renumber[i], SImode);
	}
      else if (exregs[i])
	{
	  pop (rev_renumber[i], HImode);
	}
      else if (exregs[i+1])
	{
	  pop (rev_renumber[i+1], HImode);
	}
    }

}

/* Adjust the stack and return the number of bytes taken to do it */

static void
output_stack_adjust (direction, size)
     int direction;
     int size;
{
  if (size)
    {
      rtx val = GEN_INT (size * direction);
      rtx insn;

      insn = (TARGET_BIG ? gen_addpsi3 : gen_addhi3) (stack_pointer_rtx, stack_pointer_rtx, val);

      emit_insn (insn);
    }
}

void
z8k_expand_prologue ()
{
  int live_regs_mask;
  int d;

  live_regs_mask = calc_live_regs (&d);

  output_stack_adjust (-1, current_function_pretend_args_size);

  if (frame_pointer_needed)
    {
      push_regs (live_regs_mask);
      if (TARGET_BIG)
	emit_insn (gen_movsi (frame_pointer_rtx, stack_pointer_rtx));
      else
	emit_insn (gen_movhi (frame_pointer_rtx, stack_pointer_rtx));
    }
  else
    {
      push_regs (live_regs_mask);
    }

  output_stack_adjust (-1, get_frame_size ());
}

void
z8k_expand_epilogue ()
{
  int live_regs_mask;
  int d;
  int i;
  int need;
  live_regs_mask = calc_live_regs (&d);

  if (frame_pointer_needed)
    {
      if (TARGET_BIG)
	emit_insn (gen_movsi (stack_pointer_rtx, frame_pointer_rtx));
      else
	emit_insn (gen_movhi (stack_pointer_rtx, frame_pointer_rtx));
      need = 0;
    }
  else
    {
      need = get_frame_size ();
    }

  if (live_regs_mask)
    {
      if (need)
	{
	  output_stack_adjust (1, need);
	  need = 0;
	}

      pop_regs (live_regs_mask);
    }
  output_stack_adjust (1, extra_push + need +
		       current_function_pretend_args_size);

  current_function_anonymous_args = 0;
}

int
pop_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) != MEM)
    return 0;

  if (GET_MODE (op) != mode)
    return 0;

  op = XEXP (op, 0);

  if (GET_CODE (op) != POST_INC)
    return 0;

  return XEXP (op, 0) == stack_pointer_rtx;
}

int
smallint_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return GET_CODE (op) == CONST_INT
    && (CONST_OK_FOR_LETTER_P (INTVAL (op), 'J')
	|| CONST_OK_FOR_LETTER_P (INTVAL (op), 'K'));
}


int
address_cost (mode)
     rtx mode;
{
  if (GET_CODE (mode) == PLUS)
    {
      rtx lhs = XEXP (mode, 0);
      rtx rhs = XEXP (mode, 1);
      if (GET_CODE (lhs) == REG
	  && GET_CODE (rhs) == CONST_INT)
	return 10;

      if (GET_CODE (lhs) == MEM)
	return 40;
    }
  return 1;
}

int
power_two_operand (operand, mode)
     rtx operand;
     enum machine_mode mode;
{
  if (GET_CODE (operand) == CONST_INT)
    {
      int i;
      return POWER_OF_2 (INTVAL (operand));
    }
  return 0;
}

COM_POWER_OF_2 (value)
     int value;
{
  return POWER_OF_2 (~(value | (~0xffff)));
}

com_power_two_operand (operand, mode)
     rtx operand;
     enum machine_mode mode;
{

  if (GET_CODE (operand) == CONST_INT)
    return COM_POWER_OF_2 (INTVAL (operand));
  return 0;
}


int
prd_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) != MEM)
    return 0;
  if (GET_MODE (op) != mode)
    return 0;
  op = XEXP (op, 0);
  if (GET_CODE (op) != PRE_DEC)
    return 0;
  return register_operand (XEXP (op, 0), Pmode);
}

int
poi_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) != MEM)
    return 0;
  if (GET_MODE (op) != mode)
    return 0;
  op = XEXP (op, 0);
  if (GET_CODE (op) != POST_INC)
    return 0;
  return register_operand (XEXP (op, 0), Pmode);
}


r_im_ir_da_x_ba_bx_poi_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  int r;

  r = r_operand (op, mode)
    || im_operand (op, mode)
    || ir_operand (op, mode)
    || da_operand (op, mode)
    || x_operand (op, mode)
    || ba_operand (op, mode)
    || bx_operand (op, mode)
    || poi_operand (op, mode);
  return r;
}

int
r_ir_da_x_ba_bx_poi_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  int r;

  r = r_operand (op, mode)
    || ir_operand (op, mode)
    || da_operand (op, mode)
    || x_operand (op, mode)
    || ba_operand (op, mode)
    || bx_operand (op, mode)
    || poi_operand (op, mode);
  return r;
}

int
r_ir_da_x_ba_bx_prd_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  int r;

  r = r_operand (op, mode)
    || ir_operand (op, mode)
    || da_operand (op, mode)
    || x_operand (op, mode)
    || ba_operand (op, mode)
    || bx_operand (op, mode)
    || prd_operand (op, mode);
  return r;
}


rtx
legitimize_address (oldx, mode)
     rtx oldx;
     enum machine_mode mode;
{
#if 0
  if (TARGET_PIC)
    {
      if (GET_CODE (oldx) == SYMBOL_REF
	  || GET_CODE (oldx) == LABEL_REF)
	{
	  rtx ptr = gen_reg_rtx (Pmode);
	  emit_insn (gen_rtx (SET, VOIDmode, ptr,
			      gen_rtx (LO_SUM, pic_reg, oldx)));
	  return gen_rtx (MEM, mode, ptr);
	}
    }
#endif
  return oldx;
}

symbol_ref (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == SYMBOL_REF)
    return 1;
  return 0;
}

not_subreg_register_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == REG);
}

BADSUBREG (op)
     rtx op;
{
  /* Can't subreg someting like subreg:HI (mem:SI (plus: reg reg) )
     cause there's no room to put the extra +1 to get to the low part */


  if (GET_CODE (op) == SUBREG)
    {
      /* Always ok if want the high word */
      rtx inside;

      /* Subreg of a reg is ok too */
      if (GET_CODE (SUBREG_REG (op)) == REG)
	return 0;

      inside = XEXP (op, 0);
      if (GET_CODE (inside) == MEM)
	{

	  /* Can't do paradoxical subregs in memory */
	  if (GET_MODE_SIZE (GET_MODE (op)) > GET_MODE_SIZE (GET_MODE (inside)))
	    return 1;
	  /* Ok if want the low part of a mem(symbol_ref), mem(reg), or mem(reg+k)

	   since they can have indexing added to them */
	  inside = XEXP (inside, 0);

	  if (inside_da_p (inside, 0)
	      || inside_x_p (inside, 0)
	      || inside_ba_p (inside, 0))
	    return 0;
	  return 1;
	  /* If you want the high part (the low addressed bit) then the other modes
	   work too */
	  if (SUBREG_WORD (op) == 0 && 0)
	    {

	      if (inside_bx_p (inside, 0))
		return 0;

	    }



	  return 1;
	}
    }
  return 0;
}

/* Return the register class of a scratch register needed to copy IN into
   or out of a register in CLASS in MODE.  If it can be done directly,
   NO_REGS is returned.  */

enum reg_class
secondary_reload_class (class, mode, in)
     enum reg_class class;
     enum machine_mode mode;
     rtx in;
{
  int regno = -1;
  enum rtx_code code = GET_CODE (in);

  /* Can move all but qis any time */
  if (mode != QImode)
    return NO_REGS;

  /* Can easily move in and out of qi regs */
  if (class == QI_REGS)
    return NO_REGS;
  if (!CONSTANT_P (in))
    {
      regno = true_regnum (in);

      /* A pseudo is the same as memory.  */
      if (regno == -1 || regno >= FIRST_PSEUDO_REGISTER)
	code = MEM;
    }

  /* If between memory we may need QI */

  if (code == MEM)
    return SQI_REGS;

  return NO_REGS;

}

int
ir_da_x_ba_bx_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{

  return ir_operand (op, mode)
    || da_operand (op, mode)
    || x_operand (op, mode)
    || ba_operand (op, mode)
    || bx_operand (op, mode);
}

int
r_da_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return r_operand (op, mode) || da_operand (op, mode);
}


int
immediate15_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return GET_CODE (op) == CONST_INT &&  (!(INTVAL (op) & 0x8000));
}

int
moveok (operands, mode)
     rtx *operands;
     enum machine_mode mode;
{
  if (r_operand (operands[0], mode) && r_ir_da_x_ba_bx_operand (operands[1], mode))
    return 1;
  if (ir_da_x_ba_bx_operand (operands[0], mode) && r_operand (operands[1], mode))
    return 1;
  if (push_operand (operands[0], mode) && r_im_ir_da_x_operand (operands[1], mode))
    return 1;
  if (r_ir_da_x_operand (operands[0], mode) && pop_operand (operands[1], mode))
    return 1;
  if (mode == HImode || mode == QImode)
    if (r_ir_da_x_operand (operands[0], mode) && immediate_operand (operands[1], mode))
      return 1;
  if (mode == SImode || mode == SFmode || mode == PSImode)
    if (r_da_operand (operands[0], mode) && immediate_operand (operands[1], mode))
      return 1;
  return 0;
}


void
asm_output_ascii (file, p, size)
     FILE *file;
     char *p;
     int size;
{
  int i, col;
  if (TARGET_YASM)
    {
      int max_buffer_size = 70;
      fprintf (file, "\tsval\t'");
      for (i = col = 0; i < size; i++, col++)
	{
	  register unsigned int c = (p[i] & 0xff);
	  if (c != ';' && c != '\\' && c != '\'' && c != '\"' && c != '%' && c >= ' ' && c < 0177)
	    putc (c, file);
	  else
	    fprintf (file, "%%%02x", c);
	  /* If line is too long split the line */
	  if (col > max_buffer_size && i < size - 1)
	    {
	      fprintf (file, "'\n\tsval\t'");
	      col = 0;
	    }
	}
      fprintf (file, "\047\n");
    }
  else
    {
      FILE *_hide_asm_out_file = file;
      unsigned char *_hide_p = (unsigned char *) p;
      int _hide_thissize = size;
      {
	FILE *asm_out_file = _hide_asm_out_file;
	unsigned char *p = _hide_p;
	int thissize = _hide_thissize;
	int i;
	fprintf (asm_out_file, "\t.ascii \"");

	for (i = 0; i < thissize; i++)
	  {
	    register int c = p[i];
	    if (c == '\"' || c == '\\')
	      putc ('\\', asm_out_file);
	    if (c >= ' ' && c < 0177)
	      putc (c, asm_out_file);
	    else
	      {
		fprintf (asm_out_file, "\\%o", c);
		/* After an octal-escape, if a digit follows,
		   terminate one string constant and start another.
		   The Vax assembler fails to stop reading the escape
		   after three digits, so this is the only way we
		   can get it to parse the data properly.  */
		if (i < thissize - 1
		    && p[i + 1] >= '0' && p[i + 1] <= '9')
		  fprintf (asm_out_file, "\"\n\t.ascii \"");
	      }
	  }
	fprintf (asm_out_file, "\"\n");
      }
    }
}

/* Keep track of all externs, so that we can output an .extern declaration
   for them at the end of the file, but only if they are not defined in
   this file.  We assume that all names passed to us are in the permanent
   obstack, so that they will be valid at the end of the compilation.  */

void
z8k_output_external (name)
     char *name;
{
  struct extern_list *p;

  p = (struct extern_list *) permalloc ((long) sizeof (struct extern_list));
  p->next = extern_head;
  p->name = name;
  extern_head = p;
}

/* Work out which way a 64bit move should happen -
   least or most significant word first */
static
int 
whichway (dst, src)
     rtx dst;
     rtx src;
{
  if (GET_CODE (dst) == REG)
    {
      int rdst = REGNO (dst);
      rtx dst_msw = gen_rtx (REG, SImode, rdst + 2);
      if (GET_CODE (src) == REG)
	{
	  int rsrc = REGNO (src);
	  if (rdst == rsrc + 2)
	    return -1;
	  else
	    return 1;
	}

      /* if msw of dst is in the src, move the lsw first */
      if (reg_overlap_mentioned_p (dst_msw, src))
	{
	  return 1;
	}
      if (reg_overlap_mentioned_p (dst, src))
	{
	  return -1;
	}


    }

  return 1;
}

/* Return string to move 64 bit operand without trampling arguments. */

char *
output_move64 (dst, src)
     rtx dst;
     rtx src;
{
  if (push_operand (dst, GET_MODE (dst)))
    {
      /* Easy, unless source uses stack pointer */
      if (reg_overlap_mentioned_p (stack_pointer_rtx, src))
	{
	  return "pushl	@%H0,%T1\n\tpushl	@%H0,%T1";
	}
      return "pushl	@%H0,%T1\n\tpushl	@%H0,%S1";
    }
  if (whichway (dst, src) < 0)
    return "$ldl	%T0,%T1\n\t$ldl	%S0,%S1 ! di a";
  else
    return "$ldl	%S0,%S1\n\t$ldl	%T0,%T1 ! di b";
}
