/* Definitions for "naked" Motorola 88110 using coff object format files
   and coff debugging info.

   Copyright (C) 1993, Free Software Foundation, Inc.

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

#include "m88k/m88k-coff.h"

/* Default to m88110 unless stated otherwise.  */

#undef CPP_SPEC
#define CPP_SPEC "%{m88000:-D__m88000__} \
	%{!m88000:%{m88100:-D__m88100__}%{!m88100:-D__m88110__}}"

#undef CC1_SPEC
#define CC1_SPEC "%{m88000:-m88000} \
	%{!m88000:%{m88100:-m88100}%{!m88100:-m88110}}"

/* Undo some minor damage from svr3.h. */

#undef READONLY_DATA_SECTION
#undef SELECT_RTX_SECTION
#undef SELECT_SECTION

/* A list of other sections which the compiler might be "in" at any
   given time.  We have constructors and destructors only.  */

#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS in_tdesc, in_ctors, in_dtors

/* A list of extra section function definitions.  */

#define CTORS_SECTION_FUNCTION						\
void									\
ctors_section ()							\
{									\
  if (in_section != in_ctors)						\
    {									\
      fprintf (asm_out_file, "%s\n", CTORS_SECTION_ASM_OP);		\
      in_section = in_ctors;						\
    }									\
}

#define DTORS_SECTION_FUNCTION						\
void									\
dtors_section ()							\
{									\
  if (in_section != in_dtors)						\
    {									\
      fprintf (asm_out_file, "%s\n", DTORS_SECTION_ASM_OP);		\
      in_section = in_dtors;						\
    }									\
}

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS \
  TDESC_SECTION_FUNCTION \
  CTORS_SECTION_FUNCTION \
  DTORS_SECTION_FUNCTION

/* A C statement (sans semicolon) to output an element in the table of
   global constructors.  */
#define ASM_OUTPUT_CONSTRUCTOR(FILE,NAME)				\
  do {									\
    ctors_section ();							\
    fprintf (FILE, "\t%s\t ", INT_ASM_OP);				\
    assemble_name (FILE, NAME);						\
    fprintf (FILE, "\n");						\
  } while (0)

/* A C statement (sans semicolon) to output an element in the table of
   global destructors.  */
#undef ASM_OUTPUT_DESTRUCTOR
#define ASM_OUTPUT_DESTRUCTOR(FILE,NAME)       				\
  do {									\
    dtors_section ();                   				\
    fprintf (FILE, "\t%s\t ", INT_ASM_OP);				\
    assemble_name (FILE, NAME);              				\
    fprintf (FILE, "\n");						\
  } while (0)

#undef DO_GLOBAL_CTORS_BODY                     
#define DO_GLOBAL_CTORS_BODY			\
{						\
  typedef (*pfunc)();				\
  extern pfunc __ctors[];			\
  extern pfunc __ctors_end[];			\
  pfunc *p;					\
  for (p = __ctors_end; p > __ctors; )		\
    {						\
      (*--p)();					\
    }						\
}						

#undef DO_GLOBAL_DTORS_BODY			 
#define DO_GLOBAL_DTORS_BODY                    \
{						\
  typedef (*pfunc)();				\
  extern pfunc __dtors[];			\
  extern pfunc __dtors_end[];			\
  pfunc *p;					\
  for (p = __dtors; p < __dtors_end; p++)	\
    {						\
      (*p)();					\
    }						\
}						 


/* end of m88110-coff.h */
