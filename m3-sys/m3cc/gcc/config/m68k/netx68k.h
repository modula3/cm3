/* CYGNUS LOCAL ??? */
/* Definitions of target machine for GNU compiler.  Vxworks 68000/68020
   version.
   Copyright 1987, 1988, 1992 Free Software Foundation, Inc.

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

/* This comment is here to see if it will keep Sun's cpp from dying.  */

#include "m68k/m68k.h"
#include "aoutos.h"

/* See m68k.h.  2055 means 68020 with 68881, long alignment.  */

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT 2055
#endif

/* Define __HAVE_FPA__ or __HAVE_68881__ in preprocessor,
   according to the -m flags.
   This will control the use of inline 68881 insns in certain macros.
   Also inform the program which CPU this is for.  */

#if TARGET_DEFAULT & 02

/* -m68881 is the default */
#define CPP_SPEC \
"%{!msoft-float:%{mfpa:-D__HAVE_FPA__ }%{!mfpa:-D__HAVE_68881__ }}\
%{!ansi:%{m68000:-Dmc68010}%{mc68000:-Dmc68010}%{!mc68000:%{!m68000:-Dmc68020}}}"

#else
#if TARGET_DEFAULT & 0100

/* -mfpa is the default */
#define CPP_SPEC \
"%{!msoft-float:%{m68881:-D__HAVE_68881__ }%{!m68881:-D__HAVE_FPA__ }}\
%{!ansi:%{m68000:-Dmc68010}%{mc68000:-Dmc68010}%{!mc68000:%{!m68000:-Dmc68020}}}"

#else

/* -msoft-float is the default */
#define CPP_SPEC \
"%{m68881:-D__HAVE_68881__ }%{mfpa:-D__HAVE_FPA__ }\
%{!ansi:%{m68000:-Dmc68010}%{mc68000:-Dmc68010}%{!mc68000:%{!m68000:-Dmc68020}}}"

#endif
#endif

/* Prevent error on `-sun3' and `-target sun3' options.  */

#define CC1_SPEC "%{sun3:} %{target:}"

#define PTRDIFF_TYPE "int"
#define SIZE_TYPE "unsigned int"
#undef WCHAR_TYPE
#define WCHAR_TYPE "short unsigned int"
#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 16

/* -m68000 requires special flags to the assembler.  */

#define ASM_SPEC \
 "%{m68000:-mc68010}%{mc68000:-mc68010}%{!mc68000:%{!m68000:-mc68020}}"

/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-Dmc68000"

/* VxWorks does all the library stuff itself.  */

#define LIB_SPEC ""

/* Provide required defaults for linker -e. */
 
#define LINK_SPEC "%{!nostdlib:%{!r*:%{!e*:-e start}}}"

/* VxWorks provides the functionality of crt0.o and friends itself.  */

#define STARTFILE_SPEC ""

/* Every structure or union's size must be a multiple of 2 bytes.  */

#define STRUCTURE_SIZE_BOUNDARY 32

/* This is BSD, so it wants DBX format.  */

#define DBX_DEBUGGING_INFO

/* Allow folding division by zero.  */
#define REAL_INFINITY

/* This is how to output an assembler line defining a `double' constant.  */

#undef ASM_OUTPUT_DOUBLE
#define ASM_OUTPUT_DOUBLE(FILE,VALUE)					\
  {									\
    if (REAL_VALUE_ISINF (VALUE))					\
      fprintf (FILE, "\t.double 0r%s99e999\n", (VALUE) > 0 ? "" : "-");	\
    else if (target_isnan (VALUE))						\
      {									\
	union { double d; long l[2];} t;				\
	t.d = (VALUE);							\
	fprintf (FILE, "\t.long 0x%lx\n\t.long 0x%lx\n", t.l[0], t.l[1]); \
      }									\
    else								\
      fprintf (FILE, "\t.double 0r%.17g\n", VALUE);			\
  }

/* This is how to output an assembler line defining a `float' constant.  */

#undef ASM_OUTPUT_FLOAT
#define ASM_OUTPUT_FLOAT(FILE,VALUE)					\
  {									\
    if (REAL_VALUE_ISINF (VALUE))					\
      fprintf (FILE, "\t.single 0r%s99e999\n", (VALUE) > 0 ? "" : "-");	\
    else if (target_isnan (VALUE))						\
      {									\
	union { float f; long l;} t;					\
	t.f = (VALUE);							\
	fprintf (FILE, "\t.long 0x%lx\n", t.l);				\
      }									\
    else								\
      fprintf (FILE, "\t.single 0r%.9g\n", VALUE);			\
  }

/* This is how to output an assembler lines defining floating operands.
   There's no way to output a NaN's fraction, so we lose it.  */
  
#undef ASM_OUTPUT_FLOAT_OPERAND
#define ASM_OUTPUT_FLOAT_OPERAND(FILE,VALUE)				\
  (REAL_VALUE_ISINF ((VALUE))						\
   ? asm_fprintf (FILE, "%I0r%s99e999", ((VALUE) > 0 ? "" : "-")) \
   : (VALUE) == -0.0							\
   ? asm_fprintf (FILE, "%I0r-0.0")					\
   : asm_fprintf (FILE, "%I0r%.9g", (VALUE)))

#undef ASM_OUTPUT_DOUBLE_OPERAND
#define ASM_OUTPUT_DOUBLE_OPERAND(FILE,VALUE)				\
  (REAL_VALUE_ISINF ((VALUE))						\
   ? asm_fprintf (FILE, "%I0r%s99e999", ((VALUE) > 0 ? "" : "-")) \
   : (VALUE) == -0.0							\
   ? asm_fprintf (FILE, "%I0r-0.0")					\
   : asm_fprintf (FILE, "%I0r%.17g", (VALUE)))

/* Compile with 32-bit alignment (instead of 16-bit).  */
#define TARGET_LONG (target_flags & 04000)

#undef TARGET_SWITCHES
#define TARGET_SWITCHES  \
  { { "68020", 5},				\
    { "c68020", 5},				\
    { "68881", 2},				\
    { "bitfield", 4},				\
    { "68000", -5},				\
    { "c68000", -5},				\
    { "soft-float", -0102},			\
    { "nobitfield", -4},			\
    { "rtd", 8},				\
    { "nortd", -8},				\
    { "short", 040},				\
    { "noshort", -040},				\
    { "fpa", 0100},				\
    { "nofpa", -0100},				\
    { "sky", 0200},				\
    { "nosky", -0200},				\
    { "68040", 0407},				\
    { "68030", -01400},				\
    { "68030", 7},				\
    { "68040-only", 01000},			\
    { "space", 02000},				\
    { "long", 04000},				\
    { "nolong", -04000},			\
    { "", TARGET_DEFAULT}}
/* TARGET_DEFAULT is defined in sun*.h and isi.h, etc.  */

#undef STACK_BOUNDARY
/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY (TARGET_LONG ? 32 : 16)

#undef FUNCTION_BOUNDARY
/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY (TARGET_LONG ? 32 : 16)

#undef EMPTY_FIELD_BOUNDARY
/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY (TARGET_LONG ? 32 : 16)

#undef BIGGEST_ALIGNMENT
/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT (TARGET_LONG ? 32 : 16)

#undef OVERRIDE_OPTIONS
#define OVERRIDE_OPTIONS		\
{					\
  if (! TARGET_68020 && flag_pic == 2)	\
    error("-fPIC is not currently supported on the 68000 or 68010\n");	\
  if (TARGET_LONG && TARGET_SHORT)	\
    error("it is invalid to specify -mlong and -mshort on the same command line");	\
}

#undef ASM_OUTPUT_ALIGN
/* Without gas, we don't have a way to align to more than a two-byte
   boundary, so do the best we can and don't complain.  */
#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) >= 1)			\
    fprintf (FILE, ((TARGET_LONG) ? "\t.align %d\n" : "\t.even\n"), (LOG));

