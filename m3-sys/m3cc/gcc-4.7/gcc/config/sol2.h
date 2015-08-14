/* Modula-3: modified */

/* Operating system specific defines to be used when targeting GCC for any
   Solaris 2 system.
   Copyright 2002, 2003, 2004, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* We are compiling for Solaris 2 now.  */
#define TARGET_SOLARIS 1

#ifdef USE_GLD
/* Solaris 11 build 135+ implements dl_iterate_phdr.  GNU ld needs
   --eh-frame-hdr to create the required .eh_frame_hdr sections.  */
#if defined(HAVE_LD_EH_FRAME_HDR) && defined(TARGET_DL_ITERATE_PHDR)
#define LINK_EH_SPEC "%{!static:--eh-frame-hdr} "
#endif /* HAVE_LD_EH_FRAME && TARGET_DL_ITERATE_PHDR */
#endif

#ifndef USE_GLD
/* The default MFLIB_SPEC is GNU ld specific.  */
#define MFLIB_SPEC ""
#endif

/* collect2.c can only parse GNU nm -n output.  Solaris nm needs -png to
   produce the same format.  */
#define NM_FLAGS "-png"

#define STDC_0_IN_SYSTEM_HEADERS 1

/* Support Solaris-specific format checking for cmn_err.  */
#define TARGET_N_FORMAT_TYPES 1
#define TARGET_FORMAT_TYPES solaris_format_types

/* #pragma init and #pragma fini are implemented on top of init and
   fini attributes.  */
#define SOLARIS_ATTRIBUTE_TABLE						\
  { "init",      0, 0, true,  false,  false, NULL, false },		\
  { "fini",      0, 0, true,  false,  false, NULL, false }

/* Solaris-specific #pragmas are implemented on top of attributes.  Hook in
   the bits from config/sol2.c.  */
#define SUBTARGET_INSERT_ATTRIBUTES solaris_insert_attributes
#define SUBTARGET_ATTRIBUTE_TABLE SOLARIS_ATTRIBUTE_TABLE

/* Allow macro expansion in #pragma pack.  */
#define HANDLE_PRAGMA_PACK_WITH_EXPANSION

#define TARGET_CXX_DECL_MANGLING_CONTEXT solaris_cxx_decl_mangling_context

/* Solaris/x86 as and gas support unquoted section names.  */
#define SECTION_NAME_FORMAT	"%s"

/* This is how to declare the size of a function.  For Solaris, we output
   any .init or .fini entries here.  */
#undef ASM_DECLARE_FUNCTION_SIZE
#define ASM_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL)		\
  do								\
    {								\
      if (!flag_inhibit_size_directive)				\
	ASM_OUTPUT_MEASURED_SIZE (FILE, FNAME);			\
      solaris_output_init_fini (FILE, DECL);			\
    }								\
  while (0)

/* Solaris as has a bug: a .common directive in .tbss or .tdata section
   behaves as .tls_common rather than normal non-TLS .common.  */
#undef  ASM_OUTPUT_ALIGNED_COMMON
#define ASM_OUTPUT_ALIGNED_COMMON(FILE, NAME, SIZE, ALIGN)		\
  do									\
    {									\
      if (TARGET_SUN_TLS						\
	  && in_section							\
	  && ((in_section->common.flags & SECTION_TLS) == SECTION_TLS))	\
	switch_to_section (bss_section);				\
      fprintf ((FILE), "%s", COMMON_ASM_OP);				\
      assemble_name ((FILE), (NAME));					\
      fprintf ((FILE), ","HOST_WIDE_INT_PRINT_UNSIGNED",%u\n",		\
	       (SIZE), (ALIGN) / BITS_PER_UNIT);			\
    }									\
  while (0)

#ifndef USE_GAS
#undef TARGET_ASM_ASSEMBLE_VISIBILITY
#define TARGET_ASM_ASSEMBLE_VISIBILITY solaris_assemble_visibility

#define AS_NEEDS_DASH_FOR_PIPED_INPUT

/* The Solaris assembler cannot grok .stabd directives.  */
#undef NO_DBX_BNSYM_ENSYM
#define NO_DBX_BNSYM_ENSYM 1
#endif

#ifndef USE_GLD
/* The Solaris linker doesn't understand constructor priorities.  */
#undef SUPPORTS_INIT_PRIORITY
#define SUPPORTS_INIT_PRIORITY 0
#endif

/* Solaris has an implementation of __enable_execute_stack.  */
#define HAVE_ENABLE_EXECUTE_STACK

/* Static stack checking is supported by means of probes.  */
#define STACK_CHECK_STATIC_BUILTIN 1

#define TARGET_POSIX_IO

extern GTY(()) tree solaris_pending_aligns;
extern GTY(()) tree solaris_pending_inits;
extern GTY(()) tree solaris_pending_finis;
