/* Definitions for switches for C++.
   Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000
   Free Software Foundation, Inc.

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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

DEFINE_LANG_NAME ("Modula-3")
     
/* This is the contribution to the `documented_lang_options' array in
   toplev.c for g++.  */

  { "-m3cg-trace-source-line", 
    N_("Trace the set-source-line operations in m3cg.") },
  { "-m3cg-trace-opcodes", 
    N_("Trace the opcodes read by m3cg.") },
  { "-m3cg-trace-procs", 
    N_("Trace the procedure declarations and calls in m3cg.") },
  { "-m3cg-trace-vars", 
    N_("Trace the variable declarations in m3cg.") },
  { "-m3cg-trace-procs", 
    N_("Trace the expression construction in m3cg.") },
  { "-m3cg-trace-types", 
    N_("Trace type declarations in m3cg.") },
  { "-m3cg-trace-misc", 
    N_("Trace miscellaneous other operations in m3cg.") },
