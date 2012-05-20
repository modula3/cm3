/* Modula-3: modified */

/* Header file for graph routines.
   Copyright (C) 1999, 2003, 2004, 2007 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_GRAPH_H
#define GCC_GRAPH_H

EXTERN_C_START

extern void print_rtl_graph_with_bb (const char *, rtx);
extern void clean_graph_dump_file (const char *);
extern void finish_graph_dump_file (const char *);

EXTERN_C_END

#endif /* ! GCC_GRAPH_H */
