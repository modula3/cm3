/* M3 language support routines for GDB, the GNU debugger.
   Copyright 2006 Free Software Foundation, Inc.

This file is part of GDB.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#if !defined (M3_EXP_H)
#define M3_EXP_H 1

#include "defs.h" 
#include "expression.h" 
#include "parser-defs.h" 
 
extern int m3_parse PARAMS ((void));

extern void m3_print_subexp (
    struct expression *exp, 
    int *pos,
    struct ui_file *stream, 
    enum precedence prec
 );

extern int 
m3_dump_subexp ( struct expression *exp, struct ui_file *stream, int elt );

#endif /* !defined (M3_EXP_H) */

/* End of file m3-exp.h */ 
