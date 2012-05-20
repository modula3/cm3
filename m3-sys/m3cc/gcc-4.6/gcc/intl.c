/* Modula-3: modified */

/* Message translation utilities.
   Copyright (C) 2001, 2003, 2004, 2005, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "intl.h"

EXTERN_C_START

const char *
fake_ngettext (const char *singular, const char *plural, unsigned long n)
{
  if (n == 1UL)
    return singular;

  return plural;
}

/* Return the indent for successive lines, using the width of
   the STR.  STR must have been translated already.  The string
   must be freed by the caller.  */

char *
get_spaces (const char *str)
{
   size_t len = gcc_gettext_width (str);
   char *spaces = XNEWVEC(char, len + 1);
   memset (spaces, ' ', len);
   spaces[len] = '\0';
   return spaces;
}

EXTERN_C_END
