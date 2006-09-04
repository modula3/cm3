/* M3 language support routines for GDB, the GNU debugger.
   Copyright 1992, 1993 Free Software Foundation, Inc.

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

/*
   Modula-3 type uids are unsiged 32-bit values.  They are passed as signed
   decimal integers in the intermediate code, but converted to 6-byte, base 62
   strings of characters between the back end and the debugger.  To avoid
   surprises, these  strings are also legal C identifiers.
*/

#include "defs.h"
#include "m3-uid.h"
#include "gdb_string.h"

char *
m3uid_from_int (const int x)
{
  char *res;
  unsigned digit;
  int i;
  int lx = x;

  if (lx == -1) return "zzzzzz";

  res = (char *) malloc (M3UID_LEN + 1);
  res[M3UID_LEN] = 0;
  for (i = M3UID_LEN-1; i >= 0; i--) {
    digit = ((unsigned)lx) % 62;
    lx = ((unsigned)lx) / 62;
    if      (digit < 26) { res[i] = 'A' + digit; }
    else if (digit < 52) { res[i] = 'a' + (digit - 26); }
    else                 { res[i] = '0' + (digit - 52); }
  }

  if ((lx != 0) || (res[0] < 'A') || ('Z' < res[0])) {
    error ("bad uid -> identifier conversion!!"); }

  return res;
}

int
m3uid_to_int (const char *uid, int *val)
{
  int value, digit, i;
  char c;

  *val = 0;
  if (uid == NULL) return 0;

  if (strlen (uid) < M3UID_LEN) return 0;

  if (strcmp (uid, "zzzzzz") == 0) { *val = -1;  return 1; };

  value = 0;
  for (i = 0; i < M3UID_LEN; i++) {
    c = uid[i];
    if      (('A' <= c) && (c <= 'Z')) digit = c - 'A';
    else if (('a' <= c) && (c <= 'z')) digit = c - 'a' + 26;
    else if (('0' <= c) && (c <= '9')) digit = c - '0' + 52;
    else return 0;
    value = value * 62 + digit;
  }
  *val = value;
  return 1;
}
