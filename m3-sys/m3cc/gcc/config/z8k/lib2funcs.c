/* Copyright (C) 1994 Free Software Foundation, Inc.

This file is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file with other programs, and to distribute
those programs without any restriction coming from the use of this
file.  (The General Public License restrictions do apply in other
respects; for example, they cover modification of the file, and
distribution when not linked into another program.)

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */


/* This file overrides some functions defined in libgcc2.c, and implements
   simpler forms of the routines.  The z8k is so limited that I can get
   faster code using less smart algorithms.. */


#define divnorm(num, den, sign) 		\
{						\
  if (num < 0) 					\
    {						\
      num = -num;				\
      sign = 1;					\
    }						\
  else 						\
    {						\
      sign = 0;					\
    }						\
						\
  if (den < 0) 					\
    {						\
      den = - den;				\
      sign = 1 - sign;				\
    } 						\
}





static unsigned long 
divmodsi4(int modwanted, unsigned long num, unsigned long den)	
{						
  unsigned long int bit = 1;				
  long int res = 0;				
  long prevden;
  while (den < num && bit && !(den & (1L<<31)))			       
    {
      den <<=1;					
      bit <<=1;					
    }						
  while (bit)
    {					
      if (num >= den)
	{				
	  num -= den;				
	  res |= bit;				
	}						
      bit >>=1;					
      den >>=1;					
    }						
  if (modwanted) return num;
  return res;
}





long 
__umodsi3 (unsigned long numerator, unsigned long denominator)
{
  long dividend;
  long modul;

  modul= divmodsi4 (1,  numerator, denominator);
  return modul;
}

long 
__udivsi3 (unsigned long numerator, unsigned long denominator)
{
  int sign;
  long dividend;
  long modul;
  dividend =   divmodsi4 (0, numerator, denominator);
  return dividend;
}

