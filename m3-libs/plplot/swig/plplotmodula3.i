/* 
Copyright 2002 Gary Bishop
Copyright 2002 Alan W. Irwin
This file is part of PLplot.

This file is free software; you can redistribute it and/or modify
it under the terms of the GNU Library General Public License as published by
the Free Software Foundation; version 2 of the License.

This file is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Library General Public License for more details.

You should have received a copy of the GNU Library General Public License
along with the file; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/

/* 
A SWIG interface to PLplot for Modula3. This wrapper does the following:

   1) it strictly provides the C-API with the usual change of not
      requiring lengths for arrays,

   2) it attempts to provide the entire API *excluding* callbacks for
      plcont and plshade(s) (for now).
      
   3) it works both with the single and double-precision versions of the
      PLplot library.

This is known to work with swig-1.3.17 on Linux.

*/
%module PLPlot
%include typemaps.i

%{
#include "plplotP.h"      
%}

#ifdef PL_DOUBLE
typedef double PLFLT;
#else
typedef float PLFLT;
#endif

typedef int PLINT;

/***************************
	A trick for docstrings
****************************/

%define DOC(func, string) 
%wrapper %{#define _doc_ ## func string %}
%enddef

%rename("Init")  plinit;
%rename("Exit")  plend;
%rename("PlotLines")  plline;
%rename("PlotPoints") plpoin;

%typemap("m3intype") (PLINT n, PLFLT *Array) %{READONLY data: ARRAY OF R.T%}
%typemap("m3rawarg") (PLINT n, PLFLT *Array) %{NUMBER(data), data[0]%}

%typemap("m3intype") (PLINT n, PLFLT *Array, PLFLT *ArrayCk) %{READONLY x, y: ARRAY OF R.T%}
%typemap("m3in")     (PLINT n, PLFLT *Array, PLFLT *ArrayCk) %{IF NUMBER(x) # NUMBER(y) THEN RAISE NA.Error(Err.bad_size) END;%}
%typemap("m3rawarg") (PLINT n, PLFLT *Array, PLFLT *ArrayCk) %{NUMBER(x), x[0], y[0]%}

%typemap("m3intype") (PLINT n, PLFLT *Array, PLFLT *ArrayCk, PLFLT *ArrayCk) %{READONLY x, y, z: ARRAY OF R.T%}
%typemap("m3in")     (PLINT n, PLFLT *Array, PLFLT *ArrayCk, PLFLT *ArrayCk) %{IF NUMBER(x) # NUMBER(y) OR NUMBER(x) # NUMBER(z) THEN RAISE NA.Error(Err.bad_size) END;%}
%typemap("m3rawarg") (PLINT n, PLFLT *Array, PLFLT *ArrayCk, PLFLT *ArrayCk) %{NUMBER(x), x[0], y[0], z[0]%}

/* swig compatible PLplot API definitions from here on. */
%include plplotcapi.i
