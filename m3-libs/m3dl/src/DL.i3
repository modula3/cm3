(*
   Interface to the dynamic linking loader. 
   See the man pages for reference.

Copyright (C) <2013>  <Peter McKinna>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)


INTERFACE DL;

FROM Ctypes IMPORT char_star, int;

CONST
(* One of the following 2 values must be included in the flag parm to open *)
  RTLD_LAZY = 16_00001;
  RTLD_NOW  = 16_00002;
  
(* Zero or more of the following values maybe ORd into the flag parm to open *)
  RTLD_GLOBAL   = 16_00100;
  RTLD_LOCAL    = 0;
  RTLD_NODELETE = 16_01000;
  RTLD_NOLOAD   = 16_00004;  
  RTLD_DEEPBIND = 16_00008;  

EXCEPTION OpenFail(TEXT);
EXCEPTION SymbolError(TEXT);

(* Safe M3 procedures *)

PROCEDURE Open(fileName : TEXT; flag : INTEGER) : ADDRESS RAISES{OpenFail};
PROCEDURE Error() : TEXT;
PROCEDURE Sym(handle : ADDRESS; symbol : TEXT) : ADDRESS RAISES{SymbolError};
PROCEDURE Close(handle : ADDRESS) : INTEGER;

(* external functions *)

<* EXTERNAL *>
PROCEDURE dlopen(filename : char_star; flag : int) : ADDRESS;

<* EXTERNAL *>
PROCEDURE dlerror() : char_star;

<* EXTERNAL *>
PROCEDURE dlsym(handle : ADDRESS; symbol : char_star) : ADDRESS;

<* EXTERNAL *>
PROCEDURE dlclose(handle : ADDRESS) : int;

END DL.