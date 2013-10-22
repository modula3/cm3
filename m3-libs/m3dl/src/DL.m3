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

UNSAFE MODULE DL;

IMPORT M3toC;
FROM Ctypes IMPORT char_star;

PROCEDURE Open(fileName : TEXT; flag : INTEGER) : ADDRESS RAISES{OpenFail} =
VAR handle : ADDRESS;
BEGIN
  handle := dlopen(M3toC.CopyTtoS(fileName),flag);
  IF handle = NIL THEN
    RAISE OpenFail(Error());
  END;
  RETURN handle;
END Open;

PROCEDURE Error() : TEXT =
VAR err : char_star;
BEGIN
  err := dlerror();
  IF err # NIL THEN
    RETURN M3toC.CopyStoT(err);
  ELSE
    RETURN NIL;
  END;
END Error;

PROCEDURE Sym(handle : ADDRESS; symbol : TEXT) : ADDRESS RAISES{SymbolError} =
VAR proc : ADDRESS;
  error : TEXT;
BEGIN
  (* call dlerror to clear any existing error *)
  EVAL dlerror();
  proc := dlsym(handle,M3toC.CopyTtoS(symbol));  
  (* the proc value may be a legitimate null so call error again to check failure *)
  error := Error();
  IF error # NIL THEN
    RAISE SymbolError(error);
  END;
  RETURN proc;
END Sym;

PROCEDURE Close(handle : ADDRESS) : INTEGER =
BEGIN
  RETURN dlclose(handle);
END Close;


BEGIN
END DL.