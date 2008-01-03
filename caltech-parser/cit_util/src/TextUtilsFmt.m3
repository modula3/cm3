(*                                                                           *)
(*  TextUtilsFmt.m3                                                          *)
(*                                                                           *)
(*  Some useful text processing routines for the PL1 compiler.               *)
(*                                                                           *)
(*  Copyright (c) 2000 California Institute of Technology                    *)
(*  All rights reserved.                                                     *)
(*  Department of Computer Science                                           *)
(*  Pasadena, CA 91125.                                                      *)
(*                                                                           *)
(*  Author: Mika Nystrom <mika@cs.caltech.edu>                               *)
(*                                                                           *)
(*  Permission to use, copy, modify, and distribute this software            *)
(*  and its documentation for any purpose and without fee is hereby          *)
(*  granted, provided that the above copyright notice appear in all          *)
(*  copies. The California Institute of Technology makes no representations  *)
(*  about the suitability of this software for any purpose. It is            *)
(*  provided "as is" without express or implied warranty. Export of this     *)
(*  software outside of the United States of America may require an          *)
(*  export license.                                                          *)
(*                                                                           *)
(* $Id: TextUtilsFmt.m3,v 1.2 2001-09-19 14:07:43 wagner Exp $ *)

MODULE TextUtilsFmt EXPORTS TextUtils;
IMPORT TextList;
IMPORT Text;

PROCEDURE InfixFormat(sep : TEXT; list : TextList.T; 
                      ignoreNulls : BOOLEAN ) : TEXT =
  VAR
    res := "";
  BEGIN
    IF ignoreNulls THEN list := StripNulls(list) END;
    WHILE list # NIL DO
      res := res & list.head;
      IF list.tail # NIL THEN res := res & sep END;
      list := list.tail
    END;
    RETURN res
  END InfixFormat;

PROCEDURE StripNulls(list : TextList.T) : TextList.T =
  VAR
    res, strip : TextList.T := NIL;
  BEGIN
    WHILE list # NIL DO
      IF NOT Text.Equal("", list.head) THEN 
        strip := TextList.Cons(list.head,strip)
      END;
      list := list.tail
    END;
    WHILE strip # NIL DO
      res := TextList.Cons(strip.head,res);
      strip := strip.tail
    END;
    RETURN res
  END StripNulls;

BEGIN END TextUtilsFmt.
