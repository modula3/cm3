(*                                                                           *)
(*  TextUtils.m3                                                             *)
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
(* $Id: TextUtils.m3,v 1.2 2001-09-19 14:07:43 wagner Exp $ *)

MODULE TextUtils;
IMPORT Text;
IMPORT Fmt;
IMPORT TextSet, TextSetDef, TextList;

PROCEDURE CountCharOccurences(in: TEXT; c: CHAR): CARDINAL =
  VAR
    count := 0;
  BEGIN
    FOR i := 0 TO Text.Length(in) - 1 DO
      IF Text.GetChar(in,i) = c THEN
        INC(count);
      END;
    END;
    RETURN count;
  END CountCharOccurences;

PROCEDURE ReplaceChar(in : TEXT; old, new : CHAR) : TEXT =
  VAR
    res := NEW(REF ARRAY OF CHAR, Text.Length(in));
  BEGIN
    FOR i := 0 TO Text.Length(in) - 1 DO
      WITH char = Text.GetChar(in,i) DO
        IF char = old THEN res[i] := new ELSE res[i] := char END
      END
    END;
    RETURN Text.FromChars(res^)
  END ReplaceChar;

PROCEDURE Replace(in, old, new : TEXT) : TEXT =
  VAR 
    startpos := 0;
    nextpos : CARDINAL;
  BEGIN
    WHILE FindSub(in, old, nextpos, startpos) DO
      in := Text.Sub(in, 0, nextpos) & new & 
                 Text.Sub(in, nextpos + Text.Length(old));
      startpos := nextpos + Text.Length(old) - Text.Length(new)
    END;
    RETURN in
  END Replace;

(* find first occurrence of sub in in *)
PROCEDURE FindSub(in, sub : TEXT; VAR pos : CARDINAL; start := 0) : BOOLEAN =
  VAR
    inA := NEW(REF ARRAY OF CHAR, Text.Length(in));
    subA := NEW(REF ARRAY OF CHAR, Text.Length(sub));
  BEGIN
    Text.SetChars(inA^,in);
    Text.SetChars(subA^,sub);
    FOR i := start TO LAST(inA^) - LAST(subA^) DO
      VAR
        success := TRUE;
      BEGIN
        FOR j := 0 TO LAST(subA^) DO
          IF subA[j] # inA[i + j] THEN 
            success := FALSE; 
            EXIT 
          END
        END;
        IF success THEN pos := i; RETURN TRUE END
      END
    END;
    RETURN FALSE
  END FindSub;

PROCEDURE HaveSub(in, sub : TEXT) : BOOLEAN = 
  VAR x : CARDINAL; BEGIN RETURN FindSub(in, sub, x) END HaveSub;


PROCEDURE HavePrefix(in, prefix: TEXT): BOOLEAN =
  BEGIN
    RETURN Text.Equal(Text.Sub(in, 0, Text.Length(prefix)), prefix);
  END HavePrefix;

PROCEDURE HaveSuffix(in, suffix: TEXT): BOOLEAN =
  VAR
    pos := Text.Length(in) - Text.Length(suffix);
  BEGIN
    RETURN pos >= 0 AND Text.Equal(Text.Sub(in, pos), suffix);
  END HaveSuffix;

PROCEDURE RemoveSuffix(in, suffix: TEXT): TEXT =
  VAR
    pos := Text.Length(in) - Text.Length(suffix);
  BEGIN
    <* ASSERT pos >= 0 AND Text.Equal(Text.Sub(in, pos), suffix) *>
    RETURN Text.Sub(in, 0, pos);
  END RemoveSuffix;

PROCEDURE Pluralize(noun : TEXT; n : INTEGER; 
                    ending : TEXT; printNum : BOOLEAN) : TEXT =
  VAR
    res : TEXT;
  BEGIN 
    IF printNum THEN res := Fmt.Int(n) & " " ELSE res := "" END;
    IF n = 1 THEN RETURN res & noun ELSE RETURN res & noun & ending END 
  END Pluralize;

PROCEDURE ListToSet(l : TextList.T) : TextSet.T =
  VAR
    res := NEW(TextSetDef.T).init();
  BEGIN
    WHILE l # NIL DO EVAL res.insert(l.head); l := l.tail END;
    RETURN res
  END ListToSet;
  
PROCEDURE SetToList(set : TextSet.T) : TextList.T =
  VAR
    iter := set.iterate();
    t : TEXT;
    res : TextList.T := NIL;
  BEGIN
    WHILE iter.next(t) DO res := TextList.Cons(t,res) END;
    RETURN res
  END SetToList;

BEGIN END TextUtils.
