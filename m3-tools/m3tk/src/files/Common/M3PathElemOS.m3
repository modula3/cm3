(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3PathElemOS;

IMPORT Text, TextExtras, M3PathElem, M3PathElemList;
FROM M3PathElemOSPriv IMPORT SCurrentS, SParentS, PathnameSeparator,
                             PathSeparator;

PROCEDURE RemoveParentDenotations(t: TEXT): TEXT=
  VAR a, b: CARDINAL := 0; spsl := Text.Length(SParentS);
  BEGIN
    LOOP
      IF TextExtras.FindSub(t, SParentS, b) THEN
        IF FindPreDirSepChar(t, b, a) THEN
      	  t := TextExtras.Extract(t, 0, a) & (* includes separator *)
	       TextExtras.Extract(t, b+spsl, Text.Length(t));
          (* step 'b', but 't' has shrunk! *)
	  b := a-1;
	ELSE
	  INC(b, spsl);
        END; 
      ELSE
      	RETURN t;
      END; (* if *)
    END; (* loop *)
  END RemoveParentDenotations;

PROCEDURE FindPreDirSepChar(t: TEXT; b: CARDINAL; 
    VAR (*out*) a: CARDINAL): BOOLEAN RAISES {}=
  BEGIN
    (* t[b] begins "SParentS", look back for preceding separator. *)
    a := b;
    WHILE a > 0 DO
      IF Text.GetChar(t, a-1) = PathnameSeparator THEN
        VAR n := TextExtras.Extract(t, a-1, b+1);
	BEGIN
          IF Text.Equal(n, SCurrentS) OR Text.Equal(n, SParentS) THEN
            RETURN FALSE
	  ELSE
	    RETURN TRUE;
	  END; (* if *)
	END;
      END;
      DEC(a);
    END; (* while *)
    RETURN FALSE;
  END FindPreDirSepChar;

PROCEDURE DecomposePath(t: TEXT; readOnly := FALSE): M3PathElemList.T=
  VAR
    index, sindex: CARDINAL := 0;
    l := Text.Length(t);
    name: TEXT;
    result: M3PathElemList.T := NIL;
  BEGIN
    WHILE index < l DO
      IF NOT TextExtras.FindChar(t, PathSeparator, index) THEN
        index := l;
      END; (* if *)
      name := TextExtras.Extract(t, sindex, index);
      result := M3PathElemList.AppendD(result, M3PathElemList.List1(
                    M3PathElem.FromText(EnvExpand(name), name, readOnly)));
      sindex := index+1; index := sindex;
    END; (* while *)
    RETURN result;
  END DecomposePath;

BEGIN
END M3PathElemOS.
