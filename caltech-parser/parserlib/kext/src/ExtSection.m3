(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

MODULE ExtSection;
IMPORT ExtFormBundle;
IMPORT TextList;
IMPORT TextReader;
IMPORT Bundle;
IMPORT Text;
CONST
  BundleTypes = "tly";
  lastBundle = 2;
  lastSection = ORD(LAST(T));
VAR
  form := ExtFormBundle.Get();
  sections: ARRAY [-1..3] OF ARRAY [0..3] OF TEXT;

PROCEDURE Res(name: TEXT): TEXT =
  BEGIN
    RETURN Bundle.Get(form, name);
  END Res;

PROCEDURE GetSections() =
  VAR
    t: TEXT;
    tl: TextList.T;
  BEGIN
    FOR i := 0 TO lastSection DO sections[-1,i] := NIL;END;
    FOR j := 0 TO lastBundle DO
      t := Bundle.Get(form, "extform." & Text.Sub(BundleTypes, j, 1) & ".m3");
      tl := NEW(TextReader.T).init(t).shatter("~","");
      FOR i := 0 TO lastSection DO
        sections[j,i] := tl.head;
        tl := tl.tail;
      END;
    END;
  END GetSections;

PROCEDURE GetText(kind: CHAR; i: T): TEXT =
  VAR
    j: INTEGER;
  BEGIN
    j := Text.FindChar(BundleTypes, kind);
    RETURN sections[j, ORD(i)];
  END GetText; 

BEGIN
  GetSections();
END ExtSection. 
