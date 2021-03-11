(* $Id$ *)

MODULE MagPath;
IMPORT TextList;
IMPORT TextReader;

VAR path : TextList.T := NIL;

PROCEDURE SetFromList(newpath : TextList.T) =
  BEGIN path := newpath END SetFromList;

PROCEDURE Set(pathText : TEXT) =
  VAR
    reader := NEW(TextReader.T).init(pathText);
    element : TEXT;
    tempPath : TextList.T := NIL;
  BEGIN
    WHILE reader.next(" :", element, skipNulls := TRUE) DO
      tempPath := TextList.Cons(element,tempPath)
    END;
    path := TextList.Reverse(tempPath)
  END Set;

(* Not yet implemented *)
PROCEDURE Append(newDir : TEXT) =
  BEGIN
    path := TextList.Cons(newDir, path)
  END Append;

PROCEDURE Get() : TextList.T =
  BEGIN 
    IF path = NIL THEN RETURN TextList.List1(".") ELSE RETURN path END
  END Get;

BEGIN END MagPath.
