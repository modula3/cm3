(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3PathFile;

IMPORT Rd, OSError, FileRd, Text, RdExtras, ASCII, Pathname, TextList;
IMPORT M3PathElem, M3PathElemList, M3PathElemOS;

PROCEDURE Read(
    dir := M3PathElem.CurrentDir;
    name := FileName;
    doTransitiveClosure := TRUE)
    : M3PathElemList.T
    RAISES {OSError.E, Rd.Failure}=
  VAR
    result: M3PathElemList.T := NIL;
  BEGIN
    IF NOT doTransitiveClosure THEN result := ReadOneDir(dir, name);
    ELSE result :=  ReadAllDirs(dir, name);
    END;
    RETURN result;
  END Read;

PROCEDURE ReadOneDir(
    dir := "";
    name := FileName)
    : M3PathElemList.T
    RAISES {OSError.E, Rd.Failure}=
  VAR
    result: M3PathElemList.T := NIL;
  BEGIN
    AddOneDir("", Concat(dir, name), result);
    RETURN result;
  END ReadOneDir;

(*PRIVATE*)
PROCEDURE AddOneDir(
    relativeTo := "";
    m3pathName := FileName;
    VAR result: M3PathElemList.T)
    RAISES {OSError.E, Rd.Failure}=
  VAR
    s: Rd.T;
    readOnly := FALSE;
    name: TEXT;
  BEGIN
    TRY
      s := FileRd.Open(m3pathName);
    EXCEPT
    | OSError.E =>  RETURN (* ignore if cannot be opened *)
    END;
      
    TRY (*finally close *)
      LOOP
        TRY
	  name := ReadName(s, readOnly);
	  AddUniqueName(relativeTo, name, readOnly, list := result);
        EXCEPT Rd.EndOfFile => EXIT;
        END;
      END; (*loop through file *)
    FINALLY Rd.Close(s);
    END;
  END AddOneDir;

(*PRIVATE*)
PROCEDURE ReadName(
    s: Rd.T;
    VAR (*out*) readOnly: BOOLEAN)
    : Text.T
    RAISES {Rd.Failure, Rd.EndOfFile} =
  CONST SpaceOrTab = SET OF CHAR{' ', '\t'};
  BEGIN
    readOnly := FALSE;
    TRY
      WHILE RdExtras.Skip(s, SpaceOrTab)
            IN SET OF CHAR{'#'} + SET OF CHAR{'\n', '\r', '\f'} DO
        FlushLine(s);
      END;
      VAR result := RdExtras.GetText(s);
      BEGIN
      	IF ASCII.Upper[RdExtras.Skip(s, SpaceOrTab)] = 'R' THEN
	  readOnly := TRUE;
	END; (* if *)
        RETURN result;
      END;
    FINALLY FlushLine(s);
    END;
  END ReadName;

(*PRIVATE*)
PROCEDURE ReadAllDirs(
    dir, m3PathName: Text.T)
    : M3PathElemList.T
    RAISES {Rd.Failure, OSError.E}=
  VAR
    initialList: M3PathElemList.T := NIL;
  BEGIN
    AddOneDir("", Concat(dir, m3PathName), initialList);
    DirWalk(dir, m3PathName, initialList);
    RETURN initialList;
  END ReadAllDirs;

(*PRIVATE*)
PROCEDURE DirWalk(
    relativeTo: Text.T;
    m3PathName: Text.T;
    VAR listSoFar: M3PathElemList.T)
    RAISES {OSError.E, Rd.Failure}=
  VAR
    temp := listSoFar;
  BEGIN
    WHILE temp # NIL DO
      WITH t = temp.head.text() DO
        AddOneDir(t, Concat(relativeTo, Concat(t, m3PathName)), listSoFar);
        temp := temp.tail;
      END;
    END; (*while*)
  END DirWalk;


(*PRIVATE*)
PROCEDURE Concat(head, tail: Text.T): Text.T RAISES {} =
  BEGIN
    IF IsLocalDir(tail) THEN RETURN head;
    ELSIF IsLocalDir(head) THEN RETURN tail;
    ELSE RETURN M3PathElemOS.RemoveParentDenotations(
                    Pathname.Join(head, tail, NIL));
    END;
  END Concat;

PROCEDURE IsLocalDir(dir: Text.T): BOOLEAN RAISES {} =
  BEGIN
    RETURN (Text.Length(dir) = 0) OR Text.Equal(dir, Pathname.Current);
  END IsLocalDir;

(*PRIVATE*)
PROCEDURE AddUniqueName(
    dir, name: Text.T;
    readOnly := FALSE;
    VAR list: M3PathElemList.T)
    RAISES {OSError.E} =
  VAR
    expName := M3PathElemOS.EnvExpand(name); (* full name, expanded *)
    unexpName: Text.T; (* full name, unexpanded *)
  BEGIN
    IF Pathname.Absolute(expName) THEN
      unexpName := name;
    ELSE
      (* concat dir & name, both expanded and not *)
      unexpName := Concat(dir, name);
      expName := Concat(dir, expName);
    END;
    VAR t := list;
        elem := M3PathElem.FromText(expName, unexpName, readOnly);
    BEGIN
      WHILE t # NIL DO
        IF t.head = elem THEN RETURN
        ELSE t := t.tail;
        END
      END;
      list := M3PathElemList.AppendD(list, M3PathElemList.List1(elem));
    END
  END AddUniqueName;

(*PRIVATE*)
PROCEDURE FlushLine(s: Rd.T) RAISES {Rd.Failure, Rd.EndOfFile}=
  BEGIN
    EVAL RdExtras.Skip(s, ASCII.Asciis - ASCII.Set{'\n', '\r', '\f'},
                       unget := FALSE);
  END FlushLine;

BEGIN
END M3PathFile.
