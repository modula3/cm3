(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

IMPORT RefList, Wr, Fmt, Text;
FROM Stdio IMPORT stdout;
FROM Test IMPORT done;
<*FATAL ANY*>

PROCEDURE PrintList(wr: Wr.T; name: Text.T; subject: RefList.T) RAISES {} =

  PROCEDURE DoIt(l: RefList.T) RAISES {} =
  VAR
    prefix := "";
    f: RefList.T;
  BEGIN
    Wr.PutText(wr, "(");
    f := l;
    WHILE f # NIL DO
      Wr.PutText(wr, prefix);
      prefix := ", ";
      TYPECASE f.head OF
      | NULL =>
      | RefList.T(listFoo) =>
	  DoIt(listFoo);
      | Text.T(textFoo) =>
	  Wr.PutText(wr, textFoo);
      | REF INTEGER(intFoo) =>
	  Wr.PutText(wr, Fmt.Int(intFoo^));
      ELSE
	Wr.PutText(wr, "?");
      END; (* typecase *)
      f := f.tail;
    END; (* while *)
    Wr.PutText(wr, ")");
  END DoIt;

BEGIN
  Wr.PutText(wr, name & " -> ");
  DoIt(subject);
  Wr.PutText(wr, "\n");
END PrintList;

PROCEDURE NewInt (i: INTEGER): REF INTEGER =
  VAR x := NEW (REF INTEGER);
  BEGIN
    x^ := i;
    RETURN x;
  END NewInt;

PROCEDURE Assoc (l: RefList.T;  x: REFANY): RefList.T RAISES {} =
  BEGIN
    WHILE l # NIL DO
      TYPECASE l.head OF
      | NULL => 
      | RefList.T (pair) => IF (pair.head = x) THEN RETURN pair; END;
      ELSE
      END;
      l := l.tail;
    END;
    RETURN NIL;
  END Assoc;

BEGIN
  WITH state = RefList.List2(RefList.List2("blah", NewInt(0)),
      	      	      	  RefList.List2("Blag", NewInt(25))) DO
    PrintList(stdout, "Entire list", state);
    PrintList(stdout, "List.Assoc(..., \"blah\")", Assoc(state, "blah"));
    PrintList(stdout, "List.Assoc(..., \"Blag\")", Assoc(state, "Blag"));
  END; (* with *)

  done ();
END Main.
