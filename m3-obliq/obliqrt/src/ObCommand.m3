(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE ObCommand;
IMPORT Text, ObErr, SynWr;

TYPE 
  List = OBJECT
      first: T;
      rest: List;
    END;

REVEAL
  Set = 
    BRANDED OBJECT 
      list: List;
    END;

  PROCEDURE Setup() =
    BEGIN
    END Setup;

  PROCEDURE NewSet(): Set =
    BEGIN
      RETURN NEW(Set, list:=NIL);
   END NewSet;

  PROCEDURE Insert(command: T; list: List): List =
    BEGIN
      IF list=NIL THEN 
	RETURN NEW(List, first:=command, rest:=list);
      ELSIF Text.Compare(command.sortingName, list.first.sortingName) < 0 THEN
	RETURN NEW(List, first:=command, rest:=list);
      ELSE
	list.rest := Insert(command, list.rest);
	RETURN list;
      END;
  END Insert;

  PROCEDURE Register(set: Set; command: T) =
    BEGIN
      set.list:=Insert(command, set.list);
    END Register;

  PROCEDURE ReRegister(set: Set; name: TEXT; proc: Proc) =
    VAR scan: List;
    BEGIN
      scan := set.list;
      WHILE scan#NIL DO
	IF Text.Equal(name, scan.first.name) THEN
          scan.first.Exec := proc;
        END;
	scan:=scan.rest;
      END;
    END ReRegister;

  PROCEDURE Exec(name: TEXT; arg: TEXT:=NIL; set: Set; data: REFANY:=NIL) 
      RAISES {ObErr.Fail} =
    VAR scan: List;
    BEGIN
      IF Text.Equal(name, "?") THEN
	scan := set.list;
	WHILE scan#NIL DO
	  IF scan.first.Exec # NIL THEN 
            scan.first.Exec(scan.first, "!", data);
          END;
	  scan:=scan.rest;
	END;
	SynWr.Flush(SynWr.out);
      ELSE
        scan:=set.list;
	WHILE scan#NIL DO
	  IF Text.Equal(name, scan.first.name) THEN
	    IF scan.first.Exec # NIL THEN 
              scan.first.Exec(scan.first, arg, data);
            END;
	    SynWr.Flush(SynWr.out);
	    RETURN;
	  END;
	  scan:=scan.rest;
	END;
        SynWr.Text(SynWr.out, "Command not found: " & name & "\n");
	SynWr.Flush(SynWr.out);
      END;
    END Exec;

BEGIN

END ObCommand.
