(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(*                                                                           *)
(* Parts Copyright (C) 1997, Columbia University                             *)
(* All rights reserved.                                                      *)
(*
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Aug  4 15:04:09 1997
 *)

MODULE ObCommand;
IMPORT Text, ObErr, SynWr;

TYPE 
  List = OBJECT
      first: T;
      rest: List;
    END;

REVEAL
  Set = 
    BRANDED "ObCommand.Set" OBJECT 
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

  PROCEDURE Exec(wr: SynWr.T; name: TEXT; arg: TEXT:=NIL; 
                 set: Set; data: REFANY:=NIL) 
      RAISES {ObErr.Fail} =
    VAR scan: List;
    BEGIN
      IF Text.Equal(name, "?") THEN
	scan := set.list;
	WHILE scan#NIL DO
	  IF scan.first.Exec # NIL THEN 
            scan.first.Exec(wr, scan.first, "!", data);
          END;
	  scan:=scan.rest;
	END;
	SynWr.Flush(wr);
      ELSE
        scan:=set.list;
	WHILE scan#NIL DO
	  IF Text.Equal(name, scan.first.name) THEN
	    IF scan.first.Exec # NIL THEN 
              scan.first.Exec(wr, scan.first, arg, data);
            END;
	    SynWr.Flush(wr);
	    RETURN;
	  END;
	  scan:=scan.rest;
	END;
        SynWr.Text(wr, "Command not found: " & name & "\n");
	SynWr.Flush(wr);
      END;
    END Exec;

BEGIN
END ObCommand.
