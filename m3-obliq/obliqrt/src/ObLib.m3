(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE ObLib;
IMPORT SynLocation, ObCommand, Text;

  PROCEDURE Setup()  =
  BEGIN
    libraries := NIL;
    helpCommandSet := ObCommand.NewSet();
  END Setup;

  PROCEDURE Extend(library: T; env: Env): Env =
    BEGIN
      IF Lookup(library.name, env)#NIL THEN
        RETURN NEW(Env, library:=library, rest:=env);
      ELSE
        RETURN NEW(Env, library:=library, rest:=env);
      END;
    END Extend;

  PROCEDURE Register(library: T) =
    BEGIN
      libraries := Extend(library, libraries);
    END Register;

  PROCEDURE RegisterHelp(name: TEXT; helpProc: HelpProc) =
    BEGIN
      ObCommand.Register(helpCommandSet,
          NEW(ObCommand.T, name:=name, 
              sortingName:="mod " & name,
              Exec:=helpProc));
    END RegisterHelp;

  PROCEDURE Lookup(name: TEXT; env: Env): Env =
    BEGIN
      LOOP
        IF env=NIL THEN RETURN NIL END;
        IF Text.Equal(name, env.library.name) THEN RETURN env END;
        env := env.rest;
      END;
    END Lookup;

  PROCEDURE LookupFixity(opName: TEXT; env: Env; VAR (*out*)libName: TEXT)
      : OpFixity =
    VAR opCodes: REF ARRAY OF OpCode;
    BEGIN
      LOOP
        IF env=NIL THEN RETURN OpFixity.Undefined END;
        opCodes := env.library.opCodes;
        FOR i:=0 TO NUMBER(opCodes^)-1 DO
          IF Text.Equal(opName, opCodes^[i].name) THEN
            libName := env.library.name;
            RETURN opCodes^[i].fixity;
          END;
        END;
        env := env.rest;
      END;
    END LookupFixity;

  PROCEDURE EncodeTermOp(self: T; opName: TEXT; 
      VAR(*out*)code: OpCode; <*UNUSED*>location: SynLocation.T): BOOLEAN =
    BEGIN
      FOR i:=FIRST(self.opCodes^) TO LAST(self.opCodes^) DO
        IF Text.Equal(opName, self.opCodes^[i].name) THEN
          code := self.opCodes^[i];
          RETURN TRUE;
        END;
      END;
      RETURN FALSE;
    END EncodeTermOp;

BEGIN
END ObLib.
