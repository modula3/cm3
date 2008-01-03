(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE ObTree;
IMPORT ObCommand, Text, Fmt;

  PROCEDURE SameIdeName(name1, name2: IdeName): BOOLEAN  =
    BEGIN
      RETURN Text.Equal(name1.text, name2.text) AND 
        (name1.variant=name2.variant);
    END SameIdeName;

  PROCEDURE BeEnv(env: Env; name: IdeName; rest: Env)  =
  BEGIN
    env.name := name;
    env.decoration := FreshDecoration(name, rest);
    env.rest := rest;
  END BeEnv;

  PROCEDURE NewEnv(name: IdeName; rest: Env): Env  =
  VAR env: Env;
  BEGIN
    env := NEW(Env);
    BeEnv(env, name, rest);
    RETURN env;
  END NewEnv;

  PROCEDURE ExtendEnv(binders: IdeList; env: Env): Env  =
  BEGIN
    IF binders=NIL THEN RETURN env;
    ELSE RETURN ExtendEnv(binders.rest, NewEnv(binders.first, env));
    END;
  END ExtendEnv;

  PROCEDURE FreshDecoration(name: IdeName; env: Env): INTEGER  =
  BEGIN
    LOOP
      IF env=NIL THEN RETURN 0 END;
      IF Text.Equal(env.name.text, name.text) THEN RETURN env.decoration+1 END;
      env := env.rest;
    END;
  END FreshDecoration;

  PROCEDURE FmtBool(bool: BOOLEAN): TEXT =
  BEGIN
    IF bool 
    THEN RETURN "true";
    ELSE RETURN "false";
    END;    
  END FmtBool;

  PROCEDURE FmtInt(int: INTEGER): TEXT =
  BEGIN
    IF int >=0
    THEN RETURN Fmt.Int(int);
    ELSE RETURN "~" & Fmt.Int(-int);
    END;
  END FmtInt;

  PROCEDURE FmtReal(real: LONGREAL): TEXT =
  VAR r: TEXT;
  BEGIN
    IF real >= 0.0d0 THEN
      r := Fmt.LongReal(real);
    ELSE
      r := Fmt.LongReal(-real);
    END;
    IF (Text.FindChar(r, '.')=-1) AND (Text.FindChar(r, 'e')=-1)
      AND (Text.FindChar(r, 'D')=-1) 
      (* -- 'D' check is temporary, until the reimplementation of Fmt *)
    THEN
      r := r & ".0";
    END;
    IF real >= 0.0d0 
    THEN RETURN r;
    ELSE RETURN "~" & r;
    END;
  END FmtReal;

  PROCEDURE Setup()  =
  BEGIN
    noName := NEW(IdeName, text:="", variant:=-1);
    doCommandSet := ObCommand.NewSet();
  END Setup;

BEGIN
END ObTree.
