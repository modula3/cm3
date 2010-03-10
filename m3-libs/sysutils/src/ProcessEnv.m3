(* Copyright 1999-2002 elego Software Solutions GmbH, Berlin, Germany.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

(*---------------------------------------------------------------------------*)
MODULE ProcessEnv;

IMPORT (* EnvFixed AS *) Env, TextTextTbl;


(*---------------------------------------------------------------------------*)
PROCEDURE Current() : T =
  BEGIN
    IF cur = NIL THEN
      cur := NEW(TextTextTbl.Default).init(Env.Count);
      FOR i := 0 TO Env.Count - 1 DO
        VAR name, val : TEXT; BEGIN
          Env.GetNth(i, name, val);
          EVAL cur.put(name, val);
        END;
      END;
    END;
    RETURN cur;
  END Current;

(*---------------------------------------------------------------------------*)
PROCEDURE Copy(env : T) : T =
  VAR
    e := NEW(TextTextTbl.Default).init(env.size());
    iter := env.iterate();
    name, val : TEXT;
  BEGIN
    WHILE iter.next(name, val) DO
      EVAL e.put(name, val);
    END;
    RETURN e;
  END Copy;

(*---------------------------------------------------------------------------*)
PROCEDURE SystemRepr(env : T) : REF ARRAY OF TEXT =
  VAR
    e : REF ARRAY OF TEXT;
    s : INTEGER;
  BEGIN
    IF env = NIL THEN RETURN NIL END;
    s := env.size();
    e := NEW(REF ARRAY OF TEXT, s);
    VAR
      iter := env.iterate();
      i    := 0;
      name :  TEXT;
      val  :  TEXT;
    BEGIN
      WHILE iter.next(name, val) DO
        e[i] := name & "=" & val;
        INC(i);
      END;
    END;
    RETURN e;
  END SystemRepr;

(*---------------------------------------------------------------------------*)
PROCEDURE Names(env : T) : REF ARRAY OF TEXT =
  VAR
    e : REF ARRAY OF TEXT;
    s : INTEGER;
  BEGIN
    IF env = NIL THEN RETURN NIL END;
    s := env.size();
    e := NEW(REF ARRAY OF TEXT, s);
    VAR
      iter := env.iterate();
      i    := 0;
      name :  TEXT;
      val  :  TEXT;
    BEGIN
      WHILE iter.next(name, val) DO
        e[i] := name;
        INC(i);
      END;
    END;
    RETURN e;
  END Names;

(*---------------------------------------------------------------------------*)
PROCEDURE Defined(env : T; name : TEXT) : BOOLEAN =
  VAR val : TEXT;
  BEGIN
    RETURN env.get(name, val);
  END Defined;

(*---------------------------------------------------------------------------*)
PROCEDURE Value(env : T; name : TEXT) : TEXT =
  VAR val : TEXT;
  BEGIN
    IF env.get(name, val) THEN
      RETURN val;
    ELSE
      RETURN NIL;
    END;
  END Value;

(*---------------------------------------------------------------------------*)
PROCEDURE Set(env : T; name : TEXT; value : TEXT) =
  BEGIN
    EVAL env.put(name, value);
  END Set;

(*---------------------------------------------------------------------------*)
PROCEDURE Delete(env : T; name : TEXT) =
  VAR val : TEXT;
  BEGIN
    EVAL env.delete(name, val);
  END Delete;

(*---------------------------------------------------------------------------*)
PROCEDURE Add(env : T; env2 : T) =
  VAR
    iter := env2.iterate();
    name, val : TEXT;
  BEGIN
    WHILE iter.next(name, val) DO
      EVAL env.put(name, val);
    END;
  END Add;

(*---------------------------------------------------------------------------*)
VAR
  cur : T;
BEGIN
  cur := NIL;
END ProcessEnv.
