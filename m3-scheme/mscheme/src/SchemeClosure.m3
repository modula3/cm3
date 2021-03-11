(* $Id$ *)


(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)
MODULE SchemeClosure;
IMPORT SchemeClosureClass;
IMPORT SchemeEnvironment;
FROM Scheme IMPORT Object, E, IsSpecialForm;
FROM SchemeUtils IMPORT Cons, First, Rest, Stringify;
FROM SchemeSymbol IMPORT Symbol;
IMPORT SchemePair, Scheme;
IMPORT SchemeSymbol;
IMPORT Debug;
IMPORT SchemeMacro;

TYPE Pair = SchemePair.T;

REVEAL
  T = SchemeClosureClass.Private BRANDED Brand OBJECT
  OVERRIDES
    init  := Init;
    apply := Apply;
  END;

PROCEDURE Apply(t : T; interp : Scheme.T; args : Object) : Object 
  RAISES { E } =
  VAR dummy : BOOLEAN;
  BEGIN 
    IF interp = NIL THEN
      RAISE E ("Internal error: SchemeClosure.Apply: interp NIL")
    END;
(*
    Debug.Out("SchemeClosure.Apply: t.params=" & Stringify(t.params) & " args=" & Stringify(args) & " t.body=" & Stringify(t.body));
*)
    RETURN interp.eval(t.body, 
                       (* dont think this one can be shared, can it? *)
                       NEW(SchemeEnvironment.Unsafe).init(t.params, args, t.env,
                                                     dummy))
  END Apply;

VAR SYMdefine := Symbol("define");
VAR SYMbegin  := Symbol("begin");

PROCEDURE Init(t             : T; 
               params, body  : Object;
               env           : SchemeEnvironment.Instance;
               bind          : BOOLEAN) : T =

PROCEDURE DoBindings(body          : Pair; VAR wasDefine : BOOLEAN) : Object =

  PROCEDURE IsMacroOrUnknown(sym : SchemeSymbol.T) : BOOLEAN =
    BEGIN
      IF NOT env.haveBinding(sym) THEN 
        RETURN TRUE 
      ELSE
        RETURN ISTYPE(env.lookup(sym),SchemeMacro.T)
      END
    END IsMacroOrUnknown;

  VAR 
    p := body;
    sentinel := NEW(Pair, rest := NIL); 
    r := sentinel;
  BEGIN
    IF NOT bind THEN RETURN body END;

    wasDefine := FALSE;

    WHILE p # NIL DO
      r.rest := NEW(Pair, rest := NIL); r := r.rest;

      TYPECASE p.first OF
        Pair(pair) =>
        VAR dummy : BOOLEAN;
        BEGIN r.first := DoBindings(pair, dummy) END
      | SchemeSymbol.T(sym) =>
        IF   p = body AND (IsSpecialForm(sym) OR 
                           IsMacroOrUnknown(sym)) THEN 
          IF sym = SYMdefine THEN wasDefine := TRUE END;
          RETURN body 
        ELSIF MemberP(sym, params) THEN
          r.first := sym
        ELSE
          TRY
            r.first := env.bind(sym)
          EXCEPT
            E(txt) => 
(*
            Debug.Warning("Trouble binding symbol: " & 
              SchemeSymbol.ToText(sym) & ": " & txt);
*)
            (* we hit this in case we are referencing back to a local
               variable that we havent actually allocated yet, which
               can happen when binding a sequence *)

            RETURN body
          END
        END
      ELSE
        r.first := p.first
      END;

      IF NOT ISTYPE(p.rest,Pair) THEN 
        r.rest := p.rest; EXIT
      ELSE
        p := p.rest
      END

    END;

    RETURN sentinel.rest
  END DoBindings;

  PROCEDURE MapBindings(p : Pair) : Pair =
    VAR
      sentinel := NEW(Pair, rest := NIL);
      r := sentinel;
      wasDefine : BOOLEAN;
    BEGIN
      WHILE p # NIL DO
        r.rest := NEW(Pair, rest := NIL); r := r.rest;
        IF ISTYPE(p.first,Pair) THEN
          r.first := DoBindings(p.first,wasDefine)
        ELSE
          r.first := p.first
        END;

        (* if we saw a define, give up on this... not smart enough
           to track all the changes yet (mutates symbol tables) *)
        IF NOT wasDefine AND ISTYPE(p.rest, Pair) THEN
          p := p.rest
        ELSE
          r.rest := p.rest; EXIT
        END
      END;
      RETURN sentinel.rest
    END MapBindings;

  VAR dummy : BOOLEAN;
  BEGIN
    env.assigned := TRUE;
    t.params := params;
    t.env := env;
    IF body # NIL AND ISTYPE(body, Pair) AND Rest(body) = NIL THEN
      t.body := First(DoBindings(body, dummy))
    ELSIF ISTYPE(body,Pair) THEN
(*      Debug.Out("Before MapBindings: " & Stringify(body));*)
      t.body := Cons(SYMbegin, MapBindings(body));
(*      Debug.Out("After MapBindings: " & Stringify(t.body));*)
    ELSE
      t.body := Cons(SYMbegin, body)
    END;
    RETURN t
  END Init;

PROCEDURE MemberP(obj : SchemeSymbol.T; lst : Object) : BOOLEAN =
  VAR 
    p := lst;
  BEGIN
    WHILE ISTYPE(p,Pair) AND p # NIL DO
      WITH pp = NARROW(p,Pair) DO
        IF obj = pp.first THEN RETURN TRUE END;
        p := pp.rest
      END
    END;
    RETURN obj = p
  END MemberP;

BEGIN END SchemeClosure.
