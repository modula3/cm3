(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE ObCheck;
IMPORT ObErr, ObTree;

  REVEAL

    Env =
      ObTree.Env BRANDED OBJECT END;

    TermEnv =
      TermEnvBase BRANDED OBJECT END;

PROCEDURE NewTermEnv(name: ObTree.IdeName; rest: Env): Env =
  VAR env: Env;
  BEGIN
    env := NEW(TermEnv);
    ObTree.BeEnv(env, name, rest);
    RETURN env;
  END NewTermEnv;

  PROCEDURE Setup() =
    BEGIN 
    END Setup;

PROCEDURE CheckTermBindingSeq(binding: ObTree.TermBinding; initEnv, env: Env)
    : Env RAISES {ObErr.Fail} =
  VAR env1: Env;
  BEGIN
    TYPECASE binding OF
    | NULL => RETURN env;
    | ObTree.TermBinding(node) =>
        env1 := initEnv;
	CheckTerm(node.term, (*in-out*)env1);
	RETURN CheckTermBindingSeq(node.rest, initEnv,
	  NewTermEnv(node.binder, env));
    END;
  END CheckTermBindingSeq;

PROCEDURE CheckTermBindingRec1(binding: ObTree.TermBinding; env: Env)
    : Env RAISES {ObErr.Fail} =
  BEGIN
    TYPECASE binding OF
    | NULL => RETURN env;
    | ObTree.TermBinding(node) =>
	RETURN CheckTermBindingRec1(node.rest,
	  NewTermEnv(node.binder, env));
    END;
  END CheckTermBindingRec1;

PROCEDURE CheckTermBindingRec2(binding: ObTree.TermBinding; recEnv: Env)
    RAISES {ObErr.Fail} =
  VAR env1: Env;
  BEGIN
    TYPECASE binding OF
    | NULL =>
    | ObTree.TermBinding(node) =>
        env1 := recEnv;
	CheckTerm(node.term, (*in-out*)env1);
	CheckTermBindingRec2(node.rest, recEnv);
    END;
  END CheckTermBindingRec2;

PROCEDURE CheckTerm(term: ObTree.Term; VAR (*in-out*)env: Env) 
    RAISES {ObErr.Fail} =
  BEGIN
    TYPECASE term OF
    | ObTree.TermLet(node) =>
        IF node.rec THEN
          env := CheckTermBindingRec1(node.binding, env);
          CheckTermBindingRec2(node.binding, env);
        ELSE
          env := CheckTermBindingSeq(node.binding, env, env);
        END;
(*
    | NULL => ObErr.Fault("Check.Term NIL");
    | ObTree.TermIde =>
    | ObTree.TermOk =>
    | ObTree.TermBool =>
    | ObTree.TermChar =>
    | ObTree.TermText =>
    | ObTree.TermInt =>
    | ObTree.TermReal =>
    | ObTree.TermArray =>
    | ObTree.TermOption =>
    | ObTree.TermAlias =>
    | ObTree.TermOp =>
    | ObTree.TermFun =>
    | ObTree.TermAppl =>
    | ObTree.TermMeth =>
    | ObTree.TermObj =>
    | ObTree.TermClone =>
    | ObTree.TermRedirect =>
    | ObTree.TermSelect =>
    | ObTree.TermUpdate =>
    | ObTree.TermSeq =>
    | ObTree.TermAssign =>
    | ObTree.TermIf =>
    | ObTree.TermCase =>
    | ObTree.TermLoop =>
    | ObTree.TermExit =>
    | ObTree.TermFor => (* check binder *)
    | ObTree.TermForeach => (* check binder *)
    | ObTree.TermException =>
    | ObTree.TermRaise =>
    | ObTree.TermTry =>
    | ObTree.TermTryFinally =>
    | ObTree.TermWatch =>
*)
    ELSE 
    END;
  END CheckTerm;

BEGIN
END ObCheck.
