(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE ObLib;
IMPORT SynLocation, ObCommand, ObValue;

(* To set up a library one must provide (1) a name, (2) an array of opcodes,
   (3) an evaluator for the opcodes, (4) optionally, a help routine.
   Then one calls "ObLib.Register(...)" and "ObLib.RegisterHelp(...).
   Look at the end for an example. *)

TYPE

  T = (* To be subtyped. *)
    OBJECT
      name: TEXT;
        (* The name the obliq parser recognizes as the name of the library. *)

      opCodes: REF OpCodes;
        (* The array of opcodes for the operations in this library. *)

    METHODS
      Eval(code: OpCode; arity: OpArity; READONLY args: ObValue.ArgArray; 
        temp: BOOLEAN; location: SynLocation.T): ObValue.Val 
          RAISES {ObValue.Error, ObValue.Exception};
      (* To be overridden; the routine that evaluates opcodes.
         "args" is a 1-based array of ObValue.Val, filled with as
         many good arguments as specified by the arity.
         (arity>=-1 is the arity of the call; arity=code.arity unless 
          code.arity=-2). "location" is to be passed along to error 
         message routines.
	*)
       (*
	 A TRUE temp parameter means that the value returned by this
	 operation is "temporary", that is, it will immediately be passed
	 as a parameter to another operation. A temp optimization
	 is implemented for integers and reals, to recycle their allocated
	 value cells, within the built-in "int", "real", and "math" libraries.
	 IMPORTANT: Any library operations that returns an integer or a
	 real cells must obey these invariants: (A) It is sufficient to 
	 actively ensure cell.temp=FALSE for any int or real result cell. 
         (B) However, it is preferable to satisfy the following: (1) The temp
	 field of an int or real cell that is returned more than once must be 
	 set to FALSE. (2) The temp field of a newly allocated int or real
	 cell that is returned must be set to the temp parameter of Eval. 
	 (3) An int or real cell that is received as an argument has cell.temp
	 already set to FALSE (except for "int", "real", and "math" operations)..
	 (C) In any other situation, the temp parameter may be ignored.
       *)

      Encode(opName: TEXT; VAR(*out*)code: OpCode; 
        location: SynLocation.T): BOOLEAN := EncodeTermOp;
      (* Not to be overridden. *)

    END;

  OpCodes = ARRAY OF OpCode;

  OpCode = (* To be subtyped *)
    OBJECT 
      name: TEXT; 
      (* The name the obliq parser recognizes as the name of the operation. *)

      arity: OpArity; 
      (* "arity" >= 0 for library procedures of "arity" arguments, 
         "arity" = -1 for library constants. 
         "arity" = -2 for "don't check the arity" (internal use).
      *)

      fixity: OpFixity:=OpFixity.Qualified; 
      (* Its fixity (see below). *)
    END;

  OpArity = INTEGER;

  OpFixity = 
    (* Fixity of a library operation. For library "l" and operation "o", 
       the operation can be invoked as follows within obliq: *)
    {Undefined,    (*  *)
     Qualified,    (*  "l_o(...)" *)
     Prefix,       (* "o(...)" or "l_o(...)" *)
     Infix         (* "a1 o a2", "o(a1,a2)", or "l_o(a1,a2)" *)
    };
 
  HelpProc = 
    PROCEDURE(self: ObCommand.T; arg: TEXT; data: REFANY:=NIL);

  Env = 
    BRANDED OBJECT
      library: T;
      rest: Env;
    END;
    (* May contain multiple libraries of the same name, non-contiguously,
       forming a single conceptual library. *)

  VAR libraries: Env;
    (* Registered libraries are consend in front of this list. *)

  PROCEDURE Setup();
  (* To be called before any other use of this module *)

  PROCEDURE Extend(library: T; env: Env): Env;
  (* Add a library to the head of a library environment. The new library 
     is searched before all the existing ones. If a library of the same name 
     already exists in env, then the two libraries are conceptually merged. *)

  PROCEDURE Register(library: T);
  (* Register a library. (Same as libraries:=Extend(library, libraries, 
     allowMerge).) Multiple libraries can be registered under the same name, 
     achieving the effect of a single library with the union of the operations.
     (E.g. one can extend the built-in libraries this way.) *)

  PROCEDURE RegisterHelp(name: TEXT; helpProc: HelpProc);
  (* Register a Help proc under the name "name"; this name does not have
     to be a library name, but usually will be one. *)

  (* Internal use. *)
  PROCEDURE Lookup(name: TEXT; env: Env): Env;
  (* Return the first enviroment cell whose library has the given name,
     or NIL if not found. *)
  PROCEDURE LookupFixity(opName: TEXT; env: Env; VAR (*out*)pkgName: TEXT)
    : OpFixity;
  PROCEDURE EncodeTermOp(self: T; opName: TEXT; 
      VAR(*out*)code: OpCode; location: SynLocation.T): BOOLEAN;
  VAR helpCommandSet: ObCommand.Set;

END ObLib.

(* ================= Example of a user library ================
   A library called "foo" with operations "failure" and "op1"

TYPE
  FooCode =
    {Failure, Op1}

  FooOpCode =
    ObLib.OpCode OBJECT
      code: FooCode;
    END;

  LibraryFoo =
    ObLib.T OBJECT
        (* data for EvalFoo *)
      OVERRIDES
        Eval := EvalFoo;
      END;

  VAR fooException: ObValue.Val;

  PROCEDURE SetupFoo() =
  VAR opCodes: ObLib.OpCodes;
  BEGIN
    opCodes := NEW(REF ObLib.OpCodes, NUMBER(FooCode));
    opCodes^ :=
        ObLib.OpCodes{
          NEW(FooOpCode, name:="failure", arity:=-1, code:=FooCode.Failure);
          NEW(FooOpCode, name:="op1", arity:=1, code:=FooCode.Op1)
        };
    fooException := Obliq.NewException("foo_failure");
    ObLib.Register(NEW(LibraryFoo, name:="foo", opCodes:=opCodes));
  END SetupFoo;

  PROCEDURE EvalFoo(self: LibraryFoo; opCode: ObLib.OpCode;
      READONLY args: ObValue.ArgArray; temp: BOOLEAN; loc: SynLocation.T)
      : ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
    VAR int1: INTEGER;
    BEGIN
      CASE NARROW(opCode, FooOpCode).code OF
      | FooCode.Failure =>
          RETURN fooException;
      | FooCode.Op1 =>
          TYPECASE args[1] OF | ObValue.ValInt(node) => int1:=node.int;
          ELSE ObValue.BadArgType(1, "int", self.name, opCode.name, loc) END;
          IF (int1 < 0) THEN (* Raise an error *)
            ObValue.BadArgVal(1, "non-negative", self.name, opCode.name, loc);
          END;
          IF (int1 > 99) THEN (* Raise an exception *)
            ObValue.RaiseException(fooException, 
              self.name & "_" & opCode.name & ": argument too big", loc);
          END;

          (* Do whatever you want with the argument *)

          RETURN ObValue.valOk;
      ELSE
        ObValue.BadOp(self.name, opCode.name, loc);
      END;
    END EvalFoo;

NOTES:
  How to structure the body of EvalFoo is up to you; this is just an
    example with efficient opcode dispatching. The body could also be a big 
    if-then-else testing the text names of the opcodes.

  In ObValue.BadArgType, "1" is the argument number, and "int" is
    the expected argument type.

  In ObValue.BadArgVal, "1" is the argument number, and "non-negative"
    completes the error message "argument 1 must be ...".

  Results must be legal ObValue.Val (not NIL!). See Obliq.i3
    for convenient value constructors and constants.
*)
