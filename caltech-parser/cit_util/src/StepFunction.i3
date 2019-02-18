(* $Id$ *)

INTERFACE StepFunction;

(* 
   Copyright (c) 2007,  Generation Capital Ltd.  All rights reserved.

   Author: Mika Nystrom <mika@alum.mit.edu>
*)

IMPORT LongRealPair, LongRealSeq, Wr;

TYPE Err = RECORD f : T; x : LONGREAL END;

EXCEPTION DomainError(Err);

TYPE
  T = OBJECT METHODS
    eval(at : LONGREAL) : LONGREAL RAISES { DomainError } ;
    domain() : LongRealPair.T;
    steps() : LongRealSeq.T;   (* arguments where it changes *)

    iterateSteps() : Iterator;
    
    formatStepsWr(wr : Wr.T) RAISES { Wr.Failure };
  END;

  Iterator <: PubIterator;

  PubIterator = OBJECT METHODS
    next(VAR x : LONGREAL) : BOOLEAN;
  END;

PROCEDURE Add(a, b : T) : T;                    (* a + b *)
PROCEDURE Sub(a, b : T) : T;                    (* a - b *)
PROCEDURE Mul(a, b : T) : T;                    (* a x b *)
PROCEDURE Div(a, b : T) : T;                    (* a / b *)
PROCEDURE ScalarMul(a : LONGREAL; b : T) : T;   (* ab *)
PROCEDURE One(from := FIRST(LONGREAL); (* support *)
              to := LAST(LONGREAL)) : T;        (* 1 *)

TYPE Func = PROCEDURE(a : LONGREAL) : LONGREAL;

PROCEDURE G(f : Func; b : T) : T;               (* f(b) *)
  
PROCEDURE H(e : Evaluator; b : T) : T;          (* e.eval(b) *)

TYPE Evaluator = OBJECT METHODS eval(x : LONGREAL) : LONGREAL; END;
     (* used for Currying *)

CONST Brand = "StepFunction";

TYPE
  Private <: T;

  Default <: PubDefault;

  PubDefault = Private OBJECT METHODS
    init() : Default;
    add(x, y : LONGREAL);
  END;


(**********************************************************************)
(* special operations *)

PROCEDURE ShiftSteps(a : T; by : LONGREAL) : T;

PROCEDURE Project(a : T; on : T) : T;
  (* project a on only the points also in on *)

END StepFunction.
