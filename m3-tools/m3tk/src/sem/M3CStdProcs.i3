INTERFACE M3CStdProcs;

(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)

IMPORT M3AST_AS;


TYPE
   T = {Inc, Dec, Dispose,
      New,
      Abs, Float, Floor, Ceiling, Round, Trunc, Max, Min,
      Ord, Val, Number, First, Last, TypeCode, Narrow, IsType,
      BitSize, ByteSize, AdrSize, Loophole, Adr, Subarray};
  Proc = [T.Inc..T.Dispose];
  Func = [T.New..T.Subarray];

  ProcFuncSet = SET OF T;
  ProcSet = SET OF Proc;
  FuncSet = SET OF Func;


CONST
  AllProcFunc = ProcFuncSet{FIRST(T)..LAST(T)};
  AllProc = ProcFuncSet{FIRST(Proc)..LAST(Proc)};
  AllFunc = ProcFuncSet{FIRST(Func)..LAST(Func)};

  Unsafe = ProcFuncSet{T.Dispose, T.Loophole, T.Adr};
  Safe = AllProcFunc - Unsafe;

  ResultTypeIsFirstArg = ProcFuncSet{T.New};
  ResultTypeIsSecondArg = ProcFuncSet{T.Val, T.Narrow, T.Loophole, T.Float};
  PolymorphicResult =
      ProcFuncSet{T.Abs, T.Max, T.Min, T.First, T.Last, T.Subarray} +
      ResultTypeIsFirstArg + ResultTypeIsSecondArg;
  NonPolymorphicResult = AllFunc - PolymorphicResult;

  OneParameter =
      ProcFuncSet{T.Dispose, T.Abs, T.Floor, T.Ceiling,
          T.Round, T.Trunc, T.Ord, T.Number, T.First, T.Last, T.TypeCode,
          T.BitSize, T.ByteSize, T.AdrSize, T.Adr};
  TwoParameters =
      ProcFuncSet{T.Max, T.Min, T.Val, T.Narrow, T.IsType, T.Loophole};
  OneOrTwoParameters = ProcFuncSet{T.Inc, T.Dec, T.Float};
  ThreeParameters = ProcFuncSet{T.Subarray};
  VariableParameters = ProcFuncSet{T.New};

  FirstParameterMustBeType = ProcFuncSet{T.New};
  FirstParameterCanBeType = FirstParameterMustBeType +
      ProcFuncSet{T.Number, T.First, T.Last, T.TypeCode,
          T.BitSize, T.ByteSize, T.AdrSize};
  SecondParameterMustBeType = ResultTypeIsSecondArg + ProcFuncSet{T.IsType};
  SecondParameterCanBeType = SecondParameterMustBeType;
  FirstParameterCanBeNormal = AllProcFunc - FirstParameterMustBeType;
  FirstParameterMustBeNormal = AllProcFunc - FirstParameterCanBeType;
  HaveSecondParameter = AllProcFunc - OneParameter;
  SecondParameterCanBeNormal = HaveSecondParameter - SecondParameterMustBeType;
  SecondParameterMustBeNormal = HaveSecondParameter - SecondParameterCanBeType;
  HaveThirdParameter =
      HaveSecondParameter - (TwoParameters + OneOrTwoParameters);

  AllowedInConstantExpressions = AllFunc -
      ProcFuncSet{T.New,T.Adr,T.Loophole,T.TypeCode,
          T.Narrow,T.IsType,T.Subarray};

PROCEDURE IsStandardCall(
    call: M3AST_AS.Call;
    VAR pf: T)
    : BOOLEAN
    RAISES {};
(* is the given call a call of a standard procedure? If so return TRUE and
set 'pf' to indicate which standard procedure is being called. If not return
FALSE and leave 'pf' untouched. *)

PROCEDURE IsStandard(
    id: M3AST_AS.Proc_id;
    VAR pf: T)
    : BOOLEAN
    RAISES {};
(* does the given identifier refer to a standard procedure? If so return TRUE
and set 'pf' to indicate which standard procedure. If not return FALSE and
leave 'pf' untouched. *)

END M3CStdProcs.
