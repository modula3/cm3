INTERFACE M3CActualUtil;

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

IMPORT M3AST_LX, M3AST_AS, M3AST_SM;


PROCEDURE Passable(
    formalType: M3AST_SM.TYPE_SPEC_UNSET;
    actual: M3AST_AS.EXP;
    safe: BOOLEAN)
    : BOOLEAN
    RAISES {};
(* Is 'actual' passable to type 'formalType'? 'safe' indicates whether or not
the 'actual' is being passed in a safe module. Not the same as expression
assignable because 'actual' can be a local procedure constant *)

PROCEDURE AddDefault(
    call: M3AST_AS.Call;
    formal: M3AST_AS.Formal_param)
    : BOOLEAN
    RAISES {};
(* Check to see if 'formal' has a default. If it has add the default value to
the end of the 'sm_actual_s' list of 'call' and return TRUE; otherwise return
FALSE *)

PROCEDURE CheckIsVARActual(actual: M3AST_AS.EXP) RAISES {};
(* Check that 'actual' is a writable designator; give an appropriate message
if it is not *)

PROCEDURE TooFewArguments(call: M3AST_AS.Call) RAISES {};
PROCEDURE TooManyArguments(call: M3AST_AS.Call) RAISES {};
PROCEDURE ArgumentIsWrongType(exp: M3AST_AS.EXP) RAISES {};
(* Output appropriate error message *)

(* The following group of procedures help the processing of parameter actuals
and record constructor elements (which have many similar properties - e.g. a
possible mix of positional and keyword binding of actual expressions). *)

TYPE
  List <: REFANY; (* Represents a list of actuals or elements *)

PROCEDURE ElementList(cons: M3AST_AS.Constructor): List RAISES {};
(* Scans list of elements and builds 'List' structure. Checks for the
following errors: positional elements appearing after keyword elements, badly
formed keyword elements, range elements (illegal in a record constructor),
elements which are not normal expressions *)

PROCEDURE ActualList(call: M3AST_AS.Call; typeOk := FALSE): List RAISES {};
(* Scans list of actuals and builds 'List' structure. Checks for the
following errors: positional elements appearing after keyword elements, badly
formed keyword elements, actuals which are not normal expressions.
  If 'typeOk' is true any positional actuals which are types are not flagged
as errors (keyword actuals which are types are always errors). Instead a
'TypeActual' expression, with the appropriate type, is inserted into the list.
Type actuals are only permitted for the standard, built in, procedures so
'typeOk' is only TRUE when doing the argument handling for the standard
procedures.  *)

PROCEDURE TotalActuals(e: List): INTEGER RAISES {};
(* Total number of actuals/elements in list *)

PROCEDURE PositionalActuals(e: List): INTEGER RAISES {};
(* Number of positional actuals/elements at start of list *)

PROCEDURE ActualAt(
    a: List;
    pos: INTEGER;
    id: M3AST_LX.Symbol_rep)
    : M3AST_SM.EXP_UNSET
    RAISES {};
(* Returns expression for actual/element at given position in list. It is a
fatal error if the position given does not fall in the positional elements.
'id' is the keyword associated with the element. A check is done to make sure
the element is not also specified with a keyword; if it is an error message is
given. If 'id' is NIL no such check is done.
  'ActualAt' may return NIL if the parameter at the given position was an
invalid expression *)

PROCEDURE ActualByKeyword(
    a: List;
    typedId: M3AST_AS.TYPED_ID;
    VAR exp: M3AST_AS.EXP)
    : BOOLEAN
    RAISES {};
(* Finds expression of actual/element bound to keyword with the same name as
'typedId'. The keyword part of the actual found will be an 'Exp_used_id'. This
id is resolved to point at its definition - i.e. 'typedId' - and its type is
set up to be the type of 'typedId'. Resolving and setting the type of the
keyword is done so late to avoid duplicating the  code for finding which
keyword corresponds to which procedure actual or constructor element. The coe
is not that much work for normal procedures but is a real pain for NEW!
  If no keyword can be found which matches 'typedId' FALSE is returned and
'exp' is not set.
  If a keyword is found to match 'typedId' 'exp' is set to the expression bound
to the keyword. A check is done for any other bindings to the same keyword; an
error is given such a duplicate is found. 'exp' may be NIL if the expression
bound to the keyword was invalid *)

PROCEDURE FindUnmatched(e: List) RAISES {};
(* To be called when all other processing is done - complains about any keyword
actuals left unmatched *)

PROCEDURE OriginalActual(
    call: M3AST_AS.Call;
    pos: INTEGER)
    : M3AST_AS.Actual
    RAISES {};
(* Returns the positional actual at position 'pos' in the original call. This
can be useful if a 'Type_actual' was constructed to correspond to a type being
passed to a standard procedure. This 'Type_actual' (which must have been given
positionally) is not part of the syntax tree and should not have errors
attached to it. 'OriginalActual' allows the user to get back to the actual
corresponding to the type actual; any errors can be hung on this original *)

END M3CActualUtil.
