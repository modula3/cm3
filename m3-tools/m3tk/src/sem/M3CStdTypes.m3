MODULE M3CStdTypes;

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

(* return (shared) instances of standard predefined TYPE_SPECs. *)

IMPORT M3AST_AS, M3AST_SM;

IMPORT M3AST_LX_F, M3AST_AS_F;


IMPORT M3CSrcPos;

VAR
  integer: M3AST_AS.Integer_type;
  real: M3AST_AS.Real_type;
  longReal: M3AST_AS.LongReal_type;
  extended: M3AST_AS.Extended_type;
  char: M3AST_AS.TYPE_SPEC;
  text: M3AST_AS.TYPE_SPEC;
  null: M3AST_AS.Null_type;
  boolean: M3AST_AS.TYPE_SPEC;
  cardinal: M3AST_AS.TYPE_SPEC;
  refany: M3AST_AS.RefAny_type;
  address: M3AST_AS.Address_type;
  root: M3AST_AS.Root_type;
  untracedRoot: M3AST_AS.Root_type;
  mutex: M3AST_AS.TYPE_SPEC;
  any: M3AST_SM.Any_type;
  type: M3AST_SM.Type_type;
  void: M3AST_SM.Void_type;

  charpos: CARDINAL;   (* each node gets a unique lx_srcpos *)

PROCEDURE Integer(): M3AST_AS.Integer_type RAISES {}=
  BEGIN
    RETURN integer;
  END Integer;

PROCEDURE Real(): M3AST_AS.Real_type RAISES {}=
  BEGIN
    RETURN real;
  END Real;

PROCEDURE LongReal(): M3AST_AS.LongReal_type RAISES {}=
  BEGIN
    RETURN longReal;
  END LongReal;

PROCEDURE Extended(): M3AST_AS.Extended_type RAISES {}=
  BEGIN
    RETURN extended;
  END Extended;

PROCEDURE Char(): M3AST_AS.TYPE_SPEC RAISES {}=
  BEGIN
    RETURN char;
  END Char;

PROCEDURE Text(): M3AST_AS.TYPE_SPEC RAISES {}=
  BEGIN
    RETURN text;
  END Text;

PROCEDURE Boolean(): M3AST_AS.TYPE_SPEC RAISES {}=
  BEGIN
    RETURN boolean;
  END Boolean;

PROCEDURE Cardinal(): M3AST_AS.TYPE_SPEC RAISES {}=
  BEGIN
    RETURN cardinal;
  END Cardinal;

PROCEDURE RefAny(): M3AST_AS.RefAny_type RAISES {}=
  BEGIN
    RETURN refany;
  END RefAny;

PROCEDURE Address(): M3AST_AS.Address_type RAISES {}=
  BEGIN
    RETURN address;
  END Address;

PROCEDURE Root(): M3AST_AS.Root_type RAISES {}=
  BEGIN
    RETURN root;
  END Root;

PROCEDURE Untraced_Root(): M3AST_AS.Root_type RAISES {}=
  BEGIN
    RETURN untracedRoot;
  END Untraced_Root;

PROCEDURE Mutex(): M3AST_AS.TYPE_SPEC RAISES {}=
  BEGIN
    RETURN mutex;
  END Mutex;

PROCEDURE Null(): M3AST_AS.Null_type RAISES {}=
  BEGIN
    RETURN null;
  END Null;

PROCEDURE Any(): M3AST_SM.Any_type RAISES {}=
  BEGIN
    RETURN any;
  END Any;

PROCEDURE Type(): M3AST_SM.Type_type RAISES {}=
  BEGIN
    RETURN type;
  END Type;

PROCEDURE Void(): M3AST_SM.Void_type RAISES {}=
  BEGIN
    RETURN void;
  END Void;

PROCEDURE RegisterChar(ts: M3AST_AS.TYPE_SPEC) RAISES {}=
  BEGIN
    char := ts;
  END RegisterChar;

PROCEDURE RegisterText(ts: M3AST_AS.TYPE_SPEC) RAISES {}=
  BEGIN
    text := ts;
  END RegisterText;

PROCEDURE RegisterBoolean(ts: M3AST_AS.TYPE_SPEC) RAISES {}=
  BEGIN
    boolean := ts;
  END RegisterBoolean;

PROCEDURE RegisterCardinal(ts: M3AST_AS.TYPE_SPEC) RAISES {}=
  BEGIN
    cardinal := ts;
  END RegisterCardinal;

PROCEDURE RegisterMutex(ts: M3AST_AS.TYPE_SPEC) RAISES {}=
  BEGIN
    mutex := ts;
  END RegisterMutex;

PROCEDURE Init(n: M3AST_AS.SRC_NODE) RAISES {}=
  BEGIN
    INC(charpos);
    n.lx_srcpos := M3CSrcPos.Pack(0, charpos);
  END Init;

BEGIN
  charpos := 0;
  integer := NEW(M3AST_AS.Integer_type).init(); Init(integer);
  real := NEW(M3AST_AS.Real_type).init(); Init(real);
  longReal := NEW(M3AST_AS.LongReal_type).init(); Init(longReal);
  extended := NEW(M3AST_AS.Extended_type).init(); Init(extended);
  null := NEW(M3AST_AS.Null_type).init(); Init(null);
  any := NEW(M3AST_SM.Any_type).init(); Init(any);
  type := NEW(M3AST_SM.Type_type).init(); Init(type);
  void := NEW(M3AST_SM.Void_type).init(); Init(void);
  refany := NEW(M3AST_AS.RefAny_type).init(); Init(refany);
  address := NEW(M3AST_AS.Address_type).init(); Init(address);
  root := NEW(M3AST_AS.Root_type).init(); Init(root);
  untracedRoot := NEW(M3AST_AS.Root_type).init();
  untracedRoot.as_trace_mode := NEW(M3AST_AS.Untraced).init();
  Init(untracedRoot);
END M3CStdTypes.
