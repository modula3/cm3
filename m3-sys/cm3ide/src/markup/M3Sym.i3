(* Copyright 1996, Critical Mass, Inc.  All rights reserved. *)

INTERFACE M3Sym;

IMPORT Buf;

PROCEDURE Scan (buf: Buf.T;  cb: CallBack;  ignore := KindSet {});
(* Scan the Modula-3 file in "buf" and call the "cb.note" routines
   for any top-level  symbol definitions or uses.  If any of the
   "cb.note" routines returns "TRUE", the scan is terminated prematurely.
   Otherwise, the scan continues through the entire buffer.  The
   "cb.note" routines are not called for symbols whose kinds are in
   the "ignore" set. *)

TYPE
  KindSet = SET OF Kind;
  Kind = {
    IntfName,   (* INTERFACE <sym> ... *)
    ImplName,   (* MODULE <sym> ... *)
    GIntfName,  (* GENERIC INTERFACE <sym> ... *)
    GImplName,  (* GENERIC MODULE <sym> ... *)
    GIntfUse,   (* INTERFACE Foo = <sym> ... *)
    GImplUse,   (* MODULE Foo = <sym> ... *)
    GFormal,    (* GENERIC MODULE Baz (<sym>, <sym>, ...) *)
    GActual,    (* MODULE Foo = Baz (<sym>, <sym>, ... ) *)
    Export,     (* MODULE <sym>; // MODULE <sym> = // MODULE Foo EXPORTS <sym>... *)
    Import,     (* IMPORT <sym>, <sym>, ... *)
    FromImport, (* FROM <sym> IMPORT ... *)
    SymImport,  (* FROM Baz IMPORT <sym>, <sym>, ...   [ cb.intf = "Baz" ] *)
    ImportXX,   (* IMPORT <sym> AS Foo, ...  *)
    ImportAs,   (* IMPORT Foo AS <sym>, ...   [ cb.intf = "Foo" ] *)
    ConstDecl,  (* CONST <sym> = ... *)
    VarDecl,    (* VAR <sym>, <sym>, ... : Foo.T ...  *)
    ExceptDecl, (* EXCEPTION <sym> ...  *)
    ProcDecl,   (* PROCEDURE <sym> (...) *)
    TypeDecl,   (* TYPE <sym> = ... *)

    TypeUse,    (* TYPE T = <sym>  //  PROC P (): <sym>  // VAR v: <sym> ... *)
    ExceptUse,  (* PROC P () RAISES {<sym>, ... } *)
    ProcUse,    (* OVERRIDES M := <sym> ... *)
    MiscUse,    (* other unclassified ID or QID uses in expressions *)
    
    Keyword,      (* AND, ANY, ARRAY, AS, BEGIN, BITS, BRANDED, BY, CASE, ... *)
    BuiltinOp,    (* ABS, ADR, ADRSIZE, BITSIZE, BYTESIZE, CEILING, DEC, ... *)
    BuiltinType,  (* ADDRESS, BOOLEAN, CARDINAL, CHAR, EXTENDED, INTEGER, ... *)
    BuiltinConst  (* FALSE, TRUE, NIL *)
  };

TYPE
  CallBack = OBJECT METHODS
    note_sym (READONLY sym: Id;   kind: Kind;  intf: TEXT): BOOLEAN  := IgnoreSym;
    note_qid (READONLY qid: QId;  kind: Kind): BOOLEAN  := IgnoreQID;
  END;

  QId = ARRAY OF Id;
  Id = RECORD
    start : CARDINAL;  (* offset into "buf" *)
    len   : CARDINAL;  (* length of the identifer *)
  END;
 
PROCEDURE IgnoreSym (cb   : CallBack;
            READONLY sym  : Id;
                     kind : Kind;
                     intf : TEXT): BOOLEAN;

PROCEDURE IgnoreQID (cb   : CallBack;
            READONLY sym  : QId;
                     kind : Kind;): BOOLEAN;
(* no-op *)

END M3Sym.
