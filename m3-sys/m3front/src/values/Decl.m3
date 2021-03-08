(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Decl.m3                                               *)
(* Last modified on Tue Dec 20 14:54:22 PST 1994 by kalsow     *)
(*      modified on Sat Mar 16 01:56:20 1991 by muller         *)

MODULE Decl;

IMPORT M3, M3ID, M3String, Token, Error, ESet, Module, Exceptionz;
IMPORT Constant, Tipe, Variable, Procedure, Revelation, CG, Target;
FROM Scanner IMPORT GetToken, Match, cur;

TYPE
  TK = Token.T;

TYPE
  Attr    = { External, Inline, Implicit, CallConv, Obsolete, Unused,
              LazyAligned };
  AttrSet = SET OF Attr;
CONST
  RevealOK    = AttrSet { };
  ConstantOK  = AttrSet { Attr.Obsolete, Attr.Unused, Attr.LazyAligned };
  TypeOK      = AttrSet { Attr.Obsolete, Attr.Unused, Attr.LazyAligned };
  ExceptionOK = AttrSet { Attr.Obsolete, Attr.Unused, Attr.Implicit };
  VariableOK  = AttrSet { Attr.Obsolete, Attr.Unused, Attr.External,
                          Attr.LazyAligned };
  ProcedureOK = AttrSet { Attr.Obsolete, Attr.Unused, Attr.External,
                          Attr.Inline, Attr.CallConv };

PROCEDURE Parse (interface, top_level: BOOLEAN;  VAR fails: M3.ExSet) =
  VAR att: Attributes;  got_cc := FALSE;
  BEGIN
    att.isInline    := FALSE;
    att.isExternal  := FALSE;
    att.isUnused    := FALSE;
    att.isObsolete  := FALSE;
    att.isImplicit  := FALSE;
    att.alias       := M3ID.NoID;
    att.callingConv := NIL;
    att.isLazyAligned := Module.LazyAlignmentOn ();
    LOOP
      CASE cur.token OF
      | TK.tEXTERNAL =>
          IF NOT Module.IsInterface () THEN
            Error.Msg ("External declarations only allowed in interfaces");
          END;
          ParseExternalPragma (att.alias, att.callingConv, got_cc);
          att.isExternal := TRUE;
      | TK.tINLINE   =>
          att.isInline := TRUE;
          GetToken (); (* INLINE *)
          Match (TK.tENDPRAGMA);
      | TK.tUNUSED   =>
          att.isUnused := TRUE;
          GetToken (); (* UNUSED *)
          Match (TK.tENDPRAGMA);
      | TK.tOBSOLETE =>
          att.isObsolete := TRUE;
          GetToken (); (* OBSOLETE *)
          Match (TK.tENDPRAGMA);
      | TK.tIMPLICIT =>
          att.isImplicit := TRUE;
          GetToken (); (* IMPLICIT *)
          Match (TK.tENDPRAGMA);
      | TK.tCALLCONV   =>
          att.callingConv := Target.FindConvention (M3ID.ToText (cur.id));
          got_cc := TRUE;
          GetToken (); (* convention name *)
          Match (TK.tENDPRAGMA);
      | TK.tLAZYALIGN =>
          Error.Msg ("<*LazyAlign*> is no longer supported");
          att.isLazyAligned := TRUE;
          Module.SetLazyAlignment (TRUE);
          GetToken (); (* LAZYALIGN *)
          Match (TK.tENDPRAGMA);
      | TK.tSTRICTALIGN =>
          Error.Msg ("<*StrictAlign*> is no longer supported");
          att.isLazyAligned := FALSE;
          Module.SetLazyAlignment (FALSE);
          GetToken (); (* LAZYALIGN *)
          Match (TK.tENDPRAGMA);
      ELSE EXIT;
      END;
    END;

    CASE cur.token OF
    | TK.tCONST =>
        CheckAttrs (att, got_cc, ConstantOK, "constant");
        Constant.ParseDecl (att);
    | TK.tTYPE =>
        CheckAttrs (att, got_cc, TypeOK, "type");
        Tipe.Parse (att);
    | TK.tVAR =>
        att.isExternal := att.isExternal OR Module.IsExternal ();
        CheckAttrs (att, got_cc, VariableOK, "variable");
        Variable.ParseDecl (att);
    | TK.tPROCEDURE =>
        att.isExternal := att.isExternal OR Module.IsExternal ();
        CheckAttrs (att, got_cc, ProcedureOK, "procedure");
        Procedure.ParseDecl (att, interface);
    | TK.tREVEAL =>
        IF (NOT top_level) THEN
          Error.Msg ("revelations in nested scopes are not allowed");
        END;
        CheckAttrs (att, got_cc, RevealOK, "revelation");
        Revelation.Parse (att);
    | TK.tEXCEPTION =>
        IF (NOT top_level) THEN
          Error.Msg ("exception declarations in nested scopes are not allowed");
        END;
        CheckAttrs (att, got_cc, ExceptionOK, "exception");
        Exceptionz.ParseDecl (att);
    | TK.tFATAL =>
        fails := ESet.ParseFails (fails);
    ELSE 
        IF att.isInline OR att.isExternal OR att.isUnused
           OR att.isObsolete OR att.isImplicit THEN
          Error.Msg ("declaration pragma not followed by a declaration");
        END;
    END;
  END Parse;

PROCEDURE ParseExternalPragma (VAR alias  : M3ID.T;
                               VAR cc     : CG.CallingConvention;
                               VAR got_cc : BOOLEAN) =
  VAR txt: TEXT;
  BEGIN
    <* ASSERT cur.token = TK.tEXTERNAL *>
    GetToken (); (* EXTERNAL *)

    alias  := M3ID.NoID;  (* default => use the Modula-3 name *)
    cc     := Target.DefaultCall;
    got_cc := FALSE;

    IF (cur.token = TK.tIDENT) OR (cur.token = TK.tTEXTCONST) THEN
      IF (cur.token = TK.tIDENT)
        THEN alias := cur.id;
        ELSE alias := M3ID.Add (M3String.ToText (cur.str));
      END;
      GetToken (); (* IDENT, TEXTCONST *)

      IF (cur.token = TK.tCOLON) THEN
        GetToken (); (* : *)
        IF    (cur.token = TK.tIDENT)     THEN  txt := M3ID.ToText (cur.id);
        ELSIF (cur.token = TK.tTEXTCONST) THEN  txt := M3String.ToText (cur.str);
        ELSE                                    txt := NIL;
        END;
        IF (txt # NIL) THEN
          cc := Target.FindConvention (txt);
          got_cc := TRUE;
          IF (cc = NIL) THEN
            Error.ID (cur.id, "unsupported language or calling convention");
            cc := Target.DefaultCall;
          END;
          GetToken (); (* IDENT/TEXTCONST *)
        ELSE
          Error.Msg ("Missing language for <*EXTERNAL*> pragma");
        END;
      END;

    END;

    Match (TK.tENDPRAGMA);
  END ParseExternalPragma;

PROCEDURE CheckAttrs (VAR att: Attributes;  got_cc: BOOLEAN;
                      allowed: AttrSet;  tag: TEXT) =
  BEGIN
    IF att.isUnused AND NOT Attr.Unused IN allowed THEN
      Error.Msg (tag & "s cannot be unused");
      att.isUnused := FALSE;
    END;
    IF att.isObsolete AND NOT Attr.Obsolete IN allowed THEN
      Error.Msg (tag & "s cannot be obsolete");
      att.isObsolete := FALSE;
    END;
    IF att.isInline AND NOT Attr.Inline IN allowed THEN
      Error.Msg (tag & "s cannot be inline");
      att.isInline := FALSE;
    END;
    IF att.isImplicit AND NOT Attr.Implicit IN allowed THEN
      Error.Msg (tag & "s cannot be declared <*IMPLICIT*>");
      att.isImplicit := FALSE;
    END;
    IF att.isExternal AND NOT Attr.External IN allowed THEN
      Error.Msg (tag & "s cannot be external");
      att.isExternal  := FALSE;
      att.callingConv := NIL;
      att.alias       := M3ID.NoID;
    ELSIF got_cc AND att.callingConv # NIL AND NOT Attr.CallConv IN allowed THEN
      Error.Msg (tag & "s do not have calling conventions");
      att.callingConv := NIL;
    END;
  END CheckAttrs;

BEGIN
END Decl.
