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
(**)
(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3CParse;

IMPORT Text, TextExtras, Fmt;
IMPORT Rd;

IMPORT M3AST, M3AST_PG, M3AST_AS;


IMPORT M3AST_LX_F, M3AST_AS_F, M3AST_PG_F; 


IMPORT
    SeqM3AST_AS_IMPORTED, SeqM3AST_AS_Import_item,
    SeqM3AST_AS_Used_interface_id, SeqM3AST_AS_Used_def_id,
    SeqM3AST_AS_REVELATION, SeqM3AST_AS_DECL_REVL,
    SeqM3AST_AS_Const_decl, SeqM3AST_AS_TYPE_DECL,
    SeqM3AST_AS_Var_decl, SeqM3AST_AS_Exc_decl,
    SeqM3AST_AS_Var_id, SeqM3AST_AS_F_Interface_id,
    SeqM3AST_AS_Enum_id, SeqM3AST_AS_Field_id,
    SeqM3AST_AS_FORMAL_ID, SeqM3AST_AS_Qual_used_id,
    SeqM3AST_AS_Fields, SeqM3AST_AS_Method, SeqM3AST_AS_Override,
    SeqM3AST_AS_M3TYPE, 
    SeqM3AST_AS_Formal_param, SeqM3AST_AS_CONS_ELEM,
    SeqM3AST_AS_EXP, SeqM3AST_AS_Actual,
    SeqM3AST_AS_Case, SeqM3AST_AS_STM,
    SeqM3AST_AS_Elsif, SeqM3AST_AS_Tcase,
    SeqM3AST_AS_Handler, SeqM3AST_AS_Binding,
    SeqM3AST_AS_RANGE_EXP;

IMPORT M3CHash, M3CToken, M3CReservedWord, M3CSrcPos;
IMPORT M3CPragma, M3CLex, M3CLiteral, M3CComment;
<*NOWARN*> IMPORT M3CWhitespace;

TYPE
  Token = M3CToken.T;
  TokenSet = M3CToken.Set;


CONST
  None = TokenSet{};

  StartOfUnit = TokenSet{M3CToken.UNSAFE_, M3CToken.MODULE_, M3CToken.INTERFACE_,
                         M3CToken.GENERIC_};

  StartOfImport = TokenSet{M3CToken.FROM_, M3CToken.IMPORT_};

  StartOfDeclaration =
      TokenSet{M3CToken.CONST_, M3CToken.TYPE_, M3CToken.VAR_, M3CToken.EXCEPTION_,
          M3CToken.PROCEDURE_};

  StartOfRevelation = TokenSet{M3CToken.REVEAL_};

  StartOfDeclarationOrRevelation = StartOfDeclaration + StartOfRevelation;

  StartOfBlock = TokenSet{M3CToken.BEGIN_} + StartOfDeclarationOrRevelation;

  AlwaysStartOfType = TokenSet{M3CToken.CurlyBra, M3CToken.SquareBra, M3CToken.ADDRESS_,
      M3CToken.ARRAY_, M3CToken.BITS_, M3CToken.INTEGER_, M3CToken.LONGREAL_, M3CToken.NULL_,
      M3CToken.OBJECT_, M3CToken.REAL_, M3CToken.RECORD_, M3CToken.REF_, M3CToken.REFANY_,
      M3CToken.ROOT_, M3CToken.SET_, M3CToken.BRANDED_, M3CToken.UNTRACED_,
      M3CToken.EXTENDED_};
  StartOfType = AlwaysStartOfType +
      TokenSet{M3CToken.Identifier, M3CToken.Bra, M3CToken.PROCEDURE_};
      

  AlwaysStartOfExpression =
      TokenSet{M3CToken.NOT_, M3CToken.Plus, M3CToken.Minus, M3CToken.NIL_} +
          M3CToken.Literals;
  StartOfExpression = AlwaysStartOfExpression + StartOfType;

  AlwaysStartOfStatement = TokenSet{M3CToken.CASE_, M3CToken.EXIT_, M3CToken.EVAL_,
      M3CToken.FOR_, M3CToken.IF_, M3CToken.LOCK_, M3CToken.LOOP_, M3CToken.RAISE_,
      M3CToken.REPEAT_, M3CToken.RETURN_, M3CToken.TRY_, M3CToken.TYPECASE_, M3CToken.WHILE_,
      M3CToken.WITH_};
  StartOfStatement = AlwaysStartOfStatement + StartOfBlock + StartOfExpression;

  Start = StartOfUnit + StartOfImport + StartOfStatement + StartOfBlock;

  IdAsSet = TokenSet{M3CToken.Identifier};
  EndAsSet = TokenSet{M3CToken.END_};
  ElseOrEnd = TokenSet{M3CToken.ELSE_} + EndAsSet;


REVEAL
  T = Public BRANDED OBJECT
    lexer: M3CLex.T;
    lastErrorPos := M3CSrcPos.Null;
    lastSrcPosNode: M3AST_AS.SRC_NODE := NIL;
    terminators := SET OF CHAR{};
    interface := FALSE;
    commentOrPragma := FALSE;
    pragmas: M3CPragma.Store;
    comments: M3CComment.Store;
    lastPragma: M3CPragma.T := NIL;
    lastComment: M3CComment.T := NIL;
    errorHandler: ErrorHandler := NIL;
    identifiers: M3CReservedWord.Table;
    idNEW: M3CHash.Id := NIL;
    nil_litrep: M3CLiteral.T;
  OVERRIDES
    init := Init;
    compilation := Compilation;
    any := Any;
    reset := Reset;
  END;


PROCEDURE ErrorMessage(t: T; text: Text.T) RAISES {}=
  VAR
    pos := t.lexer.position();
  BEGIN
    t.lastErrorPos := pos;
    t.errorHandler.handle(pos, text);
  END ErrorMessage;


PROCEDURE UnexpectedMessage(t: T; text: Text.T := NIL) RAISES {}=
  BEGIN
    IF text = NIL THEN
      ErrorMessage(t, Fmt.F("Unexpected symbol: %s",
          t.lexer.currentTokenToText()));
    ELSE
      ErrorMessage(t,
          Fmt.F("%s expected, %s found", text, t.lexer.currentTokenToText()));
    END;
  END UnexpectedMessage;


<*INLINE*> PROCEDURE FirstErrorHere(t: T): BOOLEAN RAISES {}=
  BEGIN
    RETURN t.lastErrorPos # t.lexer.position();
  END FirstErrorHere;


PROCEDURE Unexpected(t: T) RAISES {}=
  BEGIN
    IF FirstErrorHere(t) THEN
      UnexpectedMessage(t, NIL);
    END;
  END Unexpected;


PROCEDURE Expected(t: T; token: Token) RAISES {}=
  BEGIN
    IF FirstErrorHere(t) THEN
      UnexpectedMessage(t, M3CLex.TokenToText(token));
    END;
  END Expected;


PROCEDURE SetToText(set: TokenSet): Text.T RAISES {}=
  TYPE
    ST = RECORD set: TokenSet; text: Text.T END;
  CONST
    CommonSets = ARRAY [0..2] OF ST{
        ST{StartOfType, "Type"},
        ST{StartOfExpression, "Expression"},
        ST{StartOfStatement, "Statement"}};
  BEGIN
    FOR i := FIRST(CommonSets) TO LAST(CommonSets) DO
      WITH st = CommonSets[i] DO
        IF st.set = set THEN RETURN st.text END;
      END;
    END;
    VAR
      count := 0;
      save: ARRAY [0..2] OF Token;
    BEGIN
      FOR i := FIRST(Token) TO LAST(Token) DO
        IF i IN set THEN
          IF count < NUMBER(save) THEN save[count] := i END;
          INC(count);
        END;
      END;
      IF 0 < count AND count <= NUMBER(save) THEN
        VAR
          result: Text.T;
        BEGIN
          FOR i := 0 TO count - 1 DO
            VAR
              tokenText := M3CLex.TokenToText(save[i]);
              join: Text.T;
            BEGIN
              IF i = 0 THEN
                result := tokenText;
              ELSE
                IF i = count - 1 THEN join := " or " ELSE join := ", " END;
                result := result & join & tokenText;
              END;
            END;
          END;
          RETURN result;
        END;
      ELSE
        RETURN NIL;
      END;
    END;
  END SetToText;


PROCEDURE ExpectedSet(t: T; READONLY valid: TokenSet) RAISES {}=
  BEGIN
    IF FirstErrorHere(t) THEN
      UnexpectedMessage(t, SetToText(valid));
    END;
  END ExpectedSet;


PROCEDURE NodeAfter(t: T; srcNode: M3AST_AS.SRC_NODE) RAISES {}=
  BEGIN
    IF t.lastPragma # NIL THEN
      M3CPragma.AddFollowingNode(srcNode, t.pragmas);
      t.lastPragma := NIL;
    END;
    IF t.lastComment # NIL THEN
      M3CComment.AddFollowingNode(srcNode, t.comments);
      t.lastComment := NIL;
    END;
    t.commentOrPragma := FALSE;
  END NodeAfter;


<*INLINE*> PROCEDURE Pos(
    t: T;
    srcNode: M3AST_AS.SRC_NODE;
    next := FALSE)
    RAISES {Rd.Failure}=
  BEGIN
    srcNode.lx_srcpos := t.lexer.position();
    t.lastSrcPosNode := srcNode;
    IF t.commentOrPragma THEN NodeAfter(t, srcNode) END;
    IF next THEN EVAL t.lexer.next() END;
  END Pos;


<*INLINE*> PROCEDURE EndPos(
    t: T;
    mustBeAt := M3CToken.END_)
    RAISES {Rd.Failure}=
  BEGIN
    EVAL MustBeAt(t, mustBeAt);
  END EndPos;


<*INLINE*> PROCEDURE At(t: T; token: Token): BOOLEAN RAISES {Rd.Failure}=
  BEGIN
    IF t.lexer.current() = token THEN
      EVAL t.lexer.next();
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END; (* if *)
  END At;


<*INLINE*> PROCEDURE MustBeAt(
    t: T;
    token: Token)
    : BOOLEAN
    RAISES {Rd.Failure}=
  VAR
    at := At(t, token);
  BEGIN
    IF NOT at THEN Expected(t, token) END;
    RETURN at;
  END MustBeAt;


PROCEDURE LenientAt(
    t: T;
    token, alternative: Token)
    : BOOLEAN
    RAISES {Rd.Failure}=
  BEGIN
    IF NOT At(t, token) THEN
      IF t.lexer.current() = alternative THEN
        Expected(t, token);
        EVAL t.lexer.next();
        RETURN TRUE;
      ELSE
        RETURN FALSE;
      END;
    ELSE
      RETURN TRUE;
    END;
  END LenientAt;


<*INLINE*> PROCEDURE LenientMustBeAt(
    t: T;
    token, alternative: Token)
    : BOOLEAN
    RAISES {Rd.Failure}=
  BEGIN
    RETURN MustBeAt(t, token) OR At(t, alternative);
  END LenientMustBeAt;


PROCEDURE FindExpected(
    t: T;
    token: Token;
    READONLY term: TokenSet)
    : BOOLEAN
    RAISES {Rd.Failure}=
  VAR
    current := t.lexer.current();
    stop := term + TokenSet{token};
  BEGIN
    Expected(t, token);
    LOOP
      IF current IN stop THEN
        IF current = token THEN
          EVAL t.lexer.next();
          RETURN TRUE;
        ELSE
          RETURN FALSE;
        END;
      ELSE
        current := t.lexer.next();
      END;
    END;
  END FindExpected;


<*INLINE*> PROCEDURE Expect(
    t: T;
    token: Token;
    READONLY term: TokenSet)
    : BOOLEAN
    RAISES {Rd.Failure}=
  BEGIN
    IF t.lexer.current() = token THEN
      EVAL t.lexer.next();
      RETURN TRUE;
    ELSE
      RETURN FindExpected(t, token, term);
    END;
  END Expect;


PROCEDURE FindExpectedSet(
    t: T;
    READONLY valid, term: TokenSet)
    : BOOLEAN
    RAISES {Rd.Failure}=
  VAR
    current := t.lexer.current();
    stop := valid + term;
  BEGIN
    ExpectedSet(t, valid);
    LOOP
      IF current IN stop THEN
        RETURN current IN valid;
      ELSE
        current := t.lexer.next();
      END;
    END;
  END FindExpectedSet;


<*INLINE*> PROCEDURE ExpectSet(
    t: T;
    READONLY valid: TokenSet;
    READONLY term := None)
    : BOOLEAN
    RAISES {Rd.Failure}=
  BEGIN
    IF t.lexer.current() IN valid THEN
      RETURN TRUE;
    ELSE
      RETURN FindExpectedSet(t, valid, term);
    END;
  END ExpectSet;


PROCEDURE EndOfSequenceSet(
    t: T;
    sep: Token;
    READONLY validTerm, continue, term: TokenSet)
    : BOOLEAN
    RAISES {Rd.Failure}=
(* After a call of 'EndOfSequenceSet' the current token not 'sep' and is
in one of the sets:
'validTerm' => result is TRUE
'continue' => result is FALSE
'term' => result is TRUE *)
  VAR
    sepAllowedAtEnd := sep = M3CToken.Semicolon;
    atSep := At(t, sep);
  BEGIN
    LOOP
      WITH current = t.lexer.current() DO
        IF current = sep THEN
          Unexpected(t);
          EVAL t.lexer.next();
        ELSIF (NOT atSep OR sepAllowedAtEnd) AND current IN validTerm THEN
          RETURN TRUE;
        ELSIF current IN continue THEN
          IF NOT atSep THEN EVAL Expect(t, sep, continue) END;
          RETURN FALSE;
        ELSIF current IN term THEN
          IF atSep AND NOT sepAllowedAtEnd THEN Unexpected(t) END;
          EVAL FindExpectedSet(t, validTerm, term);
          RETURN TRUE;
        ELSE
          IF atSep THEN
            EVAL ExpectSet(t, continue + validTerm + term + TokenSet{sep});
            atSep := At(t, sep); 
          ELSE
            atSep := Expect(t, sep, continue + validTerm + term);
          END;
        END;
      END;
    END;
  END EndOfSequenceSet;


<*INLINE*> PROCEDURE EndOfSequence(
    t: T;
    sep, validTerm: Token;
    READONLY continue, term: TokenSet)
    : BOOLEAN
    RAISES {Rd.Failure}=
  BEGIN
    WITH
      result = EndOfSequenceSet(t, sep, TokenSet{validTerm}, continue, term)
    DO
      EVAL At(t, validTerm);
      RETURN result;
    END;
  END EndOfSequence;


PROCEDURE Id(t: T; id: M3AST_AS.ID) RAISES {Rd.Failure}=
  BEGIN
    Pos(t, id);
    IF t.lexer.current() = M3CToken.Identifier THEN
      id.lx_symrep := t.lexer.identifier();
      EVAL t.lexer.next();
    ELSE
      Expected(t, M3CToken.Identifier);
    END;
  END Id;


PROCEDURE SingleIdQualId(
    t: T;
    id: M3CLex.Symbol_rep;
    pos: M3CSrcPos.T)
    : M3AST_AS.Qual_used_id RAISES {}=
  BEGIN
    VAR q: M3AST_AS.Qual_used_id := NEW(M3AST_AS.Qual_used_id).init();
    BEGIN
      q.lx_srcpos := pos;
      q.as_id := NEW(M3AST_AS.Used_def_id).init();
      q.as_id.lx_symrep := id;
      q.as_id.lx_srcpos := pos;
      t.lastSrcPosNode := q.as_id;
      RETURN q;
    END;
  END SingleIdQualId;


PROCEDURE DoubleIdQualId(
    t: T;
    id1, id2: M3CLex.Symbol_rep;
    pos1, pos2: M3CSrcPos.T)
    : M3AST_AS.Qual_used_id
    RAISES {}=
  BEGIN
    VAR q: M3AST_AS.Qual_used_id := NEW(M3AST_AS.Qual_used_id).init();
    BEGIN
      q.lx_srcpos := pos1;
      q.as_intf_id := NEW(M3AST_AS.Used_interface_id).init();
      q.as_intf_id.lx_symrep := id1;
      q.as_intf_id.lx_srcpos := pos1;
      q.as_id := NEW(M3AST_AS.Used_def_id).init();
      q.as_id.lx_symrep := id2;
      q.as_id.lx_srcpos := pos2;
      t.lastSrcPosNode := q.as_id;
      RETURN q;
    END;
  END DoubleIdQualId;


PROCEDURE QualId(t: T): M3AST_AS.Qual_used_id RAISES {Rd.Failure}=
  VAR
    id1 := t.lexer.identifier();
    pos1 := t.lexer.position();
  BEGIN
    IF NOT MustBeAt(t, M3CToken.Identifier) THEN id1 := NIL END;
    IF At(t, M3CToken.Dot) THEN
      VAR
        id2 := t.lexer.identifier();
        pos2 := t.lexer.position();
      BEGIN
        IF NOT MustBeAt(t, M3CToken.Identifier) THEN id2 := NIL END;
        RETURN DoubleIdQualId(t, id1, id2, pos1, pos2);
      END;
    ELSE
      RETURN SingleIdQualId(t, id1, pos1);
    END;
  END QualId;


PROCEDURE NamedType(q: M3AST_AS.Qual_used_id): M3AST_AS.Named_type RAISES {}=
  VAR
    n: M3AST_AS.Named_type := NEW(M3AST_AS.Named_type).init();
  BEGIN
    n.lx_srcpos := q.lx_srcpos;
    n.as_qual_id := q;
    RETURN n;
  END NamedType;


PROCEDURE Array(
    t: T;
    READONLY term: TokenSet)
    : M3AST_AS.Array_type
    RAISES {Rd.Failure}=
  VAR
    a: M3AST_AS.Array_type := NEW(M3AST_AS.Array_type).init();
  BEGIN
    Pos(t, a, TRUE);
    a.as_indextype_s := SeqM3AST_AS_M3TYPE.Null;
    IF NOT At(t, M3CToken.OF_) THEN
      WITH arrayTerm = term + TokenSet{M3CToken.Comma, M3CToken.OF_} + StartOfType DO
        REPEAT
          SeqM3AST_AS_M3TYPE.AddRear(a.as_indextype_s, Type(t, arrayTerm));
        UNTIL EndOfSequence(t, M3CToken.Comma, M3CToken.OF_, StartOfType, arrayTerm);
      END;
    END;
    a.as_elementtype := Type(t, term);
    RETURN a;
  END Array;


PROCEDURE Packed(
    t: T;
    READONLY term: TokenSet)
    : M3AST_AS.Packed_type
    RAISES {Rd.Failure}=
  VAR
    packedTerm := term + TokenSet{M3CToken.FOR_} + StartOfType;
    p: M3AST_AS.Packed_type := NEW(M3AST_AS.Packed_type).init();
  BEGIN
    Pos(t, p, TRUE);
    p.as_exp := Expr(t, packedTerm);
    EVAL Expect(t, M3CToken.FOR_, packedTerm);
    p.as_type := Type(t, term);
    RETURN p;
  END Packed;


PROCEDURE TypeAndOrDefault(
    t: T;
    READONLY term: TokenSet;
    VAR default: M3AST_AS.EXP_NULL)
    : M3AST_AS.M3TYPE_NULL
    RAISES {Rd.Failure}=
  VAR
    type: M3AST_AS.M3TYPE_NULL := NIL;
    typeTerm := term + TokenSet{M3CToken.Becomes} + StartOfExpression;
  BEGIN
    default := NIL;
    IF At(t, M3CToken.Colon) THEN
      type := Type(t, typeTerm);
    ELSIF t.lexer.current() IN StartOfExpression THEN
      TYPECASE Expr(t, typeTerm, TRUE) OF <*NOWARN*>
      | M3AST_AS.M3TYPE(m3Type) =>
          type := m3Type;
      | M3AST_AS.EXP(exp) =>
          default := exp;
      END;
    END;
    IF default = NIL THEN
      WITH at = LenientAt(t, M3CToken.Becomes, M3CToken.Equal) DO
        IF at OR t.lexer.current() IN StartOfExpression - IdAsSet THEN
          IF NOT at THEN Expected(t, M3CToken.Becomes) END;
          default := Expr(t, term);
        END;
      END;
    END;
    IF type = NIL AND default = NIL THEN
      RETURN NEW(M3AST_AS.Bad_M3TYPE).init();
    ELSE
      RETURN type;
    END;
  END TypeAndOrDefault;


PROCEDURE Fields(
    t: T;
    READONLY validTerm, term: TokenSet)
    : SeqM3AST_AS_Fields.T
    RAISES {Rd.Failure}=
  VAR
    seqFields := SeqM3AST_AS_Fields.Null;
  CONST
    PossibleStartOfField = StartOfType + StartOfExpression +
        TokenSet{M3CToken.Identifier, M3CToken.Colon, M3CToken.Becomes};
  BEGIN
    WITH
      fieldTerm = validTerm + term +
          TokenSet{M3CToken.Semicolon} + PossibleStartOfField
    DO
      REPEAT
        VAR fields: M3AST_AS.Fields := NEW(M3AST_AS.Fields).init();
        BEGIN
          SeqM3AST_AS_Fields.AddRear(seqFields, fields);
          Pos(t, fields);
          fields.as_id_s := SeqM3AST_AS_Field_id.Null;
          (* IdList *)
          REPEAT
            VAR id: M3AST_AS.Field_id := NEW(M3AST_AS.Field_id).init();
            BEGIN
              SeqM3AST_AS_Field_id.AddRear(fields.as_id_s, id);
              Id(t, id);
            END;
          UNTIL EndOfSequenceSet(t, M3CToken.Comma,
              TokenSet{M3CToken.Colon, M3CToken.Becomes}, IdAsSet, fieldTerm);
          (* ( ":=" Expr & ":" Type ) *)
          fields.as_type := TypeAndOrDefault(t, fieldTerm, fields.as_default);
        END;
      UNTIL EndOfSequenceSet(t, M3CToken.Semicolon,
          validTerm, PossibleStartOfField, term);
    END;
    RETURN seqFields;
  END Fields;


PROCEDURE Methods(
    t: T;
    READONLY validTerm, term: TokenSet)
    : SeqM3AST_AS_Method.T
    RAISES {Rd.Failure}=
  VAR
    methods := SeqM3AST_AS_Method.Null;
  CONST
    PossibleStartOfMethod =
        TokenSet{M3CToken.Identifier, M3CToken.Bra, M3CToken.Becomes};
  BEGIN
    WITH
      methodTerm = validTerm + term +
          TokenSet{M3CToken.Semicolon} + PossibleStartOfMethod
    DO
      REPEAT
        VAR method: M3AST_AS.Method := NEW(M3AST_AS.Method).init();
        BEGIN
          SeqM3AST_AS_Method.AddRear(methods, method);
          Pos(t, method);
          method.as_id := NEW(M3AST_AS.Method_id).init();
          Id(t, method.as_id);
          WITH pos = t.lexer.position() DO
            method.as_type := Signature(t, methodTerm);
            method.as_type.lx_srcpos := pos;
          END;
          EVAL ExpectSet(t, methodTerm);
          IF LenientAt(t, M3CToken.Becomes, M3CToken.Equal) THEN
            method.as_default := Expr(t, methodTerm, FALSE);
          END;
        END;
      UNTIL EndOfSequenceSet(t, M3CToken.Semicolon,
          validTerm, PossibleStartOfMethod, term);
    END;
    RETURN methods;
  END Methods;


PROCEDURE Overrides(
    t: T;
    READONLY validTerm, term: TokenSet)
    : SeqM3AST_AS_Override.T
    RAISES {Rd.Failure}=
  VAR
    overrides := SeqM3AST_AS_Override.Null;
  CONST
    PossibleStartOfOverride =
        TokenSet{M3CToken.Identifier, M3CToken.Becomes};
  BEGIN
    WITH
      overrideTerm = validTerm + term +
          TokenSet{M3CToken.Semicolon} + PossibleStartOfOverride
    DO
      REPEAT
        VAR override: M3AST_AS.Override := NEW(M3AST_AS.Override).init();
        BEGIN
          SeqM3AST_AS_Override.AddRear(overrides, override);
          Pos(t, override);
          override.as_id := NEW(M3AST_AS.Override_id).init();
          Id(t, override.as_id);
          IF LenientMustBeAt(t, M3CToken.Becomes, M3CToken.Equal) THEN
            override.as_default := Expr(t, overrideTerm, FALSE);
          ELSE
            override.as_default := NEW(M3AST_AS.Bad_EXP).init();
          END;
        END;
      UNTIL EndOfSequenceSet(t, M3CToken.Semicolon,
          validTerm, PossibleStartOfOverride, term);
    END;
    RETURN overrides;
  END Overrides;


PROCEDURE ObjectCheck(
    t: T;
    READONLY term: TokenSet;
    ancestor: M3AST_AS.M3TYPE)
    : M3AST_AS.M3TYPE
    RAISES {Rd.Failure}=
  VAR
    token := t.lexer.current();
  BEGIN
    IF token = M3CToken.OBJECT_ THEN
      RETURN Object(t, term, ancestor);
    ELSIF token = M3CToken.BRANDED_ THEN
      RETURN Branded(t, term, ancestor := ancestor);
    ELSE
      RETURN ancestor;
    END; (* if *)
  END ObjectCheck;


PROCEDURE Object(
    t: T;
    READONLY term: TokenSet;
    ancestor: M3AST_AS.M3TYPE := NIL;
    brand: M3AST_AS.Brand := NIL)
    : M3AST_AS.Object_type
    RAISES {Rd.Failure}=
  CONST
    MethodsOrOverrides = TokenSet{M3CToken.METHODS_, M3CToken.OVERRIDES_};
    MethodsOrOverridesOrEnd =  MethodsOrOverrides +  EndAsSet;
    OverridesOrEnd = TokenSet{M3CToken.OVERRIDES_} + EndAsSet;
  VAR
    o: M3AST_AS.Object_type := NEW(M3AST_AS.Object_type).init();
  BEGIN
    Pos(t, o, TRUE);
    o.as_ancestor := ancestor;
    o.as_brand := brand;
    IF t.lexer.current() # M3CToken.END_ THEN
      IF NOT(t.lexer.current() IN MethodsOrOverrides) THEN
        o.as_fields_s := Fields(t, MethodsOrOverridesOrEnd, term);
      ELSE
        o.as_fields_s := SeqM3AST_AS_Fields.Null;
      END;
      IF At(t, M3CToken.METHODS_) AND t.lexer.current() # M3CToken.END_ THEN
        o.as_method_s := Methods(t, OverridesOrEnd, term);
      ELSE
        o.as_method_s := SeqM3AST_AS_Method.Null;
      END;
      IF At(t, M3CToken.OVERRIDES_) AND t.lexer.current() # M3CToken.END_ THEN
        o.as_override_s := Overrides(t, EndAsSet, term);
      ELSE
        o.as_override_s := SeqM3AST_AS_Override.Null;
      END;
    ELSE
      o.as_fields_s := SeqM3AST_AS_Fields.Null;
      o.as_method_s := SeqM3AST_AS_Method.Null;
    END;
    EndPos(t);
    RETURN ObjectCheck(t, term, o);
  END Object;


CONST
  PossibleStartOfFormal =
      StartOfType + StartOfExpression +
      TokenSet{M3CToken.VALUE_, M3CToken.VAR_, M3CToken.READONLY_} +
      TokenSet{M3CToken.Identifier, M3CToken.Colon, M3CToken.Becomes};


PROCEDURE NewF_Value_id(): M3AST_AS.FORMAL_ID RAISES {}=
  BEGIN
    RETURN NEW(M3AST_AS.F_Value_id).init();
  END NewF_Value_id;


PROCEDURE NewF_Readonly_id(): M3AST_AS.FORMAL_ID RAISES {}=
  BEGIN
    RETURN NEW(M3AST_AS.F_Readonly_id).init();
  END NewF_Readonly_id;


PROCEDURE NewF_Var_id(): M3AST_AS.FORMAL_ID RAISES {}=
  BEGIN
    RETURN NEW(M3AST_AS.F_Var_id).init();
  END NewF_Var_id;


PROCEDURE Formals(
    t: T;
    READONLY term: TokenSet)
    : SeqM3AST_AS_Formal_param.T
    RAISES {Rd.Failure}=
  VAR
    formals := SeqM3AST_AS_Formal_param.Null;
  BEGIN
    IF NOT At(t, M3CToken.Ket) THEN
      WITH
        formalTerm = term +
            TokenSet{M3CToken.Ket, M3CToken.Semicolon} + PossibleStartOfFormal
      DO
        REPEAT
          VAR
            create: PROCEDURE(): M3AST_AS.FORMAL_ID RAISES {};
            formal: M3AST_AS.Formal_param := NEW(M3AST_AS.Formal_param).init();
          BEGIN
            SeqM3AST_AS_Formal_param.AddRear(formals, formal);
            Pos(t, formal);
            formal.as_id_s := SeqM3AST_AS_FORMAL_ID.Null;
            IF At(t, M3CToken.VAR_) THEN
              create := NewF_Var_id;
            ELSIF At(t, M3CToken.READONLY_) THEN
              create := NewF_Readonly_id;
            ELSE
              EVAL At(t, M3CToken.VALUE_);
              create := NewF_Value_id;
            END;
            REPEAT
              WITH formalId = create() DO
                SeqM3AST_AS_FORMAL_ID.AddRear(formal.as_id_s, formalId);
                Id(t, formalId);
              END;
            UNTIL EndOfSequenceSet(t, M3CToken.Comma,
                TokenSet{M3CToken.Colon, M3CToken.Becomes}, IdAsSet, formalTerm);
            (* ( ":=" Expr & ":" Type ) *)
            formal.as_formal_type :=
                TypeAndOrDefault(t, formalTerm, formal.as_default);
          END;
        UNTIL EndOfSequence(t, M3CToken.Semicolon,
            M3CToken.Ket, PossibleStartOfFormal, term);
      END;
    END;
    RETURN formals;
  END Formals;


PROCEDURE Signature(
    t: T;
    READONLY term: TokenSet)
    : M3AST_AS.Procedure_type
    RAISES {Rd.Failure}=
  VAR
    p: M3AST_AS.Procedure_type := NEW(M3AST_AS.Procedure_type).init();
  BEGIN
    EVAL Expect(t, M3CToken.Bra, term + PossibleStartOfFormal +
        TokenSet{M3CToken.Ket, M3CToken.Colon, M3CToken.RAISES_});
    p.as_formal_param_s := Formals(t,
        term + TokenSet{M3CToken.Colon, M3CToken.RAISES_});
    WITH atColon = At(t, M3CToken.Colon) DO
      IF atColon OR t.lexer.current() IN StartOfType THEN
        IF NOT atColon THEN Expected(t, M3CToken.Colon) END;
        p.as_result_type := Type(t, term + TokenSet{M3CToken.RAISES_});
      END;
    END;
    IF t.lexer.current() = M3CToken.RAISES_ THEN
     WITH pos = t.lexer.position() DO
      EVAL t.lexer.next();
      IF At(t, M3CToken.ANY_) THEN
        p.as_raises := NEW(M3AST_AS.Raisees_any).init();
      ELSE
        p.as_raises := NEW(M3AST_AS.Raisees_some).init();
        WITH r = NARROW(p.as_raises, M3AST_AS.Raisees_some) DO
          r.as_raisees_s := SeqM3AST_AS_Qual_used_id.Null;
          EVAL Expect(t, M3CToken.CurlyBra, term);
          IF NOT At(t, M3CToken.CurlyKet) THEN
            REPEAT
              SeqM3AST_AS_Qual_used_id.AddRear(
                  r.as_raisees_s, QualId(t));
            UNTIL EndOfSequence(t, M3CToken.Comma, M3CToken.CurlyKet, IdAsSet, term);
          END;
        END;
      END;
      p.as_raises.lx_srcpos := pos;
     END;
    END;
    RETURN p;
  END Signature;


PROCEDURE ProcedureType(
    t: T;
    READONLY term: TokenSet)
    : M3AST_AS.Procedure_type
    RAISES {Rd.Failure}=
  VAR
    pos := t.lexer.position();
  BEGIN
    EVAL t.lexer.next();
    WITH p = Signature(t, term) DO
      p.lx_srcpos := pos;
      RETURN p;
    END;
  END ProcedureType;


PROCEDURE Record(
    t: T;
    READONLY term: TokenSet)
    : M3AST_AS.Record_type
    RAISES {Rd.Failure}=
  VAR
    r: M3AST_AS.Record_type := NEW(M3AST_AS.Record_type).init();
  BEGIN
    Pos(t, r, TRUE);
    IF t.lexer.current() # M3CToken.END_ THEN
      r.as_fields_s := Fields(t, EndAsSet, term);
    ELSE
      r.as_fields_s := SeqM3AST_AS_Fields.Null;
    END;
    EndPos(t);
    RETURN r;
  END Record;


PROCEDURE Ref(
    t: T;
    READONLY term: TokenSet;
    untraced: M3AST_AS.Untraced := NIL;
    brand: M3AST_AS.Brand := NIL)
    : M3AST_AS.Ref_type
    RAISES {Rd.Failure}=
  VAR
    r: M3AST_AS.Ref_type := NEW(M3AST_AS.Ref_type).init();
  BEGIN
    Pos(t, r, TRUE);
    r.as_trace_mode := untraced;
    r.as_brand := brand;
    r.as_type := Type(t, term);
    RETURN r;
  END Ref;


PROCEDURE Set(
    t: T;
    READONLY term: TokenSet)
    : M3AST_AS.Set_type
    RAISES {Rd.Failure}=
  VAR
    s: M3AST_AS.Set_type := NEW(M3AST_AS.Set_type).init();
  BEGIN
    Pos(t, s, TRUE);
    EVAL Expect(t, M3CToken.OF_, term + StartOfType);
    s.as_type := Type(t, term);
    RETURN s;
  END Set;


PROCEDURE Branded(
    t: T;
    READONLY term: TokenSet;
    untraced: M3AST_AS.Untraced := NIL;
    ancestor: M3AST_AS.M3TYPE := NIL)
    : M3AST_AS.M3TYPE
    RAISES {Rd.Failure}=
  VAR
    b: M3AST_AS.Brand := NEW(M3AST_AS.Brand).init();
  CONST
    StartOfBrandName = TokenSet{M3CToken.TextLiteral, M3CToken.Identifier};
  BEGIN
    Pos(t, b, TRUE);
    VAR
      expected: TokenSet;
      object := FALSE;
      type: M3AST_AS.M3TYPE;
    BEGIN
      IF ancestor # NIL THEN
        expected := TokenSet{M3CToken.OBJECT_};
      ELSIF untraced # NIL THEN
        expected := TokenSet{M3CToken.REF_};
      ELSE
        expected := TokenSet{M3CToken.OBJECT_, M3CToken.REF_};
      END;
      IF t.lexer.current() IN StartOfBrandName THEN
        b.as_exp := Expr(t, term + expected);
      END;
      EVAL ExpectSet(t, expected, term + StartOfType);
      CASE t.lexer.current() OF <*NOWARN*>
      | M3CToken.OBJECT_ =>
          type := Object(t, term, ancestor, b);
          object := TRUE;
      | M3CToken.REF_ =>
          type := Ref(t, term, untraced, b);
      ELSE
        type := Type(t, term);
      END;
      IF ancestor # NIL AND NOT object THEN
        RETURN ancestor;
      ELSE
        RETURN type;
      END;
    END;
  END Branded;


PROCEDURE Untraced(
    t: T;
    READONLY term: TokenSet)
    : M3AST_AS.M3TYPE
    RAISES {Rd.Failure}=
  CONST
    PossiblyUntraced =
        TokenSet{M3CToken.REF_, M3CToken.BRANDED_, M3CToken.ROOT_};
  VAR
    u := NEW(M3AST_AS.Untraced).init();
  BEGIN
    Pos(t, u, TRUE);
    EVAL ExpectSet(t, PossiblyUntraced, term + StartOfType);
    CASE t.lexer.current() OF
    | M3CToken.REF_ =>
        RETURN Ref(t, term, u);
    | M3CToken.BRANDED_ =>
        RETURN Branded(t, term, u);
    | M3CToken.ROOT_ =>
        VAR root: M3AST_AS.Root_type := NEW(M3AST_AS.Root_type).init();
    BEGIN
          Pos(t, root, TRUE);
          root.as_trace_mode := u;
          RETURN root;
        END;
    ELSE
      RETURN Type(t, term);
    END; (* if *)
  END Untraced;


PROCEDURE Enumeration(
    t: T;
    READONLY term: TokenSet)
    : M3AST_AS.Enumeration_type
    RAISES {Rd.Failure}=
  VAR
    e: M3AST_AS.Enumeration_type := NEW(M3AST_AS.Enumeration_type).init();
  BEGIN
    Pos(t, e, TRUE);
    e.as_id_s := SeqM3AST_AS_Enum_id.Null;
    IF NOT At(t, M3CToken.CurlyKet) THEN
      REPEAT
        VAR id: M3AST_AS.Enum_id := NEW(M3AST_AS.Enum_id).init();
    BEGIN
          SeqM3AST_AS_Enum_id.AddRear(e.as_id_s, id);
          Id(t, id);
        END;
      UNTIL EndOfSequence(t, M3CToken.Comma, M3CToken.CurlyKet, IdAsSet, term);
    END;
    RETURN e;
  END Enumeration;


PROCEDURE Range(exp1, exp2: M3AST_AS.EXP): M3AST_AS.Range RAISES {}=
  VAR
    r: M3AST_AS.Range := NEW(M3AST_AS.Range).init();
  BEGIN
    r.lx_srcpos := exp1.lx_srcpos;
    r.as_exp1 := exp1;
    r.as_exp2 := exp2;
    RETURN r;
  END Range;


PROCEDURE Subrange(
    t: T;
    READONLY term: TokenSet)
    : M3AST_AS.Subrange_type
    RAISES {Rd.Failure}=
  VAR
    s: M3AST_AS.Subrange_type := NEW(M3AST_AS.Subrange_type).init();
  BEGIN
    Pos(t, s, TRUE);
    WITH
      secondExprTerm = term + TokenSet{M3CToken.SquareKet},
      firstExprTerm = secondExprTerm
           + TokenSet{M3CToken.Range} + StartOfExpression,
      exp1 = Expr(t, firstExprTerm)
    DO
      EVAL Expect(t, M3CToken.Range, firstExprTerm);
      s.as_range := Range(exp1, Expr(t, secondExprTerm));
      EVAL Expect(t, M3CToken.SquareKet, secondExprTerm);
    END;
    RETURN s;
  END Subrange;


PROCEDURE Type(
    t: T;
    READONLY term: TokenSet)
    : M3AST_AS.M3TYPE
    RAISES {Rd.Failure}=
  BEGIN
    IF ExpectSet(t, StartOfType, term) THEN
      VAR
        type: M3AST_AS.M3TYPE;
      BEGIN
        CASE t.lexer.current() OF <*NOWARN*>
        | M3CToken.Identifier =>
            type := NamedType(QualId(t));
        | M3CToken.ADDRESS_ =>
            VAR address: M3AST_AS.Address_type := NEW(M3AST_AS.Address_type).init();
    BEGIN
              Pos(t, address, TRUE);
              type := address;
            END;
        | M3CToken.ARRAY_ =>
            type := Array(t, term);
        | M3CToken.BITS_ =>
            type := Packed(t, term);
        | M3CToken.INTEGER_ =>
            VAR integer: M3AST_AS.Integer_type := NEW(M3AST_AS.Integer_type).init();
    BEGIN
              Pos(t, integer, TRUE);
              type := integer;
            END;
        | M3CToken.LONGREAL_ =>
            VAR longreal: M3AST_AS.LongReal_type := NEW(M3AST_AS.LongReal_type).init();
    BEGIN
              Pos(t, longreal, TRUE);
              type := longreal;
            END;
        | M3CToken.EXTENDED_ =>
            VAR extended: M3AST_AS.Extended_type := NEW(M3AST_AS.Extended_type).init();
    BEGIN
              Pos(t, extended, TRUE);
              type := extended;
            END;
        | M3CToken.NULL_ =>
            VAR null: M3AST_AS.Null_type := NEW(M3AST_AS.Null_type).init();
    BEGIN
              Pos(t, null, TRUE);
              type := null;
            END;
        | M3CToken.OBJECT_ =>
            type := Object(t, term);
        | M3CToken.PROCEDURE_ =>
            type := ProcedureType(t, term);
        | M3CToken.REAL_ =>
            VAR real: M3AST_AS.Real_type := NEW(M3AST_AS.Real_type).init();
    BEGIN
              Pos(t, real, TRUE);
              type := real;
            END;
        | M3CToken.RECORD_ =>
            type := Record(t, term);
        | M3CToken.REF_ =>
            type := Ref(t, term);
        | M3CToken.REFANY_ =>
            VAR refany: M3AST_AS.RefAny_type := NEW(M3AST_AS.RefAny_type).init();
    BEGIN
              Pos(t, refany, TRUE);
              type := refany;
            END;
        | M3CToken.ROOT_ =>
            VAR root: M3AST_AS.Root_type := NEW(M3AST_AS.Root_type).init();
    BEGIN
              Pos(t, root, TRUE);
              type := root;
            END;
        | M3CToken.SET_ =>
            type := Set(t, term);
        | M3CToken.BRANDED_ =>
            type := Branded(t, term);
        | M3CToken.UNTRACED_ =>
            type := Untraced(t, term);
        | M3CToken.CurlyBra =>
            type := Enumeration(t, term);
        | M3CToken.SquareBra =>
            type := Subrange(t, term);
        | M3CToken.Bra =>
            EVAL t.lexer.next();
            type := Type(t, term + TokenSet{M3CToken.Ket});
            EVAL Expect(t, M3CToken.Ket, term);
        END; (* case *)
        type := ObjectCheck(t, term, type);
        EVAL ExpectSet(t, term);
        RETURN type;
      END;
    ELSE
      RETURN NEW(M3AST_AS.Bad_M3TYPE).init();
    END;
  END Type;


PROCEDURE NewNumericLiteral(token: Token): M3AST_AS.NUMERIC_LITERAL RAISES {}=
  BEGIN
    CASE token OF <*NOWARN*>
    | M3CToken.IntegerLiteral =>
        RETURN NEW(M3AST_AS.Integer_literal).init();
    | M3CToken.RealLiteral =>
        RETURN NEW(M3AST_AS.Real_literal).init();
    | M3CToken.LongRealLiteral =>
        RETURN NEW(M3AST_AS.LongReal_literal).init();
    | M3CToken.ExtendedLiteral =>
        RETURN NEW(M3AST_AS.Extended_literal).init();
    END; (* case *)
  END NewNumericLiteral;


PROCEDURE E8(
    t: T;
    READONLY term: TokenSet)
    : M3AST_AS.EXP_TYPE
    RAISES {Rd.Failure}=
  CONST
    NumericLiterals = TokenSet{
        M3CToken.IntegerLiteral, M3CToken.RealLiteral, M3CToken.LongRealLiteral,
        M3CToken.ExtendedLiteral};
  VAR
    token := t.lexer.current();
    result: M3AST_AS.EXP_TYPE;
  BEGIN
    CASE token OF
    | ORD(FIRST(M3CToken.Literal))..ORD(LAST(M3CToken.Literal)) =>
        IF token IN NumericLiterals THEN
          WITH numeric = NewNumericLiteral(token) DO
            numeric.lx_litrep := t.lexer.literal();
            result := numeric;
          END;
        ELSIF token = M3CToken.TextLiteral THEN
          VAR text: M3AST_AS.Text_literal := NEW(M3AST_AS.Text_literal).init();
    BEGIN
            text.lx_litrep := t.lexer.literal();
            result := text;
          END;
        ELSE
          VAR char: M3AST_AS.Char_literal := NEW(M3AST_AS.Char_literal).init();
    BEGIN
            char.lx_litrep := t.lexer.literal();
            result := char;
          END;
        END;
        Pos(t, result, TRUE);
    | M3CToken.NIL_ =>
        VAR nil: M3AST_AS.Nil_literal := NEW(M3AST_AS.Nil_literal).init();
    BEGIN
          Pos(t, nil, TRUE);
          nil.lx_litrep := t.nil_litrep;
          result := nil;
        END;
    | M3CToken.Identifier =>
        VAR expUsedId: M3AST_AS.Exp_used_id := NEW(M3AST_AS.Exp_used_id).init();
    BEGIN
          Pos(t, expUsedId);
          Id(t, expUsedId.vUSED_ID);
          t.lastSrcPosNode := expUsedId; (* cos Id sets it wrong *)
          result := expUsedId;
        END;
    | M3CToken.Bra =>
        EVAL t.lexer.next();
        result := Expr(t, term + TokenSet{M3CToken.Ket}, TRUE);
        EVAL Expect(t, M3CToken.Ket, term);
    ELSE
      result := Type(t, term);
    END; (* case *)
    EVAL ExpectSet(t, term);
    RETURN result;
  END E8;


PROCEDURE Select(
    t: T;
    lhs: M3AST_AS.EXP)
    : M3AST_AS.Select
    RAISES {Rd.Failure}=
  VAR
    s: M3AST_AS.Select := NEW(M3AST_AS.Select).init();
    expUsedId: M3AST_AS.Exp_used_id := NEW(M3AST_AS.Exp_used_id).init();
  BEGIN
    Pos(t, s, TRUE);
    Id(t, expUsedId.vUSED_ID);
    expUsedId.lx_srcpos := expUsedId.vUSED_ID.lx_srcpos;
    t.lastSrcPosNode := expUsedId;
    s.as_exp := lhs; s.as_id := expUsedId;
    RETURN s;
  END Select;


PROCEDURE Index(
    t: T;
    READONLY term: TokenSet;
    array: M3AST_AS.EXP)
    : M3AST_AS.Index
    RAISES {Rd.Failure}=
  VAR
    i: M3AST_AS.Index := NEW(M3AST_AS.Index).init();
  BEGIN
    EVAL t.lexer.next();
    i.lx_srcpos := array.lx_srcpos;
    i.as_array := array;
    i.as_exp_s := SeqM3AST_AS_EXP.Null;
    WITH indexTerm = term + TokenSet{M3CToken.Comma, M3CToken.SquareKet} +
        StartOfExpression DO
      REPEAT
        SeqM3AST_AS_EXP.AddRear(i.as_exp_s, Expr(t, indexTerm));
      UNTIL EndOfSequence(t, M3CToken.Comma,
          M3CToken.SquareKet, StartOfExpression, term);
    END;
    RETURN i;
  END Index;


PROCEDURE Call(
    t: T;
    READONLY term: TokenSet;
    callexp: M3AST_AS.EXP)
    : M3AST_AS.Call
    RAISES {Rd.Failure}=
  CONST
    PossibleStartOfActual = StartOfExpression +
        TokenSet{M3CToken.Identifier, M3CToken.Comma, M3CToken.Becomes};
  VAR
    c: M3AST_AS.Call := NIL;
  BEGIN
    (* Trap NEW(...) and use NEWCall instead of Call *)
    TYPECASE callexp OF
    | M3AST_AS.Exp_used_id(id) =>
        IF id.vUSED_ID.lx_symrep = t.idNEW THEN
          c := NEW(M3AST_AS.NEWCall).init();  
        END; (* if *)
    ELSE
    END; (* typecase *)
    IF c = NIL THEN c := NEW(M3AST_AS.Call).init() END;

    EVAL t.lexer.next();
    c.lx_srcpos := callexp.lx_srcpos;
    c.as_callexp := callexp;
    c.as_param_s := SeqM3AST_AS_Actual.Null;
    IF NOT At(t, M3CToken.Ket) THEN
      WITH actualTerm = term + TokenSet{M3CToken.Ket} + PossibleStartOfActual DO
        REPEAT
          VAR
            actual: M3AST_AS.Actual := NEW(M3AST_AS.Actual).init();
            expType := Expr(t, actualTerm, TRUE);
          BEGIN
            SeqM3AST_AS_Actual.AddRear(c.as_param_s, actual);
            actual.lx_srcpos := expType.lx_srcpos;
            IF ISTYPE(expType, M3AST_AS.EXP) AND At(t, M3CToken.Becomes) THEN
              actual.as_id := expType;
              actual.as_exp_type := Expr(t, actualTerm);
            ELSE
              actual.as_exp_type := expType;
            END;
          END;
        UNTIL EndOfSequence(t, M3CToken.Comma,
            M3CToken.Ket, PossibleStartOfActual, actualTerm);
      END;
    END;
    RETURN c;
  END Call;


PROCEDURE RangeExp(exp: M3AST_AS.EXP): M3AST_AS.Range_EXP RAISES {}=
  BEGIN
    VAR new: M3AST_AS.Range_EXP := NEW(M3AST_AS.Range_EXP).init();
    BEGIN
      new.lx_srcpos := exp.lx_srcpos;
      new.as_exp := exp;
      RETURN new;
    END;
  END RangeExp;


PROCEDURE Constructor(
    t: T;
    READONLY term: TokenSet;
    type: M3AST_AS.M3TYPE)
    : M3AST_AS.Constructor
    RAISES {Rd.Failure}=
  VAR
    c: M3AST_AS.Constructor := NEW(M3AST_AS.Constructor).init();
  BEGIN
    EVAL t.lexer.next();
    c.lx_srcpos := type.lx_srcpos;
    c.as_type := type;
    c.as_element_s := SeqM3AST_AS_CONS_ELEM.Null;
    IF NOT At(t, M3CToken.CurlyKet) THEN
      CONST
        PossibleStartOfElement = StartOfExpression +
            TokenSet{M3CToken.Identifier, M3CToken.Comma,
                M3CToken.Becomes, M3CToken.Range};
      VAR
        first := TRUE;
        elementTerm := term + TokenSet{M3CToken.CurlyKet} +
            PossibleStartOfElement;
      BEGIN
        REPEAT
          IF NOT first AND t.lexer.current() = M3CToken.Range THEN
            c.as_propagate := NEW(M3AST_AS.Propagate).init();
            Pos(t, c.as_propagate, TRUE);
            IF Expect(t, M3CToken.CurlyKet, elementTerm) THEN EXIT END;
            IF NOT t.lexer.current() IN PossibleStartOfElement THEN EXIT END;
          ELSE
            VAR
              element: M3AST_AS.CONS_ELEM;
              expr := Expr(t, elementTerm);
            BEGIN
              IF At(t, M3CToken.Becomes) THEN
                VAR actualElem: M3AST_AS.Actual_elem := NEW(M3AST_AS.Actual_elem).init();
                BEGIN
                  VAR actual: M3AST_AS.Actual := NEW(M3AST_AS.Actual).init();
                  BEGIN
                    actual.lx_srcpos := expr.lx_srcpos;
                    actual.as_id := expr;
                    actual.as_exp_type := Expr(t, elementTerm);
                    actualElem.lx_srcpos := actual.lx_srcpos;
                    actualElem.as_actual := actual;
                  END;
                  element := actualElem;
                END;
              ELSE
                VAR rangeExpElem: M3AST_AS.RANGE_EXP_elem := NEW(M3AST_AS.RANGE_EXP_elem).init();
                BEGIN
                  rangeExpElem.lx_srcpos := expr.lx_srcpos;
                  IF At(t, M3CToken.Range) THEN
                    rangeExpElem.as_range_exp :=
                        Range(expr, Expr(t, elementTerm));
                  ELSE
                    rangeExpElem.as_range_exp := RangeExp(expr);
                  END;
                  element := rangeExpElem;
                END;
              END;
              SeqM3AST_AS_CONS_ELEM.AddRear(c.as_element_s, element);
            END;
          END;
          first := FALSE;
        UNTIL EndOfSequence(t, M3CToken.Comma,
            M3CToken.CurlyKet, PossibleStartOfElement, elementTerm);
      END;
    END;
    RETURN c;
  END Constructor;


EXCEPTION
  IsType(M3AST_AS.M3TYPE);


PROCEDURE IsId(e: M3AST_AS.EXP): BOOLEAN RAISES {}=
  BEGIN
    TYPECASE e OF <*NOWARN*>
    | M3AST_AS.Exp_used_id =>
        RETURN TRUE;
    | M3AST_AS.Select(b) =>
        RETURN ISTYPE(b.as_exp, M3AST_AS.Exp_used_id);
    ELSE
      RETURN FALSE;
    END;
  END IsId;


PROCEDURE EXP_TYPEToM3TYPE(
    t: T;
    e: M3AST_AS.EXP_TYPE)
    : M3AST_AS.M3TYPE
    RAISES {}=
  BEGIN
    TYPECASE e OF <*NOWARN*>
    | M3AST_AS.M3TYPE(m3type) =>
        RETURN m3type;
    | M3AST_AS.Exp_used_id(usedId) =>
        RETURN NamedType(SingleIdQualId(
            t, usedId.vUSED_ID.lx_symrep, usedId.vUSED_ID.lx_srcpos));
    | M3AST_AS.Select(select) =>
        VAR
          e := select.as_exp;
          id := select.as_id;
        BEGIN
          RETURN NamedType(DoubleIdQualId(t,
              NARROW(e, M3AST_AS.Exp_used_id).vUSED_ID.lx_symrep,
              id.vUSED_ID.lx_symrep,
              e.lx_srcpos, id.lx_srcpos));
        END;
    END; (* typecase *)
  END EXP_TYPEToM3TYPE;


PROCEDURE E7(
    t: T;
    READONLY term: TokenSet;
    canBeType := FALSE)
    : M3AST_AS.EXP
    RAISES {Rd.Failure, IsType}=
  VAR
    e7Term := term +
        TokenSet{M3CToken.Dereference, M3CToken.Dot, M3CToken.SquareBra, M3CToken.Bra};
    e7FullTerm := e7Term +
        TokenSet{M3CToken.CurlyBra, M3CToken.OBJECT_, M3CToken.BRANDED_};
    bra := (t.lexer.current() = M3CToken.Bra);
    e8 := E8(t, e7FullTerm);
    token := t.lexer.current();
    e8IsType := ISTYPE(e8, M3AST_AS.M3TYPE);
    e8MayBeType := e8IsType OR IsId(e8);
  BEGIN
    IF token = M3CToken.Dot AND e8MayBeType AND NOT (e8IsType OR bra) THEN
      e8 := Select(t, e8);
      EVAL ExpectSet(t, e7FullTerm);
      token := t.lexer.current();
    END;

    IF e8MayBeType AND canBeType AND
      token IN TokenSet{M3CToken.OBJECT_, M3CToken.BRANDED_} THEN
      WHILE token IN TokenSet{M3CToken.OBJECT_, M3CToken.BRANDED_} DO
        e8IsType := TRUE;
        e8 := ObjectCheck(t, e7FullTerm, EXP_TYPEToM3TYPE(t, e8));
        EVAL ExpectSet(t, e7FullTerm);
        token := t.lexer.current();
      END;
    ELSIF token = M3CToken.CurlyBra THEN
      IF e8MayBeType THEN
        e8 := Constructor(t, e7Term, EXP_TYPEToM3TYPE(t, e8));
      END;
      EVAL ExpectSet(t, e7Term);
      token := t.lexer.current();
    ELSIF e8IsType THEN
      IF canBeType THEN
        RAISE IsType(e8);
      ELSE
        EVAL MustBeAt(t, M3CToken.CurlyBra);
        e8 := NEW(M3AST_AS.Bad_EXP).init();
      END;
    END;

    VAR
      result: M3AST_AS.EXP := e8;
    BEGIN    
      LOOP
        CASE token OF
        | M3CToken.Dereference =>
            VAR d: M3AST_AS.Deref := NEW(M3AST_AS.Deref).init();
    BEGIN
              d.as_exp := result;
              Pos(t, d, TRUE);
              result := d;
            END;
        | M3CToken.Dot =>
            result := Select(t, result);
        | M3CToken.SquareBra =>
            result := Index(t, e7Term, result);
        | M3CToken.Bra =>
            result := Call(t, e7Term, result);
        ELSE
          EXIT;
        END; (* case *)
        EVAL ExpectSet(t, e7Term);
        token := t.lexer.current();
      END; (* loop *)
      RETURN result;
    END;
  END E7;


PROCEDURE E6(
    t: T;
    READONLY term: TokenSet;
    canBeType := FALSE)
    : M3AST_AS.EXP
    RAISES {Rd.Failure, IsType}=
  VAR
    current := t.lexer.current();
  BEGIN
    IF current IN TokenSet{M3CToken.Plus, M3CToken.Minus} THEN
      VAR
        unary: M3AST_AS.UNARY;
      BEGIN
        IF current = M3CToken.Plus THEN
          unary := NEW(M3AST_AS.Unaryplus).init();
        ELSE
          unary := NEW(M3AST_AS.Unaryminus).init();
        END;
        Pos(t, unary, TRUE);
        unary.as_exp := E6(t, term);
        RETURN unary;
      END;
    ELSE
      RETURN E7(t, term, canBeType);
    END;
  END E6;


PROCEDURE E5(
    t: T;
    READONLY term: TokenSet;
    canBeType := FALSE)
    : M3AST_AS.EXP
    RAISES {Rd.Failure, IsType}=
  CONST
    Mulop = TokenSet{M3CToken.Times, M3CToken.Divide, M3CToken.DIV_, M3CToken.MOD_};
  VAR
    e5Term := term + Mulop;
    result := E6(t, e5Term, canBeType);
  BEGIN
    LOOP
      WITH current = t.lexer.current() DO
        IF current IN Mulop THEN
          VAR
            binary: M3AST_AS.BINARY;
          BEGIN
            CASE current OF <*NOWARN*>
            | M3CToken.Times =>  binary := NEW(M3AST_AS.Times).init();
            | M3CToken.Divide => binary := NEW(M3AST_AS.Rdiv).init();
            | M3CToken.DIV_ =>   binary := NEW(M3AST_AS.Div).init();
            | M3CToken.MOD_ =>   binary := NEW(M3AST_AS.Mod).init();
            END;
            Pos(t, binary, TRUE);
            binary.as_exp1 := result; binary.as_exp2 := E6(t, e5Term);
            result := binary;
          END;
        ELSE
          RETURN result;
        END;
      END;
    END;
  END E5;


PROCEDURE E4(
    t: T;
    READONLY term: TokenSet;
    canBeType := FALSE)
    : M3AST_AS.EXP
    RAISES {Rd.Failure, IsType}=
  CONST
    Addop = TokenSet{M3CToken.Plus, M3CToken.Minus, M3CToken.Ampersand};
  VAR
    e4Term := term + Addop;
    result := E5(t, e4Term, canBeType);
  BEGIN
    LOOP
      WITH current = t.lexer.current() DO
        IF current IN Addop THEN
          VAR
            binary: M3AST_AS.BINARY;
          BEGIN
            CASE current OF <*NOWARN*>
            | M3CToken.Plus =>  binary := NEW(M3AST_AS.Plus).init();
            | M3CToken.Minus => binary := NEW(M3AST_AS.Minus).init();
            | M3CToken.Ampersand =>   binary := NEW(M3AST_AS.Textcat).init();
            END;
            Pos(t, binary, TRUE);
            binary.as_exp1 := result; binary.as_exp2 := E5(t, e4Term);
            result := binary;
          END;
        ELSE
          RETURN result;
        END;
      END;
    END;
  END E4;


PROCEDURE E3(
    t: T;
    READONLY term: TokenSet;
    canBeType := FALSE)
    : M3AST_AS.EXP
    RAISES {Rd.Failure, IsType}=
  CONST
    Relop = TokenSet{M3CToken.Equal, M3CToken.NotEqual,
        M3CToken.LessThan, M3CToken.LessThanOrEqual,
        M3CToken.GreaterThan, M3CToken.GreaterThanOrEqual,
        M3CToken.IN_};
  VAR
    e3Term := term + Relop;
    result := E4(t, e3Term, canBeType);
  BEGIN
    LOOP
      WITH current = t.lexer.current() DO
        IF current IN Relop THEN
          VAR
            binary: M3AST_AS.BINARY;
          BEGIN
            CASE current OF <*NOWARN*>
            | M3CToken.Equal =>              binary := NEW(M3AST_AS.Eq).init();
            | M3CToken.NotEqual =>           binary := NEW(M3AST_AS.Ne).init();
            | M3CToken.LessThan =>           binary := NEW(M3AST_AS.Lt).init();
            | M3CToken.LessThanOrEqual =>    binary := NEW(M3AST_AS.Le).init();
            | M3CToken.GreaterThan =>        binary := NEW(M3AST_AS.Gt).init();
            | M3CToken.GreaterThanOrEqual => binary := NEW(M3AST_AS.Ge).init();
            | M3CToken.IN_ =>                binary := NEW(M3AST_AS.In).init();
            END;
            Pos(t, binary, TRUE);
            binary.as_exp1 := result; binary.as_exp2 := E4(t, e3Term);
            result := binary;
          END;
        ELSE
          RETURN result;
        END;
      END;
    END;
  END E3;


PROCEDURE E2(
    t: T;
    READONLY term: TokenSet;
    canBeType := FALSE)
    : M3AST_AS.EXP
    RAISES {Rd.Failure, IsType}=
  BEGIN
    IF t.lexer.current() = M3CToken.NOT_ THEN
      VAR not: M3AST_AS.Not := NEW(M3AST_AS.Not).init();
    BEGIN
        Pos(t, not, TRUE);
        not.as_exp := E2(t, term);
        RETURN not;
      END;
    ELSE
      RETURN E3(t, term, canBeType);
    END;
  END E2;


PROCEDURE E1(
    t: T;
    READONLY term: TokenSet;
    canBeType := FALSE)
    : M3AST_AS.EXP
    RAISES {Rd.Failure, IsType}=
  VAR
    e1Term := term + TokenSet{M3CToken.AND_};
    result := E2(t, e1Term, canBeType);
  BEGIN
    WHILE t.lexer.current() = M3CToken.AND_ DO
      VAR and: M3AST_AS.And := NEW(M3AST_AS.And).init();
    BEGIN
        Pos(t, and, TRUE);
        and.as_exp1 := result; and.as_exp2 := E2(t, e1Term);
        result := and;
      END;
    END;
    RETURN result;
  END E1;


PROCEDURE Expr(
    t: T;
    READONLY term: TokenSet;
    canBeType := FALSE)
    : M3AST_AS.EXP_TYPE
    RAISES {Rd.Failure}=
  BEGIN
    IF ExpectSet(t, StartOfExpression, term) THEN
      TRY
        VAR
          exprTerm := term + TokenSet{M3CToken.OR_};
          result := E1(t, exprTerm, canBeType);
        BEGIN
          WHILE t.lexer.current() = M3CToken.OR_ DO
            VAR or: M3AST_AS.Or := NEW(M3AST_AS.Or).init();
    BEGIN
              Pos(t, or, TRUE);
              or.as_exp1 := result; or.as_exp2 := E1(t, exprTerm);
              result := or;
            END;
          END;
          RETURN result;
        END;
      EXCEPT
      | IsType(type) => RETURN type;
      END;
    ELSE
      RETURN NEW(M3AST_AS.Bad_EXP).init();
    END;
  END Expr;


PROCEDURE Else(
    t: T;
    READONLY term: TokenSet;)
    : M3AST_AS.Else_stm_NULL
    RAISES {Rd.Failure}=
  BEGIN
    IF t.lexer.current() = M3CToken.ELSE_ THEN
      VAR e: M3AST_AS.Else_stm := NEW(M3AST_AS.Else_stm).init();
    BEGIN
        Pos(t, e, TRUE);
        e.as_stm_s := StmtsThenEnd(t, term);
        RETURN e;
      END;
    ELSE
      EndPos(t);
      RETURN NIL;
    END; (* if *)
  END Else;


PROCEDURE Case(
    t: T;
    READONLY term: TokenSet)
    : M3AST_AS.Case_st
    RAISES {Rd.Failure}=
  VAR
    possibleStartOfCase := StartOfExpression + StartOfStatement +
        TokenSet{M3CToken.Bar, M3CToken.Range, M3CToken.Implies};
    caseTerm := term + possibleStartOfCase + TokenSet{M3CToken.END_, M3CToken.ELSE_};
    caseLabelTerm := caseTerm + TokenSet{M3CToken.Comma};
    case_st: M3AST_AS.Case_st := NEW(M3AST_AS.Case_st).init();
  BEGIN
    Pos(t, case_st, TRUE);
    case_st.as_exp := Expr(t, caseTerm + TokenSet{M3CToken.OF_});
    EVAL MustBeAt(t, M3CToken.OF_);
    case_st.as_case_s := SeqM3AST_AS_Case.Null;
    IF NOT t.lexer.current() IN TokenSet{M3CToken.ELSE_, M3CToken.END_} THEN
      EVAL At(t, M3CToken.Bar);
      REPEAT
        VAR case: M3AST_AS.Case := NEW(M3AST_AS.Case).init();
        BEGIN
          SeqM3AST_AS_Case.AddRear(case_st.as_case_s, case);
          Pos(t, case);
          case.as_case_label_s := SeqM3AST_AS_RANGE_EXP.Null;
          REPEAT
            VAR
              rangeExp: M3AST_AS.RANGE_EXP;
              exp := Expr(t, caseLabelTerm);
            BEGIN
              IF At(t, M3CToken.Range) THEN
                rangeExp := Range(exp, Expr(t, caseLabelTerm));
              ELSE
                rangeExp := RangeExp(exp);
              END;
              SeqM3AST_AS_RANGE_EXP.AddRear(case.as_case_label_s, rangeExp);
            END;
          UNTIL EndOfSequence(t, M3CToken.Comma,
              M3CToken.Implies, StartOfExpression + TokenSet{M3CToken.Range},
              caseTerm);
          case.as_stm_s := Stmts(t,
              TokenSet{M3CToken.Bar, M3CToken.ELSE_, M3CToken.END_}, caseTerm);
        END;
      UNTIL EndOfSequenceSet(t, M3CToken.Bar,
          ElseOrEnd, possibleStartOfCase, caseTerm);
    END;
    case_st.as_else := Else(t, term);
    RETURN case_st;
  END Case;


PROCEDURE Exit(t: T): M3AST_AS.Exit_st RAISES {Rd.Failure}=
  VAR
    e := NEW(M3AST_AS.Exit_st).init();
  BEGIN
    Pos(t, e, TRUE);
    RETURN e;
  END Exit;


PROCEDURE Eval(
    t: T;
    READONLY term: TokenSet)
    : M3AST_AS.Eval_st
    RAISES {Rd.Failure}=
  VAR
    e: M3AST_AS.Eval_st := NEW(M3AST_AS.Eval_st).init();
  BEGIN
    Pos(t, e, TRUE);
    e.as_exp := Expr(t, term);
    RETURN e;
  END Eval;


PROCEDURE For(
    t: T;
    READONLY term: TokenSet)
    : M3AST_AS.For_st
    RAISES {Rd.Failure}=
  VAR
    forTerm := term + StartOfStatement +
        TokenSet{M3CToken.TO_, M3CToken.BY_, M3CToken.DO_, M3CToken.END_};
    f: M3AST_AS.For_st := NEW(M3AST_AS.For_st).init();
  BEGIN
    Pos(t, f, TRUE);
    f.as_id := NEW(M3AST_AS.For_id).init();
    Id(t, f.as_id);
    EVAL Expect(t, M3CToken.Becomes, forTerm);
    f.as_from := Expr(t, forTerm);
    EVAL Expect(t, M3CToken.TO_, forTerm);
    f.as_to := Expr(t, forTerm - TokenSet{M3CToken.TO_});
    IF t.lexer.current() = M3CToken.BY_ THEN
      f.as_by := NEW(M3AST_AS.By).init();
      Pos(t, f.as_by, TRUE);
      f.as_by.as_exp := Expr(t, forTerm - TokenSet{M3CToken.TO_, M3CToken.BY_});
    END;
    EVAL Expect(t, M3CToken.DO_, forTerm - TokenSet{M3CToken.TO_, M3CToken.BY_});
    f.as_stm_s := StmtsThenEnd(t, term);
    RETURN f;
  END For;


PROCEDURE If(
    t: T;
    READONLY term: TokenSet)
    : M3AST_AS.If_st
    RAISES {Rd.Failure}=
  CONST
    EndOfIfArm = TokenSet{M3CToken.ELSE_, M3CToken.ELSIF_, M3CToken.END_};
  VAR
    ifExprTerm := term + StartOfStatement + EndOfIfArm + TokenSet{M3CToken.THEN_};
    if: M3AST_AS.If_st := NEW(M3AST_AS.If_st).init();
  BEGIN
    Pos(t, if, TRUE);
    if.as_exp := Expr(t, ifExprTerm);
    EVAL MustBeAt(t, M3CToken.THEN_);
    if.as_stm_s := Stmts(t, EndOfIfArm, term);
    if.as_elsif_s := SeqM3AST_AS_Elsif.Null;
    WHILE t.lexer.current() = M3CToken.ELSIF_ DO
      VAR elsif: M3AST_AS.Elsif := NEW(M3AST_AS.Elsif).init();
    BEGIN
        SeqM3AST_AS_Elsif.AddRear(if.as_elsif_s, elsif);
        Pos(t, elsif, TRUE);
        elsif.as_exp := Expr(t, ifExprTerm);
        EVAL MustBeAt(t, M3CToken.THEN_);
        elsif.as_stm_s := Stmts(t, EndOfIfArm, term);
      END;
    END;
    if.as_else := Else(t, term);
    RETURN if;
  END If;


PROCEDURE Lock(
    t: T;
    READONLY term: TokenSet)
    : M3AST_AS.Lock_st
    RAISES {Rd.Failure}=
  VAR
    l: M3AST_AS.Lock_st := NEW(M3AST_AS.Lock_st).init();
  BEGIN
    Pos(t, l, TRUE);
    l.as_exp := Expr(t, term + EndAsSet + TokenSet{M3CToken.DO_});
    EVAL MustBeAt(t, M3CToken.DO_);
    l.as_stm_s := StmtsThenEnd(t, term);
    RETURN l;
  END Lock;


PROCEDURE Loop(
    t: T;
    READONLY term: TokenSet)
    : M3AST_AS.Loop_st
    RAISES {Rd.Failure}=
  VAR
    l: M3AST_AS.Loop_st := NEW(M3AST_AS.Loop_st).init();
  BEGIN
    Pos(t, l, TRUE);
    l.as_stm_s := StmtsThenEnd(t, term);
    RETURN l;
  END Loop;


PROCEDURE Repeat(
    t: T;
    READONLY term: TokenSet)
    : M3AST_AS.Repeat_st
    RAISES {Rd.Failure}=
  VAR
    r: M3AST_AS.Repeat_st := NEW(M3AST_AS.Repeat_st).init();
  BEGIN
    Pos(t, r, TRUE);
    r.as_stm_s := Stmts(t, TokenSet{M3CToken.UNTIL_}, term);
    EndPos(t, M3CToken.UNTIL_);
    r.as_exp := Expr(t, term);
    RETURN r;
  END Repeat;


PROCEDURE Raise(
    t: T;
    READONLY term: TokenSet)
    : M3AST_AS.Raise_st
    RAISES {Rd.Failure}=
  VAR
    r: M3AST_AS.Raise_st := NEW(M3AST_AS.Raise_st).init();
  BEGIN
    Pos(t, r, TRUE);
    r.as_qual_id := QualId(t);
    EVAL ExpectSet(t, term + TokenSet{M3CToken.Bra});
    IF At(t, M3CToken.Bra) THEN
      r.as_exp_void := Expr(t, term + TokenSet{M3CToken.Ket});
      EVAL MustBeAt(t, M3CToken.Ket);
    END;
    RETURN r;
  END Raise;


PROCEDURE Return(
    t: T;
    READONLY term: TokenSet)
    : M3AST_AS.Return_st
    RAISES {Rd.Failure}=
  VAR
    r: M3AST_AS.Return_st := NEW(M3AST_AS.Return_st).init();
  BEGIN
    Pos(t, r, TRUE);
    IF NOT t.lexer.current() IN term - StartOfExpression THEN
      r.as_exp := Expr(t, term);
    END;
    RETURN r;
  END Return;


PROCEDURE Try(
    t: T;
    READONLY term: TokenSet)
    : M3AST_AS.Try_st
    RAISES {Rd.Failure}=
  VAR
    try: M3AST_AS.Try_st := NEW(M3AST_AS.Try_st).init();
  BEGIN
    Pos(t, try, TRUE);
    try.as_stm_s := Stmts(t, TokenSet{M3CToken.FINALLY_, M3CToken.EXCEPT_},
        term + TokenSet{M3CToken.Bar, M3CToken.ELSE_, M3CToken.END_});
    IF t.lexer.current() = M3CToken.FINALLY_ THEN
      VAR f: M3AST_AS.Try_finally := NEW(M3AST_AS.Try_finally).init();
    BEGIN
        try.as_try_tail := f;
        Pos(t, f, TRUE);
        f.as_stm_s := StmtsThenEnd(t, term);
      END;
    ELSE
      VAR e: M3AST_AS.Try_except := NEW(M3AST_AS.Try_except).init();
      BEGIN
        try.as_try_tail := e;
        Pos(t, e);
        EVAL At(t, M3CToken.EXCEPT_);
        e.as_handler_s := SeqM3AST_AS_Handler.Null;
        IF NOT t.lexer.current() IN TokenSet{M3CToken.ELSE_, M3CToken.END_} THEN
          VAR
            possibleStartOfHandler := StartOfStatement + IdAsSet +
                TokenSet{M3CToken.Bar, M3CToken.Bra, M3CToken.Implies};
            handlerTerm := term + possibleStartOfHandler +
                TokenSet{M3CToken.END_, M3CToken.ELSE_};
          BEGIN
            EVAL At(t, M3CToken.Bar);
            REPEAT
              VAR h: M3AST_AS.Handler := NEW(M3AST_AS.Handler).init();
              BEGIN
                SeqM3AST_AS_Handler.AddRear(e.as_handler_s, h);
                Pos(t, h);
                h.as_qual_id_s := SeqM3AST_AS_Qual_used_id.Null;
                REPEAT
                  SeqM3AST_AS_Qual_used_id.AddRear(h.as_qual_id_s, QualId(t));
                UNTIL EndOfSequenceSet(t, M3CToken.Comma,
                    TokenSet{M3CToken.Bra, M3CToken.Implies}, IdAsSet, handlerTerm);
                IF At(t, M3CToken.Bra) THEN
                  h.as_id := NEW(M3AST_AS.Handler_id).init();
                  Id(t, h.as_id);
                  EVAL Expect(t, M3CToken.Ket, handlerTerm);
                END;
                EVAL LenientMustBeAt(t, M3CToken.Implies, M3CToken.Colon);
                h.as_stm_s := Stmts(t,
                    TokenSet{M3CToken.Bar, M3CToken.ELSE_, M3CToken.END_}, handlerTerm);
              END;
            UNTIL EndOfSequenceSet(t, M3CToken.Bar,
                ElseOrEnd, possibleStartOfHandler, handlerTerm);
          END;
        END;
        e.as_else := Else(t, term);
      END;
    END;
    RETURN try;
  END Try;


PROCEDURE Typecase(
    t: T;
    READONLY term: TokenSet)
    : M3AST_AS.Typecase_st
    RAISES {Rd.Failure}=
  VAR
    possibleStartOfTypecase := StartOfStatement + StartOfType +
        TokenSet{M3CToken.Bar, M3CToken.Bra, M3CToken.Implies};
    typecaseTerm := term + possibleStartOfTypecase +
        TokenSet{M3CToken.END_, M3CToken.ELSE_};
    typecaseLabelTerm := typecaseTerm + TokenSet{M3CToken.Comma};
  VAR
    typecase_st: M3AST_AS.Typecase_st := NEW(M3AST_AS.Typecase_st).init();
  BEGIN
    Pos(t, typecase_st, TRUE);
    typecase_st.as_exp := Expr(t, typecaseTerm + TokenSet{M3CToken.OF_});
    EVAL Expect(t, M3CToken.OF_, typecaseTerm);
    typecase_st.as_tcase_s := SeqM3AST_AS_Tcase.Null;
    IF NOT t.lexer.current() IN TokenSet{M3CToken.ELSE_, M3CToken.END_} THEN
      EVAL At(t, M3CToken.Bar);
      REPEAT
        VAR tcase: M3AST_AS.Tcase := NEW(M3AST_AS.Tcase).init();
    BEGIN
          SeqM3AST_AS_Tcase.AddRear(typecase_st.as_tcase_s, tcase);
          Pos(t, tcase);
          tcase.as_type_s := SeqM3AST_AS_M3TYPE.Null;
          REPEAT
            SeqM3AST_AS_M3TYPE.AddRear(tcase.as_type_s,
                Type(t, typecaseLabelTerm));
          UNTIL EndOfSequenceSet(t, M3CToken.Comma,
              TokenSet{M3CToken.Bra, M3CToken.Implies}, StartOfType, typecaseTerm);
          IF At(t, M3CToken.Bra) THEN
            tcase.as_id := NEW(M3AST_AS.Tcase_id).init();
            Id(t, tcase.as_id);
            EVAL Expect(t, M3CToken.Ket, typecaseTerm);
          END;
          EVAL Expect(t, M3CToken.Implies, typecaseTerm);
          tcase.as_stm_s := Stmts(
              t, TokenSet{M3CToken.Bar, M3CToken.ELSE_, M3CToken.END_}, typecaseTerm);
        END;
      UNTIL EndOfSequenceSet(t, M3CToken.Bar,
          ElseOrEnd, possibleStartOfTypecase, typecaseTerm);
    END;
    typecase_st.as_else := Else(t, term);
    RETURN typecase_st;
  END Typecase;


PROCEDURE While(
    t: T;
    READONLY term: TokenSet)
    : M3AST_AS.While_st
    RAISES {Rd.Failure}=
  VAR
    w: M3AST_AS.While_st := NEW(M3AST_AS.While_st).init();
  BEGIN
    Pos(t, w, TRUE);
    w.as_exp := Expr(t, term + TokenSet{M3CToken.DO_, M3CToken.END_});
    EVAL MustBeAt(t, M3CToken.DO_);
    w.as_stm_s := StmtsThenEnd(t, term);
    RETURN w;
  END While;


PROCEDURE With(
    t: T;
    READONLY term: TokenSet)
    : M3AST_AS.With_st
    RAISES {Rd.Failure}=
  VAR
    possibleStartOfBinding := TokenSet{M3CToken.Identifier, M3CToken.Equal} +
        StartOfExpression;
    bindingTerm := term + StartOfStatement + possibleStartOfBinding +
        TokenSet{M3CToken.Comma, M3CToken.DO_, M3CToken.END_};
    w: M3AST_AS.With_st := NEW(M3AST_AS.With_st).init();
  BEGIN
    Pos(t, w, TRUE);
    w.as_binding_s := SeqM3AST_AS_Binding.Null;
    REPEAT
      VAR b: M3AST_AS.Binding := NEW(M3AST_AS.Binding).init();
    BEGIN
        SeqM3AST_AS_Binding.AddRear(w.as_binding_s, b);
        Pos(t, b);
        b.as_id := NEW(M3AST_AS.With_id).init();
        Id(t, b.as_id);
        EVAL Expect(t, M3CToken.Equal, bindingTerm);
        b.as_exp := Expr(t, bindingTerm);
      END;
    UNTIL EndOfSequence(t, M3CToken.Comma,
        M3CToken.DO_, possibleStartOfBinding, bindingTerm);
    w.as_stm_s := StmtsThenEnd(t, term);
    RETURN w;
  END With;


<*INLINE*> PROCEDURE ExprOrInit(
    t: T;
    READONLY term: TokenSet;
    VAR init: M3AST_AS.EXP)
    : M3AST_AS.EXP
    RAISES {Rd.Failure}=
  VAR
    old := init;
  BEGIN
    IF old = NIL THEN
      RETURN Expr(t, term);
    ELSE
      init := NIL;
      RETURN old;
    END;
  END ExprOrInit;


PROCEDURE Stmts(
    t: T;
    READONLY validTerm, term: TokenSet;
    initialExp: M3AST_AS.EXP := NIL)
    : SeqM3AST_AS_STM.T
    RAISES {Rd.Failure}=
  VAR
    fullTerm := validTerm + term;
    result := SeqM3AST_AS_STM.Null;
  BEGIN
    IF initialExp = NIL AND t.lexer.current() IN validTerm THEN
      RETURN result;
    ELSIF initialExp # NIL OR ExpectSet(t, StartOfStatement, fullTerm) THEN
      WITH stmtTerm = fullTerm + TokenSet{M3CToken.Semicolon} DO
        LOOP
          VAR
            token := t.lexer.current();
            stm: M3AST_AS.STM;
          BEGIN
            IF initialExp = NIL AND token IN StartOfBlock THEN
              stm := Block(t, stmtTerm);
            ELSIF initialExp # NIL OR token IN StartOfExpression THEN
              VAR
                lhsTerm := stmtTerm + TokenSet{M3CToken.Becomes};
                exp := ExprOrInit(t, lhsTerm, initialExp);
                assignment: BOOLEAN;
                isCall := ISTYPE(exp, M3AST_AS.Call);
              BEGIN
                IF isCall THEN
                  EVAL ExpectSet(t, lhsTerm);
                  assignment := At(t, M3CToken.Becomes);
                ELSE
                  assignment := Expect(t, M3CToken.Becomes, stmtTerm);
                END;
                IF isCall AND NOT assignment THEN
                  VAR c: M3AST_AS.Call_st := NEW(M3AST_AS.Call_st).init();
    BEGIN
                    c.lx_srcpos := exp.lx_srcpos;
                    c.as_call := exp;
                    stm := c;
                  END;
                ELSE
                  VAR a: M3AST_AS.Assign_st := NEW(M3AST_AS.Assign_st).init();
    BEGIN
                    a.lx_srcpos := exp.lx_srcpos;
                    a.as_lhs_exp := exp;
                    IF assignment THEN
                      a.as_rhs_exp := Expr(t, stmtTerm);
                    ELSE
                      a.as_rhs_exp := NEW(M3AST_AS.Bad_EXP).init();
                    END;
                    stm := a;
                  END;
                END;
              END;
            ELSE
              CASE token OF <*NOWARN*>
              | M3CToken.CASE_ => stm := Case(t, stmtTerm);
              | M3CToken.EXIT_ => stm := Exit(t);
              | M3CToken.EVAL_ => stm := Eval(t, stmtTerm);
              | M3CToken.FOR_ => stm := For(t, stmtTerm);
              | M3CToken.IF_ => stm := If(t, stmtTerm);
              | M3CToken.LOCK_ => stm := Lock(t, stmtTerm);
              | M3CToken.LOOP_ => stm := Loop(t, stmtTerm);
              | M3CToken.RAISE_ => stm := Raise(t, stmtTerm);
              | M3CToken.REPEAT_ => stm := Repeat(t, stmtTerm);
              | M3CToken.RETURN_ => stm := Return(t, stmtTerm);
              | M3CToken.TRY_ => stm := Try(t, stmtTerm);
              | M3CToken.TYPECASE_ => stm := Typecase(t, stmtTerm);
              | M3CToken.WHILE_ => stm := While(t, stmtTerm);
              | M3CToken.WITH_ => stm := With(t, stmtTerm);
              END;
            END;
            SeqM3AST_AS_STM.AddRear(result, stm);
            WITH exit = EndOfSequenceSet(t, M3CToken.Semicolon,
                validTerm, StartOfStatement, term) DO
              IF t.lastPragma # NIL THEN
                M3CPragma.AddPrecedingStmOrDecl(stm, t.pragmas);
              END;
              IF exit THEN EXIT END;
            END;
          END;
        END;
      END;
    END;
    RETURN result;
  END Stmts;


PROCEDURE StmtsThenEnd(
    t: T;
    READONLY term: TokenSet)
    : SeqM3AST_AS_STM.T
    RAISES {Rd.Failure}=
  BEGIN
    WITH result = Stmts(t, EndAsSet, term) DO
      EndPos(t);
      RETURN result;
    END;
  END StmtsThenEnd;


PROCEDURE EndOfDecl(
    t: T;
    decl: M3AST.NODE;
    READONLY term: TokenSet)
    : BOOLEAN
    RAISES {Rd.Failure}=
  BEGIN
    EVAL Expect(t, M3CToken.Semicolon, term + IdAsSet);
    LOOP
      WITH token = t.lexer.current() DO 
        IF token = M3CToken.Semicolon THEN
          Unexpected(t);
          EVAL t.lexer.next();
        ELSE
          IF t.lastPragma # NIL THEN
            M3CPragma.AddPrecedingStmOrDecl(decl, t.pragmas);
          END;
          RETURN token # M3CToken.Identifier;
        END;
      END;
    END;
  END EndOfDecl;


PROCEDURE ConstDecl(
    t: T;
    READONLY term: TokenSet)
    : M3AST_AS.Const_decl_s
    RAISES {Rd.Failure}=
  VAR
    constDeclS: M3AST_AS.Const_decl_s := NEW(M3AST_AS.Const_decl_s).init();
  BEGIN
    Pos(t, constDeclS, TRUE);
    constDeclS.as_const_decl_s := SeqM3AST_AS_Const_decl.Null;
    IF NOT t.lexer.current() IN StartOfBlock THEN
      LOOP
        VAR c: M3AST_AS.Const_decl := NEW(M3AST_AS.Const_decl).init();
    BEGIN
          SeqM3AST_AS_Const_decl.AddRear(constDeclS.as_const_decl_s, c);
          Pos(t, c);
          c.as_id := NEW(M3AST_AS.Const_id).init();
          Id(t, c.as_id);
          IF At(t, M3CToken.Colon) THEN
            c.as_type := Type(t, term + TokenSet{M3CToken.Equal});
          END;
          EVAL Expect(t, M3CToken.Equal, term);
          c.as_exp := Expr(t, term);
          IF EndOfDecl(t, c, term) THEN EXIT END;
        END;
      END;
    END;
    RETURN constDeclS;
  END ConstDecl;


PROCEDURE Opaque(t: M3AST_AS.M3TYPE): M3AST_AS.M3TYPE RAISES {} =
  VAR
    new: M3AST_AS.Opaque_type := NEW(M3AST_AS.Opaque_type).init();
  BEGIN
    new.lx_srcpos := t.lx_srcpos;
    new.as_type := t;
    RETURN new;
  END Opaque;


PROCEDURE TypeDecl(
    t: T;
    READONLY term: TokenSet)
    : M3AST_AS.Type_decl_s
    RAISES {Rd.Failure}=
  VAR
    typeDeclS: M3AST_AS.Type_decl_s := NEW(M3AST_AS.Type_decl_s).init();
  BEGIN
    Pos(t, typeDeclS, TRUE);
    typeDeclS.as_type_decl_s := SeqM3AST_AS_TYPE_DECL.Null;
    IF NOT t.lexer.current() IN StartOfBlock THEN
      LOOP
        VAR
          td: M3AST_AS.TYPE_DECL;
          id: M3AST_AS.Type_id := NEW(M3AST_AS.Type_id).init();
          opaque: BOOLEAN;
        BEGIN
          Id(t, id);
          opaque := At(t, M3CToken.Subtype);
          IF opaque THEN
            td := NEW(M3AST_AS.Subtype_decl).init();
          ELSE
            EVAL Expect(t, M3CToken.Equal, term + StartOfType);
            td := NEW(M3AST_AS.Concrete_decl).init();
          END;
          SeqM3AST_AS_TYPE_DECL.AddRear(typeDeclS.as_type_decl_s, td);
          td.lx_srcpos := id.lx_srcpos;
          td.as_id := id;
          td.as_type := Type(t, term);
          IF opaque THEN
            td.as_type := Opaque(td.as_type);
          END;
          IF EndOfDecl(t, td, term) THEN EXIT END;
        END;
      END;
    END;
    RETURN typeDeclS;
  END TypeDecl;


PROCEDURE ExceptionDecl(
    t: T;
    READONLY term: TokenSet)
    : M3AST_AS.Exc_decl_s
    RAISES {Rd.Failure}=
  VAR
    excDeclS: M3AST_AS.Exc_decl_s := NEW(M3AST_AS.Exc_decl_s).init();
  BEGIN
    Pos(t, excDeclS, TRUE);
    excDeclS.as_exc_decl_s := SeqM3AST_AS_Exc_decl.Null;
    IF NOT t.lexer.current() IN StartOfBlock THEN
      LOOP
        VAR e: M3AST_AS.Exc_decl := NEW(M3AST_AS.Exc_decl).init();
        BEGIN
          SeqM3AST_AS_Exc_decl.AddRear(excDeclS.as_exc_decl_s, e);
          Pos(t, e);
          e.as_id := NEW(M3AST_AS.Exc_id).init();
          Id(t, e.as_id);
          IF At(t, M3CToken.Bra) THEN
            e.as_type := Type(t, term + TokenSet{M3CToken.Ket});
            EVAL Expect(t, M3CToken.Ket, term);
          END;
          IF EndOfDecl(t, e, term) THEN EXIT END;
        END;
      END;
    END;
    RETURN excDeclS;
  END ExceptionDecl;


PROCEDURE IdAfterEnd(t: T; id: M3CLex.Symbol_rep) RAISES {Rd.Failure}=
  BEGIN
    IF t.lexer.current() = M3CToken.Identifier THEN
      IF id # NIL AND id # t.lexer.identifier() THEN
        ErrorMessage(t,
            Fmt.F("name after END should be \'%s\'", id.toText()));
      END;
      EVAL t.lexer.next();
    ELSE
      Expected(t, M3CToken.Identifier);
    END;
  END IdAfterEnd;


PROCEDURE ProcedureDecl(
    t: T;
    READONLY term: TokenSet)
    : M3AST_AS.Proc_decl
    RAISES {Rd.Failure}=
  VAR
    p: M3AST_AS.Proc_decl := NEW(M3AST_AS.Proc_decl).init();
  BEGIN
    Pos(t, p, TRUE);
    p.as_id := NEW(M3AST_AS.Proc_id).init();
    Id(t, p.as_id);
    WITH pos = t.lexer.position() DO
      p.as_type := Signature(t, term + TokenSet{M3CToken.Equal} + StartOfBlock);
      p.as_type.lx_srcpos := pos;
    END;
    EVAL ExpectSet(t,
        TokenSet{M3CToken.Equal, M3CToken.Semicolon}, StartOfStatement + term);
    IF t.interface THEN
      EVAL MustBeAt(t, M3CToken.Semicolon);
    ELSE
      EVAL MustBeAt(t, M3CToken.Equal);
      EVAL ExpectSet(t, StartOfBlock, StartOfStatement + term);
      p.as_body := Block(t, term);
      IdAfterEnd(t, p.as_id.lx_symrep);
      EVAL Expect(t, M3CToken.Semicolon, term);
    END;
    IF t.lastPragma # NIL THEN
      M3CPragma.AddPrecedingStmOrDecl(p, t.pragmas);
    END;
    RETURN p;
  END ProcedureDecl;


PROCEDURE VarDecl(
    t: T;
    READONLY term: TokenSet)
    : M3AST_AS.Var_decl_s
    RAISES {Rd.Failure}=
  VAR
    varTerm := term + TokenSet{M3CToken.Colon, M3CToken.Becomes} + StartOfType +
        StartOfExpression;
    varDeclS: M3AST_AS.Var_decl_s := NEW(M3AST_AS.Var_decl_s).init();
  BEGIN
    Pos(t, varDeclS, TRUE);
    varDeclS.as_var_decl_s := SeqM3AST_AS_Var_decl.Null;
    IF NOT t.lexer.current() IN StartOfBlock THEN
      LOOP
        VAR v: M3AST_AS.Var_decl := NEW(M3AST_AS.Var_decl).init();
        BEGIN
          SeqM3AST_AS_Var_decl.AddRear(varDeclS.as_var_decl_s, v);
          Pos(t, v);
          v.as_id_s := SeqM3AST_AS_Var_id.Null;
          REPEAT
            VAR id: M3AST_AS.Var_id := NEW(M3AST_AS.Var_id).init();
    BEGIN
              SeqM3AST_AS_Var_id.AddRear(v.as_id_s, id);
              Id(t, id);
            END;
          UNTIL EndOfSequenceSet(t, M3CToken.Comma,
              TokenSet{M3CToken.Colon, M3CToken.Becomes}, IdAsSet, varTerm);
          v.as_type := TypeAndOrDefault(t, varTerm, v.as_default);
          IF EndOfDecl(t, v, term) THEN EXIT END;
        END;
      END;
    END;
    RETURN varDeclS;
  END VarDecl;


PROCEDURE Reveal(
    t: T;
    READONLY term: TokenSet)
    : M3AST_AS.Revelation_s
    RAISES {Rd.Failure}=
  VAR
    revelationS: M3AST_AS.Revelation_s := NEW(M3AST_AS.Revelation_s).init();
  BEGIN
    Pos(t, revelationS, TRUE);
    revelationS.as_reveal_s := SeqM3AST_AS_REVELATION.Null;
    IF NOT t.lexer.current() IN StartOfBlock THEN
      LOOP
        VAR
          qualId := QualId(t);
          r: M3AST_AS.REVELATION;
        BEGIN
          IF At(t, M3CToken.Subtype) THEN
            r := NEW(M3AST_AS.Subtype_reveal).init();
          ELSE
            EVAL Expect(t, M3CToken.Equal, term + StartOfType);
            r := NEW(M3AST_AS.Concrete_reveal).init();
          END;
          SeqM3AST_AS_REVELATION.AddRear(revelationS.as_reveal_s, r);
          r.lx_srcpos := qualId.lx_srcpos;
          r.as_qual_id := qualId;
          r.as_type := Type(t, term);
          IF EndOfDecl(t, r, term) THEN EXIT END;
        END;
      END;
    END;
    RETURN revelationS;
  END Reveal;


<*INLINE*> PROCEDURE LastPos(srcNode: M3AST_AS.SRC_NODE): M3CSrcPos.T RAISES {}=
  BEGIN
    IF srcNode = NIL THEN
      RETURN M3CSrcPos.Null;
    ELSE
      RETURN srcNode.lx_srcpos;
    END;
  END LastPos;


PROCEDURE ExternalPragma(
    pragmas: M3CPragma.Store;
    last: M3AST_AS.SRC_NODE;
    VAR langSpec: Text.T)
    : M3CPragma.T
    RAISES {}=
  VAR
    iter := M3CPragma.NewIter(pragmas, LastPos(last));
    pragma: M3CPragma.T;
  BEGIN
    WHILE M3CPragma.Next(iter, pragma) DO
      IF M3CPragma.Match(pragma, "EXTERNAL", langSpec) THEN
        RETURN pragma;
      END;
    END;
    RETURN NIL;
  END ExternalPragma;


PROCEDURE InlinePragma(
    pragmas: M3CPragma.Store;
    last: M3AST_AS.SRC_NODE)
    : M3CPragma.T
    RAISES {}=
  VAR
    iter := M3CPragma.NewIter(pragmas, LastPos(last));
    pragma: M3CPragma.T;
    args: Text.T;
  BEGIN
    WHILE M3CPragma.Next(iter, pragma) DO
      IF M3CPragma.Match(pragma, "INLINE", args) AND args = NIL THEN
        RETURN pragma;
      END;
    END;
    RETURN NIL;
  END InlinePragma;


PROCEDURE External(
    pragma: M3CPragma.T;
    langSpec: Text.T)
    : M3AST_PG.External
    RAISES {}=
  VAR
    external: M3AST_PG.External := NEW(M3AST_PG.External).init();
  BEGIN
    external.lx_srcpos := M3CPragma.Position(pragma);
    IF langSpec = NIL THEN external.lx_lang_spec := NIL
    ELSE 
      (* M3AST_PG_F says its a Text_rep, so it must be quoted (sigh) *)
      IF NOT Text.GetChar(langSpec, 0) = '"' THEN
        langSpec := TextExtras.Join("\"", langSpec, "\"");
      END;
      external.lx_lang_spec := M3CLiteral.Enter(langSpec);
    END;
    RETURN external;
  END External;


PROCEDURE Declarations(
    t: T;
    READONLY term: TokenSet;
    revealOk := FALSE)
    : SeqM3AST_AS_DECL_REVL.T
    RAISES {Rd.Failure}=
  VAR
    declTerm := term +
        TokenSet{M3CToken.Semicolon} + StartOfDeclarationOrRevelation;
    result := SeqM3AST_AS_DECL_REVL.Null;
  BEGIN
    LOOP
      VAR
        token := t.lexer.current();
      BEGIN
        IF token IN StartOfDeclaration THEN
          VAR
            d: M3AST_AS.DECL;
            langSpec: Text.T;
            externalPragma :=
                ExternalPragma(t.pragmas, t.lastSrcPosNode, langSpec);
          BEGIN
            CASE token OF <*NOWARN*>
            | M3CToken.CONST_ =>
                d := ConstDecl(t, declTerm);
            | M3CToken.TYPE_ =>
                d := TypeDecl(t, declTerm);
            | M3CToken.EXCEPTION_ =>
                d := ExceptionDecl(t, declTerm);
            | M3CToken.PROCEDURE_ =>
                VAR
                  inlinePragma := InlinePragma(t.pragmas, t.lastSrcPosNode);
                  procDecl := ProcedureDecl(t, declTerm);
                BEGIN
                  IF inlinePragma # NIL THEN
                    VAR inline: M3AST_PG.Inline := NEW(M3AST_PG.Inline).init();
                    BEGIN
                      inline.lx_srcpos := M3CPragma.Position(inlinePragma);
                      procDecl.pg_inline := inline;
                    END;
                    M3CPragma.SetHook(inlinePragma, procDecl);
                  END;
                  d := procDecl;
                END;
            | M3CToken.VAR_ =>
                d := VarDecl(t, declTerm);
            END; (* case *)
            IF externalPragma # NIL THEN
              VAR
                externalDecl: M3AST_PG.EXTERNAL_DECL;
              BEGIN
                IF M3AST_PG.IsA_EXTERNAL_DECL(d, externalDecl) THEN
                  externalDecl.pg_external :=
                      External(externalPragma, langSpec);
                  M3CPragma.SetHook(externalPragma, d);
                END;
              END;
            END;
            SeqM3AST_AS_DECL_REVL.AddRear(result, d);
          END;
        ELSIF token = M3CToken.REVEAL_ THEN
          IF NOT revealOk THEN Unexpected(t) END;
          WITH reveal = Reveal(t, declTerm) DO
            SeqM3AST_AS_DECL_REVL.AddRear(result, reveal);
          END;
        ELSIF token = M3CToken.Semicolon THEN
          Unexpected(t);
          EVAL t.lexer.next();
        ELSE
          EXIT;
        END;
      END;
    END; (* loop *)
    RETURN result;
  END Declarations;


PROCEDURE Block(
    t: T;
    READONLY term: TokenSet;
    revealOk := FALSE)
    : M3AST_AS.Block
    RAISES {Rd.Failure}=
  VAR
    blockTerm := term + StartOfStatement + EndAsSet;
    b: M3AST_AS.Block := NEW(M3AST_AS.Block).init();
  BEGIN
    Pos(t, b);
    b.as_decl_s :=
        Declarations(t, blockTerm + TokenSet{M3CToken.BEGIN_}, revealOk);
    EVAL Expect(t, M3CToken.BEGIN_, blockTerm);
    b.as_stm_s := StmtsThenEnd(t, blockTerm);
    RETURN b;
  END Block;


PROCEDURE Imports(
    t: T;
    READONLY term: TokenSet)
    : SeqM3AST_AS_IMPORTED.T
    RAISES {Rd.Failure}=
  VAR
    possibleStartOfImport := StartOfImport +
        TokenSet{M3CToken.Identifier, M3CToken.AS_, M3CToken.Comma, M3CToken.Semicolon};
    importTerm := term + possibleStartOfImport;
    seqImported := SeqM3AST_AS_IMPORTED.Null;
  BEGIN
    IF t.lexer.current() IN StartOfImport THEN
      REPEAT
        VAR
          pos := t.lexer.position();
          imported: M3AST_AS.IMPORTED;
        BEGIN
          IF At(t, M3CToken.FROM_) THEN
            VAR f: M3AST_AS.From_import := NEW(M3AST_AS.From_import).init();
            BEGIN
              f.lx_srcpos := pos;
              f.as_intf_id := NEW(M3AST_AS.Used_interface_id).init();
              Id(t, f.as_intf_id);
              f.as_id_s := SeqM3AST_AS_Used_def_id.Null;
              EVAL Expect(t, M3CToken.IMPORT_, importTerm);
              REPEAT
                VAR id: M3AST_AS.Used_def_id := NEW(M3AST_AS.Used_def_id).init();
                BEGIN
                  SeqM3AST_AS_Used_def_id.AddRear(f.as_id_s, id);
                  Id(t, id);
                END;
              UNTIL EndOfSequence(t, M3CToken.Comma,
                  M3CToken.Semicolon, IdAsSet, importTerm);
              imported := f;
            END;
          ELSE
            VAR i: M3AST_AS.Simple_import := NEW(M3AST_AS.Simple_import).init();
            BEGIN
              i.lx_srcpos := pos;
              i.as_import_item_s := SeqM3AST_AS_Import_item.Null;
              EVAL Expect(t, M3CToken.IMPORT_, importTerm);
              REPEAT
                VAR import_item: M3AST_AS.Import_item := NEW(M3AST_AS.Import_item).init();
                BEGIN
                  SeqM3AST_AS_Import_item.AddRear(i.as_import_item_s, import_item);
                  Pos(t, import_item, FALSE);
                  import_item.as_intf_id := NEW(M3AST_AS.Used_interface_id).init();
                  Id(t, import_item.as_intf_id);
                  IF At(t, M3CToken.AS_) THEN
                    import_item.as_id := NEW(M3AST_AS.Interface_AS_id).init();
                    Id(t, import_item.as_id);
                  END;
                END;
              UNTIL EndOfSequence(t, M3CToken.Comma,
                  M3CToken.Semicolon, IdAsSet, importTerm);
              imported := i;
            END;
          END;
          SeqM3AST_AS_IMPORTED.AddRear(seqImported, imported);
        END;
      UNTIL NOT t.lexer.current() IN possibleStartOfImport;
    END;
    RETURN seqImported;
  END Imports;

PROCEDURE GenericFormals(t: T;
    READONLY term: TokenSet): SeqM3AST_AS_F_Interface_id.T RAISES {Rd.Failure}=
  VAR seqF_Interface_id := SeqM3AST_AS_F_Interface_id.Null;
  BEGIN
    EVAL Expect(t, M3CToken.Bra, term);
    IF NOT At(t, M3CToken.Ket) THEN
      REPEAT
        VAR id: M3AST_AS.F_Interface_id := NEW(M3AST_AS.F_Interface_id).init();
        BEGIN
          SeqM3AST_AS_F_Interface_id.AddRear(seqF_Interface_id, id);
          Id(t, id);
        END;
      UNTIL EndOfSequence(t, M3CToken.Comma, M3CToken.Ket, IdAsSet, term);
    END; (* if *)
    RETURN seqF_Interface_id;
  END GenericFormals;


PROCEDURE GenericActuals(t: T;
    READONLY term: TokenSet
    ): SeqM3AST_AS_Used_interface_id.T RAISES {Rd.Failure}=
  VAR seqUsed_interface_id := SeqM3AST_AS_Used_interface_id.Null;
  BEGIN
    EVAL Expect(t, M3CToken.Bra, term);
    IF NOT At(t, M3CToken.Ket) THEN
      REPEAT
        VAR id: M3AST_AS.Used_interface_id := NEW(M3AST_AS.Used_interface_id).init();
        BEGIN
          SeqM3AST_AS_Used_interface_id.AddRear(seqUsed_interface_id, id);
          Id(t, id);
        END;
      UNTIL EndOfSequence(t, M3CToken.Comma, M3CToken.Ket, IdAsSet, term);
    END; (* if *)
    RETURN seqUsed_interface_id;
  END GenericActuals;


PROCEDURE TruncatedUnit(t: T; unit: M3AST_AS.UNIT): M3AST_AS.UNIT RAISES {}=
  VAR
    b: M3AST_AS.Block := NEW(M3AST_AS.Block).init();
    pos := t.lexer.position();
  BEGIN
    b.lx_srcpos := pos;
    b.as_decl_s := SeqM3AST_AS_DECL_REVL.Null;
    b.as_stm_s := SeqM3AST_AS_STM.Null;
    TYPECASE unit OF
    | M3AST_AS.UNIT_WITH_BODY(ub) => ub.as_block := b;
    ELSE
    END;
    RETURN unit;
  END TruncatedUnit;


PROCEDURE Unit(
    t: T;
    headerOnly := FALSE)
    : M3AST_AS.UNIT
    RAISES {Rd.Failure}=
  CONST
    UnitTerm = StartOfImport + StartOfDeclaration + StartOfRevelation +
        TokenSet{M3CToken.END_, M3CToken.Void};
  VAR
    unit: M3AST_AS.UNIT; unit_with_body: M3AST_AS.UNIT_WITH_BODY;
    unsafe: M3AST_AS.Unsafe := NIL;
    generic := FALSE;
  BEGIN
    EVAL ExpectSet(t, StartOfUnit, UnitTerm + IdAsSet);
    IF t.lexer.current() = M3CToken.UNSAFE_ THEN
      unsafe := NEW(M3AST_AS.Unsafe).init();
      Pos(t, unsafe, TRUE);
    END;
    IF t.lexer.current() = M3CToken.GENERIC_ THEN
      generic := TRUE;
      IF unsafe # NIL THEN Unexpected(t) END;
      EVAL t.lexer.next();
    END;
    IF t.lexer.current() = M3CToken.INTERFACE_ THEN
      VAR
        interface: M3AST_AS.Interface := NEW(M3AST_AS.Interface).init();
        langSpec: Text.T;
        externalPragma := ExternalPragma(t.pragmas, NIL, langSpec);
      BEGIN
        IF externalPragma # NIL THEN
          interface.vEXTERNAL_DECL.pg_external :=
              External(externalPragma, langSpec);
          M3CPragma.SetHook(externalPragma, interface);
        END;
        t.interface := TRUE;
        Pos(t, interface, TRUE);
        interface.as_id := NEW(M3AST_AS.Interface_id).init();
        interface.as_unsafe := unsafe;
        Id(t, interface.as_id);
        IF generic THEN
          VAR interface_gen_def: M3AST_AS.Interface_gen_def := NEW(M3AST_AS.Interface_gen_def).init();
          BEGIN
            unit := interface_gen_def;
            interface_gen_def.as_id_s := GenericFormals(t, UnitTerm);
            interface_gen_def.as_id := interface.as_id;
            interface_gen_def.lx_srcpos := interface.lx_srcpos;
            interface_gen_def.vEXTERNAL_DECL.pg_external :=
                interface.vEXTERNAL_DECL.pg_external;
            EVAL Expect(t, M3CToken.Semicolon, UnitTerm);
          END;
        ELSE
          EVAL ExpectSet(t, TokenSet{M3CToken.Semicolon, M3CToken.Equal}, UnitTerm);
          IF At(t, M3CToken.Equal) THEN
            VAR interface_gen_ins: M3AST_AS.Interface_gen_ins := NEW(M3AST_AS.Interface_gen_ins).init();
            BEGIN
              unit := interface_gen_ins;
              interface_gen_ins.as_id := interface.as_id;
              interface_gen_ins.lx_srcpos := interface.lx_srcpos;
              interface_gen_ins.as_unsafe := unsafe;
              interface_gen_ins.as_gen_id := NEW(M3AST_AS.Used_interface_id).init();
              Id(t, interface_gen_ins.as_gen_id);
              interface_gen_ins.as_id_s := GenericActuals(t, UnitTerm);
            END;
          ELSE
            EVAL Expect(t, M3CToken.Semicolon, UnitTerm);
            unit := interface;
          END;
        END;
        IF ISTYPE(unit, M3AST_AS.UNIT_WITH_BODY) THEN
          unit_with_body := unit;
          EVAL ExpectSet(t, UnitTerm - TokenSet{M3CToken.Void}, UnitTerm);
          unit_with_body.as_import_s := Imports(t, UnitTerm);
          IF headerOnly THEN RETURN TruncatedUnit(t, unit_with_body) END;
          WITH block = unit_with_body.as_block DO
            block := NEW(M3AST_AS.Block).init();
            block.lx_srcpos := t.lexer.position();
            block.as_decl_s :=
                Declarations(t, UnitTerm - StartOfImport, TRUE);
            block.as_stm_s := SeqM3AST_AS_STM.Null;
          END;
        END;
        EVAL Expect(t, M3CToken.END_, UnitTerm);
        t.interface := FALSE;
      END;
    ELSE
      CONST
        ModuleTerm = UnitTerm + StartOfBlock;
        ExportsTerm = ModuleTerm + TokenSet{M3CToken.Semicolon};
        StartOfModuleBody = StartOfImport + StartOfBlock;
      VAR
        module: M3AST_AS.Module := NEW(M3AST_AS.Module).init();
      BEGIN
        module.as_unsafe := unsafe;
        Pos(t, module);
        EVAL MustBeAt(t, M3CToken.MODULE_);
        module.as_id := NEW(M3AST_AS.Module_id).init();
        Id(t, module.as_id);
        module.as_export_s := SeqM3AST_AS_Used_interface_id.Null;
        IF generic THEN
          VAR module_gen_def: M3AST_AS.Module_gen_def := NEW(M3AST_AS.Module_gen_def).init();
          BEGIN
            unit := module_gen_def;
            module_gen_def.as_id := module.as_id;
            module_gen_def.lx_srcpos := module.lx_srcpos;
            module_gen_def.as_id_s := GenericFormals(t, UnitTerm);
            EVAL Expect(t, M3CToken.Semicolon, UnitTerm);
          END;
        ELSE
          IF At(t, M3CToken.EXPORTS_) THEN
            REPEAT
              VAR id: M3AST_AS.Used_interface_id := NEW(M3AST_AS.Used_interface_id).init();
              BEGIN
                SeqM3AST_AS_Used_interface_id.AddRear(module.as_export_s, id);
                Id(t, id);
              END;
            UNTIL EndOfSequenceSet(t, M3CToken.Comma,
                TokenSet{M3CToken.Semicolon, M3CToken.Equal}, IdAsSet, ExportsTerm);
          END;
          EVAL ExpectSet(t, TokenSet{M3CToken.Semicolon, M3CToken.Equal},
                         ModuleTerm);
          IF At(t, M3CToken.Equal) THEN
            VAR module_gen_ins: M3AST_AS.Module_gen_ins := NEW(M3AST_AS.Module_gen_ins).init();
            BEGIN
              unit := module_gen_ins;
              module_gen_ins.as_id := module.as_id;
              module_gen_ins.lx_srcpos := module.lx_srcpos;
              module_gen_ins.as_export_s := module.as_export_s;
              module_gen_ins.as_unsafe := unsafe;
              module_gen_ins.as_gen_id := NEW(M3AST_AS.Used_interface_id).init();
              Id(t, module_gen_ins.as_gen_id);
              module_gen_ins.as_id_s := GenericActuals(t, UnitTerm);
            END;
            EVAL MustBeAt(t, M3CToken.END_);
          ELSE
            EVAL Expect(t, M3CToken.Semicolon, ModuleTerm);
            unit := module
          END; (* if *)
        END; (* if *)
        IF ISTYPE(unit, M3AST_AS.UNIT_WITH_BODY) THEN
          unit_with_body := unit;
          EVAL ExpectSet(t, StartOfModuleBody, ModuleTerm);
          unit_with_body.as_import_s := Imports(t, ModuleTerm);
          IF headerOnly THEN RETURN TruncatedUnit(t, unit) END;
          unit_with_body.as_block := Block(t, ModuleTerm - StartOfImport, TRUE);
        END;
      END;
    END;
    IdAfterEnd(t, unit.as_id.lx_symrep);
    EVAL MustBeAt(t, M3CToken.Dot);
    RETURN unit;
  END Unit;


EXCEPTION
  BadTerminators;

<*INLINE*> PROCEDURE CheckTerminators(chars: SET OF CHAR): SET OF CHAR=
   <*FATAL BadTerminators*>
   BEGIN
    IF chars <= AnyTerminators THEN
      RETURN chars;
    ELSE
      RAISE BadTerminators;
    END;
  END CheckTerminators;


PROCEDURE Any(
    t: T;
    terminators := SET OF CHAR{})
    : REFANY
    RAISES {Rd.Failure}=
  CONST
    VoidAsSet = TokenSet{M3CToken.Void};
    DefaultTerm = Start + VoidAsSet;
  VAR
    token := t.lexer.current();
    result: REFANY := NIL;
  BEGIN
    t.terminators := CheckTerminators(terminators);
    t.pragmas := M3CPragma.NewStore();
    t.comments := M3CComment.NewStore();
    IF token = M3CToken.Void THEN token := t.lexer.next() END;
    IF ExpectSet(t, Start, DefaultTerm) THEN
      IF token IN StartOfUnit THEN
        result := Unit(t);
      ELSIF token IN StartOfImport THEN
        result := Imports(t, DefaultTerm);
      ELSIF token IN StartOfBlock THEN
        VAR
	  pos := t.lexer.position();
          decls := Declarations(t, DefaultTerm);
        BEGIN
          IF At(t, M3CToken.BEGIN_) THEN
            VAR b: M3AST_AS.Block := NEW(M3AST_AS.Block).init();
            BEGIN
	      b.lx_srcpos := pos;
              b.as_decl_s := decls;
              b.as_stm_s := StmtsThenEnd(t,
                  StartOfStatement + EndAsSet);
              IF At(t, M3CToken.Semicolon) THEN
                VAR
                  seqStm := Stmts(t, VoidAsSet, DefaultTerm);
                BEGIN
                  SeqM3AST_AS_STM.AddFront(seqStm, b);
                  result := seqStm;
                END;
              ELSE
                result := b;
              END;
            END;
          ELSE
            result := decls;
          END;
        END;
      ELSIF token IN StartOfExpression THEN
        CONST
          PartOfStatement = TokenSet{M3CToken.Semicolon, M3CToken.Becomes};
        VAR
          expr := Expr(t, DefaultTerm + PartOfStatement, TRUE);
        BEGIN
          IF ISTYPE(expr, M3AST_AS.M3TYPE) OR
              NOT t.lexer.current() IN PartOfStatement THEN
            result := expr;
          ELSE
            result := Stmts(t, VoidAsSet, DefaultTerm, expr);
          END;
        END;
      ELSE
        result := Stmts(t, VoidAsSet, DefaultTerm);
      END;
    END;
    IF t.lexer.current() # M3CToken.Void THEN Unexpected(t) END;
    t.terminators := SET OF CHAR{};
    Reset(t);
    RETURN result;
  END Any;


TYPE
  CallBack = M3CLex.CallBack OBJECT
    parser: T;
  OVERRIDES
    badChar := BadChar;
    comment := Comment;
    pragma := Pragma;
    whiteSpace := WhiteSpace;
  END;


PROCEDURE BadChar(c: CallBack; ch: CHAR) RAISES {}=
  VAR
    text: Text.T;
  BEGIN
    IF ch IN c.parser.terminators THEN
      c.parser.lexer.disable();
    ELSE
      IF ch IN AnyTerminators THEN
        text := Fmt.Char(ch);
      ELSE
        text := Fmt.F("%s", Fmt.Int(ORD(ch), 8));
      END;
      ErrorMessage(c.parser, "Bad char - " & text);
    END;
  END BadChar;


PROCEDURE Comment(c: CallBack; comment: Text.T) RAISES {}=
  VAR
    high := Text.Length(comment) - 1; (* will be at least 2 *)
    t := c.parser;
  BEGIN
    IF Text.GetChar(comment, high) # ')' OR
        Text.GetChar(comment, high - 1) # '*' THEN
      ErrorMessage(c.parser, "Non terminated comment");
    ELSE
      t.lastComment := M3CComment.AddToStore(comment, t.lexer.position(),
          t.lastSrcPosNode, t.comments);
      t.commentOrPragma := TRUE;
    END;
  END Comment;


PROCEDURE Pragma(c: CallBack; pragma: Text.T) RAISES {}=
  VAR
    high := Text.Length(pragma) - 1; (* will be at least 2 *)
    t := c.parser;
  BEGIN
    IF Text.GetChar(pragma, high) # '>' OR
        Text.GetChar(pragma, high - 1) # '*' THEN
      ErrorMessage(t, "Non terminated pragma");
    ELSE
      t.lastPragma := M3CPragma.AddToStore(pragma, t.lexer.position(),
          t.lastSrcPosNode, t.pragmas);
      t.commentOrPragma := TRUE;
    END;
  END Pragma;

PROCEDURE WhiteSpace(<*UNUSED*> c: CallBack; <*UNUSED*> ws: Text.T)=
  BEGIN
  END WhiteSpace;

PROCEDURE Init(
    t: T;
    rd: Rd.T;
    identifiers: M3CReservedWord.Table;
    literals: M3CHash.Table;
    errorHandler: ErrorHandler;
    lexer: M3CLex.T := NIL)
    : T
    RAISES {}=
  BEGIN
    t.identifiers := identifiers;
    t.idNEW  := identifiers.enter("NEW");
    t.nil_litrep := literals.enter("NIL");
    t.errorHandler := errorHandler;
    IF lexer = NIL THEN
      lexer := NEW(M3CLex.T).init(rd, identifiers, literals,
                                  NEW(CallBack, parser := t));
    END;
    t.lexer := lexer;
    RETURN t;
  END Init;


PROCEDURE ResetLastFields(t: T) RAISES {}=
  BEGIN
    t.lastErrorPos := M3CSrcPos.Null;
    t.lastSrcPosNode := NIL;
    t.commentOrPragma := FALSE;
    t.lastPragma := NIL;
    t.lastComment := NIL;
  END ResetLastFields;


PROCEDURE Compilation(
    t: T;
    headerOnly := FALSE)
    : M3AST_AS.Compilation_Unit
    RAISES {Rd.Failure}=
  VAR
    c: M3AST_AS.Compilation_Unit := NEW(M3AST_AS.Compilation_Unit).init();
  BEGIN
    t.pragmas := M3CPragma.NewStore();
    t.comments := M3CComment.NewStore();
    ResetLastFields(t);
    EVAL t.lexer.next();
    c.as_root := Unit(t, headerOnly);
    c.lx_pragmas := t.pragmas;
    c.lx_comments := t.comments;
    ResetLastFields(t);
    t.pragmas := NIL;
    RETURN c;
  END Compilation;


PROCEDURE Reset(t: T; pos := M3CSrcPos.Null; rd: Rd.T := NIL) RAISES {}=
  BEGIN
    ResetLastFields(t);
    t.lexer.reset(pos, rd);
  END Reset;


BEGIN

END M3CParse.
