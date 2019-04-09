(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ArrayExpr.m3                                          *)

MODULE ArrayExpr;
(* An array constructor. Built only during semantic procssing. *)

IMPORT M3, M3ID, CG, Expr, ExprRep, Error, Type, ArrayType;
IMPORT ConsExpr, KeywordExpr, RangeExpr, Int, OpenArrayType, Module;
IMPORT IntegerExpr, EnumExpr, SubrangeType, Target, TInt, M3Buf;
IMPORT AssignStmt, RefType, M3RT, Procedure, RunTyme, ErrType;

(* Monitor sequencing of operations: *)
TYPE StateTyp
  = { Fresh, Checking, Checked, Representing, Represented,
      Prepping, Prepped, Compiling, Compiled};

(* Information collected/matched across all cousins at a given array
   multidimensional nesting depth. *)
TYPE LevelInfoTyp = RECORD
    semType           : Type.T := NIL;
    repType           : Type.T := NIL;
    repIndexType      : Type.T := NIL;
    fixingVal         : CG.Val := NIL;
    dynBitsizeVal     : CG.Val := NIL;
    staticLen         : Expr.lengthTyp := Expr.lengthNonArray;
    staticFlatEltCt   : INTEGER := 1;
                        (* Product of non-neg. staticLen values for this
                           and static inner dimensions, flattened down to
                           either a non-array or dynamic-sized open array. *)
    eltPack           : INTEGER;
                        (* ^Irrelevant when NOT FixedOpenChecked *)
    FixedOpenChecked  : BOOLEAN := FALSE;
  END;

TYPE LevelsTyp = REF ARRAY OF LevelInfoTyp;

(* Different ways of delivering the result expression. *)
TYPE ResultKindTyp
  = { RKUnknown      (* Initial value. *)

    , RKGlobal       (* Mutable global data area. *)
                       (* Uses top.globalDopeOffset, top.GlobalEltsOffset,
                          and top.inConstArea. *)
    , RKDirectElts   (* Caller-provided area, elements only. *)
                       (* Uses top.buildEltsAddrVal. *)
    , RKDirectDoped  (* Caller-provided area, doped. *)
                       (* Uses top.buildAddrVal and top.buildEltsAddrVal. *)
    , RKTempElts     (* Static-sized CG.Var temp, elements only. *)
                       (* Uses top.buildTempVar. *)
    , RKTempStatic   (* Static-sized CG.Var temp, with dope. *)
                       (* Uses top.buildTempVar. *)
    , RKTempDyn      (* Dynamically sized and allocated temp. *)
                       (* Uses top.buildAddrVal and top.buildEltsAddrVal. *)
    };

TYPE RKTyp = ResultKindTyp;
TYPE RKTypSet = SET OF RKTyp;
CONST RKTypSetInitializing = RKTypSet
  { RKTyp.RKGlobal, RKTyp.RKTempElts, RKTyp.RKTempStatic, RKTyp.RKTempDyn};

(* Properties of an array constructor: *)
REVEAL 
  P = Expr.T BRANDED "ArrayExpr.P" OBJECT
    top               : P;
    tipe              : Type.T;
(* CLEANUP ^ tipe always duplicates field "type", inherited from Expr.T *) 
    args              : Expr.List;
    semType           : Type.T; (* Original source code semantic type. *)
    semIndexType      : Type.T;
    semEltType        : Type.T;
    repType           : Type.T; (* Type used in RT representation of the
                                   constructor.  Equals targetType if targetType
                                   # NIL.  Otherwise, inferred entirely from the
                                   constructor.  Could still be # semType,
                                   if semType is open but context fixes
                                   the length of some of the dimensions. *)
    repIndexType      : Type.T;
    targetType        : Type.T; (* Requested by user of the constructor. *)
    depth             : INTEGER; (* Depth this constructor is below the
                                    top-level array constructor. *)
    eltCt             : INTEGER := Expr.lengthInvalid;
    state             : StateTyp;
    isNested          : BOOLEAN;
    dots              : BOOLEAN;
    evalAttempted     : BOOLEAN; (* TRUE even if Evaluate was called unsuccessfully. *) 
    is_const          : BOOLEAN; (* Meaningless if NOT evalAttempted.
                                    Otherwise, Evaluate was successful. *)
    broken            : BOOLEAN;
    
    (* Only used in top constructor: *)
    repOpenDepth      : INTEGER; 
    globalDopeOffset  : INTEGER := FIRST (INTEGER) (* Means uninitialized. *);
    globalEltsOffset  : INTEGER := FIRST (INTEGER) (* Means uninitialized. *);
    refType           : Type.T; (* If needed, type REF repType. *)
    finalVal          : CG.Val;
    buildAddrVal      : CG.Val;
    buildEltsAddrVal  : CG.Val;
    buildTempVar      : CG.Var;
    dynOffsetVal      : CG.Val := NIL; (* Top only. *)
    shallowestDynDepth: INTEGER := FIRST (INTEGER);
    deepestDynDepth   : INTEGER := FIRST (INTEGER);
    heapTempPtrVar    : CG.Var;
    staticEltsSize    : INTEGER; (* Excluding any dope.  Zero if not static. *)
    dopeSize          : INTEGER;
    totalSize         : INTEGER; (* Including any dope. *) 
    firstArgExpr      : Expr.T := NIL;
    firstArgDepth     : INTEGER := 0;
    firstArgDopeVal   : CG.Val;
    levels            : LevelsTyp;
    resultKind        := RKTyp.RKUnknown;
    topRepAlign       : Type.BitAlignT;
    topEltsAlign      : Type.BitAlignT; (* The entire block of elements. *)
    inConstArea       : BOOLEAN;
    fixingInfoComputed: BOOLEAN;
    usesAssignProtocolCalled: BOOLEAN;
    useTargetVar      : BOOLEAN := FALSE;
    useTargetVarAccessed: BOOLEAN := FALSE;
  OVERRIDES
    typeOf       := ExprRep.NoType;
    check        := Check;
    need_addr    := NeedsAddress;
    prep         := Prep;
    compile      := Compile;
    prepLV       := ExprRep.NotLValue;(* Externally dispatched-to: *)
    compileLV    := ExprRep.NotLValue;
    prepBR       := ExprRep.NotBoolean;
    compileBR    := ExprRep.NotBoolean;
    evaluate     := Evaluate;
    isEqual      := IsEqual;
    getBounds    := ExprRep.NoBounds;
    isWritable   := ExprRep.IsNever;
    isDesignator := ExprRep.IsNever;
    isZeroes     := IsZeroes;
    genFPLiteral := GenFPLiteral;
    prepLiteral  := PrepLiteral;
    genLiteral   := GenLiteral;
    note_write   := ExprRep.NotWritable;
    repTypeOf    := RepTypeOf;
    staticLength := StaticLength;
    usesAssignProtocol := UsesAssignProtocol;
  END (*OBJECT*);

PROCEDURE IsErr (type: Type.T) =
  VAR Debug: INTEGER := 0;
  BEGIN
    IF type = ErrType.T
    THEN
      Debug := Target.Address.align (* For breakpoints. *)
    END
  END IsErr;

(* EXPORTED: *) 
PROCEDURE New (type: Type.T; args: Expr.List; dots: BOOLEAN): Expr.T =
  VAR constr := NEW (P);
  BEGIN
    ExprRep.Init (constr);
    constr.resultKind   := RKTyp.RKUnknown;
    constr.depth        := FIRST (INTEGER);
    constr.type         := type;
    constr.tipe         := type;
    constr.semType      := type;
    constr.targetType   := NIL;
    constr.useTargetVar := FALSE;
    constr.useTargetVarAccessed := FALSE;
    WITH b = ArrayType.Split (type, constr.semIndexType, constr.semEltType)
    DO <* ASSERT b *> (* Which also implies type is an array type. *) END; 
IsErr(constr.semIndexType) (* Just for debugging. *);
    constr.repIndexType := NIL;
    constr.repType      := NIL;
    constr.args         := args;
(* REVIEW: Initializations here vs. declared in P. *)    
    constr.dots                 := dots;
    constr.evalAttempted        := FALSE;
    constr.is_const             := FALSE;
    constr.broken               := FALSE;
    constr.checked              := FALSE;
    constr.fixingInfoComputed   := FALSE;
    constr.usesAssignProtocolCalled := FALSE;
    constr.refType              := NIL;
    constr.dynOffsetVal         := NIL;
    constr.directAssignableType := TRUE; (* Inherited. *)
    constr.eltCt                := Expr.lengthInvalid;
    constr.state                := StateTyp.Fresh;
    constr.shallowestDynDepth   := FIRST (INTEGER);
    constr.deepestDynDepth      := FIRST (INTEGER);
    RETURN constr;
  END New;

(* EXPORTED: *) 
PROCEDURE ArrayConstrExpr (e: Expr.T): P =
(* Look through a ConsExpr for an ArrayExpr.  NIL if not. *)
   
  VAR base: Expr.T;
  BEGIN
    ConsExpr.Seal (e);
    (* DO NOT allow ConsExpr to Check e. That would make a (should be top-level)
       call to ArrayExpr.Check on a nested array constructor. *)
    base := ConsExpr.Base (e);
    IF base = NIL THEN base := e END; 
    TYPECASE base OF
    | NULL => RETURN NIL;
    | P (arrayExpr) => RETURN arrayExpr;
    ELSE RETURN NIL;
    END;
  END ArrayConstrExpr;

(* EXPORTED: *) 
PROCEDURE NoteNested (constr: P) =
(* PRE: constr has not been checked. *)
(* Mark constr as nested (ArrayExpr nested inside an ArrayExpr, directly,
   except for a possible ConsExpr in between.  In particular, not a
   named constant. *)
  BEGIN
    IF constr = NIL THEN RETURN END;
    <* ASSERT constr.state < StateTyp.Checking *>
    constr.isNested := TRUE 
  END NoteNested;

(* EXPORTED: *) 
PROCEDURE Is (e: Expr.T): BOOLEAN =
(* Purely syntactic. Will not look through a ConsExpr. *)
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P    => RETURN TRUE;
    ELSE      RETURN FALSE;
    END;
  END Is;

(* Externally dispatched-to: *)
PROCEDURE NeedsAddress (<*UNUSED*> constr: P) =
  BEGIN
    (* We don't need to be told this. It always needs an address. *)
  END NeedsAddress;

(* Externally dispatched-to: *)
PROCEDURE IsEqual (a: P;  e: Expr.T;  x: M3.EqAssumption): BOOLEAN =
(* Purely syntactic. *)
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(b) => 
      IF   (NOT Type.IsEqual (a.semType, b.semType, x))
        OR (a.dots # b.dots)
        OR ((a.args = NIL) # (b.args = NIL))
        OR ((a.args # NIL) AND (NUMBER (a.args^) # NUMBER (b.args^))) THEN
        RETURN FALSE;
      END;
      FOR i := 0 TO LAST (a.args^) DO
        IF NOT Expr.IsEqual (a.args^[i], b.args^[i], x) THEN RETURN FALSE END;
      END;
      RETURN TRUE;
    ELSE RETURN FALSE;
    END;
  END IsEqual;

(* ------------------------- Check ------------------------ *)

PROCEDURE ArgCt (constr: P): INTEGER =
  BEGIN
    IF constr.args = NIL THEN RETURN 0
    ELSE
      RETURN NUMBER (constr.args^);
    END;
  END ArgCt; 

PROCEDURE LengthOfOrdType (ordType: Type.T): Expr.lengthTyp =
(* PRE: ordType is NIL or an ordinal type. *)
  VAR length: Expr.lengthTyp;
  VAR lengthTI: Target.Int;
  VAR b: BOOLEAN;
  BEGIN
    IF ordType = NIL THEN RETURN Expr.lengthNonStatic END;
    <* ASSERT Type.IsOrdinal (ordType) *>
    lengthTI := Type.Number (ordType);
    b := TInt.ToInt (lengthTI, length);
    IF NOT b THEN
      Error.Msg ("CM3 limit: ordinal type has too many elements.");
      RETURN Expr.lengthInvalid;
      END;
    RETURN length;
  END LengthOfOrdType;

PROCEDURE CheckArgStaticLength
  (constr: P; VAR levelInfo: LevelInfoTyp; argLength, depth: INTEGER) =
  BEGIN
    IF argLength < 0 THEN RETURN END;
    IF levelInfo.staticLen = Expr.lengthNonStatic 
    THEN (* First-found fixed-array argument.  All cousins at
            this depth must have this same length. *)
      levelInfo.staticLen := argLength;
    ELSIF argLength # levelInfo.staticLen
    THEN
      Error.Int (depth,
         "array constructor element's static length differs "
         & "from previous static length, at depth:");
      constr.broken := TRUE;
    END;
  END CheckArgStaticLength;
  
(* Externally dispatched-to: *)
PROCEDURE Check (top: P;  VAR cs: Expr.CheckState) =
(* Must be called IFF top is the top of a tree of array constructors. *)
(* This will be called from ConsExpr.Check, through Expr.TypeCheck. *)
  VAR levelType, levelIndexType, levelEltType: Type.T;
  VAR b: BOOLEAN;
  BEGIN
    IF top.isNested THEN RETURN END (* Shouldn't happen. *);
    IF top.checked THEN RETURN END;

    top.type := Type.Check (top.type);
    top.tipe := top.type;
    top.semType := top.type;

    (* Create the levels array. *)
    top.levels := NEW (LevelsTyp, ArrayType.TotalDepth (top.semType) + 1);
    (* Plus one for the non-array leaf --------------------------------^. *)

    (* Collect level info (semType, staticLen) from top-level semType. *)
    levelType := top.semType;
    FOR i := 0 TO LAST (top.levels^) - 1 DO
      WITH levelInfo = top.levels^[i] DO
        levelInfo.semType := levelType;
        b := ArrayType.Split
               (levelType, (*VAR*)levelIndexType, (*VAR*)levelEltType);
        <* ASSERT b *>
        levelInfo.staticLen := LengthOfOrdType (levelIndexType);
        levelType := levelEltType;
      END
    END;

    (* Initialize level info for the deepest, (non-array) level. *)
    WITH lastLevelInfo = top.levels^ [LAST (top.levels^)] DO 
      lastLevelInfo.staticLen := Expr.lengthNonArray;
      lastLevelInfo.repType := levelType;
      lastLevelInfo.semType := levelType;
    END;
    
    (* Recursively traverse and check the tree of nested array constructors.
       Also compute levelInfo.staticLen, top.firstArgExpr, top.firstArgDepth. *)
    CheckRecurse (top, top, cs, depth := 0);
    top.state := StateTyp.Checked;
    top.checked := TRUE;
  END Check;

PROCEDURE CheckRecurse
  (top, constr: P; VAR cs: Expr.CheckState; depth: INTEGER) =
  VAR argLength, argCt: INTEGER;
  VAR priorErrCt, priorWarnCt, laterErrCt, laterWarnCt: INTEGER;
  VAR depthWInArg, depthWInTopConstr: INTEGER;
  VAR argSemType, argRepType, argIndexType, argEltType: Type.T;
  VAR value, minE, maxE: Expr.T;
  VAR argConstr: P;
  VAR key: M3ID.T;
  VAR eltTypeInfo: Type.Info;
  VAR b: BOOLEAN;
  BEGIN
    <* ASSERT constr.state = StateTyp.Fresh *>
    constr.state := StateTyp.Checking;
    <* ASSERT constr.isNested = (depth > 0) *>
    IF depth > 0 THEN
      constr.type := Type.Check (constr.type);
      constr.tipe := constr.type;
      constr.semType := constr.type;
    END;
    constr.depth := depth;
    constr.top := top;
    EVAL Type.CheckInfo (constr.semEltType, eltTypeInfo);

    IF constr.type = ErrType.T
       OR constr.semIndexType = ErrType.T
       OR constr.semEltType = ErrType.T
    THEN
      constr.broken := TRUE;
      constr.state := StateTyp.Checked;
      RETURN
    END;
    IF constr.semIndexType # NIL THEN
      (* Don't do Type.Check on constr.semIndexType=NIL. It would turn the NIL
         into ErrType.T, losing an open array. *)
      constr.semIndexType := Type.Check (constr.semIndexType);
IsErr(constr.semIndexType) (* Just for debugging. *);
    END;
    
    (* Check argument list length against constructor index type
       and compute constr.eltCt. *)
    argCt := ArgCt (constr);
    IF constr.semIndexType # NIL THEN (* Fixed array semType. *)
      constr.eltCt := LengthOfOrdType (constr.semIndexType);
      IF argCt > constr.eltCt THEN
        Error.Int
          (argCt, "too many values specified in fixed array constructor.");
        FOR i := constr.eltCt TO argCt - 1 DO
          constr.args^[i] := NIL
        END;
      ELSIF argCt < constr.eltCt AND NOT constr.dots THEN
        Error.Msg ("not enough values specified in fixed array constructor.");
      END;
    ELSE (* Open array semType. *)
      constr.eltCt := argCt;
      IF (constr.dots) THEN
        Error.Msg ("\"..\" not allowed in open array constructor");
        (* NOTE: ^The language seems to say this is illegal.  It was
                 previously a warning, so this change could invalidate
                 existing code. *)
        constr.dots := FALSE;
      END;
    END;
    WITH selfLevelInfo = top.levels^ [constr.depth] DO
      IF selfLevelInfo.staticLen >= 0
      THEN 
        IF constr.eltCt # selfLevelInfo.staticLen
        THEN
          Error.Msg ("Constructor argument's list length unequal to fixed "
                     & " length of declared type." );
          constr.broken := TRUE;
        END;
      ELSE selfLevelInfo.staticLen := constr.eltCt;
      END;
    END;

    (* Go througn the arguments. *)
    WITH argLevelInfo = top.levels^ [constr.depth + 1] DO
      FOR i := 0 TO argCt - 1 DO
        WITH argExpr = constr.args^ [i] DO
          IF argExpr # NIL THEN
            (* Check for arg forms that are not valid in an array constructor: *)
            IF KeywordExpr.Split (argExpr, key, value) THEN
              Error.Msg ("keyword values not allowed in array constructors");
              argExpr := NIL;
              constr.broken := TRUE;
            ELSIF RangeExpr.Split (argExpr, minE, maxE) THEN
              Error.Msg ("range values not allowed in array constructors");
              argExpr := NIL;
              constr.broken := TRUE;
            ELSE
              argConstr := ArrayConstrExpr (argExpr);
              IF argConstr # NIL AND argConstr.isNested THEN
                (* argConstr is an inner array constructor. *)
                CheckRecurse (top, argConstr, cs, depth + 1);
                argLength := argConstr.eltCt;
                argSemType := argConstr.semType;
              ELSE (* argExpr is a non-array or non-constructor. *)
                Expr.TypeCheck (argExpr, cs);
                IF top.firstArgExpr = NIL THEN
                  top.firstArgExpr := argExpr;
                  top.firstArgDepth := depth;
                END;
                argSemType := Expr.SemTypeOf (argExpr);
              END (*TYPECASE*);

              (* Check type/type assignability of arg to this constructor. *)
              IF argSemType # NIL THEN
                IF NOT Type.IsAssignable (constr.semEltType, argSemType) THEN
                  Error.Int (i,
                     "expression is not assignable to containing array"
                     & " constructor's element type.");
                  argExpr := NIL;
                  constr.broken := TRUE;
                (* And to top constructor, if different. *)
                ELSIF constr # top 
                      AND NOT Type.IsAssignable (argLevelInfo.semType, argSemType)
                THEN
                  Error.Int (i,
                     "Expression is not assignable to top-level array"
                     & " constructor's element type.");
                  argExpr := NIL;
                  constr.broken := TRUE;
                END;
              END;

              IF NOT constr.broken THEN
                (* Check shape criterion of assignabilty. *)
                IF argConstr # NIL THEN
                  CheckArgStaticLength
                    (constr, argLevelInfo, argLength, constr.depth + 1);

                ELSE (* Arg is not a constructor. *)
                  Error.Count (priorErrCt, priorWarnCt);
                  AssignStmt.Check (constr.semEltType, argExpr, cs);
(* FIXME^  On a constant-valued ordinal being out of range, This will only warn,
           leaving a RT error to occur during execution.
           But if it's going into a global area, nothing happens at RT.
           We need to make it a CT error. *)
                  Error.Count (laterErrCt, laterWarnCt);
                  IF laterErrCt > priorErrCt THEN
                    argExpr := NIL
                  ELSE
                    argRepType := Expr.RepTypeOf (argExpr);
                    depthWInArg := 0;
                    LOOP (* Thru array levels of arg. *)
                      depthWInTopConstr
                        := constr.depth + 1 (*For arg*) + depthWInArg;
                      IF depthWInTopConstr >= LAST (top.levels^) THEN EXIT END;
                      b := ArrayType.Split
                             (argRepType, (*VAR*) argIndexType,  (*VAR*) argEltType);
                      <* ASSERT b *>
                      IF argIndexType # NIL THEN
                        argLength := LengthOfOrdType (argIndexType);
                        CheckArgStaticLength
                          (constr, top.levels^ [depthWInTopConstr], argLength,
                           depthWInTopConstr);
                      END;
                      argRepType := argEltType;
                      INC (depthWInArg);
                    END  (*LOOP*)
                  END
                END
              END
            END
          END (*IF argExpr # NIL*)
        END (*WITH argExpr*)
      END (*FOR*)
    END (*WITH argLevelInfo*);

    constr.state := StateTyp.Checked;
  END CheckRecurse;

(* --------- Things dependent on Check having been done. -------- *)

(* EXPORTED: *) 
PROCEDURE ConstSubscript
  (array, indexExpr: Expr.T;  VAR resultExpr: Expr.T): (*Success*) BOOLEAN =
(* Will look through a ConsExpr. *)
(* PRE: array is Checked. *)
(* PRE: indexExpr is Checked. *)
  VAR constr: P;
  VAR i, n: INTEGER;
  VAR t: Type.T;
  VAR int, min, max, offs: Target.Int;
  VAR b: BOOLEAN;
  BEGIN
    <* ASSERT array.checked *>
    constr := ArrayConstrExpr (array);
    IF constr = NIL THEN RETURN FALSE END;
    <* ASSERT indexExpr.checked *>

    IF ArgCt (constr) <= 0 THEN RETURN FALSE END;
    n := LAST (constr.args^);
    indexExpr := Expr.ConstValue (indexExpr);
    IF (NOT IntegerExpr.Split (indexExpr, int, t))
      AND (NOT EnumExpr.Split (indexExpr, int, t)) THEN
      RETURN FALSE;
    END;
    IF constr.semIndexType = NIL THEN (* Open array. *)
      min := TInt.Zero;
      b := TInt.FromInt (n, max);
      <* ASSERT b *>
    ELSE
      EVAL Type.GetBounds (constr.semIndexType, min, max);
    END;
    IF TInt.GT (int, max) THEN RETURN FALSE END;
    (* Subtract lower bound, giving a zero-based subscript. *)
    IF NOT TInt.Subtract (int, min, offs) THEN RETURN FALSE END;
    IF TInt.LT (offs, TInt.Zero) THEN RETURN FALSE END;
    IF NOT TInt.ToInt (offs, i) THEN RETURN FALSE END;

    IF (i <= n) THEN resultExpr := constr.args^[i]; RETURN TRUE END;
    IF (constr.dots) THEN resultExpr := constr.args^[n]; RETURN TRUE END;
(* TODO: ^This would benefit from args being changed to fixed. Huh? *)    
    RETURN FALSE;
  END ConstSubscript;

(* EXPORTED: *) 
PROCEDURE ShapeCheckNeeded (expr: Expr.T): BOOLEAN =
(* PRE: If expr is an array constructor, it is top-level and Checked. *)
(* If expr is an array constructor, assigning expr will require CT or RT array
   shape check. (Otherwise, ArrayExpr will take care of shape checks.) *)
(* Will look through a ConsExpr. *)
  VAR top: P;
  BEGIN
    top := ArrayConstrExpr (expr);
    IF top = NIL THEN RETURN FALSE END;
    <* ASSERT top.depth = 0 *>
    IF top.isNamedConst THEN RETURN FALSE END;
    <* ASSERT top.state >= StateTyp.Representing *>
      (* ^So targetType is final. *)
    IF top.targetType = NIL THEN RETURN TRUE END;
    IF OpenArrayType.OpenDepth (top.targetType) = 0 THEN RETURN FALSE END;
    IF NOT top.useTargetVar THEN top.useTargetVarAccessed := TRUE END;
    RETURN NOT top.useTargetVar
  END ShapeCheckNeeded;

(* Externally dispatched-to: *)
PROCEDURE StaticLength (constr: P): INTEGER =
(* PRE: constr.checked. *)
  BEGIN
    <* ASSERT constr.checked *>
    RETURN constr.eltCt;
  END StaticLength;

(* Externally dispatched-to: *)
PROCEDURE IsZeroes (constr: P;  <*UNUSED*> lhs: BOOLEAN): BOOLEAN =
(* PRE: constr.checked. *)
  BEGIN
    IF constr.args = NIL THEN RETURN TRUE (*Vacuously*) END;
    FOR i := 0 TO LAST (constr.args^) DO
      IF NOT Expr.IsZeroes (constr.args^[i]) THEN RETURN FALSE END;
    END;
    RETURN TRUE;
  END IsZeroes;

(* Externally dispatched-to: *)
PROCEDURE Evaluate (constr: P): Expr.T =
(* PRE: constr.checked. *)
(* Return a constant expr if constr is constant, otherwise NIL. *)
(* NOTE: This will fold any constant argument in place, even if the
         whole constructor is not constant. *)
  VAR e: Expr.T;
  BEGIN
    IF NOT constr.evalAttempted THEN
      constr.evalAttempted := TRUE;
      constr.is_const := TRUE;
      FOR i := 0 TO LAST (constr.args^) DO
        WITH arg = constr.args^[i] DO
          e := Expr.ConstValue (arg);
          IF (e = NIL)
          THEN constr.is_const := FALSE;
          ELSE arg := e;
          END;
        END (*WITH*)
      END;
    END;
    IF constr.is_const
    THEN RETURN constr;
    ELSE RETURN NIL;
    END;
  END Evaluate;

(* Externally dispatched-to: *)
PROCEDURE GenFPLiteral (p: P;  buf: M3Buf.T) =
(* PRE: constr.checked. *)
(* "FP" is for "fingerprint". *)
  VAR argCt: INTEGER;
  VAR e: Expr.T;
  BEGIN
    argCt := ArgCt (p);
    M3Buf.PutText (buf, "ARRAY<");
    FOR i := 0 TO argCt-1 DO
      IF (i > 0) THEN M3Buf.PutChar (buf, ',') END;
      Expr.GenFPLiteral (p.args^[i], buf);
    END;
    IF p.semIndexType # NIL AND p.dots AND p.eltCt > argCt THEN
      e := p.args^[argCt-1];
      e := Expr.ConstValue (e);
      FOR i := argCt TO p.eltCt DO
(* FIXME: reintroduce this change to go to p.eltCt-1. *)
      (* NOTE: Earlier, this loop incorrectly went to eltCt.  Fixing this
               could make earlier-written pickles unreadable by currently-
               compiled code. *)
        M3Buf.PutChar (buf, ',');
        Expr.GenFPLiteral (e, buf);
      END;
    END;
    M3Buf.PutChar (buf, '>');
  END GenFPLiteral;

(* --------------------- Representation ------------------- *)

(* EXPORTED: *)
PROCEDURE NoteTargetType (expr: Expr.T; type: Type.T) =
(* PRE: If expr is an array constructor, it is top-level.
   If so, arrange for it to be compiled having type 'type'. *)
(* Will look through a ConsExpr. *)
  VAR top: P;
  VAR baseType: Type.T; 
  BEGIN
    top := ArrayConstrExpr (expr);
    IF top = NIL THEN RETURN END;
    <* ASSERT NOT top.isNested *>
    baseType := Type.StripPacked (type);
    IF baseType = NIL THEN RETURN END;
    IF baseType = top.targetType THEN (* No change. *) RETURN END;
    IF top.targetType = NIL THEN (* First time noted. *)
      IF top.state >= StateTyp.Representing
      THEN (* Too late to use this info. *)
        Error.Msg ("Target type supplied too late to use -- "
                   & "harmless except for possible efficiency loss.");
        RETURN
      ELSE
        top.targetType := baseType;
      END;
    ELSE
      Error.Msg ("Target type changed -- "
                 & "harmless except for possible efficiency loss.");
      top.targetType := baseType;
    END;
  (* Represent (top); *)
  END NoteTargetType;
  
PROCEDURE IndexTypeForLength (length: INTEGER): Type.T =
(* Type [0..length-1] *)
  VAR result: Type.T;
  VAR LastTI: Target.Int;
  BEGIN
    WITH b = TInt.FromInt (length-1, LastTI) DO <* ASSERT b *> END;
    result := SubrangeType.New (TInt.Zero, LastTI, Int.T, builtin := FALSE);
    (* Don't call Type.Check on this.  It expects syntactic expressions
       for the bounds, and we don't have them. *)
(* TODO: Cache these types, possibly inside SubrangeType. *)
    RETURN result; 
  END IndexTypeForLength; 

CONST eltPackInconsistent = - 1;
CONST eltPackIrrelevant = - 2;
CONST eltPackUnknown = - 3;

PROCEDURE CommonEltPack
  (top: P; superType, subType: Type.T; depth: INTEGER): INTEGER =
(* Common eltPack of both types, when superType is open and subType is fixed.
   eltPackIrrelevant, if these are not array types.
   eltPackInconsistent, if element packings are unequal.
   otherwise, the common eltPack. *)
(* PRE: Both superType and subType are assignable to top.levels^[depth].repType *)
(* PRE: subType <: superType. *)

  VAR superIndexType, superEltType: Type.T; 
  VAR subIndexType, subEltType : Type.T; 
  VAR subEltPack, superEltPack, deeperEltPack: INTEGER;
  VAR subIsArray, superIsArray: BOOLEAN;

  BEGIN
    WITH levelInfo = top.levels^ [depth] DO
      IF levelInfo.FixedOpenChecked THEN RETURN levelInfo.eltPack END;
      subIsArray
        := ArrayType.Split (* Skips naming and packing. *)
             (subType, (*VAR*)subIndexType, (*VAR*)subEltType);
      superIsArray
        := ArrayType.Split
             (superType, (*VAR*)superIndexType, (*VAR*)superEltType);
      <* ASSERT subIsArray = superIsArray *>
      IF NOT subIsArray THEN
        levelInfo.eltPack := eltPackIrrelevant;
        levelInfo.FixedOpenChecked := TRUE;
      ELSE
        subEltPack := ArrayType.EltPack (subType);
        IF superIndexType # NIL
        THEN (* Both superType and subType are fixed arrays. *)
          <* ASSERT subIndexType # NIL *>
          (* No info *)
          RETURN eltPackUnknown; 
        ELSIF subIndexType = NIL
        THEN (* Both superType and subType are open arrays. *)
          (* No info about this level, but we can still check deeper. *)
          deeperEltPack
            := CommonEltPack (top, superEltType, subEltType, depth + 1);
          IF deeperEltPack = eltPackInconsistent (* This propagates. *)
          THEN
            levelInfo.eltPack := eltPackInconsistent;
            levelInfo.FixedOpenChecked := TRUE;
          ELSE 
            RETURN eltPackUnknown; 
          END
        ELSE (* superType is open, subType is fixed. *)
          (* The interesting case.  A fixed/open pair. *)
          deeperEltPack
            := CommonEltPack (top, superEltType, subEltType, depth + 1);
          IF deeperEltPack = eltPackInconsistent THEN (* This propagates. *)
            levelInfo.eltPack := eltPackInconsistent;
            levelInfo.FixedOpenChecked := TRUE;
          ELSIF deeperEltPack = eltPackIrrelevant THEN
            superEltPack := OpenArrayType.EltPack (superType);
            <* ASSERT superEltPack = subEltPack *>   
            levelInfo.eltPack := subEltPack;
            levelInfo.FixedOpenChecked := TRUE;
          ELSE
            <* ASSERT levelInfo.staticLen >= 0 *>
            superEltPack := levelInfo.staticLen * deeperEltPack; 
            IF superEltPack # subEltPack THEN
              levelInfo.eltPack := eltPackInconsistent;
              levelInfo.FixedOpenChecked := TRUE;
            ELSE
              levelInfo.eltPack := subEltPack;
              (* All fixed types here have levelInfo.staticLen.  So if any
                 such type's eltPack matched the one open type here, all
                 fixed types will match it. *)
              levelInfo.FixedOpenChecked := TRUE;
            END
          END
        END
      END;
      RETURN levelInfo.eltPack;
    END (*WITH*) 
  END CommonEltPack;

PROCEDURE CheckFixedOpenEltPack (top: P; typeX, typeY: Type.T; depth: INTEGER)
: (* Not known to be bad. *) BOOLEAN =
(* PRE: top is a top-level constructor. *)
(* PRE: typeX and typeY are both assignable to repType of levels at 'depth'.
        Thus they differ from it and each other only in fixedness vs. openness
        in each corresponding dimension. *)
(* This is all to ensure that fixed and open arrays of same element type and
   that are copied-between have the same element packing.  This could maybe
   be inferred from type checking, but it is rather complicated to do so.
   So we verify it with this and its helper procedure. *)
  VAR eltPack: INTEGER;
  BEGIN
    <* ASSERT top.depth = 0 *>
    IF NOT ArrayType.Is (typeX) THEN RETURN TRUE END;
    IF NOT ArrayType.Is (typeY) THEN RETURN TRUE END;
    IF typeX = typeY THEN RETURN TRUE END;
    IF typeX = NIL OR typeY = NIL THEN RETURN TRUE END;
    IF OpenArrayType.Is (typeX) THEN
      eltPack := CommonEltPack (top, typeX, typeY, depth)
    ELSE
      eltPack := CommonEltPack (top, typeY, typeX, depth)
    END;
    RETURN eltPack # eltPackInconsistent
  END CheckFixedOpenEltPack;

VAR DebugOrigin: INTEGER;

PROCEDURE Represent (top: P) =
(* PRE: top is the top of a tree of array constructors. *)
  VAR levelType, levelIndexType, levelEltType, semIndexType, semEltType: Type.T;
  VAR repType, repIndexType, repEltType, repSuccType: Type.T;
  VAR staticLen, staticFlatEltCt, lastArrayDepth, eltsSize: INTEGER;
  VAR topRepTypeInfo: Type.Info;
  VAR repSuccIsOpen, repSuccHasChanged, fixedOpenOK, b: BOOLEAN;
  BEGIN
    <* ASSERT top.depth = 0 *>
    IF top.state >= StateTyp.Represented THEN RETURN END;
    <* ASSERT top.state >= StateTyp.Checked *>
    IF top.broken THEN
      top.state := StateTyp.Represented;
      RETURN
    END;
    top.state := StateTyp.Representing;
    IF top.targetType # NIL THEN
(* CHECK: Clients will likely duplicate this assignability check.
          Can we eliminate the duplication? *)
      top.targetType := Type.Check (top.targetType);
      IF NOT Type.IsAssignable (top.targetType, top.semType) THEN
        Error.Msg
          ( "array constructor is not assignable to expected type.");
        top.broken := TRUE;
        top.state := StateTyp.Represented;
        RETURN;
      END;

      (* Collect more level info (staticLen) from top.targetType. *)
      levelType := top.targetType;
      FOR i := 0 TO LAST (top.levels^) - 1 DO
        WITH levelInfo = top.levels^[i] DO
          b := ArrayType.Split
                 (levelType, (*VAR*)levelIndexType, (*VAR*)levelEltType);
          <* ASSERT b *>
          staticLen := LengthOfOrdType (levelIndexType);
          IF staticLen >= 0 THEN 
            IF levelInfo.staticLen = Expr.lengthNonStatic
            THEN (* semType was open in this dimension.  Make it static. *)
              levelInfo.staticLen := staticLen
            ELSIF staticLen # levelInfo.staticLen THEN
              Error.Int
                (i, "Constructor length # expression length, in dimension." );
              <* ASSERT FALSE *>
              (* ^top.targetType assignability checks above should avert this. *)
              top.broken <* NOWARN *> (* Unreachable. *) := TRUE;
              top.state := StateTyp.Represented;
              RETURN
            END;
          END;
          levelType := levelEltType;
        END
      END;
    END;
    
    (* Construct the repType for the top-level constructor. *)
    IF top.targetType = NIL
    THEN (* repType comes solely from the array constructor and its semType.
            Build it inside to out. *)
      repSuccIsOpen := FALSE; (* The innermost, non-array level. *)
      repSuccHasChanged := FALSE;
      repSuccType := top.levels ^[LAST (top.levels ^)].semType;
      staticFlatEltCt := 1;
      FOR i := LAST (top.levels ^) - 1 TO 0 BY - 1 DO
        WITH levelInfo = top.levels ^[i] DO
          b := ArrayType.Split (levelInfo.semType, semIndexType, semEltType);
          <* ASSERT b *>
          IF semIndexType = NIL (* SemType is open in this dimension, *)
             AND levelInfo.staticLen >= 0 
                 (* but it's statically constrained by context, *)
             AND NOT repSuccIsOpen 
               (* ^and not forced open by an inner open dimension. *)
          THEN (* Make it fixed in the repType. *)
            repIndexType := IndexTypeForLength (levelInfo.staticLen);
            levelInfo.repIndexType := repIndexType;
            levelInfo.repType := ArrayType.New (repIndexType, repSuccType);
            levelInfo.repType := Type.Check (levelInfo.repType);
            repSuccHasChanged := TRUE;
          ELSE
            repIndexType := semIndexType;
            levelInfo.repIndexType := repIndexType;
            IF repSuccHasChanged
            THEN (* Same indexType, different eltType pointer. *)
              levelInfo.repType := ArrayType.New (repIndexType, repSuccType);
              levelInfo.repType := Type.Check (levelInfo.repType);
            ELSE
              levelInfo.repType := levelInfo.semType;
            END;
          END;

          (* Develop shallowestDynDepth and deepestDynDepth. *)
          IF levelInfo.staticLen = Expr.lengthNonStatic THEN
            (* All cousins at depth i are open and nonstatic. *)
            top.deepestDynDepth := MAX (top.deepestDynDepth, i);
            top.shallowestDynDepth := i;
          END;

          (* Develop static element count product. *) 
          IF levelInfo.staticLen >= 0
          THEN 
            levelInfo.staticFlatEltCt := staticFlatEltCt * levelInfo.staticLen;
            staticFlatEltCt := levelInfo.staticFlatEltCt;
          ELSE (* Start the product over. *)
            staticFlatEltCt := 1;
          END;
        repSuccType := levelInfo.repType;
        IF repIndexType = NIL THEN repSuccIsOpen := TRUE END;
        END (*WITH*);
      END (*FOR*);
      top.repType := repSuccType;
      top.repIndexType := repIndexType;
    ELSE (* repType is targetType. Just copy targetType component pointers. *)
         (* Do it outside in. *)
      repType := top.targetType;
      FOR i := 0 TO LAST (top.levels ^) - 1 DO
        WITH levelInfo = top.levels ^[i] DO
          b := ArrayType.Split (repType, repIndexType, repEltType);
          <* ASSERT b *>
          IF i = 0 THEN
            top.repType := repType;
            top.repIndexType := repIndexType;
          END;
          levelInfo.repType := repType;
          levelInfo.repIndexType := repIndexType;

          (* Develop shallowestDynDepth and deepestDynDepth. *)
          IF levelInfo.staticLen = Expr.lengthNonStatic THEN
            (* All cousins at depth i are open and nonstatic. *)
            top.deepestDynDepth := i;
            IF top.shallowestDynDepth < 0 THEN
              top.shallowestDynDepth := i;
            END
          END;

          repType := repEltType;              
        END (*WITH*);
      END (*FOR*);

      (* Compute static element count product, inside to out. *) 
      staticFlatEltCt := 1;
      FOR i := LAST (top.levels ^) - 1 TO 0 BY - 1 DO
        WITH levelInfo = top.levels ^[i] DO
          IF levelInfo.staticLen >= 0
          THEN 
            levelInfo.staticFlatEltCt := staticFlatEltCt * levelInfo.staticLen;
            staticFlatEltCt := levelInfo.staticFlatEltCt;
          ELSE (* Start the product over. *)
            staticFlatEltCt := 1;
          END;
        END (*WITH*);
      END (*FOR*);

    END (*ELSE targetType # NIL*);

    (* Compute representation sizes. *)
    IF top.shallowestDynDepth >= 0
    THEN top.staticEltsSize :=0;
    ELSE
      lastArrayDepth := LAST(top.levels^) - 1;
      eltsSize := ArrayType.EltPack (top.levels^[lastArrayDepth].repType);
      FOR depth := 0 TO lastArrayDepth DO
        staticLen := top.levels^[depth].staticLen;
        <* ASSERT staticLen >= 0 *>
        eltsSize := eltsSize * staticLen;
      END (*FOR*);
      top.staticEltsSize := eltsSize;
    END;
    top.repOpenDepth := OpenArrayType.OpenDepth (top.repType);
    IF top.repOpenDepth = 0 THEN
      top.dopeSize := 0
    ELSE
      top.dopeSize
        := Target.Address.size + top.repOpenDepth * Target.Integer.size;
    END;
    top.totalSize := top.dopeSize + top.staticEltsSize;

    (* Compute representation alignments. *)
    EVAL Type.CheckInfo (top.repType, topRepTypeInfo);
    top.topRepAlign := topRepTypeInfo.alignment;
    top.topEltsAlign := ArrayType.EltAlign (top.repType);
    IF top.topEltsAlign < Target.Word8.align THEN
      Error.InfoInt
        (top.topEltsAlign,
         "Array constructor element group has sub-byte alignment");
    END;

    (* Make sure fixed and open arrays of same length have the same bitsize. *)
    fixedOpenOK
      := CheckFixedOpenEltPack (top, top.semType, top.repType, depth := 0);
    <* ASSERT fixedOpenOK *>

    (* Apply repType to the tree of array constructors and Check.*)
    RepresentRecurse (top);
    
    (* CHECK: The below are segfaulting in Builder when elements are opaque,
              due to Host.env being NIL.  Do we need these?  Maybe do them
              later, in Compile? *)
(*
    top.semType := Type.Check (top.semType);
    Type.Compile (top.semType);
    top.repType := Type.Check (top.repType);
    Type.Compile (top.repType);
*) 
<* ASSERT top.repType # NIL *>            
    top.state := StateTyp.Represented
  END Represent;

PROCEDURE RepresentRecurse (constr: P) =
  VAR top: P;
  VAR argCt: INTEGER;
  VAR argRepType: Type.T;
  VAR argExpr: Expr.T;
  VAR argConstr: P;
  VAR fixedOpenOK: BOOLEAN;
  BEGIN
    <* ASSERT constr.state >= StateTyp.Checked *>
    constr.state := StateTyp.Representing;
    IF NOT constr.broken THEN
      top := constr.top;
      argCt := ArgCt (constr);

      (* Traverse the arguments, copying representation types into them. *)
      WITH argLevelInfo = top.levels^ [constr.depth + 1] DO
        FOR i := 0 TO argCt - 1 DO
          argExpr := constr.args^[i];
          IF argExpr # NIL THEN
            argConstr := ArrayConstrExpr (argExpr);
            IF argConstr # NIL AND argConstr.isNested THEN
              (* argConstr is an inner array constructor. *)
              RepresentRecurse (argConstr);
            ELSE (* argExpr is non-array or non-constructor. *)
              argRepType := Expr.RepTypeOf (argExpr);
              fixedOpenOK
                := CheckFixedOpenEltPack
                     (top,
                      argLevelInfo.repType, argRepType, constr.depth + 1);
              <* ASSERT fixedOpenOK *>
            END
          END
        END (*FOR args*);
      END (*WITH argLevelInfo*);

      (* Copy fields from level to constructor. *)
      IF constr # top THEN
        WITH constrLevelInfo = top.levels^[constr.depth] DO
          constr.repIndexType := constrLevelInfo.repIndexType;
          constr.repType := constrLevelInfo.repType;
        END (*WITH*);
      END;
    END;
    constr.state := StateTyp.Represented;
  END RepresentRecurse;

(* ---------------- Dependent on Represent ---------------- *)

PROCEDURE Classify (top: P) =
(* Classify the method/location for storing the constructor's value. *)
  BEGIN
    IF top.resultKind # RKTyp.RKUnknown THEN RETURN END;
    <* ASSERT top.depth = 0 *>
    <* ASSERT top.state >= StateTyp.Represented *>
    IF Evaluate (top) # NIL THEN (* It's a constant. *)
      <* ASSERT top.shallowestDynDepth < 0 *>
      top.resultKind := RKTyp.RKGlobal
    ELSIF top.doDirectAssign THEN (* Build value directly into final location. *)
      IF top.repOpenDepth <= 0
      THEN top.resultKind := RKTyp.RKDirectElts;
      ELSE top.resultKind := RKTyp.RKDirectDoped;
      END
    ELSE (* Create and build into a temporary, to be copied later. *)
      IF top.repOpenDepth <= 0
      THEN top.resultKind := RKTyp.RKTempElts;
      ELSIF top.shallowestDynDepth < 0
      THEN top.resultKind := RKTyp.RKTempStatic;
      ELSE top.resultKind := RKTyp.RKTempDyn
      END
    END;
  END Classify; 

(* EXPORTED: *) 
PROCEDURE GetBounds
  (expr: Expr.T;  VAR min, max: Target.Int): (* Success *) BOOLEAN =
(* Will look through a ConsExpr. *)
  VAR constr: P;
  VAR b: BOOLEAN;
  BEGIN
    constr := ArrayConstrExpr (expr);
    IF constr = NIL THEN RETURN FALSE END;
    IF constr.semIndexType = NIL THEN (* open array type *)
      min := TInt.Zero;
      b := TInt.FromInt (ArgCt (constr) - 1, max);
      RETURN b;
    ELSE
      RETURN Type.GetBounds (constr.semIndexType, min, max);
    END;
  END GetBounds;

(* Externally dispatched-to: *)
PROCEDURE RepTypeOf (constr: P): Type.T =
(* PRE: constr is Checked. *)
  BEGIN
    IF constr = NIL THEN RETURN NIL END;
    <* ASSERT constr.state >= StateTyp.Checked *>
 IF constr.depth > 0 THEN
   Error.Msg ("RepTypeOf called on nested array constructor.")
 END;
    Represent (constr.top);
    <* ASSERT constr.repType # NIL *>
    IF constr.repType = NIL THEN RETURN constr.semType END;
    RETURN constr.repType;
  END RepTypeOf;

(* --- Prep --- *)

(* EXPORTED: *)
PROCEDURE NoteUseTargetVar (expr: Expr.T) =
(* NOOP if expr is not an array constructor.  Otherwise: *)
  (* PRE: expr is top-level, has been checked but not prepped, and
          UsesAssignProtocol (expr) has not yet been called. *)
  (* Arrange to use LHS from the CG stack to set nonstatic shape components. *)
  (* Will look through a ConsExpr. *)
  VAR top: P;
  BEGIN
    top := ArrayConstrExpr (expr);
    IF top = NIL THEN RETURN END;
    IF top.state < StateTyp.Checked THEN RETURN END (*Called too early. *);
    IF top.isNamedConst THEN RETURN END;
    IF top.state >= StateTyp.Prepped THEN RETURN END (*Called too late. *);
    <* ASSERT top.depth = 0 *>
    <* ASSERT NOT top.usesAssignProtocolCalled *>
    <* ASSERT NOT top.useTargetVarAccessed *>
    top.useTargetVar := TRUE;
  END NoteUseTargetVar;

(* Externally dispatched-to: *)
PROCEDURE UsesAssignProtocol (expr: P): BOOLEAN =
(* PRE: IF expr is an array constructor, THEN it is top-level. *)
(* Will look through a ConsExpr. *)
  VAR top: P;
  BEGIN
    top := ArrayConstrExpr (expr);
    IF top = NIL THEN RETURN FALSE END;
    <* ASSERT top.depth = 0 *>
    IF top.is_const THEN RETURN FALSE END;
    top.usesAssignProtocolCalled := TRUE;
    IF top.doDirectAssign THEN RETURN TRUE END;
    top.useTargetVarAccessed := TRUE;
    RETURN top.useTargetVar     
  END UsesAssignProtocol;

(* Externally dispatched-to: *)
PROCEDURE Prep (top: P) =
(* PRE: top.depth = 0. *)
  (* Must be called IFF top is the top of a tree of array constructors. *)
  BEGIN
    <* ASSERT top.depth = 0 *>
    IF top.broken THEN
      top.state := StateTyp.Prepped;
      RETURN
    END;
    IF Evaluate (top) # NIL THEN (* It's a constant. *)
      top.inConstArea := TRUE
      (* Postpone InnerPrep until Compile, when we have globalDopeOffset
         and globalEltsOffzet to build into. *)
(* TODO: Or, allocate globalspaces here, instead of in Compile? *)         
    ELSIF NOT UsesAssignProtocol (top)
    THEN InnerPrep (top)
 (* ELSE postpone InnerPrep until Compile & InnerCompile, when LHS will
    have been pushed. *)
    END;
  END Prep;

(* Externally dispatched-to: *)
PROCEDURE PrepLiteral (p: P; type: Type.T; inConstArea: BOOLEAN) =
(* If NOT inConstArea, do it in the initialized global area. *)
  VAR top: P;
  BEGIN
    top := p.top;
    <* ASSERT top.checked *>
    top.inConstArea := inConstArea;
    IF top.broken THEN
      top.state := StateTyp.Prepped;
      RETURN
    END;
    <* ASSERT Evaluate (top) # NIL *>
    top.targetType := type;
    Represent (top);
    (* Postpone InnerPrep until GenLiteral, when we will have the
       globalDopeOffset and globalEltsOffset to build into. *)
  END PrepLiteral;

PROCEDURE GenEvalFixingInfo
  (top: P; fixingDopeVal: CG.Val; fixingExprDepth: INTEGER) =
(* PRE: top is a top-level array constructor. *)
  VAR depthWInFixingExpr, depthWInTopConstr: INTEGER;
  VAR repType: Type.T;
  BEGIN
    IF top.shallowestDynDepth < 0 THEN (* No RT fixing info needed. *)
    RETURN END;
    IF top.fixingInfoComputed THEN (* We already did this. *) RETURN END;

    depthWInTopConstr := top.deepestDynDepth; (* Will go inside-out. *)
    <* ASSERT top.shallowestDynDepth <= depthWInTopConstr *>
    
    (* Go through the dynamic levels inside to out. *)
    repType := top.levels^[depthWInTopConstr].repType;
    <* ASSERT OpenArrayType.Is (repType) *>
    CG.Load_intt (OpenArrayType.EltPack (repType));
    LOOP
      depthWInFixingExpr := depthWInTopConstr - fixingExprDepth;
      WITH levelInfo = top.levels^[depthWInTopConstr] DO
        CG.Push (fixingDopeVal);
        CG.Open_size (depthWInFixingExpr);
        levelInfo.fixingVal := CG.Pop ();
        CG.Push (levelInfo.fixingVal);
        CG.Multiply (Target.Integer.cg_type);
        levelInfo.dynBitsizeVal := CG.Pop ();
        IF depthWInTopConstr <= top.shallowestDynDepth THEN
          EXIT
        ELSE 
          CG.Push (levelInfo.dynBitsizeVal);
          DEC (depthWInTopConstr);
        END;
      END (*WITH*);
    END (*LOOP*);
    top.fixingInfoComputed := TRUE;
  END GenEvalFixingInfo;

PROCEDURE InitVarDope (top: P; var: CG.Var) =
(* PRE: top.depth = 0. *)
  VAR length: INTEGER;
  BEGIN
    IF top.repOpenDepth > 0 THEN
      CG.Load_addr_of
        (var, top.dopeSize, Target.Integer.align (*Irrelevant*)); 
      CG.Store
        (var, M3RT.OA_elt_ptr, Target.Address.size,
         Target.Address.align, Target.Address.cg_type);
      FOR i := 0 TO top.repOpenDepth - 1 DO
        length := top.levels^[i].staticLen;
        <* ASSERT length >= 0 *>
        CG.Load_intt (length);
        CG.Store
          (var, M3RT.OA_size_0 + i * Target.Integer.size, Target.Integer.size,
           Target.Integer.align, Target.Integer.cg_type);
      END
    END
  END InitVarDope; 

PROCEDURE InitValDope (top: P; val: CG.Val; VAR eltsAddrVal: CG.Val) =
(* PRE: top.depth = 0. *)
  VAR length: INTEGER;
  BEGIN
    IF top.repOpenDepth > 0 THEN
      (* Elements pointer. *)
      CG.Push (val);
      CG.Boost_addr_alignment (Target.Address.align);
      CG.Push (val);
      CG.Boost_addr_alignment (Target.Address.align);
      CG.Add_offset (top.dopeSize);
      eltsAddrVal := CG.Pop ();
      CG.Push (eltsAddrVal);
      CG.Boost_addr_alignment (top.topEltsAlign);
      CG.Store_indirect
        (Target.Address.cg_type, M3RT.OA_elt_ptr, Target.Address.size);

      (* Shape. *)
      FOR i := 0 TO top.repOpenDepth - 1 DO
        length := top.levels^[i].staticLen;
        <* ASSERT length >= 0 *>
        CG.Push (val);
        CG.Boost_addr_alignment (Target.Integer.align);
        CG.Load_intt (length);
        CG.Store_indirect
          (Target.Integer.cg_type, M3RT.OA_size_0 + i * Target.Integer.size,
           Target.Integer.size);
      END
    END
  END InitValDope;

PROCEDURE InnerPrep (top: P) =
(* PRE: top.depth = 0. *)
  (* Must be called IFF top is the top of a tree of array constructors. *)
  (* PRE LHS addr is on CG stack IFF UsesAssignProtocol. *)
  (* POST If UsesAssignProtocol, LHS has been popped and stored in some field. *)
  VAR
    shapeVar: CG.Var := NIL;
    dopeSize, shapeSize: INTEGER;
    NewTracedArrayProc: Procedure.T;
    length, offset: INTEGER;
    firstArgTypeInfo: Type.Info;
  BEGIN
    IF top.state >= StateTyp.Prepped THEN RETURN END;
    Represent (top);
    IF top.broken THEN
      top.state := StateTyp.Prepped;
      RETURN
    END;
    Classify (top);
    top.state := StateTyp.Prepping;

    (* Set up result location info. *)
    CASE top.resultKind OF
    | RKTyp.RKUnknown => <* ASSERT FALSE *>
    | RKTyp.RKGlobal => <* ASSERT top.globalDopeOffset >= 0 *>

    | RKTyp.RKDirectElts =>
      (* LHS Address is pushed. *)
      CG.Boost_addr_alignment (top.topRepAlign);
      top.buildEltsAddrVal := CG.Pop (); (* LHS points to elements. *)

    | RKTyp.RKDirectDoped =>
      (* LHS Address is pushed. *)
      CG.Boost_addr_alignment (Target.Address.align);
      top.buildAddrVal := CG.Pop (); (* LHS points to dope. *)
      InitValDope (top, top.buildAddrVal, (*OUT*)top.buildEltsAddrVal);
      GenEvalFixingInfo (top, top.buildAddrVal, fixingExprDepth := 0);

    | RKTyp.RKTempElts, RKTyp.RKTempStatic =>
      (* Allocate a static-sized CG.Var temp to build elements into. *)
      top.buildTempVar := CG.Declare_temp
        (top.totalSize, top.topRepAlign, CG.Type.Struct, in_memory:= TRUE);
(* TODO: CG.Declare_whatever, for this and other declared temps. *)

      IF top.resultKind = RKTyp.RKTempStatic THEN
        InitVarDope (top, top.buildTempVar);
      END;

      IF NOT top.useTargetVar THEN top.useTargetVarAccessed := TRUE END;
      IF top.useTargetVar THEN (* LHS address is on the CG stack. *)
        CG.Boost_addr_alignment (top.topRepAlign);
        top.finalVal := CG.Pop ();
      END;

    | RKTyp.RKTempDyn =>
      (* Gen code to evaluate shape-derived dynamic info. *)
      IF NOT top.useTargetVar THEN top.useTargetVarAccessed := TRUE END;
      IF top.useTargetVar THEN (* LHS address is on the CG stack. *)
        CG.Boost_addr_alignment (Target.Address.align);
        top.finalVal := CG.Pop ();
        GenEvalFixingInfo (top, top.finalVal, fixingExprDepth := 0);
        (* Postpone evaluating top.firstArgExpr. *)
      ELSE (* LHS is not pushed yet.  Use first arg to fix dynamic info. *)
        EVAL Type.CheckInfo
               (Expr.RepTypeOf (top.firstArgExpr), firstArgTypeInfo);
        Expr.PrepLValue (top.firstArgExpr, traced := firstArgTypeInfo.isTraced);
        Expr.CompileAddress
          (top.firstArgExpr, traced := firstArgTypeInfo.isTraced);
        top.firstArgDopeVal := CG.Pop ();
        GenEvalFixingInfo (top, top.firstArgDopeVal, top.firstArgDepth);
      END;

      (* Allocate a temporary "shape" array.  This confusing.  It is a
         one-dimensional open array of integers, complete with contiguous
         dope, to be passed to NewTracedArray, and its element list is the
         shape of the n-dimensional array we are building. *)
      <* ASSERT top.repOpenDepth = top.deepestDynDepth + 1 *>
      shapeSize := dopeSize + Target.Integer.size;
      shapeVar := CG.Declare_temp (shapeSize, Target.Address.align,
                                   CG.Type.Struct, in_memory := TRUE);

      (* Gen code to initialize the shape array. *)
      CG.Load_addr_of
        (shapeVar,
         M3RT.OA_size_1(* Not actually a size, rather 0th element. *),
         Target.Integer.align);
      CG.Store_addr (shapeVar, M3RT.OA_elt_ptr); (* Elements pointer. *)

      CG.Load_intt (top.repOpenDepth);
      CG.Store_int
        (Target.Integer.cg_type, shapeVar, M3RT.OA_size_0); (* Elt count. *)

      offset := M3RT.OA_size_1 (* 0th element. *);
      <* ASSERT top.shallowestDynDepth >= 0 *>

      (* Lengths w/open representation, but a static value is imposed. *)
      FOR depth := 0 TO top.shallowestDynDepth - 1 DO
        length := top.levels^ [depth].staticLen;
        <* ASSERT length >= 0 *>
        CG.Load_intt (length);
        CG.Store (shapeVar, offset, Target.Integer.size, Target.Integer.align
                  , Target.Integer.cg_type);
        INC (offset, Target.Integer.size);
      END;

      (* Lengths with dynamic, open length. *)
      FOR depth := top.shallowestDynDepth TO top.deepestDynDepth DO
        CG.Push (top.levels^ [depth].fixingVal);
        CG.Store (shapeVar, offset, Target.Integer.size, Target.Integer.align
                  , Target.Integer.cg_type);
        INC (offset, Target.Integer.size);
      END;

      (* Gen code to heap-allocate space for the Constructor's temp. *)
(* TODO: Would it be possible & better to allocate this untraced? *)        
      (* We didn't know whether refType would be needed, at Check time. *)
      top.refType := RefType.New (top.repType, traced := TRUE, brand := NIL);
      top.refType := Type.Check (top.refType);
      NewTracedArrayProc := RunTyme.LookUpProc (RunTyme.Hook.NewTracedArray);
      Procedure.StartCall (NewTracedArrayProc);
      IF Target.DefaultCall.args_left_to_right THEN
        Type.LoadInfo (top.refType, -1);
        CG.Pop_param (CG.Type.Addr);
        CG.Load_addr_of (shapeVar, 0, Target.Address.align);
        CG.Pop_param (CG.Type.Addr);
      ELSE
        CG.Load_addr_of (shapeVar, 0, Target.Address.align);
        CG.Pop_param (CG.Type.Addr);
        Type.LoadInfo (top.refType, -1);
        CG.Pop_param (CG.Type.Addr);
      END;
      top.buildAddrVal := Procedure.EmitValueCall (NewTracedArrayProc);
      (* ^Dope portion will have been initialized. *)
      CG.Push (top.buildAddrVal);
      CG.Boost_addr_alignment (Target.Address.align);
      CG.Open_elt_ptr (top.topEltsAlign);
      top.buildEltsAddrVal:= CG.Pop ();

      (* Gen code to store address of heap-allocated temp in a local temp,
         so GC will leave it alone. *)
      top.heapTempPtrVar
        := CG.Declare_temp
             (Target.Address.size, Target.Address.align, CG.Type.Addr,
              in_memory := TRUE);
      CG.Push (top.buildAddrVal);
      CG.Boost_addr_alignment (Target.Address.align);
      CG.Store_addr (top.heapTempPtrVar, (*Offset:=*) 0);

    END (*CASE top.resultKind*);
    (* If UsesAssignProtocol, LHS addr is now stored in a field of top:
       buildAddrVar, builtEltsAddrVar, or finalVal. *)

    (* Traverse the tree of array constructors. *)
    PrepRecurse (top, selfFlatEltNo := 0);

    (* remember the result and free the other temporaries *)
    CG.Free_temp (shapeVar);
    IF top.shallowestDynDepth >= 0 THEN
      FOR depth := top.shallowestDynDepth TO top.deepestDynDepth DO
        CG.Free (top.levels^ [depth].fixingVal);
        CG.Free (top.levels^ [depth].dynBitsizeVal);
      END;
    END;
    CG.Free (top.dynOffsetVal);
(* CHECK: Will Free_temps get done sometime? *)
    top.state := StateTyp.Prepped;
  END InnerPrep;

PROCEDURE GenCopyOpenArgValueDyn
  (constr: P; argDopeAddrVal: CG.Val; eltAlign: Type.BitAlignT) =
(* PRE: TOS is a CG.Val for address w/in LHS to copy to. *)
(* PRE: top.shallowestDynDepth >= 0 *)
  VAR top: P;
  BEGIN
    top := constr.top;
    (* Target address. *)
    CG.ForceStacked ();
    (* Source address. *)
    CG.Push (argDopeAddrVal);
    CG.Boost_addr_alignment (Target.Address.align);
    CG.Open_elt_ptr (eltAlign);
    CG.ForceStacked ();
    (* Size. *)
    CG.Load_intt
      (top.levels^[constr.depth + 1(*Of Arg. *)].staticFlatEltCt);
    <* ASSERT top.shallowestDynDepth >= 0 *>
    <* ASSERT top.shallowestDynDepth > constr.depth *>
    CG.Push (top.levels^[top.shallowestDynDepth].dynBitsizeVal);
    CG.Multiply (Target.Integer.cg_type);
(* TODO: call NoteWrite, as needed. *)

    CG.Copy_n (1(*bits*), overlap := TRUE);
  END GenCopyOpenArgValueDyn;

PROCEDURE GenPushLHSEltsAddr
  (top: P; bitOffset: INTEGER; eltAlign: Type.BitAlignT) =
(* PRE: top.depth = 0 *)
(* Push the address of the element at bitOffset within the elements
   portion of the temp or LHS location. *)
  BEGIN
    CASE top.resultKind OF
    | RKTyp.RKUnknown => <* ASSERT FALSE *>
    | RKTyp.RKGlobal => <* ASSERT FALSE *>
    | RKTyp.RKDirectElts, RKTyp.RKDirectDoped, RKTyp.RKTempDyn
    => CG.Push (top.buildEltsAddrVal);
      IF bitOffset # 0 THEN
        <* ASSERT bitOffset MOD eltAlign = 0 *>
        CG.Add_offset (bitOffset);
        CG.Boost_addr_alignment (eltAlign);
      END;
    | RKTyp.RKTempElts
    => CG.Load_addr_of (top.buildTempVar, bitOffset, eltAlign);
    | RKTyp.RKTempStatic
    => CG.Load_addr_of
         (top.buildTempVar, top.dopeSize + bitOffset, eltAlign);
    END (*CASE*);
  END GenPushLHSEltsAddr;

PROCEDURE PrepRecurse (constr: P; selfFlatEltNo: INTEGER) =
(* Called only on an array constructor. *)
(* selfFlatEltNo is the flattened element number of constr w/in the topmost
   array constructor. *)
  VAR top: P;
  VAR argRepType: Type.T;
  VAR dotSsVal, argAddrVal: CG.Val := NIL;
  VAR topLab: CG.Label;
  VAR argCt, argFlatEltNo: INTEGER;
  VAR eltFlatOffset, flatEltPack, constrEltPack: INTEGER;
  VAR depthWInArg, depthWInTopConstr, argOpenDepth: INTEGER;
  VAR argExpr: Expr.T;
  VAR argConstr: P;
  VAR eltAlign: Type.BitAlignT;
  VAR argRepTypeInfo: Type.Info;
  BEGIN
    IF constr.broken THEN 
(* CHECK: Do we need to do any CG stack manipulation here? *)
    RETURN END;
    top := constr.top;
    argCt := ArgCt (constr);
    <* ASSERT constr.state >= StateTyp.Represented *>
    <* ASSERT constr.depth < LAST (top.levels^) *>
    <* ASSERT top.levels^ [constr.depth].staticLen >= 0 *>
    constrEltPack := ArrayType.EltPack (constr.repType);
    eltFlatOffset := - 1;
    WITH argLevelInfo = top.levels^ [constr.depth + 1] DO
      (* Shape-check and assign the explicitly-coded arguments.*)
      FOR i := 0 TO MIN (argCt, constr.eltCt) - 1 DO
        WITH argExpr = constr.args^[i] DO

          IF argExpr # NIL THEN
            argFlatEltNo := selfFlatEltNo + i * argLevelInfo.staticFlatEltCt;
            argConstr := ArrayConstrExpr (argExpr);
            IF argConstr # NIL AND argConstr.isNested THEN
              (* argConstr is an inner array constructor. *)
              PrepRecurse (argConstr, argFlatEltNo);
            ELSE (* argExpr is a non-array or non-constructor. *)
              argRepType := Expr.RepTypeOf (argExpr);
              flatEltPack
                := ArrayType.EltPack (top.levels^[LAST(top.levels^) - 1].repType);
              eltFlatOffset := argFlatEltNo * flatEltPack;
              eltAlign
                := CG.GCD (top.topEltsAlign, eltFlatOffset MOD Target.Word.size);
              depthWInArg := 0;
              IF top.resultKind = RKTyp.RKGlobal THEN
(* TODO: Value checks. *)

                Expr.PrepLiteral
                  (argExpr, argLevelInfo.repType, top.inConstArea);
                Expr.GenLiteral
                  (argExpr, top.globalEltsOffset + eltFlatOffset,
                   argLevelInfo.repType, top.inConstArea);

              ELSIF argLevelInfo.staticLen = Expr.lengthNonArray
              THEN (* This arg is not an array. *)
                <* ASSERT top.shallowestDynDepth < 0 *>
                <* ASSERT argLevelInfo.staticFlatEltCt = 1 *>
                <* ASSERT top.firstArgDopeVal = NIL *>
(* TODO: Make do_direct work transitively through here. *)
                AssignStmt.PrepForEmit
                  (argLevelInfo.repType, argExpr,
                   initializing := top.resultKind IN RKTypSetInitializing);
(* CHECK: Do we need to do any Check[Load|Store]Traced? *)
                GenPushLHSEltsAddr (top, eltFlatOffset, eltAlign);
                AssignStmt.DoEmit (argLevelInfo.repType, argExpr, eltAlign);
(* CHECK: Does DoEmit take traced into account, as for the case below*)
(* TODO: We are skipping AssignStmt.Compile here, which my fail to do
           an Expr.NoteWrite. *)
              ELSIF top.shallowestDynDepth < 0 
              THEN (* Arg is a non-constructor array, possibly open. Top and
                      all descendent constructors are fully static. *)
                <* ASSERT top.firstArgDopeVal = NIL *>
(* TODO: Make do_direct work transitively through here. *)
                Expr.Prep (argExpr);
                EVAL Type.CheckInfo (argRepType, (*OUT*) argRepTypeInfo);
                Expr.CompileLValue
                  (argExpr, traced := argRepTypeInfo.isTraced);
(* CHECK^ Do we need optional Compile instead of CompileLValue? *)
                argAddrVal := CG.Pop ();
                argOpenDepth := OpenArrayType.OpenDepth (argRepType);
                IF argOpenDepth > 0
                THEN (* argAddrVal is address of arg's dope. *)
                  (* Go thru open dimensions of argExpr, generating needed RT
                     checks of any open & dynamic dimension against
                     this level's static length. *)
                  depthWInArg := 0;
                  LOOP
                    depthWInTopConstr
                      := constr.depth + 1 (*For arg*) + depthWInArg;
                    IF depthWInArg >= argOpenDepth THEN EXIT END;
                    WITH openLevelInfo = top.levels^ [depthWInTopConstr] DO
                      (* Gen RT check, arg's dynamic vs. top's static length. *)
                      CG.Push (argAddrVal);
                      CG.Boost_addr_alignment (Target.Integer.align);
                      CG.Open_size (depthWInArg);
                      <* ASSERT openLevelInfo.staticLen >= 0 *>
                      CG.Load_intt (openLevelInfo.staticLen);
                      CG.Check_eq
                        (Target.Integer.cg_type, CG.RuntimeError.IncompatibleArrayShape);
                    END (*WITH openLevelInfo*);
                    INC (depthWInArg); 
                  END (*LOOP*);
                END (*IF open element.*); 
                (* Copy in the arg value. *)
                GenPushLHSEltsAddr (top, eltFlatOffset, eltAlign);
                CG.Push (argAddrVal);
                <* ASSERT argLevelInfo.staticLen >= 0 *>
                CG.Copy
                  (argLevelInfo.staticLen * constrEltPack, overlap := FALSE);
              ELSE (* Arg is a non-constructor array.  All args are open and,
                      in some dimension, nonstatic, thus repType of this and
                      shallower levels is open array, and so is repType of
                      argExpr. *)
                <* ASSERT top.shallowestDynDepth >= constr.depth + 1 *>
                <* ASSERT OpenArrayType.Is (argLevelInfo.repType) *>
                <* ASSERT OpenArrayType.Is (Expr.RepTypeOf (argExpr)) *>
                <* ASSERT NOT constr.dots *>
                <* ASSERT top.resultKind # RKTyp.RKGlobal *>
                IF argExpr = top.firstArgExpr AND top.firstArgDopeVal # NIL
                THEN (* We previously compiled this arg's address. *)
                  CG.Push (top.firstArgDopeVal);
                  argAddrVal := CG.Pop ()
                ELSE (* Do so now. *)
                  EVAL Type.CheckInfo (argRepType, (*OUT*) argRepTypeInfo);
                  Expr.PrepLValue (argExpr, traced := argRepTypeInfo.isTraced);
                  Expr.CompileAddress
                    (argExpr, traced := argRepTypeInfo.isTraced);
                  argAddrVal := CG.Pop ();
                  (* This is a bit daring, but no need to RT store in
                     top.firstArgDopeVal, because we won't come through here
                     again with argExpr = top.firstArgExpr. *)
                END;
                (* argAddrVal is address of arg's dope. *)

                eltAlign := CG.GCD
                  (top.topEltsAlign,
                   OpenArrayType.EltPack
                     (top.levels^[top.deepestDynDepth].repType))
                   MOD Target.Word.size;

                (* Go thru array dimensions of argExpr, generating needed
                   RT checks. *)
                <* ASSERT top.fixingInfoComputed *>
                depthWInArg := 0; (* Array depth within the argument. *)
                LOOP (* Through levels of arg. *)
                  depthWInTopConstr
                    := constr.depth + 1 (*For arg*) + depthWInArg;
                  IF depthWInArg >= argOpenDepth THEN EXIT END;
                  WITH openLevelInfo = top.levels^ [depthWInTopConstr] DO
                    <* ASSERT openLevelInfo.staticLen # Expr.lengthNonArray *>
                    IF openLevelInfo.staticLen >= 0
                    THEN (* Arg is open but statically constrained at this level. *)
                      (* Check that RT length = static constraint. *)
                      CG.Push (argAddrVal);
                      CG.Boost_addr_alignment (Target.Integer.align);
                      CG.Open_size (depthWInArg);
                      CG.Load_intt (openLevelInfo.staticLen);
                      CG.Check_eq
                        (Target.Integer.cg_type,
                         CG.RuntimeError.IncompatibleArrayShape);
                      IF NOT top.useTargetVar
                      THEN top.useTargetVarAccessed := TRUE
                      END;
                    ELSIF argExpr # top.firstArgExpr
                          OR top.useTargetVar
                          OR top.resultKind = RKTyp.RKDirectDoped
                    THEN (* Gen RT check against openLevelInfo.fixingVal. *)
                      CG.Push (argAddrVal);
                      CG.Boost_addr_alignment (Target.Integer.align);
                      CG.Open_size (depthWInArg);
                      <* ASSERT openLevelInfo.fixingVal # NIL *>
                      CG.Push (openLevelInfo.fixingVal);
                      CG.Check_eq
                        (Target.Integer.cg_type,
                         CG.RuntimeError.IncompatibleArrayShape);
                    END;
                  END (*WITH openLevelInfo*);
                  INC (depthWInArg); 
                END (*LOOP*);

                (* Copy arg into LHS and increment LHS dynamic offset. *)
                GenPushLHSEltsAddr  (top, 0, eltAlign);
                IF top.dynOffsetVal = NIL
                THEN (* First time. Dynamic offset is zero. *)
                  GenCopyOpenArgValueDyn (constr, argAddrVal, eltAlign);
                  CG.Push (argLevelInfo.dynBitsizeVal);
                  top.dynOffsetVal := CG.Pop ();
                ELSE
(* TODO: Delay incrementing dynOffsetVal until here.  Would have to save the
         previous argLevel subscript and use it here to select the correct
         dynBitsizeVal. *)
                  CG.Push (top.dynOffsetVal);
                  CG.Index_bits (bits_addr_align := eltAlign);
                  GenCopyOpenArgValueDyn (constr, argAddrVal, eltAlign);
                  CG.Push (top.dynOffsetVal);
                  CG.Push (argLevelInfo.dynBitsizeVal);
                  CG.Index_bits (bits_addr_align := eltAlign);
                  top.dynOffsetVal := CG.Pop ();
                END;

              END (*IF [non]static.*);
            END (*IF*);
          END (*IF argExpr # NIL*)
        END (*WITH argExpr*);
      END (*FOR args*);

      IF constr.dots AND constr.eltCt > argCt AND eltFlatOffset >= 0
      THEN (* Fill in dots portion. *)
        <* ASSERT NOT OpenArrayType.Is (constr.semType ) *>
        CASE top.resultKind OF
        | RKTyp.RKGlobal =>
          argExpr := constr.args^ [argCt - 1];
          Expr.PrepLiteral
            (argExpr, argLevelInfo.repType, top.inConstArea);
          FOR argNo := argCt TO constr.eltCt - 1 DO
            INC (eltFlatOffset, constrEltPack);            
            Expr.GenLiteral
              (argExpr, top.globalEltsOffset + eltFlatOffset,
               argLevelInfo.repType,
               top.inConstArea);
          END (*FOR*)
        ELSE (* Generate a RT loop to fill in the '..' section *)
(* TODO: Is it worth it to unroll this loop for low iteration counts, or
           just let the back end do it? *)
          CG.Load_intt (argCt);
          dotSsVal := CG.Pop_temp ();
          topLab := CG.Next_label ();
          CG.Set_label (topLab);

          (* Gen code for ARRAY[dotSsVal] := ARRAY[argCt-1] *)
          GenPushLHSEltsAddr (top, bitOffset := 0, eltAlign := eltAlign);
          CG.Push (dotSsVal);
          ArrayType.GenIndex (constr.repType); (* Addr to store to. *)
          GenPushLHSEltsAddr (top, eltFlatOffset, eltAlign);
          (* ^Addr to fetch from -- where last explicit arg was assigned into
             constructor. *)
          IF ArrayType.EltsAreBitAddressed (constr.repType) THEN
            CG.Load_indirect
              (Target.Integer.cg_type, 0, constrEltPack, eltAlign);
(* CHECK: That this won't straddle word boundary. *)          
            CG.Store_indirect (Target.Integer.cg_type, 0, constrEltPack);
          ELSE
            CG.Copy (constrEltPack, overlap := FALSE);
          END;

          (* Gen RT code for dotSsVal := dotSsVal + 1 *)
          CG.Push (dotSsVal);
          CG.Load_intt (1);
          CG.Add (Target.Integer.cg_type);
          CG.Store_temp (dotSsVal);

          (* Gen RT code for IF (dotSsVal < NUMBER(ARRAY) GOTO topLab *)
          CG.Push (dotSsVal);
          CG.Load_intt (constr.eltCt);
          CG.If_compare (Target.Integer.cg_type, CG.Cmp.LT, topLab, CG.Likely);

          CG.Free (dotSsVal);
        END (*CASE*);
      END (*IF dots, etc.*);
    END (*WITH argLevelInfo*);
    CG.Free (argAddrVal);
  END PrepRecurse;

(* -------------------------- Compile --------------------- *)

PROCEDURE InitLiteralDope
  (top: P; inConstArea: BOOLEAN) =
(* PRE: top.repOpenDepth > 0 *)
(* Initialize dope fields of literal. *)
  VAR offset, eltCt: INTEGER; 
  BEGIN
    <* ASSERT top.depth = 0 *>
    <* ASSERT top.resultKind = RKTyp.RKGlobal *>
    <* ASSERT top.repOpenDepth > 0 *>
    CG.Init_var
      (top.globalDopeOffset + M3RT.OA_elt_ptr,
       Module.GlobalData (inConstArea), top.globalEltsOffset, inConstArea);

    offset := top.globalDopeOffset + M3RT.OA_size_0;
    FOR i := 0 TO top.repOpenDepth - 1 DO
      eltCt := top.levels^ [i].staticLen;
      CG.Init_intt (offset, Target.Integer.size, eltCt, inConstArea);
      INC (offset, Target.Integer.pack);
    END
  END InitLiteralDope;

(* Externally dispatched-to: *)
PROCEDURE Compile (top: P) =
(* PRE: top.depth = 0 *)
(* PRE: LHS addr is on CG stack IFF UsesAssignProtocol. *)
  BEGIN
    <* ASSERT top.depth = 0 *>
    Represent (top);
    IF top.broken THEN
      top.state := StateTyp.Compiled;
      RETURN
    END;
    Classify (top);
    
    (* Allocate static space if needed. *)
    IF top.resultKind = RKTyp.RKGlobal THEN
      top.globalDopeOffset 
        := Module.Allocate
             (top.totalSize, top.topRepAlign, top.inConstArea,
              tag := "ArrayExpr");
      IF top.repOpenDepth = 0 THEN (* Elements only. *)
        top.globalEltsOffset := top.globalDopeOffset;
      ELSE (* Dope and elements. *)
        top.globalEltsOffset := top.globalDopeOffset + top.dopeSize;
        InitLiteralDope (top, top.inConstArea);
      END;
    END;
    InnerPrep (top);
    InnerCompile (top);
  END Compile;

(* Externally dispatched-to: *)
PROCEDURE GenLiteral
  (top: P; globalOffset: INTEGER; <*UNUSED*>type: Type.T;
   inConstArea: BOOLEAN) =
(* PRE: top.depth = 0 *)
  BEGIN
    <* ASSERT top.depth = 0 *>
    <* ASSERT top.checked *>
    <* ASSERT Evaluate (top) # NIL *>
    <* ASSERT top.shallowestDynDepth < 0 *>
    <* ASSERT top.inConstArea = inConstArea *>

    Represent (top);
    IF top.broken THEN
      top.state := StateTyp.Compiled;
      RETURN
    END;
    Classify (top);
    <* ASSERT top.resultKind = RKTyp.RKGlobal *>
    top.globalDopeOffset := globalOffset;
    IF top.repOpenDepth <= 0 THEN
       (* No dope.  globalOffset is for an area for the elements. *)
      top.globalEltsOffset := globalOffset;
    ELSE (* globalOffset points to space for the dope only. *)
      top.globalEltsOffset 
        := Module.Allocate
             (top.staticEltsSize, top.topEltsAlign, inConstArea,
              tag := "StaticOpenArrayElements");
      InitLiteralDope (top, inConstArea);
    END;

    PrepRecurse (top, selfFlatEltNo := 0);
    InnerCompile (top);
    CG.Discard (Target.Address.cg_type);
  END GenLiteral;

PROCEDURE CompileGeneratedTypes (top: P) =
(* PRE: top.depth = 0 *)
(* POST: result address on TOS, even if called > once. *)
  BEGIN
    IF top.levels # NIL THEN
      FOR i := 0 TO LAST (top.levels ^) DO
        WITH levelInfo = top.levels ^ [i] DO
          Type.Compile (levelInfo.repType);
          Type.Compile (levelInfo.repIndexType);
        END
      END
    END;
    Type.Compile (top.refType);
   END CompileGeneratedTypes;

PROCEDURE InnerCompile (top: P) =
(* PRE: top.depth = 0 *)
  BEGIN
    IF top.broken THEN
      CG.Load_nil ();
      top.state := StateTyp.Compiled;
      RETURN
    END;
    CompileGeneratedTypes (top);
    
    CASE top.resultKind OF
    | RKTyp.RKUnknown => <* ASSERT FALSE *>

    | RKTyp.RKGlobal
    => Module.LoadGlobalAddr
         (Module.Current (), offset := top.globalDopeOffset,
          is_const := top.inConstArea);

    | RKTyp.RKDirectElts
    => CG.Push (top.buildEltsAddrVal);
      CG.Free (top.buildEltsAddrVal);

    | RKTyp.RKDirectDoped
    => CG.Push (top.buildAddrVal);
      CG.Free (top.buildAddrVal);

    | RKTyp.RKTempElts, RKTyp.RKTempStatic
    => (* We built into a static-sized temp Var. *) 
      <* ASSERT top.buildTempVar # NIL *>
      <* ASSERT (top.finalVal = NIL) = (NOT UsesAssignProtocol (top)) *>
      IF top.finalVal = NIL THEN
        CG.Load_addr_of_temp (top.buildTempVar, 0, top.topRepAlign)
      ELSE
        IF top.state < StateTyp.Compiled THEN (* Don't copy twice. *)
          CG.Push (top.finalVal);
          CG.Load_addr_of_temp (top.buildTempVar, 0, top.topRepAlign);
          CG.Copy (top.totalSize, overlap := FALSE);
        END;
        CG.Push (top.finalVal);
        CG.Free (top.finalVal);
      END;

    | RKTyp.RKTempDyn
    => (* We built into a heap-allocated, dynamic-sized, doped temp. *)
      <* ASSERT (top.finalVal = NIL) = (NOT UsesAssignProtocol (top)) *>
      IF top.finalVal = NIL THEN
        CG.Push (top.buildAddrVal)
      ELSE
        IF top.state < StateTyp.Compiled THEN (* Don't copy twice. *)
(* CHECK^ Actually, do we want to copy multiple times? *)        
          CG.Push (top.finalVal);
          CG.Push (top.buildAddrVal);
          CG.Load_intt (top.levels^[0].staticFlatEltCt);
          <* ASSERT top.shallowestDynDepth >= 0 *>
          CG.Push (top.levels^[top.shallowestDynDepth].dynBitsizeVal);
          CG.Multiply (Target.Integer.cg_type);
          CG.Copy_n (s := 1(*Unit size*), overlap := FALSE);
        END;
        CG.Push (top.finalVal);
        CG.Free (top.finalVal);
      END;
    END (*CASE*);
    
    (* Gen code to NIL out heap object pointer, to be collected. *)
    IF top.heapTempPtrVar # NIL THEN
      CG.Load_nil ();
      CG.Store_addr (top.heapTempPtrVar, (*Offset:=*) 0);
      CG.Free_temp (top.heapTempPtrVar);
    END;
    top.state := StateTyp.Compiled;
  END InnerCompile;

BEGIN
END ArrayExpr.
