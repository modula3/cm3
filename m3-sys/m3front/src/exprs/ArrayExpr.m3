(* -----------------------------------------------------------------------1- *)
(* File ArrayExpr.m3                                                         *)
(* Modula-3 source code.                                                     *)
(* Copyright 2020, 2021, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE ArrayExpr;
(* An array constructor. Built only during semantic procssing. *)

(* An array constructor can use one of these protocols for getting
   its value to where it belongs:
   constant protocol:   The value is built at compile time, directly
                        into one of the static areas.
   expression protocol: The value is built into a temporary,
                        independently of its ultimate destination,
                        and used or copied later.
   assign protocol:     The value is built directly into a runtime
                        variable, which must be available (on the
                        CG stack) during Prep.
*)

IMPORT Fmt;

IMPORT M3, M3ID, CG, Expr, ExprRep, Error, Type, ArrayType, PackedType;
IMPORT KeywordExpr, RangeExpr, NamedExpr, OpenArrayType, Module;
IMPORT IntegerExpr, EnumExpr, ConsExpr, SubrangeType, Target, TInt, Int, M3Buf;
IMPORT AssignStmt, RefType, M3RT, Procedure, RunTyme, ErrType;

(* Monitor sequencing of operations: *)
TYPE StateTyp
  = { Fresh, Checking, Checked, Representing, Represented,
      Prepping, Prepped, Compiling, Compiled};

(* Information collected/matched across all cousins at a given
   multidimensional array nesting depth. *)

TYPE LevelInfoTyp = RECORD
    semType           : Type.T := NIL;
    repType           : Type.T := NIL (* Packed stripped, top level only. *);
    repIndexType      : Type.T := NIL;
    fixingLenVal      : CG.Val := NIL;
                        (* If this is a dynamic level, its runtime length. *)
    eltBytepackVal    : CG.Val := NIL;
                        (* If this is a dynamic level, its runtime element
                           packing in bytes. *)
    staticLen         : Expr.lengthTyp := Expr.lengthNonArray;
    staticFlatEltCt   : INTEGER := 1;
                        (* Product of non-neg. staticLen values for this
                           and static inner dimensions, flattened down to
                           either a non-array or dynamic-sized open array. *)
    levEltPack        : INTEGER;
                        (* ^Irrelevant when NOT FixedOpenChecked *)
                        (* In a multi-dimension open array type, eltPack
                           is for the deepest open array, since lengths are
                           not static.  levEltPack is for this exact level,
                           and may be statically computable, even for an open
                           array, if its length is imposed by a constructor
                           argument count or matching fixed array type.
                           Static for deepestDynDepth and deeper, or all
                           levels, if shallowestDynDepth < 0.
                           -1 if not static. *) 
    FixedOpenChecked  : BOOLEAN := FALSE;
  END;

TYPE LevelsTyp = REF ARRAY OF LevelInfoTyp;

(* Different ways of delivering the result expression. *)
TYPE ResultKindTyp
  = { RKUnknown      (* Initial value. *)
    , RKGlobal       (* One of the global data areas. *)
                       (* Uses top.containingUnit, top.globalOffset,
                          top.globalEltsOffset, and top.inConstArea. *)
    , RKDirectElts   (* Caller-provided area, elements only. *)
                       (* Uses top.buildEltsAddrVal. *)
    , RKDirectDoped  (* Caller-provided area, doped. *)
                       (* Uses top.buildAddrVal and top.buildEltsAddrVal. *)
    , RKTempElts     (* Static-sized CG.Var temp, elements only. *)
                       (* Uses top.buildTempVar. *)
    , RKTempStatic   (* Static-sized CG.Var temp, with dope. *)
                       (* Uses top.buildTempVar. *)
    , RKTempDyn      (* Dynamically sized and allocated temp. *)
                       (* Uses top.heapTempPtrVar, top.buildAddrVal,
                          and top.buildEltsAddrVal. *)
    };

TYPE RKTyp = ResultKindTyp;
TYPE RKTypSet = SET OF RKTyp;
CONST RKTypSetInitializing = RKTypSet
  { RKTyp.RKGlobal, RKTyp.RKTempElts, RKTyp.RKTempStatic, RKTyp.RKTempDyn};
  (* ^For these result kinds, we are building into a previously uninitialized
     area whose preexisting contents will never be accessed. *)

(* Properties of an array constructor: *)
REVEAL 
  T = Expr.T BRANDED "ArrayExpr.T" OBJECT
    (* Field repType, inherited from Expr.T:
       The type used in RT representation of the constructor.  Equals targetType
       (or, for a nested constructor, an element type thereof),
       if top targetType # NIL.  Otherwise, inferred entirely from the
       constructor.  Could still be # semType, if semType is open but
       context fixes the length of some of the dimensions.
       It is computed in 'Represent'. Packed is stripped, top level only. *)
    top               : T;
    tipe              : Type.T;
(* CLEANUP ^ tipe always duplicates field "type", inherited from Expr.T *) 
    args              : Expr.List;
    semType           : Type.T; (* Original source code semantic type. *)
    semIndexType      : Type.T;
    semEltType        : Type.T;
    depth             : INTEGER; (* Depth this constructor is below the
                                    top-level array constructor. *)
    eltCt             : INTEGER := Expr.lengthInvalid;
    state             : StateTyp;
    isNested          : BOOLEAN; (* Inside another ArrayExpr/ConsExpr pair, with
                                    no NamedExpr intervening. *)
    dots              : BOOLEAN;
    evalAttempted     : BOOLEAN; (* TRUE even if Evaluate was called unsuccessfully. *) 
    is_const          : BOOLEAN; (* Meaningless if NOT evalAttempted.
                                    Otherwise, Evaluate was successful. *)
    broken            : BOOLEAN;
    
    (* Only used in top constructor: *)
    targetType        : Type.T; (* Requested by user of the constructor. *)
    repOpenDepth      : INTEGER;
    containingUnit    : Module.T; (* For RKGlobal, the unit containing this
                                     array constructor constant. *)
    globalOffset      : INTEGER := FIRST (INTEGER) (* Means uninitialized. *);
    globalEltsOffset  : INTEGER := FIRST (INTEGER) (* Means uninitialized. *);
    refType           : Type.T; (* If needed, type REF repType. *)
    finalVal          : CG.Val;
    buildAddrVal      : CG.Val;
    buildEltsAddrVal  : CG.Val;
    buildTempVar      : CG.Var;
    dynByteOffsetVal  : CG.Val := NIL; (* Top only. *)
    shallowestDynDepth: INTEGER := FIRST (INTEGER);
    deepestDynDepth   : INTEGER := FIRST (INTEGER);
    heapTempPtrVar    : CG.Var := NIL;
    staticEltsSize    : INTEGER; (* Excluding any dope.  Zero if not fully static. *)
    dopeSize          : INTEGER;
    totalSize         : INTEGER; (* Including any dope. *) 
    firstArgExpr      : Expr.T := NIL;
    firstArgDepth     : INTEGER := 0;
    firstArgDopeVal   : CG.Val;
    lastArgExpr       : Expr.T := NIL;
    levels            : LevelsTyp;
    RTErrorMsg        : TEXT := NIL;
    RTErrorCode       := CG.RuntimeError.Unknown;
    resultKind        := RKTyp.RKUnknown;
    topRepAlign       : Type.BitAlignT;
    topEltsAlign      : Type.BitAlignT; (* The entire block of elements. *)
    inConstArea       : BOOLEAN;
    fixingInfoComputed: BOOLEAN;
    useTargetVar      : BOOLEAN := FALSE;
    useTargetVarLocked: BOOLEAN := FALSE;
  OVERRIDES
    typeOf       := TypeOf;
    repTypeOf    := RepTypeOf;
    check        := Check;
    need_addr    := NeedsAddress;
    prep         := Prep;
    compile      := Compile;
    prepLV       := PrepLV;
    compileLV    := CompileLV;
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
    staticLength := StaticLength;
    usesAssignProtocol := UsesAssignProtocol;
    checkUseFailure := CheckUseFailure
  END (*OBJECT*);

(* EXPORTED: *) 
PROCEDURE New (type: Type.T; args: Expr.List; dots: BOOLEAN): Expr.T =
  VAR constr := NEW (T);
  BEGIN
    ExprRep.Init (constr);
    constr.resultKind   := RKTyp.RKUnknown;
    constr.depth        := FIRST (INTEGER);
    constr.type         := type;
    constr.tipe         := type;
    constr.semType      := type;
    constr.targetType   := NIL;
    constr.useTargetVar := FALSE;
    constr.useTargetVarLocked := FALSE;
    WITH b = ArrayType.Split (type, constr.semIndexType, constr.semEltType)
    DO <* ASSERT b *> (* Which also implies type is an array type. *) END; 
    constr.repType      := NIL;
    constr.args         := args;
(* REVIEW: Initializations here vs. declared in T. *)
    constr.dots                 := dots;
    constr.evalAttempted        := FALSE;
    constr.is_const             := FALSE;
    constr.broken               := FALSE;
    constr.checked              := FALSE;
    constr.fixingInfoComputed   := FALSE;
    constr.refType              := NIL;
    constr.dynByteOffsetVal     := NIL;
    constr.directAssignableType := TRUE; (* Inherited. *)
    constr.eltCt                := Expr.lengthInvalid;
    constr.state                := StateTyp.Fresh;
    constr.shallowestDynDepth   := FIRST (INTEGER); (* < 0 => none exists. *)
    constr.deepestDynDepth      := FIRST (INTEGER); (* < 0 => none exists. *)
    constr.heapTempPtrVar       := NIL;
    constr.RTErrorCode          := CG.RuntimeError.Unknown;
    constr.RTErrorMsg           := NIL;
    RETURN constr;
  END New;

(* EXPORTED: *) 
PROCEDURE ArrayConstrExpr (expr: Expr.T): T =
(* Look through a NamedExpr and then a ConsExpr, for an ArrayExpr.  NIL if not
   an array constuctor. *)

  VAR strippedExpr: Expr.T;
  BEGIN
    strippedExpr := Expr.StripNamedCons (expr);
    TYPECASE strippedExpr OF
    | NULL => RETURN NIL;
    | T (arrayExpr) => RETURN arrayExpr;
    ELSE RETURN NIL;
    END;
  END ArrayConstrExpr;

(* EXPORTED: *) 
PROCEDURE NoteNested (constr: T) =
(* PRE: constr has not been checked. *)
(* Mark constr as nested (ArrayExpr nested inside an ArrayExpr, directly,
   except for a possible ConsExpr in between.  In particular, must not
   be accessed by the outer ArrayExpr through a named constant. *)
  BEGIN
    IF constr = NIL THEN RETURN END;
    <* ASSERT constr.state < StateTyp.Checking *>
    constr.isNested := TRUE 
  END NoteNested;

(* EXPORTED: *) 
PROCEDURE Is (expr: Expr.T): BOOLEAN =
(* Purely syntactic. Will not look through a ConsExpr. *)
  BEGIN
    TYPECASE expr OF
    | NULL => RETURN FALSE;
    | T    => RETURN TRUE;
    ELSE      RETURN FALSE;
    END;
  END Is;

(* EXPORTED: *)
PROCEDURE IsAnon (expr: Expr.T): BOOLEAN =
(* expr is an anonymous array constructor. Will look thru' a ConsExpr. *)

  VAR strippedExpr: Expr.T;
  BEGIN
    IF NamedExpr.Is (expr) THEN RETURN FALSE END;
    IF ConsExpr.Is (expr) THEN
      ConsExpr.Seal (expr);
      strippedExpr := ConsExpr.Base (expr);
    ELSE strippedExpr := expr;
    END;
    RETURN Is (strippedExpr);
  END IsAnon;

PROCEDURE StaticSize (expr: Expr.T): INTEGER =
(* < 0, if nonstatic.  Can be static, even if open array repType.
   Does not include dope. *)
  VAR top: T;
  BEGIN
    top := ArrayConstrExpr (expr);
    IF top = NIL THEN RETURN Expr.lengthNonArray END;
    IF top.shallowestDynDepth >= 0 THEN (* Size is dynamic. *)
      RETURN Expr.lengthNonStatic
    END;
    RETURN top.staticEltsSize;
  END StaticSize;

(* Externally dispatched-to: *)
PROCEDURE NeedsAddress (<*UNUSED*> constr: T) =
  BEGIN
    (* We don't need to be told this. It always needs an address. *)
  END NeedsAddress;

(* Externally dispatched-to: *)
PROCEDURE IsEqual (a: T;  expr: Expr.T;  x: M3.EqAssumption): BOOLEAN =
(* Purely syntactic. *)
  BEGIN
    TYPECASE expr OF
    | NULL => RETURN FALSE;
    | T(b) => 
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

PROCEDURE ArgCt (constr: T): INTEGER =
  BEGIN
    IF constr.args = NIL THEN RETURN 0
    ELSE RETURN NUMBER (constr.args^);
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
  (constr: T; VAR levelInfo: LevelInfoTyp; argLength, depth: INTEGER) =
  BEGIN
    IF argLength < 0 THEN RETURN END;
    IF levelInfo.staticLen = Expr.lengthNonStatic 
    THEN (* First-found fixed-array argument.  All cousins at
            this depth must have this same length. *)
      levelInfo.staticLen := argLength;
    ELSIF argLength # levelInfo.staticLen
    THEN
      Error.Int (depth,
         "Array constructor element's static length differs "
         & "from previous static length (2.6.8), at depth:");
      constr.broken := TRUE;
    END;
  END CheckArgStaticLength;
  
(* Externally dispatched-to: *)
PROCEDURE Check (top: T;  VAR cs: Expr.CheckState) =
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
(* TODO^ Remove redundant type fields. *)

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
      lastLevelInfo.repType := levelType (* Preserve BITS..FOR.*);
      lastLevelInfo.semType := levelType;
    END;
    
    (* Recursively traverse and check the tree of nested array constructors.
       Also compute levelInfo.staticLen, top.firstArgExpr, top.firstArgDepth,
       top.lastArgExpr. *)
    CheckRecurse (top, top, cs, depth := 0);
    top.state := StateTyp.Checked;
    top.checked := TRUE;
  END Check;

PROCEDURE MergeRTError (top: T; Code: CG.RuntimeError; Msg: TEXT) =
  BEGIN
    IF Msg # NIL AND top.RTErrorMsg = NIL THEN
      top.RTErrorCode := Code;
      top.RTErrorMsg := Msg;
   (* Error.Warn
        (2, "Will raise runtime error if executed: " & Msg); *)
    END;
  END MergeRTError;

PROCEDURE CheckRecurse
  (top, constr: T; VAR cs: Expr.CheckState; depth: INTEGER) =
  VAR argLength, argDepth, argCt: INTEGER;
  VAR priorErrCt, priorWarnCt, laterErrCt, laterWarnCt: INTEGER;
  VAR depthWInArg, depthWInTopConstr: INTEGER;
  VAR argSemType, argRepType, argIndexType, argEltType: Type.T;
  VAR value, minE, maxE: Expr.T;
  VAR argConstr: T;
  VAR key: M3ID.T;
  VAR RTErrorCode: CG.RuntimeError;
  VAR RTErrorMsg: TEXT;
  VAR eltTypeInfo: Type.Info;
  BEGIN
    <* ASSERT constr.state = StateTyp.Fresh *>
    constr.state := StateTyp.Checking;
    <* ASSERT constr.isNested = (depth > 0) *>
    constr.depth := depth;
    constr.top := top;
    IF depth > 0 THEN
      constr.type := Type.Check (constr.type);
      constr.tipe := constr.type;
      constr.semType := constr.type;
    END;
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
    END;
    
    (* Check argument list length against constructor index type
       and compute constr.eltCt. *)
    argCt := ArgCt (constr);
    IF constr.semIndexType # NIL THEN (* Fixed array semType. *)
      constr.eltCt := LengthOfOrdType (constr.semIndexType);
      IF argCt > constr.eltCt THEN
        Error.Int
          (argCt, "Too many values specified in fixed array constructor (2.6.8).");
        FOR i := constr.eltCt TO argCt - 1 DO
          constr.args^[i] := NIL
        END;
      ELSIF argCt < constr.eltCt AND NOT constr.dots THEN
        Error.Msg ("Too few values specified in fixed array constructor (2.6.8).");
      END;
    ELSE (* Open array semType. *)
      constr.eltCt := argCt;
      IF (constr.dots) THEN
        Error.Msg ("\"..\" not allowed in open array constructor (2.6.8).");
        (* NOTE: ^The language seems to say this is illegal.  It was
                 previously a warning, so this change could invalidate
                 existing code. *)
        constr.dots := FALSE;
      END;
    END;
    WITH selfLevelInfo = top.levels^ [depth] DO
      IF selfLevelInfo.staticLen >= 0
      THEN 
        IF constr.eltCt # selfLevelInfo.staticLen
        THEN
          Error.Int
            (selfLevelInfo.staticLen,
             "Array constructor argument list length unequal to fixed "
             & " length of declared type (2.6.8).");
          constr.broken := TRUE;
        END;
      ELSE selfLevelInfo.staticLen := constr.eltCt;
      END;
    END (*WITH*);

    (* Go through the arguments. *)
    argDepth := depth + 1;
    WITH argLevelInfo = top.levels^ [argDepth] DO
      FOR i := 0 TO argCt - 1 DO
        WITH argExpr = constr.args^ [i] DO
          IF argExpr # NIL THEN

            (* Check for arg forms that are invalid in an array constructor: *)
            IF KeywordExpr.Split (argExpr, key, value) THEN
              Error.Msg ("Keyword values are not allowed in array constructors (2.6.8).");
              argExpr := NIL;
              constr.broken := TRUE;
            ELSIF RangeExpr.Split (argExpr, minE, maxE) THEN
              Error.Msg ("Range values are not allowed in array constructors (2.6.8).");
              argExpr := NIL;
              constr.broken := TRUE;
            ELSE (* Valid arg form. *)
              argConstr := ArrayConstrExpr (argExpr);
              IF argConstr # NIL AND argConstr.isNested THEN
                (* argConstr is an inner array constructor. *)
                CheckRecurse (top, argConstr, cs, argDepth);
                argLength := argConstr.eltCt;
                argSemType := argConstr.semType;
              ELSE (* argExpr is a non-array, or non-constructor, or named
                      constant, possibly with an array constructor as value. *)
                Expr.TypeCheck (argExpr, cs);
                IF top.firstArgExpr = NIL THEN
                  top.firstArgExpr := argExpr;
                  top.firstArgDepth := argDepth;
                END;
                top.lastArgExpr := argExpr;
                argSemType := Expr.SemTypeOf (argExpr);
              END (*TYPECASE*);

              (* Check type/type assignability of arg to this constructor. *)
              IF argSemType # NIL THEN
                IF NOT Type.IsAssignable (constr.semEltType, argSemType) THEN
                  Error.Int (i,
                     "Expression is not assignable to containing array"
                     & " constructor's element type (2.6.8).");
                  argExpr := NIL;
                  constr.broken := TRUE;
                (* And to top constructor, if different. *)
                END;
                IF constr # top 
                   AND NOT Type.IsAssignable (argLevelInfo.semType, argSemType)
                THEN
                  Error.Int (i,
                     "Expression is not assignable to top-level array"
                     & " constructor's element type (2.6.8).");
                  argExpr := NIL;
                  constr.broken := TRUE;
                END;
              END;

              IF NOT constr.broken THEN
                (* Check shape criterion of assignability. *)
                IF argConstr # NIL THEN
                  IF argConstr.isNested THEN
                    CheckArgStaticLength
                      (constr, argLevelInfo, argConstr.eltCt, constr.depth + 1);
                  ELSE
                    <* ASSERT Evaluate (argConstr) = argConstr *>
                    (* Check argConstr's static lengths against top's. *)
                    <* ASSERT argConstr.depth = 0 *>
                    depthWInArg := 0;
                    LOOP (* Thru array levels of arg. *)
                      depthWInTopConstr
                        := constr.depth + 1 (*For arg*) + depthWInArg;
                      IF depthWInTopConstr >= LAST (top.levels^) THEN EXIT END;
                      argLength := argConstr.levels^[depthWInArg].staticLen;
                      CheckArgStaticLength
                        (constr, top.levels^ [depthWInTopConstr], argLength,
                         depthWInTopConstr);
                      INC (depthWInArg);
                    END (*LOOP*);
                    AssignStmt.CheckStaticRTErrExec
                      (constr.semEltType, argConstr, cs,
                       (*VAR*)Code := RTErrorCode, (*VAR*)Msg := RTErrorMsg);
                    MergeRTError (top, RTErrorCode, RTErrorMsg);
                  END;
                ELSE (* Arg is not an array constructor. *)
                  Error.Count (priorErrCt, priorWarnCt);
                  NoteUseTargetVar (argExpr);
                  AssignStmt.CheckStaticRTErrExec
                    (constr.semEltType, argExpr, cs,
                     (*VAR*)Code := RTErrorCode, (*VAR*)Msg := RTErrorMsg);
                  MergeRTError (top, RTErrorCode, RTErrorMsg);
                  Error.Count (laterErrCt, laterWarnCt);
                  IF laterErrCt > priorErrCt THEN
                    argExpr := NIL
                  ELSE
                    (* Not array constructor => no need to call Represent. *)
                    argRepType := Expr.RepTypeOf (argExpr);
                    depthWInArg := 0;
                    LOOP (* Thru array levels of arg. *)
                      depthWInTopConstr
                        := constr.depth + 1 (*For arg*) + depthWInArg;
                      IF depthWInTopConstr >= LAST (top.levels^) THEN EXIT END;
                      IF NOT ArrayType.Split
                               (argRepType, (*VAR*) argIndexType, (*VAR*) argEltType)
                      THEN EXIT
                      END;
                      IF argIndexType # NIL THEN
                        argLength := LengthOfOrdType (argIndexType);
                        CheckArgStaticLength
                          (constr, top.levels^ [depthWInTopConstr], argLength,
                           depthWInTopConstr);
                      END;
                      argRepType := argEltType;
                      INC (depthWInArg);
                    END (*LOOP*)
                  END
                END
              END (*IF not broken*)
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
  VAR constr: T;
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
  VAR top: T;
  BEGIN
    top := ArrayConstrExpr (expr);
    IF top = NIL THEN RETURN FALSE END;
    <* ASSERT NOT top.isNested *>
    IF top.isNamedConst THEN RETURN FALSE END;
    <* ASSERT top.state >= StateTyp.Representing *>
      (* ^So targetType is final. *)
    IF top.targetType = NIL THEN RETURN TRUE END;
    IF OpenArrayType.OpenDepth (top.targetType) = 0 THEN RETURN FALSE END;
    RETURN NOT UseTargetVar (top)
  END ShapeCheckNeeded;

(* Externally dispatched-to: *)
PROCEDURE StaticLength (constr: T): INTEGER =
(* PRE: constr.checked. *)
  BEGIN
    <* ASSERT constr.checked *>
    RETURN constr.eltCt;
  END StaticLength;

(* Externally dispatched-to: *)
PROCEDURE IsZeroes (constr: T;  <*UNUSED*> lhs: BOOLEAN): BOOLEAN =
(* PRE: constr.checked. *)
  BEGIN
    IF constr.args = NIL THEN RETURN TRUE (*Vacuously*) END;
    FOR i := 0 TO LAST (constr.args^) DO
      IF NOT Expr.IsZeroes (constr.args^[i]) THEN RETURN FALSE END;
    END;
    RETURN TRUE;
  END IsZeroes;

(* Externally dispatched-to: *)
PROCEDURE Evaluate (constr: T): Expr.T =
(* PRE: constr.checked. *)
(* Return a constant expr if constr is constant, otherwise NIL. *)
(* NOTE: This will fold any constant argument in place, even if the
         whole constructor is not constant. *)
  VAR expr: Expr.T;
  BEGIN
    IF NOT constr.evalAttempted THEN
      constr.evalAttempted := TRUE;
      constr.is_const := TRUE;
      FOR i := 0 TO LAST (constr.args^) DO
        WITH arg = constr.args^[i] DO
          expr := Expr.ConstValue (arg);
          (* ^This will change arg in place to its constant value, if it has one. *)
          IF expr = NIL
          THEN constr.is_const := FALSE;
          ELSE arg := expr;
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
PROCEDURE GenFPLiteral (constr: T;  buf: M3Buf.T) =
(* PRE: constr.checked. *)
(* "FP" is for "fingerprint". *)
  VAR argCt: INTEGER;
  VAR expr: Expr.T;
  BEGIN
    <* ASSERT constr.checked *>
    argCt := ArgCt (constr);
    M3Buf.PutText (buf, "ARRAY<");
    FOR i := 0 TO argCt-1 DO
      IF (i > 0) THEN M3Buf.PutChar (buf, ',') END;
      Expr.GenFPLiteral (constr.args^[i], buf);
    END;
    IF constr.semIndexType # NIL AND constr.dots AND constr.eltCt > argCt THEN
      expr := constr.args^[argCt-1];
      expr := Expr.ConstValue (expr);
      FOR i := argCt TO constr.eltCt DO
(* FIXME^ reintroduce the change below by making this loop go to constr.eltCt-1. *)
      (* NOTE: Earlier, this loop incorrectly went to eltCt.  Fixing this
               could make earlier-written pickles unreadable by currently-
               compiled code. *)
        M3Buf.PutChar (buf, ',');
        Expr.GenFPLiteral (expr, buf);
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
  VAR top: T;
  VAR baseType: Type.T; 
  BEGIN
    top := ArrayConstrExpr (expr);
    IF top = NIL THEN RETURN END;
    <* ASSERT NOT top.isNested *>
    baseType := Type.StripPacked (type);
(* CHECK: What if it really is packed?  This can happen if the top-level
          constructor is a field. *)
    IF baseType = NIL THEN RETURN END;
    IF baseType = top.targetType THEN (* No change. *) RETURN END;
    IF top.targetType = NIL THEN (* First time noted. *)
      IF top.state >= StateTyp.Representing
      THEN (* Too late to use this info. *)
        Error.Info ("Target type supplied too late to use -- "
                    & "harmless except for possible efficiency loss.");
        RETURN
      ELSE
        top.targetType := baseType;
      END;
    ELSE
      Error.Info ("Target type changed -- "
                   & "harmless except for possible efficiency loss.");
      top.targetType := baseType;
    END;
  (* Represent (top); *)
  END NoteTargetType;
  
PROCEDURE IndexTypeForLength (length: INTEGER; origin: INTEGER): Type.T =
(* Type [0..length-1] *)
  VAR result: Type.T;
  VAR LastTI: Target.Int;
  BEGIN
    WITH b = TInt.FromInt (length-1, LastTI) DO <* ASSERT b *> END;
    result := SubrangeType.New (TInt.Zero, LastTI, Int.T, builtin := FALSE);
    (* Don't call Type.Check on 'result'.  Type.Check expects syntactic
       expressions for the bounds, and 'result' doesn't have them. *)
    result.origin := origin;
(* TODO: Cache these types, possibly inside SubrangeType. *)
    RETURN result; 
  END IndexTypeForLength; 

(* Special values for checking element packings: *)
CONST eltPackInconsistent = - 1;
CONST eltPackIrrelevant = - 2;

PROCEDURE CommonEltPack
  (top: T; superType, subType: Type.T; depth: INTEGER): INTEGER =
(* Common eltPack of both types, most interestingly when superType
   is open and subType is fixed.
   eltPackIrrelevant, if these are not array types.
   eltPackInconsistent, if element packings are unequal.
   otherwise, the common eltPack. *)
(* PRE: Both superType and subType are assignable to top.levels^[depth].repType *)
(* PRE: subType <: superType, not necessarily properly. *)

  VAR result: INTEGER;
  VAR superIndexType, superEltType: Type.T; 
  VAR subIndexType, subEltType : Type.T; 
  VAR subEltPack, superEltPack, deeperStaticLen, deeperEltPack: INTEGER;
  VAR subIsArray, superIsArray: BOOLEAN;

  BEGIN
    WITH levelInfo = top.levels^ [depth] DO
      IF levelInfo.FixedOpenChecked THEN RETURN levelInfo.levEltPack END;
      subIsArray
        := ArrayType.Split (* Skips naming and packing. *)
             (subType, (*VAR*)subIndexType, (*VAR*)subEltType);
      superIsArray
        := ArrayType.Split
             (superType, (*VAR*)superIndexType, (*VAR*)superEltType);
      <* ASSERT subIsArray = superIsArray *>
      IF NOT subIsArray THEN result := eltPackIrrelevant;
      ELSE
        IF superIndexType # NIL
        THEN (* Both superType and subType are fixed arrays. *)
          <* ASSERT subIndexType # NIL *>
          subEltPack := ArrayType.EltPack (subType);
          superEltPack := ArrayType.EltPack (superType);
          <* ASSERT subEltPack = superEltPack *>
          <* ASSERT subEltPack = levelInfo.levEltPack *>
          result := subEltPack; (* Both are the same, by assignability.*)
        ELSIF subIndexType = NIL
        THEN (* Both superType and subType are open arrays. *)
          (* No negative info about this level, but must check deeper. *)
          deeperEltPack
            := CommonEltPack (top, superEltType, subEltType, depth + 1);
          IF deeperEltPack = eltPackInconsistent
          THEN (* Inconsistent propagates to the top. *)
            result := eltPackInconsistent;
          ELSE
            result := deeperEltPack;
            (* ^Any shallower levels will both be open, and their lengths
                will be ensured equal by other mechanisms, static or dynamic,
                so no need to take the lengths into account. *)
          END
        ELSE (* superType is open, subType is fixed. *)
          (* The interesting case.  A fixed/open pair. *)
          deeperEltPack
            := CommonEltPack (top, superEltType, subEltType, depth + 1);
          IF deeperEltPack = eltPackInconsistent
          THEN (* Inconsistent propagates to the top. *)
            result := eltPackInconsistent;
          ELSIF deeperEltPack = eltPackIrrelevant
          THEN (* This is the deepest array level. *)
            subEltPack := ArrayType.EltPack (subType);
            superEltPack := OpenArrayType.EltPack (superType);
            <* ASSERT superEltPack = subEltPack *> (* By assignability. *)
            <* ASSERT levelInfo.levEltPack = subEltPack *>
            result := subEltPack;
          ELSE (* super has > 1 open levels, all matching fixed sub levels. *)
            subEltPack := ArrayType.EltPack (subType);
            deeperStaticLen := top.levels^ [depth+1].staticLen;
            IF deeperStaticLen < 0 THEN deeperStaticLen := 1 END;
            superEltPack := deeperStaticLen * deeperEltPack;
            IF superEltPack # subEltPack THEN (* An inconsistency. *)
              result := eltPackInconsistent;
            ELSE
              <* ASSERT levelInfo.levEltPack = subEltPack *>
              (* All fixed-typed cousins have deeperStaticLen.  So if
                 any of them matches the one open type here, they all will. *)
              result := subEltPack;
            END
          END
        END
      END;
      levelInfo.FixedOpenChecked := TRUE;
      RETURN result;
    END (*WITH*) 
  END CommonEltPack;

PROCEDURE CheckFixedOpenEltPack (top: T; typeX, typeY: Type.T; depth: INTEGER)
: BOOLEAN (* Not known to be bad. *) =
(* PRE: top is a top-level constructor. *)
(* PRE: typeX and typeY are both assignable to repType of levels at 'depth'.
        Thus they can differ from it and each other only in fixedness vs.
        openness in each corresponding dimension. *)
(* This is all to ensure that fixed and open arrays of same element type and
   that are copied-between have the same element packing.  This could maybe
   be inferred from type checking, but it is rather complicated to do so.
   So we verify it with this and its helper procedure CommonEltPack. *)
  VAR eltPack: INTEGER;
  BEGIN
    <* ASSERT top.depth = 0 *>
    IF NOT ArrayType.Is (typeX) THEN RETURN TRUE END;
    IF NOT ArrayType.Is (typeY) THEN RETURN TRUE END;
    IF typeX = NIL OR typeY = NIL THEN RETURN TRUE END;
    IF typeX = ErrType.T OR typeY = ErrType.T THEN RETURN TRUE END;
    IF Type.IsEqual (typeX, typeY, NIL) THEN RETURN TRUE END;
    IF OpenArrayType.Is (typeX) THEN
      eltPack := CommonEltPack (top, typeX, typeY, depth)
    ELSE
      eltPack := CommonEltPack (top, typeY, typeX, depth)
    END;
    RETURN eltPack # eltPackInconsistent
  END CheckFixedOpenEltPack;

PROCEDURE EltPackAndCount (top: T) =
  (* Compute eltPack and static element count product, inside to out. *)
  VAR eltPack, deeperStaticFlatEltCt, repOpenDepth: INTEGER;
  BEGIN
    repOpenDepth := OpenArrayType.OpenDepth (top.repType);
    deeperStaticFlatEltCt := 1;
    FOR depth := LAST(top.levels^) - 1 TO 0 BY - 1 DO
      WITH levelInfo = top.levels ^[depth] DO
        IF levelInfo.staticLen >= 0
        THEN 
          levelInfo.staticFlatEltCt
            := deeperStaticFlatEltCt * levelInfo.staticLen;
          deeperStaticFlatEltCt := levelInfo.staticFlatEltCt;
        ELSE (* Start the product over. *)
          deeperStaticFlatEltCt := 1;
        END;
        
        IF depth >= repOpenDepth THEN  (* Fixed array. *)
          levelInfo.levEltPack := ArrayType.EltPack (levelInfo.repType);
        ELSE (* Open array. *)
          IF depth = repOpenDepth - 1 THEN (* deepest open level. *)
            eltPack := OpenArrayType.EltPack (levelInfo.repType);
          END;
          levelInfo.levEltPack := eltPack;
          IF eltPack >= 0 AND levelInfo.staticLen >= 0 THEN
            eltPack := eltPack * levelInfo.staticLen
              (* ^ Will propagate one level shallower. *);
          ELSE eltPack := eltPackIrrelevant
               (* Will propagate all levels shallower. *);
          END;
        END;
      END (*WITH*);
    END (*FOR*);
  END EltPackAndCount;

PROCEDURE NewArrayType (indexType, eltType, oldType: Type.T): Type.T =
  VAR Result: Type.T;
  VAR info: Type.Info;
  BEGIN
    Result := ArrayType.New (indexType, eltType);
    IF PackedType.Is (oldType) THEN
      EVAL Type.CheckInfo (oldType, info);
      Result := PackedType.New (info.size, Result);
    END;
    RETURN Result;
  END NewArrayType;

PROCEDURE Represent (top: T) =
(* PRE: top is the top of a tree of array constructors. *)
(* Construct the representation for this constructor tree.
   This is postponed from Check until here to give clients needed opportunity
   to call NoteTargetType first. *)
  VAR levelType, levelIndexType, levelEltType, semIndexType: Type.T;
  VAR semEltType, repType, repIndexType, repEltType, repSuccType: Type.T;
  VAR locStaticLen, deeperStaticFlatEltCt, deepestArrayDepth, eltsSize: INTEGER;
  VAR eltPack: INTEGER;
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
    deepestArrayDepth := LAST(top.levels^) - 1;
    IF top.targetType # NIL AND top.targetType # top.semType THEN
(* CHECK: Callers will likely duplicate this assignability check.
          Can we eliminate the duplication? *)
      top.targetType := Type.Check (top.targetType);
      IF NOT Type.IsAssignable (top.targetType, top.semType) THEN
        Error.Msg
          ( "Array constructor is not assignable to expected type ( 2.6.8).");
        top.broken := TRUE;
        top.state := StateTyp.Represented;
        RETURN;
      END;

      (* Collect any static lengths from top.targetType into levels. *)
      (* Do it outside in. *)
      levelType := top.targetType;
      FOR depth := 0 TO deepestArrayDepth DO
        WITH levelInfo = top.levels^[depth] DO
          b := ArrayType.Split
                 (levelType, (*VAR*)levelIndexType, (*VAR*)levelEltType);
          <* ASSERT b *>
          locStaticLen := LengthOfOrdType (levelIndexType);
          IF locStaticLen >= 0 THEN
            IF levelInfo.staticLen = Expr.lengthNonStatic
            THEN (* semType is open in this dimension.  Make level static. *)
              levelInfo.staticLen := locStaticLen
            ELSIF locStaticLen # levelInfo.staticLen THEN
              (* This is a statically detected error of normally runtime kind. *)
              (* This is really not the best place to do this.  It would be
                 better to do it in Formal and AssignStmt.  But that entails
                 substantial, widespread rework.  Only because we were given
                 top.targetType, can we do it here. *)
              Error.Warn
                (depth,
                 "Shape mismatch will occur at runtime (2.3.1), at depth "
                 & Fmt.Int (depth));
              MergeRTError
                (top, CG.RuntimeError.IncompatibleArrayShape,
                 "Shape mismatch, depth " & Fmt.Int (depth));
              top.broken := TRUE;
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
      deeperStaticFlatEltCt := 1;
      eltPack := ArrayType.EltPack (top.levels ^[deepestArrayDepth].semType);
      FOR depth := deepestArrayDepth TO 0 BY - 1 DO
        WITH levelInfo = top.levels ^[depth] DO
          b := ArrayType.Split
                 (levelInfo.semType, (*VAR*)semIndexType, (*VAR*)semEltType);
          <* ASSERT b *>
          IF semIndexType = NIL (* SemType is open in this dimension, *)
             AND levelInfo.staticLen >= 0 
                 (* but it's statically constrained by context, *)
             AND NOT repSuccIsOpen 
               (* ^and not forced open by an inner open dimension. *)
          THEN (* Make it fixed in the repType. *)
            repIndexType
              := IndexTypeForLength (levelInfo.staticLen, top.origin);
            levelInfo.repIndexType := repIndexType;
            levelInfo.repType
               := NewArrayType (repIndexType, repSuccType, levelInfo.semType);
            levelInfo.repType := Type.Check (levelInfo.repType);
            repSuccHasChanged := TRUE;
          ELSE
            repIndexType := semIndexType;
            levelInfo.repIndexType := repIndexType;
            IF repSuccHasChanged
            THEN (* Same indexType, different eltType pointer. *)
              levelInfo.repType
                 := NewArrayType (repIndexType, repSuccType, levelInfo.semType);
              levelInfo.repType := Type.Check (levelInfo.repType);
            ELSE
              levelInfo.repType := levelInfo.semType;
            END;
          END;

          repSuccType := levelInfo.repType;
          IF repIndexType = NIL THEN repSuccIsOpen := TRUE END;
        END (*WITH*);
      END (*FOR*);
      top.repType := Type.StripPacked (repSuccType);
      top.levels ^[0].repType := top.repType;
      EltPackAndCount (top);
      IF top.shallowestDynDepth >= 0 THEN
        WITH levelInfo = top.levels ^[top.deepestDynDepth] DO
          IF OpenArrayType.EltPack (levelInfo.repType) MOD Target.Byte # 0 THEN
            Error.Int
              ( top . deepestDynDepth,
                "CM3 restriction: Elements of dynamic-length array "
                & "constructor must have byte-multiple size. Dimension:" );
            top.broken := TRUE;
          END;
        END (*WITH*);
      END;
    ELSE (* repType is targetType. Just copy targetType component pointers. *)
         (* Do it outside in. *)
      repType := top.targetType;
      FOR depth := 0 TO deepestArrayDepth DO
        WITH levelInfo = top.levels ^[depth] DO
          b := ArrayType.Split
                 (repType, (*VAR*)repIndexType, (*VAR*)repEltType);
          <* ASSERT b *>
          IF depth = 0 THEN
            top.repType := Type.StripPacked (repType);
            levelInfo.repType := top.repType;
          ELSE
            levelInfo.repType := repType;
          END;
          levelInfo.repIndexType := repIndexType;

          (* Develop shallowestDynDepth and deepestDynDepth. *)
          IF levelInfo.staticLen = Expr.lengthNonStatic THEN
            (* All cousins at 'depth' are open and nonstatic. *)
            top.deepestDynDepth := depth;
            IF top.shallowestDynDepth < 0 THEN
              top.shallowestDynDepth := depth;
            END
          END;
          repType := repEltType;              
        END (*WITH*);
      END (*FOR*);

      EltPackAndCount (top);
    END (*ELSE targetType # NIL*);

    (* Compute representation sizes. *)
    IF top.shallowestDynDepth >= 0
    THEN (* Nonstatic. No static allocation. *)
      top.staticEltsSize :=0;
    ELSE
      eltsSize := ArrayType.EltPack (top.levels^[deepestArrayDepth].repType);
      FOR depth := 0 TO deepestArrayDepth DO
        locStaticLen := top.levels^[depth].staticLen;
        <* ASSERT locStaticLen >= 0 *>
        eltsSize := eltsSize * locStaticLen;
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
      Error.Int
        (top.topEltsAlign,
         "CM3 restriction: array constructor cannot have sub-byte alignment.");
      top.topEltsAlign := Target.Word8.align;
    END;

    (* Make sure matching fixed and open arrays have the same eltPack. *)
    fixedOpenOK
      := CheckFixedOpenEltPack (top, top.semType, top.repType, depth := 0);
    <* ASSERT fixedOpenOK *>

    (* Apply repType to the tree of array constructors and Check.*)
    RepresentRecurse (top);
    
    (* NOTE: Trying to Check top.semType and top.repType now will segfault in
             Builder when elements are opaque,  due to Host.env being NIL. *)

    <* ASSERT top.repType # NIL *>
    top.state := StateTyp.Represented
  END Represent;

PROCEDURE RepresentRecurse (constr: T) =
  VAR top, argConstr: T;
  VAR argCt: INTEGER;
  VAR argRepType: Type.T;
  VAR argExpr: Expr.T;
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
            IF argConstr # NIL THEN
              IF argConstr.depth > 0 THEN
                (* argConstr is an inner array constructor. *)
                RepresentRecurse (argConstr);
              ELSE
                (* argConstr is a named-constant, top-level array constructor. *)
                Represent (argConstr) (* In case it's a forward reference. *);
              END;
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

      (* Copy repType from level to constructor. *)
      IF constr # top THEN
        constr.repType := top.levels^[constr.depth].repType;
      END;
    END;
    constr.state := StateTyp.Represented;
  END RepresentRecurse;

(* ---------------- Dependent on Represent ---------------- *)

PROCEDURE Classify (top: T) =
(* Classify the method/location for storing the constructor's value. *)
(* PRE: top.state >= StateTyp.Represented. *)
  BEGIN
    IF top.resultKind # RKTyp.RKUnknown THEN RETURN END;
    <* ASSERT top.depth = 0 *>
    <* ASSERT top.state >= StateTyp.Represented *>
    IF Evaluate (top) # NIL THEN (* It's a constant. *)
      top.resultKind := RKTyp.RKGlobal
    ELSIF UsesAssignProtocol (top)
    THEN (* Build value directly into final location. *)
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
  VAR constr: T;
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
PROCEDURE TypeOf (constr: T): Type.T =
(* PRE: constr is Checked. *)
  BEGIN
    <* ASSERT constr.state >= StateTyp.Checked *>
    <* ASSERT constr.depth = 0 *>
    <* ASSERT constr.type # NIL *>
    RETURN constr.type;
  END TypeOf;

(* Externally dispatched-to: *)
PROCEDURE RepTypeOf (constr: T): Type.T =
(* PRE: constr is Checked. *)
  BEGIN
    <* ASSERT constr.state >= StateTyp.Checked *>
    <* ASSERT constr.depth = 0 *>
    Represent (constr.top);
    IF constr.repType = NIL THEN
      <* ASSERT constr.broken *>
      RETURN constr.semType END;
    RETURN constr.repType;
  END RepTypeOf;

(* --- Prep --- *)

(* EXPORTED: *)
PROCEDURE NoteUseTargetVar (expr: Expr.T) =
(* NOOP if expr is not a (possibly named) array constructor.  Otherwise: *)
(* PRE: expr is top-level *)
(* Arrange to use LHS from the CG stack to set nonstatic shape components. *)
(* Will look through a ConsExpr. *)
  VAR top: T;
  BEGIN
    top := ArrayConstrExpr (expr);
    IF top = NIL THEN RETURN END;
    <* ASSERT NOT top.isNested *>
    IF top.state >= StateTyp.Prepped THEN  (* Called too late. *)
      Error.Info
        ("ArrayExpr.NoteUseTargetVar called too late"
         & " -- harmless but for possible efficiency loss." );
      RETURN
    END;
    <* ASSERT NOT top.useTargetVarLocked *>
    top.useTargetVar := TRUE;
  END NoteUseTargetVar;

PROCEDURE UseTargetVar (top: T): BOOLEAN =
(* SIDE EFFECTS !! *)
(* PRE: top is top-level. *)
  BEGIN
    <* ASSERT NOT top.isNested *>
    IF top.isNamedConst THEN RETURN FALSE END;
    top.useTargetVarLocked := TRUE;
    RETURN top.useTargetVar;
  END UseTargetVar;
  
(* Externally dispatched-to: *)
PROCEDURE UsesAssignProtocol (expr: Expr.T): BOOLEAN =
(* PRE: IF expr is an array constructor, THEN it is top-level. *)
(* Will look through a ConsExpr. *)
  VAR top: T;
  BEGIN
    top := ArrayConstrExpr (expr);
    IF top = NIL THEN RETURN FALSE END;
    <* ASSERT NOT top.isNested *>
    IF top.is_const THEN  (* The constant protocol. *)
      RETURN FALSE
    END;
    IF top.doDirectAssign THEN RETURN TRUE END;
    RETURN UseTargetVar (top)     
  END UsesAssignProtocol;

(* Externally dispatched-to: *)
PROCEDURE PrepLV (top: T; <*UNUSED*>traced: BOOLEAN) =
  BEGIN
    Prep (top);
  END PrepLV;

(* Externally dispatched-to: *)
PROCEDURE Prep (top: T) =
(* PRE: top.depth = 0. *)
  (* Must be called IFF top is the top of a tree of array constructors. *)
  BEGIN
    <* ASSERT top.depth = 0 *>
    IF top.broken THEN
      top.state := MAX (top.state , StateTyp.Prepped);
      RETURN
    END;
    IF Evaluate (top) # NIL (* Won't change if Prep is recalled. *)
    THEN (* It's a constant. *)
      top.inConstArea := TRUE
      (* Postpone InnerPrep until Compile, when we will have globalOffset
         and globalEltsOffset to build into. *)
    ELSIF NOT UsesAssignProtocol (top)
    THEN InnerPrep (top)
 (* ELSE postpone InnerPrep until Compile & InnerCompile, when LHS will
    have been pushed. *)
    END;
  END Prep;

(* Externally dispatched-to: *)
PROCEDURE PrepLiteral (constr: T; type: Type.T; inConstArea: BOOLEAN) =
(* If NOT inConstArea, set up to compile into the initialized global variable area. *)
  VAR top: T;
  BEGIN
    top := constr.top;
    <* ASSERT top.checked *>
    IF top.broken THEN
      top.state := MAX (top.state , StateTyp.Prepped);
      RETURN
    END;
    <* ASSERT Evaluate (top) # NIL *>
    top.targetType := type;
    top.inConstArea := inConstArea;
    Represent (top);
    (* Postpone InnerPrep until GenLiteral, when we will have
       globalOffset and globalEltsOffset to build into. *)
  END PrepLiteral;

PROCEDURE GenEvalFixingInfo
  (top: T; fixingDopeVal: CG.Val; fixingExprDepth: INTEGER) =
(* PRE: top is a top-level array constructor. *)
(* PRE: NOT top.broken. *)
(* Gen RT code to compute eltBytepackVal and fixingLenVal, for all
   fully dynamic levels . *)
  VAR depthWInFixingExpr, depthWInTopConstr: INTEGER;
  VAR eltPack, eltByteSize: INTEGER;
  VAR repType: Type.T;
  BEGIN
    IF top.shallowestDynDepth < 0 THEN (* No RT fixing info needed. *)
    RETURN END;
    IF top.fixingInfoComputed THEN (* We already did this. *) RETURN END;

    (* Go through the dynamic levels inside to out. *)
    depthWInTopConstr := top.deepestDynDepth;

    repType := top.levels^[depthWInTopConstr].repType;
    <* ASSERT OpenArrayType.Is (repType) *>
    eltPack := top.levels^ [top.deepestDynDepth].levEltPack;
    <* ASSERT eltPack MOD Target.Byte = 0 *>
    eltByteSize := eltPack DIV Target.Byte;
    IF eltByteSize > 1 THEN
      CG.Load_intt (eltByteSize);
    END;
    
    LOOP
      <* ASSERT top.shallowestDynDepth <= depthWInTopConstr *>
      depthWInFixingExpr := depthWInTopConstr - fixingExprDepth;
      WITH levelInfo = top.levels^[depthWInTopConstr] DO
        CG.Push (fixingDopeVal);
        CG.Open_size (depthWInFixingExpr) (* *length* *);
        levelInfo.fixingLenVal := CG.Pop ();
        CG.Push (levelInfo.fixingLenVal);
        IF depthWInTopConstr < top.deepestDynDepth OR eltByteSize > 1 THEN
          CG.Multiply (Target.Integer.cg_type);
        END; 
        levelInfo.eltBytepackVal := CG.Pop ();
        IF depthWInTopConstr <= top.shallowestDynDepth THEN
          EXIT
        ELSE 
          CG.Push (levelInfo.eltBytepackVal);
          DEC (depthWInTopConstr);
        END;
      END (*WITH*);
    END (*LOOP*);
    top.fixingInfoComputed := TRUE;
  END GenEvalFixingInfo;

PROCEDURE InitVarDope (top: T; var: CG.Var) =
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

PROCEDURE InitValDope (top: T; val: CG.Val; VAR eltsAddrVal: CG.Val) =
(* PRE: top.depth = 0. *)
  VAR length: INTEGER;
  BEGIN
    IF top.repOpenDepth <= 0 THEN
      CG.Push (val);
      eltsAddrVal := CG.Pop ();
    ELSE
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

PROCEDURE InnerPrep (top: T) =
(* PRE: top.depth = 0. *)
  (* Must be called IFF top is the top of a tree of array constructors. *)
  (* PRE LHS addr is on CG stack IFF UsesAssignProtocol. *)
  (* POST If UsesAssignProtocol, LHS has been popped and stored in some field. *)
  VAR
    shapeVar: CG.Var := NIL;
    shapeSize, length, offset: INTEGER;
    NewTracedArrayProc: Procedure.T;
    firstArgTypeInfo: Type.Info;
    name: M3ID.T;
  BEGIN
    IF top.state >= StateTyp.Prepped THEN RETURN END;
    Represent (top);
    IF top.broken THEN
      top.state := MAX (top.state, StateTyp.Prepped);
      RETURN
    END;
    Classify (top);
    top.state := StateTyp.Prepping;

    (* Set up result location info. *)
    CASE top.resultKind OF
    | RKTyp.RKUnknown => <* ASSERT FALSE *>
    | RKTyp.RKGlobal
      => <* ASSERT top.globalOffset >= 0 *> (* Already allocated. *)

    | RKTyp.RKDirectElts => (* LHS elements address is atop the CG stack. *)
      CG.Boost_addr_alignment (top.topRepAlign);
      top.buildEltsAddrVal := CG.Pop (); (* LHS points to elements. *)

    | RKTyp.RKDirectDoped => (* LHS dope address is atop the CG stack. *)
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

      IF UseTargetVar (top) THEN (* LHS address is on the CG stack. *)
        CG.Boost_addr_alignment (top.topRepAlign);
        top.finalVal := CG.Pop ();
      END;

    | RKTyp.RKTempDyn =>

      (* Gen code to evaluate shape-derived dynamic info: *)
      IF UseTargetVar (top) THEN (* LHS address is on the CG stack. *)
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
         dope, to be passed to RT library procedure NewTracedArray, and
         its elements are the shape of the n-dimensional array we are
         building. *)
      shapeSize := top.dopeSize + Target.Integer.size;
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

      (* Outer levels w/open representation, but a static value is imposed.
         There will be at least one, for the outermost constructor. *)
      FOR depth := 0 TO top.shallowestDynDepth - 1 DO
        length := top.levels^ [depth].staticLen;
        <* ASSERT length >= 0 *>
        CG.Load_intt (length);
        CG.Store (shapeVar, offset, Target.Integer.size, Target.Integer.align
                  , Target.Integer.cg_type);
        INC (offset, Target.Integer.size);
      END;

      (* Levels with dynamic, open length.  There will be at least one. *)
      FOR depth := top.shallowestDynDepth TO top.deepestDynDepth DO
        CG.Push (top.levels^ [depth].fixingLenVal);
        CG.Store (shapeVar, offset, Target.Integer.size, Target.Integer.align
                  , Target.Integer.cg_type);
        INC (offset, Target.Integer.size);
      END;

      (* Inner levels w/open representation, but a static value is imposed. *)
      FOR depth := top.deepestDynDepth + 1 TO top.repOpenDepth - 1 DO
        length := top.levels^ [depth].staticLen;
        <* ASSERT length >= 0 *>
        CG.Load_intt (length);
        CG.Store (shapeVar, offset, Target.Integer.size, Target.Integer.align
                  , Target.Integer.cg_type);
        INC (offset, Target.Integer.size);
      END;

      (* Gen code to heap-allocate space for the Constructor's temp. *)
(* TODO: Would it be possible & better to allocate this untraced? *)        
      (* We didn't know whether refType would be needed, at Check time. *)
      top.refType := RefType.New (top.repType, traced := TRUE, brand := NIL);
      top.refType := Type.Check (top.refType);
      Type.AddCell (top.refType);
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

      (* There must exist a variable of type top.refType, to get its typecell
         emitted.  Also, we gen code to store address of heap-allocated temp
         therein, so GC will leave it alone. *)
      name
        := M3ID.Add ("Heap temp ptr, line " & Fmt.Int (top.origin MOD 100000));
      top.heapTempPtrVar
        := CG.Declare_local
             (name, Target.Address.size, Target.Address.align, CG.Type.Addr,
              Type.GlobalUID (top.refType), in_memory := TRUE, up_level := FALSE,
              f := CG.Maybe);
      CG.Push (top.buildAddrVal);
      CG.Boost_addr_alignment (Target.Address.align);
      CG.Store_addr (top.heapTempPtrVar, (*Offset:=*) 0);

    END (*CASE top.resultKind*);

    (* If UsesAssignProtocol, LHS addr is now stored in
       top.buildAddrVar, top.buildEltsAddrVar, or top.finalVal,
       depending on top.resultKind. *)

    (* Traverse the tree of array constructors. *)
    PrepRecurse (top, top, selfFlatOffset := 0, depth := 0);

    (* Free temporaries: *)
    CG.Free_temp (shapeVar);
    IF top.shallowestDynDepth >= 0 THEN
      FOR depth := top.shallowestDynDepth TO top.deepestDynDepth DO
        CG.Free (top.levels^ [depth].fixingLenVal);
        CG.Free (top.levels^ [depth].eltBytepackVal);
      END;
    END;
    CG.Free (top.dynByteOffsetVal);
(* CHECK: Will Free_temps get done sometime? *)
    top.state := StateTyp.Prepped;
  END InnerPrep;

PROCEDURE GenCopyOpenArgValueDyn
  (top, constr: T; argDopeAddrVal: CG.Val; eltAlign: Type.BitAlignT) =
(* PRE: TOS is a CG.Val for address (w/in LHS) to copy to. *)
(* PRE: top.shallowestDynDepth >= 0 *)
  VAR staticFlatEltCt: INTEGER;
  BEGIN
    (* Target address: *)
    CG.ForceStacked ();
    (* Source address: *)
    CG.Push (argDopeAddrVal);
    CG.Boost_addr_alignment (Target.Address.align);
    CG.Open_elt_ptr (eltAlign);
    CG.ForceStacked ();
    (* Size in bytes: *)
    <* ASSERT top.shallowestDynDepth >= 0 *>
    <* ASSERT constr.depth < top.shallowestDynDepth *>
    CG.Push (top.levels^[top.shallowestDynDepth].eltBytepackVal);
    staticFlatEltCt
      := top.levels^[constr.depth + 1(*Of Arg.*)].staticFlatEltCt;
    IF staticFlatEltCt # 1 THEN
      CG.Load_intt (staticFlatEltCt);
      CG.Multiply (Target.Integer.cg_type);
    END;
(* TODO: call NoteWrite, as needed. *)

    CG.Copy_n (Target.Byte, overlap := FALSE);
  END GenCopyOpenArgValueDyn;

PROCEDURE GenPushLHSEltsAddr
  (top: T; bitOffset: INTEGER; eltAlign: Type.BitAlignT) =
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

PROCEDURE PushDynBytesizeVal (top: T; depth: INTEGER) =
  VAR eltPack, staticFlatEltCt: INTEGER;
  BEGIN
    <* ASSERT top.shallowestDynDepth >= 0 *>
    IF depth > top.deepestDynDepth THEN
      eltPack := top.levels^[depth].levEltPack;
      <* ASSERT eltPack >= 0 *>
      <* ASSERT eltPack MOD Target.Byte = 0 *>
      CG.Load_intt (eltPack DIV Target.Byte);
    ELSIF depth < top.shallowestDynDepth THEN
      CG.Push (top.levels^[top.shallowestDynDepth].eltBytepackVal);
      staticFlatEltCt := top.levels^[depth].staticFlatEltCt;
      IF staticFlatEltCt # 1 THEN
        CG.Load_intt (staticFlatEltCt);
        CG.Multiply (Target.Integer.cg_type);
      END
    ELSE
      CG.Push (top.levels^[depth].eltBytepackVal);
    END;
  END PushDynBytesizeVal;

PROCEDURE PrepRecurse
  (top, constr: T; selfFlatOffset: INTEGER; depth: INTEGER) =
(* Called only on an array constructor. *)
(* selfFlatOffset is the flattened offset from the beginning of top to the
   beginning of constr. *) 
  VAR argRepType: Type.T;
  VAR dotSsVal, argAddrVal: CG.Val := NIL;
  VAR topLab: CG.Label;
  VAR argCt: INTEGER;
  VAR argFlatOffset: INTEGER
      (* ^Meaningful only when there are no dynamic levels. *);
  VAR constrEltPack: INTEGER;
  VAR depthWInArg, depthWInTopConstr, argOpenDepth: INTEGER;
  VAR lArgExpr: Expr.T;
  VAR argConstr: T;
  VAR eltAlign: Type.BitAlignT;
  VAR argRepTypeInfo: Type.Info;
  BEGIN (* PrepRecurse *)
    IF constr.broken THEN RETURN END;
    argCt := ArgCt (constr);
    <* ASSERT constr.state >= StateTyp.Represented *>
    <* ASSERT depth < LAST (top.levels^) *>
    <* ASSERT top.levels^ [depth].staticLen >= 0 *>
    constrEltPack := top.levels^ [depth].levEltPack;
    WITH argLevelInfo = top.levels^ [depth + 1] DO
      (* Shape-check and assign the explicitly-coded arguments.*)
      FOR i := 0 TO MIN (argCt, constr.eltCt) - 1 DO
        WITH wArgExpr = constr.args^[i] DO
          argFlatOffset := selfFlatOffset + i * constrEltPack;
          IF wArgExpr # NIL THEN
            argConstr := ArrayConstrExpr (wArgExpr);
            IF top.resultKind = RKTyp.RKGlobal
               AND argConstr # NIL
               AND argConstr.isNamedConst
            THEN

            (* It's a named, constant array constructor. *)

              (* Retraverse it as if it were inline. *)
              <* ASSERT argConstr.top = argConstr *>
              <* ASSERT Evaluate (argConstr) # NIL *>
              PrepLiteral
                (argConstr, argLevelInfo.repType, inConstArea := TRUE);
              PrepRecurse (top, argConstr, argFlatOffset, depth + 1);
            ELSIF argConstr # NIL AND NOT argConstr.isNamedConst THEN

            (* It's an inline array constructor. *)

              PrepRecurse (top, argConstr, argFlatOffset, depth + 1);
            ELSE

            (* Not a constructor.  Handle wArgExpr here. *)

              argRepType := Expr.RepTypeOf (wArgExpr);
              eltAlign
                := CG.GCD (top.topEltsAlign, argFlatOffset MOD Target.Word.size);
              depthWInArg := 0;
              IF top.resultKind = RKTyp.RKGlobal THEN

              (* It's a literal, => no dynamic levels. *)

                <* ASSERT top.shallowestDynDepth < 0 *>
                Expr.PrepLiteral
                  (wArgExpr, argLevelInfo.repType, top.inConstArea);
                Expr.GenLiteral
                  (wArgExpr, top.globalEltsOffset + argFlatOffset,
                   argLevelInfo.repType, top.inConstArea);

              ELSIF argLevelInfo.staticLen = Expr.lengthNonArray THEN

              (* This arg is not an array. *)

                <* ASSERT top.shallowestDynDepth < 0 *>
                <* ASSERT argLevelInfo.staticFlatEltCt = 1 *>
                <* ASSERT top.firstArgDopeVal = NIL *>
(* TODO: Make do_direct work transitively through here. *)
                AssignStmt.PrepForEmit
                  (argLevelInfo.repType, wArgExpr,
                   initializing := top.resultKind IN RKTypSetInitializing);
(* CHECK: Do we need to do any Check[Load|Store]Traced? *)
                GenPushLHSEltsAddr (top, argFlatOffset, eltAlign);
                AssignStmt.DoEmit
                  (argLevelInfo.repType, wArgExpr, eltAlign, initializing := TRUE);
(* CHECK: Does DoEmit take traced into account, as for the case below*)
(* TODO: We are skipping AssignStmt.Compile here, which my fail to do
           an Expr.NoteWrite. *)
              ELSIF top.shallowestDynDepth < 0 THEN

              (* Arg is a non-constructor array, possibly open, but made
                 static by context from constructor.
                 Top and all descendent constructors are fully static. *)

                <* ASSERT top.firstArgDopeVal = NIL *>
(* TODO: Make do_direct work transitively through here. *)
                Expr.Prep (wArgExpr);
                EVAL Type.CheckInfo (argRepType, (*OUT*) argRepTypeInfo);
                Expr.Compile (wArgExpr);
                (* It's an array, so Prep/Compile will push an address
                   regardless of whether it's a designator. *)
                argAddrVal := CG.Pop ();
                argOpenDepth := OpenArrayType.OpenDepth (argRepType);
                IF argOpenDepth > 0
                THEN (* argAddrVal is address of arg's dope. *)
                  (* Go thru open dimensions of wArgExpr, generating needed RT
                     checks of any open & dynamic dimension against
                     this level's static length. *)
                  depthWInArg := 0;
                  LOOP
                    IF depthWInArg >= argOpenDepth THEN EXIT END;
                    depthWInTopConstr
                      := depth + 1 (*For arg*) + depthWInArg;
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
                GenPushLHSEltsAddr (top, argFlatOffset, eltAlign);
                CG.Push (argAddrVal);
                <* ASSERT argLevelInfo.staticLen >= 0 *>
                CG.Copy (constrEltPack, overlap := FALSE);
              ELSE

              (* The dynamic case.  top.shallowestDynDepth >= 0.
                 Arg is a non-constructor array.  Also, all cousin args
                 in the entire topmost constructor are open at this level
                 and, at some depth, nonstatic, thus repType of argLevel
                 and shallower levels is open array, and so is repType of
                 wArgExpr. *)

                <* ASSERT depth < top.shallowestDynDepth *>
                <* ASSERT OpenArrayType.Is (argLevelInfo.repType) *>
                <* ASSERT OpenArrayType.Is (Expr.RepTypeOf (wArgExpr)) *>
                <* ASSERT NOT constr.dots *>
                <* ASSERT top.resultKind # RKTyp.RKGlobal *>
                IF wArgExpr = top.firstArgExpr AND top.firstArgDopeVal # NIL
                THEN (* We previously compiled this arg's (dope) address. *)
                  CG.Push (top.firstArgDopeVal);
                  argAddrVal := CG.Pop ()
                ELSE (* Do so now. *)
                  EVAL Type.CheckInfo (argRepType, (*OUT*) argRepTypeInfo);
                  Expr.Prep (wArgExpr);
                  Expr.Compile (wArgExpr);
                  argAddrVal := CG.Pop ();
                  (* This is a bit daring, but no need to RT store in
                     top.firstArgDopeVal, because we won't come through here
                     again with wArgExpr = top.firstArgExpr. *)
                END;
                (* argAddrVal is address of arg's dope. *)

                eltAlign := CG.GCD
                  (top.topEltsAlign,
                   OpenArrayType.EltPack
                     (top.levels^[top.deepestDynDepth].repType))
                   MOD Target.Word.size;

                (* Go thru array dimensions of wArgExpr, generating needed
                   RT checks. *)
                <* ASSERT top.fixingInfoComputed *>
                depthWInArg := 0; (* Array depth within the argument. *)
                argOpenDepth := OpenArrayType.OpenDepth (argRepType);
                LOOP (* Through open levels of arg. *)
                  IF depthWInArg >= argOpenDepth THEN EXIT END;
                  depthWInTopConstr
                    := depth + 1 (*For arg*) + depthWInArg;
                  WITH openLevelInfo = top.levels^ [depthWInTopConstr] DO
                    <* ASSERT openLevelInfo.staticLen # Expr.lengthNonArray *>
                    IF openLevelInfo.staticLen >= 0
                    THEN (* Arg is open but statically constrained at this level. *)
                      (* Check that RT Arg length = static constraint. *)
                      CG.Push (argAddrVal);
                      CG.Boost_addr_alignment (Target.Integer.align);
                      CG.Open_size (depthWInArg);
                      CG.Load_intt (openLevelInfo.staticLen);
                      CG.Check_eq
                        (Target.Integer.cg_type,
                         CG.RuntimeError.IncompatibleArrayShape);
                    ELSE
                      top.useTargetVarLocked := TRUE;
                      IF UseTargetVar (top)
                         OR top.resultKind = RKTyp.RKDirectDoped
                         OR wArgExpr # top.firstArgExpr
                      THEN (* Arg is open and dynamically constrained at this level. *)
                        (* Gen RT check against openLevelInfo.fixingLenVal. *)
                        CG.Push (argAddrVal);
                        CG.Boost_addr_alignment (Target.Integer.align);
                        CG.Open_size (depthWInArg);
                        <* ASSERT openLevelInfo.fixingLenVal # NIL *>
                        CG.Push (openLevelInfo.fixingLenVal);
                        CG.Check_eq
                          (Target.Integer.cg_type,
                           CG.RuntimeError.IncompatibleArrayShape);
                      END;
                    END;
                  END (*WITH openLevelInfo*);
                  INC (depthWInArg); 
                END (*LOOP thru' open levels of arg. *);

                (* Copy arg into LHS and increment LHS dynamic offset. *)
                GenPushLHSEltsAddr  (top, bitOffset := 0, eltAlign := eltAlign);
                IF top.dynByteOffsetVal = NIL
                THEN (* First time. Dynamic offset is zero. *)
                  GenCopyOpenArgValueDyn (top, constr, argAddrVal, eltAlign);
                  PushDynBytesizeVal (top, depth + 1);
                  top.dynByteOffsetVal := CG.Pop ();
                ELSE
                  CG.ForceStacked ();
                  CG.Push (top.dynByteOffsetVal);
                  CG.Index_bytes (Target.Byte);
                  GenCopyOpenArgValueDyn (top, constr, argAddrVal, eltAlign);
                  IF wArgExpr # top.lastArgExpr THEN
                    CG.Push (top.dynByteOffsetVal);
                    PushDynBytesizeVal (top, depth + 1);
                    CG.Add (Target.Integer.cg_type);
                    top.dynByteOffsetVal := CG.Pop ();
                  END;
                END;

              END (*IF [non]static.*);
            END (*IF argConstr # NIL*);
          END (*IF wArgExpr # NIL..ELSE*)
        END (*WITH wArgExpr*);
      END (*FOR args*);

      IF constr.dots AND 0 < argCt AND argCt < constr.eltCt THEN 
	lArgExpr := constr.args^[argCt - 1];
	IF lArgExpr # NIL THEN

	  (* Fill in dots portion. *)
	  <* ASSERT
	       top.shallowestDynDepth < 0 OR depth + 1 = top.shallowestDynDepth *>
	  <* ASSERT NOT OpenArrayType.Is (constr.semType ) *>
	  <* ASSERT argFlatOffset >= 0 *>
          argConstr := ArrayConstrExpr (lArgExpr);
	  CASE top.resultKind OF

	  | RKTyp.RKGlobal =>
	    IF argConstr # NIL THEN
	      FOR argNo := argCt TO constr.eltCt - 1 DO
		INC (argFlatOffset, constrEltPack);
		PrepRecurse (top, argConstr, argFlatOffset, depth + 1);
	      END (*FOR*)
	    ELSE (* Not an array constructor. *)
	      Expr.PrepLiteral
		(lArgExpr, argLevelInfo.repType, top.inConstArea);
	      FOR argNo := argCt TO constr.eltCt - 1 DO
		INC (argFlatOffset, constrEltPack);
		Expr.GenLiteral
		  (lArgExpr, top.globalEltsOffset + argFlatOffset,
		   argLevelInfo.repType,
		   top.inConstArea);
	      END (*FOR*)
	    END (*IF*)
	  ELSE (* Generate a RT loop to fill in the '..' section *)
  (* TODO: Is it worth it to unroll this loop for low iteration counts, or
	     just let the back end do it? *)
	    CG.Load_intt (argCt);
	    dotSsVal := CG.Pop_temp ();
	    topLab := CG.Next_label ();
	    CG.Set_label (topLab);

	    (* Gen code for ARRAY[dotSsVal] := ARRAY[argCt-1] *)
	    WITH constrLevelInfo = top.levels^[depth] DO
	      GenPushLHSEltsAddr (top, bitOffset := 0, eltAlign := eltAlign);
	      CG.Push (dotSsVal);
	      ArrayType.GenIndex (constrLevelInfo.repType); (* Addr to store to. *)
	      GenPushLHSEltsAddr (top, argFlatOffset, eltAlign);
	      (* ^Addr to fetch from -- where last explicit arg was assigned into
		 constructor. *)
	      IF ArrayType.EltsAreBitAddressed (constrLevelInfo.repType) THEN
		CG.Load_indirect
		  (Target.Integer.cg_type, 0, constrEltPack, eltAlign);
  (* CHECK: That this won't straddle word boundary. *)
		CG.Store_indirect (Target.Integer.cg_type, 0, constrEltPack);
	      ELSE
		CG.Copy (constrEltPack, overlap := FALSE);
	      END;
	    END (* WITH constrLevelInfo*);

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
	END (*IF lArgExpr # NIL*)
      END (*IF dots, etc.*);
    END (*WITH argLevelInfo*);
    CG.Free (argAddrVal);
  END PrepRecurse;

(* -------------------------- Compile --------------------- *)

PROCEDURE InitLiteralDope
  (top: T; inConstArea: BOOLEAN) =
(* PRE: top.repOpenDepth > 0 *)
(* Initialize dope fields of literal. *)
  VAR shapeOffset, eltCt: INTEGER; 
  BEGIN
    <* ASSERT top.depth = 0 *>
    <* ASSERT top.resultKind = RKTyp.RKGlobal *>
    <* ASSERT top.repOpenDepth > 0 *>
    <* ASSERT top.containingUnit = Module.Current () *>
    CG.Init_var
      (top.globalOffset + M3RT.OA_elt_ptr,
       Module.GlobalData (inConstArea), top.globalEltsOffset, inConstArea);

    shapeOffset := top.globalOffset + M3RT.OA_size_0;
    FOR i := 0 TO top.repOpenDepth - 1 DO
      eltCt := top.levels^ [i].staticLen;
      CG.Init_intt (shapeOffset, Target.Integer.size, eltCt, inConstArea);
      INC (shapeOffset, Target.Integer.pack);
    END
  END InitLiteralDope;

(* Externally dispatched-to: *)
PROCEDURE CompileLV
  (top: T; <*UNUSED*>traced: BOOLEAN; StaticOnly: BOOLEAN) =
  BEGIN
    Compile (top, StaticOnly);
  END CompileLV;

(* Externally dispatched-to: *)
PROCEDURE Compile (top: T; <*UNUSED*> StaticOnly: BOOLEAN) =
(* PRE: top.depth = 0 *)
(* PRE: LHS addr is on CG stack IFF UsesAssignProtocol. *)
  BEGIN
    <* ASSERT top.depth = 0 *>
    Represent (top);
    IF top.broken THEN
      CG.Load_nil ();
      top.state := StateTyp.Compiled;
      RETURN
    END;
    Classify (top);
    
    (* Allocate static space if needed. *)
    IF top.resultKind = RKTyp.RKGlobal AND top.globalOffset < 0 THEN
      top.containingUnit := Module.Current ();
      top.globalOffset 
        := Module.Allocate
             (top.totalSize, top.topRepAlign, top.inConstArea,
              tag := "ArrayExpr");
      IF top.repOpenDepth = 0 THEN (* No dope, elements only. *)
        top.globalEltsOffset := top.globalOffset;
      ELSE (* Dope and elements. *)
        top.globalEltsOffset := top.globalOffset + top.dopeSize;
        InitLiteralDope (top, top.inConstArea);
      END;
    END;
    InnerPrep (top);
    InnerCompile (top, StaticOnly);
    EVAL CheckUseFailure (top);
  END Compile;

(* Externally dispatched-to: *)
PROCEDURE GenLiteral
  (top: T; globalOffset: INTEGER; <*UNUSED*>type: Type.T;
   inConstArea: BOOLEAN) =
(* PRE: top.depth = 0 *)
  VAR allocSize : INTEGER;
  BEGIN
    <* ASSERT top.depth = 0 *>
    <* ASSERT top.checked *>
    <* ASSERT Evaluate (top) # NIL *>
    <* ASSERT top.is_const *> 
    (* NOTE: top.shallowestDynDepth could be >= 0, but since top.is_const,
             this can happen only if the dynamic level(s) have no elements,
             due to empty constructor(s) somewhere, i.e. '<type>{}'.
             So we don't care here. *)
    <* ASSERT top.inConstArea = inConstArea *>

    Represent (top);
    IF top.broken THEN
      top.state := StateTyp.Compiled;
      RETURN;
    END;
    Classify (top);
    <* ASSERT top.resultKind = RKTyp.RKGlobal *>
    CompileGeneratedTypes (top);
    IF TRUE OR top.globalOffset < 0 (* Not yet computed. *)
    THEN
      top.containingUnit := Module.Current ();
      top.globalOffset := globalOffset;
      IF top.repOpenDepth <= 0
      THEN (* No dope.  globalOffset leads to space in a static area that our
              caller has allocated for the array elements. *)
        top.globalEltsOffset := globalOffset;
      ELSE (* globalOffset leads to space in a static area that our
              caller has allocated for the dope only.
              Allocate space for the elements now. *)
        <* ASSERT top.staticEltsSize >= 0 *>
        IF top.staticEltsSize = 0 THEN
           (* Just for safety, for a constant (thus static) empty array, let's
              allocate storage for one innermost dimension's element, thus an
              address that is unique from any other constant. *)
          allocSize
            := ArrayType.EltPack (top.levels^[LAST(top.levels^) - 1].repType);
        ELSE
          allocSize := top.staticEltsSize
        END;
        top.globalEltsOffset
          := Module.Allocate
               (allocSize, top.topEltsAlign, inConstArea,
                tag := "StaticOpenArrayElements");
        InitLiteralDope (top, inConstArea);
      END;

      top.state := StateTyp.Prepping;
      PrepRecurse (top, top, selfFlatOffset := 0, depth := 0);
      top.state := StateTyp.Prepped;
    END;
  END GenLiteral;

PROCEDURE CompileGeneratedTypes (top: T) =
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
    top.semType := Type.Check (top.semType);
    Type.Compile (top.semType);
    top.repType := Type.Check (top.repType);
    Type.Compile (top.repType);
    Type.Compile (top.refType);
   END CompileGeneratedTypes;

PROCEDURE InnerCompile (top: T; StaticOnly: BOOLEAN) =
(* PRE: top.depth = 0 *)
  BEGIN
    IF top.broken THEN
      IF NOT StaticOnly THEN
        CG.Load_nil ();
      END;
      top.state := StateTyp.Compiled;
      RETURN
    END;
    CompileGeneratedTypes (top);
    
    CASE top.resultKind OF
    | RKTyp.RKUnknown => <* ASSERT FALSE *>

    | RKTyp.RKGlobal
    => IF NOT StaticOnly THEN
          Module.LoadGlobalAddr
            (top.containingUnit, offset := top.globalOffset,
             is_const := top.inConstArea)
       END;

    | RKTyp.RKDirectElts
    => <* ASSERT NOT StaticOnly *>
      CG.Push (top.buildEltsAddrVal);
      CG.Free (top.buildEltsAddrVal);

    | RKTyp.RKDirectDoped
    => <* ASSERT NOT StaticOnly *>
      CG.Push (top.buildAddrVal);
      CG.Free (top.buildAddrVal);

    | RKTyp.RKTempElts, RKTyp.RKTempStatic
    => (* We built into a static-sized temp Var. *) 
      <* ASSERT NOT StaticOnly *>
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
    => (* We built into a heap-allocated, dynamic-sized, contiguous
          dope-and-elements temp. *)
      <* ASSERT NOT StaticOnly *>
      <* ASSERT (top.finalVal = NIL) = (NOT UsesAssignProtocol (top)) *>
      IF top.finalVal = NIL THEN
        CG.Push (top.buildAddrVal) (* Return the temp. *)
(* CHECK: Leave top.heapTempPtrVar alone.  If used in a WITH-binding, WithStmt
          will copy only the dope into a variable.  This will maintain only
          a pointer to the elements portion of the temporary.  Is a pointer
          to the interior of a heap object enough to protect it from GC? *)
      ELSE (* Copy the temp into the provided LHS area. *)
        IF top.state < StateTyp.Compiled THEN (* Don't copy twice. *)
(* CHECK^ Actually, do we want to copy multiple times? *)        
          CG.Push (top.finalVal);
          CG.Push (top.buildAddrVal);
          CG.Load_intt (top.levels^[0].staticFlatEltCt);
(* TODO ^Eliminate multiply by one. *)
          <* ASSERT top.shallowestDynDepth >= 0 *>
          CG.Push (top.levels^[top.shallowestDynDepth].eltBytepackVal);
          CG.Multiply (Target.Integer.cg_type);
          CG.Load_intt (top.dopeSize);
          CG.Add (Target.Integer.cg_type);
          CG.Copy_n (s := 1(*Unit size*), overlap := FALSE);
        END;
        CG.Push (top.finalVal);
        CG.Free (top.finalVal);

        (* Now we can NIL out the heap object pointer, to be collected. *)
        IF top.heapTempPtrVar # NIL THEN
          CG.Load_nil ();
          CG.Store_addr (top.heapTempPtrVar, (*Offset:=*) 0);
        END;
      END;
    END (*CASE*);
    
    top.state := StateTyp.Compiled;
  END InnerCompile;

(*EXPORTED:*)
PROCEDURE CheckStaticRTErrEval
  (expr: Expr.T; VAR(*OUT*) Code: CG.RuntimeError; VAR(*OUT*) Msg: TEXT) =
(* Set Code and Msg if they are not set and expr is known to produce a
   statically unconditional runtime error when evaluated. *)
(* Return the first-discovered error found and stored during Check. *)
  VAR constrExpr: T;
  BEGIN
    IF Code # CG.RuntimeError.Unknown THEN RETURN END;
    constrExpr := ArrayConstrExpr (expr);
    TYPECASE constrExpr OF
    | NULL =>
    | T(top) =>
      <* ASSERT top.state >= StateTyp.Checked *>
      IF top.RTErrorCode # CG.RuntimeError.Unknown THEN
        Code := top.RTErrorCode;
        Msg := top.RTErrorMsg;
      END;
    END;
  END CheckStaticRTErrEval;

(*EXPORTED:*)
PROCEDURE CheckStaticRTErrAssign
  (lhsType: Type.T; expr: Expr.T;
   VAR(*OUT*) Code: CG.RuntimeError; VAR(*OUT*) Msg: TEXT) =
(* PRE: expr has been Checked. *)
(* Set Code and Msg if they are not set and expr is known to produce a
   statically unconditional runtime error when assigned to a variable
   of lhsType. *)
(* Although runtime shape checks in general require the LHS variable, we
   are interested here only in statically detectable mismatches, and static
   components of the LHS shape come entirely from its type. *)
  VAR constrExpr: T;
  VAR indexType, eltType, lhsInnerType: Type.T;
  VAR arrayDepth, lhsNumber: INTEGER;
  VAR locMsg: TEXT;
  VAR b: BOOLEAN;
  BEGIN
    constrExpr := ArrayConstrExpr (expr);
    TYPECASE constrExpr OF
    | NULL =>
    | T(top) =>
      <* ASSERT top.state >= StateTyp.Checked *>
      lhsInnerType := lhsType;
      arrayDepth := LAST (top.levels^);
      FOR depth := 0 TO arrayDepth-1 DO
        WITH levelInfo = top.levels^[depth] DO
          b := ArrayType.Split (lhsInnerType, indexType, eltType);
          <* ASSERT b *>
          IF indexType # NIL AND levelInfo.staticLen >= 0 THEN
            lhsNumber := LengthOfOrdType (indexType);
            IF levelInfo.staticLen # lhsNumber
            THEN (* Found a static runtime shape mismatch. *)
              locMsg := "Shape mismatch";
              IF arrayDepth > 1 THEN
                locMsg := locMsg & ", dimension " & Fmt . Int (depth);
              END;
              locMsg
                := locMsg & ", is " & Fmt.Int (levelInfo.staticLen)
                   & ", must be " & Fmt.Int (lhsNumber) & "(2.3.1.)";
              Error.Warn
                (2, "Will raise runtime error if executed: " & locMsg);
              IF Code = CG.RuntimeError.Unknown THEN (* It's the first. *)
                Code := CG.RuntimeError.IncompatibleArrayShape;
                Msg := locMsg;
              END
            END
          END
        END;
        lhsInnerType := eltType;
      END;
    END;
  END CheckStaticRTErrAssign;

(* Externally dispatched-to: *)
PROCEDURE CheckUseFailure (top: T): BOOLEAN =
  BEGIN
    <* ASSERT top.state >= StateTyp.Checked *>
    IF AssignStmt.DoGenRTAbort (top.RTErrorCode) AND Evaluate (top) # NIL THEN
      CG.Comment
        (top.globalOffset, top.inConstArea,
         "Use of array constructor with statically detected runtime error: ",
         top.RTErrorMsg);
      CG.Abort (top.RTErrorCode);
      RETURN FALSE;
    ELSE RETURN TRUE;
    END;
  END CheckUseFailure;

BEGIN
END ArrayExpr.
