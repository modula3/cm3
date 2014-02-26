(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ExprParse.m3                                          *)
(* Last Modified On Tue Dec 20 15:25:42 PST 1994 By kalsow     *)
(*      Modified On Sat Aug 18 01:24:21 1990 By muller         *)

MODULE ExprParse;

IMPORT M3ID, Token, Expr, ExprRep, Error, Type, Charr, ObjectType;
IMPORT AndExpr, OrExpr, EqualExpr, CompareExpr, MultiplyExpr, DivExpr;
IMPORT DivideExpr, ModExpr, AddExpr, SubtractExpr, InExpr, PlusExpr;
IMPORT NegateExpr, NotExpr, ConcatExpr, IntegerExpr, ReelExpr;
IMPORT TextExpr, DerefExpr, QualifyExpr, SubscriptExpr, TypeExpr;
IMPORT CallExpr, ConsExpr, RangeExpr, NamedExpr, KeywordExpr, EnumExpr;
IMPORT NamedType, TInt, WCharr, CG, Brand, Int, LInt;

FROM Scanner IMPORT Match, MatchID, GetToken, Fail, cur, offset;

TYPE
  TK = Token.T;

PROCEDURE E0 (types: BOOLEAN): Expr.T =
  VAR a: Expr.T;  here := offset;
  BEGIN
    a := E1 (types);
    WHILE (cur.token = TK.tOR) DO
      GetToken (); (* OR *)
      a := OrExpr.New (a, E1 (FALSE));
      a.origin := here;
      here := offset;
    END;
    RETURN a;
  END E0;

PROCEDURE E1 (types: BOOLEAN): Expr.T =
  VAR a: Expr.T;  here := offset;
  BEGIN
    a := E2 (types);
    WHILE (cur.token = TK.tAND) DO
      GetToken (); (* AND *)
      a := AndExpr.New (a, E2 (FALSE));
      a.origin := here;
      here := offset;
    END;
    RETURN a;
  END E1;

PROCEDURE E2 (types: BOOLEAN;  ): Expr.T =
  VAR a: Expr.T;  n: INTEGER;  here := offset;
  BEGIN
    n := 0;
    WHILE (cur.token = TK.tNOT) DO
      GetToken (); (* NOT *)
      INC (n);
    END;
    a := E3 (types);
    IF (n > 0) THEN
      a := NotExpr.New (a);
      a.origin := here;
      IF ((n MOD 2) = 0) THEN
        a := NotExpr.New (a);
        a.origin := here;
      END;
    END;
    RETURN a;
  END E2;

PROCEDURE E3 (types: BOOLEAN;  ): Expr.T =
  CONST RelOps = Token.Set {TK.tEQUAL, TK.tSHARP, TK.tLESS, TK.tLSEQUAL,
                            TK.tGREATER, TK.tGREQUAL, TK.tIN};
  VAR a, b: Expr.T;  t: Token.T;  here := offset;
  BEGIN
    a := E4 (types);
    WHILE (cur.token IN RelOps) DO
      t := cur.token;
      GetToken ();
      b := E4 (FALSE);
      CASE t OF
      | TK.tEQUAL   => a := EqualExpr.New (a, b, CG.Cmp.EQ);
      | TK.tSHARP   => a := EqualExpr.New (a, b, CG.Cmp.NE);
      | TK.tLESS    => a := CompareExpr.New (a, b, CG.Cmp.LT);
      | TK.tLSEQUAL => a := CompareExpr.New (a, b, CG.Cmp.LE);
      | TK.tGREATER => a := CompareExpr.New (a, b, CG.Cmp.GT);
      | TK.tGREQUAL => a := CompareExpr.New (a, b, CG.Cmp.GE);
      | TK.tIN      => a := InExpr.New (a, b);
      ELSE             <*ASSERT FALSE*>
      END;
      a.origin := here;
      here := offset;
    END;
    RETURN a;
  END E3;

PROCEDURE E4 (types: BOOLEAN;  ): Expr.T =
  CONST AddOps = Token.Set {TK.tPLUS, TK.tMINUS, TK.tAMPERSAND};
  VAR a, b: Expr.T;  t: Token.T;  here := offset;
  BEGIN
    a := E5 (types);
    WHILE (cur.token IN AddOps) DO
      t := cur.token;
      GetToken ();
      b := E5 (FALSE);
      CASE t OF
      | TK.tPLUS      => a := AddExpr.New (a, b);
      | TK.tMINUS     => a := SubtractExpr.New (a, b);
      | TK.tAMPERSAND => a := ConcatExpr.New (a, b);
      ELSE               <*ASSERT FALSE*>
      END;
      a.origin := here;
      here := offset;
    END;
    RETURN a;
  END E4;

PROCEDURE E5 (types: BOOLEAN;  ): Expr.T =
  CONST MulOps = Token.Set {TK.tASTERISK, TK.tSLASH, TK.tDIV, TK.tMOD};
  VAR a, b: Expr.T;  t: Token.T;  here := offset;
  BEGIN
    a := E6 (types);
    WHILE (cur.token IN MulOps) DO
      t := cur.token;
      GetToken ();
      b := E6 (FALSE);
      CASE t OF
      | TK.tASTERISK => a := MultiplyExpr.New (a, b);
      | TK.tSLASH    => a := DivideExpr.New (a, b);
      | TK.tDIV      => a := DivExpr.New (a, b);
      | TK.tMOD      => a := ModExpr.New (a, b);
      ELSE              <*ASSERT FALSE*>
      END;
      a.origin := here;
      here := offset;
    END;
    RETURN a;
  END E5;

CONST
  SelectStart = Token.Set {TK.tARROW, TK.tDOT, TK.tLBRACKET, TK.tLPAREN,
                           TK.tLBRACE, TK.tBRANDED, TK.tOBJECT};

PROCEDURE E6 (types: BOOLEAN; ): Expr.T =
  VAR a, b: Expr.T;  p, m: INTEGER;  here := offset;
  BEGIN
    p := 0;
    m := 0;
    LOOP
      IF    (cur.token = TK.tPLUS)  THEN  GetToken (); INC (p);
      ELSIF (cur.token = TK.tMINUS) THEN  GetToken (); INC (m);
      ELSE  EXIT;
      END;
    END;
    a := E8 (types);  b := NIL;
    WHILE (a # b) AND (cur.token IN SelectStart) DO
      b := a;
      a := ESelector (types, a);
      a.origin := here;
      here := offset;
    END;
    IF (p + m > 0) THEN
      IF ((m MOD 2) = 1)
        THEN a := NegateExpr.New (a);
        ELSE a := PlusExpr.New (a); (* get the typechecking *)
      END;
      a.origin := here;
    END;
    RETURN a;
  END E6;

PROCEDURE E8 (types: BOOLEAN): Expr.T =
  VAR a: Expr.T;  here := offset;
  BEGIN
    CASE cur.token OF
    | TK.tIDENT         => a := NamedExpr.New (cur.id, cur.defn);   GetToken ();
    | TK.tCARDCONST     => a := IntegerExpr.New (Int.T, cur.int);   GetToken ();
    | TK.tLONGCARDCONST => a := IntegerExpr.New (LInt.T, cur.int);  GetToken ();
    | TK.tCHARCONST     => a := EnumExpr.New (Charr.T, cur.int);    GetToken ();
    | TK.tWCHARCONST    => a := EnumExpr.New (WCharr.T, cur.int);   GetToken ();
    | TK.tTEXTCONST     => a := TextExpr.New8 (cur.str);            GetToken ();
    | TK.tWTEXTCONST    => a := TextExpr.New32 (cur.wstr);          GetToken ();
    | TK.tREALCONST     => a := ReelExpr.New (cur.float);           GetToken ();
    | TK.tLONGREALCONST => a := ReelExpr.New (cur.float);           GetToken ();
    | TK.tEXTENDEDCONST => a := ReelExpr.New (cur.float);           GetToken();

    | TK.tLPAREN =>
        GetToken ();
        a := E0 (types);
        Match (TK.tRPAREN);

    | TK.tARRAY, TK.tBITS, TK.tRECORD, TK.tSET =>
        a := TypeExpr.New (Type.Parse ());
        a.origin := here;
        IF (NOT types) AND (cur.token # TK.tLBRACE) THEN
          Error.Msg ("expected a constructor");
        END;

    | TK.tBRANDED, TK.tLBRACE, TK.tUNTRACED, TK.tOBJECT,
      TK.tPROCEDURE, TK.tREF, TK.tLBRACKET, TK.tCALLCONV =>
        IF NOT types THEN Error.Msg ("unexpected type expression") END;
        a := TypeExpr.New (Type.Parse ());
        a.origin := here;

    ELSE
        Fail ("bad expression"); a := IntegerExpr.New (Int.T, TInt.Zero);
    END;
    RETURN a;
  END E8;

PROCEDURE ESelector (types: BOOLEAN;  a: Expr.T;
                                           ): Expr.T =
  VAR
    args: Expr.List;
    t: Type.T;
    open: BOOLEAN;
    name, module: M3ID.T;
    brand: Brand.T;
    here := offset;
  BEGIN
    CASE cur.token OF
    | TK.tARROW =>
        GetToken (); (* ^ *)
        a := DerefExpr.New (a);
    | TK.tDOT =>
        GetToken (); (* . *)
        a := QualifyExpr.New (a, MatchID ());
        a.origin := here;
    | TK.tLBRACKET =>
        GetToken (); (* [ *)
        LOOP
          a := SubscriptExpr.New (a, E0 (FALSE));
          a.origin := here;
          here := offset;
          IF (cur.token # TK.tCOMMA) THEN EXIT END;
          GetToken (); (* , *)
        END;
        Match (TK.tRBRACKET);
    | TK.tLPAREN =>
        GetToken (); (* ( *)
        args := ParseArgList ();
        Match (TK.tRPAREN);
        a := CallExpr.New (a, args);
        a.origin := here;
    | TK.tLBRACE =>
        GetToken (); (* { *)
        args := ParseConsList (open);
        Match (TK.tRBRACE);
        a := ConsExpr.New (a, args, open);
        a.origin := here;
    | TK.tBRANDED, TK.tOBJECT =>
        IF (types) THEN
          brand := Brand.Parse ();
          IF NamedExpr.SplitName (a, name) THEN
            t := NamedType.Create (M3ID.NoID, name);
          ELSIF QualifyExpr.SplitQID (a, module, name) THEN
            t := NamedType.Create (module, name);
          ELSE
            t := NIL;
            Fail ("bad selector");
          END;
          a := TypeExpr.New (ObjectType.Parse (t, TRUE, brand));
          a.origin := here;
        END;
    ELSE Fail ("bad selector");
    END;
    RETURN a;
  END ESelector;

TYPE RefExprList = REF ARRAY OF Expr.T;

PROCEDURE ParseArgList (): Expr.List =
  VAR
    i := 0;
    e: Expr.T;
    result: Expr.List;
    args: ARRAY [0..9] OF Expr.T;
    args2: RefExprList;
  BEGIN
    IF (cur.token # TK.tRPAREN) THEN
      LOOP
        e := EActual ();
        IF (i < NUMBER (args)) THEN
          args[i] := e;
        ELSIF (i = NUMBER (args)) THEN
          args2 := Expand (args);
          args2[i] := e;
        ELSIF (i = NUMBER (args2^)) THEN
          args2 := Expand (args2^);
          args2[i] := e;
        ELSE
          args2[i] := e;
        END;
        INC (i);
        IF (cur.token # TK.tCOMMA) THEN EXIT END;
        GetToken (); (* , *)
      END;
    END;
    result := NEW (Expr.List, i);
    IF (i <= NUMBER (args))
      THEN FOR j := 0 TO i - 1 DO result[j] := args[j] END;
      ELSE FOR j := 0 TO i - 1 DO result[j] := args2[j] END;
    END;
    RETURN result;
  END ParseArgList;

PROCEDURE ParseConsList (VAR open: BOOLEAN;
                         ): Expr.List =
  VAR
    i := 0;
    e: Expr.T;
    result: Expr.List;
    args: ARRAY [0..9] OF Expr.T;
    args2: RefExprList;
  BEGIN
    open := FALSE;
    IF (cur.token # TK.tRBRACE) THEN
      LOOP
        IF (cur.token = TK.tDOTDOT) THEN
          (* must be the end of an array constructor *)
          IF (i = 0) THEN Error.Msg("array constructor has no values") END;
          open := TRUE;
          GetToken (); (* .. *)
          EXIT;
        END;
        e := EConstructor ();
        IF (i < NUMBER (args)) THEN
          args[i] := e;
        ELSIF (i = NUMBER (args)) THEN
          args2 := Expand (args);
          args2[i] := e;
        ELSIF (i = NUMBER (args2^)) THEN
          args2 := Expand (args2^);
          args2[i] := e;
        ELSE
          args2[i] := e;
        END;
        INC (i);
        IF (cur.token # TK.tCOMMA) THEN EXIT END;
        GetToken (); (* , *)
      END;
    END;
    result := NEW (Expr.List, i);
    IF (i <= NUMBER (args))
      THEN FOR j := 0 TO i - 1 DO result[j] := args[j] END;
      ELSE FOR j := 0 TO i - 1 DO result[j] := args2[j] END;
    END;
    RETURN result;
  END ParseConsList;

PROCEDURE Expand (READONLY old: ARRAY OF Expr.T): RefExprList =
  VAR new := NEW (RefExprList, MAX (200, 2 * NUMBER (old)));
  BEGIN
    FOR i := 0 TO LAST (old) DO new[i] := old[i] END;
    RETURN new;
  END Expand;

PROCEDURE EActual (): Expr.T =
  VAR a: Expr.T;  name: M3ID.T;  here := offset;
  BEGIN
    a := E0 (TRUE);
    IF (cur.token = TK.tASSIGN) THEN
      GetToken (); (* := *)
      IF NamedExpr.SplitName (a, name)
        THEN a := KeywordExpr.New (name, E0 (FALSE)); a.origin := here;
        ELSE Error.Msg ("syntax error: expected \'keyword := value\'");
      END;
    END;
    RETURN a;
  END EActual;

PROCEDURE EConstructor (): Expr.T =
  VAR a: Expr.T;  name: M3ID.T;  here := offset;
  BEGIN
    a := E0 (FALSE);
    IF (cur.token = TK.tDOTDOT) THEN
      GetToken (); (* .. *)
      a := RangeExpr.New (a, E0 (FALSE));
      a.origin := here;
    ELSIF (cur.token = TK.tASSIGN) THEN
      GetToken (); (* := *)
      IF NamedExpr.SplitName (a, name)
        THEN a := KeywordExpr.New (name, E0 (FALSE));  a.origin := here;
        ELSE Error.Msg ("syntax error: expected \'keyword := value\'");
      END;
    END;
    RETURN a;
  END EConstructor;

BEGIN
END ExprParse.
