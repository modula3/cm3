MODULE Main;

IMPORT IO, Fmt;

EXCEPTION TestExcept;

VAR gCounter: INTEGER := 0;
VAR gBase: INTEGER := 100;
VAR gRef: REFANY := NIL;

PROCEDURE IncrCounter () =
  BEGIN
    gCounter := gCounter + 1;
  END IncrCounter;

PROCEDURE AddToCounter (n: INTEGER) =
  BEGIN
    gCounter := gCounter + n;
  END AddToCounter;

PROCEDURE GetCounter (): INTEGER =
  BEGIN
    RETURN gCounter + gBase;
  END GetCounter;

PROCEDURE Add (a, b: INTEGER): INTEGER =
  BEGIN
    RETURN a + b;
  END Add;

PROCEDURE Factorial (n: INTEGER): INTEGER =
  VAR result, i: INTEGER;
  BEGIN
    result := 1;
    i := n;
    WHILE i > 0 DO
      result := result * i;
      i := i - 1;
    END;
    RETURN result;
  END Factorial;

PROCEDURE Abs (n: INTEGER): INTEGER =
  BEGIN
    IF n < 0 THEN
      RETURN -n;
    ELSE
      RETURN n;
    END;
  END Abs;

PROCEDURE Sign (n: INTEGER): INTEGER =
  BEGIN
    IF n > 0 THEN
      RETURN 1;
    ELSIF n < 0 THEN
      RETURN -1;
    ELSE
      RETURN 0;
    END;
  END Sign;

PROCEDURE SumTo (n: INTEGER): INTEGER =
  VAR result, i: INTEGER;
  BEGIN
    result := 0;
    i := 1;
    WHILE i <= n DO
      result := result + i;
      i := i + 1;
    END;
    RETURN result;
  END SumTo;

PROCEDURE AbsSum (a, b: INTEGER): INTEGER =
  BEGIN
    RETURN Abs(a) + Abs(b);
  END AbsSum;

PROCEDURE FactSum (n: INTEGER): INTEGER =
  BEGIN
    RETURN Add(Factorial(n), SumTo(n));
  END FactSum;

TYPE
  Point = RECORD x, y: INTEGER END;
  Color = {Red, Green, Blue};

PROCEDURE MakePoint (x, y: INTEGER): Point =
  VAR p: Point;
  BEGIN
    p.x := x;
    p.y := y;
    RETURN p;
  END MakePoint;

PROCEDURE PointSum (p: Point): INTEGER =
  BEGIN
    RETURN p.x + p.y;
  END PointSum;

PROCEDURE ColorToInt (c: Color): INTEGER =
  BEGIN
    RETURN ORD(c);
  END ColorToInt;

PROCEDURE Swap (VAR a, b: INTEGER) =
  VAR t: INTEGER;
  BEGIN
    t := a;
    a := b;
    b := t;
  END Swap;

PROCEDURE IncBy (VAR a: INTEGER;  b: INTEGER) =
  BEGIN
    a := a + b;
  END IncBy;

PROCEDURE ReadOnlySum (READONLY p: Point): INTEGER =
  BEGIN
    RETURN p.x + p.y;
  END ReadOnlySum;

PROCEDURE SumOpenArr (READONLY a: ARRAY OF INTEGER): INTEGER =
  BEGIN
    RETURN a[0] + a[1];
  END SumOpenArr;

PROCEDURE SetFirst (VAR a: ARRAY OF INTEGER;  v: INTEGER) =
  BEGIN
    a[0] := v;
  END SetFirst;

PROCEDURE RelaySum (READONLY a: ARRAY OF INTEGER): INTEGER =
  BEGIN
    RETURN SumOpenArr (a) + a[2];
  END RelaySum;

PROCEDURE SetRef (r: REFANY) =
  BEGIN
    gRef := r;
  END SetRef;

PROCEDURE GetRef (): REFANY =
  BEGIN
    RETURN gRef;
  END GetRef;

TYPE IntPtr = UNTRACED REF INTEGER;

PROCEDURE LoadInt (p: IntPtr): INTEGER =
  BEGIN
    RETURN p^;
  END LoadInt;

PROCEDURE StoreInt (p: IntPtr;  v: INTEGER) =
  BEGIN
    p^ := v;
  END StoreInt;

PROCEDURE BumpInt (p: IntPtr) =
  BEGIN
    p^ := p^ + 1;
  END BumpInt;

TYPE Vec5 = ARRAY [0..4] OF INTEGER;

PROCEDURE FillVec (VAR v: Vec5) =
  VAR i: INTEGER;
  BEGIN
    i := 0;
    WHILE i <= 4 DO
      v[i] := i * i;
      i := i + 1;
    END;
  END FillVec;

PROCEDURE SumVec (READONLY v: Vec5): INTEGER =
  VAR i, s: INTEGER;
  BEGIN
    s := 0;
    i := 0;
    WHILE i <= 4 DO
      s := s + v[i];
      i := i + 1;
    END;
    RETURN s;
  END SumVec;

PROCEDURE GetSecond (READONLY v: Vec5): INTEGER =
  BEGIN
    RETURN v[1];
  END GetSecond;

(* --- FOR loops --- *)

PROCEDURE SumForUp (n: INTEGER): INTEGER =
  VAR result: INTEGER;
  BEGIN
    result := 0;
    FOR i := 1 TO n DO
      result := result + i;
    END;
    RETURN result;
  END SumForUp;

PROCEDURE SumForDown (n: INTEGER): INTEGER =
  VAR result: INTEGER;
  BEGIN
    result := 0;
    FOR i := n TO 1 BY -1 DO
      result := result + i;
    END;
    RETURN result;
  END SumForDown;

PROCEDURE SumForBy2 (n: INTEGER): INTEGER =
  VAR result: INTEGER;
  BEGIN
    result := 0;
    FOR i := 0 TO n BY 2 DO
      result := result + i;
    END;
    RETURN result;
  END SumForBy2;

(* --- CASE statements --- *)

PROCEDURE WeekdayNum (n: INTEGER): INTEGER =
  VAR result: INTEGER;
  BEGIN
    CASE n OF
    | 1 => result := 10;
    | 2 => result := 20;
    | 3 => result := 30;
    | 4 => result := 40;
    | 5 => result := 50;
    ELSE   result := -1;
    END;
    RETURN result;
  END WeekdayNum;

PROCEDURE ColorCode (c: Color): INTEGER =
  VAR result: INTEGER;
  BEGIN
    CASE c OF
    | Color.Red   => result := 16_FF0000;
    | Color.Green => result := 16_00FF00;
    | Color.Blue  => result := 16_0000FF;
    END;
    RETURN result;
  END ColorCode;

PROCEDURE RangeCase (n: INTEGER): INTEGER =
  VAR result: INTEGER;
  BEGIN
    CASE n OF
    | 1..3   => result := 1;
    | 4..6   => result := 2;
    | 7..10  => result := 3;
    ELSE        result := 0;
    END;
    RETURN result;
  END RangeCase;

(* --- DIV / MOD --- *)

PROCEDURE DivTest (a, b: INTEGER): INTEGER =
  BEGIN
    RETURN a DIV b;
  END DivTest;

PROCEDURE ModTest (a, b: INTEGER): INTEGER =
  BEGIN
    RETURN a MOD b;
  END ModTest;

(* --- INC / DEC --- *)

PROCEDURE CountWithINC (n: INTEGER): INTEGER =
  VAR x: INTEGER;
  BEGIN
    x := 0;
    FOR i := 1 TO n DO
      INC (x);
    END;
    RETURN x;
  END CountWithINC;

PROCEDURE CountDownWithDEC (n: INTEGER): INTEGER =
  VAR x: INTEGER;
  BEGIN
    x := n;
    REPEAT
      DEC (x);
    UNTIL x <= 0;
    RETURN x;
  END CountDownWithDEC;

PROCEDURE IncBy3 (n: INTEGER): INTEGER =
  VAR x: INTEGER;
  BEGIN
    x := n;
    INC (x, 3);
    RETURN x;
  END IncBy3;

PROCEDURE DecBy5 (n: INTEGER): INTEGER =
  VAR x: INTEGER;
  BEGIN
    x := n;
    DEC (x, 5);
    RETURN x;
  END DecBy5;

(* --- REPEAT --- *)

PROCEDURE RepeatSum (n: INTEGER): INTEGER =
  VAR result, i: INTEGER;
  BEGIN
    result := 0;
    i := 1;
    REPEAT
      result := result + i;
      INC (i);
    UNTIL i > n;
    RETURN result;
  END RepeatSum;

PROCEDURE RepeatCountdown (n: INTEGER): INTEGER =
  VAR x: INTEGER;
  BEGIN
    x := n;
    REPEAT
      DEC (x);
    UNTIL x = 0;
    RETURN x;
  END RepeatCountdown;

(* --- WITH --- *)

PROCEDURE WithDesignator (VAR a: ARRAY OF INTEGER): INTEGER =
  BEGIN
    WITH x = a[1] DO
      x := x * 2;
    END;
    RETURN a[1];
  END WithDesignator;

PROCEDURE WithField (): INTEGER =
  VAR p: Point;
  BEGIN
    p.x := 10;
    p.y := 20;
    WITH px = p.x DO
      px := px + 5;
    END;
    RETURN p.x + p.y;
  END WithField;

PROCEDURE WithScalar (n: INTEGER): INTEGER =
  BEGIN
    WITH doubled = n * 2 DO
      RETURN doubled + 1;
    END;
  END WithScalar;

(* --- AND / OR (short-circuit) --- *)

PROCEDURE BothPos (a, b: INTEGER): BOOLEAN =
  BEGIN
    RETURN a > 0 AND b > 0;
  END BothPos;

PROCEDURE EitherPos (a, b: INTEGER): BOOLEAN =
  BEGIN
    RETURN a > 0 OR b > 0;
  END EitherPos;

PROCEDURE Neither (a, b: INTEGER): BOOLEAN =
  BEGIN
    RETURN NOT (a > 0 OR b > 0);
  END Neither;

(* Method dispatch via virtual call on OBJECT types *)
TYPE
  Shape = OBJECT METHODS area(): INTEGER := ShapeArea END;
  Square = Shape OBJECT side: INTEGER := 0
             OVERRIDES area := SquareArea END;

PROCEDURE ShapeArea (<*UNUSED*>self: Shape): INTEGER = BEGIN RETURN 0 END ShapeArea;
PROCEDURE SquareArea (self: Square): INTEGER = BEGIN RETURN self.side * self.side END SquareArea;

PROCEDURE ShapeDispatch (s: Shape): INTEGER =
  BEGIN RETURN s.area() END ShapeDispatch;

(* TYPECASE: dispatch on dynamic type of a REFANY.
   Returns 1 for REF INTEGER, 2 for REF BOOLEAN, 0 for ELSE.
   No variable binding in clauses so calling with NIL is safe:
   ScanTypecase(NIL) returns 0 (first clause) without accessing runtime state. *)
TYPE RefBool = REF BOOLEAN;

PROCEDURE TypecaseKind (r: REFANY): INTEGER =
  BEGIN
    TYPECASE r OF
    | REF INTEGER => RETURN 1;
    | RefBool     => RETURN 2;
    ELSE            RETURN 0;
    END;
  END TypecaseKind;

(* TRY/FINALLY normal path: finally block runs, result = 1 + 10 = 11 *)
PROCEDURE TryFinNormal (): INTEGER =
  VAR n: INTEGER := 1;
  BEGIN
    TRY
      n := n + Add(0, 0);
    FINALLY
      n := n + 10;
    END;
    RETURN n;
  END TryFinNormal;

(* TRY/EXCEPT normal path: no exception raised, body result returned *)
PROCEDURE TryExceptNormal (): INTEGER =
  VAR n: INTEGER := 0;
  BEGIN
    TRY
      n := Add(5, 3);
    EXCEPT
      TestExcept => n := -1;
    END;
    RETURN n;
  END TryExceptNormal;

BEGIN
  IO.Put ("Add(2,3) = " & Fmt.Int(Add(2,3)) & "\n");
  IO.Put ("Factorial(5) = " & Fmt.Int(Factorial(5)) & "\n");
  IO.Put ("Abs(-7) = " & Fmt.Int(Abs(-7)) & "\n");
  IO.Put ("Abs(4) = " & Fmt.Int(Abs(4)) & "\n");
  IO.Put ("Sign(-3) = " & Fmt.Int(Sign(-3)) & "\n");
  IO.Put ("Sign(0) = " & Fmt.Int(Sign(0)) & "\n");
  IO.Put ("Sign(5) = " & Fmt.Int(Sign(5)) & "\n");
  IO.Put ("SumTo(10) = " & Fmt.Int(SumTo(10)) & "\n");
  IO.Put ("AbsSum(-3, 4) = " & Fmt.Int(AbsSum(-3, 4)) & "\n");
  IO.Put ("FactSum(4) = " & Fmt.Int(FactSum(4)) & "\n");
  IO.Put ("PointSum({3,4}) = " & Fmt.Int(PointSum(MakePoint(3,4))) & "\n");
  IO.Put ("ColorToInt(Blue) = " & Fmt.Int(ColorToInt(Color.Blue)) & "\n");

  VAR x, y: INTEGER;  q: Point;
  BEGIN
    x := 11;  y := 22;
    Swap(x, y);
    IO.Put ("after Swap(11,22): x=" & Fmt.Int(x) & " y=" & Fmt.Int(y) & "\n");
    IncBy(x, 100);
    IO.Put ("after IncBy(x,100): x=" & Fmt.Int(x) & "\n");
    q := MakePoint(5, 7);
    IO.Put ("ReadOnlySum({5,7}) = " & Fmt.Int(ReadOnlySum(q)) & "\n");
  END;

  VAR v: Vec5;
  BEGIN
    FillVec(v);
    IO.Put ("SumVec(squares 0..4) = " & Fmt.Int(SumVec(v)) & "\n");
    IO.Put ("GetSecond = " & Fmt.Int(GetSecond(v)) & "\n");
    IO.Put ("SumOpenArr({0,1}) = " & Fmt.Int(SumOpenArr(v)) & "\n");
    IO.Put ("RelaySum({0,1,4}) = " & Fmt.Int(RelaySum(v)) & "\n");
    SetFirst(v, 99);
    IO.Put ("v[0] after SetFirst = " & Fmt.Int(v[0]) & "\n");
  END;

  IncrCounter();
  IncrCounter();
  AddToCounter(8);
  IO.Put ("GetCounter() = " & Fmt.Int(GetCounter()) & "\n");

  VAR r: REFANY;
  BEGIN
    SetRef(NEW(REF INTEGER));
    r := GetRef();
    IO.Put ("gRef is NIL after SetRef: " & Fmt.Bool(r = NIL) & "\n");
  END;

  (* FOR loop tests *)
  IO.Put ("SumForUp(10) = " & Fmt.Int(SumForUp(10)) & "\n");
  IO.Put ("SumForDown(10) = " & Fmt.Int(SumForDown(10)) & "\n");
  IO.Put ("SumForBy2(10) = " & Fmt.Int(SumForBy2(10)) & "\n");

  (* CASE tests *)
  IO.Put ("WeekdayNum(3) = " & Fmt.Int(WeekdayNum(3)) & "\n");
  IO.Put ("WeekdayNum(9) = " & Fmt.Int(WeekdayNum(9)) & "\n");
  IO.Put ("ColorCode(Red) = " & Fmt.Int(ColorCode(Color.Red)) & "\n");
  IO.Put ("ColorCode(Blue) = " & Fmt.Int(ColorCode(Color.Blue)) & "\n");
  IO.Put ("RangeCase(2) = " & Fmt.Int(RangeCase(2)) & "\n");
  IO.Put ("RangeCase(5) = " & Fmt.Int(RangeCase(5)) & "\n");
  IO.Put ("RangeCase(8) = " & Fmt.Int(RangeCase(8)) & "\n");
  IO.Put ("RangeCase(0) = " & Fmt.Int(RangeCase(0)) & "\n");

  (* DIV / MOD tests *)
  IO.Put ("DivTest(17, 5) = " & Fmt.Int(DivTest(17, 5)) & "\n");
  IO.Put ("ModTest(17, 5) = " & Fmt.Int(ModTest(17, 5)) & "\n");
  IO.Put ("DivTest(-7, 2) = " & Fmt.Int(DivTest(-7, 2)) & "\n");
  IO.Put ("ModTest(-7, 2) = " & Fmt.Int(ModTest(-7, 2)) & "\n");

  (* INC / DEC / REPEAT tests *)
  IO.Put ("CountWithINC(7) = " & Fmt.Int(CountWithINC(7)) & "\n");
  IO.Put ("CountDownWithDEC(5) = " & Fmt.Int(CountDownWithDEC(5)) & "\n");
  IO.Put ("IncBy3(10) = " & Fmt.Int(IncBy3(10)) & "\n");
  IO.Put ("DecBy5(10) = " & Fmt.Int(DecBy5(10)) & "\n");
  IO.Put ("RepeatSum(5) = " & Fmt.Int(RepeatSum(5)) & "\n");
  IO.Put ("RepeatCountdown(4) = " & Fmt.Int(RepeatCountdown(4)) & "\n");

  (* WITH tests *)
  VAR arr: ARRAY [0..2] OF INTEGER;
  BEGIN
    arr[0] := 10;  arr[1] := 20;  arr[2] := 30;
    IO.Put ("WithDesignator(arr) = " & Fmt.Int(WithDesignator(arr)) & "\n");
  END;
  IO.Put ("WithField() = " & Fmt.Int(WithField()) & "\n");
  IO.Put ("WithScalar(7) = " & Fmt.Int(WithScalar(7)) & "\n");

  (* AND / OR tests *)
  IO.Put ("BothPos(3, 4) = " & Fmt.Bool(BothPos(3, 4)) & "\n");
  IO.Put ("BothPos(-1, 4) = " & Fmt.Bool(BothPos(-1, 4)) & "\n");
  IO.Put ("EitherPos(-1, 4) = " & Fmt.Bool(EitherPos(-1, 4)) & "\n");
  IO.Put ("EitherPos(-1, -2) = " & Fmt.Bool(EitherPos(-1, -2)) & "\n");
  IO.Put ("Neither(-1, -2) = " & Fmt.Bool(Neither(-1, -2)) & "\n");

  (* TYPECASE test *)
  IO.Put ("TypecaseKind(NIL) = " & Fmt.Int(TypecaseKind(NIL)) & "\n");

  (* EH tests *)
  IO.Put ("TryFinNormal() = " & Fmt.Int(TryFinNormal()) & "\n");
  IO.Put ("TryExceptNormal() = " & Fmt.Int(TryExceptNormal()) & "\n");
END Main.
