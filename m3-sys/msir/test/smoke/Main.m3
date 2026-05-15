MODULE Main;

IMPORT IO, Fmt, Text, Thread;

EXCEPTION TestExcept;
EXCEPTION TestExceptArg(INTEGER);  (* exception with an INTEGER argument *)

(* SET type tests — ColorSet and SmallSet declared after Color below *)

PROCEDURE SetUnion (a, b: ColorSet): ColorSet =
  BEGIN RETURN a + b END SetUnion;

PROCEDURE SetInter (a, b: ColorSet): ColorSet =
  BEGIN RETURN a * b END SetInter;

PROCEDURE SetDiff (a, b: ColorSet): ColorSet =
  BEGIN RETURN a - b END SetDiff;

PROCEDURE SetSymDiff (a, b: ColorSet): ColorSet =
  BEGIN RETURN a / b END SetSymDiff;

PROCEDURE SetMember (c: Color; s: ColorSet): BOOLEAN =
  BEGIN RETURN c IN s END SetMember;

PROCEDURE SetEqual (a, b: ColorSet): BOOLEAN =
  BEGIN RETURN a = b END SetEqual;

PROCEDURE SetSubset (a, b: ColorSet): BOOLEAN =
  BEGIN RETURN a <= b END SetSubset;

PROCEDURE SetProperSubset (a, b: ColorSet): BOOLEAN =
  BEGIN RETURN a < b END SetProperSubset;

PROCEDURE SmallSetMember (n: INTEGER; s: SmallSet): BOOLEAN =
  BEGIN RETURN n IN s END SmallSetMember;

(* Multi-word (128-bit) SET operations *)
PROCEDURE WideSetUnion (a, b: WideSet): WideSet =
  BEGIN RETURN a + b END WideSetUnion;
PROCEDURE WideSetInter (a, b: WideSet): WideSet =
  BEGIN RETURN a * b END WideSetInter;
PROCEDURE WideSetDiff  (a, b: WideSet): WideSet =
  BEGIN RETURN a - b END WideSetDiff;
PROCEDURE WideSetEqual (a, b: WideSet): BOOLEAN =
  BEGIN RETURN a = b END WideSetEqual;
PROCEDURE WideSetSubset(a, b: WideSet): BOOLEAN =
  BEGIN RETURN a <= b END WideSetSubset;
PROCEDURE WideSetMember(n: INTEGER; s: WideSet): BOOLEAN =
  BEGIN RETURN n IN s END WideSetMember;

(* Packed byte-array: load element at index i *)
PROCEDURE PackedByteGet (VAR a: ARRAY [0..3] OF Byte8; i: INTEGER): INTEGER =
  BEGIN RETURN a[i] END PackedByteGet;

(* Packed byte-array: store val at index i then return element at i *)
PROCEDURE PackedByteSet (VAR a: ARRAY [0..3] OF Byte8; i: INTEGER; val: INTEGER): INTEGER =
  BEGIN a[i] := val; RETURN a[i] END PackedByteSet;

(* Packed byte-array: sum elements *)
PROCEDURE PackedByteSum (VAR a: ARRAY [0..3] OF Byte8): INTEGER =
  VAR s := 0;
  BEGIN
    FOR i := 0 TO 3 DO s := s + a[i] END;
    RETURN s;
  END PackedByteSum;

(* Records with compact fields *)
PROCEDURE FillByteRec (VAR r: ByteRec; a, b, c: INTEGER) =
  BEGIN r.a := a;  r.b := b;  r.c := c  END FillByteRec;

PROCEDURE ByteRecSum (VAR r: ByteRec): INTEGER =
  BEGIN RETURN r.a + r.b + r.c END ByteRecSum;

PROCEDURE FillMixedRec (VAR r: MixedRec; flag: BOOLEAN; val, n: INTEGER) =
  BEGIN r.flag := flag;  r.val := val;  r.n := n  END FillMixedRec;

PROCEDURE MixedRecVal (VAR r: MixedRec): INTEGER =
  BEGIN
    IF r.flag THEN RETURN r.val + r.n ELSE RETURN r.n END
  END MixedRecVal;

(* Compact subrange array: [0..255] stored as byte without explicit BITS *)
PROCEDURE ByteSubGet (VAR a: ARRAY [0..3] OF [0..255]; i: INTEGER): INTEGER =
  BEGIN RETURN a[i] END ByteSubGet;

PROCEDURE ByteSubSum (VAR a: ARRAY [0..3] OF [0..255]): INTEGER =
  VAR s := 0;
  BEGIN
    FOR i := 0 TO 3 DO s := s + a[i] END;
    RETURN s;
  END ByteSubSum;

(* BOOLEAN array: stored as i8 per element, natural type i1 *)
PROCEDURE BoolArrGet (VAR a: ARRAY [0..3] OF BOOLEAN; i: INTEGER): BOOLEAN =
  BEGIN RETURN a[i] END BoolArrGet;

PROCEDURE BoolArrCount (VAR a: ARRAY [0..3] OF BOOLEAN): INTEGER =
  VAR n := 0;
  BEGIN
    FOR i := 0 TO 3 DO IF a[i] THEN INC(n) END END;
    RETURN n;
  END BoolArrCount;

VAR gLock: MUTEX := NIL;  (* initialised in module body *)
VAR gCounter: INTEGER := 0;
VAR gBase: INTEGER := 100;
VAR gRef: REFANY := NIL;

PROCEDURE IncrCounter () =
  BEGIN
    gCounter := gCounter + 1;
  END IncrCounter;

PROCEDURE LockedIncr (): INTEGER =
  BEGIN
    LOCK gLock DO
      gCounter := gCounter + 1;
    END;
    RETURN gCounter;
  END LockedIncr;

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
  Point    = RECORD x, y: INTEGER END;
  ByteRec  = RECORD a: [0..255];  b: [0..255];  c: INTEGER END;
  MixedRec = RECORD flag: BOOLEAN; val: [0..65535]; n: INTEGER END;
  Color    = {Red, Green, Blue};
  ColorSet = SET OF Color;
  SmallSet = SET OF [0..15];
  WideSet  = SET OF [0..127];  (* 128-bit set — multi-word *)
  Byte8    = BITS 8 FOR [0..255];

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

(* TYPECODE tests.
   REF INTEGER is typically owned by an imported module (visible_cells), so
   its TypeCell may not be generated in this module.  PointRef is locally
   defined here and always gets a TypeCell, making it safe to use in the
   TYPECODE(T) test. *)

PROCEDURE TypecodeOfRef (r: REFANY): INTEGER =
  BEGIN RETURN TYPECODE (r) END TypecodeOfRef;

PROCEDURE TypecodeOfPointRef (): INTEGER =
  BEGIN RETURN TYPECODE (PointRef) END TypecodeOfPointRef;

PROCEDURE MakePointRef (a, b: INTEGER): REFANY =
  VAR r: PointRef;
  BEGIN r := NEW (PointRef); r^.x := a; r^.y := b; RETURN r END MakePointRef;

(* ISTYPE / NARROW / TYPECASE-with-var tests. *)

PROCEDURE MakeIntRef (n: INTEGER): REFANY =
  VAR r: REF INTEGER;
  BEGIN r := NEW (REF INTEGER); r^ := n; RETURN r END MakeIntRef;

PROCEDURE TestIsType (r: REFANY): INTEGER =
  BEGIN
    IF ISTYPE (r, REF INTEGER) THEN RETURN 1 ELSE RETURN 0 END;
  END TestIsType;

PROCEDURE TestNarrow (r: REFANY): INTEGER =
  VAR p: REF INTEGER;
  BEGIN
    p := NARROW (r, REF INTEGER);
    RETURN p^;
  END TestNarrow;

PROCEDURE TestTypecaseVar (r: REFANY): INTEGER =
  BEGIN
    TYPECASE r OF
    | REF INTEGER (v) => RETURN v^;
    ELSE RETURN -1;
    END;
  END TestTypecaseVar;

(* GC write barrier: linked list with traced-ref field stores.
   prev^.next := cur exercises GcStore with container (write barrier). *)
TYPE
  Node    = REF NodeRec;
  NodeRec = RECORD next: Node; val: INTEGER END;

PROCEDURE BuildChain (n: INTEGER): INTEGER =
  VAR head, prev, cur: Node;  sum := 0;
  BEGIN
    FOR i := 1 TO n DO
      cur := NEW (Node);
      cur^.next := NIL;
      cur^.val  := i;
      IF head = NIL THEN
        head := cur;
      ELSE
        prev^.next := cur;   (* GcStore: traced ref into heap field *)
      END;
      prev := cur;
    END;
    cur := head;
    WHILE cur # NIL DO
      sum := sum + cur^.val;
      cur := cur^.next;
    END;
    RETURN sum;
  END BuildChain;

(* GC write barrier: store traced refs into a heap-allocated fixed-size array. *)
TYPE NodeArr3    = ARRAY [0..2] OF Node;
TYPE NodeArr3Ref = REF NodeArr3;

PROCEDURE StoreInFixedHeapArr (): INTEGER =
  VAR r: NodeArr3Ref;  n0, n1, n2: Node;
  BEGIN
    r := NEW (NodeArr3Ref);
    n0 := NEW (Node);  n0^.val := 10;  n0^.next := NIL;
    n1 := NEW (Node);  n1^.val := 20;  n1^.next := NIL;
    n2 := NEW (Node);  n2^.val := 30;  n2^.next := NIL;
    r^[0] := n0;   (* GcStore: traced ref into heap fixed-array element *)
    r^[1] := n1;
    r^[2] := n2;
    RETURN r^[0]^.val + r^[1]^.val + r^[2]^.val;
  END StoreInFixedHeapArr;

(* GC write barrier: store traced refs into a heap-allocated open array. *)
TYPE NodeArrRef = REF ARRAY OF Node;

PROCEDURE StoreInOpenHeapArr (n: INTEGER): INTEGER =
  VAR r: NodeArrRef;  cur: Node;  sum := 0;
  BEGIN
    r := NEW (NodeArrRef, n);
    FOR i := 0 TO n - 1 DO
      cur := NEW (Node);
      cur^.val  := i + 1;
      cur^.next := NIL;
      r^[i] := cur;   (* GcStore: traced ref into heap open-array element *)
    END;
    FOR j := 0 TO n - 1 DO
      sum := sum + r^[j]^.val;
    END;
    RETURN sum;
  END StoreInOpenHeapArr;

(* NEW: allocate a REF INTEGER, store, and return the stored value. *)
PROCEDURE AllocInt (n: INTEGER): INTEGER =
  VAR r: REF INTEGER;
  BEGIN r := NEW(REF INTEGER); r^ := n; RETURN r^ END AllocInt;

(* NEW(REF Record): allocate a heap Point, set fields, return sum. *)
TYPE PointRef = REF Point;

PROCEDURE AllocPair (a, b: INTEGER): INTEGER =
  VAR r: PointRef;
  BEGIN
    r := NEW(PointRef);
    r^.x := a;
    r^.y := b;
    RETURN r^.x + r^.y;
  END AllocPair;

(* NEW(REF ARRAY OF INTEGER, n): allocate, store elem 0, return it. *)
TYPE IntArrRef = REF ARRAY OF INTEGER;

PROCEDURE AllocIntArr (n: INTEGER): INTEGER =
  VAR r: IntArrRef;
  BEGIN
    r := NEW(IntArrRef, n);
    r^[0] := n * 2;
    RETURN r^[0];
  END AllocIntArr;

(* REF FixedArray deref-copy: allocate a REF [4]INTEGER, copy src into it,
   return element at index idx. *)
TYPE FixedIntArr = ARRAY [0..3] OF INTEGER;
TYPE FixedIntArrRef = REF FixedIntArr;

PROCEDURE RefFixedArrCopy (READONLY src: FixedIntArr; idx: INTEGER): INTEGER =
  VAR r: FixedIntArrRef;  copy: FixedIntArr;
  BEGIN
    r := NEW(FixedIntArrRef);
    r^ := src;
    copy := r^;
    RETURN copy[idx];
  END RefFixedArrCopy;

(* NEW: allocate a Square object, set the side field, return side*side. *)
PROCEDURE AllocSquare (side: INTEGER): INTEGER =
  VAR s: Square;
  BEGIN
    s := NEW(Square);
    s.side := side;
    RETURN s.side * s.side;
  END AllocSquare;

(* NEW + vtable dispatch: allocate a Square and call area() through the vtable. *)
PROCEDURE DispatchSquare (side: INTEGER): INTEGER =
  VAR s: Square;
  BEGIN
    s := NEW(Square);
    s.side := side;
    RETURN ShapeDispatch(s);
  END DispatchSquare;

(* Nested procedure: tests up-level variable access and static-link call. *)
PROCEDURE NestedSum (n: INTEGER): INTEGER =
  VAR acc := 0;
  PROCEDURE Add (k: INTEGER) =
  BEGIN
    INC (acc, k);
  END Add;
BEGIN
  FOR i := 1 TO n DO Add (i) END;
  RETURN acc;
END NestedSum;

(* Read-only capture optimisation: Scale reads base but never writes it.
   base is passed by value (not by pointer) in the lambda-lifted IR. *)
PROCEDURE NestedScale (base, n: INTEGER): INTEGER =
  PROCEDURE Scale (k: INTEGER): INTEGER =
  BEGIN
    RETURN base * k;
  END Scale;
BEGIN
  RETURN Scale (n);
END NestedScale;

(* Wide-char literal: exercises the WIDECHAR text-literal encoding path.
   Returns the length of W"Hi" (2 chars) so the harness can verify it. *)
PROCEDURE WideLen (): INTEGER =
  VAR w: TEXT := W"Hi";
  BEGIN
    RETURN Text.Length (w);
  END WideLen;

(* RAISE with argument: raises TestExceptArg(42) and extracts the value. *)
PROCEDURE TryRaiseArg (): INTEGER =
  BEGIN
    TRY
      RAISE TestExceptArg(42);
      RETURN 0;
    EXCEPT
      TestExceptArg(v) => RETURN v;
    END;
  END TryRaiseArg;

(* RAISE: raise TestExcept and catch it in the same proc.
   Returns 1 if the exception is caught, 0 if it unexpectedly falls through. *)
PROCEDURE TryRaise (): INTEGER =
  BEGIN
    TRY
      RAISE TestExcept;
      RETURN 0;       (* unreachable *)
    EXCEPT
      TestExcept => RETURN 1;
    END;
  END TryRaise;

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

(* IN operator — element IN constant SET *)
TYPE Weekday = {Mon, Tue, Wed, Thu, Fri, Sat, Sun};
CONST Weekends = SET OF Weekday{Weekday.Sat, Weekday.Sun};
CONST WorkWeek = SET OF Weekday{Weekday.Mon, Weekday.Tue, Weekday.Wed, Weekday.Thu, Weekday.Fri};

PROCEDURE IsWeekend (d: Weekday): BOOLEAN =
  BEGIN RETURN d IN Weekends END IsWeekend;

PROCEDURE IsWorkday (d: Weekday): BOOLEAN =
  BEGIN RETURN d IN WorkWeek END IsWorkday;

(* CONST ARRAY subscript — runtime index into a compile-time CONST array *)
CONST SmallPrimes = ARRAY [0..4] OF INTEGER{2, 3, 5, 7, 11};
CONST BoolName    = ARRAY BOOLEAN OF TEXT{"FALSE", "TRUE"};

PROCEDURE GetPrime (i: INTEGER): INTEGER =
  BEGIN RETURN SmallPrimes[i] END GetPrime;

PROCEDURE GetBoolName (b: BOOLEAN): TEXT =
  BEGIN RETURN BoolName[b] END GetBoolName;

(* Procedure-variable (indirect) calls *)
TYPE BinaryIntOp = PROCEDURE(a, b: INTEGER): INTEGER;
TYPE UnaryIntOp  = PROCEDURE(n: INTEGER): INTEGER;

PROCEDURE ApplyBinOp (f: BinaryIntOp; a, b: INTEGER): INTEGER =
  BEGIN RETURN f(a, b) END ApplyBinOp;

PROCEDURE ApplyUnary (f: UnaryIntOp; n: INTEGER): INTEGER =
  BEGIN RETURN f(n) END ApplyUnary;

(* VALUE open-array formal: caller-side copy *)
PROCEDURE SumOA (a: ARRAY OF INTEGER): INTEGER =
  VAR s := 0;
  BEGIN
    FOR i := 0 TO LAST(a) DO INC(s, a[i]) END;
    RETURN s;
  END SumOA;

(* VALUE open-array formal with open actual: passes a VAR open-array to SumOA.
   Exercises the dynamic alloca + memcpy path (open actual → VALUE open formal). *)
PROCEDURE SumViaOpenActual (VAR src: ARRAY OF INTEGER): INTEGER =
  BEGIN RETURN SumOA (src) END SumViaOpenActual;

(* Array-copy: open→fixed.  RETURN an open-array VAR param as a fixed result. *)
TYPE Fixed4 = ARRAY [0..3] OF INTEGER;

PROCEDURE FirstFour (READONLY src: ARRAY OF INTEGER): Fixed4 =
  BEGIN
    RETURN src;
  END FirstFour;

PROCEDURE CopyFirst4 (READONLY src: ARRAY OF INTEGER): Fixed4 =
  VAR dst: Fixed4;
  BEGIN
    dst := src;
    RETURN dst;
  END CopyFirst4;

(* Scalar wrappers to allow C-harness testing without ABI issues for
   large-aggregate returns.  Index the fixed-array result directly. *)
PROCEDURE FirstFourElem (READONLY src: ARRAY OF INTEGER; i: INTEGER): INTEGER =
  BEGIN
    RETURN FirstFour(src)[i];
  END FirstFourElem;

PROCEDURE CopyFirst4Elem (READONLY src: ARRAY OF INTEGER; i: INTEGER): INTEGER =
  BEGIN
    RETURN CopyFirst4(src)[i];
  END CopyFirst4Elem;

(* SUBARRAY — slicing a fixed array *)
PROCEDURE SubarrayFixedElem (start, len, idx: INTEGER): INTEGER =
  VAR a: ARRAY [0..7] OF INTEGER;
  BEGIN
    a[0] := 10; a[1] := 20; a[2] := 30; a[3] := 40;
    a[4] := 50; a[5] := 60; a[6] := 70; a[7] := 80;
    RETURN SUBARRAY (a, start, len) [idx];
  END SubarrayFixedElem;

(* SUBARRAY — slicing an open array *)
PROCEDURE SubarrayOpenElem (READONLY a: ARRAY OF INTEGER;
                             start, len, idx: INTEGER): INTEGER =
  BEGIN
    RETURN SUBARRAY (a, start, len) [idx];
  END SubarrayOpenElem;

(* SUBARRAY — sum a slice of an open array *)
PROCEDURE SumSubarray (READONLY a: ARRAY OF INTEGER;
                        start, len: INTEGER): INTEGER =
  VAR sum := 0;
  BEGIN
    WITH s = SUBARRAY (a, start, len) DO
      FOR i := 0 TO len - 1 DO sum := sum + s[i] END;
    END;
    RETURN sum;
  END SumSubarray;

(* TRUNC/FLOOR/CEILING/ROUND — runtime params prevent constant folding *)
PROCEDURE TruncTest (x: REAL): INTEGER =
  BEGIN RETURN TRUNC(x) END TruncTest;

PROCEDURE FloorTest (x: REAL): INTEGER =
  BEGIN RETURN FLOOR(x) END FloorTest;

PROCEDURE CeilingTest (x: REAL): INTEGER =
  BEGIN RETURN CEILING(x) END CeilingTest;

PROCEDURE RoundTest (y: LONGREAL): INTEGER =
  BEGIN RETURN ROUND(y) END RoundTest;

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

  (* IN operator tests *)
  IO.Put ("IsWeekend(Sat) = " & Fmt.Bool(IsWeekend(Weekday.Sat)) & "\n");
  IO.Put ("IsWeekend(Mon) = " & Fmt.Bool(IsWeekend(Weekday.Mon)) & "\n");
  IO.Put ("IsWorkday(Wed) = " & Fmt.Bool(IsWorkday(Weekday.Wed)) & "\n");
  IO.Put ("IsWorkday(Sun) = " & Fmt.Bool(IsWorkday(Weekday.Sun)) & "\n");

  (* CONST array subscript tests *)
  IO.Put ("GetPrime(0) = " & Fmt.Int(GetPrime(0)) & "\n");
  IO.Put ("GetPrime(2) = " & Fmt.Int(GetPrime(2)) & "\n");
  IO.Put ("GetPrime(4) = " & Fmt.Int(GetPrime(4)) & "\n");
  IO.Put ("GetBoolName(FALSE) = " & GetBoolName(FALSE) & "\n");
  IO.Put ("GetBoolName(TRUE) = " & GetBoolName(TRUE) & "\n");

  (* VALUE open-array formal tests: fixed actuals *)
  IO.Put ("SumOA({10,20,30}) = " & Fmt.Int(SumOA(ARRAY OF INTEGER{10, 20, 30})) & "\n");
  IO.Put ("SumOA({1,2,3,4,5}) = " & Fmt.Int(SumOA(ARRAY OF INTEGER{1, 2, 3, 4, 5})) & "\n");
  (* VALUE open-array formal tests: open actuals (dynamic alloca + memcpy) *)
  VAR oa3 := ARRAY [0..2] OF INTEGER{10, 20, 30};
      oa5 := ARRAY [0..4] OF INTEGER{1, 2, 3, 4, 5};
  BEGIN
    IO.Put ("SumViaOpenActual({10,20,30}) = " &
            Fmt.Int(SumViaOpenActual(oa3)) & "\n");
    IO.Put ("SumViaOpenActual({1,2,3,4,5}) = " &
            Fmt.Int(SumViaOpenActual(oa5)) & "\n");
  END;

  (* Procedure-variable (indirect) call tests *)
  IO.Put ("ApplyBinOp(Add,7,8) = " & Fmt.Int(ApplyBinOp(Add, 7, 8)) & "\n");
  IO.Put ("ApplyUnary(Abs,-5) = " & Fmt.Int(ApplyUnary(Abs, -5)) & "\n");
  IO.Put ("ApplyUnary(Factorial,5) = " & Fmt.Int(ApplyUnary(Factorial, 5)) & "\n");

  (* TRUNC / FLOOR / CEILING / ROUND tests *)
  IO.Put ("TruncTest(2.7) = " & Fmt.Int(TruncTest(FLOAT(2.7))) & "\n");
  IO.Put ("TruncTest(-1.3) = " & Fmt.Int(TruncTest(FLOAT(-1.3))) & "\n");
  IO.Put ("FloorTest(2.7) = " & Fmt.Int(FloorTest(FLOAT(2.7))) & "\n");
  IO.Put ("FloorTest(-1.3) = " & Fmt.Int(FloorTest(FLOAT(-1.3))) & "\n");
  IO.Put ("CeilingTest(2.7) = " & Fmt.Int(CeilingTest(FLOAT(2.7))) & "\n");
  IO.Put ("CeilingTest(-1.3) = " & Fmt.Int(CeilingTest(FLOAT(-1.3))) & "\n");
  IO.Put ("RoundTest(3.5D0) = " & Fmt.Int(RoundTest(3.5D0)) & "\n");
  IO.Put ("RoundTest(2.5D0) = " & Fmt.Int(RoundTest(2.5D0)) & "\n");
  IO.Put ("RoundTest(-0.5D0) = " & Fmt.Int(RoundTest(-0.5D0)) & "\n");

  (* EH tests *)
  IO.Put ("TryFinNormal() = " & Fmt.Int(TryFinNormal()) & "\n");
  IO.Put ("TryExceptNormal() = " & Fmt.Int(TryExceptNormal()) & "\n");

  (* Array-copy tests: open→fixed-array (via scalar wrappers to avoid ABI issues) *)
  IO.Put ("FirstFourElem({7,8,9,10},0) = " & Fmt.Int(FirstFourElem(ARRAY OF INTEGER{7,8,9,10}, 0)) & "\n");
  IO.Put ("FirstFourElem({7,8,9,10},3) = " & Fmt.Int(FirstFourElem(ARRAY OF INTEGER{7,8,9,10}, 3)) & "\n");
  IO.Put ("CopyFirst4Elem({1,2,3,4},0) = " & Fmt.Int(CopyFirst4Elem(ARRAY OF INTEGER{1,2,3,4}, 0)) & "\n");
  IO.Put ("CopyFirst4Elem({1,2,3,4},3) = " & Fmt.Int(CopyFirst4Elem(ARRAY OF INTEGER{1,2,3,4}, 3)) & "\n");

  (* REF FixedArray deref-copy tests (element-by-element init to avoid ArrayExpr) *)
  VAR srcArr: FixedIntArr;
  BEGIN
    srcArr[0] := 10;  srcArr[1] := 20;  srcArr[2] := 30;  srcArr[3] := 40;
    IO.Put ("RefFixedArrCopy({10,20,30,40},0) = " & Fmt.Int(RefFixedArrCopy(srcArr, 0)) & "\n");
    IO.Put ("RefFixedArrCopy({10,20,30,40},3) = " & Fmt.Int(RefFixedArrCopy(srcArr, 3)) & "\n");
  END;

  (* SUBARRAY tests *)
  IO.Put ("SubarrayFixed(2,4,0) = " &
          Fmt.Int(SubarrayFixedElem(2, 4, 0)) & "\n");  (* a[2] = 30 *)
  IO.Put ("SubarrayFixed(2,4,3) = " &
          Fmt.Int(SubarrayFixedElem(2, 4, 3)) & "\n");  (* a[5] = 60 *)
  VAR oa8 := ARRAY [0..7] OF INTEGER{10, 20, 30, 40, 50, 60, 70, 80};
  BEGIN
    IO.Put ("SubarrayOpen(3,3,0) = " &
            Fmt.Int(SubarrayOpenElem(oa8, 3, 3, 0)) & "\n");  (* a[3] = 40 *)
    IO.Put ("SubarrayOpen(3,3,2) = " &
            Fmt.Int(SubarrayOpenElem(oa8, 3, 3, 2)) & "\n");  (* a[5] = 60 *)
    IO.Put ("SumSubarray(2,4) = " &
            Fmt.Int(SumSubarray(oa8, 2, 4)) & "\n");  (* 30+40+50+60 = 180 *)
  END;

  (* Fmt.Real — floating-point TEXT formatting *)
  IO.Put ("Fmt.Real(1.5) = " & Fmt.Real(1.5) & "\n");
  IO.Put ("Fmt.Real(2.5) = " & Fmt.Real(2.5) & "\n");

  (* TYPECODE tests — use PointRef, which is locally defined and has a TypeCell *)
  IO.Put ("TYPECODE(NIL) = " & Fmt.Int(TypecodeOfRef(NIL)) & "\n");
  VAR pr: PointRef;
  BEGIN
    pr := NEW (PointRef);
    IO.Put ("TYPECODE(pr)=TYPECODE(PointRef) = " &
            Fmt.Int(ORD(TypecodeOfRef(pr) = TypecodeOfPointRef())) & "\n");
  END;

  (* ISTYPE / NARROW / TYPECASE-with-var tests *)
  VAR ri2: REF INTEGER;
  BEGIN
    ri2 := NEW (REF INTEGER);
    ri2^ := 42;
    IO.Put ("IsType(ri, REF INTEGER) = " & Fmt.Int(TestIsType(ri2)) & "\n");
    IO.Put ("Narrow(ri, REF INTEGER)^ = " & Fmt.Int(TestNarrow(ri2)) & "\n");
    IO.Put ("TypecaseVar(ri) = " & Fmt.Int(TestTypecaseVar(ri2)) & "\n");
  END;

  (* SET type tests *)
  VAR sRG  := ColorSet{Color.Red, Color.Green};
      sGB  := ColorSet{Color.Green, Color.Blue};
      sR   := ColorSet{Color.Red};
      sAll := ColorSet{Color.Red, Color.Green, Color.Blue};
      sm   := SmallSet{3, 7, 12};
  BEGIN
    IO.Put ("SetUnion(rg,gb)=rgb = " & Fmt.Int(ORD(SetUnion(sRG,sGB) = sAll)) & "\n");
    IO.Put ("SetInter(rg,gb)=g   = " & Fmt.Int(ORD(SetInter(sRG,sGB) = ColorSet{Color.Green})) & "\n");
    IO.Put ("SetDiff(rg,gb)=r    = " & Fmt.Int(ORD(SetDiff(sRG,sGB) = sR)) & "\n");
    IO.Put ("SetSymDiff(rg,gb)=rb= " & Fmt.Int(ORD(SetSymDiff(sRG,sGB) = ColorSet{Color.Red,Color.Blue})) & "\n");
    IO.Put ("SetMember(Red,rg)   = " & Fmt.Int(ORD(SetMember(Color.Red,sRG))) & "\n");
    IO.Put ("SetMember(Blue,rg)  = " & Fmt.Int(ORD(SetMember(Color.Blue,sRG))) & "\n");
    IO.Put ("SetEqual(rg,rg)     = " & Fmt.Int(ORD(SetEqual(sRG,sRG))) & "\n");
    IO.Put ("SetEqual(rg,gb)     = " & Fmt.Int(ORD(SetEqual(sRG,sGB))) & "\n");
    IO.Put ("SetSubset(r,rg)     = " & Fmt.Int(ORD(SetSubset(sR,sRG))) & "\n");
    IO.Put ("SetSubset(rg,r)     = " & Fmt.Int(ORD(SetSubset(sRG,sR))) & "\n");
    IO.Put ("SetProperSubset(r,rg)  = " & Fmt.Int(ORD(SetProperSubset(sR,sRG))) & "\n");
    IO.Put ("SetProperSubset(rg,rg) = " & Fmt.Int(ORD(SetProperSubset(sRG,sRG))) & "\n");
    IO.Put ("SmallSetMember(7,sm)   = " & Fmt.Int(ORD(SmallSetMember(7,sm))) & "\n");
    IO.Put ("SmallSetMember(5,sm)   = " & Fmt.Int(ORD(SmallSetMember(5,sm))) & "\n");
  END;

  (* Multi-word (128-bit) SET tests — WideSet = SET OF [0..127] *)
  (* wlo={0,63}: bits 0 and 63 (low 64-bit half only)  *)
  (* whi={64,127}: bits 64 and 127 (high 64-bit half only) *)
  (* wboth={0,63,64,127}: all four corner bits *)
  VAR wlo   := WideSet{0, 63};
      whi   := WideSet{64, 127};
      wboth := WideSet{0, 63, 64, 127};
  BEGIN
    IO.Put ("WideSetUnion(lo,hi)=both = " & Fmt.Int(ORD(WideSetUnion(wlo,whi) = wboth)) & "\n");
    IO.Put ("WideSetInter(lo,wboth)=lo= " & Fmt.Int(ORD(WideSetInter(wlo,wboth) = wlo)) & "\n");
    IO.Put ("WideSetDiff(wboth,whi)=lo= " & Fmt.Int(ORD(WideSetDiff(wboth,whi) = wlo)) & "\n");
    IO.Put ("WideSetEqual(lo,lo)      = " & Fmt.Int(ORD(WideSetEqual(wlo,wlo))) & "\n");
    IO.Put ("WideSetEqual(lo,hi)      = " & Fmt.Int(ORD(WideSetEqual(wlo,whi))) & "\n");
    IO.Put ("WideSetSubset(lo,wboth)  = " & Fmt.Int(ORD(WideSetSubset(wlo,wboth))) & "\n");
    IO.Put ("WideSetSubset(wboth,lo)  = " & Fmt.Int(ORD(WideSetSubset(wboth,wlo))) & "\n");
    IO.Put ("WideSetMember(63,wboth)  = " & Fmt.Int(ORD(WideSetMember(63,wboth))) & "\n");
    IO.Put ("WideSetMember(64,wboth)  = " & Fmt.Int(ORD(WideSetMember(64,wboth))) & "\n");
    IO.Put ("WideSetMember(63,whi)    = " & Fmt.Int(ORD(WideSetMember(63,whi))) & "\n");
  END;

  (* Packed byte-array tests *)
  VAR pb: ARRAY [0..3] OF Byte8;
  BEGIN
    pb[0] := 10;
    pb[1] := 20;
    pb[2] := 30;
    pb[3] := 40;
    IO.Put ("PackedByte[0] = " & Fmt.Int(pb[0]) & "\n");
    IO.Put ("PackedByte[2] = " & Fmt.Int(pb[2]) & "\n");
    pb[2] := pb[0] + pb[1];
    IO.Put ("PackedByte[0]+[1] stored in [2] = " & Fmt.Int(pb[2]) & "\n");
  END;
END Main.
