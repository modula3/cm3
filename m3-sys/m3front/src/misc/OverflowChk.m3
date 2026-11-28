(* See file COPYRIGHT-CMASS for details. *)

MODULE OverflowChk;

IMPORT CG, Target, TInt;

(* Each check computes a flag, 0 or 1 of type Int, that is 1 exactly when
   the operation overflowed, and passes it to Trap. *)

PROCEDURE Int (): CG.IType =
  BEGIN
    RETURN Target.Integer.cg_type;
  END Int;

PROCEDURE Signed (t: CG.Type): BOOLEAN =
  BEGIN
    RETURN (t = Target.Integer.cg_type) OR (t = Target.Longint.cg_type);
  END Signed;

PROCEDURE First (t: CG.IType): Target.Int =
  BEGIN
    IF t = Target.Integer.cg_type
      THEN RETURN Target.Integer.min;
      ELSE RETURN Target.Longint.min;
    END;
  END First;

PROCEDURE Trap () =
  (* s0.Int is the overflow flag: abort if it is 1;  pop *)
  BEGIN
    CG.Check_hi (Int (), TInt.Zero, CG.RuntimeError.IntegerOverflow);
    CG.Discard (Int ());
  END Trap;

PROCEDURE PushEq (v: CG.Val;  t: CG.IType;  READONLY i: Target.Int) =
  (* push (v = i) as a flag *)
  BEGIN
    CG.Push (v);
    CG.Load_integer (t, i);
    CG.Compare (t, CG.Cmp.EQ);
  END PushEq;

PROCEDURE Widen (t: CG.IType) =
  (* s0.t := s0.Int, for a flag *)
  BEGIN
    IF t # Int () THEN CG.Loophole (Int (), t) END;
  END Widen;

PROCEDURE Add (t: CG.IType) =
  VAR a, b, r: CG.Val;
  BEGIN
    b := CG.Pop_temp ();
    a := CG.Pop_temp ();
    CG.Push (a);  CG.Push (b);  CG.Add (t);
    r := CG.Pop_temp ();
    (* overflow iff a and b have the same sign and r the other:
       ((a XOR r) AND (b XOR r)) < 0 *)
    CG.Push (a);  CG.Push (r);  CG.Xor (t);
    CG.Push (b);  CG.Push (r);  CG.Xor (t);
    CG.And (t);
    CG.Load_integer (t, TInt.Zero);
    CG.Compare (t, CG.Cmp.LT);
    Trap ();
    CG.Push (r);
    CG.Free (a);  CG.Free (b);  CG.Free (r);
  END Add;

PROCEDURE Subtract (t: CG.IType) =
  VAR a, b, r: CG.Val;
  BEGIN
    b := CG.Pop_temp ();
    a := CG.Pop_temp ();
    CG.Push (a);  CG.Push (b);  CG.Subtract (t);
    r := CG.Pop_temp ();
    (* overflow iff a and b have different signs and r has b's:
       ((a XOR b) AND (a XOR r)) < 0 *)
    CG.Push (a);  CG.Push (b);  CG.Xor (t);
    CG.Push (a);  CG.Push (r);  CG.Xor (t);
    CG.And (t);
    CG.Load_integer (t, TInt.Zero);
    CG.Compare (t, CG.Cmp.LT);
    Trap ();
    CG.Push (r);
    CG.Free (a);  CG.Free (b);  CG.Free (r);
  END Subtract;

PROCEDURE Multiply (t: CG.IType) =
  VAR a, b, r, z: CG.Val;
  BEGIN
    b := CG.Pop_temp ();
    a := CG.Pop_temp ();
    CG.Push (a);  CG.Push (b);  CG.Multiply (t);
    r := CG.Pop_temp ();

    (* Overflow iff
         (a = -1 AND b = FIRST)  OR  (a NOT IN {0, -1} AND r DIV a # b).
       If r DIV a = b with a NOT IN {0, -1}, then r - a*b is a multiple of
       2^n smaller in magnitude than a, so r = a*b exactly.  To keep the
       division itself from trapping, divide by d = 1 when a IN {0, -1},
       and mask that case out. *)

    (* z := (a = 0) OR (a = -1) *)
    PushEq (a, t, TInt.Zero);
    PushEq (a, t, TInt.MOne);
    CG.Or (Int ());
    z := CG.Pop_temp ();

    (* (r DIV d # b) AND NOT z, where d := (a AND (z - 1)) OR z *)
    CG.Push (r);
    CG.Push (a);
    CG.Push (z);  Widen (t);  CG.Load_integer (t, TInt.One);  CG.Subtract (t);
    CG.And (t);
    CG.Push (z);  Widen (t);
    CG.Or (t);
    CG.Div (t, CG.Sign.Unknown, CG.Sign.Unknown);
    CG.Push (b);
    CG.Compare (t, CG.Cmp.NE);
    CG.Push (z);  CG.Load_integer (Int (), TInt.One);  CG.Xor (Int ());
    CG.And (Int ());

    (* OR (a = -1 AND b = FIRST) *)
    PushEq (a, t, TInt.MOne);
    PushEq (b, t, First (t));
    CG.And (Int ());
    CG.Or (Int ());

    Trap ();
    CG.Push (r);
    CG.Free (a);  CG.Free (b);  CG.Free (r);  CG.Free (z);
  END Multiply;

PROCEDURE Negate (t: CG.IType) =
  VAR a: CG.Val;
  BEGIN
    a := CG.Pop_temp ();
    PushEq (a, t, First (t));
    Trap ();
    CG.Push (a);
    CG.Negate (t);
    CG.Free (a);
  END Negate;

PROCEDURE Abs (t: CG.IType) =
  VAR a: CG.Val;
  BEGIN
    a := CG.Pop_temp ();
    PushEq (a, t, First (t));
    Trap ();
    CG.Push (a);
    CG.Abs (t);
    CG.Free (a);
  END Abs;

PROCEDURE Div (t: CG.IType;  sa, sb: CG.Sign) =
  VAR a, b: CG.Val;
  BEGIN
    b := CG.Pop_temp ();
    a := CG.Pop_temp ();
    (* overflow iff a = FIRST AND b = -1 *)
    PushEq (a, t, First (t));
    PushEq (b, t, TInt.MOne);
    CG.And (Int ());
    Trap ();
    CG.Push (a);  CG.Push (b);
    CG.Div (t, sa, sb);
    CG.Free (a);  CG.Free (b);
  END Div;

BEGIN
END OverflowChk.
