GENERIC MODULE RootApproximation(R,RT,RRt,C,CT,CP,CRt);
(*Copyright (c) 1996, Harry George

Abstract: Implementation of Root.

1/28/96    Harry George    Initial version, from earlier work
2/17/96    Harry George    Converted from OO to ADT format
*)
FROM NADefinitions IMPORT Error;
(*
IMPORT LongRealComplexVectorFmtLex AS VF,
       LongRealComplexFmtLex AS CF,
       LongRealFmtLex AS RF,
       IO;
*)

<*UNUSED*>
CONST Module = "RootApproximation.";
(*==========================*)
(*=====================*)
(* Quadratics          *)
(*=====================*)

(*---------------------*)
PROCEDURE RealQuadratic   (READONLY x:RealPolynomial2;        (*coefs*)
                           ):RootArray2=
(*Given a*t^2+b*t+c=0, solve for t.*)
VAR
  a:=x[2];
  b:=x[1];
  c:=x[0];
  disc,q,q1,q2:R.T;
BEGIN
  disc:=b*b-FLOAT(4.0,R.T)*a*c;
  IF disc<=R.Zero THEN
    q1:=-b/a*RT.Half;
    q2:=RT.SqRt(-disc)/a*RT.Half;
    RETURN RootArray2{
             C.T{re:=q1,im:= q2},
             C.T{re:=q1,im:=-q2}
           };
  ELSE
    (*avoid cancelation*)
    IF b<R.Zero THEN
      q:=RT.Half*(-b+RT.SqRt(disc));
    ELSE
      q:=RT.Half*(-b-RT.SqRt(disc));
    END;
    (*fails because Sgn(0)=0 :
    q:=-RT.Half*(b+RT.Sgn(b)*RT.SqRt(disc));
    *)
    q1:=q/a;
    q2:=c/q;
    RETURN RootArray2{
             C.T{re:=q1,im:=R.Zero},
             C.T{re:=q2,im:=R.Zero}
           };
  END;
END RealQuadratic;
(*---------------------*)
PROCEDURE ComplexQuadratic(READONLY x:ComplexPolynomial2;     (*coefs*)
                           ):RootArray2 RAISES {Error}=
(*Given a*t^2+b*t+c=0, solve for t.*)
CONST
  c4=FLOAT(4.0,R.T);
VAR
  a:=x[2];
  b:=x[1];
  c:=x[0];
  disc,disc_sqrt,q:C.T;
BEGIN
  disc:=C.Sub(C.Mul(b,b),C.Scale(C.Mul(a,c),c4));
  disc_sqrt:=CT.SqRt(disc);

  (*---set sign of sqrt via NR92 eqn 5.6.6---*)
  IF C.Mul(C.Conj(b),disc_sqrt).re>R.Zero THEN
    disc_sqrt:=C.Neg(disc_sqrt);
  END;

  (*---calculate per NR92 eqn 5.6.4, 5.6.5.---*)
  q:=C.Scale(C.Sub(disc_sqrt,b),RT.Half);
  RETURN RootArray2{C.Div(q,a),C.Div(c,q)};
END ComplexQuadratic;
(*---------------------*)
PROCEDURE RealNewtonMaehli   (x:RRt.T):REF CRt.RootArray RAISES {Error}=
VAR
  xc:=NEW(CRt.T,NUMBER(x^));
BEGIN
  FOR j:=0 TO LAST(xc^) DO
    xc[j] := C.T{x[j],R.Zero};
  END;
  RETURN ComplexNewtonMaehli(xc);
END RealNewtonMaehli;
(*---------------------*)
PROCEDURE ComplexNewtonMaehli(x:CRt.T):REF CRt.RootArray RAISES {Error}=
CONST maxArgError = RT.Eps*FLOAT(10.0,R.T);  (* error in the argument r *)
<*UNUSED*>
CONST maxValError = RT.Eps*FLOAT(100.0,R.T);  (* error in the value p(r) *)
VAR
  z:=NEW(REF CRt.RootArray,LAST(x^));
  maxIter:=100;
BEGIN
  VAR
    jr,nr:R.T;
  BEGIN
    nr:=FLOAT(NUMBER(z^),R.T);
    FOR j:=0 TO LAST(z^) DO
      jr:=R.One-FLOAT(j,R.T)/nr*R.Two;
      z[j]:=C.T{jr,jr};
    END;
  END;

  VAR
    iterating:BOOLEAN;
  BEGIN
    REPEAT
(*
IO.Put(VF.Fmt(z) & "\n");
*)
      iterating := FALSE;
      FOR j:=0 TO LAST(z^) DO
        VAR
          y : ARRAY [0..1] OF C.T;
          sumRec := C.Zero;
        BEGIN
          CP.EvalDerivative(x,z[j],y);
          (*iterating := iterating OR (norm(y) > maxValError);*)

          FOR k:=0 TO LAST(z^) DO
            IF j # k THEN
              sumRec := C.Add (sumRec, C.Rec(C.Sub(z[j],z[k])));
            END;
          END;

          VAR
            dz:=C.Div(y[0],(C.Sub(y[1],C.Mul(y[0],sumRec))));
          BEGIN
            z[j] := C.Sub(z[j],dz);
(*
IO.Put("z "&CF.Fmt(z[j])&", dz "&CF.Fmt(dz)&"\n");
IO.Put("Abs2(z) "&RF.Fmt(CT.AbsSqr(z[j]))&", Abs2(dz) "&RF.Fmt(CT.AbsSqr(dz))&
        ", maxArgError "&RF.Fmt(maxArgError)&"\n");
*)
          (*
            I'm not sure if using the relative change is the right criteria
            but it works even if one zero is at 6e11 and another is at 0.2
            this may occur if the coefficent of the highest power of z is close to zero
          *)
            iterating := iterating OR (CT.AbsSqr(dz) > CT.AbsSqr(z[j]) * maxArgError * maxArgError);
          END;
        END;
      END;
      DEC(maxIter);
      iterating := iterating AND maxIter>0;
    UNTIL NOT iterating;
  END;
  RETURN z;
END ComplexNewtonMaehli;

(*
// calculates all zeros simultanously
// calculation is made with complex numbers to catch all zeros
// iteration is aborted if polynomial value at these points is small enough
void RPolynomial::zeros (list<cdouble>& zeros) const {
  const double max_arg_error = 1.e-13;  // error in the argument r
  const double max_val_error = 1.e-10;  // error in the value p(r)
  for (int i=1; i<size(); i++) {
    //zeros.append(cdouble(i,i)/cdouble(size())*2-1);
    //zeros.append(1);  // iteration changes nothing
    //zeros.append(1+double(i)/1000);
    zeros.append(cdouble(i,i)/cdouble(size())*2-1);
  };

  // logging fails with linear and absolute polynomials because of the index [size()-2]
  //bool log = abs(( *this)[size()-1]) < 2e-12 * abs(( *this)[size()-2]);
  bool iterating;
  do {
    iterating = false;
    //if (log) {cout << zeros << "  dr "; }
    list<cdouble>::iterator it_dst;
    foreach_const (it_dst, zeros) {
      cdouble y, dy;
      cdouble& r = zeros[it_dst];
      value_diff(r, y, dy);
      //iterating = iterating || (norm(y) > max_val_error);

      cdouble sum_rec=0;
      list<cdouble>::iterator it;
      foreach_const (it, zeros) {
        if (it != it_dst) {
          sum_rec += 1/(r-zeros[it]);
        };
      };

      cdouble dr = y/(dy-y*sum_rec);
      r -= dr;
      //if (log) {cout << dr << "  "; }
      // I'm not sure if using the relative change is the right criteria
      // but it works even if one zero is at 6e11 and another is at 0.2
      // this may occur if the coefficent of the highest power of z is close to zero
      iterating = iterating || (norm(dr) > norm(r) * max_arg_error);
    };
    //if (log) {cout << endl; }
  } while (iterating);
};

*)

(*==========================*)
BEGIN
END RootApproximation.
