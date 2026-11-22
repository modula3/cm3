(* $Id: Example.i3,v 1.11 2010/04/22 20:05:48 mika Exp $ *)

INTERFACE Example;
IMPORT Text;

EXCEPTION X1; EXCEPTION X2;

TYPE

  T = OBJECT METHODS
    xyz(a : LONGREAL := LAST(LONGREAL));
    hello() (*:= THello*);
    meow(s1s := FIRST(S); s2integer : INTEGER) : INTEGER RAISES { X1, X2 };
    goodbye(s1 : S; s2 : REF W; s3 : W; s4 : INTEGER) : V;
    helloAgain(a : [VAL(1,INTEGER)..VAL(3,INTEGER)]; b : ARRAY OF S);
  END;

  O = T BRANDED "XYZ" OBJECT 
    f1 : INTEGER;
    f2 : X;
  METHODS
    m();
    n() (*:= THello;
  OVERRIDES
    hello := THello;
 *)
  END;

  RRR = REF R;
  
  OAB = T BRANDED OBJECT END;

  TT = T OBJECT METHODS
    ttMethod();
    x() := THello;
  OVERRIDES 
  END;

  R = RECORD
    first : LONGREAL;
    second : INTEGER;
  END;

  Q = [1900..2009];

  S = { One, Two, Three, Four };
  
  U = [S.Two .. S.Three];

  V = SET OF U;

  W = INTEGER;

  B = BOOLEAN;

  C = CHAR;

  M = MUTEX;

  X = [S.Two .. S.Three];

  Y = X;

CONST True = BOOLEAN.TRUE;

CONST Brand = "Example";

(*PROCEDURE TakesS(s : S := S.Two);*)

(*CONST PP = TakesS;*)

TYPE PType = PROCEDURE (s : S);

CONST AnS = S.Two;

PROCEDURE TTHello(tt : TT);

PROCEDURE THello(t : T);

CONST x = 1;

CONST TE = Text.Equal;

TYPE RR = RECORD x, y : LONGREAL END;

CONST rr = RR { 1.0d0, -1.0d0 };

CONST sss = SET OF Q { 1950 };

CONST qqq = ARRAY OF CARDINAL { 1, LAST(CARDINAL), LAST(CARDINAL) DIV 2, 0 };

CONST a1 = ARRAY OF CARDINAL { 1, 2, 3 };

CONST a2 = ARRAY [0..5] OF CARDINAL { 19, .. };

CONST rrr = R { 3.0d0, 11 };

CONST rrrr = R { second := 23, first := 42.0d0 };

CONST BigArray = ARRAY OF ARRAY OF CARDINAL {
  ARRAY OF CARDINAL { 0, 1, 2 },
  ARRAY OF CARDINAL { 4, 5, 6 },
  ARRAY OF CARDINAL { 7, 8, 9 }};

TYPE Color = { Red, Green, Blue };

CONST ColArray = ARRAY Color OF ARRAY [0..2] OF CARDINAL {
  ARRAY OF CARDINAL { 0, 1, 2 },
  ARRAY OF CARDINAL { 4, 5, 6 },
  ARRAY OF CARDINAL { 7, 8, 9 }};

PROCEDURE Sum(a : ARRAY OF LONGREAL) : LONGREAL;

END Example.
