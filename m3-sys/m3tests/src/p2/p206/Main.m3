MODULE Main;

IMPORT IO;

TYPE
  OAC = ARRAY OF CHAR;
  OAOAC = ARRAY OF OAC;

PROCEDURE p1( READONLY a: OAC ) =
  BEGIN
    FOR i := 0 TO LAST(a) DO
    END;
  END p1;

PROCEDURE p2( READONLY a: OAOAC ) =
  BEGIN
    FOR i := 0 TO LAST(a) DO
      FOR j := 0 TO LAST(a[i]) DO
      END;
    END;
  END p2;

VAR
  ac : ARRAY [0..2] OF CHAR := OAC {'a', 'b', 'c'};
  ad : ARRAY [0..2] OF CHAR := ARRAY OF CHAR {'d', 'e', 'f'};
  ae := ARRAY [0..2] OF CHAR {'g', 'h', 'i'};
CONST
  af = ARRAY [1..3] OF CHAR {'g', 'h', 'i'};
  ag = ARRAY [1..4] OF CHAR {'g', 'h', 'i', ..};
  ah = OAC {'a', 'b', 'c'};
  ai = ARRAY OF CHAR {'d', 'e', 'f'};
VAR
  aj : ARRAY [0..2] OF CHAR := OAC {'d', 'e', 'f'};
  ak : ARRAY [-1..1] OF CHAR := ARRAY OF CHAR {'d', 'e', 'f'};
BEGIN

  ac := ac;
  ac := ad;
  ac := ae;
  ac := af;
  (*ac := ag;*)
  ac := ah;
  ac := ai;

  ad := ac;
  ad := ad;
  ad := ae;
  ad := af;
  (*ad := ag;*)
  ad := ah;
  ad := ai;

  ae := ac;
  ae := ad;
  ae := ae;
  ae := af;
  (*ae := ag;*)
  ae := ah;
  ae := ai;

  aj := ac;
  aj := ad;
  aj := ae;
  aj := af;
  (*aj := ag;*)
  aj := ah;
  aj := ai;
  aj := aj;
  aj := ak;

  ak := ac;
  ak := ad;
  ak := ae;
  ak := af;
  (*ak := ag;*)
  ak := ah;
  ak := ai;
  ak := aj;
  ak := ak;

  p1( OAC{'j', 'k', 'l', 'm'} );
  p1( ag );
  p2( OAOAC{ac, ad} );
  p2( OAOAC{ac, ai} );
  IO.Put("OK\n");

END Main.

(*
  An open array type can be used only as the type of a formal parameter,
  the referent of a reference type, the element type of another open
  array type, or as the type in an array constructor.
*)
