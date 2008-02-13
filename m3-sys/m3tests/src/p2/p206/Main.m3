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
  ac := OAC {'a', 'b', 'c'};
  ad := ARRAY OF CHAR {'d', 'e', 'f'};
  ae := ARRAY [0..2] OF CHAR {'g', 'h', 'i'};
BEGIN
  ac := ad;
  ad := ae;
  p1( OAC{'j', 'k', 'l', 'm'} );
  p2( OAOAC{ac, ad} );
  IO.Put("OK\n");
END Main.

(*
  An open array type can be used only as the type of a formal parameter,
  the referent of a reference type, the element type of another open
  array type, or as the type in an array constructor.
*)
