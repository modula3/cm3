INTERFACE Range;
(* Arithmetic for Modula-3, see doc for details

   Abstract: Range of integers similar to the built-in subrange type of
   Modula-3.  In contrast to a Modula-3 subrange a Range.T may be
   initialised with numbers at run-time. *)


CONST Brand = "Range";


TYPE
  T = RECORD
        first : INTEGER;
        number: CARDINAL;
      END;

<* INLINE *>
PROCEDURE New (first: INTEGER; number: CARDINAL; ): T;

<* INLINE *>
PROCEDURE First (READONLY x: T; ): INTEGER;

<* INLINE *>
PROCEDURE Last (READONLY x: T; ): INTEGER;

<* INLINE *>
PROCEDURE Number (READONLY x: T; ): CARDINAL;



<* INLINE *>
PROCEDURE Add (READONLY x, y: T; ): T;

<* INLINE *>
PROCEDURE Scale (READONLY x: T; y: CARDINAL; ): T;

END Range.
