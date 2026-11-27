(* A record with packed fields keeps the alignment its other fields need:
   EXTENDED is 128-bit aligned on AMD64_LINUX, and the packed-field layout
   used to lower the record to word alignment, so the front end rejected
   stores to the EXTENDED field (#1404, c134). *)

UNSAFE MODULE Main;
IMPORT IO, Fmt, Word;

TYPE
  Int = BITS 3 FOR [0..7];
  R = RECORD
        a : CHAR := 'x';
        h : Int := 5;
        g : EXTENDED := 8.5x0;
        i : Int := 6;
        o : INTEGER := 99;
      END;

VAR
  global: R;
  arr: ARRAY [0..2] OF R;
  heap := NEW (REF R);

PROCEDURE Show (name: TEXT;  READONLY r: R) =
  (* Only an EXTENDED wider than LONGREAL (128 bits, on AMD64_LINUX) is
     aligned to its size.  Where EXTENDED is a LONGREAL, its alignment is
     the target's: 4 bytes in records on I386_LINUX. *)
  VAR aligned := BYTESIZE (EXTENDED) <= BYTESIZE (LONGREAL)
                 OR Word.Mod (LOOPHOLE (ADR (r.g), Word.T),
                              BYTESIZE (EXTENDED)) = 0;
  BEGIN
    IO.Put (name & ": " & Fmt.Char (r.a) & " " & Fmt.Int (r.h) & " "
            & Fmt.LongReal (FLOAT (r.g, LONGREAL)) & " " & Fmt.Int (r.i)
            & " " & Fmt.Int (r.o) & " aligned " & Fmt.Bool (aligned) & "\n");
  END Show;

PROCEDURE Local () =
  VAR l: R;
  BEGIN
    l.g := 2.25x0;  l.h := 7;
    Show ("local", l);
  END Local;

BEGIN
  Show ("global", global);
  arr[1].g := 1.5x0;  arr[2].i := 1;
  FOR k := 0 TO 2 DO Show ("arr", arr[k]) END;
  heap.g := 3.75x0;
  Show ("heap", heap^);
  Local ();
END Main.
