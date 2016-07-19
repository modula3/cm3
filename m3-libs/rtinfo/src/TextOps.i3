INTERFACE TextOps;

IMPORT
  RefSeq;

(* Razne operacije nad TEXT-om, koje se ne nalaze u Text modulu
*)

PROCEDURE CAP(ch: CHAR): CHAR;
(* Vraca veliko slovo od slova 'ch'
*)

PROCEDURE LOW(ch: CHAR): CHAR;
(* Vraca malo slovo od slova 'ch'
*)

PROCEDURE Caps(t: TEXT): TEXT;
(* Sve slova iz t su pretovrena u velika slova
*)

PROCEDURE Lows(t: TEXT): TEXT;
(* Sve slova iz t su pretovrena u mala slova
*)

PROCEDURE FromChars(READONLY chars: ARRAY OF CHAR): TEXT;

PROCEDURE Escape(t: TEXT; s: SET OF CHAR; esc: CHAR := '\\'): TEXT;

PROCEDURE EscapeS(t, s: TEXT; esc: CHAR := '\\'): TEXT;

PROCEDURE UnEscape(t: TEXT; esc: CHAR := '\\'): TEXT;


(* Nalazi prvu (from=0) i <from>-tu poziciju <p>-a u <t>-u.
   Ako je <p>="" vraca 0. Ako nije nadjen vraca LAST(CARDINAL)
*)
PROCEDURE Pos(t, p: TEXT) : CARDINAL;

PROCEDURE NextPos(s, p: TEXT; from: CARDINAL) : CARDINAL;


PROCEDURE Item(t: TEXT; s: SET OF CHAR; n: CARDINAL; escape: CHAR := '\\'; skipSucc: BOOLEAN := TRUE): TEXT;

PROCEDURE ItemS(t: TEXT; s: TEXT; n: CARDINAL; escape: CHAR := '\\'; skipSucc: BOOLEAN := TRUE): TEXT;
(* Item i ItemS vracaju n-tu rec (n=0 za prvu rec). Reci
   su razdvojene delimiterima <s>. Ako je <skipSucc> = TRUE, onda se uzastopni
   delimiteri tretiraju kao jedan, tj. preskacu se.
*)

PROCEDURE Split(t: TEXT; s: SET OF CHAR; escape: CHAR := '\\'; skipSucc: BOOLEAN := TRUE): RefSeq.T;

PROCEDURE SplitS(t: TEXT; s: TEXT; escape: CHAR := '\\'; skipSucc: BOOLEAN := TRUE): RefSeq.T;
(* Split i SplitS vracaju listu tekstova. Tekstovi
   su razdvojene delimiterima <s>. Ako je <skipSucc> = TRUE, onda se uzastopni
   delimiteri tretiraju kao jedan, tj. preskacu se.
*)

PROCEDURE ToInt(t: TEXT; base: CARDINAL := 10; min: INTEGER := FIRST(INTEGER); max: INTEGER := LAST(INTEGER)): INTEGER;

PROCEDURE ToRReal(t: TEXT): REAL;

PROCEDURE ToReal(t: TEXT): LONGREAL;

PROCEDURE ToText(ra: REFANY): TEXT;

PROCEDURE ToBoolean(t: TEXT): BOOLEAN;

PROCEDURE RemoveSpaces(t: TEXT): TEXT;

PROCEDURE RemoveChars(t: TEXT; s: SET OF CHAR): TEXT;
(* Uklanja sve karaktere iz skupa <s> sa pocetka i kraja teksta <t>.
*)

PROCEDURE CompareWithNIL(t1, t2: TEXT): [-1..+1];


PROCEDURE F(fmt: TEXT; t1, t2, t3, t4, t5, t6, t7, t8, t9, t10: REFANY := NIL): TEXT;

PROCEDURE FN(fmt: TEXT; READONLY t: ARRAY OF REFANY): TEXT;
(* mask = % [-] [0] [<padding>] [.<maxwidth>] [:<precision|base>] ['"' <submask> '"'] 's'
   fmt  = <mask>|<text> [<fmt>]
*)

PROCEDURE Match(text, pattern: TEXT): BOOLEAN;

END TextOps.
