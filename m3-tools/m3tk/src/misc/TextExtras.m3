MODULE TextExtras;

(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)

IMPORT TextF;
FROM TextF IMPORT New;
FROM Text IMPORT Length, Sub;
IMPORT ASCII;

PROCEDURE Compare(t, u: T): INTEGER RAISES {} =
  VAR
    minLength := Length(t);
    otherLength := Length(u);
    lengthDiff: INTEGER := minLength - otherLength;
    i: CARDINAL := 0;
  BEGIN
    IF lengthDiff > 0 THEN minLength := otherLength END;
    WHILE i < minLength DO
      VAR ti := ORD(t[i]); ui := ORD(u[i]); (* workaround compiler bug *)
      BEGIN                                 (* on byte subtractions *)
        WITH diff = ti - ui DO
          IF diff # 0 THEN RETURN diff ELSE INC(i) END;
        END;
      END;
    END;
    RETURN lengthDiff;
  END Compare;


PROCEDURE CICompare(t, u: T): INTEGER RAISES {} =
  VAR
    minLength := Length(t);
    otherLength := Length(u);
    lengthDiff := minLength - otherLength;
    i: CARDINAL := 0;
  BEGIN
    IF lengthDiff > 0 THEN minLength := otherLength END;
    WHILE i < minLength DO
      WITH diff = ORD(ASCII.Upper[t[i]]) - ORD(ASCII.Upper[u[i]]) DO
        IF diff # 0 THEN RETURN diff ELSE INC(i) END;
      END;
    END;
    RETURN lengthDiff;
  END CICompare;


PROCEDURE CIEqual(t, u: T): BOOLEAN RAISES {} =
  VAR
    lt: CARDINAL := Length(t);
    lu: CARDINAL := Length(u);
    i: CARDINAL := 0;
  BEGIN
    IF lt = lu THEN 
      WHILE i<lt DO
        IF ASCII.Upper[t[i]] # ASCII.Upper[u[i]] THEN 
          RETURN FALSE 
        ELSE INC(i) 
        END;
      END;
      RETURN TRUE;
    ELSE RETURN FALSE
    END;
  END CIEqual;

EXCEPTION BadFind;

PROCEDURE FindChar(t: T; ch: CHAR; VAR index: CARDINAL): BOOLEAN RAISES {} =
  VAR
    i: CARDINAL := index;
    lt: CARDINAL := Length(t);
  BEGIN
    IF i >= lt THEN
      IF i = lt THEN RETURN FALSE ELSE 
        <*FATAL BadFind *> BEGIN RAISE BadFind END;
      END;
    END;
    REPEAT
      IF t[i] = ch THEN index := i; RETURN TRUE END;
      INC(i);
    UNTIL i = lt;
    index := i;
    RETURN FALSE;
  END FindChar;


PROCEDURE FindCharSet(
    t: T;
    READONLY charSet: ASCII.Set;
    VAR index: CARDINAL)
    : BOOLEAN
    RAISES {} =
  VAR
    i: CARDINAL := index;
    lt: CARDINAL := Length(t);
  BEGIN
    IF i >= lt THEN
      IF i = lt THEN RETURN FALSE ELSE
        <*FATAL BadFind*> BEGIN RAISE BadFind END;
      END
    END;
    REPEAT
      IF t[i] IN charSet THEN index := i; RETURN TRUE END;
      INC(i);
    UNTIL i = lt;
    index := i;
    RETURN FALSE;
  END FindCharSet;


PROCEDURE FindSub(t, sub: T; VAR index: CARDINAL): BOOLEAN RAISES {} =
  VAR
    i: CARDINAL := index;
    lt: CARDINAL := Length(t);
    lsub: CARDINAL := Length(sub);
  BEGIN
    IF i > lt THEN <*FATAL BadFind*> BEGIN RAISE BadFind END; END;
    IF lsub = 0 THEN
      RETURN TRUE 
    ELSE
      IF lsub <= lt THEN
        VAR
          lastStart := lt - lsub;
          firstCh := sub[0];
        BEGIN
          WHILE i <= lastStart DO
            IF t[i] = firstCh THEN 
              VAR 
                j: CARDINAL := 1;
              BEGIN
                LOOP
                  IF j = lsub THEN
                    index := i;
                    RETURN TRUE;
                  ELSIF i + j >= lt OR t[i+j] # sub[j] THEN
                    EXIT
                  ELSE
                    INC(j);
                  END;
                END;
              END;
            END;
            INC(i);
          END;
        END;
      END;
      index := lt;
      RETURN FALSE;
    END;
  END FindSub;


<*INLINE*> PROCEDURE Extract(t: T; fx, tx: CARDINAL): T RAISES {} =
  BEGIN
    RETURN Sub(t, fx, tx-fx);
  END Extract;


EXCEPTION
  JoinFailed;

PROCEDURE Join(t1, t2, t3, t4, t5: T := NIL): T RAISES {}=
  VAR
    a := ARRAY [0..4] OF T {t1, t2, t3, t4, t5};
    pos := LAST(a);
  BEGIN
    LOOP
      IF a[pos] # NIL THEN
        RETURN JoinN(SUBARRAY(a, 0, pos + 1));
      ELSIF pos = 0 THEN
        <*FATAL JoinFailed*> BEGIN RAISE JoinFailed; END; 
      ELSE
        DEC(pos);
      END;
    END;
  END Join;


PROCEDURE JoinN(READONLY texts: ARRAY OF TEXT): T RAISES {}=
  VAR
    n := NUMBER(texts);
    result: T;
  BEGIN
    IF n = 0 THEN <*FATAL JoinFailed*> BEGIN RAISE JoinFailed END; END;

    VAR
      length := 0;
    BEGIN
      FOR i := 0 TO n - 1 DO INC(length, Length(texts[i])) END;
      result := New(length);
    END;

    VAR
      pos := 0;
    BEGIN
      FOR i := 0 TO n - 1 DO
        WITH t = texts[i], tl = Length(t) DO
          IF tl > 0 THEN
            SUBARRAY(result^, pos, tl) := SUBARRAY(t^, 0, tl);
            INC(pos, tl);
          END;
        END;
      END; (* for *)
    END;

    RETURN result;
  END JoinN;

CONST
    Multiplier  = -1664117991; 
        (* = LOOPHOLE( ROUND( .6125423371 * 2^32 ), INTEGER ) *)

PROCEDURE CIHash (t: T): INTEGER =
  VAR result := 0;
  BEGIN
    FOR i := 0 TO NUMBER (t^) - 1 DO
      result := result * Multiplier + ORD (ASCII.Upper[t [i]]);
    END;
    RETURN result;
  END CIHash;

BEGIN

END TextExtras.
