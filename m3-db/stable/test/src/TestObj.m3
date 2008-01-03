(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Created by Carsten Weich                                    *)
(*                                                             *)
(* Last modified on Thu Jan 19 13:35:42 PST 1995 by kalsow     *)
(*      modified on Tue Sep 27 18:30:00 PDT 1994 by weich      *)

MODULE TestObj;

IMPORT Text, Log, Thread;

TYPE
  (* Internal State: We combine everything that can be
     compared using "=" in the "scalar" record *)
  Scalars = RECORD
              b, vb      : BOOLEAN     := FALSE;
              c, vc      : CHAR        := '\000';
              i, vi      : INTEGER     := 0;
              ic, vic    : CARDINAL    := 0;
              i32, vi32  : Int32       := 0;
              b8, vb8    : Byte8       := 0;
              s, vs      : [1 .. 5]    := 1;
              r, vr      : REAL        := 0.0;
              d, vd      : LONGREAL    := 0.0d0;
              x, vx      : EXTENDED    := 0.0x0;
              set, vset                := Set{};
              arr, varr                := Array{0, ..};
              enum, venum              := Enum.one;
              carr, vcarr: CharArray;
              parr, vparr          := PackedArray{0, ..};
              except     : INTEGER := 0;
            END;

REVEAL

  T = Public BRANDED OBJECT
        (* fields that can be compared with "=": *)
        scalar: Scalars;

        (* fields containing references *)
        t, vt          : TEXT      := "";
        rec, vrec                  := Record{0, NIL};
        refrec, vrefrec: RefRecord := NIL;
        refarr, vrefarr: RefArray  := NIL;
        recarr, vrecarr := ARRAY [1 .. 5] OF Record{Record{0, NIL}, ..};

      OVERRIDES
        bool      := Bool;
        vbool     := Vbool;
        char      := Char;
        vchar     := Vchar;
        txt       := Txt;
        vtxt      := Vtxt;
        enum      := DoEnum;
        venum     := Venum;
        set       := DoSet;
        vset      := Vset;
        int       := Int;
        vint      := Vint;
        real      := Real;
        vreal     := Vreal;
        recarray  := Recarray;
        vrecarray := Vrecarray;
        exception := Exception;
      END;

(* The "Equal" procedure compares two instances of "TestObj.T".  If all the
   values are equal (by dereferencing all the pointers) "TRUE" is
   returned. *)
PROCEDURE Equal (o1, o2: T): BOOLEAN =

  PROCEDURE TEqual (t1, t2: TEXT): BOOLEAN =
    BEGIN
      IF (t1 = NIL AND t2 # NIL) OR (t1 # NIL AND t2 = NIL) THEN
        RETURN FALSE
      END;
      RETURN t1 = NIL AND t2 = NIL OR Text.Equal(t1, t2);
    END TEqual;

  PROCEDURE RecEq (r1, r2: Record): BOOLEAN =
    BEGIN
      RETURN r1.i = r2.i AND TEqual(r1.t, r2.t);
    END RecEq;

  BEGIN
    (* first compare those that need a loop for comparison *)
    FOR i := FIRST(o1.recarr) TO LAST(o1.recarr) DO
      IF NOT RecEq(o1.recarr[i], o2.recarr[i]) THEN RETURN FALSE END
    END;
    FOR i := FIRST(o1.vrecarr) TO LAST(o1.vrecarr) DO
      IF NOT RecEq(o1.vrecarr[i], o2.vrecarr[i]) THEN RETURN FALSE END
    END;

    (* awkward pointer comparisons: *)
    IF (o1.refrec = NIL AND o2.refrec # NIL)
         OR (o1.refrec # NIL AND o2.refrec = NIL)
         OR (o1.vrefrec = NIL AND o2.vrefrec # NIL)
         OR (o1.vrefrec # NIL AND o2.vrefrec = NIL)
         OR (o1.refarr = NIL AND o2.refarr # NIL)
         OR (o1.refarr # NIL AND o2.refarr = NIL)
         OR (o1.vrefarr = NIL AND o2.vrefarr # NIL)
         OR (o1.vrefarr # NIL AND o2.vrefarr = NIL) THEN
      RETURN FALSE
    END;

    (* arrays of records and pointers seem to be ok, look at the rest: *)
    RETURN
      o1.scalar = o2.scalar AND TEqual(o1.t, o2.t) AND TEqual(o1.vt, o2.vt)
        AND RecEq(o1.rec, o2.rec) AND RecEq(o1.vrec, o2.vrec) AND
        (* both NIL *) ((o1.refrec = NIL AND o2.refrec = NIL)
                          OR RecEq(o1.refrec^, o2.refrec^)) AND
        (* both NIL *) ((o1.vrefrec = NIL AND o2.vrefrec = NIL)
                          OR RecEq(o1.vrefrec^, o2.vrefrec^)) AND
        (* both NIL *) ((o1.vrefrec = NIL AND o2.vrefrec = NIL)
                          OR RecEq(o1.vrefrec^, o2.vrefrec^)) AND
        (* both NIL *) (o1.refarr = NIL AND o2.refarr = NIL
                          OR o1.refarr^ = o2.refarr^
                               AND o1.vrefarr^ = o1.vrefarr^);
  END Equal;


(* Implementation of the test objects methods.  All methods just assing the
   value of their parameter(s) to fields of the state of the object.  There
   is one "CrashPoint" in the method "bool" (see "Main.m3"). *)

PROCEDURE Bool (self: T; b: BOOLEAN) =
  BEGIN
    Log.CrashPoint(403);
    self.scalar.b := b;
  END Bool;

PROCEDURE Vbool (self: T; VAR b: BOOLEAN) =
  BEGIN
    self.scalar.vb := b;
  END Vbool;

PROCEDURE Char (self: T; c: CHAR) =
  BEGIN
    self.scalar.c := c;
  END Char;

PROCEDURE Vchar (self: T; VAR c: CHAR) =
  BEGIN
    self.scalar.vc := c;
  END Vchar;

PROCEDURE Txt (self: T; t: TEXT) =
  BEGIN
    self.t := t;
  END Txt;

PROCEDURE Vtxt (self: T; VAR t: TEXT) =
  BEGIN
    self.vt := t;
  END Vtxt;

PROCEDURE DoEnum (self: T; e: Enum) =
  BEGIN
    self.scalar.enum := e;
  END DoEnum;

PROCEDURE Venum (self: T; VAR e: Enum) =
  BEGIN
    self.scalar.venum := e;
  END Venum;

PROCEDURE DoSet (self: T; s: Set) =
  BEGIN
    self.scalar.set := s;
  END DoSet;

PROCEDURE Vset (self: T; VAR s: Set) =
  BEGIN
    self.scalar.vset := s;
  END Vset;

PROCEDURE Int (self: T;
               i   : INTEGER;
               c   : CARDINAL;
               s   : [1 .. 5];
               i32 : Int32;
               b8  : Byte8     ) =
  BEGIN
    self.scalar.i := i;
    self.scalar.ic := c;
    self.scalar.s := s;
    self.scalar.i32 := i32;
    self.scalar.b8 := b8;
  END Int;

PROCEDURE Vint (    self: T;
                VAR i   : INTEGER;
                VAR c   : CARDINAL;
                VAR s   : [1 .. 5];
                VAR i32 : Int32;
                VAR b8  : Byte8     ) =
  BEGIN
    self.scalar.vi := i;
    self.scalar.vic := c;
    self.scalar.vs := s;
    self.scalar.vi32 := i32;
    self.scalar.vb8 := b8;
  END Vint;

PROCEDURE Real (self: T; r: REAL; d: LONGREAL; x: EXTENDED) =
  BEGIN
    self.scalar.r := r;
    self.scalar.d := d;
    self.scalar.x := x;
  END Real;

PROCEDURE Vreal (self: T;
               VAR r: REAL; VAR d: LONGREAL; VAR x: EXTENDED) =
  BEGIN
    self.scalar.vr := r;
    self.scalar.vd := d;
    self.scalar.vx := x;
  END Vreal;

PROCEDURE Recarray (self: T;
                    r   : Record;
                    READONLY a   : Array; 
                    rr  : RefRecord;
                 READONLY cr: CharArray;
                   READONLY pa  : PackedArray;
                    ra  : RefArray;
                    READONLY ar: ARRAY OF Record) =
  BEGIN
    self.rec := r;
    self.scalar.arr := a;
    self.refrec := NEW(RefRecord, i := rr.i, t := rr.t);
    self.scalar.carr:= cr;
    self.scalar.parr := pa;
    self.refarr := NEW(RefArray);
    self.refarr^ := ra^;
    self.recarr := ar;
  END Recarray;

PROCEDURE Vrecarray (    self: T;
                     VAR r   : Record;
                     VAR a   : Array;
                     VAR rr  : RefRecord;
                     VAR ca  : CharArray;
                     VAR pa  : PackedArray;
                     VAR ra  : RefArray;
                     VAR ar  : ARRAY OF Record) =
  BEGIN
    self.vrec := r;
    self.scalar.varr := a;
    self.vrefrec := NEW(RefRecord, i := rr.i, t := rr.t);
    self.scalar.vcarr := ca;
    self.scalar.vparr := pa;
    self.vrefarr := NEW(RefArray);
    self.vrefarr^ := ra^;
    self.vrecarr := ar;
  END Vrecarray;

PROCEDURE Exception (self: T): 
  INTEGER RAISES {Thread.Alerted, Error} =
  BEGIN
    INC(self.scalar.except);
    RETURN 0;
  END Exception;

(* Test modifications: set the object such that it equals the test object.
   "Modify1()" will set the passed object to values equal to the state of
   "test1".  "Modify2()" will set to "test2".  "Equal1()" returns "TRUE" if
   the passed object has the same values as "test1"; "Equal2()" checks for
   values of "test2".

   All values of "test1" and "test2" are chosen such that they all change
   if an object of type "T" is first statically initialized, then set to
   "test1" finally set to "test2". *)

VAR
  test1 := NEW(
             T,
             scalar :=
               Scalars{
                 b := TRUE, vb := TRUE, c := '1', vc := '2', i := -1, vi :=
                 -11, ic := 1, vic := 11, i32 := -32, vi32 := -322, b8 :=
                 8, vb8 := 88, s := 2, vs := 3, r := 1.0, vr := 11.0, d :=
                 2.0d0, vd := 22.0d0, x := 3.0x0, vx := 33.0x0,
                 enum:= Enum.one, venum:= Enum.two,
                 set:= Set{1}, vset:= Set{1,2},
                 arr :=Array{101, 102, 103, 104, 105, 106, 107, 108, 109, 110},
                 varr := Array{1101, 1102, 1103, 1104, 1105, 1106, 1107,
                               1108, 1109, 1110},
                 carr := CharArray {'a', 'b', 'c', 'd', 'e', 'f', 'g', ..},
                 vcarr := CharArray {'A', 'B', 'C', 'D', 'E', 'F', 'G', ..},
                 parr :=
                 PackedArray{11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
                             22, 23, 24, 25, 26}, vparr :=
                 PackedArray{111, 112, 113, 114, 115, 116, 117, 118, 119,
                             120, 121, 122, 123, 124, 125, 126},
                 except:= 1},
             t := "1",
             vt := "11", rec := Record{4, "4"}, vrec := Record{44, "44"},
             refrec := NEW(RefRecord, i := 5, t := "5"),
             vrefrec := NEW(RefRecord, i := 55, t := "55"), refarr := NIL,
             vrefarr := NIL,
             recarr := ARRAY [1 .. 5] OF
                         Record{Record{100, "100"}, Record{101, "101"},
                                Record{102, "102"}, Record{103, "103"},
                                Record{104, "104"}},
             vrecarr :=
               ARRAY [1 .. 5] OF
                 Record{Record{1000, "1000"}, Record{1001, "1001"},
                        Record{1002, "1002"}, Record{1003, "1003"},
                        Record{1004, "1004"}});
  test2 := NEW(
             T,
             scalar :=
               Scalars{
                 b := FALSE, vb := FALSE, c := '3', vc := '4', i := -2,
                 vi := -22, ic := 2, vic := 22, i32 := -42, vi32 := -422,
                 b8 := 9, vb8 := 99, s := 3, vs := 4, r := 2.0, vr := 22.0,
                 d := 3.0d0, vd := 33.0d0, x := 4.0x0, vx := 44.0x0,
                 enum:= Enum.three, venum:= Enum.one,
                 set:= Set{3}, vset:= Set{4,5},
                 arr := Array{201, 202, 203, 204, 205, 206, 207, 208, 209, 210},
                 varr := Array{2201, 2202, 2203, 2204, 2205, 2206, 2207,
                               2208, 2209, 2210}, 
                 carr := CharArray {'z', 'y', 'x', 'w', 'v', 'u', 'q', ..},
                 vcarr := CharArray {'Z', 'Y', 'X', 'W', 'V', 'U', 'Q', ..},
                 parr := PackedArray{21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
                             32, 33, 34, 35, 36}, vparr :=
                 PackedArray{211, 212, 213, 214, 215, 216, 217, 218, 219,
                             220, 221, 222, 223, 224, 225, 226},
                 except:= 2},
             t := "2",
             vt := "22", rec := Record{5, "5"}, vrec := Record{55, "55"},
             refrec := NEW(RefRecord, i := 6, t := "6"),
             vrefrec := NEW(RefRecord, i := 66, t := "66"), refarr := NIL,
             vrefarr := NIL,
             recarr := ARRAY [1 .. 5] OF
                         Record{Record{200, "200"}, Record{201, "201"},
                                Record{202, "202"}, Record{203, "203"},
                                Record{204, "204"}},
             vrecarr :=
               ARRAY [1 .. 5] OF
                 Record{Record{2000, "2000"}, Record{2001, "2001"},
                        Record{2002, "2002"}, Record{2003, "2003"},
                        Record{2004, "2004"}});

PROCEDURE Modify1 (o: T) =
  BEGIN
    o.bool(test1.scalar.b);
    o.vbool(test1.scalar.vb);
    o.char(test1.scalar.c);
    o.vchar(test1.scalar.vc);
    o.txt(test1.t);
    o.vtxt(test1.vt);
    o.enum(test1.scalar.enum);
    o.venum(test1.scalar.venum);
    o.set(test1.scalar.set);
    o.vset(test1.scalar.vset);
    o.int(test1.scalar.i, test1.scalar.ic, test1.scalar.s,
          test1.scalar.i32, test1.scalar.b8);
    o.vint(test1.scalar.vi, test1.scalar.vic, test1.scalar.vs,
           test1.scalar.vi32, test1.scalar.vb8);
    o.real(test1.scalar.r, test1.scalar.d, test1.scalar.x);
    o.vreal(test1.scalar.vr, test1.scalar.vd, test1.scalar.vx);
    o.recarray(test1.rec, test1.scalar.arr, test1.refrec,
               test1.scalar.carr, test1.scalar.parr, test1.refarr, test1.recarr);
    o.vrecarray(test1.vrec, test1.scalar.varr, test1.vrefrec,
                test1.scalar.vcarr, test1.scalar.vparr, test1.vrefarr, test1.vrecarr);
    TRY EVAL o.exception() EXCEPT ELSE END;
  END Modify1;

PROCEDURE Modify2 (o: T) =
  BEGIN
    o.bool(test2.scalar.b);
    o.vbool(test2.scalar.vb);
    o.char(test2.scalar.c);
    o.vchar(test2.scalar.vc);
    o.txt(test2.t);
    o.vtxt(test2.vt);
    o.enum(test2.scalar.enum);
    o.venum(test2.scalar.venum);
    o.set(test2.scalar.set);
    o.vset(test2.scalar.vset);
    o.int(test2.scalar.i, test2.scalar.ic, test2.scalar.s,
          test2.scalar.i32, test2.scalar.b8);
    o.vint(test2.scalar.vi, test2.scalar.vic, test2.scalar.vs,
           test2.scalar.vi32, test2.scalar.vb8);
    o.real(test2.scalar.r, test2.scalar.d, test2.scalar.x);
    o.vreal(test2.scalar.vr, test2.scalar.vd, test2.scalar.vx);
    o.recarray(test2.rec, test2.scalar.arr, test2.refrec,
               test2.scalar.carr, test2.scalar.parr, test2.refarr, test2.recarr);
    o.vrecarray(test2.vrec, test2.scalar.varr, test2.vrefrec,
                test2.scalar.vcarr, test2.scalar.vparr, test2.vrefarr, test2.vrecarr);
    TRY EVAL o.exception() EXCEPT ELSE END;
  END Modify2;

PROCEDURE Equal0 (o: T): BOOLEAN =
  BEGIN
    RETURN Equal(o, NEW(T));
  END Equal0;

PROCEDURE Equal1 (o: T): BOOLEAN =
  BEGIN
    RETURN Equal(o, test1);
  END Equal1;

PROCEDURE Equal2 (o: T): BOOLEAN =
  BEGIN
    RETURN Equal(o, test2);
  END Equal2;

BEGIN
  test1.refarr := NEW(RefArray);
  FOR i := FIRST(test1.refarr^) TO LAST(test1.refarr^) DO
    test1.refarr^[i] := i
  END;
  test1.vrefarr := NEW(RefArray);
  FOR i := FIRST(test1.vrefarr^) TO LAST(test1.vrefarr^) DO
    test1.vrefarr^[i] := i * 2
  END;
  test2.refarr := NEW(RefArray);
  FOR i := FIRST(test2.refarr^) TO LAST(test2.refarr^) DO
    test2.refarr^[i] := i * 3
  END;
  test2.vrefarr := NEW(RefArray);
  FOR i := FIRST(test2.vrefarr^) TO LAST(test2.vrefarr^) DO
    test2.vrefarr^[i] := i * 4
  END;
END TestObj.
