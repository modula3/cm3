(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Created by Carsten Weich                                    *)
(*                                                             *)
(* Last modified on Thu Jan 19 13:35:19 PST 1995 by kalsow     *)
(*      modified on Tue Sep 27 18:30:00 PDT 1994 by weich      *)

<*PRAGMA STABLE*>

(* This interface describes the object used to test the stable object
    package.  The main goal in designing this testobject was to use
    all possible parameter data types. ``All possible'' means that all
    procedures from "StableLog.i3" have to be used. The methods of the
    test object pass their parameter to an invisible state variable of
    the same type.

    The "Equal0()" test returns "TRUE", if the passed object has the
    same values as an object just after static initialisations (i.e.\
    it equals an object returned by "NEW(TestObj.T)").

    There are two modification procedures "Modify1()" and "Modify2()"
    that assign a value to the object that is independent from it's
    previous state (to test parameterless procedures also, the method
    "exception()" is an exception to that rule: It increments a state
    variable of the test object).  This assignment is done by calling
    all of the methods of the testobject.  Two tests "Equal1()" and
    "Equal2() return "TRUE", if the passed object has the same value
    as it should have after the corresponding modification procedure.
    Because of the behabiour of the method "exception()" you have
    to call first "Modify1()" then Modify2()" in that order.

    This is the ``unstable'' version of the object. The stable subtype
    is in "StableTestObj.T"
*)

INTERFACE TestObj;

IMPORT Thread;
EXCEPTION Error;

CONST Brand = "TestObj";

TYPE
  Int32 = [-16_7FFFFFFF - 1 .. 16_7FFFFFFF];
  Byte8 = [0..255];
  Enum = {one, two, three};
  Set = SET OF [0 .. 127];
  Array = ARRAY [0 .. 9] OF INTEGER;
  RefArray = REF Array;
  CharArray = ARRAY [0 .. 255] OF CHAR;
  Record = RECORD
             i: INTEGER;
             t: TEXT
           END;
  RefRecord = REF Record;
  PackedArray = BITS 128 FOR ARRAY [0 .. 15] OF Byte8;

  T <: Public;

 <*STABLE UPDATE METHODS ANY*>

  Public = OBJECT METHODS
      bool  (b: BOOLEAN);
      vbool (VAR b: BOOLEAN);
      char  (c: CHAR);
      vchar (VAR c: CHAR);
      txt   (t: TEXT);
      vtxt  (VAR t: TEXT);
      enum  (e: Enum);
      venum (VAR e: Enum);
      set   (s: Set);
      vset  (VAR s: Set);
      int (i  : INTEGER;
           c  : CARDINAL;
           s  : [1 .. 5];
           i32: Int32;
           b8 : Byte8     );
      vint (VAR i  : INTEGER;
            VAR c  : CARDINAL;
            VAR s  : [1 .. 5];
            VAR i32: Int32;
            VAR b8 : Byte8     );
      real  (r: REAL; d: LONGREAL; e: EXTENDED);
      vreal (VAR r: REAL; VAR d: LONGREAL; VAR e: EXTENDED);
      recarray (         r : Record;
                READONLY a : Array;
                         rr: RefRecord;
                READONLY cr: CharArray;
                READONLY pa: PackedArray;
                         ra: RefArray;
                READONLY ar: ARRAY OF Record);
      vrecarray (VAR r : Record;
                 VAR a : Array;
                 VAR rr: RefRecord;
                 VAR cr: CharArray;
                 VAR pa: PackedArray;
                 VAR ra: RefArray;
                 VAR ar: ARRAY OF Record);
      exception(): INTEGER RAISES{Thread.Alerted, Error};
    END;

PROCEDURE Modify1(o: T);
PROCEDURE Modify2(o: T);
PROCEDURE Equal0(o: T): BOOLEAN;
PROCEDURE Equal1(o: T): BOOLEAN;
PROCEDURE Equal2(o: T): BOOLEAN;

END TestObj.





