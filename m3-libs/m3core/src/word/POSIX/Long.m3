(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Mon Dec 16 11:02:01 PST 1991 by kalsow     *)
(*      modified on Sat May 19 07:28:29 1990 by muller         *)

MODULE Long;
IMPORT Long; (* let the compiler implement each of these as inlines *)

PROCEDURE Plus (x, y: T): T     = BEGIN RETURN Long.Plus (x, y)   END Plus;
PROCEDURE Times (x, y: T): T    = BEGIN RETURN Long.Times (x, y)  END Times;
PROCEDURE Minus (x, y: T): T    = BEGIN RETURN Long.Minus (x, y)  END Minus;
PROCEDURE Divide (x, y: T): T   = BEGIN RETURN Long.Divide (x, y) END Divide;
PROCEDURE Mod (x, y: T): T      = BEGIN RETURN Long.Mod (x, y)    END Mod;
PROCEDURE LT (x, y: T): BOOLEAN = BEGIN RETURN Long.LT (x, y)     END LT;
PROCEDURE LE (x, y: T): BOOLEAN = BEGIN RETURN Long.LE (x, y)     END LE;
PROCEDURE GT (x, y: T): BOOLEAN = BEGIN RETURN Long.GT (x, y)     END GT;
PROCEDURE GE (x, y: T): BOOLEAN = BEGIN RETURN Long.GE (x, y)     END GE;
PROCEDURE And (x, y: T): T      = BEGIN RETURN Long.And (x, y)    END And;
PROCEDURE Or (x, y: T): T       = BEGIN RETURN Long.Or (x, y)     END Or;
PROCEDURE Xor (x, y: T): T      = BEGIN RETURN Long.Xor (x, y)    END Xor;
PROCEDURE Not (x: T): T         = BEGIN RETURN Long.Not (x)       END Not;

PROCEDURE Shift (x: T; n: INTEGER): T
 = BEGIN RETURN Long.Shift (x, n) END Shift;

PROCEDURE LeftShift (x: T; n: [0..Size-1]): T
 = BEGIN RETURN Long.LeftShift (x, n) END LeftShift;

PROCEDURE RightShift (x: T; n: [0..Size-1]): T
 = BEGIN RETURN Long.RightShift (x, n) END RightShift;

PROCEDURE Rotate (x: T; n: INTEGER): T
 = BEGIN RETURN Long.Rotate (x, n) END Rotate;

PROCEDURE LeftRotate (x: T; n: [0..Size-1]): T
 = BEGIN RETURN Long.LeftRotate (x, n) END LeftRotate;

PROCEDURE RightRotate (x: T; n: [0..Size-1]): T
 = BEGIN RETURN Long.RightRotate (x, n) END RightRotate;
 
PROCEDURE Extract (x: T; i, n: CARDINAL): T
 = BEGIN RETURN Long.Extract (x, i, n) END Extract;

PROCEDURE Insert (x, y: T; i, n: CARDINAL): T
 = BEGIN RETURN Long.Insert (x, y, i, n) END Insert;

BEGIN
END Long.
