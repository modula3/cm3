(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 12:18:05 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:10 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

INTERFACE wraps;

PROCEDURE GetTransform ( ctxt: INTEGER; 
  VAR ctm, invctm: ARRAY [0..5] OF REAL;
  VAR xOffset, yOffset: INTEGER);

PROCEDURE FetchInteger ( ctxt: INTEGER;
 u: TEXT; alreadyLocked: BOOLEAN := FALSE ): INTEGER;
PROCEDURE FetchNumber ( ctxt: INTEGER;
 u: TEXT; alreadyLocked: BOOLEAN := FALSE ): REAL;
PROCEDURE FetchString ( ctxt: INTEGER;
 u: TEXT; alreadyLocked: BOOLEAN := FALSE ): TEXT;

PROCEDURE XYHit ( ctxt: INTEGER; 
  x, y: REAL;
  u: TEXT ): BOOLEAN;

PROCEDURE Stringwidth ( ctxt: INTEGER; 
  f: TEXT; s: TEXT;
  VAR x, y: REAL );

<*EXTERNAL*> PROCEDURE GetTransformWrap ( ctxt: INTEGER;
   ctm, invctm: UNTRACED REF ARRAY [0..5] OF REAL; 
   xOffset, yOffset: UNTRACED REF INTEGER );

<*EXTERNAL*> PROCEDURE FetchIntegerWrap ( ctxt: INTEGER;
   u: UNTRACED REF CHAR;
   r: UNTRACED REF INTEGER );

<*EXTERNAL*> PROCEDURE FetchNumberWrap ( ctxt: INTEGER;
   u: UNTRACED REF CHAR;
   r: UNTRACED REF REAL );

<*EXTERNAL*> PROCEDURE FetchStringWrap ( ctxt: INTEGER;
   u: UNTRACED REF CHAR;
   s: UNTRACED REF CHAR );

<*EXTERNAL*> PROCEDURE XYHitWrap ( ctxt: INTEGER;
   x, y: REAL; 
   u: UNTRACED REF CHAR;
   b: UNTRACED REF BOOLEAN );

<*EXTERNAL*> PROCEDURE StringwidthWrap ( ctxt: INTEGER;
   f, s: UNTRACED REF CHAR; 
   x, y: UNTRACED REF REAL );

  END wraps.


