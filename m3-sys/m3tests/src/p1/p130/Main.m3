(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Mon Oct 26 10:07:46 PST 1992 by kalsow *)
(*      modified on Mon Aug  6 20:16:41 PDT 1990 by ellis  *)
(*      modified on Tue Apr 17 19:36:39 1990 by saxe       *)

MODULE Main;

(* Verify the sad truth about unchecked conversion between INTEGER
   and UNSIGNED *)

TYPE Short = BITS 8 FOR [0..255];

VAR
  i: INTEGER;
  u, v: UNSIGNED;
  j, k: Short;
  m: BITS 32 FOR ARRAY [0..3] OF Short;

BEGIN

  u := LAST(UNSIGNED);
  i := u;
  ASSERT(i < LAST(INTEGER));

  i := FIRST(INTEGER);
  u := i;
  ASSERT(u > FIRST(UNSIGNED));

  i := 3000000000;
  ASSERT(i < 0);
  u := - 10;
  ASSERT(u > 0);

  j := u;
  k := j;
  m[0] := k;
  ASSERT(m[0] # k);
  m[1] := 0;
  m[3] := k;
  m[2] := k;
  ASSERT(m[0] = m[2]);
  ASSERT(m[1] = 0);
  ASSERT(m[3] = m[0]);
  ASSERT(0 <= m[3]);
  ASSERT(m[3] <= 255);
  v := m[3];
  ASSERT(0 <= v);
  ASSERT(v <= 255);
  ASSERT(u > 255);
  ASSERT(v # u);
  ASSERT(u = k);

END Main.
