INTERFACE Integer32;
(*Copyright (c) 1996, m3na project

Abstract: Integer type for instantiation with IntegerBasic.ig

*)

(*==========================*)
TYPE
(*
  T = BITS 32 FOR [-16_80000000..16_7FFFFFFF];

-->  
"../src/na/integer/Integer32.i3", line 10: subrange lower bound is not constant
1 error encountered
*)
  T = BITS 32 FOR INTEGER;
(*
  T = INTEGER;
*)

(*==========================*)
END Integer32.
