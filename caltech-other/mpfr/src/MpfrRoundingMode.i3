INTERFACE MpfrRoundingMode;

TYPE T = {
  N,    (* round to nearest, with ties to even        *)
  Z,    (* round toward zero                          *)
  U,    (* round toward +Inf                          *)
  D,    (* round toward -Inf                          *)
  A,    (* round away from zero                       *)
  F,    (* faithful rounding                          *)
  NA    (* round to nearest, with ties away from zero *)
  };
  
CONST Brand = "MpfrRoundingMode";

END MpfrRoundingMode.
