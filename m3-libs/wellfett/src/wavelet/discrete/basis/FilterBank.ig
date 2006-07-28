GENERIC INTERFACE FilterBank(S, SV, SM);

TYPE
  T = SV.T;
  TBody = SV.TBody;

PROCEDURE ToPolyphase (READONLY x: TBody; scaling: S.ScalingType; ): SM.T;
(* 'scaling' is the factor of sub-sampling which may differ from the number
   of channels, in that case the polyphase matrix is not square. *)

PROCEDURE FromPolyphase (READONLY x: SM.TBody; ): T;
(* scaling=NUMBER(x[0]) *)

END FilterBank.
