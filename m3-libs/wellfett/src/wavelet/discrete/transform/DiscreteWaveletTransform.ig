
GENERIC INTERFACE DiscreteWaveletTransform(S, VS, MS);

IMPORT NADefinitions AS NA;

CONST Brand = "DiscreteWaveletTransform";

PROCEDURE FilterBankToPolyphase (READONLY x: VS.TBody; ): MS.T;
PROCEDURE PolyphaseToFilterBank (READONLY x: MS.TBody; ): VS.T
  RAISES {NA.Error};

PROCEDURE FilterBankTransform (x: S.T; READONLY y: VS.TBody; ): VS.T;
(*Transform signal x into NUMBER(y) downsampled channels*)

END DiscreteWaveletTransform.
