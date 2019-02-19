INTERFACE CPtrImpl;
IMPORT Ctypes,Word;

CONST
  Brand = "CPtrImpl";

TYPE
  T = Ctypes.void_star;

PROCEDURE Hash(a: T) : Word.T;
PROCEDURE Equal(a,b : T) : BOOLEAN;

END CPtrImpl.
