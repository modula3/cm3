(*--------------------------------------------------------------------------*)
INTERFACE FileStatus;
(*object type for file status information.*)

(*--------------------------------------------------------------------------*)
IMPORT Time;

(*--------------------------------------------------------------------------*)
CONST Brand = "FileStatus";

(*--------------------------------------------------------------------------*)
TYPE 
  T = BRANDED OBJECT 
    exists : BOOLEAN;
    isFile : BOOLEAN;
    isDir  : BOOLEAN;
    isTerm : BOOLEAN;
    readable : BOOLEAN;
    writeable : BOOLEAN;
    traversable : BOOLEAN;
    modified : Time.T;
    size   : LONGINT;
  METHODS
    repr() : TEXT := Repr;
  END;

(*--------------------------------------------------------------------------*)
PROCEDURE Repr(self : T) : TEXT;

(*--------------------------------------------------------------------------*)
END FileStatus.
