INTERFACE Signal;
(* author: thielema *)

IMPORT AtomList, Thread;

(* A signal should have values from -1 to 1.  Some processes can be
   simplified if they know that their input signals have this range. *)
TYPE
  Array = ARRAY OF LONGREAL;
  RefArray = REF Array;


(* Abstract class for signals and signal processes. *)
TYPE
  T = OBJECT
      METHODS
        get (): LONGREAL RAISES {End, Error, Thread.Alerted};
        (* Get the next sample or raises End if no further sample can be
           fetched. *)
        exit () := Exit;
        (* Finish reading of samples: Free resources. *)
      END;

PROCEDURE Exit (SELF: T; );


EXCEPTION
  End;                           (* end of the sound stream, no further
                                    sample can be fetched *)
  Error(ErrorRoot);              (* a sample could not be fetched because
                                    of an error, see the AtomList for error
                                    messages *)

TYPE
  ErrorRoot <: ErrorRootPublic;
  ErrorRootPublic =
    AtomList.T BRANDED OBJECT
    METHODS
      init (msg: TEXT := ""; oldErr: AtomList.T := NIL; ): ErrorRoot;
    END;


END Signal.
