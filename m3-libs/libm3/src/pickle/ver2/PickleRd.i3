UNSAFE INTERFACE PickleRd;

IMPORT RTPacking, Pickle2 AS Pickle, ConvertPacking;

REVEAL
  Pickle.Reader <: Private;

TYPE
  Private = Pickle.ReaderPublic OBJECT
      packing: RTPacking.T;
      packingCode: INTEGER;
      conversion: ConvertPacking.Kind;
    END;

VAR myPacking: RTPacking.T;       (* our local packing. *)

END PickleRd.
