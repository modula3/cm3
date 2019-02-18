(* $Id$ *)

GENERIC INTERFACE PersistentTable(Elem, IntElemTbl);
IMPORT Pathname;
IMPORT OSError, Rd, Wr;
IMPORT Pickle;

EXCEPTION Error(TEXT);

(* Elem.T must be a TRACED reference type (something that can be Pickled) *)

TYPE
  T <: Public;

  Mode = { ExistingOnly, ExistingOrCreate, Replace };

  Public = IntElemTbl.T OBJECT METHODS
    init(path : Pathname.T; 
         mode := Mode.ExistingOrCreate;
         initSize : [1..LAST(CARDINAL)] := 1000) : T RAISES { OSError.E, 
                                                              Rd.Failure,
                                                              Wr.Failure,
                                                              Error };

    getE(READONLY key : INTEGER; VAR val : Elem.T) : BOOLEAN 
      RAISES { Rd.Failure, Rd.EndOfFile, Pickle.Error, Wr.Failure, OSError.E };

    putE(READONLY key : INTEGER; READONLY val : Elem.T) : BOOLEAN 
      RAISES { Rd.Failure, Rd.EndOfFile, Wr.Failure, Pickle.Error, Error, OSError.E };

    deleteE(READONLY key : INTEGER; VAR val : Elem.T) : BOOLEAN 
      RAISES { Rd.Failure, Rd.EndOfFile, Wr.Failure, Pickle.Error, Error, OSError.E };

    (* the standard, inherited put and get work like these but abort
       in case of difficulty *)

    iterateE() : Iterator;
    (* this is actually exactly the same as the normal iterate; both
       return an Iterator, but this one is more strongly typed *)
    
    keyEqual(READONLY a, b : INTEGER) : BOOLEAN;

    close() RAISES { Rd.Failure, Wr.Failure };
    (* you don't---STRICTLY SPEAKING---need this on Unix *)
  END;

TYPE 
  Iterator <: PubIterator;

  PubIterator = IntElemTbl.Iterator OBJECT METHODS
    nextE(VAR k : INTEGER; VAR r : Elem.T) : BOOLEAN RAISES { Rd.Failure, Rd.EndOfFile, Pickle.Error, OSError.E, Wr.Failure };
  END;

TYPE Default = T; (* some clients need this so names match with std impl *)

CONST Brand = "ObjectStore";

END PersistentTable.
