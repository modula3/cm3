INTERFACE IDGen;


TYPE
  T = OBJECT METHODS
    alloc(force := -1): ID; (* if "force>=0" then must return "force". *)
    free(id: ID);
  END;

  ID = CARDINAL;

    
(* "Low"

   Use a "Low" to allocate the lowest non-negative ID
   that is not *currently* in use (unless "force" is used).

   Memory consumption is at most proportional to the number of IDs currently
   in use, and delay is often sublinear (at least as good as "Region.T").
*)

  Low <: LowPublic;
  LowPublic = T OBJECT
  METHODS
    init(): Low;
  END;

  

(* "Unique"

   A "unique" never returns the same ID twice (unless "force" is used)

*)

  Unique <: UniquePublic;
  UniquePublic = T OBJECT
  METHODS
    init(): Unique;
  END;


END IDGen.
