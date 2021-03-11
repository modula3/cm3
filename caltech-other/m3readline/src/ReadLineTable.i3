(* $Id$ *)

INTERFACE ReadLineTable;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(cols : CARDINAL;
         fmt : TEXT := NIL; (* in the format of Fmt.FN *)
         READONLY headings : ARRAY OF TEXT := ARRAY OF TEXT {}) : T;
    addRow(READONLY cols : ARRAY OF TEXT);
    addHline(of := '-');
  END;

CONST Brand = "ReadLineTable";

END ReadLineTable.
