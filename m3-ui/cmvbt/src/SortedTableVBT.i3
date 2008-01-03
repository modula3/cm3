
INTERFACE SortedTableVBT;
IMPORT TableVBT, VBT;

(* A "SortedTableVBT" is similar to a "TableVBT",
   except that it allows sorted input into
   the table according to a client-specified order. *)

TYPE
  T <: Public;
  Public = Private OBJECT 
    order: Order := DefaultOrder;
  METHODS
    insert_sorted (READONLY data: ARRAY OF VBT.T): CARDINAL;
  END;
  Private <: TableVBT.T;
  (* The call "insert_sorted" will insert an entry into
     the table sorted, according to "order". *)

TYPE
  Order      = PROCEDURE (v: T; READONLY v1: ARRAY OF VBT.T): INTEGER;

PROCEDURE DefaultOrder(v: T; READONLY data: ARRAY OF VBT.T): INTEGER;
(* Default order: insert on top. *)

END SortedTableVBT.
