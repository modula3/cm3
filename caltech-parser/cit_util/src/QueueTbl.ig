GENERIC INTERFACE QueueTbl(Tbl);

TYPE
  T <: Public;
  Public = Tbl.Default OBJECT
  METHODS
    iterateQOrdered(): Tbl.Iterator;
    (* iterate in the order that the table was constructed.
       Not thread-safe.
    *)
  END;
  Default = T;

END QueueTbl.
