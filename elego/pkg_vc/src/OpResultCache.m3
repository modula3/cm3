(*---------------------------------------------------------------------------*)
MODULE OpResultCache;

IMPORT TextRefTbl, TextSeq, Process;

(*---------------------------------------------------------------------------*)
REVEAL 
  T = CacheIF BRANDED "OpResultCacheImpl 1.0" OBJECT
    table : TextRefTbl.T;
  METHODS
  OVERRIDES
    init := Init;
    contains := Contains;
    getText := GetText;
    putText := PutText;
    getSeq := GetSeq;
    putSeq := PutSeq;
  END;

(*---------------------------------------------------------------------------*)
PROCEDURE New() : T =
  BEGIN
    RETURN NEW(T).init();
  END New;

(*---------------------------------------------------------------------------*)
PROCEDURE Init(self : T) : T =
  BEGIN
    self.table := NEW(TextRefTbl.Default).init();
    RETURN self;
  END Init;

(*---------------------------------------------------------------------------*)
PROCEDURE Contains(self : T; op, arg : TEXT) : BOOLEAN =
  VAR res : REFANY;
  BEGIN
    RETURN self.table.get(op & "!" & arg, res);
  END Contains;

(*---------------------------------------------------------------------------*)
PROCEDURE GetText(self : T; op, arg : TEXT) : TEXT =
  VAR 
    res : TEXT := "";
    ref : REFANY;
  BEGIN
    IF self.table.get(op & "!" & arg, ref) THEN
      res := NARROW(ref, TEXT);
      RETURN res;
    ELSE
      <* NOWARN *> Process.Crash("OpResultCache.Get(" & 
                                 op & ", " & arg & ") failed");
    END;
  END GetText;

(*---------------------------------------------------------------------------*)
PROCEDURE PutText(self : T; op, arg, res : TEXT) =
  BEGIN
    EVAL self.table.put(op & "!" & arg, res);
  END PutText;

(*---------------------------------------------------------------------------*)
PROCEDURE GetSeq(self : T; op, arg : TEXT) : TextSeq.T =
  VAR 
    res : TextSeq.T;
    ref : REFANY;
  BEGIN
    IF self.table.get(op & "!" & arg, ref) THEN
      res := NARROW(ref, TextSeq.T);
      RETURN res;
    ELSE
      <* NOWARN *> Process.Crash("OpResultCache.Get(" & 
                                 op & ", " & arg & ") failed");
    END;
  END GetSeq;

(*---------------------------------------------------------------------------*)
PROCEDURE PutSeq(self : T; op, arg : TEXT; res : TextSeq.T) =
  BEGIN
    EVAL self.table.put(op & "!" & arg, res);
  END PutSeq;

(*---------------------------------------------------------------------------*)
BEGIN
END OpResultCache.
