(*--------------------------------------------------------------------------*)
MODULE RsrcFilter;

IMPORT Bundle, Env, Pathname, Rd,  Rsrc, RefSeq, RefList, Text, Thread;
IMPORT TextUtils;

(*--------------------------------------------------------------------------*)
TYPE 
  SubstClosure = Closure OBJECT
    subst, replacement : TEXT;
  METHODS
    init(s, t : TEXT) : SubstClosure := SubstClosureInit;
  OVERRIDES
    apply := SubstClosureApply;
  END;

(*--------------------------------------------------------------------------*)
PROCEDURE SubstClosureInit(self : SubstClosure; s, t : TEXT) : SubstClosure =
  BEGIN
    self.subst := s;
    self.replacement := t;
    RETURN self;
  END SubstClosureInit;

(*--------------------------------------------------------------------------*)
PROCEDURE SubstClosureApply(self : SubstClosure; t : TEXT) : TEXT =
  BEGIN
    RETURN TextUtils.Substitute(t, self.subst, self.replacement, 0);
  END SubstClosureApply;

(*--------------------------------------------------------------------------*)
REVEAL
  T = Public BRANDED "RsrcFilter 0.0" OBJECT
    path   : Rsrc.Path;
    filter : RefSeq.T;
  METHODS
  OVERRIDES
    init := Init;
    addPath := AddPath;
    addFilter := AddFilter;
    addSubstFilter := AddSubstFilter;
    getPath := GetPath;
    getRsrc := GetRsrc;
    getRaw := GetRaw;
  END;


(*--------------------------------------------------------------------------*)
PROCEDURE Convert (a: REFANY): Rsrc.Path =
  BEGIN
    TYPECASE a OF
    | NULL => RETURN NIL
    | Bundle.T (b) => RETURN RefList.List1 (b)
    | TEXT (t) => RETURN ExpandPath (t)
    ELSE                         <* ASSERT FALSE *>
    END
  END Convert;

(*--------------------------------------------------------------------------*)
PROCEDURE ExpandPath (path: TEXT): RefList.T =
  BEGIN
    IF NOT Text.Empty (path) AND Text.GetChar (path, 0) = '$' THEN
      path := Env.Get (Text.Sub (path, 1, LAST (CARDINAL)))
    END;
    IF path = NIL OR Text.Empty (path) THEN
      RETURN NIL
    ELSIF Pathname.Valid (path) THEN
      RETURN RefList.List1 (path)
    ELSE                         <* ASSERT FALSE *>
    END
  END ExpandPath;

(*--------------------------------------------------------------------------*)
PROCEDURE Init(self : T; p1, p2, p3, p4, p5, p6 := NIL) : T =
  BEGIN (* Init *)
    self.filter := NEW(RefSeq.T).init();
    self.path := RefList.AppendD (
                     Convert (p1),
                     RefList.AppendD (
                         Convert (p2), 
                         RefList.AppendD (
                             Convert (p3), 
                             RefList.AppendD (
                                 Convert (p4), 
                                 RefList.AppendD (
                                     Convert (p5), Convert (p6))))));
    RETURN self;
  END Init;

(*--------------------------------------------------------------------------*)
PROCEDURE AddPath(self : T; p1, p2, p3, p4 : REFANY := NIL) : T =
  BEGIN
    self.path := RefList.AppendD (
                     self.path,
                     RefList.AppendD (
                         Convert (p1),
                         RefList.AppendD (
                             Convert (p2), 
                             RefList.AppendD (
                                 Convert (p3), Convert (p4)))));
    RETURN self;
  END AddPath;

(*--------------------------------------------------------------------------*)
PROCEDURE AddFilter(self : T; f : Closure) : T =
  BEGIN
    self.filter.addhi(f);
    RETURN self;
  END AddFilter;

(*--------------------------------------------------------------------------*)
PROCEDURE AddSubstFilter(self : T; s, t : TEXT) : T =
  BEGIN
    self.filter.addhi(NEW(SubstClosure).init(s, t));
    RETURN self;
  END AddSubstFilter;

(*--------------------------------------------------------------------------*)
PROCEDURE GetPath(self : T; ) : Rsrc.Path =
  BEGIN
    RETURN self.path;
  END GetPath;

(*--------------------------------------------------------------------------*)
PROCEDURE GetRsrc(self : T; name : TEXT) : TEXT 
  RAISES {Rsrc.NotFound, Rd.Failure, Thread.Alerted} =
  VAR t : TEXT;
  BEGIN
    t := Rsrc.Get(name, self.path);
    FOR i := 0 TO self.filter.size() - 1 DO
      WITH f = NARROW(self.filter.get(i), Closure) DO
        t := f.apply(t);
      END;
    END;
    RETURN t;
  END GetRsrc;

(*--------------------------------------------------------------------------*)
PROCEDURE GetRaw(self : T; name : TEXT) : TEXT 
  RAISES {Rsrc.NotFound, Rd.Failure, Thread.Alerted} =
  BEGIN
    RETURN Rsrc.Get(name, self.path);
  END GetRaw; 

BEGIN
END RsrcFilter.
