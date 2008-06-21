(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

MODULE Decl;

IMPORT IntList, Text, Thread, Wr;
IMPORT BrowserDB, Buf, ErrLog, ID, M3MarkUp, M3Sym, Node, Pkg;
IMPORT RegExpr, Source, (**Type,**) Wx;

REVEAL
  T = Node.Named_T BRANDED "Decl.T" OBJECT
    kind      : Node.Class;
  OVERRIDES
    class     := Class;
    filename  := FileName;
    iterate   := Iterate;
    next      := Next;
    gen_page  := GenPage;
  END;

PROCEDURE Class (t: T): Node.Class =
  BEGIN
    RETURN t.kind;
  END Class;

PROCEDURE FileName (<*UNUSED*> t: T): TEXT =
  BEGIN
    RETURN NIL; (* assume we're being concatenated with the parent source *)
    (** RETURN t.parent.filename (); **)
  END FileName;

PROCEDURE Iterate (t: T;  VAR s: Node.IteratorState) =
  (* declaration nodes are fixed-points => they only return self. *)
  BEGIN
    s.d := t;
  END Iterate;

PROCEDURE Next (t: T;  VAR s: Node.IteratorState): BOOLEAN =
  BEGIN
    IF (s.d # NIL) THEN
      s.d := NIL;
      s.match := NIL;
      IF t.match (s.pattern) THEN s.match := t; END;
      RETURN (s.match # NIL);
    ELSE
      RETURN FALSE;
    END;
    (***
    s.match := s.d;  s.d := NIL;
    RETURN (s.match # NIL);
    ***)
  END Next;

PROCEDURE GenPage (t: T;  wx: Wx.T;  action: ID.T;  data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    Source.EmitPage (t.parent, wx, action, data, ID.ToText (t.name));
  END GenPage;

TYPE
  Scan = M3Sym.CallBack OBJECT
    results     : Node.Set;
    parent      : Source.T;
    parent_pkg  : Pkg.T;
    pattern     : RegExpr.T;
    find_one    : BOOLEAN;
    cur_file    : Source.T;
    unit_name   : ID.T;
    path        : TEXT;
    buf         : Buf.T;
    others      : OtherSym;
    second_try  : BOOLEAN;
    all_exports : BOOLEAN;
    is_interface: BOOLEAN;
    class_match : ARRAY Node.Class OF BOOLEAN;
  OVERRIDES
    note_sym := NoteSym;
  END;

  OtherSym = REF RECORD
    next : OtherSym;
    name : ID.T;
    kind : Source.Kind;
  END;

TYPE
  SKind = M3Sym.Kind;

CONST
  IgnoreSet = M3Sym.KindSet {
    SKind.TypeUse, SKind.ExceptUse, SKind.ProcUse, SKind.MiscUse,
    SKind.Keyword, SKind.BuiltinOp, SKind.BuiltinType, SKind.BuiltinConst
  };

CONST
  ProcClass = ARRAY BOOLEAN(*is_interface*) OF Node.Class {
                Node.Class.Proc, Node.Class.ProcDecl };


PROCEDURE FindNodes (parent: Source.T;  pattern: RegExpr.T): Node.Set =
  VAR s := NEW (Scan);  tag: TEXT;
  BEGIN
    s.parent       := parent;
    s.parent_pkg   := Pkg.Home (parent);
    s.pattern      := pattern;
    s.find_one     := RegExpr.SimpleString (pattern) # NIL;
    s.others       := NIL;
    s.all_exports  := FALSE;
    s.is_interface := FALSE;

    FOR c := FIRST (s.class_match) TO LAST (s.class_match) DO
      tag := Node.ClassTag[c];
      IF (tag # NIL) AND RegExpr.Match (s.pattern, tag)
        THEN  s.class_match [c] := TRUE;  s.find_one := FALSE;
        ELSE  s.class_match [c] := FALSE;
      END;
    END;

    IF RegExpr.Match (s.pattern, M3MarkUp.Intf_to_Impl_Mark) THEN
      FindExporters (s);
    END;

    IF RegExpr.Match (s.pattern, M3MarkUp.Impl_to_Intf_Mark) THEN
      s.all_exports := TRUE;
    END;

    ScanFile (s);

    RETURN s.results;
  END FindNodes;

PROCEDURE FindExporters (s: Scan) =
  VAR ids: IntList.T;  ref: REFANY;  nm: TEXT;
  BEGIN
    IF BrowserDB.db.exporters.get (s.parent.name, ref) THEN
      ids := ref;
      WHILE (ids # NIL) DO
        EVAL FindSource (s, ids.head, Source.Kind.Other (** M3 **));
        ids := ids.tail;
      END;
    END;
    IF (s.parent.kind = Source.Kind.IG) THEN
      (* we'll pretend that generic interfaces are "exported" by the
         corresponding generic modules -- usually they are. *)
      nm := ID.ToText (s.parent.name);
      nm := Text.Sub (nm, 0, Text.Length (nm) - Source.ExtLen[Source.Kind.IG]);
      EVAL FindSource (s, ID.Add (nm), Source.Kind.MG);
    END;
  END FindExporters;

PROCEDURE ScanFile (s: Scan) =
  VAR n_intfs, start: INTEGER;  intfs: Node.Array;
  BEGIN
    s.second_try := FALSE;
    s.cur_file := s.parent;
    s.path := Node.FullPath (s.parent);
    s.buf := Buf.FromFile (s.path, pad := 1);
    M3Sym.Scan (s.buf, s, IgnoreSet);

    IF ((NOT s.find_one) OR (s.results.cnt <= 0))
      AND (NOT s.all_exports) THEN
      (* we didn't find what we're looking for, try the exported
         interfaces for a matching symbol *)
      s.second_try := TRUE;
      start := s.results.cnt;
      WHILE (s.others # NIL) DO
        EVAL FindSource (s, s.others.name, s.others.kind);
        s.others := s.others.next;
      END;

      (* steal the results (== exported interfaces & used generics) *)
      n_intfs := s.results.cnt - start;
      intfs := NEW (Node.Array, n_intfs);
      FOR i := 0 TO n_intfs-1 DO
        WITH z = s.results.elts[i+start] DO
          intfs[i] := z;  z := NIL;
        END;
      END;
      s.results.cnt := start;

      (* finally, scan each of these interfaces *)
      FOR i := 0 TO n_intfs - 1 DO
        s.cur_file := intfs[i];
        s.path := Node.FullPath (s.cur_file);
        s.buf := Buf.FromFile (s.path, pad := 1);
        M3Sym.Scan (s.buf, s, IgnoreSet);
      END;
    END;
  END ScanFile;

PROCEDURE NoteSym (s: Scan;  READONLY sym: M3Sym.Id;
                   kind: SKind;  intf: TEXT): BOOLEAN =
  BEGIN
    CASE kind OF
    | SKind.IntfName =>   s.unit_name := SymID (s, sym);
                          s.is_interface := TRUE;
                          RETURN MatchSource (s, sym, Source.Kind.M3);

    | SKind.ImplName =>   s.unit_name := SymID (s, sym);
                          s.is_interface := FALSE;
                          RETURN MatchSource (s, sym, Source.Kind.I3);

    | SKind.GIntfName =>  s.unit_name := SymID (s, sym);
                          s.is_interface := TRUE;
                          RETURN MatchSource (s, sym, Source.Kind.MG);

    | SKind.GImplName =>  s.unit_name := SymID (s, sym);
                          s.is_interface := FALSE;
                          RETURN MatchSource (s, sym, Source.Kind.IG);

    | SKind.GFormal =>    RETURN MatchLocal  (s, sym, Node.Class.GFormal);
    | SKind.GActual =>    RETURN MatchSource (s, sym, Source.Kind.I3);
    | SKind.Import =>     RETURN MatchSource (s, sym, Source.Kind.I3);
    | SKind.FromImport => RETURN MatchSource (s, sym, Source.Kind.I3);
    | SKind.SymImport =>  RETURN MatchImport (s, sym, intf);
    | SKind.ImportXX =>   RETURN MatchSource (s, sym, Source.Kind.I3);
    | SKind.ImportAs =>   RETURN MatchRename (s, sym, intf);
    | SKind.ConstDecl =>  RETURN MatchLocal (s, sym, Node.Class.Const);
    | SKind.VarDecl =>    RETURN MatchLocal (s, sym, Node.Class.Var);
    | SKind.ExceptDecl => RETURN MatchLocal (s, sym, Node.Class.Except);
    | SKind.ProcDecl =>   RETURN MatchLocal (s, sym, ProcClass [s.is_interface]);
    | SKind.TypeDecl =>   (** RETURN MatchType (s, sym); **)
                          RETURN MatchLocal (s, sym, Node.Class.TypeDecl);

    | SKind.GIntfUse =>
        IF MatchSource (s, sym, Source.Kind.IG) THEN RETURN TRUE; END;
        Remember (s, sym, Source.Kind.IG);

    | SKind.GImplUse =>
        IF MatchSource (s, sym, Source.Kind.MG) THEN RETURN TRUE; END;
        Remember (s, sym, Source.Kind.MG);

    | SKind.Export =>
        IF (s.all_exports) THEN
          (* keep any export *)
          EVAL FindSource (s, SymID (s, sym), Source.Kind.I3);
        ELSE
          IF MatchSource (s, sym, Source.Kind.I3) THEN RETURN TRUE; END;
          IF NOT s.second_try THEN  Remember (s, sym, Source.Kind.I3); END;
        END;

    | SKind.TypeUse, SKind.ExceptUse, SKind.ProcUse, SKind.MiscUse, 
      SKind.Keyword, SKind.BuiltinOp, SKind.BuiltinType, SKind.BuiltinConst =>
        <*ASSERT FALSE*>
    END;
    RETURN FALSE;
  END NoteSym;

PROCEDURE Remember (s: Scan;  READONLY sym: M3Sym.Id;  kind: Source.Kind) =
  (* remember this source file in case we need to scan it too...  *)
  BEGIN
    s.others := NEW (OtherSym, next := s.others,
                     name := SymID (s, sym), kind := kind);
  END Remember;

PROCEDURE MatchLocal (s: Scan;  READONLY sym: M3Sym.Id;
                      kind: Node.Class): BOOLEAN =
  VAR id: ID.T;
  BEGIN
    IF Match (s, sym, kind, id) THEN
      Node.Append (s.results, NEW (T, parent := s.cur_file, name := id,
                                   kind := kind));
      RETURN s.find_one;
    END;
    RETURN FALSE;
  END MatchLocal;

(********
PROCEDURE MatchType (s: Scan;  READONLY sym: M3Sym.Id): BOOLEAN =
  VAR id: ID.T;  cnt: INTEGER;
  BEGIN
    IF Match (s, sym, Node.Class.TypeDecl, id) THEN
      cnt := FindType (s, id);
      RETURN s.find_one AND (cnt > 0);
    END;
    RETURN FALSE;
  END MatchType;

PROCEDURE FindType (s: Scan;  id: ID.T): INTEGER =
  VAR
    name : ID.T    := ID.Add (ID.ToText (s.unit_name) & "." & ID.ToText (id));
    home : ID.T    := s.cur_file.name;
    cnt  : INTEGER := 0;
    ref  : REFANY;
    nd   : Node.List;
    tipe : Type.T;
  BEGIN
    IF BrowserDB.db.type_names.get (name, ref) THEN
      nd := NARROW (ref, Node.List);
      WHILE (nd # NIL) DO
        tipe := nd.head;  nd := nd.tail;
        WHILE (tipe # NIL) DO
          IF (tipe.name = name) AND (tipe.home = home) THEN
            Node.Append (s.results, tipe);
            INC (cnt);
          END;
          tipe := tipe.alias;
        END;
      END;
    END;
    
    IF (cnt = 0) THEN
      (* we didn't find a matching type node => just point to the declaration *)
      Node.Append (s.results, NEW (T, parent := s.cur_file, name := id,
                                   kind := Node.Class.Type));
      INC (cnt);
    END;

    RETURN cnt;
  END FindType;
***********)

PROCEDURE MatchSource (s: Scan;  READONLY sym: M3Sym.Id;
                       kind: Source.Kind): BOOLEAN =
  VAR id: ID.T;
  BEGIN
    IF Match (s, sym, Source.NodeClass[kind], id)
      AND (FindSource (s, id, kind) > 0) THEN
      RETURN s.find_one;
    END;
    RETURN FALSE;
  END MatchSource;

PROCEDURE MatchImport (s: Scan;  READONLY sym: M3Sym.Id;  intf: TEXT): BOOLEAN =
  (* FROM <intf> IMPORT <sym> *)
  VAR
    id: ID.T;
    iter: Node.IteratorState;
    intf_cnt : INTEGER;
    match_cnt := 0;
    interface : Node.T;
    start, stop: INTEGER;
  BEGIN
    IF Match (s, sym, Node.Class.Unknown, id) THEN

      TRY
        iter.pattern := RegExpr.Compile (ID.ToText (id));
      EXCEPT RegExpr.Error (msg) =>
        ErrLog.Msg ("Bad regular expression:  Decl.MatchImport (\"",
                    ID.ToText (id), "\") => ", msg);
      END;

      intf_cnt := FindSource (s, ID.Add (intf), Source.Kind.I3);
      start := s.results.cnt - intf_cnt;
      stop := s.results.cnt - 1;

      FOR xx := start TO stop DO
        interface := s.results.elts[xx];
        IF (interface # NIL) THEN
          interface.iterate (iter);
          WHILE interface.next (iter) DO
            IF iter.match # NIL THEN
              Node.Append (s.results, iter.match);
              INC (match_cnt);
            END;
          END;
        END;
      END;

      (* remove the intermediate result nodes *)
      FOR xx := start TO stop DO
        DEC (s.results.cnt);
        s.results.elts[xx] := s.results.elts[s.results.cnt];
      END;

      RETURN s.find_one AND (match_cnt > 0);
    END;
    RETURN FALSE;
  END MatchImport;

PROCEDURE MatchRename (s: Scan;  READONLY sym: M3Sym.Id;  intf: TEXT): BOOLEAN =
  (* IMPORT <intf> AS <sym> *)
  VAR id: ID.T;
  BEGIN
    IF Match (s, sym, Node.Class.Interface, id)
      AND (FindSource (s, ID.Add (intf), Source.Kind.I3) > 0) THEN
      RETURN s.find_one;
    END;
    RETURN FALSE;
  END MatchRename;

PROCEDURE Match (s: Scan;  READONLY sym: M3Sym.Id;
                 class: Node.Class;  VAR(*OUT*)nm: ID.T): BOOLEAN =
  BEGIN
    WITH id = SUBARRAY (s.buf^, sym.start, sym.len) DO
      IF s.class_match[class] OR RegExpr.MatchSub (s.pattern, id)
        THEN nm := ID.FromStr (id);  RETURN TRUE;
        ELSE nm := ID.NoID;          RETURN FALSE;
      END;
    END;
  END Match;

PROCEDURE SymID (s: Scan;  READONLY sym: M3Sym.Id): ID.T =
  BEGIN
    RETURN ID.FromStr (SUBARRAY (s.buf^, sym.start, sym.len));
  END SymID;

PROCEDURE FindSource (s: Scan;  id: ID.T;  kind: Source.Kind): INTEGER =
  VAR
    nm := SourceName (id, kind);
    nd: Node.List;
    start := s.results.cnt;
    quality := 0;
    n: Node.T;
    qual: INTEGER;
    ref: REFANY;
  BEGIN
    IF BrowserDB.db.units.get (nm, ref) THEN
      nd := NARROW (ref, Node.List);
      WHILE (nd # NIL) DO
        n := nd.head;  nd := nd.tail;
        qual := MatchQuality (s, n, kind);
        IF qual > quality THEN
          (* discard any matches found so far *)
          s.results.cnt := start;
          quality := qual;
          Node.Append  (s.results, n);
        ELSIF (qual = quality) THEN
          Node.Append (s.results, n);
        END;
      END;
    END;
    RETURN s.results.cnt - start; (* # of matches *)
  END FindSource;

PROCEDURE SourceName (id: ID.T;  kind: Source.Kind): ID.T =
  VAR ext := Source.Ext [kind];  nm := id;
  BEGIN
    IF ext # NIL THEN  nm := ID.Add (ID.ToText (id) & ext);  END;
    RETURN nm;
  END SourceName;

PROCEDURE MatchQuality (s: Scan;  n: Node.Named_T;  kind: Source.Kind): INTEGER =
  (* Find the best match:  same package > same root > same kind > any source *)
  VAR pkg: Pkg.T;
  BEGIN
    TYPECASE n OF
    | Source.T (src) =>
        IF (src.kind # kind) AND (kind # Source.Kind.Other) THEN  RETURN -1; END;
        pkg := Pkg.Home (n);
        IF (pkg # NIL) AND (s.parent_pkg = pkg) THEN
          RETURN 2;
        ELSIF (s.parent_pkg # NIL) AND (pkg # NIL)
          AND (s.parent_pkg.parent = pkg.parent) THEN
          RETURN 1;
        ELSE
          RETURN 0;
        END;
    ELSE
        RETURN -1;
    END;
  END MatchQuality;

PROCEDURE Init () =
  BEGIN
  END Init;

BEGIN
END Decl.
