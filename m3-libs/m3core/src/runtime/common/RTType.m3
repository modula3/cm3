(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Mon Jun  3 16:57:11 PDT 1996 by heydon     *)
(*      modified on Mon Jun  5 10:28:26 PDT 1995 by kalsow     *)
(*      modified on Fri May 28 14:54:41 PDT 1993 by muller     *)

UNSAFE MODULE RTType EXPORTS RTType, RTTypeSRC;

IMPORT RT0, RT0u, RTMisc, RTModule, RTHeapRep, M3toC;
IMPORT Ctypes, Cstdlib, Cstring, Word;
FROM RTIO IMPORT PutInt, PutString, PutText, PutAddr, PutHex, Flush;

TYPE
 TypePtr = UNTRACED REF RT0.TypeDefn;

(*------------------------------------------------ user callable routines ---*)

PROCEDURE MaxTypecode (): Typecode =
  BEGIN
    RETURN RT0u.nTypes - 1;
  END MaxTypecode;

PROCEDURE IsSubtype (a, b: Typecode): BOOLEAN =
  VAR t := Get (b);
  BEGIN
    IF (a >= RT0u.nTypes) THEN BadType (a) END;
    IF (a = 0)            THEN RETURN TRUE END;
    RETURN (t.typecode <= a AND a <= t.lastSubTypeTC);
  END IsSubtype;

PROCEDURE Supertype (tc: Typecode): Typecode =
  VAR t := Get (tc);
  BEGIN
    IF (t.parent = NIL)
      THEN RETURN NoSuchType;
      ELSE RETURN t.parent.typecode;
    END;
  END Supertype;

PROCEDURE IsTraced (tc: Typecode): BOOLEAN =
  VAR t := Get (tc);
  BEGIN
    RETURN t.traced # 0;
  END IsTraced;

PROCEDURE Get (tc: Typecode): RT0.TypeDefn =
  VAR p: TypePtr := RT0u.types + tc * ADRSIZE (RT0.TypeDefn);
  BEGIN
    IF (tc >= RT0u.nTypes) THEN BadType (tc) END;
    RETURN p^;
  END Get;

PROCEDURE GetNDimensions (tc: Typecode): CARDINAL =
  VAR t := Get (tc);
  BEGIN
    RETURN t.nDimensions;
  END GetNDimensions;

PROCEDURE TypeName (ref: REFANY): TEXT =
  VAR t := Get (TYPECODE (ref));
  BEGIN
    RETURN TypeDefnToName (t);
  END TypeName;

PROCEDURE TypecodeName (tc: Typecode): TEXT =
  VAR t := Get (tc);
  BEGIN
    RETURN TypeDefnToName (t);
  END TypecodeName;

PROCEDURE TypeDefnToName (t: RT0.TypeDefn): TEXT =
  BEGIN
    IF (t.name = NIL) THEN RETURN "<anon type>"; END;
    RETURN M3toC.CopyStoT (LOOPHOLE (t.name, Ctypes.char_star));
  END TypeDefnToName;

(*--------------------------------------------------- UID -> typecell map ---*)

TYPE
 IDMap = RECORD uid: INTEGER;  defn: RT0.TypeDefn END;

VAR
  (* map from type id to typecode, sorted by type id. *)
  n_type_ids : INTEGER;
  type_ids   : ADDRESS; (* REF ARRAY [0..n_type_ids-1] OF IDMap *)

PROCEDURE FindType (id: INTEGER): RT0.TypeDefn =
  VAR
    base : ADDRESS  := type_ids;
    lo   : CARDINAL := 0;
    hi   : CARDINAL := n_type_ids;
    mid  : CARDINAL; 
    p    : UNTRACED REF IDMap;
  BEGIN
    WHILE (lo < hi) DO
      mid := (lo + hi) DIV 2;
      p := base + mid * ADRSIZE (p^);
      IF (id < p.uid)
        THEN hi := mid;
        ELSE lo := mid + 1;
      END;
    END;
    IF (lo > 0) THEN DEC (lo) END;
    p := base + lo * ADRSIZE (p^);
    IF (p.uid # id) THEN RETURN NIL END;
    RETURN p.defn;
  END FindType;

(*-------------------------------------------------------- initialization ---*)

VAR
  init_done := FALSE;
  null  : RT0.TypeDefn;
  text  : RT0.TypeDefn;
  root  : RT0.TypeDefn;
  uroot : RT0.TypeDefn;

PROCEDURE Init () =
  BEGIN
    <* ASSERT NOT init_done *>
    init_done := TRUE;

    RegisterTypes ();
    CheckOpaques ();
    CheckBrands ();
    FindChildren ();
    CheckParents ();
    AssignTypecodes ();
    FixLinks ();
    FixSizes ();
    CallSetupProcs ();
    CheckRevelations ();
    RTHeapRep.CheckTypes ();
  END Init;

PROCEDURE RegisterTypes () =
  (* "register" each typecell with a distinct temporary typecode *)
  VAR
    mi  : RT0.ModulePtr;
    t   : RT0.TypeDefn;
    cnt, key : INTEGER;
    tp, x, y, z : TypePtr;
  BEGIN
    (* count the typecells *)
    cnt := 0;
    FOR i := 0 TO RT0u.nModules - 1 DO
      mi := RTModule.Get (i);
      t := mi.type_cells;
      WHILE (t # NIL) DO INC (cnt); t := t.next; END;
    END;

    (* allocate the space *)
    RT0u.nTypes      := cnt;
    RT0u.types       := Cstdlib.malloc (cnt * BYTESIZE (t));
    RT0u.alloc_cnts  := Cstdlib.malloc (cnt * BYTESIZE (INTEGER));
    RT0u.alloc_bytes := Cstdlib.malloc (cnt * BYTESIZE (INTEGER));

    (* initialize the allocation counts *)
    RTMisc.Zero (RT0u.alloc_cnts,  cnt * BYTESIZE (INTEGER));
    RTMisc.Zero (RT0u.alloc_bytes, cnt * BYTESIZE (INTEGER));

    (* collect pointers to all the typecells *)
    tp := RT0u.types;
    FOR i := 0 TO RT0u.nModules - 1 DO
      mi := RTModule.Get (i);
      t := mi.type_cells;
      WHILE (t # NIL) DO
        tp^ := t;  INC (tp, ADRSIZE (t));
        t := t.next;
      END;
    END;

    (* sort the cells by uid *)
    x := RT0u.types;
    FOR i := 1 TO cnt-1 DO
      tp := x + i * ADRSIZE (t);
      t := tp^;
      key := t.selfID;
      y := x + (i - 1) * ADRSIZE (t);
      WHILE (y >= x) AND (y^.selfID > key) DO
        z := y + ADRSIZE (t);
        z^ := y^;
        DEC (y, ADRSIZE (t));
      END;
      z := y + ADRSIZE (t);
      z^ := t;
    END;

    (* remove duplicates, but keep names *)
    cnt := 1;
    x := RT0u.types;
    y := x;
    FOR i := 1 TO RT0u.nTypes-1 DO
      INC (y, ADRSIZE (t));
      IF x^.selfID = y^.selfID THEN
        (* a duplicate, if we don't have one yet, save the name *)
        IF (x^.name = NIL) THEN x^.name := y^.name; END;
      ELSE (* a new typecell *)
        INC (cnt);
        INC (x, ADRSIZE (t));
        x^ := y^;
      END;
    END;
    RT0u.nTypes := cnt;
  END RegisterTypes;

PROCEDURE CheckOpaques () =
  (* build the UID->Defn maps including the opaque types *)
  VAR
    cnt : INTEGER;
    mi  : RT0.ModulePtr;
    t   : RT0.TypeDefn;
    r   : RT0.RevPtr;
    s, v: UNTRACED REF IDMap;
    tp  : TypePtr;
  BEGIN
    (* count the opaques *)
    cnt := RT0u.nTypes;
    FOR i := 0 TO RT0u.nModules - 1 DO
      mi := RTModule.Get (i);
      r := mi.full_rev;
      IF (r # NIL) THEN
        WHILE (r.lhs_id # 0) DO INC (cnt);  INC (r, ADRSIZE (r^)); END;
      END;
    END;

    (* allocate the space *)
    n_type_ids := cnt;
    type_ids   := Cstdlib.malloc (cnt * BYTESIZE (IDMap));

    (* initialize the map with the concrete typecells *)
    tp := RT0u.types;
    s  := type_ids;
    FOR i := 0 TO RT0u.nTypes-1 DO
      t := tp^;
      s.uid  := t.selfID;
      s.defn := t;
      INC (tp, ADRSIZE (tp^));
      INC (s, ADRSIZE (s^));
    END;
    n_type_ids := RT0u.nTypes;

    (* finally, add each of the opaque types *)
    FOR i := 0 TO RT0u.nModules - 1 DO
      mi := RTModule.Get (i);
      r := mi.full_rev;
      IF (r # NIL) THEN
        WHILE (r.lhs_id # 0) DO
          t := FindType (r.lhs_id);
          IF (t # NIL) THEN DuplicateLHS (mi, r, t) END;
          t := FindType (r.rhs_id);
          IF (t = NIL) THEN UndefinedRHS (mi, r) END;

          (* insert the new entry *)
          v := type_ids + n_type_ids * ADRSIZE (v^);
          s := v - ADRSIZE (v^);
          WHILE (s >= type_ids) AND (s.uid > r.lhs_id) DO
            v^ := s^;
            DEC (v, ADRSIZE (v^));
            DEC (s, ADRSIZE (s^));
          END;
          v.uid  := r.lhs_id;
          v.defn := t;
          INC (n_type_ids);

          INC (r, ADRSIZE (r^));
        END;
      END;
    END;
  END CheckOpaques;

PROCEDURE CheckBrands () =
  (* ensure that all brands are distinct *)
  VAR
    t, a, b : RT0.TypeDefn;
    tp      : TypePtr;
    hash    : INTEGER;
    buckets := ARRAY [0..292] OF RT0.TypeDefn {NIL, ..};
  BEGIN
    (* Hash each type with a non-nil brand into the table
       using the type's sibling pointer to resolve collisions. *)
    tp := RT0u.types;
    FOR i := 0 TO RT0u.nTypes-1 DO
      t := tp^;
      IF (t.brand # NIL) THEN
        hash := HashString (t.brand) MOD NUMBER (buckets);
        t.sibling := buckets[hash];
        buckets[hash] := t;
      END;
      INC (tp, ADRSIZE (tp^));
    END;

    (* Run the naive O(n^2) check on each hash bucket. *)
    FOR i := 0 TO LAST (buckets) DO
      a := buckets[i];
      WHILE (a # NIL) DO
        b := a.sibling;
        WHILE (b # NIL) DO
          IF Cstring.strcmp (LOOPHOLE(a.brand, Ctypes.char_star),
                             LOOPHOLE(b.brand, Ctypes.char_star)) = 0 THEN
            StartError ();
            PutText    ("Two types have the same brand: \"");
            PutString  (a.brand);
            PutText    ("\"\n***    ");
            PutType    (a);
            PutText    ("\n***    ");
            PutType    (b);
            EndError   ();
          END;
          b := b.sibling;
        END;
        a := a.sibling;
      END;
    END;

    (* Reset the sibling pointers. *)
    tp := RT0u.types;
    FOR i := 0 TO RT0u.nTypes-1 DO
      tp^.sibling := NIL;
      INC (tp, ADRSIZE (tp^));
    END;
  END CheckBrands;


PROCEDURE HashString (cp: UNTRACED REF CHAR): INTEGER =
  VAR hash := 0;
  BEGIN
    WHILE (cp^ # '\000') DO
      hash := Word.Plus (Word.LeftShift (hash, 1), ORD (cp^));
      INC (cp, BYTESIZE (cp^));
    END;
    RETURN hash;
  END HashString;

PROCEDURE FindChildren () =
  VAR tp: TypePtr;  t, p: RT0.TypeDefn;
  BEGIN
    tp := RT0u.types;
    FOR i := 0 TO RT0u.nTypes -1 DO
      t := tp^;
      IF (t.parentID # 0) THEN
        p := FindType (t.parentID);
        IF (p = NIL) THEN BadParent (t) END;
        t.parent := p;
        t.sibling := p.children;
        p.children := t;
      END;
      INC (tp, ADRSIZE (tp^));
    END;
  END FindChildren;

PROCEDURE CheckParents () =
  VAR tp: TypePtr;  t, u: RT0.TypeDefn;
  BEGIN
    tp := RT0u.types;
    FOR i := 0 TO RT0u.nTypes -1 DO
      t := tp^;  u := t;
      WHILE (u # NIL) AND (t # NIL) DO
        t := t.parent;
        u := u.parent;
        IF (u = NIL) THEN EXIT; END;
        u := u.parent;
        IF (t = u) THEN ParentCycle (tp^);  EXIT; END;
      END;
      INC (tp, ADRSIZE (tp^));
    END;
  END CheckParents;

PROCEDURE AssignTypecodes () =
  VAR
    tp, up        : TypePtr;
    t, u          : RT0.TypeDefn;
    next_typecode : INTEGER;
  BEGIN
    (* find the types with reserved typecodes *)
    null  := FindType (16_48ec756e);
    text  := FindType (16_50f86574);
    root  := FindType (16_ffffffff9d8fb489);
    uroot := FindType (16_ffffffff898ea789);

    (* reset the typecodes *)
    tp := RT0u.types;
    FOR i := 0 TO RT0u.nTypes-1 DO
      tp^.typecode := LAST (RT0.Typecode);
      INC (tp, ADRSIZE (tp^));
    END;

    (* assign the fixed typecodes *)
    null.typecode := RT0.NilTypecode;   null.lastSubTypeTC := RT0.NilTypecode;
    text.typecode := RT0.TextTypecode;  text.lastSubTypeTC := RT0.TextTypecode;
    next_typecode := MAX (RT0.NilTypecode, RT0.TextTypecode) + 1;

    (* assign the OBJECT typecodes *)
    AssignObjectTypecode (root, next_typecode);
    AssignObjectTypecode (uroot, next_typecode);

    (* assign the remaining REF typecodes *)
    tp := RT0u.types;
    FOR i := 0 TO RT0u.nTypes-1 DO
      t := tp^;
      IF (t.typecode = LAST (RT0.Typecode)) THEN
        t.typecode := next_typecode;
        t.lastSubTypeTC := next_typecode;
        INC (next_typecode);
      END;
      INC (tp, ADRSIZE (tp^));
    END;

    <* ASSERT next_typecode = RT0u.nTypes *>

    (* shuffle the typecells into their correct slots *)
    tp := RT0u.types;
    FOR i := 0 TO RT0u.nTypes-1 DO
      t := tp^;
      WHILE (t.typecode # i) DO
        up := RT0u.types + t.typecode * ADRSIZE (up^);
        u := up^;
        up^ := t;
        t := u;
      END;
      tp^ := t;
      INC (tp, ADRSIZE (tp^));
    END;
  END AssignTypecodes;

PROCEDURE AssignObjectTypecode (t: RT0.TypeDefn;  VAR next: INTEGER) =
  VAR u: RT0.TypeDefn;
  BEGIN
    <* ASSERT t.typecode = LAST (RT0.Typecode) *>
    t.typecode := next;  INC (next);
    u := t.children;
    WHILE (u # NIL) DO
      AssignObjectTypecode (u, next);
      u := u.sibling;
    END;
    t.lastSubTypeTC := next-1;
  END AssignObjectTypecode;

PROCEDURE FixLinks () =
  VAR
    mi   : RT0.ModulePtr;
    t, u : UNTRACED REF RT0.TypeLink;
    defn : RT0.TypeDefn;
  BEGIN
    FOR i := 0 TO RT0u.nModules - 1 DO
      mi := RTModule.Get (i);
      t := mi.type_cell_ptrs;
      WHILE (t # NIL) DO
        u := t.next;
        defn := FindType (t.type);
        IF (defn = NIL) THEN BadTypeId (mi, t.type) END;
        t.next := defn;
        t.type := defn.typecode;
        t := u;
      END;
    END;
  END FixLinks;

PROCEDURE FixSizes () =
  (* fix the data(method) sizes and offsets *)
  VAR t: RT0.TypeDefn;  tp: TypePtr;
  BEGIN
    (* make sure that all the REF types are some multiple of header words *)
    tp := RT0u.types;
    FOR i := 0 TO RT0u.nTypes-1 DO
      t := tp^;
      IF (t.typecode # RT0.NilTypecode)
        AND (t.parent = NIL)
        AND (t.children = NIL) THEN
        t.dataSize := RTMisc.Upper (t.dataSize, BYTESIZE (RTHeapRep.Header));
      END;
      INC (tp, ADRSIZE (tp^));
    END;

    (* fix the objects *)
    FixObjectSizes (root);
    FixObjectSizes (uroot);
  END FixSizes;

PROCEDURE FixObjectSizes (t: RT0.TypeDefn) =
  VAR u: RT0.TypeDefn;
  BEGIN
    (* fix my sizes *)
    u := t.parent;
    IF (u # NIL) THEN
      t.dataOffset := RTMisc.Upper (u.dataSize, t.dataAlignment);
      INC (t.dataSize, t.dataOffset);
      t.dataAlignment := MAX (t.dataAlignment, u.dataAlignment);
      t.methodOffset := u.methodSize;
      INC (t.methodSize, t.methodOffset);
    END;
    t.dataSize := RTMisc.Upper (t.dataSize, BYTESIZE (RTHeapRep.Header));

    (* allocate my default method list *)
    t.defaultMethods := Cstdlib.malloc (t.methodSize);
    IF (t.defaultMethods = NIL) THEN
      StartError ();
      PutText ("unable to allocate method suite for ");
      PutType (t);
      EndError ();
    END;
    RTMisc.Zero (t.defaultMethods,  t.methodSize);

    (* fix my children *)
    u := t.children;
    WHILE (u # NIL) DO
      FixObjectSizes (u);
      u := u.sibling;
    END;
  END FixObjectSizes;

PROCEDURE CallSetupProcs () =
  VAR t: RT0.TypeDefn;  tp: TypePtr;
  BEGIN
    (* set up the REF types *)
    tp := RT0u.types;
    FOR i := 0 TO RT0u.nTypes-1 DO
      t := tp^;
      IF (t.parent = NIL) AND (t.children = NIL) AND (t.linkProc # NIL) THEN
        t.linkProc (t);
      END;
      INC (tp, ADRSIZE (tp^));
    END;

    (* set up the objects *)
    SetupObject (root);
    SetupObject (uroot);
  END CallSetupProcs;

PROCEDURE SetupObject (t: RT0.TypeDefn) =
  VAR u: RT0.TypeDefn;  a: UNTRACED REF ADDRESS;
  BEGIN
    (* initialize my method suite from my parent *)
    u := t.parent;
    IF (u # NIL) THEN
      RTMisc.Copy (u.defaultMethods, t.defaultMethods, u.methodSize);
    END;
    LOOPHOLE (t.defaultMethods, UNTRACED REF INTEGER)^ := t.typecode;

    (* initialize any remaining methods to the undefined procedure *)
    a := t.defaultMethods + ADRSIZE (ADDRESS);
    FOR j := 1 TO t.methodSize DIV BYTESIZE (ADDRESS) - 1 DO
      IF (a^ = NIL) THEN a^ := LOOPHOLE (UndefinedMethod, ADDRESS) END;
      INC (a, ADRSIZE (ADDRESS));
    END;

    (* call my setup proc *)
    IF (t.linkProc # NIL) THEN t.linkProc (t) END;

    (* set up my children *)
    u := t.children;
    WHILE (u # NIL) DO
      SetupObject (u);
      u := u.sibling;
    END;
  END SetupObject;

PROCEDURE CheckRevelations () =
  VAR
    mi  : RT0.ModulePtr;
    r   : RT0.RevPtr;
    lhs : RT0.TypeDefn;
    rhs : RT0.TypeDefn;
  BEGIN
    FOR i := 0 TO RT0u.nModules - 1 DO
      mi := RTModule.Get (i);
      r := mi.partial_rev;
      IF (r # NIL) THEN
        WHILE (r.lhs_id # 0) DO
          lhs := FindType (r.lhs_id);
          rhs := FindType (r.rhs_id);
          IF (lhs = NIL) OR (rhs = NIL)
            OR (lhs.typecode < rhs.typecode)
            OR (rhs.lastSubTypeTC < lhs.typecode) THEN
            BadRevelation (mi, r, lhs, rhs);
          END;
          INC (r, ADRSIZE (r^));
        END;
      END;
    END;
  END CheckRevelations;

(*-------------------------------------------------------- runtime errors ---*)

PROCEDURE UndefinedMethod () =
  BEGIN
    RTMisc.FatalError (NIL, 0, "attempted invocation of undefined method");
  END UndefinedMethod;

PROCEDURE BadType (tc: Typecode) =
  BEGIN
    RTMisc.FatalErrorI ("improper typecode: ", tc);
  END BadType;

(*----------------------------------------------------------- init errors ---*)

PROCEDURE StartError () =
  BEGIN
    PutText ("\n\n***\n*** ");
  END StartError;

PROCEDURE EndError () =
  BEGIN
    PutText ("\n***");
    Flush ();
    RTMisc.FatalError (NIL, 0, "unable to initialize runtime types");
  END EndError;

PROCEDURE BadTypeId (mi: RT0.ModulePtr;  id: INTEGER) =
  BEGIN
    StartError ();
    PutText    ("unable to resolve type id: ");
    PutHex     (id);
    PutText    ("\n***    in ");
    PutModule  (mi);
    EndError   ();
  END BadTypeId;

PROCEDURE DuplicateLHS (mi: RT0.ModulePtr;  r: RT0.RevPtr;  t: RT0.TypeDefn) =
  BEGIN
    StartError ();
    PutText    ("opaque type redefined: ");
    PutText    ("\n***    REVEAL _t");
    PutHex     (r.lhs_id);
    PutText    (" = _t");
    PutHex     (r.rhs_id);
    PutText    ("\n***    in ");
    PutModule  (mi);
    PutText    ("\n***    but, already = ");
    PutType    (t);
    EndError   ();
  END DuplicateLHS;

PROCEDURE UndefinedRHS (mi: RT0.ModulePtr;  r: RT0.RevPtr) =
  BEGIN
    StartError ();
    PutText    ("opaque type revealed as undefined type: ");
    PutText    ("\n***    REVEAL _t");
    PutHex     (r.lhs_id);
    PutText    (" = _t");
    PutHex     (r.rhs_id);
    PutText    ("\n***    in ");
    PutModule  (mi);
    EndError   ();
  END UndefinedRHS;

PROCEDURE BadParent (t: RT0.TypeDefn) =
  BEGIN
    StartError ();
    PutText    ("super type undefined:\n***    child = ");
    PutType    (t);
    PutText    ("\n***    parent = _t");
    PutHex     (t.parentID);
    EndError   ();
  END BadParent;

PROCEDURE ParentCycle (t: RT0.TypeDefn) =
  VAR u: RT0.TypeDefn;
  BEGIN
    StartError ();
    PutText    ("illegal cycle in super types:\n***    child  = ");
    PutType    (t);
    u := t.parent;
    WHILE (u # NIL) DO
      PutText    ("\n***    parent = ");
      PutType    (u);
      IF (u = t) THEN EXIT; END;
      u := u.parent;
    END;
    EndError   ();
  END ParentCycle;

PROCEDURE BadRevelation (mi: RT0.ModulePtr;  r: RT0.RevPtr;
                         lhs, rhs: RT0.TypeDefn) =
  BEGIN
    StartError ();
    PutText    ("inconsistent partial revelation: ");
    PutText    ("\n***    REVEAL _t");
    PutHex     (r.lhs_id);
    PutText    (" <: _t");
    PutHex     (r.rhs_id);
    PutText    ("\n***           ");
    PutType    (lhs);
    PutText    (" <: ");
    PutType    (rhs);
    PutText    ("\n***    in ");
    PutModule  (mi);
    EndError   ();
  END BadRevelation;

(*---------------------------------------------------- internal debugging ---*)

(***********************************
PROCEDURE ShowTypes (full := TRUE) =
  VAR t: RT0.TypeDefn;
  BEGIN
    PutText ("Here are the types: nTypes = ");
    PutInt  (RT0u.nTypes);
    PutText ("\n");
    FOR i := 0 TO RT0u.nTypes-1 DO
      t := Get (i);
      WHILE (t # NIL) DO
        PutType (t); PutText ("\n");
        IF full THEN
          PutText ("  data   ");
          PutText ("  S= "); PutInt (t.dataSize);
          PutText ("  A= "); PutInt (t.dataAlignment);
          PutText ("  O= "); PutInt (t.dataOffset);
          PutText ("\n");
          IF (t.methodSize # 0) OR (t.methodOffset # 0) THEN
            PutText ("  method ");
            PutText ("  S= ");  PutInt (t.methodSize);
            PutText ("  O= ");  PutInt (t.methodOffset);
            PutText ("\n");
          END;
          IF (t.nDimensions # 0) OR (t.elementSize # 0) THEN
            PutText (" array   ");
            PutText ("  D= ");  PutInt (t.nDimensions);
            PutText ("  S= ");  PutInt (t.elementSize);
            PutText ("\n");
          END;
        END;
      END;
    END;
    Flush ();
    EVAL ShowTypes; (* to prevent an "unused symbol" warning *)
  END ShowTypes;
************************************)

PROCEDURE PutType (t: RT0.TypeDefn) =
  BEGIN
    PutText ("[");
    PutAddr (t);

    IF (t # NIL) THEN
      PutText ("  _t");
      PutHex  (t.selfID);

      PutText ("  typecode= ");
      PutInt  (t.typecode, 3);
      IF (t.lastSubTypeTC # 0) THEN
        PutText (" .. ");
        PutInt  (t.lastSubTypeTC, 3);
      END;

      IF (t.name # NIL) THEN
        PutText   ("  ");
        PutString (t.name);
      END;
    END;

    PutText ("]");
  END PutType;

PROCEDURE PutModule (mi: RT0.ModulePtr) =
  BEGIN
    IF (mi.file = NIL)
      THEN PutText ("???");
      ELSE PutString (mi.file);
    END;
  END PutModule;

BEGIN
END RTType.

