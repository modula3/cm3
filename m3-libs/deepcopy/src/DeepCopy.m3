UNSAFE MODULE DeepCopy;

IMPORT
  RTAllocator, RTTypeMap, RefSeq, IntRefTbl, Fmt, Atom, RTTypeSRC;

CONST
  Debug = FALSE;

TYPE
  Copier = RTTypeMap.Visitor OBJECT
    done, copies: RefSeq.T;
  METHODS
    init(READONLY dontcopy: Refs): Copier := CopierInit;
    seen(ref: REFANY; VAR copy: REFANY): BOOLEAN := CopierSeen;
    copy(from: REFANY): REFANY := CopierCopy;
    special(ref: REFANY; VAR copy: REFANY): BOOLEAN := CopierSpecial;
  OVERRIDES
    apply := CopierApply;
  END;

  AtomSpec = Special OBJECT
  OVERRIDES
    copy := AtomSpecCopy;
  END;

VAR
  Spec: IntRefTbl.T := NIL;


PROCEDURE Copy(from: REFANY; READONLY dontcopy := Refs{}): REFANY = 
VAR
  ret: REFANY;
BEGIN
  WITH copier = NEW(Copier).init(dontcopy) DO
    ret := copier.copy(from);
    IF Debug THEN <*DEBUG "Count = "&Fmt.Int(copier.done.size())*> END;
    RETURN ret;
  END;
END Copy;

PROCEDURE CopierInit(this: Copier; READONLY dontcopy: Refs): Copier =
BEGIN
  this.copies := NEW(RefSeq.T).fromArray(dontcopy);
  this.done := NEW(RefSeq.T).fromArray(dontcopy);
  RETURN this;
END CopierInit;

PROCEDURE CopierCopy(this: Copier; from: REFANY): REFANY =
VAR
  copy: REFANY := NIL;
BEGIN
  IF from = NIL THEN RETURN NIL END;
  IF Debug THEN <*DEBUG "Copying "&Fmt.Unsigned(LOOPHOLE(from, INTEGER))*> END;
  IF NOT this.seen(from, copy) AND NOT this.special(from, copy) THEN
    (* shallow copy*)
    copy := RTAllocator.Clone(from);
    (* memo *)
    this.done.addhi(from);
    this.copies.addhi(copy);
    (* do structure *)
    RTTypeMap.WalkRef(copy, RTTypeMap.Mask{RTTypeMap.Kind.Ref}, this);
  END;
  RETURN copy;
END CopierCopy;

PROCEDURE CopierSeen(this: Copier; ref: REFANY; VAR copy: REFANY): BOOLEAN =
BEGIN
  (* forced to do linear searches because ref values can change at any time *)
  IF Debug THEN <*DEBUG "Check seen "&Fmt.Unsigned(LOOPHOLE(ref, INTEGER))*> END;
  FOR i := 0 TO this.done.size()-1 DO
    IF ref = this.done.get(i) THEN
      copy := this.copies.get(i);
      RETURN TRUE;
    END;
  END;
  RETURN FALSE;
END CopierSeen;

PROCEDURE CopierSpecial(this: Copier; ref: REFANY; VAR copy: REFANY): BOOLEAN =
VAR
  s: REFANY;
BEGIN
  IF Spec = NIL THEN RETURN FALSE END;
  IF Spec.get(TYPECODE(ref), s) THEN
    IF Debug THEN <*DEBUG "Doing special: "&RTTypeSRC.TypecodeName(TYPECODE(ref))*> END;
    copy := NARROW(s, Special).copy(ref);
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END CopierSpecial;

PROCEDURE CopierApply(this: Copier; field: ADDRESS; <*UNUSED*> k: RTTypeMap.Kind) RAISES ANY =
BEGIN
  IF Debug THEN <*DEBUG "Apply of "&Fmt.Unsigned(LOOPHOLE(field, REF INTEGER)^)*> END;
  WITH ref = LOOPHOLE(field, REF REFANY) DO
    ref^ := this.copy(ref^);
  END;
END CopierApply;

PROCEDURE RegisterSpecial(tc: CARDINAL; copier: Special) =
BEGIN
  IF Spec = NIL THEN
    Spec := NEW(IntRefTbl.Default).init();
  END;
  EVAL Spec.put(tc, copier);
END RegisterSpecial;

(* Atom Special Methods *)

PROCEDURE AtomSpecCopy(this: AtomSpec; from: REFANY): REFANY =
BEGIN
  RETURN Atom.FromText(Atom.ToText(from));
END AtomSpecCopy;

BEGIN
  RegisterSpecial(TYPECODE(Atom.T), NEW(AtomSpec)); 
END DeepCopy.

