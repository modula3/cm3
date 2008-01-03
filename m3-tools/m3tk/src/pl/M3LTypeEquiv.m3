(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)
(**)
(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3LTypeEquiv;

IMPORT M3AST_AS;

IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_TM_F;


IMPORT M3Assert;
IMPORT M3LTypeHash;
IMPORT M3CTypeCompare, M3CStdTypes;

IMPORT RefSeq;

(* DEBUG (**)
IMPORT Wr;
IMPORT Fmt, Text;
IMPORT M3ASTDisplay, M3CUnit, M3CSrcPos;
(**) DEBUG *)


TYPE
  TypeList = REF RECORD
    next: TypeList := NIL;
    typeSpec: M3AST_AS.TYPE_SPEC;
  END;


<*INLINE*> PROCEDURE AddToTypeList(elem: TypeList; VAR list: TypeList) RAISES {}=
  BEGIN
    elem.next := list;
    list := elem;
  END AddToTypeList;


(* The basic types are given type codes which are guaranteed unique. These
codes fall in the following range: *)

CONST
  MaxBasicTypeCode = -1;
  MinBasicTypeCode =
      MaxBasicTypeCode - VAL(ORD(LAST(M3LTypeHash.BasicType)), INTEGER);

  FirstKnownTypeCode = MinBasicTypeCode - 1;



(* If a group of types are found to be identical before the main type
equivalence algorithm is complete they are assigned negative type codes and
stored out of the way of the main algorithm. This saves time because the main
algorithm does not have to compare them any more.
  Such groups of types are called the known types and they are assigned
negative type codes in decreasing order starting with 'FirstKnownTypeCode'. *)

TYPE
  BrandedTypeArray = ARRAY [0..1023] OF M3AST_AS.TYPE_SPEC;


VAR
  brandedTypes_g: BrandedTypeArray;
  knownTypes_g: TypeList;
  knownCodes_g: INTEGER; (* initialised to 'FirstKnownTypeCode' *)


(* The branded types are treated specially. Branded and opaque types are unique
and it would be pointless to try and find other types identical to them. Hence
the exported 'Add' procedure detects them and stores them in the array
'brandedTypes_g'. If the array overflows they are prepended to the
'knownTypes_g' list.
  One messy problem is that an opaque type and the branded type which it is
concretely revealed to be must have the same type code. As the opaque type and
its concrete type may be given to the 'Add' procedure in any order the 'Add'
procedure may has to:
1: Check each branded type to see if it is the concrete form of an opaque type
  which has already been entered.
2: Check each opaque type to see if its concrete form has already been entered.
At the time this check occurs 'brandedTypes_g' and 'knownTypes_g' only contain
branded types; the code which follows relies on this. *)

<*INLINE*> PROCEDURE IsConcreteFormOf(
    type, maybe: M3AST_AS.TYPE_SPEC)
    : BOOLEAN
    RAISES {}=
  BEGIN
    TYPECASE type OF
    | M3AST_AS.Opaque_type(opaque) =>
        RETURN opaque.sm_concrete_type_spec = maybe;
    ELSE
      RETURN FALSE;
    END;
  END IsConcreteFormOf;


PROCEDURE FindBrandedType(
    t: M3AST_AS.TYPE_SPEC;
    replacement: M3AST_AS.Opaque_type := NIL)
    : BOOLEAN
    RAISES {}=
  VAR
    code := t.tmp_type_code;
  BEGIN
    IF code <= knownCodes_g OR code >= 0 THEN RETURN FALSE END;
    VAR
      index := FirstKnownTypeCode - code;
    BEGIN
      IF index < 0 THEN
        RETURN FALSE;
      ELSIF index < NUMBER(brandedTypes_g) THEN
        WITH type = brandedTypes_g[index] DO
          IF type # NIL AND (type = t OR IsConcreteFormOf(type, t)) THEN
            IF replacement # NIL THEN type := replacement END;
            RETURN TRUE;
          ELSE
            RETURN FALSE;
          END;
        END;
      ELSE
        (* 'brandedTypes_g' has overflowed into 'knownTypes_g'. The higher the
         typecode the further up the list it is *)
        VAR
          listPos := (code - knownCodes_g) - 1;
          list := knownTypes_g;
        BEGIN
          WHILE listPos # 0 DO list := list.next; DEC(listPos) END;
          WITH type = list.typeSpec DO
            IF type = t OR IsConcreteFormOf(type, t) THEN
              IF replacement # NIL THEN type := replacement END;
              RETURN TRUE;
            ELSE
              RETURN FALSE;
            END;
          END;
        END;
      END;
    END;
  END FindBrandedType;


PROCEDURE AddBrandedType(t: M3AST_AS.TYPE_SPEC) RAISES {}=
  VAR
    index := FirstKnownTypeCode - knownCodes_g;
  BEGIN
    t.tmp_type_code := knownCodes_g;
    DEC(knownCodes_g);
    IF index < NUMBER(brandedTypes_g) THEN
      brandedTypes_g[index] := t;
    ELSE
      AddToTypeList(NEW(TypeList, typeSpec := t), knownTypes_g);
    END;
  END AddBrandedType;


(* As the algorithm progresses more types are discovered to be unique or are
found to be identical to a set of other types. When this happens the list of
identical types is assigned a type code and prepended to 'knownTypes_g' list *)

PROCEDURE KnownTypeList(tl: TypeList) RAISES {}=
  VAR
    code := knownCodes_g;
    oldHead := knownTypes_g;
  BEGIN
    knownTypes_g := tl;
    DEC(knownCodes_g);
    LOOP
      tl.typeSpec.tmp_type_code := code;
      WITH next = tl.next DO
        IF next = NIL THEN next := oldHead; EXIT END;
        tl := next;
      END;
    END;
  END KnownTypeList;


(* Initialize - first procedure called by user *)

TYPE
  TypeListArray = ARRAY OF TypeList;
  ConstructorTypeListArray = ARRAY M3LTypeHash.ConstructorType OF TypeList;
  BasicTypeSeq = RefSeq.T (* OF M3AST_AS.TYPE_SPEC *);
  BasicTypeArray = ARRAY M3LTypeHash.BasicType OF BasicTypeSeq;

VAR
(* DEBUG (**)
  stream_g: IO.Stream := NIL;
(**) *)
  verbose_g: BOOLEAN;
  typeCount_g: CARDINAL;
  basicTypes_g: BasicTypeArray;
  sortByConstructor_g: ConstructorTypeListArray;


PROCEDURE Initialize() RAISES {}=
  BEGIN
    verbose_g := FALSE;
    typeCount_g := 0;
    knownTypes_g := NIL;
    brandedTypes_g := BrandedTypeArray{NIL,..};
    knownCodes_g := FirstKnownTypeCode;
    basicTypes_g := BasicTypeArray{NIL,..};
    sortByConstructor_g := ConstructorTypeListArray{NIL,..};
    (* Add the basic types *)
    Add(M3CStdTypes.Null());
    Add(M3CStdTypes.Integer());
    Add(M3CStdTypes.Real());
    Add(M3CStdTypes.LongReal());
    Add(M3CStdTypes.Extended());
    Add(M3CStdTypes.RefAny());
    Add(M3CStdTypes.Address());
    Add(M3CStdTypes.Root());
    Add(M3CStdTypes.Untraced_Root());
    (* Add the standard composite types *)
    Add(M3CStdTypes.Char());
    Add(M3CStdTypes.Boolean());
    Add(M3CStdTypes.Cardinal());
    (* Add the other standard reference types *)
    Add(M3CStdTypes.Text());
    Add(M3CStdTypes.Mutex());
  END Initialize;


(* After calling 'Initialize' the user calls 'Add' to build up the set of
types which is to be partitioned. 'Add' sorts the types into different
constructor classes *)

PROCEDURE Add(t: M3AST_AS.TYPE_SPEC) RAISES {}=
  VAR
    class := M3LTypeHash.Classify(t);
    new := t.tmp_type_code = 0;
  BEGIN
    IF class IN M3LTypeHash.SetOfAllBasicTypes THEN
      (* assigns a known code straight away *)
      (* this is a bit of a fudge.  The first instance of a basic type
         always comes from initialize and is one of the M3CStdTypes.
         We want that one in our array.  Due to changes, there are now
         multiple instances of the typespecs for the basic types, but
         we don't care about them.  So just take the first and ignore
         the rest. SCG 11/89

         Well, I do care about the rest, so I made it always set the
         type code!  DLD 12/93
      *)
      t.tmp_type_code := MaxBasicTypeCode - ORD(class);
      IF basicTypes_g[class] = NIL THEN
        basicTypes_g[class] := NEW(RefSeq.T).init(20);
      END (* IF *);
      basicTypes_g[class].addhi(t);
    ELSIF class = M3LTypeHash.Class.Branded THEN
      TYPECASE t OF
      | M3AST_AS.Opaque_type(opaque) =>
          VAR
            concrete := opaque.sm_concrete_type_spec;
            concreteEnteredAlready: BOOLEAN := FALSE;
          BEGIN
            IF concrete # NIL THEN
              IF concrete.tmp_type_code = 0 THEN
                concreteEnteredAlready := FALSE;
              ELSE
                concreteEnteredAlready := FindBrandedType(concrete, opaque);
              END;
            END;
            IF concreteEnteredAlready THEN
              opaque.tmp_type_code := concrete.tmp_type_code;
            ELSE
              AddBrandedType(opaque);
              IF concrete # NIL THEN
                concrete.tmp_type_code := opaque.tmp_type_code;
              END
            END;
          END;
      ELSE
        IF new OR NOT FindBrandedType(t) THEN
          (* Has not already been entered as the concrete form of an opaque
           type *)
          AddBrandedType(t);
        END;
      END;
    ELSE
      AddToTypeList(NEW(TypeList, typeSpec := t), sortByConstructor_g[class]);
      IF NOT class IN M3LTypeHash.SetOfAllNonRecursiveTypes THEN
        INC(typeCount_g);
      END;
    END;
  END Add;


(* First some procedures which deal with the non recursive types i.e.
enumerations, subranges and sets. These are dealt with in that order so they
have the special property that either they have no component types (e.g.
enumerations) or their possible component types have already been assigned
type codes (e.g. when considering subranges integers and enumerations have
already been dealt with).
  So after 'InitialPartition' has done a hashing pass on a non recursive type
class it passes the hash array to 'PartitionNonRecursiveTypes' to finish the
job. *)

PROCEDURE PartitionNonRecursiveTypeList(
    VAR list: TypeList)
    : TypeList
    RAISES {}=
  VAR
    new: TypeList := NIL;
    compare := list.typeSpec;
    compareCode := compare.tmp_type_code;
    prev := list;
    search := prev.next;
  BEGIN
    LOOP
      VAR
        typeSpec := search.typeSpec;
        next := search.next;
      BEGIN
        IF compareCode = typeSpec.tmp_type_code AND
            M3CTypeCompare.Similar(compare, typeSpec) THEN
          prev := search;
        ELSE
          prev.next := next;
          AddToTypeList(search, new);
        END;
        IF next = NIL THEN EXIT END;
        search := next;
      END;
    END;
    RETURN new;
  END PartitionNonRecursiveTypeList;


PROCEDURE PartitionNonRecursiveTypes(VAR a: TypeListArray) RAISES {}=
  BEGIN
    FOR i := 0 TO LAST(a) DO
      WITH head = a[i] DO
        VAR
          list := head;
        BEGIN
          IF list # NIL THEN
            head := NIL;
            LOOP
              VAR
                new: TypeList;
              BEGIN
                IF list.next = NIL THEN
                  new := NIL;
                ELSE
                  new := PartitionNonRecursiveTypeList(list);
                END;
                KnownTypeList(list);
                IF new = NIL THEN EXIT END;
                list := new;
              END;
            END;
          END;
        END;
      END;
    END;
  END PartitionNonRecursiveTypes;


(* Takes a first cut at spotting non identical types. Starts with the array
'sortByConstructor_g' which contains lists of types. Each list contains types
of the same class (as defined by M3LTypeHash.Classify).
  The type lists are taken one at a time. Each type in the current list is
given a hash value, based on its structure and entered into a hash table
according to this value.
  The hash table is iterated. If the types in the table are non recursive (see
above) they are immediately partitioned into equivalent groups. Otherwise the
types, apart from any which are obviously unique, are put into a large array
which will be used by the main type equivalence algorithm. *)

PROCEDURE InitialPartition(VAR size: CARDINAL): REF TypeListArray RAISES {}=
  CONST
    HashSize = 1023;
  VAR
    a: REF TypeListArray := NIL;
    hashArray := ARRAY [0..HashSize - 1] OF TypeList {NIL, ..};
    index: CARDINAL := 0;
  BEGIN
(* DEBUG (**)
    IF verbose_g THEN PrintTypeListArray(sortByConstructor_g,
        "containing types sorted by constructor"); 
    END;
(**) DEBUG *)

    FOR c := FIRST(M3LTypeHash.ConstructorType) TO
        LAST(M3LTypeHash.ConstructorType) DO
      WITH head = sortByConstructor_g[c] DO
        VAR
          list := head;
        BEGIN
          IF list # NIL THEN
            head := NIL;
            IF list.next = NIL THEN
              KnownTypeList(list);
            ELSE
              WHILE list # NIL DO
                VAR
                  next := list.next;
                BEGIN
                  AddToTypeList(list, hashArray[
                      M3LTypeHash.Value(list.typeSpec, c) MOD HashSize]);
                  list := next;
                END;
              END;
(* DEBUG (**)
              IF verbose_g THEN
                PrintTypeListArray(hashArray,
                    "containing types sorted by hash value"); 
              END;
(**) DEBUG *)
              IF c IN M3LTypeHash.SetOfAllNonRecursiveTypes THEN
                PartitionNonRecursiveTypes(hashArray);
              ELSE
                (* Transfer type lists from 'hashArray' to 'a' *)
                FOR i := 0 TO LAST(hashArray) DO
                  WITH hashHead = hashArray[i] DO
                    VAR
                      hashList := hashHead;
                    BEGIN
                      IF hashList # NIL THEN
                        hashHead := NIL;
                        IF hashList.next = NIL THEN
                          DEC(size);
                          KnownTypeList(hashList);
                        ELSE
                          IF a = NIL THEN a := NEW(REF TypeListArray, size) END;
                          a[index] := hashList;
                          WHILE hashList # NIL DO
                            hashList.typeSpec.tmp_type_code := index;
                            hashList := hashList.next;
                          END;
                          INC(index);
                        END;
                      END;
                    END;
                  END;
                END;
              END;
            END;
          END;
        END;
      END;
    END;
    size := index;
    RETURN a;
  END InitialPartition;


(* Now a procedure that partitions a list of type specs. The first type spec
in the list is compared against all the other type specs in the list.
The first type spec and all those type specs which are similar to the first
type spec remain in the list. Any other type specs are inserted into a new
list *)

PROCEDURE PartitionTypeList(
    list: TypeList;
    subSets: INTEGER)
    : TypeList
    RAISES {}=
  VAR
    new: TypeList := NIL;
    compare := list;
    prev := compare;
    search := compare.next;
  BEGIN
    LOOP
      VAR
        next := search.next;
      BEGIN
        IF M3CTypeCompare.Similar(compare.typeSpec, search.typeSpec) THEN
          prev := search;
        ELSE
          prev.next := next;
          search.typeSpec.tmp_type_code := subSets;
          AddToTypeList(search, new);
        END;
        IF next = NIL THEN EXIT END;
        search := next;
      END;
    END;
    RETURN new;
  END PartitionTypeList;


PROCEDURE AssignTypeCodeDontForgetOpaques(ts: M3AST_AS.TYPE_SPEC;
                                          tc: INTEGER) RAISES {}=
  BEGIN
    TYPECASE ts OF
    | M3AST_AS.Opaque_type(opaque) =>
        (* trevor sez: don't forget the concrete revelation of opaque types *)
        IF opaque.sm_concrete_type_spec # NIL THEN
          opaque.sm_concrete_type_spec.tmp_type_code := tc;
        END;
    ELSE
    END;
    ts.tmp_type_code := tc;
  END AssignTypeCodeDontForgetOpaques;


PROCEDURE CopyTypeSpecArray(
    READONLY from: TypeSpecArray;
    VAR to: TypeSpecArray;
    VAR index: CARDINAL)
    RAISES {}=
  BEGIN
    FOR i := FIRST(from) TO LAST(from) DO
      VAR
        type := from[i];
      BEGIN
        AssignTypeCodeDontForgetOpaques(type, index);
        to[index] := type;
        INC(index);
      END;
    END;
  END CopyTypeSpecArray;

PROCEDURE CopyBasicTypeArray(
    READONLY from: BasicTypeArray;
    VAR to: TypeSpecArray;
    VAR index: CARDINAL)
    RAISES {}=
  BEGIN
    FOR i := FIRST(from) TO LAST(from) DO
      VAR
        typeSeq := from[i];
      BEGIN
        FOR j := 0 TO typeSeq.size() - 1 DO
          AssignTypeCodeDontForgetOpaques(typeSeq.get(j), index);
        END (* FOR *);
        to[index] := typeSeq.getlo();
        INC(index);
      END;
    END;
  END CopyBasicTypeArray;

(* Now, at last, the main algorithm. It partitions up the type specs supplied
by the user. It then cycles over these sub sets of type specs further
partitioning them when type specs in the same set are not similar. It repeats
this process until no further partitioning is possible *)

PROCEDURE Partition(): REF TypeSpecArray RAISES {}=
  VAR
    brandedTypesInArray :=
        MIN(FirstKnownTypeCode - knownCodes_g, NUMBER(brandedTypes_g));
    a := InitialPartition(typeCount_g);
    subSets := typeCount_g;
    oldSubSets: CARDINAL;
    empty, iterations: CARDINAL := 0;
  BEGIN
(* DEBUG (**)
    IF verbose_g AND subSets > 0 THEN
      PrintTypeListArray(SUBARRAY(a^, 0, subSets), "before partition starts"); 
    END;
(**) DEBUG *)
    IF subSets > 0 THEN
      REPEAT
        oldSubSets := subSets;
        VAR
          count: CARDINAL := 0;
        BEGIN
          WHILE count < subSets DO
            WITH head = a[count] DO
              IF head # NIL THEN
                WITH new = PartitionTypeList(head, subSets) DO
                  IF new # NIL THEN
                    IF new.next = NIL THEN
                      KnownTypeList(new);
                    ELSE
                      a[subSets] := new;
                      INC(subSets);
                    END;
                    IF head.next = NIL THEN
                      KnownTypeList(head);
                      head := NIL;
                      INC(empty);
                    END;
                  END;
                END;
              END;
            END;
            INC(count);
          END;
        END;
        INC(iterations);
      UNTIL subSets <= oldSubSets OR empty = subSets;
    END;

(* DEBUG (**)
    IF verbose_g THEN
      Wr.PutText(stream_g, Fmt.Int(
          "**Type equivalence computation completed after %s iterations\n",
          Fmt.Int(iterations)));
      IF subSets > 0 THEN
        PrintTypeListArray(
            SUBARRAY(a^, 0, subSets), "after partition complete"); 
      END;
    END;
(**) DEBUG *)

    VAR
      res := NEW(REF TypeSpecArray, 
          (subSets - empty) + (FirstKnownTypeCode - knownCodes_g) +
          NUMBER(basicTypes_g));
      index: CARDINAL := 0;
    BEGIN
      CopyBasicTypeArray(basicTypes_g, res^, index);
      CopyTypeSpecArray(
          SUBARRAY(brandedTypes_g, 0, brandedTypesInArray), res^, index);
      VAR
        known := knownTypes_g;
        last := 0; (* typecodes of items on 'knownTypes_g' are never 0 *)
      BEGIN
        WHILE known # NIL DO
          WITH code = known.typeSpec.tmp_type_code DO
            IF code = last THEN
              known.typeSpec.tmp_type_code := index - 1;
            ELSIF code = index-1 THEN
(* MAJOR kludge!!!   This loop goes down the list of known types and 
   assigns the same official type code to all types with the same
   initial (temporary) type code.  The problem comes from multiple entries
   that SHARE the same type spec (particularly CHAR, CARDINAL and BOOLEAN
   but maybe others).  When the new type code is stored in, all shared
   type specs get the new type code then.  So when we check to see if we
   are still in the same type code group, we must also check to see if we
   have the new type index.

   Problems: It is possible (although unlikely) then the new type code for
   a group will equal the temporary typecode for the new group and the two
   groups will get the same type code.  The code will blow up later if this
   happens as the assert below.  This could be fixed by being clever about
   remembering the head of groups and checking here to see if two typespecs
   in the group are shared...

   index-1 is necessary because the index is incremented after it is first
   used.
*)

            ELSE
              last := code;
              AssignTypeCodeDontForgetOpaques(known.typeSpec, index); (* fix *)
              res[index] := known.typeSpec;
              INC(index);
            END;
          END;
          known := known.next;
        END;
        knownTypes_g := NIL;
      END;
      FOR i := 0 TO subSets - 1 DO
        VAR
          list := a[i];
        BEGIN
          IF list # NIL THEN
            res[index] := list.typeSpec;
            REPEAT
              list.typeSpec.tmp_type_code := index;
              list := list.next;
            UNTIL list = NIL;
            INC(index);
          END;
        END;
      END;
(*DEBUG (**)
      IF verbose_g THEN PrintTypeSpecArray(res^, "Final result!") END;
(**) DEBUG*)
      IF index # NUMBER(res^) THEN M3Assert.Fail() END;
(* WARNING see above kludge comment to see why this might happen *)
      RETURN res;
    END;
  END Partition;


(* DEBUG (**)
PROCEDURE PrintTypeSpec(s: IO.Stream; t: M3AST_AS.TYPE_SPEC) RAISES {IO.Error}=
  VAR
    line, column: CARDINAL;
    cu: M3AST_AS.Compilation_Unit;
    save: M3CSrcPos.T;
  BEGIN
    TYPECASE t OF
    | M3AST_AS.Opaque_type(opaqueType) =>
        IF opaqueType.sm_concrete_type_spec # NIL THEN
          t := opaqueType.sm_concrete_type_spec;
        END;
    ELSE
    END;
    IF t.tmp_unit_id # NIL THEN
      line := M3CSrcPos.Unpack(t.lx_srcpos, column);
      cu := NARROW(t.tmp_unit_id.sm_spec, M3AST_AS.UNIT).sm_comp_unit;
      IO.PutF(s, "  File \'%s\' line %s column %s\n",
          M3CUnit.TextName(cu.fe_uid), Fmt.Int(line), Fmt.Int(column));
    ELSE
      IO.PutText(s, "  Basic type\n");
    END;
    IO.PutText(s, "    ");
    save := t.lx_srcpos; (* hack alert - see below *)
    M3ASTDisplay.Nodes(t, s, 4); (* unexpected Error *)
    IO.Put(s, '\n');
    (* horrible hack needed until M3ASTDisplay.Nodes is non destructive: *)
    t.lx_srcpos := save;
  END PrintTypeSpec;


PROCEDURE PrintTypeList(
    s: IO.Stream;
    list: TypeList;
    pos: INTEGER)
    RAISES {IO.Error}=
  BEGIN
    IF list # NIL THEN
      VAR
        count := 0;
        tl := list;
      BEGIN
        WHILE tl # NIL DO INC(count); tl := tl.next END;
        IO.PutF(s, "List at position %s contains %s elements:\n",
            Fmt.Int(pos), Fmt.Int(count));
        tl := list;
        WHILE tl # NIL DO
          PrintTypeSpec(stream_g, tl.typeSpec);
          tl := tl.next;
        END; (* while *)
      END;
    END; (* if *)
  END PrintTypeList;


PROCEDURE PrintTypeListArray(
    READONLY a: ARRAY OF TypeList;
    msg: Text.T)
    RAISES {IO.Error}=
  BEGIN
    IO.PutF(stream_g, "Printing array of type lists %s\n", msg);
    FOR i := 0 TO LAST(a) DO
      PrintTypeList(stream_g, a[i], i);
    END; (* for *)
    IO.Flush(stream_g);
  END PrintTypeListArray;


PROCEDURE PrintTypeSpecArray(READONLY a: TypeSpecArray; msg: Text.T) 
  RAISES {IO.Error}=
  BEGIN
    IO.PutF(stream_g, "Printing array of types %s\n", msg);
    FOR i := 0 TO LAST(a) DO
      IO.PutF(stream_g, "Entry %s:\n", Fmt.Int(i));
      PrintTypeSpec(stream_g, a[i]);
    END;
    IO.Flush(stream_g);
  END PrintTypeSpecArray;
(**) DEBUG *)

BEGIN
END M3LTypeEquiv.
