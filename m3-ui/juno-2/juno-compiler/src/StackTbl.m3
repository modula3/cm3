(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jan 20 15:17:09 PST 1995 by heydon                   *)

MODULE StackTbl;

IMPORT Atom, AtomIntTbl;

TYPE
  Entry = RECORD a: Atom.T; n: INTEGER END;
  Stack = REF ARRAY OF Entry;

REVEAL
  T = Public BRANDED "StackTbl.T" OBJECT
    tbl: AtomIntTbl.Default := NIL;
    stack: Stack := NIL;
    sp: CARDINAL
  OVERRIDES
    init := Init
  END;

CONST InitSize = 20;

(* IMPLEMENTATION:

   The stack table is implemented with a table and a stack of "(Atom.T,
   INTEGER)" pairs. The table maps "Atom.T"'s to "INTEGER"'s, and it holds the
   true mapping at any point in time. Hence, the "Lookup" procedure simply
   looks up the supplied name in the table. The stack is used to store marks,
   and old "(Atom.T, INTEGER)" associations.

   The "Mark" procedure pushes a special "(NIL, 0)" pair on the stack. The
   procedure "Push(nm)" pushes the pair "(nm, oldVal)" to the stack if "nm"
   was associated with the integer "oldVal" in the table, or pushes the pair
   "(nm, 0)" if "nm" was not previously associated in the table. It also
   associates "nm" with the next index in the table. The "PushFormal"
   procedure is similar.

   Finally the "PopToMark" procedure pops entries from the stack until it gets
   to a distinguished "(NIL, 0)" pair. For each entry "(nm, val)", it restores
   the association for "nm" in table according to "val": if "val" is non-zero,
   it associates "nm" with "val" in the table, and if "val" is zero, it
   deletes "nm" from the table. *)


PROCEDURE Mark(t: T) =
(* Push a special NIL marker on the stack. *)
  BEGIN
    PushP(t, NIL, 0)
  END Mark;

PROCEDURE PopToMark(t: T) =
  VAR dummy: INTEGER; BEGIN
    DEC(t.sp);
    LOOP
      WITH entry = t.stack[t.sp] DO
        IF entry.a = NIL THEN EXIT END;
        IF entry.n = 0
          THEN EVAL t.tbl.delete(entry.a, dummy)
          ELSE EVAL t.tbl.put(entry.a, entry.n)
        END
      END;
      DEC(t.sp)
    END
  END PopToMark;

PROCEDURE Push(t: T; nm: Atom.T) =
  VAR n: INTEGER; BEGIN
    IF t.tbl.get(nm, n)
      THEN PushP(t, nm, n);
      ELSE PushP(t, nm, 0);
    END;
    EVAL t.tbl.put(nm, t.next_index);
    INC(t.next_index)
  END Push;

PROCEDURE PushFormal(t: T; nm: Atom.T) =
  VAR n: INTEGER; BEGIN
    IF t.tbl.get(nm, n)
      THEN PushP(t, nm, n);
      ELSE PushP(t, nm, 0);
    END;
    EVAL t.tbl.put(nm, t.next_formal);
    DEC(t.next_formal)
  END PushFormal;

PROCEDURE Lookup(t: T; nm: Atom.T): INTEGER =
  VAR n: INTEGER; BEGIN
    IF NOT t.tbl.get(nm, n) THEN n := 0 END;
    RETURN n
  END Lookup;

PROCEDURE PushP(t: T; a: Atom.T; n: INTEGER) =
  BEGIN
    IF NUMBER(t.stack^) = t.sp THEN
      (* grow the stack by a factor of 2 *)
      VAR new := NEW(Stack, 2 * NUMBER(t.stack^)); BEGIN
        SUBARRAY(new^, 0, NUMBER(t.stack^)) := t.stack^;
        t.stack := new;
      END
    END;
    t.stack[t.sp].a := a;
    t.stack[t.sp].n := n;
    INC(t.sp)
  END PushP;

PROCEDURE Init(t: T): T =
  BEGIN
    t.next_index := 1;
    t.next_formal := -1;
    IF t.tbl = NIL
      THEN t.tbl := NEW(AtomIntTbl.Default).init(sizeHint := InitSize)
      ELSE EVAL t.tbl.init(sizeHint := InitSize)
    END;
    IF t.stack = NIL THEN
      t.stack := NEW(Stack, InitSize)
    END;
    t.sp := 0;
    RETURN t
  END Init;

BEGIN
END StackTbl.
