(* Copyright 1993 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Fri Jun 25 14:36:40 PDT 1993 by mcjones *)

(* Tests of Unix implementation of Pathname. *)

MODULE PathnameTests;

IMPORT Pathname, RefList, Sx, Text;
FROM Pathname IMPORT
  Valid, Decompose, Compose,
  Absolute, Prefix, Last, Base, Join, LastBase, LastExt, ReplaceExt;

PROCEDURE Eq(p1, p2: Pathname.T): BOOLEAN =
  BEGIN RETURN Text.Equal(p1, p2) END Eq;

PROCEDURE EqArcs(arcs1, arcs2: Pathname.Arcs): BOOLEAN =
  BEGIN
    WITH n = arcs1.size() DO
      IF n # arcs2.size() THEN RETURN FALSE END;
      FOR i := 0 TO n - 1 DO
        WITH arc1 = arcs1.get(i), arc2 = arcs2.get(i) DO
          IF arc1 = NIL OR arc2 = NIL THEN
            <* ASSERT i = 0 *>
            IF arc1 # arc2 THEN RETURN FALSE END
          ELSIF NOT Text.Equal(arc1, arc2) THEN RETURN FALSE
          END
        END
      END
    END;
    RETURN TRUE
  END EqArcs;

(* VAR w := Stdio.stderr; <* FATAL Wr.Failure, Thread.Alerted *> *)

(*    
PROCEDURE P(actual, expected: TEXT) =
  BEGIN
    Wr.PutText(w, "actual=\"" & actual & "\" expected=\"" & expected & "\"\n")
  END P;
PROCEDURE PArcs(a: Pathname.Arcs) =
  BEGIN
    IF a = NIL THEN Wr.PutText(w, "NIL!\n"); RETURN END;
    Wr.PutText(w, "[");
      WHILE a # NIL DO
        IF a.head = NIL THEN Wr.PutText(w, "NIL")
        ELSE Wr.PutText(w, "\"" & a.head & "\"")
        END;
        IF a.tail # NIL THEN Wr.PutText(w, ",") END;
        a := a.tail
      END;    
    Wr.PutText(w, "]\n")
  END PArcs;
*)

PROCEDURE PosixTests(<*UNUSED*>args: RefList.T): Sx.T =
  BEGIN
    <* ASSERT Valid("") *>
    <* ASSERT Valid("/") *>
    <* ASSERT Valid("//") *>
    <* ASSERT Valid("/a/b") *>
    <* ASSERT Valid("a/b") *>
    <* ASSERT Valid("a//b") *>
    VAR t := "";
    BEGIN
      FOR i := 1 TO 8_177 DO
	IF VAL(i, CHAR) # '/' THEN
	  t := Text.Cat(t, Text.FromChar(VAL(i, CHAR)))
	END
      END;
      <* ASSERT Valid(t) *>
      <* ASSERT NOT Valid("\000") *>
    END;

    VAR l: Pathname.Arcs;
   <* FATAL Pathname.Invalid *>
    BEGIN
  (*
      P(Compose(Decompose("/")), "/");
      P(Compose(Decompose("//")), "//");
      P(Compose(Decompose("")), "");
      P(Compose(Decompose("a")), "a");
      P(Compose(Decompose("a/b/c//d")), "a/b/c//d");
  *)

      <* ASSERT Eq(Compose(Decompose("/")), "/") *>
      <* ASSERT Eq(Compose(Decompose("//")), "//") *>
      <* ASSERT Eq(Compose(Decompose("a")), "a") *>
      <* ASSERT Eq(Compose(Decompose("a/b/c//d/")), "a/b/c//d/") *>

      l := NEW(Pathname.Arcs).fromArray(ARRAY OF TEXT{"/"});
  (*  Wrv.PutText(w, "\n"); P(Compose(l), "/"); PArcs(Decompose(Compose(l))); *)
      <* ASSERT EqArcs(Decompose(Compose(l)), l) *>

      l := NEW(Pathname.Arcs).fromArray(ARRAY OF TEXT{NIL, ""});
  (*  Wr.PutText(w, "\n"); P(Compose(l), ""); PArcs(Decompose(Compose(l))); *)
      <* ASSERT EqArcs(Decompose(Compose(l)), l) *>

      l := NEW(Pathname.Arcs).fromArray(ARRAY OF TEXT{NIL, "a"});
  (*  Wr.PutText(w, "\n"); P(Compose(l), "/a"); PArcs(Decompose(Compose(l))); *)
      <* ASSERT EqArcs(Decompose(Compose(l)), l) *>

      l := NEW(Pathname.Arcs).fromArray(ARRAY OF TEXT{NIL, "a"});
  (*  Wr.PutText(w, "\n"); P(Compose(l), "a"); PArcs(Decompose(Compose(l))); *)
      <* ASSERT EqArcs(Decompose(Compose(l)), l) *>

      l := NEW(Pathname.Arcs).fromArray(ARRAY OF TEXT{"/", "a", "", "b"});
  (*  Wr.PutText(w, "\n"); P(Compose(l), "/a//b"); PArcs(Decompose(Compose(l)));
  *)
      <* ASSERT EqArcs(Decompose(Compose(l)), l) *>

      l := NEW(Pathname.Arcs).fromArray(ARRAY OF TEXT{NIL, "a", "", "b"});
  (*  Wr.PutText(w, "\n"); P(Compose(l), "a//b"); PArcs(Decompose(Compose(l)));
  *)
      <* ASSERT EqArcs(Decompose(Compose(l)), l) *>
    END;

    <* ASSERT Absolute("/") *>
    <* ASSERT Absolute("/a") *>
    <* ASSERT Absolute("//a/b/c") *>
    <* ASSERT NOT Absolute("") *>
    <* ASSERT NOT Absolute("a") *>
    <* ASSERT NOT Absolute("a/b") *>
    <* ASSERT NOT Absolute("\\") *>

    <* ASSERT Eq(Prefix("/a"), "/") *>
    <* ASSERT Eq(Last("/a"), "a") *>
    <* ASSERT Eq(Prefix("/"), "/") *>
    <* ASSERT Eq(Last("/"), "") *>
    <* ASSERT Eq(Prefix("/a/b"), "/a") *>
    <* ASSERT Eq(Last("/a/b"), "b") *>
    <* ASSERT Eq(Prefix("/a/"), "/a") *>
    <* ASSERT Eq(Last("/a/"), "") *>

    <* ASSERT Eq(Base("/"), "/") *>
    <* ASSERT Eq(Base(""), "") *>
    <* ASSERT Eq(Base("a.b"), "a") *>
    <* ASSERT Eq(Base("/a.b"), "/a") *>
    <* ASSERT Eq(Base("/a/b/c.d"), "/a/b/c") *>
    <* ASSERT Eq(Base("a/b/c.d"), "a/b/c") *>

    <* ASSERT Eq(Join(NIL, "b", NIL), "b") *>
    <* ASSERT Eq(Join(NIL, "b", "c"), "b.c") *>
    <* ASSERT Eq(Join("/", "b", NIL), "/b") *>
    <* ASSERT Eq(Join("/", "b", "c"), "/b.c") *>
    <* ASSERT Eq(Join("x/y", "b", NIL), "x/y/b") *>
    <* ASSERT Eq(Join("x/y", "b", "c"), "x/y/b.c") *>

    <* ASSERT Eq(LastBase("/a.b"), "a") *>
    <* ASSERT Eq(LastBase("/"), "") *>
    <* ASSERT Eq(LastBase("/a/b.c"), "b") *>
    <* ASSERT Eq(LastBase("/a.b/"), "") *>

    <* ASSERT Eq(LastExt("/a.b"), "b") *>
    <* ASSERT Eq(LastExt("/"), "") *>
    <* ASSERT Eq(LastExt("/a/b.c"), "c") *>
    <* ASSERT Eq(LastExt("/a.b/"), "") *>

    <* ASSERT Eq(ReplaceExt("/a.b", "c"), "/a.c") *>
    <* ASSERT Eq(ReplaceExt("/", "c"), "/") *>
    <* ASSERT Eq(ReplaceExt("/a/b.c", "d"), "/a/b.d") *>
    <* ASSERT Eq(ReplaceExt("/a.b/", "c"), "/a.b/") *>

    <* ASSERT Eq(Pathname.Parent, "..") *>
    <* ASSERT Eq(Pathname.Current, ".") *>
    RETURN "PathnameTests.PosixTests completed"
  END PosixTests;
BEGIN
END PathnameTests.
