(*                                                                           *)
(*  Equivalence.ig                                                           *)
(*                                                                           *)
(*  Generic equivalence classes                                              *)
(*                                                                           *)
(*  Copyright (c) 2000 California Institute of Technology                    *)
(*  All rights reserved.                                                     *)
(*  Department of Computer Science                                           *)
(*  Pasadena, CA 91125.                                                      *)
(*                                                                           *)
(*  Author: Karl Papadantonakis <kp@caltech.edu                              *)
(*                                                                           *)
(*  Permission to use, copy, modify, and distribute this software            *)
(*  and its documentation for any purpose and without fee is hereby          *)
(*  granted, provided that the above copyright notice appear in all          *)
(*  copies. The California Institute of Technology makes no representations  *)
(*  about the suitability of this software for any purpose. It is            *)
(*  provided "as is" without express or implied warranty. Export of this     *)
(*  software outside of the United States of America may require an          *)
(*  export license.                                                          *)

GENERIC MODULE Equivalence(Elem, ElemElemTbl);

TYPE
  Public = T OBJECT METHODS
    init(sizeHint: CARDINAL := 0;
         leaderPreference: Preference := NIL): Default;
  END;

  PrivateIter = Iterator BRANDED "DefEquivIter(" & Elem.Brand & ")" OBJECT
    iter: ElemElemTbl.Iterator;
    tbl: Default;
  OVERRIDES
    next := Next;
  END;

REVEAL
  Default = Public BRANDED "DefEquiv(" & Elem.Brand & ")" OBJECT
    t: ElemElemTbl.T;
    p: Preference;
  OVERRIDES
    init := Init;
    equal := Equal;
    identify := Identify;
    canon := Canon;
    iterate := Iterate;
  END;

PROCEDURE Init(self: Default;
               sizeHint: CARDINAL := 0;
               leaderPreference: Preference := NIL): Default =
  BEGIN
    self.t := NEW(ElemElemTbl.Default).init(sizeHint);
    self.p := leaderPreference;
    RETURN self;
  END Init;

PROCEDURE Equal(self: Default; e1, e2: Elem.T): BOOLEAN =
  BEGIN
    RETURN Elem.Equal(Canon(self, e1), Canon(self, e2));
  END Equal;

PROCEDURE Identify(self: Default; e1, e2: Elem.T): BOOLEAN =
  VAR
    c1 := Canon(self, e1);
    c2 := Canon(self, e2);
    not: BOOLEAN;
  BEGIN
    IF Elem.Equal(c1, c2) THEN
      RETURN TRUE;
    ELSE
      IF self.p = NIL OR self.p.is(c2, c1) THEN
        not := self.t.put(c1, c2);
      ELSE
        not := self.t.put(c2, c1);
      END;
      <* ASSERT NOT not *>
      RETURN FALSE;
    END;
  END Identify;

PROCEDURE CanonNonMutating(self: Default; cur: Elem.T): Elem.T =
  BEGIN
    WHILE self.t.get(cur, cur) DO END;
    RETURN cur;
  END CanonNonMutating;

PROCEDURE Canon(self: Default; e: Elem.T): Elem.T =
  VAR
    cur := e;
    len := 0;
  BEGIN
    WHILE self.t.get(cur, cur) DO
      INC(len);
    END;
    (* path compression: *)
    IF len # 0 THEN
      WITH y = self.t.put(e, cur) DO <* ASSERT y *> END;
    END;
    RETURN cur;
  END Canon;

PROCEDURE Iterate(self: Default): Iterator =
  BEGIN
    RETURN NEW(PrivateIter, iter := self.t.iterate(), tbl := self);
  END Iterate;

PROCEDURE Next(self: PrivateIter; VAR alias, canon: Elem.T): BOOLEAN =
  VAR
    dummy: Elem.T;
    result := self.iter.next(alias, dummy);
  BEGIN
    canon := CanonNonMutating(self.tbl, alias);
    RETURN result;
  END Next;

BEGIN
END Equivalence.
