(*                                                                           *)
(*  Sop.m3                                                                   *)
(*                                                                           *)
(*  S-O-P expressions.                                                       *)
(*                                                                           *)
(*  Copyright (c) 2000 California Institute of Technology                    *)
(*  All rights reserved.                                                     *)
(*  Department of Computer Science                                           *)
(*  Pasadena, CA 91125.                                                      *)
(*                                                                           *)
(*  Author: Mika Nystrom <mika@cs.caltech.edu>                               *)
(*                                                                           *)
(*  Permission to use, copy, modify, and distribute this software            *)
(*  and its documentation for any purpose and without fee is hereby          *)
(*  granted, provided that the above copyright notice appear in all          *)
(*  copies. The California Institute of Technology makes no representations  *)
(*  about the suitability of this software for any purpose. It is            *)
(*  provided "as is" without express or implied warranty. Export of this     *)
(*  software outside of the United States of America may require an          *)
(*  export license.                                                          *)
(*                                                                           *)
(* $Id$ *)

GENERIC MODULE SopG(SopRep, Bool, BoolTextTbl, SopLiteral, BoolImpl, 
                   SortSopConjunct, ArraySortSopLiteral);

IMPORT Word;
IMPORT SopFormatStyle;
IMPORT Debug;
IMPORT IntList;

(* implementation strategy: the basic data structures are immutable:
   once created, they are not changed.  The garbage collector makes this
   implementation simpler.

   We can thus re-use conjuncts to do Or...(e.g.) *)

TYPE
  Conjunct = SopRep.Conjunct;
  Rep = SopRep.Rep;

REVEAL 
  T = SopRep.Private BRANDED Brand OBJECT
    bool : Bool.T;
  OVERRIDES
    init := Init;
    format := Format;
    toBool := ToBool;
    invariantSimplify := InvariantSimplify;
    map := Map;
    mapSimple := MapSimple;
  END;

(* constants --- this is the rationale:
   False: Adding conjuncts weakens an expression, deleting them strengthens.
          The limit of strengthening is the constant FALSE.

   True:  Adding literals to a conjunct strengthens, deleting them weakens.
          The limit of weakening is the constant TRUE.
*)

PROCEDURE IsFalse(self : T) : BOOLEAN =
  BEGIN RETURN NUMBER(self.rep^) = 0 END IsFalse;

PROCEDURE IsTrue(self : T) : BOOLEAN =
  BEGIN 
    <* ASSERT self.rep # NIL *>
    IF IsFalse(self) THEN RETURN FALSE END;
    <* ASSERT self.rep[0] # NIL *>
    RETURN NUMBER(self.rep[0]^) = 0 
  END IsTrue;

PROCEDURE FromConjunct(c : Conjunct) : T =
  VAR 
    res := NEW(T, rep := NEW(REF ARRAY OF Conjunct,1));
  BEGIN
    res.rep[0] := c;
    RETURN res
  END FromConjunct;

PROCEDURE Init(self : T; from : Bool.T) : T =
  BEGIN
    (* first check special cases *)
    IF from = Bool.False() THEN RETURN false
    ELSIF from = Bool.True() THEN RETURN true 
    END;

    (* ok not one of those, build a sop expression *)
    self.rep := NEW(Rep, 1);
    WITH conj = self.rep[0] DO
      conj := NEW(Conjunct, 1);
      conj[0].var := from;
      conj[0].mode := TRUE
    END;
    RETURN self
  END Init;

PROCEDURE IdentityCanon(<*UNUSED*>am : AliasMapper; txt : TEXT) : TEXT =
  BEGIN RETURN txt END IdentityCanon;

PROCEDURE Format(self : T; symTab : BoolTextTbl.T;
                 READONLY style := SopFormatStyle.C;
                 prefix : TEXT;
                 inQuotes : BOOLEAN;
                 am : AliasMapper) : TEXT =

  PROCEDURE FormatConjunct(c : Conjunct) : TEXT =
    VAR res := ""; BEGIN 
      IF NUMBER(c^) = 0 THEN RETURN "TRUE" END;
      FOR i := 0 TO LAST(c^) DO
        IF NOT c[i].mode THEN res := res & style.notSym END;
        res := res & q & 
                   am.canon(Bool.Format(c[i].var,symTab,pfx := prefix)) & 
                     q;
        IF i < LAST(c^) THEN res := res & style.andSym END
      END;
      RETURN res
    END FormatConjunct;

  VAR res := ""; 
      q   := "";
  BEGIN 

    IF am = NIL THEN am := IdentityMapper END;

    IF inQuotes THEN q := "\"" END;

    IF IsFalse(self) THEN RETURN "FALSE" 
    ELSIF IsTrue(self) THEN RETURN "TRUE"
    END;

    FOR i := 0 TO LAST(self.rep^) DO
      res := res & FormatConjunct(self.rep[i]);
      IF i < LAST(self.rep^) THEN res := res & style.orSym END
    END;
    RETURN res
  END Format;


PROCEDURE ToBool(self : T) : Bool.T =

  PROCEDURE ConjunctToBool(c : Conjunct) : Bool.T =
    VAR res := Bool.True(); BEGIN
      FOR i := 0 TO LAST(c^) DO 
        IF c[i].mode THEN 
          res := Bool.And(res,c[i].var) 
        ELSE
          res := Bool.And(res,Bool.Not(c[i].var))
        END
      END;
      RETURN res
    END ConjunctToBool;

  VAR res := Bool.False(); BEGIN 
    IF self.bool # NIL THEN RETURN self.bool END;
    FOR i := 0 TO LAST(self.rep^) DO
      res := Bool.Or(res,ConjunctToBool(self.rep[i]))
    END;
    RETURN res
  END ToBool;

PROCEDURE And(READONLY a, b : T) : T =

  PROCEDURE MergeConjuncts(a, b : Conjunct) : Conjunct =
    VAR res := NEW(Conjunct, NUMBER(a^) + NUMBER(b^)); BEGIN
      (*Debug.Out("Sop.And.MergeConjuncts()");*)
      SUBARRAY(res^,0,NUMBER(a^)) := a^;
      SUBARRAY(res^,NUMBER(a^),NUMBER(b^)) := b^;
      RETURN res
    END MergeConjuncts;

  VAR res := NEW(T, rep := NEW(Rep,
                               NUMBER(a.rep^) * NUMBER(b.rep^)));
  BEGIN 
(*    Debug.Out("Sop.And()");*)
    <* ASSERT RepIsOK(a.rep) *>
    <* ASSERT RepIsOK(b.rep) *>

    (* check special cases first *)

    IF IsFalse(a) OR IsFalse(b) THEN RETURN false END;
    IF IsTrue(a) THEN RETURN b ELSIF IsTrue(b) THEN RETURN a END;

    <* ASSERT a.rep # NIL AND b.rep # NIL *>
    FOR i := FIRST(a.rep^) TO LAST(a.rep^) DO
      FOR j := FIRST(b.rep^) TO LAST(b.rep^) DO
        res.rep[i * NUMBER(b.rep^) + j] := MergeConjuncts(a.rep[i],b.rep[j])
      END
    END;
    <* ASSERT RepIsOK(res.rep) *>
    RETURN Simplify(res)
  END And;

PROCEDURE Or(READONLY a, b : T) : T =
  VAR res : T; BEGIN 

    <* ASSERT RepIsOK(a.rep) *>
    <* ASSERT RepIsOK(b.rep) *>

    (* check special cases first *)
    IF IsFalse(a) THEN RETURN b ELSIF IsFalse(b) THEN RETURN a END;
    IF IsTrue(a) OR IsTrue(b) THEN RETURN true END;

    <* ASSERT a.rep # NIL AND b.rep # NIL *>
    res := NEW(T, rep := NEW(Rep,NUMBER(a.rep^) + NUMBER(b.rep^)));
    SUBARRAY(res.rep^,0,NUMBER(a.rep^)) := a.rep^;
    SUBARRAY(res.rep^,NUMBER(a.rep^),NUMBER(b.rep^)) := b.rep^;
    <* ASSERT RepIsOK(res.rep) *>
    RETURN Simplify(res)
  END Or;

PROCEDURE FromLiteral(literal : SopLiteral.T) : T = 
  VAR res := NEW(T, rep := NEW(Rep, 1)); BEGIN
    res.rep[0] := NEW(Conjunct,1);
    res.rep[0,0] := literal;
    RETURN res
  END FromLiteral;

PROCEDURE Not(READONLY a : T) : T = 
  VAR res :=true; BEGIN 
    <* ASSERT RepIsOK(a.rep) *>
    FOR i := 0 TO LAST(a.rep^) DO
      VAR c := false; BEGIN WITH conjunct = a.rep[i] DO
        FOR j := 0 TO LAST(conjunct^) DO
          c := Or(c,FromLiteral(SopLiteral.T { conjunct[j].var, 
                                          NOT conjunct[j].mode } ))
        END;
        res := And(res,c)
      END END
    END;
    <* ASSERT RepIsOK(res.rep) *>
    RETURN Simplify(res)
  END Not;

PROCEDURE DeleteConjunct( rep : Rep; i : CARDINAL) : Rep =
  (* Delete conjunct #i from rep *)
  BEGIN
    <* ASSERT RepIsOK(rep) *>
    IF NUMBER(rep^) = 1 THEN <* ASSERT i = 0 *> RETURN false.rep END;

    <* ASSERT i < NUMBER(rep^) *>

    VAR newRep := NEW(Rep, NUMBER(rep^) - 1); BEGIN
      SUBARRAY(newRep^,0,i) := SUBARRAY(rep^,0,i);
      IF i + 1 <= LAST(rep^) THEN 
        SUBARRAY(newRep^,i, NUMBER(newRep^) - i) :=
            SUBARRAY(rep^,i+1, NUMBER(rep^) - (i + 1))
      END;
      <* ASSERT RepIsOK(newRep) *>
      RETURN newRep
    END
  END DeleteConjunct;

(* basic simplifications, just remove redundancies *)
(* this is by far the most complex piece of code in the module *)
PROCEDURE Simplify(aOld : T) : T =

  PROCEDURE ConjunctIsTrue(c : Conjunct) : BOOLEAN =
    BEGIN RETURN NUMBER(c^) = 0 END ConjunctIsTrue;

  PROCEDURE ConjunctIsFalse(c : Conjunct) : BOOLEAN =
    (* REQUIRES sorted(c) *)
    (* ENSURES res = TRUE iff exists(i,j) s.t. c[i] = NOT(c[j]) *)
    BEGIN 
      IF NUMBER(c^) < 2 THEN RETURN FALSE END;
      FOR i := 0 TO LAST(c^) - 1 DO
        IF c[i].var = c[i+1].var 
          AND c[i].mode # c[i+1].mode THEN RETURN TRUE END
      END;
      RETURN FALSE
    END ConjunctIsFalse;

  PROCEDURE CleanConjunct( c : Conjunct ) : Conjunct =
    (* REQUIRES sorted(c) AND NOT ConjunctIsFalse(c) *)
    BEGIN
      <* ASSERT NOT ConjunctIsFalse(c) *>
      <* ASSERT ConjunctIsOK(c) *>
      IF NUMBER(c^) < 2 THEN RETURN c END;
      FOR i := 0 TO LAST(c^) - 1 DO
        <* ASSERT i + 1 <= LAST(c^) *>
        IF c[i].var = c[i+1].var THEN
          (* delete c[i+1] *)
          VAR new := NEW(Conjunct, NUMBER(c^) - 1); BEGIN
            SUBARRAY(new^, 0, i + 1) := SUBARRAY(c^, 0, i + 1);
            IF i + 1 < LAST(c^) THEN
              SUBARRAY(new^, i + 1, NUMBER(new^) - (i + 1)) := 
                  SUBARRAY(c^, i + 2, NUMBER(c^) - (i + 2))
            END;

            <* ASSERT NUMBER(new^) < NUMBER(c^) *> (* variant fxn *)
            <* ASSERT ConjunctIsOK(new) *>
            RETURN CleanConjunct(new)
          END
        END
      END;
      <* ASSERT ConjunctIsOK(c) *>
      RETURN c
    END CleanConjunct;

  PROCEDURE EqualConjuncts( a , b : Conjunct ) : BOOLEAN =
    BEGIN
      <* ASSERT ConjunctIsOK(a) *>
      <* ASSERT ConjunctIsOK(b) *>
      IF NUMBER(a^) # NUMBER(b^) THEN RETURN FALSE END;
      FOR i := 0 TO LAST(a^) DO
        IF a[i].var # b[i].var OR a[i].mode # b[i].mode THEN RETURN FALSE END
      END;
      RETURN TRUE
    END EqualConjuncts;

  VAR
    a := Copy(aOld);
  BEGIN (* Simplify *)
    (*Debug.Out("Sop.Simplify()");*)
    (* check special cases *)
    IF IsFalse(a) OR IsTrue(a) THEN RETURN a END;

    (* first sort conjuncts and 
       remove all conjuncts that are, strictly speaking, FALSE *)

    <* ASSERT RepIsOK(a.rep) *>
    VAR 
      falseConjuncts : IntList.T := NIL; 
      newRep : Rep;
      simplify := FALSE;
    BEGIN
      FOR i := FIRST(a.rep^) TO LAST(a.rep^) DO
        IF ConjunctIsTrue(a.rep[i]) THEN RETURN true END;

        ArraySortSopLiteral.Sort(a.rep[i]^);
        IF ConjunctIsFalse(a.rep[i]) THEN
          (* note: consed on in ascending order *)
          falseConjuncts := IntList.Cons(i,falseConjuncts);
          simplify := TRUE;
        END
      END;
      
      newRep := a.rep;

      (* falseConjuncts now in descending order. *)
      WHILE falseConjuncts # NIL DO
        newRep := DeleteConjunct(newRep,falseConjuncts.head);
        falseConjuncts := falseConjuncts.tail;
      END;

      IF simplify THEN RETURN Simplify(NEW(T, rep := newRep)) END
    END;

    <* ASSERT RepIsOK(a.rep) *>
    (* then simplify remaining conjuncts *)
    FOR i := 0 TO LAST(a.rep^) DO a.rep[i] := CleanConjunct(a.rep[i]) END;

    <* ASSERT RepIsOK(a.rep) *>
    (* and remove redundant remaining conjuncts *)
    FOR i := 0 TO LAST(a.rep^) - 1 DO
      FOR j := i + 1 TO LAST(a.rep^) DO
        IF EqualConjuncts(a.rep[i],a.rep[j]) THEN 
          <* ASSERT j < NUMBER(a.rep^) *>
          RETURN Simplify(NEW(T,rep := DeleteConjunct(a.rep,j)))
        END
      END
    END;
    <* ASSERT RepIsOK(a.rep) *>

    a.bool := NIL;
    <* ASSERT a.toBool() = aOld.toBool() *>

    RETURN a
  END Simplify;

PROCEDURE ConvertBool(bool : Bool.T) : T =
  VAR res, node : T; BEGIN
    IF    bool = Bool.False() THEN RETURN false 
    ELSIF bool = Bool.True() THEN RETURN true 
    END;

    node := NEW(T).init(BoolImpl.NodeVar(bool));
    
    (*Debug.Out("Sop.ConvertBool: node : " & node.format(NIL));*)

    (* see code in sop.c and bool.c for why this works *)
    res := Or( And(node, ConvertBool(BoolImpl.Left(bool))),
               And(Not(node), ConvertBool(BoolImpl.Right(bool))));

    (*Debug.Out("Sop.ConvertBool: res : " & res.format(NIL));*)

    RETURN res

  END ConvertBool;

PROCEDURE Equivalent( a, b : T) : BOOLEAN =
  BEGIN RETURN a.toBool() = b.toBool() END Equivalent;

(* this could be done much better *)
PROCEDURE Equal(a, b : T) : BOOLEAN = 
  BEGIN RETURN a = b END Equal;

(* this is OK though---we don't anticipate to have a lot of Sops that *)
(* are equivalent under Boolean algebra *)
PROCEDURE Hash(a : T) : Word.T = BEGIN RETURN Bool.Hash(a.toBool()) END Hash;

PROCEDURE CopyConjunct(READONLY a : Conjunct) : Conjunct =
  VAR res := NEW(Conjunct, NUMBER(a^)); BEGIN
    res^ := a^;
    RETURN res
  END CopyConjunct;

(* call copy before making destructive changes.. *)
PROCEDURE Copy(READONLY a : T) : T = 
  VAR res := NEW(T, rep := NEW(Rep,NUMBER(a.rep^))); BEGIN
    FOR i := FIRST(res.rep^) TO LAST(res.rep^) DO
      <* ASSERT a.rep[i] # NIL *>
      res.rep[i] := CopyConjunct(a.rep[i])
    END;
    RETURN res
  END Copy;

(* CleanNegatives attempts to weaken by deleting negatives first *)
PROCEDURE CleanNegatives(READONLY self : T; s : T; invariant : Bool.T) = 
  BEGIN
    FOR i := FIRST(s.rep^) TO LAST(s.rep^) DO
      WITH c = s.rep[i] DO
        VAR
          oldc : Conjunct;
        BEGIN
          FOR j := LAST(c^) TO FIRST(c^) BY -1 DO
            IF c[j].mode = FALSE THEN
              oldc := c;
              c := DeleteLiteral(c,j);
              IF Bool.And(s.toBool(),invariant) #
                Bool.And(self.toBool(),invariant) THEN
                c := oldc
              END
            END
          END
        END
      END
    END
  END CleanNegatives;

(* we may carry out any operations we please under the invariant, *)
(* but we may only weaken under the disjunctiveInvariant *)
(* Because the code is recursive and depends heavily on the garbage *)
(* collector, it is probably extremely inefficient. *)
PROCEDURE InvariantSimplify(self : T;
                            invariant,
                            weakeningInvariant,
                            eventualInvariant : Bool.T) : T =
  VAR 
    res : T; 
    fullInvariant := Bool.And(invariant,weakeningInvariant);
  BEGIN
    (* use a greedy algorithm to simplify Sop.T's *)
    (* use both invariants to weaken (remove literals) *)
    (* use only real invariant to strengthen (remove terms) *)

    (* the goal of this routine is to come up with a "simpler" *)
    (* sum-of-products implementation, as allowed by the invariants *)

    (* pre-process a bit *)
    res := Copy(Simplify(self));

    <* ASSERT RepIsOK(res.rep) *>

    SortSopConjunct.Sort(res.rep^);

    <* ASSERT res.toBool() = self.toBool() *>

    (* clean out negatives if possible *)
    CleanNegatives(self, res, fullInvariant);

    <* ASSERT Bool.And(fullInvariant,res.toBool()) = 
              Bool.And(fullInvariant,self.toBool()) *>

    (* first remove all conjuncts that are false under the invariant *)
    FOR i := LAST(res.rep^) TO FIRST(res.rep^) BY -1 DO
      IF Bool.And(FromConjunct(res.rep[i]).toBool(),
                  invariant) = Bool.False() THEN
        res.rep := DeleteConjunct(res.rep,i)
      END
    END;

    (* Greedily look at the longest conjunct first.
       If deleting a literal from a conjunct leaves the resulting
       expression still satisfying the equivalence, then continue
       with the new, simpler, expression. *)

    <* ASSERT RepIsOK(res.rep) *>

    VAR
      simplify := FALSE;
    BEGIN
      FOR i := LAST(res.rep^) TO FIRST(res.rep^) BY -1 DO
        (* for each conjunct, try removing literals, one by one *)
        WITH c = res.rep[i] DO 
          VAR 
            oldc : Conjunct; 
          BEGIN
            
            FOR j := LAST(c^) TO FIRST(c^) BY -1 DO
              oldc := c;
              c := DeleteLiteral(c,j);
              IF Bool.And(res.toBool(),fullInvariant) =
                Bool.And(self.toBool(),fullInvariant) THEN
                simplify := TRUE;

                IF NUMBER(c^) = 0 THEN
                  (* if we have deleted all the literals, it's always TRUE..*)
                  RETURN true
                END     
              ELSE
                c := oldc
              END
            END
        END END (* WITH c *)
      END; (* FOR i *)

      <* ASSERT RepIsOK(res.rep) *>

      IF simplify THEN 
        RETURN res.invariantSimplify(invariant,
                                     weakeningInvariant,
                                     eventualInvariant) 
      END
    END;

    <* ASSERT RepIsOK(res.rep) *>
    
    (* now try removing unnecessary conjuncts *)
    (* this is a little odd *)
    (* we CANNOT use the SEI here because it might "never" become true *)
    (* on the other hand, we can use the eventualInvariant, because *)
    (* that will "eventually" become true. *)
    VAR 
      oldRep := res.rep; 
    BEGIN
      FOR i := 0 TO LAST(res.rep^) DO
        <* ASSERT i < NUMBER(res.rep^) *>
        res.rep := DeleteConjunct(res.rep,i);
        IF Bool.And(res.toBool(),eventualInvariant) = 
           Bool.And(self.toBool(),eventualInvariant) THEN
          IF NUMBER(res.rep^) = 0 THEN
            (* if we have deleted all the conjuncts, it's always FALSE..*)
            RETURN false
          END;
          RETURN res.invariantSimplify(invariant,
                                       weakeningInvariant,
                                       eventualInvariant)
        ELSE
          res.rep := oldRep
        END
      END
    END;
    RETURN res
  END InvariantSimplify;

PROCEDURE DeleteLiteral(c : Conjunct ; idx : CARDINAL) : Conjunct =
  VAR res := NEW(Conjunct, NUMBER(c^) - 1); BEGIN
    (* hmm..if idx is 0 and NUMBER(c^) is 1, then we return TRUE... *)
    SUBARRAY(res^,0,idx) := SUBARRAY(c^,0,idx);
    IF idx + 1 <= LAST(c^) THEN
      SUBARRAY(res^,idx,NUMBER(res^) - idx) := 
          SUBARRAY(c^, idx + 1, NUMBER(c^) - (idx + 1))
    END;
    RETURN res
  END DeleteLiteral;

PROCEDURE Map(self : T; context : REFANY; mapProc : MapProc) : T =
  VAR res := false; BEGIN
    FOR i := FIRST(self.rep^) TO LAST(self.rep^) DO
      VAR cMap :=true; BEGIN
        FOR j := FIRST(self.rep[i]^) TO LAST(self.rep[i]^) DO
          cMap := And(cMap,mapProc(context,self.rep[i][j]))
        END;
        res := Or(cMap,res);
      END
    END;
    RETURN res
  END Map;

PROCEDURE MapSimple(self : T; mapProc : SimpleMapProc) : T =
  VAR res := false; BEGIN
    FOR i := FIRST(self.rep^) TO LAST(self.rep^) DO
      VAR cMap :=true; BEGIN
        FOR j := FIRST(self.rep[i]^) TO LAST(self.rep[i]^) DO
          cMap := And(cMap,mapProc(self.rep[i][j]))
        END;
        res := Or(cMap,res);
      END
    END;
    RETURN res
  END MapSimple;

(**************** debugging code ****************)

PROCEDURE RepIsOK(a : Rep) : BOOLEAN =
  BEGIN 
    FOR i := FIRST(a^) TO LAST(a^) DO
      IF a[i] = NIL THEN RETURN FALSE END;
      IF NUMBER(a^) > 1 AND NUMBER(a[i]^) = 0 THEN 
        Debug.Out("Non-trivial Sop with TRUE conjunct.");
        RETURN FALSE
      END
    END;
    RETURN TRUE
  END RepIsOK;

PROCEDURE ConjunctIsOK(c : Conjunct) : BOOLEAN =
  BEGIN 
    FOR i := FIRST(c^) TO LAST(c^) DO 
      IF c[i].var = NIL THEN RETURN FALSE END 
    END;
    RETURN TRUE 
  END ConjunctIsOK;

(****************************************)

PROCEDURE False() : T = BEGIN RETURN false END False;

PROCEDURE True() : T = BEGIN RETURN true END True;

VAR
  false, true : T;

BEGIN 
  false := NEW(T, rep := NEW(Rep, 0));

  true := NEW(T, rep := NEW(Rep, 1));
  true.rep[0] := NEW(Conjunct,0);

  IdentityMapper := NEW(AliasMapper, canon := IdentityCanon);
END SopG.




