(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed May 10 19:22:29 PDT 1995 by heydon                   *)
(*      modified on Sun Jun  5 15:47:38 PDT 1994 by gnelson                  *)

MODULE RTVal;

IMPORT JunoValue, InUseRec, InUseRecSeq;
IMPORT Text AS TextIntf;  (* to avoid name clash with "RTVal.Text" type *)

IMPORT Wr, Fmt, Stdio, Thread;
<* FATAL Wr.Failure, Thread.Alerted *>
VAR debug := FALSE;

REVEAL 
  Number = NumberPublic BRANDED "RTVal.Number" OBJECT link: Number END;
  Text = TextPublic BRANDED "RTVal.Text" OBJECT link: Text END;
  Pair = PairPublic BRANDED "RTVal.Pair" OBJECT link: Pair END;

VAR 
  markStack := NEW(InUseRecSeq.T).init();
  numAvail, numInUse: Number := NIL;
  textAvail, textInUse: Text := NIL;
  pairAvail, pairInUse: Pair := NIL;

PROCEDURE FromReal(r: Real): Number =
  VAR res := numAvail; BEGIN
    IF res = NIL
      THEN res := NEW(Number)
      ELSE numAvail := numAvail.link
    END;
    res.val := r;
    res.link := numInUse;
    numInUse := res;
    RETURN res
  END FromReal;

PROCEDURE FromInt(i: INTEGER): Number =
  VAR res := numAvail; BEGIN
    IF res = NIL
      THEN res := NEW(Number)
      ELSE numAvail := numAvail.link
    END;
    res.val := FLOAT(i, Real);
    res.link := numInUse;
    numInUse := res;
    RETURN res
  END FromInt;

PROCEDURE FromText(txt: TEXT): Text =
  VAR res := textAvail; BEGIN
    <* ASSERT txt # NIL *>
    IF res = NIL
      THEN res := NEW(Text) 
      ELSE textAvail := textAvail.link 
    END;
    res.val := txt;
    res.link := textInUse;
    textInUse := res;
    RETURN res
  END FromText;

PROCEDURE FromPair(car, cdr: T): Pair =
  VAR res := pairAvail; BEGIN
    <* ASSERT car # NIL AND cdr # NIL *>
    IF res = NIL
      THEN res := NEW(Pair)
      ELSE pairAvail := pairAvail.link
    END;
    res.car := car;
    res.cdr := cdr;
    res.link := pairInUse;
    pairInUse := res;
    RETURN res
  END FromPair;
  
PROCEDURE FromJV(jv: JunoValue.T): T =
  BEGIN
    TYPECASE jv OF <*NOWARN*>
      NULL => RETURN NIL
    | JunoValue.Null => RETURN nil
    | REF Real (r) => RETURN FromReal(r^)
    | TEXT (t) => RETURN FromText(t)
    | REF JunoValue.Pair (r) => RETURN FromJVPair(r)
    END
  END FromJV;

PROCEDURE FromJVPair(pr: REF JunoValue.Pair): Pair =
(* Equivalent to "RETURN FromPair(FromJV(pr.car), FromJV(pr.cdr))", but
   uses fewer stack frames in the case that "pr" is a long list.

   Note: The calls to "FromPair" belowonly need to pass a valid first
   argument. They pass the same second argument only because "FromPair"'s
   arguments must be non-NIL. The bogus "cdr" value gets overwritten on the
   next iteration, or after the loop. *)
  VAR
    car: T := FromJV(pr.car);
    res: Pair := FromPair(car, car);
    curr: Pair := res;
  BEGIN
    LOOP
      TYPECASE pr.cdr OF
        NULL => EXIT
      | REF JunoValue.Pair (newPr) =>
          car := FromJV(newPr.car);
          curr.cdr := FromPair(car, car);
          pr := newPr;
          curr := curr.cdr
      ELSE EXIT
      END
    END;
    curr.cdr := FromJV(pr.cdr);
    RETURN res
  END FromJVPair;

PROCEDURE ToJV(v: T): JunoValue.T =
  BEGIN
    TYPECASE v OF <* NOWARN *>
      NULL => RETURN NIL
    | Null => RETURN JunoValue.Nil
    | Number (r) => RETURN JunoValue.RefReal(r.val)
    | Text (t) => RETURN t.val
    | Pair (p) => RETURN ToJVPair(p)
    END
  END ToJV;

PROCEDURE ToJVPair(pr: Pair): REF JunoValue.Pair =
(* Equivalent to:
|
|    RETURN NEW(REF JunoValue.Pair,
|      car := ToJV(pr.car), cdr := ToJV(pr.cdr))
|
   but uses fewer stack frames in the case that "pr" is a long list. *)
  VAR
    res := NEW(REF JunoValue.Pair,
      car := ToJV(pr.car), cdr := NIL);
    curr := res;
  BEGIN
    LOOP
      TYPECASE pr.cdr OF
        NULL => EXIT
      | Pair (newPr) =>
          curr.cdr := NEW(REF JunoValue.Pair,
            car := ToJV(newPr.car), cdr := NIL);
          pr := newPr;
          curr := curr.cdr
      ELSE EXIT
      END
    END;
    curr.cdr := ToJV(pr.cdr);
    RETURN res
  END ToJVPair;

PROCEDURE Equal(v, w: T): BOOLEAN =
  BEGIN
    IF v = NIL OR w = NIL THEN RETURN FALSE END;
    TYPECASE v OF <*NOWARN*>
      Null => RETURN w = nil
    | Number (vv) =>
        TYPECASE w OF Number (ww) =>
          RETURN vv.val = ww.val
        ELSE RETURN FALSE
        END
    | Text (vv) =>
        TYPECASE w OF Text (ww) =>
          RETURN TextIntf.Equal(vv.val, ww.val)
        ELSE RETURN FALSE
        END
    | Pair (vv) =>
        TYPECASE w OF Pair (ww) =>
          RETURN EqualPair(vv, ww)
        ELSE RETURN FALSE
        END
    END
  END Equal;

PROCEDURE EqualPair(p1: Pair; p2: Pair): BOOLEAN =
(* Equivalent to "RETURN Equal(p1.car, p2.car) AND Equal(p1.cdr, p2.cdr)", but
   uses fewer stack frames when "p1" and "p2" are long lists. *)
  BEGIN
    LOOP
      IF NOT Equal(p1.car, p2.car) THEN RETURN FALSE END;
      TYPECASE p1.cdr OF
        NULL => RETURN FALSE
      | Pair (newP1) =>
          TYPECASE p2.cdr OF
            NULL => RETURN FALSE
          | Pair (newP2) => p1 := newP1; p2 := newP2
          ELSE RETURN FALSE
          END    
      ELSE EXIT
      END
    END;
    RETURN Equal(p1.cdr, p2.cdr)
  END EqualPair;

PROCEDURE Mark() =
  VAR r := InUseRec.T{numInUse, textInUse, pairInUse}; BEGIN
    markStack.addhi(r);
    numInUse := NIL;
    textInUse := NIL;
    pairInUse := NIL
  END Mark;

PROCEDURE Dispose() =
  VAR deletedAny := FALSE; BEGIN
    IF debug THEN
      Wr.PutText(Stdio.stderr, "RTVal.Dispose:\n");
      Wr.Flush(Stdio.stderr)
    END;
    DisposeNum(deletedAny);
    DisposeText(deletedAny);
    DisposePair(deletedAny);
    IF markStack.size() > 0 THEN
      VAR r := markStack.remhi(); BEGIN
    	numInUse := r.numInUse;
    	textInUse := r.textInUse;
    	pairInUse := r.pairInUse
      END
    END;
    IF debug THEN
      IF NOT deletedAny THEN
        Wr.PutText(Stdio.stderr, "  Nothing deleted\n")
      END;
      Wr.PutChar(Stdio.stderr, '\n');
      Wr.Flush(Stdio.stderr)
    END
  END Dispose;

PROCEDURE DisposeNum(VAR deletedAny: BOOLEAN) =
  VAR l := numInUse; cnt := 1; BEGIN
    IF l = NIL THEN RETURN END;
    WHILE l.link # NIL DO l := l.link; INC(cnt) END;
    l.link := numAvail;
    numAvail := numInUse;
    numInUse := NIL;
    IF debug THEN
      deletedAny := TRUE;
      Wr.PutText(Stdio.stderr, Fmt.Pad(Fmt.Int(cnt), 7));
      Wr.PutText(Stdio.stderr, " number(s)\n"); Wr.Flush(Stdio.stderr)
    END
  END DisposeNum;

PROCEDURE DisposeText(VAR deletedAny: BOOLEAN) =
  VAR l := textInUse; cnt := 1; BEGIN
    IF l = NIL THEN RETURN END;
    WHILE l.link # NIL DO l := l.link; INC(cnt) END;
    l.link := textAvail;
    textAvail := textInUse;
    textInUse := NIL;
    IF debug THEN
      deletedAny := TRUE;
      Wr.PutText(Stdio.stderr, Fmt.Pad(Fmt.Int(cnt), 7));
      Wr.PutText(Stdio.stderr, " text(s)\n"); Wr.Flush(Stdio.stderr)
    END
  END DisposeText;

PROCEDURE DisposePair(VAR deletedAny: BOOLEAN) =
  VAR l := pairInUse; cnt := 1; BEGIN
    IF l = NIL THEN RETURN END;
    WHILE l.link # NIL DO l := l.link; INC(cnt) END;
    l.link := pairAvail;
    pairAvail := pairInUse;
    pairInUse := NIL;
    IF debug THEN
      deletedAny := TRUE;
      Wr.PutText(Stdio.stderr, Fmt.Pad(Fmt.Int(cnt), 7));
      Wr.PutText(Stdio.stderr, " pair(s)\n"); Wr.Flush(Stdio.stderr)
    END
  END DisposePair;
  
BEGIN
  nil := NEW(Null)
END RTVal.
