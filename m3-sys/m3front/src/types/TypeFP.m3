(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: TypeFP.m3                                             *)
(* Last modified on Thu Jan 26 14:21:55 PST 1995 by kalsow     *)
(*      modified on Tue May 25 14:38:54 PDT 1993 by muller     *)

MODULE TypeFP;

IMPORT Text, Convert;
IMPORT M3, M3FP, M3Buf, Host, Type, TypeRep, ErrType;

(*
   The "scc_id" field of an "Node" "n" is used as follows:
     = NO_SCC => the "n" has never been visited
     > NO_SCC => the "n" is done, its fingerprint is in "n.fp"
     < NO_SCC => the "n" is on the active stack, stack[-n.scc_id] is its entry.
*)

CONST
  DFS_not_visited = 0;
  DFS_reset = DFS_not_visited + 1;
  NO_SCC = TypeRep.NO_SCC;

TYPE
  Node = Type.T;
  Info = RECORD
    node   : Node;
    info   : M3.FPInfo;
    dfs_id : INTEGER;
  END;

TYPE
  IntList  = REF ARRAY OF INTEGER;
  TypeList = REF ARRAY OF Type.T;

VAR n_reps     : INTEGER  := 0;
VAR reps       : TypeList := NIL;
VAR hash_table : IntList  := NIL;
VAR stack      : REF ARRAY OF Info;
VAR tos        : INTEGER := NO_SCC + 1;
VAR dfs_cnt    : INTEGER := DFS_reset;
VAR next_scc   : INTEGER := NO_SCC + 1;
VAR mbuf       : M3Buf.T := NIL;
VAR busy       : BOOLEAN := FALSE;

(*---------------------------------------------------------------------------*)

PROCEDURE Initialize () =
  BEGIN
    stack := NEW (REF ARRAY OF Info, 100);
    mbuf  := M3Buf.New ();
    reps  := NEW (TypeList, 256);
    ExpandHash ();
  END Initialize;

PROCEDURE Reset () =
  VAR t: Type.T;
  BEGIN
    (* remove the reps and reinstall them as needed in the next compilation *)
    FOR i := 0 TO n_reps - 1 DO
      t := reps[i];
      t.rep_id := TypeRep.NO_UID;
      t.scc_id := TypeRep.NO_SCC;
      reps[i] := NIL;
    END;
    (* clear the hash table *)
    FOR i := FIRST (hash_table^) TO LAST (hash_table^) DO
      hash_table[i] := TypeRep.NO_UID;
    END;
    n_reps := 0;
  END Reset;

(*-------------------------------------------------------- FP computation ---*)

PROCEDURE FromType (t: Type.T): M3FP.T =
  VAR n: Node;
  BEGIN
    <*ASSERT NOT busy *> busy := TRUE;
    IF (t = NIL) THEN RETURN M3FP.Zero; END;
    n := Type.Check (t);
    n := GetRep (n);
    IF (n.uid = TypeRep.NO_UID) THEN
      dfs_cnt := DFS_reset;
      EVAL Visit_SCC (n);
    END;
    IF (n # t) THEN
      t.fp  := n.fp;
      t.uid := n.uid;
    END;
    busy := FALSE;
    RETURN n.fp;
  END FromType;

PROCEDURE Visit_SCC (n: Node): INTEGER =
  (* Find the strongly connected component containing 'n' and
     compute the fingerprints of its elements *)
  VAR here, min, x: INTEGER;  ch: Node;
  BEGIN
    (* push 'n' on the stack *)
    here := tos;  INC (tos);
    IF (tos > NUMBER (stack^)) THEN ExpandStack () END;
    WITH z = stack[here] DO
      z.node := n;
      n.scc_id := -here;

      (* mark 'n' as visited *)
      INC (dfs_cnt);
      z.dfs_id := dfs_cnt;
      min := dfs_cnt;

      (* get 'n's tag and list of children *)
      z.info.tag := NIL;
      z.info.buf := mbuf;
      n.fprint (z.info);
      IF (z.info.tag = NIL) THEN z.info.tag := M3Buf.ToText (z.info.buf); END;
      z.info.buf := NIL;

      (* visit each of 'n's children *)
      FOR i := 0 TO z.info.n_nodes-1 DO
        IF (z.info.n_nodes <= NUMBER (z.info.nodes))
          THEN ch := Type.Check (z.info.nodes[i]);
          ELSE ch := Type.Check (z.info.others[i]);
        END;
        ch := GetRep (ch);
        x := ch.scc_id;
        IF (x = NO_SCC) THEN
          (* we've never visited 'ch' *)
          min := MIN (min, Visit_SCC (ch));
        ELSIF (x < NO_SCC) THEN
          (* 'ch' is active *)
          min := MIN (min, stack [-x].dfs_id);
        (*
        ELSE (x > 0) 
          => we've already computed the full fp of 'ch', therefore it's not
             part of the strongly connected component containing 'n'.
        *)
        END;
      END;

      IF (min = z.dfs_id) THEN
        (* stack[tos-1 .. here] is a new strongly connected component *)
        Finish_SCC (SUBARRAY (stack^, here, tos-here));
        tos := here;
      END;
    END; (* WITH z=stack[here] *)

    RETURN min;
  END Visit_SCC;

TYPE
  VisitInfo = RECORD
    dfs     : INTEGER;
    tags    : M3Buf.T;
    fp      : M3FP.T;
    fp_used : BOOLEAN;
  END;

PROCEDURE Finish_SCC (VAR x: ARRAY OF Info) =
(* Given the strongly connected component "x" where the fingerprints
   of all other reachable nodes are computed, compute the fingerprints
   of each node in "x". *)
  VAR min: CARDINAL;  zz: VisitInfo;  n_missing := 0;  n: Node;
  BEGIN
    IF NUMBER (x) <= 0 THEN RETURN END;

    (* check to see if we've already computed the fingerprints for
       this component *)
    FOR i := FIRST (x) TO LAST (x) DO
      IF (x[i].node.uid = TypeRep.NO_UID) THEN INC (n_missing); END;
    END;

    IF (n_missing > 0) THEN
      IF (Host.vs_debug) THEN DumpHeader () END;

      IF (NUMBER (x) = 1) AND (x[0].info.n_nodes = 0) THEN
        (* a simple leaf node *)
        n := x[0].node;
        n.fp  := FromText (x[0].info.tag);
        n.uid := M3FP.ToInt (n.fp);
        x[0].dfs_id := -1;
      ELSE
        (* tag each node with its index in the component *)
        FOR i := FIRST (x) TO LAST (x) DO  x[i].node.scc_id := -i;  END;

        (* find the "least" element *)
        min := 0;
        FOR i := 1 TO LAST (x) DO
          IF CompareInfo (x, i, min) < 0 THEN min := i END;
        END;

        (* reset the DFS tags *)
        FOR i := 0 TO LAST (x) DO x[i].dfs_id := DFS_not_visited; END;

        (* compute this component's fingerprint *)
        zz.dfs     := DFS_reset;
        zz.tags    := mbuf;
        zz.fp      := M3FP.Zero;
        zz.fp_used := FALSE;
        Visit (zz, x, min);
        IF (zz.fp_used)
          THEN zz.fp := FromPair (FromBuf (zz.tags), zz.fp);
          ELSE zz.fp := FromBuf (zz.tags);
        END;

        (* compute the fingerprint of each element of the component *)
        FOR i := FIRST (x) TO LAST (x) DO
          WITH z = x[i] DO
            z.node.fp  := M3FP.ExtendByInt (zz.fp, z.dfs_id);
            z.node.uid := M3FP.ToInt (z.node.fp);
          END;
        END;
      END;

      (* dump the final fingerprints *)
      IF (Host.vs_debug) THEN
        FOR i := FIRST (x) TO LAST (x) DO
          DumpFP (x[i].dfs_id, NIL, x[i].node.fp);
        END;
      END;

    END; (*IF n_missing > 0*)

    (* set the scc_id's and reset the stack nodes *)
    FOR i := FIRST (x) TO LAST (x) DO
      WITH z = x[i] DO
        FOR j := 0 TO MIN (z.info.n_nodes, LAST (z.info.nodes)) DO
          z.info.nodes[j] := NIL;
        END;
        z.node.scc_id  := next_scc;
        z.node         := NIL;
        z.info.tag     := NIL;
        z.info.buf     := NIL;
        z.info.others  := NIL;
        z.info.n_nodes := 0;
        z.dfs_id       := DFS_not_visited;
      END;
    END;
    INC (next_scc);
  END Finish_SCC;

PROCEDURE Visit (VAR zz: VisitInfo;  VAR x: ARRAY OF Info;  n: CARDINAL) =
  VAR ch: Node;
  BEGIN
    WITH z = x[n] DO
      IF (z.dfs_id # DFS_not_visited) THEN
        M3Buf.PutChar (zz.tags, '|');
        M3Buf.PutInt  (zz.tags, z.dfs_id);
        RETURN;
      END;

      (* this is the first visit to the node 'z' *)
      (* mark 'z' and add its tag *)
      z.dfs_id := zz.dfs;  INC (zz.dfs);
      M3Buf.PutChar (zz.tags, '|');
      M3Buf.PutText (zz.tags, z.info.tag);

      (* visit each of its children *)
      FOR i := 0 TO z.info.n_nodes-1 DO
        IF (z.info.n_nodes <= NUMBER (z.info.nodes))
          THEN ch := z.info.nodes[i];
          ELSE ch := z.info.others[i];
        END;
        ch := GetRep (ch);
        IF (ch.scc_id > NO_SCC) THEN
          (* child 'i' outside the component *)
          M3Buf.PutChar (zz.tags, '|');
          M3Buf.PutChar (zz.tags, '*');
          zz.fp := FromPair (zz.fp, ch.fp);
          zz.fp_used := TRUE;
        ELSE
          (* child 'i' inside the component *)
          Visit (zz, x, -ch.scc_id);
        END;
      END;
    END;
  END Visit;

PROCEDURE CompareInfo (READONLY x: ARRAY OF Info;  na, nb: CARDINAL): [-1..1] =
(* Determine if "x[na]" is less than or greater than "x[nb]".  Note that
   an order on the nodes is not defined by the language, but an arbitrary
   total order that is constant for the duration of "Finish_SCC"
   will do.  In fact, the order defined by this procedure changes
   from one call of "Finish_SCC" to the next. *)
  VAR z: INTEGER;  ch_a, ch_b: Node;
  BEGIN
    WITH a = x[na], b = x[nb] DO
      (* first, use the # of children *)
      IF (a.info.n_nodes < b.info.n_nodes) THEN RETURN -1 END;
      IF (a.info.n_nodes > b.info.n_nodes) THEN RETURN +1 END;

      (* next, use the tags *)
      z := Text.Compare (a.info.tag, b.info.tag);
      IF (z < 0) THEN RETURN -1 END;
      IF (z > 0) THEN RETURN +1 END;

      (* finally, check the children *)
      FOR i := 0 TO a.info.n_nodes-1 DO
        IF (a.info.n_nodes <= NUMBER (a.info.nodes)) THEN
          ch_a := a.info.nodes[i];
          ch_b := b.info.nodes[i];
        ELSE
          ch_a := a.info.others[i];
          ch_b := b.info.others[i];
        END;
        ch_a := GetRep (ch_a);
        ch_b := GetRep (ch_b);
        z := CompareNode (x, ch_a, ch_b);
        IF (z # 0) THEN RETURN z END;
      END;
      RETURN 0;
    END; (* WITH *)
  END CompareInfo;

PROCEDURE CompareNode (READONLY x: ARRAY OF Info;  a, b: Node): [-1..1] =
  VAR xa, xb: INTEGER;
  BEGIN
    IF (a = b)   THEN RETURN 0  END;
    IF (a = NIL) THEN RETURN -1 END;
    IF (b = NIL) THEN RETURN +1 END;

    xa := a.scc_id;
    xb := b.scc_id;

    IF (xa > NO_SCC) AND (xb > NO_SCC) THEN
      (* both nodes have already have fingerprints *)
      WITH fp_a = a.fp, fp_b = b.fp DO
        FOR i := 0 TO 7 DO
          IF fp_a.byte[i] < fp_b.byte[i] THEN RETURN -1 END;
          IF fp_a.byte[i] > fp_b.byte[i] THEN RETURN +1 END;
        END;
      END;
      RETURN 0;
    END;

    IF Type.IsEqual (a, b, NIL) THEN RETURN 0 END;

    IF (xa > NO_SCC) THEN RETURN +1 END;
    IF (xb > NO_SCC) THEN RETURN -1 END;
    RETURN CompareInfo (x, -xa, -xb);
  END CompareNode;

PROCEDURE ExpandStack () =
  VAR n   := NUMBER (stack^);
  VAR new := NEW (REF ARRAY OF Info, 2 * n);
  BEGIN
    SUBARRAY (new^, 0, n) := stack^;
    stack := new;
  END ExpandStack;

(*---------------------------------------------------------- FP utilities ---*)

PROCEDURE FromText (txt: TEXT): M3FP.T =
  VAR fp := M3FP.FromText (txt);
  BEGIN
    IF (Host.vs_debug) THEN DumpFP (-1, txt, fp); END;
    RETURN fp;
  END FromText;

PROCEDURE FromPair (READONLY a, b: M3FP.T): M3FP.T =
  VAR fp := M3FP.Combine (a, b);
  BEGIN
    IF (Host.vs_debug) THEN DumpOne (b); END;
    RETURN fp;
  END FromPair;

PROCEDURE FromBuf (buf: M3Buf.T): M3FP.T =
  BEGIN
    IF (Host.vs_debug)
      THEN RETURN FromText (M3Buf.ToText (buf));
      ELSE RETURN M3Buf.ToFP (buf); (* avoids the TEXT allocation *)
    END;
  END FromBuf;

(*----------------------------------------------------------- unique reps ---*)

PROCEDURE GetRep (t: Type.T): Type.T =
  VAR uid: INTEGER;  hash: INTEGER;  r: Type.T;
  BEGIN
    IF (t = NIL) THEN t := ErrType.T; END;

    IF (t.rep_id # TypeRep.NO_UID) THEN
      (* this type has a uid, but it might be from a old compilation... *)
      IF (t.rep_id < n_reps) THEN
        r := reps[t.rep_id];
        IF (r = t) OR Type.IsEqual (r, t, NIL) THEN RETURN r; END;
      END;
      t.rep_id := TypeRep.NO_UID;
    END;

    (* search the hash table for an existing type that's equal *)
    hash := t.info.hash MOD NUMBER (hash_table^);
    LOOP
      uid := hash_table[hash];
      IF (uid = TypeRep.NO_UID) THEN (* empty bucket *) EXIT END;
      r := reps[uid];
      IF (r.info.hash = t.info.hash) AND Type.IsEqual (r, t, NIL) THEN
        (* we found a match *)
        t.rep_id := uid;
        RETURN r;
      END;
      INC (hash); IF (hash > LAST(hash_table^)) THEN hash := 0 END;
    END;

    (* a new type! *)
    uid := n_reps;
    t.rep_id := uid;

    (* add a new type to the rep table *)
    IF (uid > LAST (reps^)) THEN ExpandReps () END;
    reps [uid] := t;
    INC (n_reps);

    (* update the hash table *)
    hash_table [hash] := uid;
    IF (n_reps + n_reps > NUMBER (hash_table^)) THEN ExpandHash () END;
    RETURN t;
  END GetRep;

PROCEDURE ExpandReps () =
  VAR n := NUMBER (reps^);
  VAR new := NEW (TypeList, 2 * n);
  BEGIN
    SUBARRAY (new^, 0, n_reps) := reps^;
    reps := new;
  END ExpandReps;

PROCEDURE ExpandHash () =
  VAR n, hash: INTEGER;  new: IntList;
  BEGIN
    IF (hash_table = NIL)
      THEN n := 512;
      ELSE n := 2 * NUMBER (hash_table^);
    END;
    new := NEW (IntList, n);

    (* initialize the new table to all empty slots *)
    FOR i := 0 TO LAST (new^) DO new[i] := TypeRep.NO_UID END;

    (* re-insert each type in the new table *)
    FOR i := 0 TO n_reps-1 DO
      hash := reps[i].info.hash MOD n;
      WHILE (new[hash] # TypeRep.NO_UID) DO
        INC (hash); IF (hash >= n) THEN hash := 0 END;
      END;
      new[hash] := i;
    END;

    hash_table := new;
  END ExpandHash;

(*------------------------------------------------------------- debugging ---*)

VAR wr: M3Buf.T := NIL;

PROCEDURE DumpHeader () =
  BEGIN
    Host.env.note_comment ("---------------------------------------");
  END DumpHeader;

PROCEDURE DumpFP (i: INTEGER;  tag: TEXT;  READONLY fp: M3FP.T) =
  <*FATAL Convert.Failed*>
  VAR len: INTEGER;  buf: M3FP.CharBuf;  fp_x := M3FP.ToInt (fp);
  BEGIN
    IF (wr = NIL) THEN wr := M3Buf.New () END;

    IF (tag # NIL) THEN
      Host.env.note_comment (tag);
    END;

    IF (i >= 0) THEN
      len := Convert.FromInt (buf, i);
      M3Buf.PutSub  (wr, SUBARRAY (buf, 0, len));
      M3Buf.PutText (wr, ": ");
    END;

    M3Buf.PutText (wr, "  FP ==> 16_");
    M3FP.ToChars (fp, buf);
    M3Buf.PutSub  (wr, buf);

    M3Buf.PutText (wr, " => 16_");
    len := Convert.FromUnsigned (buf, fp_x, 16);
    M3Buf.PutSub  (wr, SUBARRAY (buf, 0, len));

    M3Buf.PutText (wr, " = ");
    len := Convert.FromInt (buf, fp_x);
    M3Buf.PutSub  (wr, SUBARRAY (buf, 0, len));

    Host.env.note_comment (M3Buf.ToText (wr));
  END DumpFP;

PROCEDURE DumpOne (READONLY fp: M3FP.T) =
  VAR buf: M3FP.CharBuf;
  BEGIN
    IF (wr = NIL) THEN wr := M3Buf.New () END;
    M3Buf.PutText (wr, "  Combine ==> 16_");
    M3FP.ToChars (fp, buf);
    M3Buf.PutSub  (wr, buf);
    Host.env.note_comment (M3Buf.ToText (wr));
  END DumpOne;

BEGIN
END TypeFP.
