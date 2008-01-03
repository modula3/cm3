% Copyright (C) 1993 Digital Equipment Corporation
% All rights reserved.
% See the file COPYRIGHT for a full description. 
% Last modified on Wed Jun 23 12:40:37 PDT 1993 by horning

list(E): trait
  % Trait for sequences implemented as linked lists.
  % Hd$E and Tl$E correspond to the .head and .tail fields.
  includes
    string(E),
    lm3Store(Hd$E, L$E, E),
    lm3Store(Tl$E, L$E, L$E),
    Integer,
    set(L$E)
  introduces
    NIL: -> L$E
    seq:  L$E, Hd$E, Tl$E -> String$E
    acyclic: L$E, Tl$E -> Bool
    iTail:  L$E, Int,  Tl$E -> L$E
    fresh:  L$E, Tl$E, Tl$E -> Bool
    freshTo: L$E, L$E,  Tl$E, Tl$E -> Bool
    isPrefix: String$E, String$E -> Bool
    reach:  L$E, Tl$E -> Set$L$E
    last:   L$E, Tl$E -> L$E
  asserts
    \forall l, l1, l2: L$E, tails, tails': Tl$E, heads: Hd$E, i: Int, e: E,
            s1, s2: String$E

      seq(l, heads, tails) == if l = NIL then empty
         else (l.heads) -| seq(l.tails, heads, tails);

      acyclic(l, tails) == l = NIL
         \/ (l \notin reach(l.tails, tails) /\ acyclic(l.tails, tails));

      fresh(l, tails, tails') == l = NIL
         \/ (defined(tails', l) /\ ~defined(tails, l)
             /\ fresh(l.tails', tails, tails'));

      freshTo(l1, l2, tails, tails') == l1 = l2
         \/ (~defined(tails, l1) /\ defined(tails', l1)
             /\ freshTo(l1.tails', l2, tails, tails'));

      isPrefix(s1, s2) == s1 = prefix(s2, len(s1));

      iTail(l, 0, tails) == l;
      iTail(l, i+1, tails) == iTail(l.tails, i, tails);

      reach(l, tails) == if l = NIL then {}
                         else insert(l, reach(l.tails, tails));

      last(l, tails) == if l.tails = NIL then l else last(l.tails, tails);

  implies converts seq, fresh, reach, last:L$E,Tl$E->L$E
    exempting \forall tails: Tl$E last(NIL, tails)
