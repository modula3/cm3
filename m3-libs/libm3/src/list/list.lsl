% Copyright (C) 1993 Digital Equipment Corporation
% All rights reserved.
% See the file COPYRIGHT for a full description. 
% Last modified on Fri Jun 18 17:07:55 PDT 1993 by horning

list(E): trait
  % Trait for sequences implemented as linked lists.
  % L$Hd and L$Tl correspond to the .head and .tail fields.
  includes
    string(E),
    lm3Store(L$Hd, L, E),
    lm3Store(L$Tl, L, L),
    Integer,
    set(L)
  introduces
    NIL: -> L
    seq:  L, L$Hd, L$Tl -> String$E
    acyclic: L, L$Tl -> Bool
    iTail:  L, Int,  L$Tl -> L
    fresh:  L, L$Tl, L$Tl -> Bool
    freshTo: L, L,  L$Tl, L$Tl -> Bool
    isPrefix: String$E, String$E -> Bool
    reach:  L, L$Tl -> Set$L
    last:   L, L$Tl -> L
  asserts
    \forall l, l1, l2: L, tails, tails': L$Tl, heads: L$Hd, i: Int, e: E,
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

  implies converts seq, fresh, reach, last:L,L$Tl->L
    exempting \forall tails: L$Tl last(NIL, tails)
