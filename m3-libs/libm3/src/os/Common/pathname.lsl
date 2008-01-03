% Copyright (C) 1993 Digital Equipment Corporation
% All rights reserved.
% See the file COPYRIGHT for a full description. 
% Last modified on Wed Jun 23 18:42:22 PDT 1993 by horning

pathname: trait               
  % OS-independent definitions for path names

  includes
    osPathname
  introduces
    absolute, valid, goodSeq: StrSeq -> Bool
    problem, prob: StrSeq -> Str
  asserts forall s: Str, ss: StrSeq
    absolute(ss) == head(ss) = NIL;
    valid(ss) == goodRoot(head(ss)) /\ goodSeq(tail(ss));
    goodSeq(ss) ==
      len(ss) = 0
      \/ (goodName(head(ss)) /\ goodSeq(tail(ss)));
    problem(ss) ==
      if goodRoot(head(ss)) then prob(tail(ss)) else head(ss);
    prob(ss) ==
      if goodName(head(ss)) then prob(tail(ss))
      else head(ss);

             
