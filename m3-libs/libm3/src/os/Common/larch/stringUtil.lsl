% Copyright (C) 1993 Digital Equipment Corporation
% All rights reserved.
% See the file COPYRIGHT for a full description. 
% Last modified on Wed Jun 23 12:20:56 PDT 1993 by horning

stringUtil(Char, Str): trait
  % Convenient operations on strings for pathname manipulation

  includes
    String(Char, Str),
    Sequence(Str, StrSeq)
  introduces
    Slash, Bkslash, Colon, Period: -> Char
    alpha: Char -> Bool 
    printable: Str -> Bool
    NIL: -> Str
% The above functions would have the obvious definitions
    {__, __}: Char, Char -> Str
    split: Str, Char -> StrSeq
    first, rest: Str, Char -> Str
    find: Str, Char -> Int
  asserts forall s: Str, ss: StrSeq, ch, ch1: Char
    {ch, ch1} == {ch} || {ch1};
    split(s, ch) ==
      if ch \in s
        then {first(s, ch)} || split(rest(s, ch), ch)
        else {s};
    first(s, ch) == prefix(s, find(s, ch));
    rest(s, ch) == removePrefix(s, find(s, ch)+1);
    find(ch1 \precat s, ch) ==
      if ch1 = ch then 0 else find(s, ch) + 1;
