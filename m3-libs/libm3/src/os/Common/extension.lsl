% Copyright (C) 1993 Digital Equipment Corporation
% All rights reserved.
% See the file COPYRIGHT for a full description. 
% Last modified on Wed Jun 23 16:29:05 PDT 1993 by horning

extension: trait
  % Separate file names into base and extension

  includes stringUtil(Char, Str)
  introduces
    ext, base: Str -> Str
    trailDelim: Str -> Bool
  asserts forall s: Str, c: Char
    ext(empty) == empty;
    ext(s || {c}) ==
      if c ~= Period
         /\ (ext(s) ~= empty \/ trailDelim(s))
        then ext(s) || {c} 
        else empty;
    base(s) == if ext(s) = empty then s
               else prefix(s, len(s)-len(ext(s))-1);
    trailDelim(s) == len(s) > 1
                     /\ last(s) = Period
                     /\ last(init(s)) ~= Period;
