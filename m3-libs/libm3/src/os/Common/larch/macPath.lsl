% Copyright (C) 1993 Digital Equipment Corporation
% All rights reserved.
% See the file COPYRIGHT for a full description. 
% Last modified on Wed Jun 23 17:45:37 PDT 1993 by horning

macPath: trait
  % Macintosh-specific definitions for pathnames

  includes osSigs
  asserts forall s: Str, ch: Char
    parent(Mac) == empty;
    parse(s, Mac) ==
      if Colon \in s then split(s, Colon)
      else {NIL} || {s};
    goodRoot(s, Mac) ==
      1 <= len(s) /\ len(s) <= 27 /\ count(Colon, s) = 0;
