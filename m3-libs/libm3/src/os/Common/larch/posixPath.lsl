% Copyright (C) 1993 Digital Equipment Corporation
% All rights reserved.
% See the file COPYRIGHT for a full description. 
% Last modified on Wed Jun 23 12:09:23 PDT 1993 by horning

posixPath: trait
  % POSIX-specific definitions for pathnames

  includes osSigs
  asserts forall s: Str, b, only: Bool
    parent(POSIX) == {Period, Period};
    current(POSIX) == {Period};
    parse(s, POSIX) ==
      if len(s) > 0 /\ s[0] = Slash
        then {{Slash}} || split(tail(s), Slash)
        else {NIL} || split(s, Slash);      
    goodRoot(s, POSIX) == s = {Slash};
    goodName(s, POSIX) == ~(Slash \in s);

