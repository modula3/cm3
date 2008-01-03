% Copyright (C) 1993 Digital Equipment Corporation
% All rights reserved.
% See the file COPYRIGHT for a full description. 
% Last modified on Wed Jun 23 17:46:24 PDT 1993 by horning

win32path: trait
  % Windows32-specific definitions for pathnames

  includes osSigs
  introduces
    winVolEnd, wve: Str -> Int
    winAbs: Str -> Bool
  asserts forall s: Str, ch: Char
    parent(Win32) == {Period, Period};
    current(Win32) == {Period};
    parse(s, Win32) ==
      if winAbs(s)
        then {prefix(s, winVolEnd(s))}
             || split(removePrefix(s, winVolEnd(s)+1), s[0]) 
      else if count(Bkslash, s) > 0 then {NIL} || split(s, Bkslash)
      else {NIL} || split(s, Slash);      
    winAbs(s) == s[1] = Colon 
      \/ ((s[0] = Slash \/ s[0] = Bkslash) /\ s[0] = s[1]);
    goodRoot(s, Win32) ==
      if s[1] = Colon then len(s) = 2 /\ alpha(s[0])
      else len(s) > 4
        /\((s[0] = Slash \/ s[0] = Bkslash) /\ s[0] = s[1])
        /\ count(s[0], removePrefix(s, 2)) = 1;
    goodName(s, Win32) == ~(Slash \in s \/ Bkslash \in s);
    winVolEnd(s) == if s[1] = Colon then 2
      else wve(s) + find(removePrefix(s, wve(s)), s[0]);
    wve(s) = 2 + find(removePrefix(s, 2), s[0]);
