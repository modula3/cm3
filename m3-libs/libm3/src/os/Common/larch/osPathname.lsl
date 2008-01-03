% Copyright (C) 1993 Digital Equipment Corporation
% All rights reserved.
% See the file COPYRIGHT for a full description. 
% Last modified on Wed Jun 23 12:04:43 PDT 1993 by horning

osPathname: trait
  % Collected pathname definitions for the various OSs

  includes
    stringUtil,  % Defines printable, alphabetic, ...
    posixPath,
    win32path,
    macPath 
  introduces
    parse: Str -> StrSeq
    goodRoot, goodName: Str -> Bool
    os: -> OS
  asserts forall s: Str, ss: StrSeq, ch: Char
    parse(s) == parse(s, os);
    goodRoot(s) == printable(s) /\ (s = empty \/ goodRoot(s, os));
    goodName(s) == printable(s) /\ goodName(s, os);

