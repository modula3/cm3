% Copyright (C) 1993 Digital Equipment Corporation
% All rights reserved.
% See the file COPYRIGHT for a full description. 
% Last modified on Wed Jun 23 18:27:57 PDT 1993 by horning

osSigs: trait
  % Signatures of OS-specific functions to be defined separately

  includes
    stringUtil,  % Defines printable, alphabetic, ...
    Sequence(Str, StrSeq)
  OS enumeration of POSIX, Win32, Mac  % ...
  introduces
    parent, current: OS -> Str
    goodRoot, goodName: Str, OS -> Bool
    parse: Str, OS -> StrSeq
