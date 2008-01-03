% Copyright (C) 1993 Digital Equipment Corporation
% All rights reserved.
% See the file COPYRIGHT for a full description. 
% Last modified on Fri Jun 18 17:09:11 PDT 1993 by horning

lm3Store(St, T, Val): trait
  % Instantiated by LM3 once per object/record field type.
  includes
    FiniteMap(St, T, Val)
  introduces
    __.__: T, St -> Val
  asserts
    \forall t: T, st: St
      t.st == apply(st, t);
