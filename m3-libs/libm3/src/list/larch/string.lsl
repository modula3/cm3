% Copyright (C) 1993 Digital Equipment Corporation
% All rights reserved.
% See the file COPYRIGHT for a full description. 
% Last modified on Fri Jun 18 17:00:22 PDT 1993 by horning

string(E): trait

  % Handbook String trait (List plus index and substring),
  % plus a put operator.

  includes String(E, String$E)
  introduces put: String$E, Int, E -> String$E
  asserts \forall s: String$E, i, j: Int, e: E
    put(s, i, e)[j] == if i = j then e else s[j]