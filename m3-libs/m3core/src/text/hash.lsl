% Copyright (C) 1993 Digital Equipment Corporation
% All rights reserved.
% See the file COPYRIGHT for a full description. 
% Last modified on Fri Jun 18 14:31:18 PDT 1993 by horning

hash(T, R): trait
  % Gives the signature of a hash function from T to R, specifying
  % no properties--other than that it is a function, i.e.,
  %
  %     x = y => hash(x) = hash(y)

  introduces hash: T -> R
