% Copyright (C) 1993 Digital Equipment Corporation
% All rights reserved.
% See the file COPYRIGHT for a full description. 
% Last modified on Fri Jun 18 17:14:52 PDT 1993 by horning

permutation(E, C): trait
  % Provides an operator to test whether one container is
  % a permutation of another, by checking whether they are equal
  % as bags.
  assumes InsertGenerated(E, C, __-|__ for insert)
  includes Bag(E, Bag$E)
  introduces
    toBag: C -> Bag$E
    permutation: C, C -> Bool
  asserts \forall e: E, c,c1,c2: C
    toBag(empty) = {};
    toBag(e -| c) == insert(e, toBag(c));

    permutation(c1, c2) == toBag(c1) = toBag(c2);


