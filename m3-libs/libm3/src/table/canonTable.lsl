% Copyright (C) 1993 Digital Equipment Corporation
% All rights reserved.
% See the file COPYRIGHT for a full description. 
% Last modified on Fri Jun 18 14:31:26 PDT 1993 by horning

canonTable(K, V, Eq): trait
  % Canonical table; keys are treated modulo an equivalence relation.
  includes
    FiniteMap(Map$K$V, K, V),
    Relation(K, Eq),
    Integer
  introduces
    canon: K, Eq, Map$K$V -> K
    remove: K, Map$K$V -> Map$K$V
    size: Map$K$V -> Int
  asserts \forall k, k1, k2: K, eq: Eq, map: Map$K$V, v: V

    canon(k, eq, {}) == k;
    canon(k1, eq, update(map, k2, v)) ==
      if k1 \langle eq \rangle k2 then k2 else canon(k1, eq, map);

    remove(k, {}) == {};
    remove(k1, update(map, k2, v)) ==
      if k1 = k2 then remove(k1, map) else update(remove(k1, map), k2, v);

    size({}) == 0;
    size(update(map, k, v)) == size(remove(k, map)) + 1;

