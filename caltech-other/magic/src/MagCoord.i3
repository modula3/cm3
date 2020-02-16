INTERFACE MagCoord;

(* for 32-bit machine *)
CONST
  Infinity = 67108860; (* 2^24 - 6, see tile.h *)
  MInfinity = -Infinity;

TYPE
  T = [MInfinity .. Infinity + 1];

END MagCoord.



