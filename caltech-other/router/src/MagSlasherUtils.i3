INTERFACE MagSlasherUtils;
IMPORT Pathname;
IMPORT MagSlasher;
TYPE
  T = MagSlasher.T;

PROCEDURE FromMagPath(path: Pathname.T): T;
(* crashes process with a meaningful message if there is an error *)

END MagSlasherUtils.
