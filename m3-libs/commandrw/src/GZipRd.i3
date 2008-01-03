INTERFACE GZipRd;

IMPORT Pathname, Rd, OSError;

TYPE T = Rd.T;

PROCEDURE Open (p: Pathname.T; ): T RAISES {OSError.E};
(* Open a gzipped file for reading unpacked data.

   Bug: If the file does not exist or something different fails, then you
   won't get the correct exception but a parsing error on subsequent read
   commands. *)

END GZipRd.
