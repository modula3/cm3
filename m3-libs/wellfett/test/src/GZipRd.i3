INTERFACE GZipRd;

IMPORT Pathname, Rd, OSError;

TYPE T <: Rd.T;

PROCEDURE Open (p: Pathname.T; ): T RAISES {OSError.E};

END GZipRd.
