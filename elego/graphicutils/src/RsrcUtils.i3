(*---------------------------------------------------------------------------*)
INTERFACE RsrcUtils;

IMPORT Rsrc, Rd;

(*---------------------------------------------------------------------------*)
EXCEPTION Error(TEXT);


(*---------------------------------------------------------------------------*)
PROCEDURE ListResource(rpath : Rsrc.Path; name : TEXT) RAISES {Error};

(*---------------------------------------------------------------------------*)
PROCEDURE PipeResource(rpath : Rsrc.Path; name : TEXT; cmd : TEXT)
  RAISES {Error};

(*---------------------------------------------------------------------------*)
PROCEDURE FilterResource(rpath : Rsrc.Path; name : TEXT; cmd : TEXT;
                         VAR rd : Rd.T) RAISES {Error};

(*---------------------------------------------------------------------------*)
PROCEDURE PageResource(rpath : Rsrc.Path; name : TEXT;
                       usePager := TRUE) RAISES {Error};


END RsrcUtils.
