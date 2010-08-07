(*---------------------------------------------------------------------------*)
INTERFACE ConfigRsrcService;

IMPORT RsrcService, Rsrc, MsgIF;

(*---------------------------------------------------------------------------*)
TYPE
  T <: Public;
  Public = RsrcService.T OBJECT
  METHODS
    init(internalRsrcs : Rsrc.Path := NIL;
         msgif : MsgIF.T := NIL) : T RAISES {};
  END;

END ConfigRsrcService.
