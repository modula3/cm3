(* $Id$ *)

INTERFACE GridRouter2;
IMPORT MagRouter;
IMPORT Wr;
IMPORT RouterDeferralList;

TYPE
  ResultMethod = { UseSubCell, DumpInSelf };

TYPE Public = MagRouter.T OBJECT
  METHODS
    (* default is to insert a subcell in the passed-in top-level cell *)
    initResultMethod(resultMethod := ResultMethod.UseSubCell) : T;
    initDelWr(delWr : Wr.T) : T;

    setDeferrals(deferrals : RouterDeferralList.T; 
                 keepInternalDeferredWiring : BOOLEAN);
(* set routes to defer; routes must have LabelLists properly defined! *)
  END;

CONST WiringCellId = "gen_wiring";
(* default name of wiring cell -- exported here so that other tools that
   depend on it can know what it's called, specifically used by myrope
   ("myriphus") when it flattens cells. *)

TYPE T <: Public;

CONST Brand = "GridRouter2";

END GridRouter2.
