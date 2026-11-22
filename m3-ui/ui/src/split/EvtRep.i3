INTERFACE EvtRep;

IMPORT Evt;

REVEAL

  Evt.T = Evt.Public BRANDED OBJECT
    private : REFANY;
  METHODS
    callback();
  END;

END EvtRep.
