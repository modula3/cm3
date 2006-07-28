INTERFACE UnitTestAtomRep;

IMPORT UnitTestAtom, UnitTestTerminal, TextSeq;

REVEAL UnitTestAtom.T <: Public;

TYPE
  Public = UnitTestAtom.Public BRANDED OBJECT
             name    : TEXT;
             errors  : TextSeq.T;
             terminal: UnitTestTerminal.T;
           END;

END UnitTestAtomRep.
