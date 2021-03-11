(* $Id$ *)

INTERFACE SchemeInputPortClass;
IMPORT SchemeInputPort;
FROM Scheme IMPORT E;

REVEAL SchemeInputPort.T <: Private;

TYPE
  Private = SchemeInputPort.Public OBJECT METHODS
    fastGetCh() : INTEGER RAISES { E };
    lock();
    unlock();
  END;

END SchemeInputPortClass.
    
