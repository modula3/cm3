INTERFACE Evt;

  (* Users who only send events would import this interface plus
     possibly an interface defining new event types. Users who need
     to express interest and receive events need to import EvtRep as well
     to gain access to the callback method. *)

TYPE
  T <: Public;
  Public = OBJECT
    sender : INTEGER;
  END;

PROCEDURE ExpressInterest(v : T; includeSupertypes : BOOLEAN := FALSE );
PROCEDURE RevokeInterest(v : T);

(* send event v to all objects expressed interest. Return number of
   events delivered *)
PROCEDURE Send(v : T) : INTEGER;

PROCEDURE QueueEvent(v: T);

PROCEDURE DumpEvents();

END Evt.
