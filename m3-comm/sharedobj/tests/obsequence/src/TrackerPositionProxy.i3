INTERFACE TrackerPositionProxy;

IMPORT TrackerPosition, TrackerPositionCB, EmbProxiedObj;

VAR 
  MkProxyT  : PROCEDURE (x: TrackerPosition.T)   := NIL;
  MkProxyCB : PROCEDURE (x: TrackerPositionCB.T) := NIL;

(* The "TrackerPositionCB" type is an abstract supertypes; concrete
   callbacks are created by overriding the various methods. We want to
   be able to do this not only from Modula-3, but also from the
   embedded language (e.g. Obliq). This is achieved by providing
   a special proxy type.

   The "proxy" field of a "TrackerPositionCB" must contain either NIL
   or an object of type "CallbackProxy". 
*)

TYPE
  CallbackProxy = EmbProxiedObj.Proxy OBJECT
  METHODS
    pre_set (READONLY obj: TrackerPosition.T; 
             READONLY val: TrackerPosition.Data): BOOLEAN;
    pre_anyChange(READONLY obj: TrackerPosition.T);
    post_set (READONLY obj: TrackerPosition.T; 
              READONLY val: TrackerPosition.Data): BOOLEAN;
    post_anyChange(READONLY obj: TrackerPosition.T);
  END;

END TrackerPositionProxy.
