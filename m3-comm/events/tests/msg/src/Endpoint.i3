INTERFACE Endpoint;

IMPORT NetObj, Thread, Wr, EventSpaceID, Event, Rd;

TYPE
  T = NetObj.T OBJECT METHODS 
    space(): EventSpaceID.T RAISES {Thread.Alerted, NetObj.Error};
    connect (id: EventSpaceID.T; rd: Rd.T; wr: Wr.T)
      RAISES {Thread.Alerted, NetObj.Error, Event.Error};
    (*
    connect (id: EventSpaceID.T; wr: Wr.T): Wr.T
      RAISES {Thread.Alerted, NetObj.Error, Event.Error};
    *)
    disconnect (id: EventSpaceID.T) RAISES {Thread.Alerted,
                                            NetObj.Error, Event.Error}; 
  END;

END Endpoint.
