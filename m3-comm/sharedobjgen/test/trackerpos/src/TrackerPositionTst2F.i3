INTERFACE TrackerPositionTst2F;

FROM TrackerPositionTst IMPORT T, U, Brand, Data;
FROM TrackerPositionTstF IMPORT Public;
 
REVEAL
   U = Public BRANDED Brand OBJECT
      OVERRIDES
        init := Init;
        set := Set;
        get := Get;
        <*SHARED UPDATE METHODS U.set, U.init *>
      END;

PROCEDURE Init (self: U): T;

PROCEDURE Set (self: U; READONLY val: Data);

PROCEDURE Get (self: U): Data; 

END TrackerPositionTst2F.
