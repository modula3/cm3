INTERFACE TrackerPositionTstF;

IMPORT SharedObj;
FROM TrackerPositionTst IMPORT T, Brand, Data;
 
TYPE
  Public = SharedObj.T OBJECT
           METHODS
             init (): T;
             set (READONLY val: Data);
             get (): Data;
             <*SHARED UPDATE METHODS set, init *>
           END;

REVEAL
   T = Public BRANDED Brand OBJECT
      OVERRIDES
        init := Init;
        set := Set;
        get := Get;
      END;

PROCEDURE Init (self: T): T;

PROCEDURE Set (self: T; READONLY val: Data);

PROCEDURE Get (self: T): Data; 

END TrackerPositionTstF.
