INTERFACE TrackerPositionTst;

IMPORT Tracker, SharedObj, Thread;

CONST Brand = "TrackerPosition";

TYPE
  Data = REF Tracker.Report;

  <*PRAGMA SHARED*>

  T <: SharedObj.T;
  U <: SharedObj.T;
  
  <*SHARED UPDATE METHODS T.set, T.init *>
  <*SHARED UPDATE METHODS U.set *>

END TrackerPositionTst.
