INTERFACE RefRecord;
TYPE
  T = REFANY;
CONST
  Brand = "RefRecord";
PROCEDURE Format(refRecord: T): TEXT;
PROCEDURE Hash(refRecord: T): INTEGER;
PROCEDURE Equal(a,b: T): BOOLEAN;
END RefRecord.
