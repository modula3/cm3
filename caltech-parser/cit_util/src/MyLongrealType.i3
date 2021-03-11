INTERFACE MyLongrealType;
IMPORT LongrealType AS Super;

TYPE T = Super.T;

CONST Equal = Super.Equal;
CONST Hash  = Super.Hash;
CONST Compare = Super.Compare;

CONST Brand = Super.Brand;

PROCEDURE Copy(a : T) : T;

END MyLongrealType.
