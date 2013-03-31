INTERFACE Private;
IMPORT Public;

REVEAL Public.Private = BRANDED "Public.Private" OBJECT
    b := 2;
METHODS
    F2() := F2;
END;

PROCEDURE F2(a:Public.Private);

END Private.
