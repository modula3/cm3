INTERFACE ScalarVar;
IMPORT CommandLoop;
IMPORT FmtScanVar;

PROCEDURE PutCommand(cl: CommandLoop.T;
                     fs: FmtScanVar.T;
                     typeName, name, desc: TEXT;
                     userChange: BOOLEAN);

END ScalarVar.
