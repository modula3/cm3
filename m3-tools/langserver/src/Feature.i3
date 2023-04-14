INTERFACE Feature;

IMPORT M3Context;

PROCEDURE GetSymbolType(c: M3Context.T; unit : TEXT; line,col : INTEGER;
                        VAR startCol,len : INTEGER) : TEXT;
PROCEDURE GetDeclaration(c: M3Context.T; unit : TEXT; line,col : INTEGER;
                     VAR defLine,defCol,defLen : INTEGER) : TEXT;
PROCEDURE GetTypeDefinition(c: M3Context.T; unit : TEXT; line,col : INTEGER;
                            VAR defLine,defCol,defLen : INTEGER) : TEXT;

END Feature.
