INTERFACE BTree;

CONST
  N = 3; (* arbitrary - set low to trigger overflow and underflow tests *)
  PageSize = 2 * N;

TYPE
  Page = REF PageRec;

  Entry = RECORD
    key: INTEGER;
    p: Page
  END;

  PageRec = RECORD
    pageNum : INTEGER;
    m: INTEGER; (*no. of entries on page*)
    p0: Page;
    e: ARRAY [0..PageSize-1] OF Entry
  END;

PROCEDURE Search(root : Page; x: INTEGER; VAR p: Page; VAR k: INTEGER) : BOOLEAN;
PROCEDURE InsertKey(VAR p : Page; k: INTEGER);
PROCEDURE DeleteKey(VAR p : Page; k: INTEGER);

PROCEDURE ShowTree(p: Page; level: INTEGER);
PROCEDURE ValidTree (p: Page);

END BTree.
