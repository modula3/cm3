INTERFACE BTree;

CONST
  MaxKeys = 10;

EXCEPTION Error(TEXT);
EXCEPTION EOF;

CONST
  MaxKeyLen = 32;

TYPE
  KeyRange = [1..MaxKeys];
  DataRef = INTEGER;

  KeyType = ARRAY[0..MaxKeyLen-1] OF CHAR;

  T <: Public;
  Public = OBJECT METHODS
    create(fName : TEXT; numKeys : KeyRange; blockLen : CARDINAL) : T;
    open(fName : TEXT) : T RAISES{Error};
    close();
    insertKey(READONLY key : KeyType; dRef : DataRef; keyNum : KeyRange) : BOOLEAN RAISES{Error};
    deleteKey(READONLY key : KeyType; keyNum : KeyRange) : BOOLEAN RAISES{Error};
    findKey(READONLY key : KeyType; keyNum : KeyRange; VAR dr : DataRef) : BOOLEAN RAISES{Error};
    addRec(a : REFANY) : DataRef;
    delRec(dRef : DataRef);
    putRec(dRef : DataRef; a : REFANY);
    getRec(dRef : DataRef; a : REFANY) RAISES {EOF};
    iterate() : Iterator;
    print(keyNum : KeyRange) RAISES{Error};
    printInfo();
  END;

  (* To iterate over the data file we need this. *)
  Iterator = OBJECT METHODS
    init() : Iterator;
    next(data : REFANY) : BOOLEAN;
  END;

END BTree.

(* A Btree.T is representation of data file containging a BTree index and
  associated datafile.

  The call "bt.create(name,numKeys,blockLen) creates a new Btree instancei
  and a pair of files with names "name".idx and "name".dat. 
  The keyNum parm specifies the maximum number of keys that can be 
  stored with this BTree and the blockLen specifies the bytesize 
  of the associated data record.

  The call "bt.open(name)" opens the previously created Btree using
  name as the path and filename prefix.

  The call "bt.close()" closes a previously opened BTree and flushes
  all associated data to disk.

  The call "bt.insertKey(key,dref,keynum)" attempts to insert the key "key"
  into the index file using key number keynum and data offset dref. It
  returns TRUE if the key did not exist and is now inserted or FALSE if
  the key was found to exist. A subsequent search for the key will return
  the data offset to be used as the index to do a getRec(dref, rec) to rapidly
  read the data record associated with the key. The same data record can
  be associated with multiple keys.

  The call "bt.deleteKey(key,keynum)" attempts to delete the key "key"
  from the index file.
*)
