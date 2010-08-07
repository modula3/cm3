(*--------------------------------------------------------------------------*)
INTERFACE FileInfo;
(*file status cache*)

(*--------------------------------------------------------------------------*)
IMPORT Fingerprint, TextSeq, Wr;
IMPORT APN, APNSeq, FileStatus, FindExpr, MsgIF;
IMPORT TextHashTbl, APNHashTbl AS AbsPathnHashTbl;

(*--------------------------------------------------------------------------*)
CONST Brand = "FileInfo";

(*--------------------------------------------------------------------------*)
TYPE 
  T <: Public;
  (*object type for file status caches*)

  Public = OBJECT METHODS

    (*----------------------------------------------------------------------*)
    init (sizeHint : CARDINAL; base : APN.T; 
          msgif : MsgIF.T := NIL) : T;
    (*Initialization with a size hint and a base path.
      The base path is present to make path processing more efficient.
      All subsequent operations are performed relative to this path.

      In particular, note that any parameter of p in the methods below
      is internally concatenated with base. As a consequence, it is
      inefficient to initialize base to, say, "." and instantiate p to the 
      same path, because this will lead to internal pathnames 
      with a prefix of "././". The right thing to do in this case is to 
      instantiate base to "." and p to "". Generally speaking, one should 
      always make base as long and p as short as possible. 

      If base is NIL or has got an empty text denotation, then
      it will be ignored in all cache operations.

      Error Conditions:
        invalid instantiation of base*)

    (*----------------------------------------------------------------------*)
    getBase () : APN.T;
    (*return the base pathname
     
      Error Conditions
        self = NIL*)

    (*----------------------------------------------------------------------*)
    size () : CARDINAL;
    (*return the number of entries
      
      Error Conditions:
        self = NIL*)

    (*----------------------------------------------------------------------*)
    getStatus (p : APN.T) : FileStatus.T;
    (*Look up the status of the file or directory b/p, where b is the base 
      path. Do not >update< the status. 

      If the cache does not contain any status entry for b/p, then
      create one and return it. The exists-flag 
      contained in this entry is set to false.

      Note that getStatus will return exists = false if p is of the form 
      ./q and, at the same time, the status record in the cache is 
      associated not with ./q but with q, and conversely.

      If p is an absolute pathname or base is undefined, then just p
      will be used for the operations described above.

      Error Conditions:
        self = NIL 
        invalid instantiation of p*)

    (*----------------------------------------------------------------------*)
    update (READONLY p : APN.T) : FileStatus.T;
    (*Update the status of the file b/p, where b is the base path,
      and return it.

      If p is an absolute pathname or base is undefined, then just p
      will be used for the operations described above.

      Error Conditions:
        self = NIL
        invalid instantiation of p*)

    (*----------------------------------------------------------------------*)
    updateRec (
        p : APN.T; 
        ext : TextSeq.T; 
        ignoreDirs : APNSeq.T;
        ignoreDirExpr : FindExpr.T := NIL; 
        ignoreFileExpr : FindExpr.T := NIL
    );
    (*Update recursively starting from b/p, where b is the base path.
      As for files, update an entry for a file only if its extension
      is in ext, but update an entry for each file if ext is empty; 
      as for directories, do not traverse any directory whose name is in 
      ignoreDirs, or whose name is matched by ignoreDirExpr. Do not
      cache any file name matched by ignoreFileExpr.

      If p is an absolute pathname or base is undefined, then just p
      will be used for the operations described above.

      Error Conditions:
        self = NIL 
        invalid instantiation of p

      Remarks
        ignoreDirs = NIL is allowed and has the expected effect.*)

    (*----------------------------------------------------------------------*)
    updateFlat (
        p : APN.T; 
        ext : TextSeq.T;
        ignoreDirExpr : FindExpr.T := NIL; 
        ignoreFileExpr : FindExpr.T := NIL
    ) : APNSeq.T ;
    (*Update non-recursively starting from b/p, where b is the base path,
      and return a list of all updated entries.
      As for files, update an entry for a file only if its extension
      is in ext, but update an entry for each file if ext is empty;
      as for directories, do not traverse any of them.
      Do not update any files whose name matches ignoreFileExpr.

      If p is an absolute pathname or base is undefined, then just p
      will be used for the operations described above.

      Error Conditions:
        self = NIL
        invalid instantiation of p*)

    (*----------------------------------------------------------------------*)
    getDir(READONLY p : APN.T) : APNSeq.T;
    (* Get all filenames of directory `p' that are contained in the
       cache. If `p' is not contained in the cache, this method
       returns NIL. *)

    (*----------------------------------------------------------------------*)
    dirList(p : APN.T; long := TRUE) : TEXT;
    (* Get a listing of directory `p', that is all information of `p'
       that is contained in the cache. If `long' is true, then the
       file type and modification time are included; otherweise only
       the file name is listed. If `p' is not contained in the cache, 
       this method returns NIL. *)

    (*----------------------------------------------------------------------*)
    fingerprint(
        READONLY p : APN.T;
        READONLY ignoreDirExpr : FindExpr.T := NIL; 
        READONLY ignoreFileExpr : FindExpr.T := NIL
    ) : REF Fingerprint.T;
    (* Compute a fingerprint for file or directory `p'. If `p' is a
       file, the fingerprint is computed based on the name and
       the modification time of `p'. If `p' is a directory, then
       the fingerprint is based on the fingerprints of all 
       contained files and directories as well. If `p' is not contained 
       in the cache, this method returns NIL. 
       Do not use any directories matched by `ignoreDirExpr' and
       any files matched by `ignoreFileExpr' to compute the fingerprint.
    *)

    (*----------------------------------------------------------------------*)
    dump (wr : Wr.T);
    (* dump the cache contents to wr for debugging purposes *)

  END (*of Public*);

(*--------------------------------------------------------------------------*)
PROCEDURE TextSeqToTextHashTbl(s : TextSeq.T) : TextHashTbl.T;
PROCEDURE AbsPathnSeqToAbsPathnHashTbl(s : APNSeq.T) : 
  AbsPathnHashTbl.T;
(* Auxiliary procedures for the conversion of sequences of strings or 
   abstract pathnames to hash tables of strings or abstract pathnames. 

   A NIL argument sequence is allowed and will be converted to a hash table
   that has been initialized with a size hint of 0. *)

(*--------------------------------------------------------------------------*)
PROCEDURE ComputeFingerprint(
    self : T; READONLY p : APN.T;
    READONLY ignoreDirExpr : FindExpr.T := NIL; 
    READONLY ignoreFileExpr : FindExpr.T := NIL) : REF Fingerprint.T;

PROCEDURE ComputeFingerprintRec(
    self : T; READONLY p : APN.T;
    READONLY ignoreDirExpr : FindExpr.T := NIL; 
    READONLY ignoreFileExpr : FindExpr.T := NIL) : REF Fingerprint.T;

(*--------------------------------------------------------------------------*)
END FileInfo.
