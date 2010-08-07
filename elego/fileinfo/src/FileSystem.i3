(*--------------------------------------------------------------------------*)
INTERFACE FileSystem;
(*file system interface*)

(*--------------------------------------------------------------------------*)
IMPORT Text, FileStatus, Pathname, APN;

(*--------------------------------------------------------------------------*)
CONST Brand = "FileSystem";

(*--------------------------------------------------------------------------*)
PROCEDURE Lookup  (p : APN.T) : FileStatus.T;
PROCEDURE LookupS (p : Pathname.T) : FileStatus.T;
(*Obtain file status information for the directory or file designated
  by p. If there is not any, status record is returned whose exists-flag 
  is set to false.*) 

(*--------------------------------------------------------------------------*)
TYPE 
  Iterator <: IteratorPublic;
  (*object type for file and directory name iterators*)  

  IteratorPublic = OBJECT METHODS
    next (VAR (*OUT*) n : Text.T) : BOOLEAN;
    (*Try to get the next file or directory name.
      If there is a name, the return value is true. 
      Otherwise, the return value is false and n is undefined.*)

    nextWithStatus (VAR (*OUT*) n : APN.T; 
                    VAR (*OUT*) s : FileStatus.T) : BOOLEAN;
    (*Try to get the next file or directory name and a status record.
      If there are a name and a record, the return value is true. 
      Otherwise, the return value is false and n and s are undefined.*)

    nextWithStatusS (VAR (*OUT*) n : Pathname.T; 
                     VAR (*OUT*) s : FileStatus.T) : BOOLEAN;

    close ();
    (*Close the iterator. Iterators should always be closed.*)
  END;

(*--------------------------------------------------------------------------*)
PROCEDURE NextWithStatus (
    sf : Iterator; 
    VAR (*OUT*) n : APN.T; 
    VAR (*OUT*) s : FileStatus.T) : BOOLEAN;
PROCEDURE NextWithStatusS (
    sf : Iterator; 
    VAR (*OUT*) n : Pathname.T; 
    VAR (*OUT*) s : FileStatus.T) : BOOLEAN;

(*--------------------------------------------------------------------------*)
PROCEDURE GetIterator  (p : APN.T) : Iterator;
PROCEDURE GetIteratorS (p : Pathname.T) : Iterator;
(*Get an iterator over the contents of the directory designated by p.
  If p does not designate any accessible directory, then the result
  is still defined but next and nextWithStatus will immediately return
  false.*)

(*--------------------------------------------------------------------------*)
END FileSystem.
 
