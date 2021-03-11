(* $Id$ *)

INTERFACE RemoteFileRd;
IMPORT Pathname, Rd, OSError;

PROCEDURE Open(p : Pathname.T) : Rd.T RAISES { OSError.E };
(* open file in one of following formats
   <localfile>
   <remotehost>:<remotefile>
   <remoteuser>@<remotehost>:<remotefile>

   <remotefile> may contain colons, <localfile> may not

   can't seek on the returned Rd.T!

   [Command that is run is "ssh <remoteuser>@<remotehost> cat <remotefile>,
   and all that applies to that command applies to this Rd.T as well.
   
   Before using this interface, check that you can access the remote 
   host without typing a password or confirming ssh keys, etc.]
 *)

END RemoteFileRd.
