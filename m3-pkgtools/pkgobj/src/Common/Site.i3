(* Copyright 1990 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Site.i3 *)
(* Last modified on Wed Mar  2 16:41:06 PST 1994 by wobber *)
(*      modified on Mon Nov 25 17:15:57 GMT+1:00 1991 by prusker *)

INTERFACE Site;

IMPORT IP, NetPath, Thread;

TYPE Int32  = BITS 32 FOR [-16_7FFFFFFF-1..16_7FFFFFFF];

TYPE
  T = REF RECORD
    name: TEXT;
    lockserver: TEXT;
    siphonserver: TEXT;
    ipPort: BITS 32 FOR IP.Port;
       (* if not IP.NullPort, then servers in this site
          will listen on the named well-known IP port ...
          server names must map into IP addresses by the
          standard Unix gethostbyname trick. *)
       (* otherwise, depend on the NetObj runtime to decide
          upon a transport and how to map names to addresses *)
    padding: Int32;  (* for pickle weirdness *)
    replicas: REF ARRAY OF TEXT;
    backupHosts: REF ARRAY OF TEXT;
    owner: TEXT;
    defaultRepository: NetPath.T;
    foreignSites: REF ARRAY OF Remote;
  END;
  
  Remote = RECORD
    name: TEXT;
    siphonserver: TEXT;
    ipPort: BITS 32 FOR IP.Port;
    padding: Int32;  (* for pickle weirdness *)
    route: REF ARRAY OF TEXT;
    (* if non-NIL, "route" specifies set of sites through
       which one gets to the destination site *)
  END;

CONST EnvVarName = "SIPHON_SITE";
   (* This environment variable is used to specify how the packagetools
      find configuration information.  Each siphon and packageserver
      export an object from which such information can be obtained.
      The environment variable names the network address where a
      site info object might be found.

      (Package and siphon servers always read from a configuration 
      file and therefore this environment variable has no effect 
      upon them.) 
      
      The recognized formats for the environment string are as follows:

            hostname          (e.g. aspen or aspen.pa.dec.com) 
            hostname:tcpport  (e.g. aspen.pa.dec.com:5555)

      The former directs the Network Object runtime to interpret
      "hostname" according to the specification of NetObj.Locate.
      The latter indicates that hostname is an Internet host name
      and that the TCP transport should be used to a well-known
      port. *)

TYPE EC = {
   MissingEnvVariable,
   InfoServerUnavailable,
   BadInfoServerSpec,
   BadIPPortSpec
 };

EXCEPTION Error(EC);

PROCEDURE Init() : T RAISES {Error, Thread.Alerted};
    (* init must be called first, error code indicates problem *)

PROCEDURE Get(cacheOK: BOOLEAN := TRUE): T;

PROCEDURE FindRemote(
      site: TEXT;
      VAR (*OUT*) remote: Remote;
      cacheOK: BOOLEAN := FALSE) : (*found*) BOOLEAN;
      (* returns FALSE if the argument site doesn't exist. *)

PROCEDURE ErrMsg(ec: EC) : TEXT;

END Site.
