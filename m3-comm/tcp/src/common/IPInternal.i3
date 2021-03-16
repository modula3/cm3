INTERFACE IPInternal;
IMPORT Ctypes, IP;

TYPE
  char_star = Ctypes.char_star;
  const_char_star = Ctypes.const_char_star;
  EP = IP.EP;
  int = Ctypes.int;
  Address4 = IP.Address4;
  Address16 = IP.Address16;

(* Address4 is a 4 byte IPv4 address
 * ADDRESS is Modula3 like void* and mimic
 * old behavior of null or non-null hostent*
 *)

(* temporary until m3core updated *)
<*EXTERNAL "IPInternal__AF_INET6"*> VAR AF_INET6: int;

<*EXTERNAL "IPInternal__TRY_AGAIN"*>   VAR TRY_AGAIN:   int;
<*EXTERNAL "IPInternal__NO_RECOVERY"*> VAR NO_RECOVERY: int;
<*EXTERNAL "IPInternal__NO_ADDRESS"*>  VAR NO_ADDRESS:  int;

(* Modula3 code so C never handles traced references. *)
PROCEDURE CopyStoT(s: char_star; VAR text: TEXT);
PROCEDURE NewEndpoint4(VAR endpoint: EP; port: int; VAR address: Address4);
PROCEDURE NewEndpoint6(VAR endpoint: EP; port: int; VAR address: Address16);

PROCEDURE InterpretError(err: int) RAISES {IP.Error};

(* The rest is C code so Modula3 does not interact directly with /usr/include. *)

<*EXTERNAL "IPInternal__Init"*>
PROCEDURE Init();

(* return hostent=NIL for error to mimic old patterns *)
<*EXTERNAL "IPInternal__GetHostByName"*>
PROCEDURE GetHostByName(nm: char_star; VAR res: Address4; VAR hostent: ADDRESS): int;

(* return hostent=NIL for error to mimic old patterns *)
<*EXTERNAL "IPInternal__GetCanonicalByName"*>
PROCEDURE GetCanonicalByName(nm: char_star; VAR text: TEXT; VAR hostent: ADDRESS): int;

<*EXTERNAL "IPInternal__GetCanonicalByAddr"*>
PROCEDURE GetCanonicalByAddr(VAR addr: Address4; VAR text: TEXT; VAR hostent: ADDRESS): int;

(* return hostent=NIL for error to mimic old patterns *)
<*EXTERNAL "IPInternal__GetHostAddr"*>
PROCEDURE GetHostAddr(VAR addr: Address4; VAR hostent: ADDRESS): int;

<*EXTERNAL "IPInternal__GetAddrInfo"*>
PROCEDURE GetAddrInfo(VAR endpoint: EP; node, port: char_star): int;

<*EXTERNAL "IPInternal__GetNameInfo"*>
PROCEDURE GetNameInfo(family, port: int; addr: ADDRESS; VAR host, service: TEXT): int;

<*EXTERNAL "IPInternal__getsockname"*>
PROCEDURE getsockname(fd: INTEGER; address: char_star; VAR port: INTEGER): INTEGER;

<*EXTERNAL "IPInternal__NewConnector_Bind"*>
PROCEDURE NewConnector_Bind(fd: INTEGER; address: const_char_star; port: INTEGER): INTEGER;

END IPInternal.
