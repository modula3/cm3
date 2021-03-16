UNSAFE INTERFACE UDPInternal;

(* Isolate Modula-3 from /usr/include e.g. struct sockaddr *)

IMPORT IP;
FROM Ctypes IMPORT int;
TYPE Address4 = Ctypes.char_star; (* instead of IP.Address4 for clarity of ABI *)

<*EXTERNAL "UDPInternal__Init"*>
PROCEDURE Init(VAR fd: INTEGER; addr: Address4; port: int; VAR err, status: int);

<*EXTERNAL "UDPInternal__Send"*>
PROCEDURE Send(fd: INTEGER; VAR data: ADDRESS; len: INTEGER; addr: Address4; port: int): INTEGER;

<*EXTERNAL "UDPInternal__Receive"*>
PROCEDURE Receive(fd: INTEGER; VAR data: ADDRESS; len: INTEGER; addr: Address4; port: int): INTEGER;

END UDPInternal.
