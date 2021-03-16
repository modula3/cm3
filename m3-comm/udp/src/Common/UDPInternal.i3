UNSAFE INTERFACE UDPInternal;

(* Isolate Modula-3 from /usr/include e.g. struct sockaddr *)

FROM Ctypes IMPORT int, char_star;
TYPE Address4 = char_star; (* instead of IP.Address4 for clarity of ABI *)

<*EXTERNAL "UDPInternal__Init"*>
PROCEDURE Init(VAR fd: INTEGER; addr: Address4; port: int; VAR err, status: int);

<*EXTERNAL "UDPInternal__Send"*>
PROCEDURE Send(fd: INTEGER; VAR data: ADDRESS; len: INTEGER; addr: Address4; port: int): INTEGER;

<*EXTERNAL "UDPInternal__Receive"*>
PROCEDURE Receive(fd: INTEGER; VAR data: ADDRESS; len: INTEGER; addr: Address4; port: int): INTEGER;

END UDPInternal.
