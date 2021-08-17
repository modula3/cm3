UNSAFE INTERFACE UDPInternal;

(* Isolate Modula-3 from /usr/include e.g. struct sockaddr *)

FROM Ctypes IMPORT char_star;
TYPE Address4 = char_star; (* instead of IP.Address4 for clarity of ABI *)

<*EXTERNAL "UDPInternal__Init"*>
PROCEDURE Init(VAR fd: INTEGER; addr: Address4; port: INTEGER; VAR err, status: INTEGER);

<*EXTERNAL "UDPInternal__Send"*>
PROCEDURE Send(fd: INTEGER; VAR data: ADDRESS; len: INTEGER; addr: Address4; port: INTEGER): INTEGER;

<*EXTERNAL "UDPInternal__Receive"*>
PROCEDURE Receive(fd: INTEGER; VAR data: ADDRESS; len: INTEGER; addr: Address4; VAR port: INTEGER): INTEGER;

END UDPInternal.
