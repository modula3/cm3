
(* Copyright 1996-2000 Critical Mass, Inc. All Rights Reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE IPTypeinVBT;
IMPORT IP, TypeinVBT;

(* "IPTypeinVBT" is a type-in which allows 
   input of dotted TCP/IP addresses, e.g., "127.0.0.1". 
   The user can enter the number itself; dots are
   inserted automatically upon the completion of each
   3-character octet. The user cannot enter numbers 
   greater than 255 for an octet, hence, the first digit 
   of the octet affects the number of octets allowed
   in the rest of that particular octet. *)

TYPE
  T <: TypeinVBT.T;

PROCEDURE Get(v: T): IP.Address RAISES {InvalidAddress};
PROCEDURE Put(v: T; addr: IP.Address) RAISES {InvalidAddress};

(* "Get" and "Put" procedures convert "IP.Address" datatypes
   to and from what is displayed by the VBT. If the address
   is incomplete or invalid, the "InvalidAddress" exception
   is raised. *)

EXCEPTION InvalidAddress;

END IPTypeinVBT.
