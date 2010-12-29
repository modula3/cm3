(* zconf.h -- configuration of the zlib compression library
 * Copyright (C) 1995-2005 Jean-loup Gailly.
 * For conditions of distribution and use, see copyright notice in zlib.h
 *)

INTERFACE zconf;

IMPORT Ctypes;

TYPE
  Byte = Ctypes.unsigned_char;  (* 8 bits *)
  uInt = Ctypes.unsigned_int;   (* 16 bits or more *)
  uLong = Ctypes.unsigned_long; (* 32 bits or more *)
  Bytef = Byte; (* "f" for "far" *)
  charf = Ctypes.char;
  intf = Ctypes.int;
  uLongf = uLong;
  uIntf = uInt;
  voidpc = Ctypes.const_void_star;
  voidpf = Ctypes.void_star;
  voidp = Ctypes.void_star;
  z_off_t = Ctypes.long;

END zconf.
